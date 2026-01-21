;;=============================================================================
;; core.sls - Pluggable linter for Scheme code (CST-based)
;;=============================================================================
;; SPDX-License-Identifier: WTFPL

(library (scheme-lint core)
  (export make-lint-rule
          lint-rule?
          lint-rule-name
          lint-rule-pattern
          lint-rule-predicate
          lint-rule-severity
          lint-rule-message
          lint-rule-fixer

          make-lint-violation
          lint-violation?
          lint-violation-severity
          lint-violation-message
          lint-violation-location
          lint-violation-code
          lint-violation-fix

          lint-file
          lint-string
          load-rule
          apply-fixes

          severity/error
          severity/warning
          severity/style

          scheme-lint-version

          ;; Exports for discovery system
          eval-rule-dsl
          make-plugin-environment

          ;; Helpers for fix expressions
          string-contains
          string-replace-first)

  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs sorting)
          (rnrs eval)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs files)
          (rnrs exceptions)
          (rnrs conditions)
          (rnrs records syntactic)
          (scheme-lint reader)
          (scheme-lint matcher)
          (prefix (chezscheme) chez:))

;;=============================================================================
;; Version

(define scheme-lint-version "2.0.0")

;;=============================================================================
;; Severity Levels

(define severity/error 'error)
(define severity/warning 'warning)
(define severity/style 'style)

;;=============================================================================
;; Rule Definition

;; Rule: pattern-based linting rule
;; - name: symbol identifying the rule
;; - pattern: s-expression pattern to match
;; - predicate: (lambda (bindings expr) => boolean) - additional check
;; - severity: error/warning/style
;; - message: string or (lambda (bindings expr) => string)
;; - fixer: #f or (lambda (bindings expr) => string) - replacement text

(define-record-type (lint-rule make-lint-rule lint-rule?)
  (nongenerative)
  (sealed #t)
  (fields name pattern predicate severity message fixer))

;;=============================================================================
;; Violation Definition

;; Violation: linting violation found in code
(define-record-type (lint-violation make-lint-violation lint-violation?)
  (nongenerative)
  (sealed #t)
  (fields severity message location code fix))

;;=============================================================================
;; Linting Core

;; lint-file : string list => list
;;   effects: io/read
;;   raises: if file not found
;;   Lints file with given rules.
(define (lint-file path rules)
  (if (not (file-exists? path))
      (list (make-lint-violation severity/error
                           (string-append "File not found: " path)
                           #f
                           #f
                           #f))
      (guard (exn
              (else
               (list (make-lint-violation severity/error
                                    (string-append "Error reading " path ": "
                                                 (if (message-condition? exn)
                                                     (condition-message exn)
                                                     "unknown error"))
                                    #f
                                    #f
                                    #f))))
        (call-with-input-file path
          (lambda (port)
            (lint-port port path rules))))))

;; lint-string : string string list => list
;;   Lints string of code with given rules.
(define (lint-string code filename rules)
  (let ((port (open-string-input-port code)))
    (lint-port port filename rules)))

(define (lint-port port filename rules)
  ;; Lint all CST nodes from port
  (let read-all ((nodes '()))
    (let ((node (read-cst port filename)))
      (if (eof-object? node)
          (let ((tree (reverse nodes)))
            (apply append (map (lambda (rule)
                                (find-violations rule tree filename))
                              rules)))
          (read-all (cons node nodes))))))

(define (find-violations rule tree filename)
  ;; Find all violations of rule in CST tree
  (let ((matches (find-matches (lint-rule-pattern rule)
                               (lint-rule-predicate rule)
                               tree)))
    (map (lambda (match)
           (let ((bindings (car match))
                 (node (cdr match)))
             (make-lint-violation-from-rule rule bindings node filename)))
         matches)))

(define (find-matches pattern predicate tree)
  ;; Find all CST nodes matching pattern and predicate
  (let ((all-matches
          (apply append
                 (map (lambda (node)
                        (walk-tree
                          (lambda (n)
                            (let ((bindings (match-pattern pattern n '())))
                              (if (and bindings (predicate bindings n))
                                  (cons bindings n)
                                  #f)))
                          node))
                      tree))))
    ;; Deduplicate by source location
    (dedup-by-location all-matches)))

(define (dedup-by-location matches)
  ;; Remove duplicate matches at same source location
  (let loop ((matches matches)
             (seen '())
             (result '()))
    (if (null? matches)
        (reverse result)
        (let* ((match (car matches))
               (node (cdr match))
               (loc-key (cons (cst-node-start-line node)
                             (cst-node-start-column node))))
          (if (member loc-key seen)
              (loop (cdr matches) seen result)
              (loop (cdr matches)
                    (cons loc-key seen)
                    (cons match result)))))))

(define (make-lint-violation-from-rule rule bindings node filename)
  ;; Create violation from rule match
  (let* ((location (make-source-location
                     filename
                     (cst-node-start-line node)
                     (cst-node-start-column node)))
         (code (cst-node-text node))
         (message-fn (lint-rule-message rule))
         (message (if (procedure? message-fn)
                     (message-fn bindings node)
                     message-fn))
         (fixer (lint-rule-fixer rule))
         (fix (if fixer
                 (fixer bindings node)
                 #f)))
    (make-lint-violation (lint-rule-severity rule)
                   message
                   location
                   code
                   fix)))

;;=============================================================================
;; Auto-fixing (REAL IMPLEMENTATION)

;; apply-fixes : list string => void
;;   effects: io/read io/write
;;   Applies fixes from violations to file.
(define (apply-fixes violations file-path)
  ;; Apply all auto-fixes to file
  (when (and (file-exists? file-path)
             (not (null? (filter lint-violation-fix violations))))
    (let* ((fixes (filter lint-violation-fix violations))
           (content (call-with-input-file file-path
                     (lambda (port)
                       (get-string-all port))))
           (fixed (apply-fixes-to-string content fixes))
           (temp-path (string-append file-path ".tmp")))
      ;; Write to temp file using R6RS open-file-output-port, then atomically rename
      (when (file-exists? temp-path)
        (delete-file temp-path))
      (let ((port (open-file-output-port temp-path
                                         (file-options no-fail)
                                         (buffer-mode block)
                                         (native-transcoder))))
        (put-string port fixed)
        (close-port port))
      (delete-file file-path)
      (chez:rename-file temp-path file-path))))

(define (apply-fixes-to-string content fixes)
  ;; Apply all fixes to string content
  ;; Sort fixes by position (reverse order to avoid offset issues)
  (let ((sorted-fixes (list-sort
                        (lambda (a b)
                          (let ((loc-a (lint-violation-location a))
                                (loc-b (lint-violation-location b)))
                            (if (and loc-a loc-b)
                                (or (> (source-location-line loc-a)
                                      (source-location-line loc-b))
                                    (and (= (source-location-line loc-a)
                                           (source-location-line loc-b))
                                         (> (source-location-column loc-a)
                                            (source-location-column loc-b))))
                                #f)))
                        fixes)))
    ;; Apply each fix
    (fold-left apply-single-fix content sorted-fixes)))

(define (apply-single-fix content fix)
  ;; Apply single fix to content
  (let* ((location (lint-violation-location fix))
         (code (lint-violation-code fix))
         (replacement (lint-violation-fix fix)))
    ;; Find the position of the code to replace
    ;; For now, use simple string replacement
    ;; TODO: Use precise character positions from CST
    (string-replace-first content code replacement)))

(define (string-replace-first str old new)
  ;; Replace first occurrence of old with new in str
  (let ((idx (string-contains str old)))
    (if idx
        (string-append (substring str 0 idx)
                      new
                      (substring str (+ idx (string-length old))
                                (string-length str)))
        str)))

(define (string-contains str substr)
  ;; Find index of substr in str, or #f
  (let ((slen (string-length str))
        (sslen (string-length substr)))
    (let loop ((i 0))
      (cond
        ((> (+ i sslen) slen) #f)
        ((string=? (substring str i (+ i sslen)) substr) i)
        (else (loop (+ i 1)))))))

;;=============================================================================
;; Rule Loading

;; load-rule : string => list
;;   effects: io/read
;;   Loads rules from file, supporting both DSL and direct make-lint-rule.
(define (load-rule rule-file)
  (call-with-input-file rule-file
    (lambda (port)
      (let loop ((expr (read port))
                 (rules '()))
        (if (eof-object? expr)
            (reverse rules)
            (cond
              ((and (pair? expr) (eq? (car expr) 'rule))
               (loop (read port) (cons (eval-rule-dsl expr) rules)))
              ((and (pair? expr) (eq? (car expr) 'make-lint-rule))
               (let ((env (make-plugin-environment)))
                 (loop (read port) (cons (eval expr env) rules))))
              (else
               (loop (read port) rules))))))))

(define (make-plugin-environment)
  ;; Create environment for rule evaluation
  (environment '(rnrs)
               '(scheme-lint core)
               '(scheme-lint reader)
               '(scheme-lint matcher)))

(define (eval-rule-dsl expr)
  ;; Evaluate DSL rule form
  (unless (and (pair? expr) (eq? (car expr) 'rule))
    (error 'eval-rule-dsl "Expected (rule ...) form" expr))

  (let* ((name (cadr expr))
         (body-raw (cddr expr))
         ;; Filter out metadata clause
         (body (filter (lambda (clause)
                         (not (and (pair? clause) (eq? (car clause) 'metadata))))
                       body-raw))
         (pattern-entry (assq 'pattern body))
         (pattern (if pattern-entry
                     (cadr pattern-entry)
                     (error 'eval-rule-dsl "Missing 'pattern' clause" name)))
         (where (let ((w (assq 'where body)))
                  (if w (cadr w) #t)))
         (severity-entry (assq 'severity body))
         (severity-sym (if severity-entry
                          (cadr severity-entry)
                          (error 'eval-rule-dsl "Missing 'severity' clause" name)))
         (severity (case severity-sym
                    ((error) severity/error)
                    ((warning) severity/warning)
                    ((style) severity/style)
                    (else severity/warning)))
         (message-entry (assq 'message body))
         (message (if message-entry
                     (cadr message-entry)
                     (error 'eval-rule-dsl "Missing 'message' clause" name)))
         (fix (let ((f (assq 'fix body)))
               (if f (cadr f) #f))))

    (make-lint-rule
      name
      pattern
      (compile-where-clause where)
      severity
      message
      (if fix (compile-fix-clause fix) #f))))

(define (compile-where-clause where-expr)
  ;; Compile where clause into predicate function
  (if (eq? where-expr #t)
      (lambda (bindings node) #t)
      ;; Evaluate where clause with CST helpers in scope
      (let* ((wrapped-expr
              `(lambda (bindings expr)
                 ;; Define CST helpers
                 (define (get-binding bindings name)
                   (let ((pair (assq name bindings)))
                     (if pair (cdr pair) #f)))

                 (define (get-semantic-value bindings name)
                   (let ((node (get-binding bindings name)))
                     (if node (cst->sexp node) #f)))

                 ;; CST type predicates available
                 (let ((cst-atom? cst-atom?)
                       (cst-list? cst-list?)
                       (cst-string? cst-string?)
                       (cst-comment? cst-comment?)
                       (cst-whitespace? cst-whitespace?)
                       (cst-quote? cst-quote?)
                       (cst-vector? cst-vector?)
                       (cst-atom-value cst-atom-value)
                       (cst-string-value cst-string-value)
                       (cst-list-children cst-list-children)
                       (cst-vector-children cst-vector-children)
                       (cst-node-text cst-node-text)
                       (semantic-children semantic-children)
                       (trivia-node? trivia-node?)
                       (cst->sexp cst->sexp))
                   ,where-expr)))
             (env (make-plugin-environment)))
        (eval wrapped-expr env))))

(define (compile-fix-clause fix-expr)
  ;; Compile fix clause into fixer function (bindings node -> string)
  (let ((env (make-plugin-environment)))
    (eval fix-expr env)))

;;=============================================================================
;; Utilities

(define (expr->string node)
  ;; Convert CST node to string representation
  (cst-node-text node))

) ;; end library
