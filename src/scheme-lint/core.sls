;;=============================================================================
;; core.sls - Pluggable linter for Scheme code
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
          make-plugin-environment)

  (import (rnrs)
          (rnrs eval)
          (rnrs io ports)
          (rnrs io simple)
          (scheme-lint reader)
          (scheme-lint matcher))

;;=============================================================================
;; Version

(define scheme-lint-version "1.0.0")

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
;; - fixer: #f or (lambda (bindings expr) => new-expr)

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
  (let read-all ((exprs '()))
    (let ((expr (read-annotated port filename)))
      (if (eof-object? expr)
          (let ((tree (reverse exprs)))
            (apply append (map (lambda (rule)
                                (find-violations rule tree))
                              rules)))
          (read-all (cons expr exprs))))))

(define (find-violations rule tree)
  (let ((matches (find-matches (lint-rule-pattern rule)
                               (lint-rule-predicate rule)
                               tree)))
    (map (lambda (match)
           (let ((bindings (car match))
                 (expr (cdr match)))
             (make-lint-violation-from-rule rule bindings expr)))
         matches)))

(define (find-matches pattern predicate tree)
  (let ((all-matches
          (apply append
                 (map (lambda (expr)
                        (walk-tree
                          (lambda (node)
                            (let ((bindings (match-pattern pattern node '())))
                              (if (and bindings (predicate bindings node))
                                  (cons bindings node)
                                  #f)))
                          expr))
                      tree))))
    ;; Deduplicate by source location
    (dedup-by-location all-matches)))

(define (dedup-by-location matches)
  (let loop ((matches matches)
             (seen '())
             (result '()))
    (if (null? matches)
        (reverse result)
        (let* ((match (car matches))
               (expr (cdr match))
               (loc (if (annotated? expr)
                       (annotated-source expr)
                       #f))
               (loc-key (if loc
                           (cons (source-location-file loc)
                                 (cons (source-location-line loc)
                                       (source-location-column loc)))
                           #f)))
          (if (and loc-key (member loc-key seen))
              (loop (cdr matches) seen result)
              (loop (cdr matches)
                    (if loc-key (cons loc-key seen) seen)
                    (cons match result)))))))

(define (make-lint-violation-from-rule rule bindings expr)
  (let* ((location (if (annotated? expr)
                      (annotated-source expr)
                      #f))
         (code (expr->string expr))
         (message-fn (lint-rule-message rule))
         (message (if (procedure? message-fn)
                     (message-fn bindings expr)
                     message-fn))
         (fixer (lint-rule-fixer rule))
         (fix (if fixer
                 (fixer bindings expr)
                 #f)))
    (make-lint-violation (lint-rule-severity rule)
                   message
                   location
                   code
                   fix)))

;;=============================================================================
;; Auto-fixing

;; apply-fixes : list string => void
;;   effects: io/read io/write
;;   Applies fixes from violations to file.
(define (apply-fixes violations file-path)
  (when (file-exists? file-path)
    (let* ((fixes (filter lint-violation-fix violations))
           (content (call-with-input-file file-path
                     (lambda (port)
                       (let read-all ((chars '()))
                         (let ((ch (get-char port)))
                           (if (eof-object? ch)
                               (list->string (reverse chars))
                               (read-all (cons ch chars))))))))
           (fixed (apply-fixes-to-string content fixes)))
      (with-output-to-file file-path
         (lambda () (display fixed))))))

(define (apply-fixes-to-string content fixes)
  ;; Sort fixes by location (reverse order to avoid offset issues)
  (let ((sorted-fixes (list-sort
                        (lambda (a b)
                          (let ((loc-a (lint-violation-location a))
                                (loc-b (lint-violation-location b)))
                            (if (and loc-a loc-b)
                                (or (> (source-location-line loc-a) (source-location-line loc-b))
                                    (and (= (source-location-line loc-a) (source-location-line loc-b))
                                         (> (source-location-column loc-a) (source-location-column loc-b))))
                                #f)))
                        fixes)))
    ;; For now, just return original content
    ;; Full implementation would apply text edits based on locations
    content))

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
  (environment '(rnrs)
               '(rnrs mutable-pairs)
               '(scheme-lint core)
               '(scheme-lint reader)
               '(scheme-lint matcher)))

(define (eval-rule-dsl expr)
  (unless (and (pair? expr) (eq? (car expr) 'rule))
    (error 'eval-rule-dsl "Expected (rule ...) form" expr))

  (let* ((name (cadr expr))
         (body (cddr expr))
         (pattern (cadr (assq 'pattern body)))
         (where (let ((w (assq 'where body)))
                  (if w (cadr w) #t)))
         (severity-sym (cadr (assq 'severity body)))
         (severity (case severity-sym
                    ((error) severity/error)
                    ((warning) severity/warning)
                    ((style) severity/style)
                    (else severity/warning)))
         (message (cadr (assq 'message body)))
         (fix (let ((f (assq 'fix body)))
               (if f (cadr f) #f))))

    (make-lint-rule
      name
      pattern
      (compile-where-clause where)
      severity
      message
      #f)))

(define (compile-where-clause where-expr)
  (if (eq? where-expr #t)
      (lambda (bindings expr) #t)
      ;; The where clause will be evaluated with bindings, expr, and helpers in scope
      ;; We wrap it in a lambda and eval that
      (let* ((wrapped-expr `(lambda (bindings expr)
                             (let ((get-binding (lambda (bindings name)
                                                 ;; Fully unwrap annotated values from bindings
                                                 (let* ((pair (assq name bindings))
                                                        (value (if pair (cdr pair) #f)))
                                                   (unwrap-annotated value)))))
                               ,where-expr)))
             (env (environment '(rnrs)
                              '(scheme-lint reader)
                              '(scheme-lint matcher))))
        (eval wrapped-expr env))))

;;=============================================================================
;; Utilities

(define (expr->string expr)
  (let ((raw (if (annotated? expr)
                 (annotated-expr expr)
                 expr)))
    (call-with-string-output-port
      (lambda (port)
        (write raw port)))))

) ;; end library
