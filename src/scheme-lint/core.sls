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

          ;; CST transformation specs
          make-cst-transform
          cst-transform?
          cst-transform-type
          cst-transform-nodes

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

)

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

;; CST transformation spec
;; Describes how to transform a CST node
(define-record-type (cst-transform make-cst-transform cst-transform?)
  (nongenerative)
  (sealed #t)
  (fields type    ; Symbol: 'replace | 'insert-before | 'insert-after | 'delete
          nodes)) ; List of CST nodes (or empty for delete)

;; Violation: linting violation found in code
;; fix is either #f (no fix) or cst-transform
(define-record-type (lint-violation make-lint-violation lint-violation?)
  (nongenerative)
  (sealed #t)
  (fields severity message location code fix node-kind))

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
  (let ((tree (read-all-cst port filename)))
    (apply append (map (lambda (rule)
                        (find-violations rule tree filename))
                      rules))))

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
  ;; Use walk-tree-with-trivia if pattern is ?any to include comments/whitespace
  (let* ((include-trivia? (eq? pattern '?any))
         (all-matches
          (apply append
                 (map (lambda (node)
                        (walk-tree-with-trivia
                          (lambda (n)
                            (let ((bindings (match-pattern pattern n '())))
                              (if (and bindings (predicate bindings n))
                                  (cons bindings n)
                                  #f)))
                          node
                          include-trivia?))
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
                   fix
                   (cst-node-kind node))))

;;=============================================================================
;; Auto-fixing (REAL IMPLEMENTATION)

;; apply-fixes : list string => void
;;   effects: io/read io/write
;;   Applies fixes from violations to file.
(define (apply-fixes violations file-path)
  ;; Apply all auto-fixes to file
  (when (and (file-exists? file-path)
             (not (null? (filter lint-violation-fix violations))))
    (let ((fixes (filter lint-violation-fix violations)))
      (apply-cst-based-fixes fixes file-path))))

(define (apply-cst-based-fixes fixes file-path)
  ;; CST-based fix application
  (let* (;; Read CST from file
         (cst-nodes (call-with-input-file file-path
                      (lambda (port) (read-all-cst port file-path))))
         ;; Build transformation map: node -> transform
         (transform-map (make-transform-map fixes cst-nodes))
         ;; Apply transformations to CST
         (transformed-nodes (transform-cst-nodes cst-nodes transform-map))
         ;; Render back to string
         (fixed-content (render-cst-nodes transformed-nodes))
         (temp-path (string-append file-path ".tmp")))
    (when (file-exists? temp-path)
      (delete-file temp-path))
    (let ((port (open-file-output-port temp-path
                                       (file-options no-fail)
                                       (buffer-mode block)
                                       (native-transcoder))))
      (put-string port fixed-content)
      (close-port port))
    (delete-file file-path)
    (chez:rename-file temp-path file-path)))

(define (assoc-lookup key alist)
  ;; Lookup in association list, return #f if not found
  (let ((pair (assoc key alist)))
    (if pair (cdr pair) #f)))

(define (flatten-cst nodes)
  ;; Get all CST nodes from a tree (depth-first)
  (apply append
         (map (lambda (node)
                (cons node
                      (cond
                        ((cst-list? node)
                         (flatten-cst (cst-list-children node)))
                        ((cst-vector? node)
                         (flatten-cst (cst-vector-children node)))
                        ((cst-quote? node)
                         (flatten-cst (list (cst-quote-expr node))))
                        (else '()))))
              nodes)))

(define (make-transform-map fixes nodes)
  ;; Build map from CST node to transformation
  ;; Match nodes by position AND kind since multiple nodes can have same position
  ;; Returns association list: (node . transform)
  (let ((position-map
         (map (lambda (violation)
                (cons (list (source-location-line (lint-violation-location violation))
                           (source-location-column (lint-violation-location violation))
                           (lint-violation-node-kind violation))
                      (lint-violation-fix violation)))
              fixes)))
    ;; Now map nodes to transforms based on position AND kind
    (filter (lambda (pair) (cdr pair))  ; Remove nodes without transforms
            (map (lambda (node)
                   (let* ((key (list (cst-node-start-line node)
                                    (cst-node-start-column node)
                                    (cst-node-kind node)))
                          (transform (assoc-lookup key position-map)))
                     (cons node transform)))
                 (flatten-cst nodes)))))

(define (transform-cst-nodes nodes transform-map)
  ;; Transform a list of CST nodes according to transform-map
  ;; Returns new list of CST nodes
  (apply append
         (map (lambda (node)
                (transform-cst-node node transform-map))
              nodes)))

(define (transform-cst-node node transform-map)
  ;; Transform a single CST node
  ;; Returns list of nodes (empty for delete, multiple for insert-before/after)
  (let ([transform-pair (assq node transform-map)])
    (let ([transform (if transform-pair (cdr transform-pair) #f)])
    (cond
      ;; Node has a transformation
      (transform
       (case (cst-transform-type transform)
         ((delete) '())
         ((replace) (cst-transform-nodes transform))
         ((insert-before)
          (append (cst-transform-nodes transform)
                  (list (transform-node-children node transform-map))))
         ((insert-after)
          (append (list (transform-node-children node transform-map))
                  (cst-transform-nodes transform)))
         (else (list (transform-node-children node transform-map)))))

      ;; No transformation - recursively transform children
      (else
       (list (transform-node-children node transform-map)))))))

(define (transform-node-children node transform-map)
  ;; Transform children of a node, keeping the node structure
  (cond
    ((cst-list? node)
     (let ((transformed-children (transform-cst-nodes (cst-list-children node) transform-map)))
       (make-cst-list (cst-node-kind node)
                     (cst-node-start-line node)
                     (cst-node-start-column node)
                     (cst-node-start-pos node)
                     (cst-node-end-pos node)
                     (render-cst-nodes (list node))  ; Use original text as fallback
                     (cst-list-open-delim node)
                     transformed-children
                     (cst-list-close-delim node))))

    ((cst-vector? node)
     (let ((transformed-children (transform-cst-nodes (cst-vector-children node) transform-map)))
       (make-cst-vector (cst-node-kind node)
                       (cst-node-start-line node)
                       (cst-node-start-column node)
                       (cst-node-start-pos node)
                       (cst-node-end-pos node)
                       (render-cst-nodes (list node))
                       transformed-children)))

    ((cst-quote? node)
     (let ((transformed-expr-list (transform-cst-node (cst-quote-expr node) transform-map)))
       ;; Quote should have exactly one child
       (if (null? transformed-expr-list)
           node  ; Keep original if child was deleted
           (make-cst-quote (cst-node-kind node)
                          (cst-node-start-line node)
                          (cst-node-start-column node)
                          (cst-node-start-pos node)
                          (cst-node-end-pos node)
                          (render-cst-nodes (list node))
                          (cst-quote-type node)
                          (cst-quote-prefix node)
                          (car transformed-expr-list)))))

    ;; Leaf nodes - return as-is
    (else node)))

(define (render-cst-nodes nodes)
  ;; Render list of CST nodes back to string
  ;; Must recursively render structure, not use cached text
  (call-with-string-output-port
    (lambda (port)
      (for-each (lambda (node)
                  (render-cst-node node port))
                nodes))))

(define (render-cst-node node port)
  ;; Render a single CST node to port
  (cond
    ((cst-list? node)
     (display (cst-list-open-delim node) port)
     (for-each (lambda (child) (render-cst-node child port))
               (cst-list-children node))
     (display (cst-list-close-delim node) port))

    ((cst-vector? node)
     (display "#(" port)
     (for-each (lambda (child) (render-cst-node child port))
               (cst-vector-children node))
     (display ")" port))

    ((cst-quote? node)
     (display (cst-quote-prefix node) port)
     (render-cst-node (cst-quote-expr node) port))

    ;; Leaf nodes - use their text directly
    ((cst-atom? node)
     (display (cst-node-text node) port))

    ((cst-string? node)
     (display (cst-node-text node) port))

    ((cst-comment? node)
     (display (cst-node-text node) port))

    ((cst-whitespace? node)
     (display (cst-whitespace-text node) port))

    (else
     ;; Fallback
     (display (cst-node-text node) port))))

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
