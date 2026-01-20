;;=============================================================================
;; builtin.sls - Built-in linting rules
;;=============================================================================

(library (scheme-lint rules builtin)
  (export get-builtin-rules)
  (import (rnrs)
          (scheme-lint core)
          (scheme-lint reader)
          (scheme-lint matcher))

;;=============================================================================
;; Helper for accessing bindings

(define (get-binding bindings name)
  "Get value bound to name in bindings alist"
  (let ((pair (assq name bindings)))
    (if pair
        (cdr pair)
        #f)))

;;=============================================================================
;; Rules

(define rule-div-not-quotient
  (make-lint-rule
    'div-not-quotient
    '(quotient ?a ?b)
    (lambda (bindings expr) #t)
    severity/style
    "Use 'div' instead of 'quotient' (preferred in R6RS)"
    #f))

(define rule-mod-not-modulo
  (make-lint-rule
    'mod-not-modulo
    '(modulo ?a ?b)
    (lambda (bindings expr) #t)
    severity/style
    "Use 'mod' instead of 'modulo' (preferred in R6RS)"
    #f))

(define rule-no-rnrs-all
  (make-lint-rule
    'no-rnrs-all
    '(rnrs)
    (lambda (bindings expr)
      ;; Check if this (rnrs) is inside an import context
      ;; This is a simplified check - it will fire for any (rnrs) expression
      ;; A more sophisticated check would walk up the tree to verify we're in an import
      #t)
    severity/warning
    "Import specific (rnrs ...) modules instead of all of (rnrs)"
    #f))

;;=============================================================================
;; Export

(define (get-builtin-rules)
  "Return list of all built-in rules.
   Note: Most rules are now loaded as plugins from rules/ directory."
  '())

) ;; end library
