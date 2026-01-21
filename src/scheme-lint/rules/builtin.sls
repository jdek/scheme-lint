;;=============================================================================
;; builtin.sls - Built-in linting rules (CST-based)
;;=============================================================================

(library (scheme-lint rules builtin)
  (export get-builtin-rules)
  (import (rnrs base)
          (scheme-lint core)
          (scheme-lint reader)
          (scheme-lint matcher))

;;=============================================================================
;; Rules

;; Note: Most rules are now loaded as plugins from rules/ directory.
;; These builtins are kept for backward compatibility but are empty.

(define (get-builtin-rules)
  "Return list of all built-in rules.
   Note: Most rules are now loaded as plugins from rules/ directory."
  '())

) ;; end library
