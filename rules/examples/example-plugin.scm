;;=======================================================================
;; example-plugin.scm - Example plugin rule
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(make-lint-rule
  'example-rule
  '(+ ?a ?b)
  (lambda (bindings expr)
    ;; Only flag if both arguments are literals
    (let* ((get-binding (lambda (bindings name)
                         (let ((pair (assq name bindings)))
                           (if pair (cdr pair) #f))))
           (a (get-binding bindings '?a))
           (b (get-binding bindings '?b)))
      (and a b
           (number? (if (annotated? a) (annotated-expr a) a))
           (number? (if (annotated? b) (annotated-expr b) b)))))
  severity/style
  "Constant arithmetic should be pre-computed"
  #f)
