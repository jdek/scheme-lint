;;=======================================================================
;; import-only-chez.scm - Require explicit (only ...) for chezscheme
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule import-only-chez
  (metadata
    (version "1.0.0")
    (author "scheme-lint")
    (category style)
    (tags r6rs imports conventions chez)
    (enabled-by-default #t)
    (description "Requires (chezscheme) imports to use (only ...) to make dependencies explicit"))

  (pattern (import . ?imports))

  (where
    ;; Check if any chezscheme import doesn't use 'only
    (let check-imports ((imps (get-binding bindings '?imports)))
      (cond
        ((null? imps) #f)
        (else
          (let ((imp (car imps)))
            (cond
              ;; Check if this is a chezscheme import
              ((and (cst-list? imp)
                    (let ((children (semantic-children imp)))
                      (and (pair? children)
                           (cst-atom? (car children))
                           (let ((first-sym (cst-atom-value (car children))))
                             (cond
                               ;; (only (chezscheme) ...) - OK
                               ((eq? first-sym 'only) #f)
                               ;; (chezscheme) - violation
                               ((eq? first-sym 'chezscheme) #t)
                               ;; Other
                               (else #f))))))
               #t)

              ;; Other imports
              (else (check-imports (cdr imps)))))))))

  (severity warning)

  (message "Use (only (chezscheme) ...) to explicitly list imported identifiers"))
