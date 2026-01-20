;;=======================================================================
;; import-only-chez.scm - Require (only ...) for chezscheme imports
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule import-only-chez
  (pattern (import . ?imports))

  (where
    ;; Check if any chezscheme import lacks (only ...)
    (let check-imports ((imps (get-binding bindings '?imports)))
      (cond
        ((null? imps) #f)
        (else
          (let* ((imp (car imps))
                 (imp-expr (if (annotated? imp) (annotated-expr imp) imp)))
            (cond
              ;; Direct (chezscheme) import without (only ...)
              ((and (pair? imp-expr)
                    (eq? (car imp-expr) 'chezscheme))
               #t) ;; Violation

              ;; Check if it's an (only (chezscheme) ...) - this is OK
              ((and (pair? imp-expr)
                    (eq? (car imp-expr) 'only)
                    (pair? (cdr imp-expr))
                    (pair? (cadr imp-expr))
                    (eq? (car (cadr imp-expr)) 'chezscheme))
               (check-imports (cdr imps))) ;; OK, continue

              ;; Other imports
              (else (check-imports (cdr imps)))))))))

  (severity warning)

  (message "chezscheme imports must use (only (chezscheme) ...) for explicitness"))
