;;=======================================================================
;; no-rnrs-all.scm - Disallow importing all of (rnrs)
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule no-rnrs-all
  (pattern (import . ?imports))

  (where
    ;; Check if any import is just (rnrs) without submodule
    (let check-imports ((imps (get-binding bindings '?imports)))
      (cond
        ((null? imps) #f)
        (else
          (let* ((imp (car imps))
                 (imp-expr (if (annotated? imp) (annotated-expr imp) imp)))
            (cond
              ;; (rnrs) alone - violation
              ((and (pair? imp-expr)
                    (null? (cdr imp-expr))
                    (eq? (car imp-expr) 'rnrs))
               #t)

              ;; Other imports
              (else (check-imports (cdr imps)))))))))

  (severity warning)

  (message "Import specific (rnrs ...) modules instead of all of (rnrs)"))
