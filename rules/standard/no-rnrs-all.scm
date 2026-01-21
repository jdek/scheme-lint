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
          (let ((imp (car imps)))
            (cond
              ;; (rnrs) alone - violation
              ((and (cst-list? imp)
                    (let ((children (semantic-children imp)))
                      (and (= 1 (length children))
                           (cst-atom? (car children))
                           (eq? 'rnrs (cst-atom-value (car children))))))
               #t)

              ;; Other imports
              (else (check-imports (cdr imps)))))))))

  (severity warning)

  (message "Import specific (rnrs ...) modules instead of all of (rnrs)"))
