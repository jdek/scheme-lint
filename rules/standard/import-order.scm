;;=======================================================================
;; import-order.scm - Check import ordering (rnrs -> local -> chezscheme)
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule import-order
  (metadata
    (version "1.0.0")
    (author "scheme-lint")
    (category style)
    (tags r6rs imports conventions)
    (enabled-by-default #t)
    (description "Enforces the CLAUDE.md convention that imports should be ordered: rnrs -> local -> chezscheme. This improves readability and makes dependencies clear."))

  (pattern (library ?name
             (export . ?exports)
             (import . ?imports)
             . ?body))

  (where
    ;; Check if imports are not in correct order
    (let check-order ((imps (get-binding bindings '?imports))
                     (state 'start)) ;; start -> rnrs -> local -> chez
      (cond
        ((null? imps) #f) ;; No violations if we made it through
        (else
          (let* ((imp (car imps))
                 (imp-expr (if (annotated? imp) (annotated-expr imp) imp))
                 (lib-name (if (and (pair? imp-expr) (pair? (car imp-expr)))
                              (car (car imp-expr))
                              (if (pair? imp-expr) (car imp-expr) #f))))
            (cond
              ;; rnrs import
              ((and (symbol? lib-name)
                    (let ((s (symbol->string lib-name)))
                      (and (>= (string-length s) 4)
                           (string=? (substring s 0 4) "rnrs"))))
               (if (eq? state 'start)
                   (check-order (cdr imps) 'rnrs)
                   #t)) ;; Violation: rnrs after non-rnrs

              ;; chezscheme import
              ((eq? lib-name 'chezscheme)
               (if (memq state '(start rnrs local))
                   (check-order (cdr imps) 'chez)
                   #t)) ;; This shouldn't happen, but catch it

              ;; local import
              (else
                (if (eq? state 'chez)
                    #t ;; Violation: local after chez
                    (check-order (cdr imps) 'local)))))))))

  (severity warning)

  (message "Imports should be ordered: rnrs -> local -> chezscheme"))
