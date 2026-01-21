;;=======================================================================
;; import-order.scm - Check import ordering (rnrs -> local -> chezscheme)
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule import-order
  (pattern (import . ?imports))

  (where
    (let ((imports (get-binding bindings '?imports)))
      ;; Check if imports are not in correct order
      (let check-order ((imps imports)
                       (state 'start)) ;; start -> rnrs -> local -> chez
        (cond
          ((null? imps) #f) ;; No violations if we made it through
          (else
            (let* ((imp (car imps))
                   ;; Extract library name from import spec
                   (lib-name
                      (if (cst-list? imp)
                         (let ((children (semantic-children imp)))
                           (if (and (pair? children) (cst-atom? (car children)))
                               (let* ((first (car children))
                                      (sym (cst-atom-value first)))
                                 ;; Handle (only (chezscheme) ...) etc
                                 (if (memq sym '(only except prefix rename))
                                     (let ((rest (cdr children)))
                                       (if (and (pair? rest) (cst-list? (car rest)))
                                           (let ((inner (semantic-children (car rest))))
                                             (if (and (pair? inner) (cst-atom? (car inner)))
                                                 (cst-atom-value (car inner))
                                                 #f))
                                           #f))
                                     sym))
                               #f))
                         #f)))
              (cond
                ;; rnrs import
                ((and lib-name
                      (symbol? lib-name)
                      (let ((s (symbol->string lib-name)))
                        (and (>= (string-length s) 4)
                             (string=? (substring s 0 4) "rnrs"))))
                 (if (memq state '(start rnrs))
                     (check-order (cdr imps) 'rnrs)
                     #t)) ;; Violation: rnrs after non-rnrs

                ;; chezscheme import
                ((eq? lib-name 'chezscheme)
                 (if (memq state '(start rnrs local))
                     (check-order (cdr imps) 'chez)
                     #t)) ;; Violation

                ;; local import
                (else
                  (if (eq? state 'chez)
                      #t ;; Violation: local after chez
                      (check-order (cdr imps) 'local))))))))))

  (severity warning)

  (message "Imports should be ordered: rnrs -> local -> chezscheme")

  (fix (lambda (bindings expr)
         ;; Reorder imports: rnrs -> local -> chezscheme
         (let* ((imports (get-binding bindings '?imports))
                (rnrs-imps '())
                (local-imps '())
                (chez-imps '()))

           ;; Helper to extract library name from import spec
           (define (get-lib-name imp)
             (if (cst-list? imp)
                 (let ((children (semantic-children imp)))
                   (if (and (pair? children) (cst-atom? (car children)))
                       (let* ((first (car children))
                              (sym (cst-atom-value first)))
                         ;; Handle (only (chezscheme) ...) etc
                         (if (memq sym '(only except prefix rename))
                             (let ((rest (cdr children)))
                               (if (and (pair? rest) (cst-list? (car rest)))
                                   (let ((inner (semantic-children (car rest))))
                                     (if (and (pair? inner) (cst-atom? (car inner)))
                                         (cst-atom-value (car inner))
                                         #f))
                                   #f))
                             sym))
                       #f))
                 #f))

           ;; Classify each import
           (let loop ((imps imports))
             (when (pair? imps)
               (let* ((imp (car imps))
                      (lib-name (get-lib-name imp))
                      (text (cst-node-text imp)))
                 (cond
                   ((and lib-name (symbol? lib-name)
                         (let ((s (symbol->string lib-name)))
                           (and (>= (string-length s) 4)
                                (string=? (substring s 0 4) "rnrs"))))
                    (set! rnrs-imps (append rnrs-imps (list text))))
                   ((eq? lib-name 'chezscheme)
                    (set! chez-imps (append chez-imps (list text))))
                   (else
                    (set! local-imps (append local-imps (list text)))))
                 (loop (cdr imps)))))

           ;; Reconstruct import statement with sorted order
           (let ((all-sorted (append rnrs-imps local-imps chez-imps)))
             (string-append "(import "
                           (let join ((items all-sorted))
                             (if (null? items)
                                 ""
                                 (if (null? (cdr items))
                                     (car items)
                                     (string-append (car items) "\n          " (join (cdr items))))))
                           ")"))))))
