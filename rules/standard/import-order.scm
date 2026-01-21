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
          (let ((imp (car imps)))
            ;; Extract library name from import spec
            (let ((lib-name
                    (if (cst-list? imp)
                        (let ((children (semantic-children imp)))
                          (if (pair? children)
                              (let ((first (car children)))
                                (if (cst-atom? first)
                                    (let ((sym (cst-atom-value first)))
                                      ;; Handle (only (chezscheme) ...) etc
                                      (if (memq sym '(only except prefix rename))
                                          (if (pair? (cdr children))
                                              (let ((second (cadr children)))
                                                (if (cst-list? second)
                                                    (let ((inner (semantic-children second)))
                                                      (if (and (pair? inner) (cst-atom? (car inner)))
                                                          (cst-atom-value (car inner))
                                                          #f))
                                                    #f))
                                              #f)
                                          sym))
                                    #f))
                              #f))
                        #f)))
              (cond
                ;; rnrs import
                ((and lib-name
                      (symbol? lib-name)
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
                     #t)) ;; Violation

                ;; local import
                (else
                  (if (eq? state 'chez)
                      #t ;; Violation: local after chez
                      (check-order (cdr imps) 'local))))))))))

  (severity warning)

  (message "Imports should be ordered: rnrs -> local -> chezscheme")

  (fix (lambda (bindings expr)
         ;; Extract library name from an import spec
         (define (extract-lib-name imp)
           (if (cst-list? imp)
               (let ((children (semantic-children imp)))
                 (if (pair? children)
                     (let ((first (car children)))
                       (if (cst-atom? first)
                           (let ((sym (cst-atom-value first)))
                             ;; Handle (only (chezscheme) ...) etc
                             (if (memq sym '(only except prefix rename))
                                 (if (pair? (cdr children))
                                     (let ((second (cadr children)))
                                       (if (cst-list? second)
                                           (let ((inner (semantic-children second)))
                                             (if (and (pair? inner) (cst-atom? (car inner)))
                                                 (cst-atom-value (car inner))
                                                 #f))
                                           #f))
                                     #f)
                                 sym))
                           #f))
                     #f))
               #f))

         ;; Classify import as 'rnrs, 'chez, or 'local
         (define (classify-import imp)
           (let ((lib-name (extract-lib-name imp)))
             (cond
               ((and lib-name
                     (symbol? lib-name)
                     (let ((s (symbol->string lib-name)))
                       (and (>= (string-length s) 4)
                            (string=? (substring s 0 4) "rnrs"))))
                'rnrs)
               ((eq? lib-name 'chezscheme) 'chez)
               (else 'local))))

         ;; Sort imports into three groups
         (let* ((imports (get-binding bindings '?imports))
                (rnrs-imps '())
                (local-imps '())
                (chez-imps '()))

           ;; Classify all imports
           (let loop ((imps imports))
             (when (pair? imps)
               (let* ((imp (car imps))
                      (class (classify-import imp))
                      (text (cst-node-text imp)))
                 (case class
                   ((rnrs) (set! rnrs-imps (append rnrs-imps (list text))))
                   ((chez) (set! chez-imps (append chez-imps (list text))))
                   ((local) (set! local-imps (append local-imps (list text)))))
                 (loop (cdr imps)))))

           ;; Reconstruct import statement
           (let ((all-sorted (append rnrs-imps local-imps chez-imps)))
             (string-append "(import "
                           (let join ((items all-sorted))
                             (if (null? items)
                                 ""
                                 (if (null? (cdr items))
                                     (car items)
                                     (string-append (car items) "\n          " (join (cdr items))))))
                           ")"))))))
