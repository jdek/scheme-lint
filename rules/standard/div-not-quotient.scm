;;=======================================================================
;; div-not-quotient.scm - Prefer div/mod over quotient/modulo
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule div-not-quotient
  (pattern (quotient ?a ?b))
  (where #t)
  (severity warning)
  (message "Use 'div' instead of 'quotient' (preferred in R6RS)")
  (fix (lambda (bindings expr)
         ;; Replace quotient with div by reconstructing the list
         (if (cst-list? expr)
             (let* ((children (cst-list-children expr))
                    ;; Find and replace the quotient atom
                    (new-children
                     (map (lambda (child)
                            (if (and (cst-atom? child)
                                    (eq? (cst-atom-value child) 'quotient))
                                (make-cst-atom 'atom
                                              (cst-node-start-line child)
                                              (cst-node-start-column child)
                                              (cst-node-start-pos child)
                                              (cst-node-end-pos child)
                                              "div"
                                              'div)
                                child))
                          children)))
               (make-cst-transform 'replace
                                  (list (make-cst-list 'list
                                                      (cst-node-start-line expr)
                                                      (cst-node-start-column expr)
                                                      (cst-node-start-pos expr)
                                                      (cst-node-end-pos expr)
                                                      (cst-node-text expr)
                                                      (cst-list-open-delim expr)
                                                      new-children
                                                      (cst-list-close-delim expr)))))
             #f))))

(rule mod-not-modulo
  (pattern (modulo ?a ?b))
  (where #t)
  (severity warning)
  (message "Use 'mod' instead of 'modulo' (preferred in R6RS)")
  (fix (lambda (bindings expr)
         ;; Replace modulo with mod by reconstructing the list
         (if (cst-list? expr)
             (let* ((children (cst-list-children expr))
                    ;; Find and replace the modulo atom
                    (new-children
                     (map (lambda (child)
                            (if (and (cst-atom? child)
                                    (eq? (cst-atom-value child) 'modulo))
                                (make-cst-atom 'atom
                                              (cst-node-start-line child)
                                              (cst-node-start-column child)
                                              (cst-node-start-pos child)
                                              (cst-node-end-pos child)
                                              "mod"
                                              'mod)
                                child))
                          children)))
               (make-cst-transform 'replace
                                  (list (make-cst-list 'list
                                                      (cst-node-start-line expr)
                                                      (cst-node-start-column expr)
                                                      (cst-node-start-pos expr)
                                                      (cst-node-end-pos expr)
                                                      (cst-node-text expr)
                                                      (cst-list-open-delim expr)
                                                      new-children
                                                      (cst-list-close-delim expr)))))
             #f))))
