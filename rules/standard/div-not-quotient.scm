;;=======================================================================
;; div-not-quotient.scm - Prefer div/mod over quotient/modulo
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule div-not-quotient
  (pattern (quotient ?a ?b))
  (where #t)
  (severity style)
  (message "Use 'div' instead of 'quotient' (preferred in R6RS)")
  (fix (lambda (bindings expr)
         ;; Replace quotient with div - simple string replacement
         (let ((text (cst-node-text expr)))
           ;; Replace first occurrence of "quotient" with "div"
           (let ((idx (string-contains text "quotient")))
             (if idx
                 (string-append (substring text 0 idx)
                               "div"
                               (substring text (+ idx 8) (string-length text)))
                 text))))))

(rule mod-not-modulo
  (pattern (modulo ?a ?b))
  (where #t)
  (severity style)
  (message "Use 'mod' instead of 'modulo' (preferred in R6RS)")
  (fix (lambda (bindings expr)
         ;; Replace modulo with mod - simple string replacement
         (let ((text (cst-node-text expr)))
           ;; Replace first occurrence of "modulo" with "mod"
           (let ((idx (string-contains text "modulo")))
             (if idx
                 (string-append (substring text 0 idx)
                               "mod"
                               (substring text (+ idx 6) (string-length text)))
                 text))))))
