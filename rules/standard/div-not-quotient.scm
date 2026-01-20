;;=======================================================================
;; div-not-quotient.scm - Prefer div/mod over quotient/modulo
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule div-not-quotient
  (pattern (quotient ?a ?b))
  (where #t)
  (severity style)
  (message "Use 'div' instead of 'quotient' (preferred in R6RS)"))

(rule mod-not-modulo
  (pattern (modulo ?a ?b))
  (where #t)
  (severity style)
  (message "Use 'mod' instead of 'modulo' (preferred in R6RS)"))
