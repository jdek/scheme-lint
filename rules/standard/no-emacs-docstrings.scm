;;=======================================================================
;; no-emacs-docstrings.scm - Detect invalid Emacs-style docstrings
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule no-emacs-docstrings
  (pattern (define (?name . ?params) ?first-expr . ?rest))

  (where
    ;; Check if first expression after params is a string literal
    ;; and there's more body after it (making the string useless)
    (let ((first (get-binding bindings '?first-expr))
          (rest (get-binding bindings '?rest)))
      (and first
           rest
           (pair? rest)  ; There's more body after the string
           (cst-string? first))))  ; CST: use cst-string? predicate

  (severity warning)

  (message "Docstrings are not valid R6RS syntax. Use ;; comments instead"))
