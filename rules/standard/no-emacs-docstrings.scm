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

  (message "Docstrings are not valid R6RS syntax. Use ;; comments instead")

  (fix (lambda (bindings expr)
         ;; Convert docstring to ;; comments above the define
         (let* ((name-node (get-binding bindings '?name))
                (params-nodes (get-binding bindings '?params))
                (first-expr (get-binding bindings '?first-expr))
                (rest-nodes (get-binding bindings '?rest))
                (str-value (cst-string-value first-expr))
                (expr-text (cst-node-text expr)))

           ;; Get indentation from CST node column position
           (define indent
             (make-string (cst-node-start-column expr) #\space))

           ;; Trim leading/trailing whitespace from a string
           (define (trim-whitespace str)
             (let* ((chars (string->list str))
                    ;; Trim leading
                    (trimmed-leading
                      (let loop ((cs chars))
                        (if (and (pair? cs)
                                 (or (char=? (car cs) #\space)
                                     (char=? (car cs) #\tab)))
                            (loop (cdr cs))
                            cs))))
               (list->string trimmed-leading)))

           ;; Split docstring into lines and convert to comments
           (define comment-lines
             (let loop ((chars (string->list str-value))
                        (current-line '())
                        (lines '()))
               (cond
                 ((null? chars)
                  (reverse (if (null? current-line)
                              lines
                              (cons (trim-whitespace (list->string (reverse current-line))) lines))))
                 ((char=? (car chars) #\newline)
                  (loop (cdr chars)
                        '()
                        (cons (trim-whitespace (list->string (reverse current-line))) lines)))
                 (else
                  (loop (cdr chars) (cons (car chars) current-line) lines)))))

           ;; Build comment block
           (define comment-block
             (let loop ((lines comment-lines))
               (if (null? lines)
                   ""
                   (string-append ";; " (car lines)
                                (if (null? (cdr lines))
                                    ""
                                    (string-append "\n" indent (loop (cdr lines))))))))

           ;; Reconstruct params
           (define params-text
             (if (null? params-nodes)
                 ""
                 (let loop ((ps params-nodes))
                   (if (null? ps)
                       ""
                       (string-append (cst-node-text (car ps))
                                    (if (null? (cdr ps))
                                        ""
                                        (string-append " " (loop (cdr ps)))))))))

           ;; Reconstruct body
           (define body-text
             (if (null? rest-nodes)
                 ""
                 (let loop ((rs rest-nodes))
                   (if (null? rs)
                       ""
                       (string-append (cst-node-text (car rs))
                                    (if (null? (cdr rs))
                                        ""
                                        (string-append " " (loop (cdr rs)))))))))

           ;; Build the final replacement string
           (string-append comment-block
                        "\n" indent "(define (" (cst-node-text name-node)
                        (if (string=? params-text "") "" (string-append " " params-text))
                        ")\n" indent "  "
                        body-text
                        ")")))))
