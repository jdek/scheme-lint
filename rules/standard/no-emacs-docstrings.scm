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
         (if (not (cst-list? expr))
             #f
             (let* ((name-node (get-binding bindings '?name))
                    (params-nodes (get-binding bindings '?params))
                    (first-expr (get-binding bindings '?first-expr))
                    (rest-nodes (get-binding bindings '?rest))
                    (str-value (cst-string-value first-expr))
                    (column (cst-node-start-column expr)))

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

               ;; Build comment CST nodes with proper whitespace/indentation
               (define (build-comment-nodes lines)
                 (if (null? lines)
                     '()
                     (let* ((comment (make-comment-node (string-append ";; " (car lines))))
                            (remaining (cdr lines)))
                       (if (null? remaining)
                           (list comment)
                           (let ((newline (make-whitespace-node "\n"))
                                 (indent (if (> column 0)
                                            (make-whitespace-node (make-string column #\space))
                                            #f)))
                             (if indent
                                 (cons comment (cons newline (cons indent (build-comment-nodes remaining))))
                                 (cons comment (cons newline (build-comment-nodes remaining)))))))))

               ;; Build new define without docstring
               (define (build-new-define)
                 (let* ((old-children (cst-list-children expr))
                        ;; Filter out the docstring node and any trailing whitespace after it
                        (new-children
                         (let filter ((children old-children) (found-docstring #f))
                           (cond
                             ((null? children) '())
                             ;; Found the docstring - skip it and following whitespace
                             ((eq? (car children) first-expr)
                              (filter (cdr children) #t))
                             ;; Skip whitespace immediately after docstring
                             ((and found-docstring (cst-whitespace? (car children)))
                              (filter (cdr children) #f))
                             ;; Keep everything else
                             (else
                              (cons (car children) (filter (cdr children) #f)))))))
                   (make-cst-list 'list
                                 (cst-node-start-line expr)
                                 (cst-node-start-column expr)
                                 (cst-node-start-pos expr)
                                 (cst-node-end-pos expr)
                                 (cst-node-text expr)
                                 (cst-list-open-delim expr)
                                 new-children
                                 (cst-list-close-delim expr))))

               ;; Build replacement nodes: comments + newline + indent + define
               (let* ((comment-nodes (build-comment-nodes comment-lines))
                      (newline (make-whitespace-node "\n"))
                      (indent (if (> column 0)
                                 (make-whitespace-node (make-string column #\space))
                                 #f))
                      (new-define (build-new-define)))
                 (make-cst-transform 'replace
                                    (if indent
                                        (append comment-nodes (list newline indent new-define))
                                        (append comment-nodes (list newline new-define))))))))))
