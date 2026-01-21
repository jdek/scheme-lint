;;=======================================================================
;; comment-heading-style.scm - Enforce comment heading style
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule comment-heading-style
  (pattern ?any)

  (where
    (let check-node ((node expr))
      (cond
        ;; Check if this is a comment with bad heading style
        ((cst-comment? node)
         (let ((text (cst-node-text node)))
           (and (>= (string-length text) 3)
                (char=? (string-ref text 0) #\;)
                (char=? (string-ref text 1) #\;)
                (char=? (string-ref text 2) #\;))))

        ;; Recursively check children for lists
        ((cst-list? node)
         (let check-children ((children (cst-list-children node)))
           (cond
             ((null? children) #f)
             ((check-node (car children)) #t)
             (else (check-children (cdr children))))))

        ;; Check vector children
        ((cst-vector? node)
         (let check-children ((children (cst-vector-children node)))
           (cond
             ((null? children) #f)
             ((check-node (car children)) #t)
             (else (check-children (cdr children))))))

        ;; Check quote expressions
        ((cst-quote? node)
         (check-node (cst-quote-expr node)))

        (else #f))))

  (severity warning)

  (message "Comment headings should use ;; (normal) or ;;======= (major section), not ;;;")

  (fix (lambda (bindings expr)
         ;; Return CST transformation
         (if (cst-comment? expr)
             (let* ((text (cst-node-text expr))
                    (column (cst-node-start-column expr)))

               ;; Extract heading text
               (define (extract-heading text)
                 (let ((after-semis
                        (let loop ((i 0))
                          (if (and (< i (string-length text))
                                  (char=? (string-ref text i) #\;))
                              (loop (+ i 1))
                              i))))
                   (let ((raw-text
                          (call-with-string-output-port
                            (lambda (out)
                              (let loop ((i after-semis))
                                (when (< i (string-length text))
                                  (let ((ch (string-ref text i)))
                                    (unless (or (char=? ch #\=)
                                               (and (char=? ch #\space)
                                                    (or (= i after-semis)
                                                        (let check-trailing ((j (+ i 1)))
                                                          (cond
                                                            ((>= j (string-length text)) #t)
                                                            ((char=? (string-ref text j) #\space)
                                                             (check-trailing (+ j 1)))
                                                            ((char=? (string-ref text j) #\=) #t)
                                                            (else #f))))))
                                      (display ch out)))
                                  (loop (+ i 1))))))))
                     (let ((len (string-length raw-text)))
                       (let trim-start ((i 0))
                         (if (and (< i len) (char=? (string-ref raw-text i) #\space))
                             (trim-start (+ i 1))
                             (let trim-end ((j (- len 1)))
                               (if (and (>= j i) (char=? (string-ref raw-text j) #\space))
                                   (trim-end (- j 1))
                                   (substring raw-text i (+ j 1))))))))))

               ;; Check if major section: has === pattern AND non-empty heading text
               (define (is-major? text heading-text)
                 (and (not (string=? heading-text ""))
                      (let loop ((i 0))
                        (cond
                          ((>= i (string-length text)) #f)
                          ((char=? (string-ref text i) #\=) #t)
                          (else (loop (+ i 1)))))))

               (let ((heading-text (extract-heading text)))
                 (if (string=? heading-text "")
                     ;; Empty heading or just === lines - simple comment
                     (make-cst-transform 'replace
                                        (list (make-comment-node ";;")))
                     (if (is-major? text heading-text)
                         ;; Major section: insert hrule before, replace heading
                         ;; Preserve indentation by adding whitespace after newline
                         (let* ((hrule-length (- 79 column))
                                (hrule (make-comment-node (string-append ";;" (make-string (- hrule-length 2) #\=))))
                                (newline (make-whitespace-node "\n"))
                                (indent (if (> column 0)
                                           (make-whitespace-node (make-string column #\space))
                                           #f))
                                (heading (make-comment-node (string-append ";; " heading-text))))
                           (make-cst-transform 'replace
                                              (if indent
                                                  (list hrule newline indent heading)
                                                  (list hrule newline heading))))
                         ;; Regular heading: just fix semicolons
                         (make-cst-transform 'replace
                                            (list (make-comment-node (string-append ";; " heading-text))))))))
             ;; Not a comment
             #f))))
