;;=======================================================================
;; no-unicode-in-comments.scm - Detect unicode in comments
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(rule no-unicode-in-comments
  (pattern ?any)

  (where
    ;; Only match comment nodes with unicode
    (and (cst-comment? expr)
         (let* ((text (cst-node-text expr))
                (has-unicode? #f))
           ;; Check each character in comment text
           (let loop ((i 0))
             (when (< i (string-length text))
               (let ((ch (string-ref text i)))
                 (when (> (char->integer ch) 127)
                   (set! has-unicode? #t)))
               (loop (+ i 1))))
           has-unicode?)))

  (severity warning)

  (message "Unicode characters not allowed in comments (e.g., right arrow should be ->)")

  (fix (lambda (bindings expr)
         (if (cst-comment? expr)
             (let ((text (cst-node-text expr)))
               ;; Replace unicode characters
               (let ((new-text
                      (call-with-string-output-port
                        (lambda (out)
                          (let loop ((i 0))
                            (when (< i (string-length text))
                              (let ((ch (string-ref text i)))
                                (cond
                                  ;; Right arrow
                                  ((= (char->integer ch) #x2192)
                                   (display "->" out))
                                  ;; Left arrow
                                  ((= (char->integer ch) #x2190)
                                   (display "<-" out))
                                  ;; Lambda
                                  ((= (char->integer ch) #x03BB)
                                   (display "lambda" out))
                                  ;; Any other unicode
                                  ((> (char->integer ch) 127)
                                   (display "?" out))
                                  ;; Regular character
                                  (else
                                   (display ch out)))
                                (loop (+ i 1)))))))))
                 (make-cst-transform 'replace
                                    (list (make-comment-node new-text)))))
             #f))))
