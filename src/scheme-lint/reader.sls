;;=============================================================================
;; reader.sls - Read s-expressions with source location tracking
;;=============================================================================
;; SPDX-License-Identifier: WTFPL

(library (scheme-lint reader)
  (export read-annotated
          annotated?
          annotated-expr
          annotated-source
          make-source-location
          source-location?
          source-location-file
          source-location-line
          source-location-column
          make-annotated)
  (import (rnrs)
          (rnrs mutable-pairs))

;;=============================================================================
;; Source Location

;; source-location : (file line column)
(define-record-type source-location
  (nongenerative)
  (fields file line column))

;;=============================================================================
;; Annotated Expression

;; Annotated s-expression with source location
(define-record-type annotated
  (nongenerative)
  (fields expr source))

;;=============================================================================
;; Reader with Location Tracking

;; read-annotated : input-port string => annotated
;;   effects: io/read
;;   Reads s-expression from port with source location tracking.
(define (read-annotated port filename)
  (let ((line 1)
        (column 0)
        (peek-char-loc #f))

    (define (advance-position ch)
      (if (char=? ch #\newline)
          (begin
            (set! line (+ line 1))
            (set! column 0))
          (set! column (+ column 1))))

    (define (get-char-tracked)
      (let ((ch (get-char port)))
        (when (not (eof-object? ch))
          (advance-position ch))
        ch))

    (define (peek-char-tracked)
      (peek-char port))

    (define (skip-whitespace)
      (let loop ()
        (let ((ch (peek-char-tracked)))
          (cond
            ((eof-object? ch) ch)
            ((char-whitespace? ch)
             (get-char-tracked)
             (loop))
            ((char=? ch #\;)
             (skip-line-comment)
             (loop))
            (else ch)))))

    (define (skip-line-comment)
      (let loop ()
        (let ((ch (get-char-tracked)))
          (when (not (or (eof-object? ch) (char=? ch #\newline)))
            (loop)))))

    (define (read-expr)
      (skip-whitespace)
      (let ((start-line line)
            (start-column column))
        (let ((ch (peek-char-tracked)))
          (cond
            ((eof-object? ch) ch)
            ((or (char=? ch #\() (char=? ch #\[))
             (read-list start-line start-column))
            ((char=? ch #\')
             (read-quoted 'quote start-line start-column))
            ((char=? ch #\`)
             (read-quoted 'quasiquote start-line start-column))
            ((char=? ch #\,)
             (get-char-tracked)
             (if (and (not (eof-object? (peek-char-tracked)))
                      (char=? (peek-char-tracked) #\@))
                 (begin
                   (get-char-tracked)
                   (read-quoted 'unquote-splicing start-line start-column))
                 (read-quoted 'unquote start-line start-column)))
            ((char=? ch #\")
             (read-string start-line start-column))
            ((char=? ch #\#)
             (read-hash start-line start-column))
            (else
             (read-atom start-line start-column))))))

    (define (read-list start-line start-column)
      (let ((open-char (get-char-tracked))) ;; consume ( or [
        (let loop ((elements '()))
          (skip-whitespace)
          (let ((ch (peek-char-tracked)))
            (cond
              ((eof-object? ch)
               (error 'read-annotated "unexpected EOF in list"))
              ((or (char=? ch #\)) (char=? ch #\]))
               (get-char-tracked)
               (make-annotated (reverse elements)
                              (make-source-location filename start-line start-column)))
              ((char=? ch #\.)
               ;; Check if this is ... (ellipsis) or . (dotted pair)
               (let ((dot-line line)
                     (dot-column column))
                 (get-char-tracked) ;; consume first dot
                 (let ((ch2 (peek-char-tracked)))
                   (if (and (char? ch2) (char=? ch2 #\.))
                       ;; Might be ... - check for third dot
                       (begin
                         (get-char-tracked) ;; consume second dot
                         (let ((ch3 (peek-char-tracked)))
                           (if (and (char? ch3) (char=? ch3 #\.))
                               ;; Yes, it's ... - read as symbol
                               (begin
                                 (get-char-tracked) ;; consume third dot
                                 (loop (cons (make-annotated '...
                                                            (make-source-location filename dot-line dot-column))
                                            elements)))
                               ;; No, it's .. followed by something else - error
                               (error 'read-annotated "unexpected .. in list"))))
                       ;; Not .., so treat as dotted pair
                       (let ((rest (read-expr)))
                         (skip-whitespace)
                         (let ((close (get-char-tracked)))
                           (when (not (or (char=? close #\)) (char=? close #\])))
                             (error 'read-annotated "expected ) or ] after dotted pair"))
                           ;; Build proper dotted pair: (a b c . d) = (a . (b . (c . d)))
                           (let* ((rest-expr (annotated-expr rest))
                                  (dotted-list
                                    (let build ((elems (reverse elements)))
                                      (if (null? elems)
                                          rest-expr
                                          (cons (annotated-expr (car elems))
                                               (build (cdr elems)))))))
                             (make-annotated dotted-list
                                            (make-source-location filename start-line start-column)))))))))
              (else
               (loop (cons (read-expr) elements))))))))

    (define (read-quoted quote-type start-line start-column)
      (when (memq quote-type '(quote quasiquote))
        (get-char-tracked)) ;; consume quote char for ' and `
      (let ((expr (read-expr)))
        (make-annotated (list quote-type expr)
                       (make-source-location filename start-line start-column))))

    (define (read-string start-line start-column)
      (get-char-tracked) ;; consume opening "
      (let loop ((chars '()))
        (let ((ch (get-char-tracked)))
          (cond
            ((eof-object? ch)
             (error 'read-annotated "unexpected EOF in string"))
            ((char=? ch #\")
             (make-annotated (list->string (reverse chars))
                            (make-source-location filename start-line start-column)))
            ((char=? ch #\\)
             (let ((next (get-char-tracked)))
               (if (eof-object? next)
                   (error 'read-annotated "unexpected EOF after backslash")
                   (loop (cons (case next
                                ((#\n) #\newline)
                                ((#\t) #\tab)
                                ((#\r) #\return)
                                (else next))
                              chars)))))
            (else
             (loop (cons ch chars)))))))

    (define (read-hash start-line start-column)
      (get-char-tracked) ;; consume #
      (let ((ch (peek-char-tracked)))
        (cond
          ((eof-object? ch)
           (error 'read-annotated "unexpected EOF after #"))
          ((char=? ch #\!)
           ;; Shebang or directive - skip rest of line and read next expression
           (get-char-tracked) ;; consume !
           (let skip-rest ()
             (let ((c (get-char-tracked)))
               (when (not (or (eof-object? c) (char=? c #\newline)))
                 (skip-rest))))
           (read-expr))
          ((char=? ch #\t)
           (get-char-tracked)
           (make-annotated #t (make-source-location filename start-line start-column)))
          ((char=? ch #\f)
           (get-char-tracked)
           (make-annotated #f (make-source-location filename start-line start-column)))
          ((char=? ch #\()
           (read-vector start-line start-column))
          ((char=? ch #\;)
           (get-char-tracked) ;; datum comment
           (read-expr) ;; skip commented expression
           (read-expr)) ;; read next expression
          ((char=? ch #\\)
           (read-character start-line start-column))
          ((or (char=? ch #\x) (char=? ch #\b) (char=? ch #\o) (char=? ch #\d))
           ;; Hex, binary, octal, or decimal number
           (read-number-with-radix start-line start-column))
          ((char=? ch #\')
           ;; #' is syntax quote - read as (syntax ...)
           (get-char-tracked) ;; consume '
           (let ((expr (read-expr)))
             (make-annotated (list 'syntax (annotated-expr expr))
                            (make-source-location filename start-line start-column))))
          ((char=? ch #\`)
           ;; #` is quasisyntax - read as (quasisyntax ...)
           (get-char-tracked) ;; consume `
           (let ((expr (read-expr)))
             (make-annotated (list 'quasisyntax (annotated-expr expr))
                            (make-source-location filename start-line start-column))))
          ((char=? ch #\,)
           ;; #, is unsyntax - read as (unsyntax ...)
           (get-char-tracked) ;; consume ,
           (let ((next-ch (peek-char-tracked)))
             (if (and (not (eof-object? next-ch)) (char=? next-ch #\@))
                 (begin
                   (get-char-tracked) ;; consume @
                   (let ((expr (read-expr)))
                     (make-annotated (list 'unsyntax-splicing (annotated-expr expr))
                                    (make-source-location filename start-line start-column))))
                 (let ((expr (read-expr)))
                   (make-annotated (list 'unsyntax (annotated-expr expr))
                                  (make-source-location filename start-line start-column))))))
          (else
           (error 'read-annotated "unsupported # syntax" ch)))))

    (define (read-vector start-line start-column)
      (get-char-tracked) ;; consume (
      (let loop ((elements '()))
        (skip-whitespace)
        (let ((ch (peek-char-tracked)))
          (cond
            ((eof-object? ch)
             (error 'read-annotated "unexpected EOF in vector"))
            ((char=? ch #\))
             (get-char-tracked)
             (make-annotated (list->vector (reverse elements))
                            (make-source-location filename start-line start-column)))
            (else
             (loop (cons (read-expr) elements)))))))

    (define (read-atom start-line start-column)
      (let loop ((chars '()))
        (let ((ch (peek-char-tracked)))
          (if (or (eof-object? ch)
                  (char-whitespace? ch)
                  (memv ch '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\;)))
              (let ((str (list->string (reverse chars))))
                (make-annotated (or (string->number str)
                                   (string->symbol str))
                               (make-source-location filename start-line start-column)))
              (begin
                (get-char-tracked)
                (loop (cons ch chars)))))))

    (define (read-character start-line start-column)
      (get-char-tracked) ;; consume backslash
      (let ((ch (get-char-tracked)))
        (if (eof-object? ch)
            (error 'read-annotated "unexpected EOF after #\\")
            ;; Check for named characters
            (let ((rest-chars '()))
              (let gather ()
                (let ((next (peek-char-tracked)))
                  (if (and (not (eof-object? next))
                           (not (char-whitespace? next))
                           (not (memv next '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\;))))
                      (begin
                        (set! rest-chars (cons (get-char-tracked) rest-chars))
                        (gather)))))
              (let ((char-name (list->string (cons ch (reverse rest-chars)))))
                (make-annotated
                  (cond
                    ;; R6RS standard character names
                    ((string=? char-name "nul") #\nul)
                    ((string=? char-name "alarm") #\alarm)
                    ((string=? char-name "backspace") #\backspace)
                    ((string=? char-name "tab") #\tab)
                    ((string=? char-name "linefeed") #\linefeed)
                    ((string=? char-name "newline") #\newline)
                    ((string=? char-name "vtab") #\vtab)
                    ((string=? char-name "page") #\page)
                    ((string=? char-name "return") #\return)
                    ((string=? char-name "esc") #\esc)
                    ((string=? char-name "space") #\space)
                    ((string=? char-name "delete") #\delete)
                    ;; Single character
                    ((= (string-length char-name) 1) (string-ref char-name 0))
                    (else (error 'read-annotated "invalid character name" char-name)))
                  (make-source-location filename start-line start-column)))))))

    (define (read-number-with-radix start-line start-column)
      (let ((radix-char (get-char-tracked))) ;; consume x/b/o/d
        ;; Read digits until delimiter
        (let loop ((chars '()))
          (let ((ch (peek-char-tracked)))
            (if (or (eof-object? ch)
                    (char-whitespace? ch)
                    (memv ch '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\;)))
                (let* ((digits (list->string (reverse chars)))
                       (radix (case radix-char
                               ((#\x) 16)
                               ((#\b) 2)
                               ((#\o) 8)
                               ((#\d) 10)
                               (else 10)))
                       (num (string->number digits radix)))
                  (if num
                      (make-annotated num (make-source-location filename start-line start-column))
                      (error 'read-annotated "invalid number literal" digits)))
                (begin
                  (get-char-tracked)
                  (loop (cons ch chars))))))))

    (read-expr)))

) ;; end library
