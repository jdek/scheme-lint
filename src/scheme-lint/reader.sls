;;=============================================================================
;; reader.sls - Read s-expressions as Concrete Syntax Tree
;;=============================================================================
;; SPDX-License-Identifier: WTFPL
;;
;; CST (Concrete Syntax Tree) reader that preserves ALL source information:
;; - Exact character positions
;; - All whitespace (spaces, tabs, newlines)
;; - All comments (line, datum, block)
;; - Original source text for every node
;;
;; This enables:
;; - High-fidelity auto-fix (preserve formatting)
;; - Formatting rules (indentation, line length)
;; - Comment linting (docstrings, TODO format)
;; - Whitespace rules (trailing spaces, blank lines)

(library (scheme-lint reader)
  (export
    ;; Reading
    read-cst
    read-all-cst

    ;; Base CST node
    cst-node?
    cst-node-kind
    cst-node-start-line
    cst-node-start-column
    cst-node-start-pos
    cst-node-end-pos
    cst-node-text

    ;; List nodes
    cst-list?
    make-cst-list
    cst-list-open-delim
    cst-list-children
    cst-list-close-delim

    ;; Atom nodes
    cst-atom?
    make-cst-atom
    cst-atom-value

    ;; String nodes
    cst-string?
    make-cst-string
    cst-string-value

    ;; Comment nodes
    cst-comment?
    make-cst-comment
    cst-comment-text
    cst-comment-style

    ;; Whitespace nodes
    cst-whitespace?
    make-cst-whitespace
    cst-whitespace-text
    cst-whitespace-contains-newline?

    ;; Quote nodes
    cst-quote?
    make-cst-quote
    cst-quote-type
    cst-quote-prefix
    cst-quote-expr

    ;; Vector nodes
    cst-vector?
    make-cst-vector
    cst-vector-children

    ;; Bytevector nodes
    cst-bytevector?
    make-cst-bytevector
    cst-bytevector-children

    ;; Helpers
    trivia-node?
    semantic-children
    cst->sexp
    make-source-location
    source-location?
    source-location-file
    source-location-line
    source-location-column)

  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs records syntactic)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs unicode)
          (rnrs exceptions)
          (rnrs bytevectors))

;;=============================================================================
;; Source Location Record

(define-record-type source-location
  (nongenerative)
  (fields file line column))

;;=============================================================================
;; CST Node Types

;; Base node (not instantiated directly)
(define-record-type cst-node
  (nongenerative)
  (fields kind              ; Symbol: type of node
          start-line        ; Integer: 1-indexed
          start-column      ; Integer: 0-indexed
          start-pos         ; Integer: absolute char position
          end-pos           ; Integer: absolute char position
          text))            ; String: original source text

;; List: (...)  [...]
(define-record-type (cst-list make-cst-list cst-list?)
  (parent cst-node)
  (nongenerative)
  (fields open-delim        ; Char: #\( or #\[
          children          ; List of cst-node (includes trivia)
          close-delim))     ; Char: #\) or #\]

;; Atom: symbols, numbers, booleans, characters
(define-record-type (cst-atom make-cst-atom cst-atom?)
  (parent cst-node)
  (nongenerative)
  (fields value))           ; Symbol | Number | Boolean | Char

;; String: "..."
(define-record-type (cst-string make-cst-string cst-string?)
  (parent cst-node)
  (nongenerative)
  (fields value))           ; String: interpreted value

;; Comment: ; text
(define-record-type (cst-comment make-cst-comment cst-comment?)
  (parent cst-node)
  (nongenerative)
  (fields text              ; String: comment content
          style))           ; Symbol: 'line | 'datum

;; Whitespace: spaces, tabs, newlines
(define-record-type (cst-whitespace make-cst-whitespace cst-whitespace?)
  (parent cst-node)
  (nongenerative)
  (fields text              ; String: the whitespace
          contains-newline?)) ; Boolean

;; Quote: 'x `x ,x ,@x #'x #`x #,x #,@x
(define-record-type (cst-quote make-cst-quote cst-quote?)
  (parent cst-node)
  (nongenerative)
  (fields type              ; Symbol: 'quote | 'quasiquote | etc.
          prefix            ; String: "'" "`" "," etc.
          expr))            ; cst-node: quoted expression

;; Vector: #(...)
(define-record-type (cst-vector make-cst-vector cst-vector?)
  (parent cst-node)
  (nongenerative)
  (fields children))        ; List of cst-node (includes trivia)

(define-record-type (cst-bytevector make-cst-bytevector cst-bytevector?)
  (parent cst-node)
  (nongenerative)
  (fields children))        ; List of cst-node (includes trivia)

;;=============================================================================
;; Helpers

;; Check if node is trivia (whitespace or comment)
(define (trivia-node? node)
  (or (cst-whitespace? node)
      (cst-comment? node)))

;; Get semantic children (excluding trivia)
(define (semantic-children node)
  (cond
    ((cst-list? node)
     (filter (lambda (child) (not (trivia-node? child)))
             (cst-list-children node)))
    ((cst-vector? node)
     (filter (lambda (child) (not (trivia-node? child)))
             (cst-vector-children node)))
    ((cst-bytevector? node)
     (filter (lambda (child) (not (trivia-node? child)))
             (cst-bytevector-children node)))
    (else '())))

;; Convert CST to S-expression (loses trivia and positions)
(define (cst->sexp node)
  (cond
    ((cst-list? node)
     (map cst->sexp (semantic-children node)))

    ((cst-atom? node)
     (cst-atom-value node))

    ((cst-string? node)
     (cst-string-value node))

    ((cst-quote? node)
     (list (cst-quote-type node)
           (cst->sexp (cst-quote-expr node))))

    ((cst-vector? node)
     (list->vector (map cst->sexp (semantic-children node))))

    ((cst-bytevector? node)
     (u8-list->bytevector (map cst->sexp (semantic-children node))))

    ((trivia-node? node)
     #f)  ; Skip trivia

    (else
     (error 'cst->sexp "Unknown CST node type" node))))

;; Note: source-location extraction is done by accessing CST node fields directly

;;=============================================================================
;; CST Reader

;; read-cst : Input-Port String -> cst-node | eof-object
;;   Read one CST from port
(define (read-cst port filename)
  (let ((reader (make-reader port filename)))
    (reader)))

;; read-all-cst : Input-Port String -> List[cst-node]
;;   Read all CSTs from port
(define (read-all-cst port filename)
  (let ((reader (make-reader port filename)))
    (let loop ((nodes '()))
      (let ((node (reader)))
        (if (eof-object? node)
            (reverse nodes)
            (loop (cons node nodes)))))))

;; make-reader : Input-Port String -> (-> cst-node | eof)
;;   Create a stateful reader closure
(define (make-reader port filename)
  (let ((line 1)
        (column 0)
        (char-pos 0)
        (source-text '()))  ; Accumulate source text

    ;; Character tracking
    (define (get-char-tracked)
      (let ((ch (get-char port)))
        (when (not (eof-object? ch))
          (set! source-text (cons ch source-text))
          (set! char-pos (+ char-pos 1))
          (if (char=? ch #\newline)
              (begin
                (set! line (+ line 1))
                (set! column 0))
              (set! column (+ column 1))))
        ch))

    (define (peek-char-tracked)
      (lookahead-char port))

    ;; Extract text from source-text accumulator
    (define (extract-text start-pos end-pos)
      (let ((len (- end-pos start-pos)))
        (list->string (reverse (take (drop source-text
                                           (- (length source-text) end-pos))
                                     len)))))

    ;; Main expression reader
    (define (read-expr)
      (let* ((start-line line)
             (start-column column)
             (start-pos char-pos)
             (ch (peek-char-tracked)))
        (cond
          ((eof-object? ch) ch)

          ;; Whitespace
          ((char-whitespace? ch)
           (read-whitespace start-line start-column start-pos))

          ;; Comment
          ((char=? ch #\;)
           (read-line-comment start-line start-column start-pos))

          ;; List
          ((or (char=? ch #\() (char=? ch #\[))
           (read-list start-line start-column start-pos))

          ;; Quote forms
          ((char=? ch #\')
           (read-quote 'quote "'" start-line start-column start-pos))

          ((char=? ch #\`)
           (read-quote 'quasiquote "`" start-line start-column start-pos))

          ((char=? ch #\,)
           (read-unquote start-line start-column start-pos))

          ;; String
          ((char=? ch #\")
           (read-string start-line start-column start-pos))

          ;; Hash literals
          ((char=? ch #\#)
           (read-hash start-line start-column start-pos))

          ;; Atom (symbol, number, etc.)
          (else
           (read-atom start-line start-column start-pos)))))

    ;; Whitespace reader
    (define (read-whitespace start-line start-column start-pos)
      (let loop ((chars '()))
        (let ((ch (peek-char-tracked)))
          (if (and (not (eof-object? ch)) (char-whitespace? ch))
              (begin
                (get-char-tracked)
                (loop (cons ch chars)))
              (let* ((end-pos char-pos)
                     (text (list->string (reverse chars)))
                     (has-newline? (exists (lambda (c) (char=? c #\newline)) chars)))
                (make-cst-whitespace 'whitespace
                                    start-line start-column
                                    start-pos end-pos
                                    text
                                    text has-newline?))))))

    ;; Comment reader
    (define (read-line-comment start-line start-column start-pos)
      (get-char-tracked)  ; consume ;
      (let loop ((chars '()))
        (let ((ch (peek-char-tracked)))
          (if (or (eof-object? ch) (char=? ch #\newline))
              (let* ((end-pos char-pos)
                     (comment-text (list->string (reverse chars)))
                     (full-text (string-append ";" comment-text)))
                (make-cst-comment 'comment
                                 start-line start-column
                                 start-pos end-pos
                                 full-text
                                 comment-text 'line))
              (begin
                (get-char-tracked)
                (loop (cons ch chars)))))))

    ;; List reader
    (define (read-list start-line start-column start-pos)
      (let ((open-delim (get-char-tracked)))  ; ( or [
        (let loop ((children '()))
          (let* ((ch (peek-char-tracked)))
            (cond
              ((eof-object? ch)
               (error 'read-cst "unexpected EOF in list" filename start-line))

              ;; Closing delimiter
              ((or (char=? ch #\)) (char=? ch #\]))
               (get-char-tracked)
               (let* ((end-pos char-pos)
                      (text (extract-text start-pos end-pos)))
                 (make-cst-list 'list
                               start-line start-column
                               start-pos end-pos
                               text
                               open-delim (reverse children) ch)))

              ;; Dotted pair or atom starting with dot
              ((char=? ch #\.)
               ;; Peek ahead to see if it's a dotted pair (. followed by whitespace/delimiter)
               ;; or an atom like ... or .foo
               (let ((dot-start-line line)
                     (dot-start-col column)
                     (dot-start-pos char-pos))
                 (get-char-tracked)  ; consume .
                 (let ((ch2 (peek-char-tracked)))
                   (cond
                     ;; . followed by whitespace or close paren = dotted pair
                     ((or (eof-object? ch2)
                          (char-whitespace? ch2)
                          (char=? ch2 #\))
                          (char=? ch2 #\]))
                      ;; Put back the consumed state and treat as dotted pair
                      ;; But first create dot atom to include in children
                      (let ((dot-atom (make-cst-atom 'atom
                                                    dot-start-line dot-start-col
                                                    dot-start-pos char-pos
                                                    "." (string->symbol "."))))
                        (read-dotted-tail-with-dot start-line start-column start-pos open-delim
                                                  (cons dot-atom children))))
                     ;; Otherwise it's part of an atom, go back and read normally
                     (else
                      ;; Can't easily unget, so we need to handle this in read-atom
                      ;; For now, create a dot atom and continue reading
                      (let ((dot-atom (make-cst-atom 'atom
                                                    dot-start-line dot-start-col
                                                    dot-start-pos char-pos
                                                    "." (string->symbol "."))))
                        (loop (cons dot-atom children))))))))

              ;; Regular element
              (else
               (loop (cons (read-expr) children))))))))

    ;; Dotted pair tail with dot already in children
    (define (read-dotted-tail-with-dot start-line start-column start-pos open-delim children)
      ;; Children already includes the dot atom, now read whitespace/tail/close
      (let loop-children ((children children))
        (let ((ch (peek-char-tracked)))
          (cond
            ((eof-object? ch)
             (error 'read-cst "unexpected EOF after dot"))
            ((char-whitespace? ch)
             (loop-children (cons (read-expr) children)))  ; Include whitespace
            ((char=? ch #\;)
             (loop-children (cons (read-expr) children)))  ; Include comments
            ((or (char=? ch #\)) (char=? ch #\]))
             ;; Reached end without tail - just a dot atom
             (get-char-tracked)
             (let* ((end-pos char-pos)
                    (text (extract-text start-pos end-pos)))
               (make-cst-list 'list
                            start-line start-column
                            start-pos end-pos
                            text
                            open-delim (reverse children) ch)))
            (else
             ;; Read tail element and continue to close paren
             (let ((tail (read-expr)))
               (let loop-rest ((children (cons tail children)))
                 (let ((ch (peek-char-tracked)))
                   (cond
                     ((eof-object? ch)
                      (error 'read-cst "unexpected EOF after dotted tail"))
                     ((char-whitespace? ch)
                      (loop-rest (cons (read-expr) children)))
                     ((char=? ch #\;)
                      (loop-rest (cons (read-expr) children)))
                     ((or (char=? ch #\)) (char=? ch #\]))
                      (get-char-tracked)
                      (let* ((end-pos char-pos)
                             (text (extract-text start-pos end-pos)))
                        (make-cst-list 'list
                                     start-line start-column
                                     start-pos end-pos
                                     text
                                     open-delim (reverse children) ch)))
                     (else
                      (error 'read-cst "expected closing delimiter after dotted tail")))))))))))

    ;; Dotted pair tail (LEGACY - now using read-dotted-tail-with-dot)
    (define (read-dotted-tail start-line start-column start-pos open-delim children)
      (let ((dot-line line)
            (dot-col column))
        (get-char-tracked)  ; consume .
        (let ((ch2 (peek-char-tracked)))
          (cond
            ;; Check for ... (ellipsis)
            ((and (char? ch2) (char=? ch2 #\.))
             (get-char-tracked)
             (let ((ch3 (peek-char-tracked)))
               (if (and (char? ch3) (char=? ch3 #\.))
                   ;; It's ... - read as atom
                   (begin
                     (get-char-tracked)
                     (let* ((end-pos char-pos)
                            (text "..."))
                       (read-list-continue start-line start-column start-pos open-delim
                                         (cons (make-cst-atom 'atom
                                                             dot-line dot-col
                                                             (- char-pos 3) end-pos
                                                             text '...)
                                               children))))
                   ;; Error: .. followed by non-.
                   (error 'read-cst "invalid .. in list"))))

            ;; Regular dot - skip whitespace/comments, read tail
            (else
             ;; Skip trivia after dot
             (let skip-trivia ()
               (let ((ch (peek-char-tracked)))
                 (cond
                   ((eof-object? ch)
                    (error 'read-cst "unexpected EOF after dot"))
                   ((char-whitespace? ch)
                    (read-expr)  ; read and discard whitespace
                    (skip-trivia))
                   ((char=? ch #\;)
                    (read-expr)  ; read and discard comment
                    (skip-trivia))
                   (else
                    ;; Read the tail element
                    (let ((tail (read-expr)))
                      ;; Skip more trivia
                      (let skip-more ()
                        (let ((ch (peek-char-tracked)))
                          (cond
                            ((eof-object? ch)
                             (error 'read-cst "unexpected EOF after dotted tail"))
                            ((char-whitespace? ch)
                             (read-expr)
                             (skip-more))
                            ((char=? ch #\;)
                             (read-expr)
                             (skip-more))
                            ((or (char=? ch #\)) (char=? ch #\]))
                             (get-char-tracked)
                             (let* ((end-pos char-pos)
                                    (text (extract-text start-pos end-pos)))
                               ;; Build dotted pair CST
                               ;; Note: This is tricky - we represent (a . b) as a list
                               ;; with special marker
                               (make-cst-list 'list
                                            start-line start-column
                                            start-pos end-pos
                                            text
                                            open-delim
                                            (append (reverse children) (list tail))
                                            ch)))
                            (else
                             (error 'read-cst "expected ) or ] after dotted tail")))))))))))))))

    (define (read-list-continue start-line start-column start-pos open-delim children)
      (let loop ((children children))
        (let ((ch (peek-char-tracked)))
          (cond
            ((eof-object? ch)
             (error 'read-cst "unexpected EOF in list"))

            ((or (char=? ch #\)) (char=? ch #\]))
             (get-char-tracked)
             (let* ((end-pos char-pos)
                    (text (extract-text start-pos end-pos)))
               (make-cst-list 'list
                             start-line start-column
                             start-pos end-pos
                             text
                             open-delim (reverse children) ch)))

            (else
             (loop (cons (read-expr) children)))))))

    ;; Quote reader
    (define (read-quote quote-type prefix start-line start-column start-pos)
      (get-char-tracked)  ; consume quote char
      (let ((expr (read-expr)))
        (when (eof-object? expr)
          (error 'read-cst "unexpected EOF after quote"))
        (let* ((end-pos char-pos)
               (text (extract-text start-pos end-pos)))
          (make-cst-quote 'quote
                         start-line start-column
                         start-pos end-pos
                         text
                         quote-type prefix expr))))

    ;; Unquote reader (handles both , and ,@)
    (define (read-unquote start-line start-column start-pos)
      (get-char-tracked)  ; consume ,
      (let ((ch (peek-char-tracked)))
        (if (and (not (eof-object? ch)) (char=? ch #\@))
            (begin
              (get-char-tracked)
              (let ((expr (read-expr)))
                (when (eof-object? expr)
                  (error 'read-cst "unexpected EOF after unquote-splicing"))
                (let* ((end-pos char-pos)
                       (text (extract-text start-pos end-pos)))
                  (make-cst-quote 'quote
                                 start-line start-column
                                 start-pos end-pos
                                 text
                                 'unquote-splicing ",@" expr))))
            (let ((expr (read-expr)))
              (when (eof-object? expr)
                (error 'read-cst "unexpected EOF after unquote"))
              (let* ((end-pos char-pos)
                     (text (extract-text start-pos end-pos)))
                (make-cst-quote 'quote
                               start-line start-column
                               start-pos end-pos
                               text
                               'unquote "," expr))))))

    ;; String reader
    (define (read-string start-line start-column start-pos)
      (get-char-tracked)  ; consume opening "
      (let loop ((chars '()))
        (let ((ch (get-char-tracked)))
          (cond
            ((eof-object? ch)
             (error 'read-cst "unexpected EOF in string"))

            ((char=? ch #\")
             (let* ((end-pos char-pos)
                    (str-value (list->string (reverse chars)))
                    (text (extract-text start-pos end-pos)))
               (make-cst-string 'string
                               start-line start-column
                               start-pos end-pos
                               text
                               str-value)))

            ((char=? ch #\\)
             (let ((next (get-char-tracked)))
               (if (eof-object? next)
                   (error 'read-cst "unexpected EOF after backslash")
                   (loop (cons (case next
                                ((#\n) #\newline)
                                ((#\t) #\tab)
                                ((#\r) #\return)
                                (else next))
                              chars)))))

            (else
             (loop (cons ch chars)))))))

    ;; Hash reader
    (define (read-hash start-line start-column start-pos)
      (get-char-tracked)  ; consume #
      (let ((ch (peek-char-tracked)))
        (cond
          ((eof-object? ch)
           (error 'read-cst "unexpected EOF after #"))

          ;; Shebang or directive
          ((char=? ch #\!)
           (read-shebang start-line start-column start-pos))

          ;; Booleans
          ((char=? ch #\t)
           (get-char-tracked)
           (make-cst-atom 'atom start-line start-column
                         start-pos char-pos "#t" #t))

          ((char=? ch #\f)
           (get-char-tracked)
           (make-cst-atom 'atom start-line start-column
                         start-pos char-pos "#f" #f))

          ;; Vector
          ((char=? ch #\()
           (read-vector start-line start-column start-pos))

          ;; Bytevector
          ((char=? ch #\v)
           (get-char-tracked)  ; consume v
           (let ((u-ch (peek-char-tracked)))
             (unless (and (not (eof-object? u-ch)) (char=? u-ch #\u))
               (error 'read-cst "expected 'u' after #v"))
             (get-char-tracked)  ; consume u
             (let ((eight-ch (peek-char-tracked)))
               (unless (and (not (eof-object? eight-ch)) (char=? eight-ch #\8))
                 (error 'read-cst "expected '8' after #vu"))
               (get-char-tracked)  ; consume 8
               (let ((paren-ch (peek-char-tracked)))
                 (unless (and (not (eof-object? paren-ch)) (char=? paren-ch #\())
                   (error 'read-cst "expected '(' after #vu8"))
                 (read-bytevector start-line start-column start-pos)))))

          ;; Datum comment
          ((char=? ch #\;)
           (read-datum-comment start-line start-column start-pos))

          ;; Character
          ((char=? ch #\\)
           (read-character start-line start-column start-pos))

          ;; Number with radix
          ((or (char=? ch #\x) (char=? ch #\b) (char=? ch #\o) (char=? ch #\d))
           (read-number-with-radix start-line start-column start-pos))

          ;; Syntax quote forms
          ((char=? ch #\')
           (get-char-tracked)
           (let ((expr (read-expr)))
             (make-cst-quote 'quote start-line start-column
                            start-pos char-pos
                            (extract-text start-pos char-pos)
                            'syntax "#'" expr)))

          ((char=? ch #\`)
           (get-char-tracked)
           (let ((expr (read-expr)))
             (make-cst-quote 'quote start-line start-column
                            start-pos char-pos
                            (extract-text start-pos char-pos)
                            'quasisyntax "#`" expr)))

          ((char=? ch #\,)
           (get-char-tracked)
           (let ((next-ch (peek-char-tracked)))
             (if (and (not (eof-object? next-ch)) (char=? next-ch #\@))
                 (begin
                   (get-char-tracked)
                   (let ((expr (read-expr)))
                     (make-cst-quote 'quote start-line start-column
                                    start-pos char-pos
                                    (extract-text start-pos char-pos)
                                    'unsyntax-splicing "#,@" expr)))
                 (let ((expr (read-expr)))
                   (make-cst-quote 'quote start-line start-column
                                  start-pos char-pos
                                  (extract-text start-pos char-pos)
                                  'unsyntax "#," expr)))))

          (else
           (error 'read-cst "unsupported # syntax" ch)))))

    ;; Shebang reader
    (define (read-shebang start-line start-column start-pos)
      (get-char-tracked)  ; consume !
      (let loop ((chars '()))
        (let ((ch (peek-char-tracked)))
          (if (or (eof-object? ch) (char=? ch #\newline))
              (let* ((end-pos char-pos)
                     (text (string-append "#!" (list->string (reverse chars)))))
                (make-cst-comment 'comment
                                 start-line start-column
                                 start-pos end-pos
                                 text
                                 (list->string (reverse chars)) 'shebang))
              (begin
                (get-char-tracked)
                (loop (cons ch chars)))))))

    ;; Vector reader
    (define (read-vector start-line start-column start-pos)
      (get-char-tracked)  ; consume (
      (let loop ((children '()))
        (let ((ch (peek-char-tracked)))
          (cond
            ((eof-object? ch)
             (error 'read-cst "unexpected EOF in vector"))

            ((char=? ch #\))
             (get-char-tracked)
             (let* ((end-pos char-pos)
                    (text (extract-text start-pos end-pos)))
               (make-cst-vector 'vector
                               start-line start-column
                               start-pos end-pos
                               text
                               (reverse children))))

            (else
             (loop (cons (read-expr) children)))))))

    ;; Bytevector reader
    (define (read-bytevector start-line start-column start-pos)
      (get-char-tracked)  ; consume (
      (let loop ((children '()))
        (let ((ch (peek-char-tracked)))
          (cond
            ((eof-object? ch)
             (error 'read-cst "unexpected EOF in bytevector"))

            ((char=? ch #\))
             (get-char-tracked)
             (let* ((end-pos char-pos)
                    (text (extract-text start-pos end-pos)))
               (make-cst-bytevector 'bytevector
                                   start-line start-column
                                   start-pos end-pos
                                   text
                                   (reverse children))))

            (else
             (loop (cons (read-expr) children)))))))

    ;; Datum comment reader
    (define (read-datum-comment start-line start-column start-pos)
      (get-char-tracked)  ; consume ;
      (let ((commented-expr (read-expr)))
        (when (eof-object? commented-expr)
          (error 'read-cst "unexpected EOF after datum comment"))
        (let* ((end-pos char-pos)
               (text (extract-text start-pos end-pos)))
          (make-cst-comment 'comment
                           start-line start-column
                           start-pos end-pos
                           text
                           (cst-node-text commented-expr) 'datum))))

    ;; Character reader
    (define (read-character start-line start-column start-pos)
      (get-char-tracked)  ; consume \
      (let ((ch (get-char-tracked)))
        (when (eof-object? ch)
          (error 'read-cst "unexpected EOF after #\\"))
        ;; Read rest of character name
        (let loop ((chars (list ch)))
          (let ((next (peek-char-tracked)))
            (if (or (eof-object? next)
                    (char-whitespace? next)
                    (memv next '(#\( #\) #\[ #\] #\" #\' #\` #\, #\;)))
                ;; Done reading
                (let* ((char-name (list->string (reverse chars)))
                       (char-val (parse-character char-name))
                       (end-pos char-pos)
                       (text (extract-text start-pos end-pos)))
                  (make-cst-atom 'atom start-line start-column
                                start-pos end-pos
                                text char-val))
                (begin
                  (get-char-tracked)
                  (loop (cons next chars))))))))

    (define (parse-character name)
      (cond
        ;; R6RS standard names
        ((string=? name "nul") #\nul)
        ((string=? name "alarm") #\alarm)
        ((string=? name "backspace") #\backspace)
        ((string=? name "tab") #\tab)
        ((string=? name "linefeed") #\linefeed)
        ((string=? name "newline") #\newline)
        ((string=? name "vtab") #\vtab)
        ((string=? name "page") #\page)
        ((string=? name "return") #\return)
        ((string=? name "esc") #\esc)
        ((string=? name "space") #\space)
        ((string=? name "delete") #\delete)
        ;; Hex notation: #\xHH
        ((and (> (string-length name) 1)
              (char=? (string-ref name 0) #\x))
         (let ((hex-str (substring name 1 (string-length name))))
           (let ((code (string->number hex-str 16)))
             (if code
                 (integer->char code)
                 (error 'read-cst "invalid hex character code" name)))))
        ;; Single character
        ((= (string-length name) 1) (string-ref name 0))
        (else (error 'read-cst "invalid character name" name))))

    ;; Number with radix reader
    (define (read-number-with-radix start-line start-column start-pos)
      (let ((radix-char (get-char-tracked)))
        (let loop ((chars '()))
          (let ((ch (peek-char-tracked)))
            (if (or (eof-object? ch)
                    (char-whitespace? ch)
                    (memv ch '(#\( #\) #\[ #\] #\" #\' #\` #\, #\;)))
                (let* ((digits (list->string (reverse chars)))
                       (radix (case radix-char
                               ((#\x) 16)
                               ((#\b) 2)
                               ((#\o) 8)
                               ((#\d) 10)
                               (else 10)))
                       (num (string->number digits radix))
                       (end-pos char-pos)
                       (text (extract-text start-pos end-pos)))
                  (if num
                      (make-cst-atom 'atom start-line start-column
                                    start-pos end-pos text num)
                      (error 'read-cst "invalid number literal" digits)))
                (begin
                  (get-char-tracked)
                  (loop (cons ch chars))))))))

    ;; Atom reader (symbols, numbers)
    (define (read-atom start-line start-column start-pos)
      (let loop ((chars '()))
        (let ((ch (peek-char-tracked)))
          (if (or (eof-object? ch)
                  (char-whitespace? ch)
                  (memv ch '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\;)))
              (let* ((str (list->string (reverse chars)))
                     (value (or (string->number str) (string->symbol str)))
                     (end-pos char-pos)
                     (text (extract-text start-pos end-pos)))
                (make-cst-atom 'atom start-line start-column
                              start-pos end-pos text value))
              (begin
                (get-char-tracked)
                (loop (cons ch chars)))))))

    ;; Helper: take n elements from list
    (define (take lst n)
      (if (or (null? lst) (<= n 0))
          '()
          (cons (car lst) (take (cdr lst) (- n 1)))))

    ;; Helper: drop n elements from list
    (define (drop lst n)
      (if (or (null? lst) (<= n 0))
          lst
          (drop (cdr lst) (- n 1))))

    ;; Body: return reader function
    read-expr))

) ;; end library
