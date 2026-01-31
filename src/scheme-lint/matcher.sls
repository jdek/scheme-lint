;;=============================================================================
;; matcher.sls - Pattern matching for CST nodes
;;=============================================================================
;; SPDX-License-Identifier: WTFPL
;;
;; Pattern matching engine that works on CST while ignoring trivia.
;; Patterns match semantic structure (atoms, lists, etc.) and automatically
;; skip whitespace and comments.

(library (scheme-lint matcher)
  (export match-pattern
          walk-tree
          walk-tree-with-trivia
          pattern-var?
          pattern-splice?
          get-binding
          get-semantic-value

          ;; CST construction helpers for fix functions
          make-comment-node
          make-whitespace-node)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (scheme-lint reader))

;;=============================================================================
;; Pattern Syntax

;; Pattern syntax:
;; - ?var         : capture single CST node
;; - (?var ...)   : capture zero or more CST nodes (splice)
;; - (?head . ?rest) : capture dotted pair
;; - literal      : must match exactly (symbol, number, string, boolean)
;; - (patterns...) : match list structure (ignores trivia)

(define (pattern-var? x)
  ;; Check if x is a pattern variable (?foo)
  (and (symbol? x)
       (let ((str (symbol->string x)))
         (and (>= (string-length str) 2)
              (char=? (string-ref str 0) #\?)
              (not (char=? (string-ref str 1) #\.))))))

(define (pattern-splice? pattern)
  ;; Check if pattern is a splice (?var ...)
  (and (pair? pattern)
       (pair? (cdr pattern))
       (null? (cddr pattern))
       (pattern-var? (car pattern))
       (eq? (cadr pattern) '...)))

(define (pattern-rest? x)
  ;; Check if x is the ellipsis symbol
  (eq? x '...))

;;=============================================================================
;; Binding Helpers

(define (get-binding bindings name)
  ;; Get CST node bound to name in bindings alist
  (let ((pair (assq name bindings)))
    (if pair (cdr pair) #f)))

(define (get-semantic-value bindings name)
  ;; Get semantic value (unwrapped) bound to name
  (let ((node (get-binding bindings name)))
    (if node (cst->sexp node) #f)))

;;=============================================================================
;; Pattern Matching

;; match-pattern : pattern cst-node alist -> alist | #f
;;   Matches pattern against CST node, accumulating bindings.
;;   Automatically skips trivia when matching list structures.
(define (match-pattern pattern node bindings)
  (cond
    ;; Pattern variable - capture the CST node
    ((pattern-var? pattern)
     (cons (cons pattern node) bindings))

    ;; Both are lists - match semantic children (skip trivia)
    ((and (pair? pattern) (cst-list? node))
     (match-list pattern (semantic-children node) bindings))

    ;; Both are vectors - match semantic children
    ((and (vector? pattern) (cst-vector? node))
     (match-vector pattern (semantic-children node) bindings))

    ;; Atom matching - compare values
    ((and (not (pair? pattern))
          (not (vector? pattern))
          (cst-atom? node))
     (if (equal? pattern (cst-atom-value node))
         bindings
         #f))

    ;; String matching
    ((and (string? pattern) (cst-string? node))
     (if (equal? pattern (cst-string-value node))
         bindings
         #f))

    ;; Quote matching
    ((and (pair? pattern)
          (memq (car pattern) '(quote quasiquote unquote unquote-splicing))
          (cst-quote? node))
     (if (eq? (car pattern) (cst-quote-type node))
         (match-pattern (cadr pattern) (cst-quote-expr node) bindings)
         #f))

    ;; Type mismatch
    (else #f)))

(define (match-list pattern nodes bindings)
  ;; Match list pattern against list of CST nodes (trivia already filtered)
  (cond
    ;; Empty pattern and empty list - success
    ((and (null? pattern) (null? nodes))
     bindings)

    ;; Pattern exhausted but nodes remain - failure
    ((null? pattern)
     #f)

    ;; Nodes exhausted but pattern remains - failure
    ((null? nodes)
     #f)

    ;; Splice pattern: (?var ...)
    ((and (pair? pattern)
          (pair? (cdr pattern))
          (not (null? (cdr pattern)))
          (pattern-var? (car pattern))
          (pattern-rest? (cadr pattern)))
     (let* ((var (car pattern))
            (rest-pattern (cddr pattern))
            (required-count (length rest-pattern))
            (available (length nodes)))
       (if (< available required-count)
           #f
           (let* ((splice-count (- available required-count))
                  (splice-nodes (take-list nodes splice-count))
                  (rest-nodes (drop-list nodes splice-count))
                  ;; Bind to list of CST nodes
                  (new-bindings (cons (cons var splice-nodes) bindings)))
             (match-list-elements rest-pattern rest-nodes new-bindings)))))

    ;; Dotted pair pattern: (pattern . ?rest)
    ((and (pair? pattern)
          (not (list? pattern))
          (pattern-var? (cdr pattern)))
     (let ((result (match-pattern (car pattern) (car nodes) bindings)))
       (if result
           ;; Bind ?rest to remaining CST nodes (as a list)
           (cons (cons (cdr pattern) (cdr nodes)) result)
           #f)))

    ;; Regular list matching
    (else
     (let ((result (match-pattern (car pattern) (car nodes) bindings)))
       (if result
           (match-list (cdr pattern) (cdr nodes) result)
           #f)))))

(define (match-list-elements patterns nodes bindings)
  ;; Match list of patterns against list of nodes
  (cond
    ((and (null? patterns) (null? nodes))
     bindings)
    ((or (null? patterns) (null? nodes))
     #f)
    (else
     (let ((result (match-pattern (car patterns) (car nodes) bindings)))
       (if result
           (match-list-elements (cdr patterns) (cdr nodes) result)
           #f)))))

(define (match-vector pattern nodes bindings)
  ;; Match vector pattern against vector of CST nodes
  (let ((plen (vector-length pattern))
        (nlen (length nodes)))
    (if (not (= plen nlen))
        #f
        (let loop ((i 0) (acc bindings) (nodes nodes))
          (if (>= i plen)
              acc
              (let ((result (match-pattern (vector-ref pattern i)
                                          (car nodes)
                                          acc)))
                (if result
                    (loop (+ i 1) result (cdr nodes))
                    #f)))))))

;;=============================================================================
;; Tree Walking

;; walk-tree : (cst-node -> any) cst-node -> list
;;   Walks CST depth-first, collecting non-#f results from proc.
;;   By default, only visits semantic nodes (skips trivia).
(define (walk-tree proc tree)
  (walk-tree-with-trivia proc tree #f))

;; walk-tree-with-trivia : (cst-node -> any) cst-node boolean -> list
;;   Walks CST depth-first with optional trivia inclusion.
;;   If include-trivia? is #t, visits whitespace and comment nodes.
(define (walk-tree-with-trivia proc tree include-trivia?)
  (let walk ((node tree) (results '()) (depth 0))
    (if (> depth 500)
        results  ; Depth limit to prevent infinite loops
        (let ((result (proc node)))
          (let ((new-results (if result
                                (cons result results)
                                results)))
            (cond
              ;; List node - walk children
              ((cst-list? node)
               (let ((children (if include-trivia?
                                  (cst-list-children node)
                                  (semantic-children node))))
                 (fold-left (lambda (acc child)
                             (walk child acc (+ depth 1)))
                           new-results
                           children)))

              ;; Quote node - walk quoted expression
              ((cst-quote? node)
               (walk (cst-quote-expr node) new-results (+ depth 1)))

              ;; Vector node - walk children
              ((cst-vector? node)
               (let ((children (if include-trivia?
                                  (cst-vector-children node)
                                  (semantic-children node))))
                 (fold-left (lambda (acc child)
                             (walk child acc (+ depth 1)))
                           new-results
                           children)))

              ;; Leaf nodes (atoms, strings, comments, whitespace)
              (else new-results)))))))

;;=============================================================================
;; Helpers

(define (take-list lst n)
  ;; Take first n elements from list
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-list (cdr lst) (- n 1)))))

(define (drop-list lst n)
  ;; Drop first n elements from list
  (if (or (null? lst) (<= n 0))
      lst
      (drop-list (cdr lst) (- n 1))))

;;=============================================================================
;; CST Node Construction Helpers

;; make-comment-node : string => cst-comment
;;   Create a comment CST node with the given text.
;;   Position fields are set to 0 (will be ignored during rendering).
(define (make-comment-node text)
  (make-cst-comment 'comment 0 0 0 0
                   text
                   (if (and (> (string-length text) 0)
                           (char=? (string-ref text 0) #\;))
                       (substring text 1 (string-length text))
                       text)
                   'line))

;; make-whitespace-node : string => cst-whitespace
;;   Create a whitespace CST node.
(define (make-whitespace-node text)
  (let ((has-newline? (let loop ((i 0))
                        (cond
                          ((>= i (string-length text)) #f)
                          ((char=? (string-ref text i) #\newline) #t)
                          (else (loop (+ i 1)))))))
    (make-cst-whitespace 'whitespace 0 0 0 0
                        text
                        text has-newline?)))

) ;; end library
