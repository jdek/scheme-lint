;;=============================================================================
;; matcher.sls - Pattern matching for annotated s-expressions
;;=============================================================================
;; SPDX-License-Identifier: WTFPL

(library (scheme-lint matcher)
  (export match-pattern
          walk-tree
          pattern-var?
          pattern-splice?
          unwrap-annotated)
  (import (rnrs)
          (scheme-lint reader))

;;=============================================================================
;; Pattern Syntax

;; Pattern syntax:
;; - ?var         : capture single expression
;; - (?var ...)   : capture zero or more expressions (splice)
;; - (?head . ?rest) : capture dotted pair
;; - literal      : must match exactly (symbol, number, string, boolean)
;; - (patterns...) : match list structure

(define (pattern-var? x)
  (and (symbol? x)
       (let ((str (symbol->string x)))
         (and (>= (string-length str) 2)
              (char=? (string-ref str 0) #\?)
              (not (char=? (string-ref str 1) #\.))))))

(define (pattern-splice? pattern)
  (and (pair? pattern)
       (pair? (cdr pattern))
       (null? (cddr pattern))
       (pattern-var? (car pattern))
       (eq? (cadr pattern) '...)))

(define (pattern-rest? x)
  (eq? x '...))

(define (unwrap-annotated expr)
  (cond
    ((annotated? expr)
     (unwrap-annotated (annotated-expr expr)))
    ((pair? expr)
     (cons (unwrap-annotated (car expr))
           (unwrap-annotated (cdr expr))))
    ((vector? expr)
     (vector-map unwrap-annotated expr))
    (else expr)))

;;=============================================================================
;; Pattern Matching

;; match-pattern : pattern expr alist => alist | #f
;;   Matches pattern against expr, accumulating bindings.
(define (match-pattern pattern expr bindings)
  (let ((raw-expr (if (annotated? expr)
                      (annotated-expr expr)
                      expr)))
    (cond
      ;; Pattern variable - capture anything
      ((pattern-var? pattern)
       (cons (cons pattern expr) bindings))

      ;; Both are pairs - match structure
      ((and (pair? pattern) (pair? raw-expr))
       (match-list pattern expr bindings))

      ;; Both are vectors - match elements
      ((and (vector? pattern) (vector? raw-expr))
       (match-vector pattern expr bindings))

      ;; Atoms must match exactly
      ((and (not (pair? pattern))
            (not (vector? pattern))
            (not (pair? raw-expr))
            (not (vector? raw-expr)))
       (if (equal? pattern raw-expr)
           bindings
           #f))

      ;; Type mismatch
      (else #f))))

(define (match-list pattern expr bindings)
  (let ((raw-expr (if (annotated? expr)
                      (annotated-expr expr)
                      expr)))

    (cond
      ;; Empty pattern and empty list - success
      ((and (null? pattern) (null? raw-expr))
       bindings)

      ;; Pattern exhausted but list has more - failure
      ((null? pattern)
       #f)

      ;; List exhausted but pattern has more - failure
      ((null? raw-expr)
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
              (available (list-length raw-expr)))
         (if (or (not available) (< available required-count))
             #f
             (let* ((splice-count (- available required-count))
                    (splice-elements (take-list raw-expr splice-count))
                    (rest-elements (drop-list raw-expr splice-count))
                    (new-bindings (cons (cons var splice-elements) bindings)))
               (match-list-elements rest-pattern rest-elements new-bindings)))))

      ;; Dotted pair pattern: (pattern . ?rest)
      ((and (pair? pattern)
            (not (list? pattern))
            (pattern-var? (cdr pattern)))
       (let ((result (match-pattern (car pattern) (car raw-expr) bindings)))
         (if result
             ;; Bind to cdr of ANNOTATED expr, not raw-expr, to preserve annotations
             (cons (cons (cdr pattern) (cdr (if (annotated? expr) (annotated-expr expr) expr))) result)
             #f)))

      ;; Regular list matching
      (else
       ;; Guard against improper lists (dotted pairs) in the data.
       ;; e.g., pattern (+ ?a ?b) vs data (+ . -)
       ;; After matching +, (cdr raw-expr) would be -, an atom, not a list
       (if (not (pair? raw-expr))
           #f
           (let ((result (match-pattern (car pattern) (car raw-expr) bindings)))
             (if result
                 (match-list (cdr pattern) (cdr raw-expr) result)
                 #f)))))))

(define (match-list-elements patterns exprs bindings)
  (cond
    ((and (null? patterns) (null? exprs))
     bindings)
    ((or (null? patterns) (null? exprs))
     #f)
    (else
     (let ((result (match-pattern (car patterns) (car exprs) bindings)))
       (if result
           (match-list-elements (cdr patterns) (cdr exprs) result)
           #f)))))

(define (match-vector pattern expr bindings)
  (let* ((raw-expr (if (annotated? expr)
                       (annotated-expr expr)
                       expr))
         (plen (vector-length pattern))
         (elen (vector-length raw-expr)))
    (if (not (= plen elen))
        #f
        (let loop ((i 0) (acc bindings))
          (if (>= i plen)
              acc
              (let ((result (match-pattern (vector-ref pattern i)
                                          (vector-ref raw-expr i)
                                          acc)))
                (if result
                    (loop (+ i 1) result)
                    #f)))))))

;;=============================================================================
;; Tree Walking

;; walk-tree : (node => any) tree => list
;;   Walks tree depth-first, collecting non-#f results from proc.
(define (walk-tree proc tree)
  (let walk ((node tree) (results '()) (depth 0))
    (if (> depth 500)
        results
        (let ((unwrapped (if (annotated? node)
                            (annotated-expr node)
                            node)))
          (let ((result (proc node)))
            (let ((new-results (if result
                                  (cons result results)
                                  results)))
              (cond
                ((pair? unwrapped)
                 (walk (cdr unwrapped) (walk (car unwrapped) new-results (+ depth 1)) (+ depth 1)))
                ((vector? unwrapped)
                 (let loop ((i (- (vector-length unwrapped) 1))
                           (acc new-results))
                   (if (< i 0)
                       acc
                       (loop (- i 1) (walk (vector-ref unwrapped i) acc (+ depth 1))))))
                (else new-results))))))))

;;=============================================================================
;; Helpers

(define (take-list lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-list (cdr lst) (- n 1)))))

(define (drop-list lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop-list (cdr lst) (- n 1))))

(define (list-length lst)
  (let loop ((l lst) (n 0))
    (cond
      ((null? l) n)
      ((not (pair? l)) #f)
      (else (loop (cdr l) (+ n 1))))))

) ;; end library
