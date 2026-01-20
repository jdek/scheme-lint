#!/usr/bin/env scheme --script
;;=======================================================================
;; test-core.ss - Test the linter with some violations
;;=======================================================================

(import (rnrs)
        (only (chezscheme) pretty-print)
        (scheme-lint core)
        (scheme-lint reader)
        (scheme-lint matcher))

;; Test source code with violations
(define test-code
  "(library (test)
     (export foo bar)
     (import (rnrs)
             (only (chezscheme) pretty-print))

     (define (foo x y)
       (quotient x y))

     (define (bar a b)
       (modulo a b)))")

;; Simple test rules
(define test-rules
  (list
    ;; Rule 1: quotient -> div
    (make-lint-rule 'div-not-quotient
              '(quotient ?a ?b)
              (lambda (bindings expr) #t)
              severity/style
              "Use 'div' instead of 'quotient'"
              #f)

    ;; Rule 2: modulo -> mod
    (make-lint-rule 'mod-not-modulo
              '(modulo ?a ?b)
              (lambda (bindings expr) #t)
              severity/style
              "Use 'mod' instead of 'modulo'"
              #f)

    ;; Rule 3: no (rnrs) import
    (make-lint-rule 'no-rnrs-all
              '(import . ?imports)
              (lambda (bindings expr)
                (let ((imps (cdr (assq '?imports bindings))))
                  (if (annotated? imps)
                      (let ((imp-expr (annotated-expr imps)))
                        (and (pair? imp-expr)
                             (let check ((lst imp-expr))
                               (cond
                                 ((null? lst) #f)
                                 (else
                                   (let ((item (car lst)))
                                     (let ((raw (if (annotated? item)
                                                   (annotated-expr item)
                                                   item)))
                                       (if (and (pair? raw)
                                               (eq? (car raw) 'rnrs)
                                               (null? (cdr raw)))
                                           #t
                                           (check (cdr lst))))))))))
                      #f)))
              severity/warning
              "Import specific (rnrs ...) modules instead of all of (rnrs)"
              #f)))

(define (test-linter)
  (display "Testing linter...\n\n")
  (display "Test code:\n")
  (display test-code)
  (display "\n\n")

  (let ((violations (lint-string test-code "test.scm" test-rules)))
    (display (string-append "Found " (number->string (length violations)) " violations:\n\n"))

    (for-each
      (lambda (v)
        (display "  - ")
        (display (lint-violation-message v))
        (when (lint-violation-location v)
          (display " at line ")
          (display (source-location-line (lint-violation-location v))))
        (display "\n"))
      violations)

    (display "\nTest ")
    (display (if (= (length violations) 3) "PASSED" "FAILED"))
    (display "\n")))

(test-linter)
