#!/usr/bin/env scheme --script
;;=======================================================================
;; test-core.ss - Test the CST-based linter
;;=======================================================================

(import (rnrs base)
        (rnrs io simple)
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

;; Simple test rules using CST
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

    ;; Rule 3: no (rnrs) import - CST version
    (make-lint-rule 'no-rnrs-all
              '(import . ?imports)
              (lambda (bindings expr)
                (let ((imps (get-binding bindings '?imports)))
                  (let check ((lst imps))
                    (cond
                      ((null? lst) #f)
                      (else
                        (let ((item (car lst)))
                          (if (and (cst-list? item)
                                   (let ((children (semantic-children item)))
                                     (and (= 1 (length children))
                                          (cst-atom? (car children))
                                          (eq? 'rnrs (cst-atom-value (car children))))))
                              #t
                              (check (cdr lst)))))))))
              severity/warning
              "Import specific (rnrs ...) modules instead of all of (rnrs)"
              #f)))

(define (test-linter)
  (display "Testing CST-based linter...\n\n")
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
