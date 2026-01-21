#!/usr/bin/env -S scheme --script
;;=======================================================================
;; scheme-lint - CLI for Scheme linter
;;=======================================================================
;; SPDX-License-Identifier: WTFPL

(import (rnrs)
        (only (chezscheme) command-line-arguments
                           directory-list
                           file-directory?
                           current-directory
                           exit
                           pretty-print)
        (scheme-lint core)
        (scheme-lint reader)
        (scheme-lint matcher)
        (scheme-lint config)
        (scheme-lint discovery)
        (scheme-lint rules builtin))

;;-----------------------------------------------------------------------
;; CLI Helpers
;;-----------------------------------------------------------------------

(define (print-usage)
  (display "Usage: scheme-lint [options] <files...>\n")
  (display "\n")
  (display "Options:\n")
  (display "  --rules <dir>       Directory containing rule files (default: ./rules)\n")
  (display "  --fix               Apply auto-fixes where available\n")
  (display "  --severity <lvl>    Minimum severity to report (error/warning/style)\n")
  (display "  --config <file>     Use specific config file\n")
  (display "  --no-config         Ignore all config files (use defaults)\n")
  (display "  --show-config       Display effective configuration and exit\n")
  (display "  --list-rules        List all available rules and exit\n")
  (display "  --describe-rule <name>  Show detailed information about a rule\n")
  (display "  --version           Show version information\n")
  (display "  --help              Show this help\n")
  (display "\n"))

(define (print-version)
  (display "scheme-lint ")
  (display scheme-lint-version)
  (display "\n"))

(define (parse-args args)
  "Parse command line arguments. Returns (options . files)"
  (let loop ((args args)
             (rules-dir #f)
             (fix-mode #f)
             (min-severity #f)
             (config-file #f)
             (no-config #f)
             (show-config #f)
             (list-rules #f)
             (describe-rule #f)
             (files '()))
    (cond
      ((null? args)
       (cons (list (cons 'rules-dir rules-dir)
                  (cons 'fix-mode fix-mode)
                  (cons 'min-severity min-severity)
                  (cons 'config-file config-file)
                  (cons 'no-config no-config)
                  (cons 'show-config show-config)
                  (cons 'list-rules list-rules)
                  (cons 'describe-rule describe-rule))
            (reverse files)))

      ((string=? (car args) "--help")
       (print-usage)
       (exit 0))

      ((string=? (car args) "--version")
       (print-version)
       (exit 0))

      ((string=? (car args) "--rules")
       (when (null? (cdr args))
         (error 'parse-args "Missing argument for --rules"))
       (loop (cddr args) (cadr args) fix-mode min-severity config-file no-config show-config list-rules describe-rule files))

      ((string=? (car args) "--fix")
       (loop (cdr args) rules-dir #t min-severity config-file no-config show-config list-rules describe-rule files))

      ((string=? (car args) "--severity")
       (when (null? (cdr args))
         (error 'parse-args "Missing argument for --severity"))
       (let ((sev (string->symbol (cadr args))))
         (unless (memq sev '(error warning style))
           (error 'parse-args "Invalid severity level" sev))
         (loop (cddr args) rules-dir fix-mode sev config-file no-config show-config list-rules describe-rule files)))

      ((string=? (car args) "--config")
       (when (null? (cdr args))
         (error 'parse-args "Missing argument for --config"))
       (loop (cddr args) rules-dir fix-mode min-severity (cadr args) no-config show-config list-rules describe-rule files))

      ((string=? (car args) "--no-config")
       (loop (cdr args) rules-dir fix-mode min-severity config-file #t show-config list-rules describe-rule files))

      ((string=? (car args) "--show-config")
       (loop (cdr args) rules-dir fix-mode min-severity config-file no-config #t list-rules describe-rule files))

      ((string=? (car args) "--list-rules")
       (loop (cdr args) rules-dir fix-mode min-severity config-file no-config show-config #t describe-rule files))

      ((string=? (car args) "--describe-rule")
       (when (null? (cdr args))
         (error 'parse-args "Missing argument for --describe-rule"))
       (loop (cddr args) rules-dir fix-mode min-severity config-file no-config show-config list-rules (string->symbol (cadr args)) files))

      (else
       (loop (cdr args) rules-dir fix-mode min-severity config-file no-config show-config list-rules describe-rule
            (cons (car args) files))))))

;;-----------------------------------------------------------------------
;; Config Display
;;-----------------------------------------------------------------------

(define (print-config cfg)
  "Pretty print effective configuration"
  (display "Effective configuration:\n\n")
  (display "  min-severity: ")
  (display (config-min-severity cfg))
  (display "\n")
  (display "  rules-dirs: ")
  (pretty-print (config-rules-dirs cfg))
  (display "  enabled-rules: ")
  (if (null? (config-enabled-rules cfg))
      (display "(all)\n")
      (begin
        (display "\n    ")
        (for-each (lambda (r) (display r) (display " ")) (config-enabled-rules cfg))
        (display "\n")))
  (display "  disabled-rules: ")
  (if (null? (config-disabled-rules cfg))
      (display "(none)\n")
      (begin
        (display "\n    ")
        (for-each (lambda (r) (display r) (display " ")) (config-disabled-rules cfg))
        (display "\n")))
  (display "  severity-overrides: ")
  (if (null? (config-severity-overrides cfg))
      (display "(none)\n")
      (begin
        (display "\n")
        (for-each (lambda (p)
                   (display "    ")
                   (display (car p))
                   (display " => ")
                   (display (cdr p))
                   (display "\n"))
                 (config-severity-overrides cfg))))
  (display "  ignore-patterns: ")
  (if (null? (config-ignore-patterns cfg))
      (display "(none)\n")
      (begin
        (display "\n")
        (for-each (lambda (p) (display "    ") (display p) (display "\n"))
                 (config-ignore-patterns cfg))))
  (display "\n"))

;;-----------------------------------------------------------------------
;; Rule Display
;;-----------------------------------------------------------------------

(define (print-rules-list rule-infos)
  "Print list of all rules with basic info"
  (display "Available rules:\n\n")
  (for-each
    (lambda (info)
      (let* ((rule (rule-info-rule info))
             (metadata (rule-info-metadata info))
             (name (lint-rule-name rule))
             (severity (lint-rule-severity rule))
             (category (rule-metadata-category metadata))
             (source (rule-info-source info)))
        (display "  ")
        (display name)
        (display " [")
        (display severity)
        (display "/")
        (display category)
        (display "] ")
        (when (not (string=? source "<builtin>"))
          (display "(")
          (display source)
          (display ")"))
        (display "\n")))
    rule-infos)
  (display "\n")
  (display "Total rules: ")
  (display (length rule-infos))
  (display "\n"))

(define (print-rule-details rule-info)
  "Print detailed information about a rule"
  (let* ((rule (rule-info-rule rule-info))
         (metadata (rule-info-metadata rule-info))
         (name (lint-rule-name rule))
         (pattern (lint-rule-pattern rule))
         (severity (lint-rule-severity rule))
         (message (lint-rule-message rule))
         (version (rule-metadata-version metadata))
         (author (rule-metadata-author metadata))
         (category (rule-metadata-category metadata))
         (tags (rule-metadata-tags metadata))
         (enabled (rule-metadata-enabled-by-default metadata))
         (description (rule-metadata-description metadata))
         (source (rule-info-source rule-info)))
    (display "Rule: ")
    (display name)
    (display "\n\n")
    (display "  Severity: ")
    (display severity)
    (display "\n")
    (display "  Category: ")
    (display category)
    (display "\n")
    (display "  Pattern: ")
    (pretty-print pattern)
    (display "  Message: ")
    (display (if (procedure? message) "<dynamic>" message))
    (display "\n")
    (display "  Source: ")
    (display source)
    (display "\n")
    (display "  Version: ")
    (display version)
    (display "\n")
    (display "  Author: ")
    (display author)
    (display "\n")
    (display "  Enabled by default: ")
    (display enabled)
    (display "\n")
    (when (not (null? tags))
      (display "  Tags: ")
      (for-each (lambda (t) (display t) (display " ")) tags)
      (display "\n"))
    (when (not (string=? description ""))
      (display "\n  Description:\n    ")
      (display description)
      (display "\n"))
    (display "\n")))

;;-----------------------------------------------------------------------
;; Violation Reporting
;;-----------------------------------------------------------------------

(define (severity-level sev)
  "Convert severity to numeric level for filtering"
  (case sev
    ((error) 3)
    ((warning) 2)
    ((style) 1)
    (else 0)))

(define (filter-violations violations min-severity)
  "Filter violations by minimum severity"
  (let ((min-level (severity-level min-severity)))
    (filter (lambda (v)
              (>= (severity-level (lint-violation-severity v)) min-level))
            violations)))

(define (format-location loc)
  "Format source location for display"
  (if loc
      (string-append (source-location-file loc) ":"
                    (number->string (source-location-line loc)) ":"
                    (number->string (source-location-column loc)))
      "<unknown>"))

(define (format-severity sev)
  "Format severity for display"
  (case sev
    ((error) "ERROR")
    ((warning) "WARN")
    ((style) "STYLE")
    (else "     ")))

(define (print-violation v)
  "Print a violation to stdout"
  (display (format-location (lint-violation-location v)))
  (display " [")
  (display (format-severity (lint-violation-severity v)))
  (display "] ")
  (display (lint-violation-message v))
  (display "\n"))

(define (print-summary violations)
  "Print summary of violations"
  (let* ((errors (filter (lambda (v)
                          (eq? (lint-violation-severity v) 'error))
                        violations))
         (warnings (filter (lambda (v)
                            (eq? (lint-violation-severity v) 'warning))
                          violations))
         (styles (filter (lambda (v)
                          (eq? (lint-violation-severity v) 'style))
                        violations)))
    (display "\n")
    (display "Summary:\n")
    (display (string-append "  Errors:   " (number->string (length errors)) "\n"))
    (display (string-append "  Warnings: " (number->string (length warnings)) "\n"))
    (display (string-append "  Style:    " (number->string (length styles)) "\n"))
    (display (string-append "  Total:    " (number->string (length violations)) "\n"))))

;;-----------------------------------------------------------------------
;; Main
;;-----------------------------------------------------------------------

(define (main args)
  (let* ((parsed (parse-args args))
         (options (car parsed))
         (files (cdr parsed)))

    ;; Load configuration
    (let* ((no-config (cdr (assq 'no-config options)))
           (config-file (cdr (assq 'config-file options)))
           (show-config (cdr (assq 'show-config options)))
           (list-rules (cdr (assq 'list-rules options)))
           (describe-rule (cdr (assq 'describe-rule options)))
           (cfg (cond
                  (no-config (default-config))
                  (config-file (parse-config-file config-file))
                  (else (load-config (current-directory)))))
           ;; CLI options override config
           (cli-rules-dir (cdr (assq 'rules-dir options)))
           (cli-severity (cdr (assq 'min-severity options)))
           (fix-mode (cdr (assq 'fix-mode options)))
           (min-severity (or cli-severity (config-min-severity cfg))))

      ;; Handle --show-config
      (when show-config
        (print-config cfg)
        (exit 0))

      ;; Discover rules
      (let* ((all-rule-infos (discover-rules cfg cli-rules-dir))
             (filtered-infos (filter-rules-by-metadata all-rule-infos cfg))
             (rules (map rule-info-rule filtered-infos)))

        ;; Handle --list-rules
        (when list-rules
          (print-rules-list filtered-infos)
          (exit 0))

        ;; Handle --describe-rule
        (when describe-rule
          (let ((info (find-rule-info-by-name describe-rule filtered-infos)))
            (if info
                (begin
                  (print-rule-details info)
                  (exit 0))
                (begin
                  (display "Error: Rule not found: ")
                  (display describe-rule)
                  (display "\n")
                  (exit 1)))))

        ;; Check files provided for linting
        (when (null? files)
          (display "Error: No files specified\n")
          (print-usage)
          (exit 1))

        ;; Lint files
        (let process-files ((files files) (all-violations '()))
          (if (null? files)
              (let ((filtered (filter-violations all-violations min-severity)))
                ;; Print all violations
                (for-each print-violation filtered)
                (print-summary filtered)

                ;; Exit with error code if errors found
                (if (exists (lambda (v) (eq? (lint-violation-severity v) 'error)) filtered)
                    (exit 1)
                    (exit 0)))

              (let* ((file (car files))
                     (violations (lint-file file rules)))
                (when fix-mode
                  (apply-fixes violations file))
                (process-files (cdr files)
                             (append all-violations violations)))))))))

(define (find-rule-info-by-name name rule-infos)
  "Find rule-info by rule name"
  (let loop ((infos rule-infos))
    (cond
      ((null? infos) #f)
      ((eq? (lint-rule-name (rule-info-rule (car infos))) name)
       (car infos))
      (else (loop (cdr infos))))))

(main (command-line-arguments))
