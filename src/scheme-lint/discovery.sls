;;=============================================================================
;; discovery.sls - Rule discovery and metadata management
;;=============================================================================
;; SPDX-License-Identifier: WTFPL

(library (scheme-lint discovery)
  (export make-rule-info
          rule-info?
          rule-info-rule
          rule-info-metadata
          rule-info-source

          make-rule-metadata
          rule-metadata?
          rule-metadata-version
          rule-metadata-author
          rule-metadata-category
          rule-metadata-tags
          rule-metadata-enabled-by-default
          rule-metadata-description

          discover-rules
          load-rules-with-metadata
          filter-rules-by-metadata)

  (import (rnrs base)
          (rnrs control)
          (rnrs files)
          (rnrs lists)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs records syntactic)
          (rnrs eval)
          (only (chezscheme) directory-list
                             file-directory?
                             getenv)
          (scheme-lint core)
          (scheme-lint config)
          (scheme-lint rules builtin))

;;=============================================================================
;; Rule Metadata

;; rule-metadata : metadata about a rule
(define-record-type rule-metadata
  (nongenerative)
  (sealed #t)
  (fields version               ;; string: "1.0.0"
          author                ;; string: "scheme-lint"
          category              ;; symbol: style/correctness/performance
          tags                  ;; list of symbols
          enabled-by-default    ;; boolean
          description))         ;; string: long description

;; Default metadata
(define (default-metadata)
  (make-rule-metadata "1.0.0"
                      "unknown"
                      'style
                      '()
                      #t
                      "No description provided"))

;;=============================================================================
;; Rule Info

;; rule-info : combines rule with metadata and source info
(define-record-type rule-info
  (nongenerative)
  (sealed #t)
  (fields rule        ;; lint-rule
          metadata    ;; rule-metadata
          source))    ;; string: file path where rule was loaded from

;;=============================================================================
;; Rule Discovery

;; discover-rules : config string|#f => list
;;   effects: io/read
;;   Discovers rules from all sources in priority order.
(define (discover-rules cfg cli-rules-dir)
  (let* ((builtin-infos (discover-builtin-rules))
         (user-infos (discover-user-rules))
         (config-infos (discover-config-rules cfg))
         (cli-infos (if cli-rules-dir
                        (discover-cli-rules cli-rules-dir)
                        '())))
    ;; Merge all sources, later overrides earlier by name
    (deduplicate-rules-by-name
      (append builtin-infos user-infos config-infos cli-infos))))

(define (discover-builtin-rules)
  "Discover built-in rules"
  (map (lambda (rule)
         (make-rule-info rule
                        (make-rule-metadata "1.0.0"
                                           "scheme-lint"
                                           'style
                                           '(builtin)
                                           #t
                                           "Built-in rule")
                        "<builtin>"))
       (get-builtin-rules)))

(define (discover-user-rules)
  "Discover rules from user directory"
  (let ((user-dir (string-append (get-home-directory)
                                 "/.config/scheme-lint/rules")))
    (if (file-directory? user-dir)
        (load-rules-from-directory user-dir "user")
        '())))

(define (discover-config-rules cfg)
  "Discover rules from config-specified directories"
  (apply append
         (map (lambda (dir)
                (if (file-directory? dir)
                    (load-rules-from-directory dir "project")
                    '()))
              (config-rules-dirs cfg))))

(define (discover-cli-rules cli-dir)
  "Discover rules from CLI-specified directory"
  (if (file-directory? cli-dir)
      (load-rules-from-directory cli-dir "cli")
      '()))

;;=============================================================================
;; Rule Loading with Metadata

;; load-rules-with-metadata : string => list
;;   effects: io/read
;;   Loads rules from file with metadata extraction.
(define (load-rules-with-metadata file-path source-type)
  (call-with-input-file file-path
    (lambda (port)
      (let loop ((expr (read port))
                 (infos '()))
        (if (eof-object? expr)
            (reverse infos)
            (cond
              ((and (pair? expr) (eq? (car expr) 'rule))
               ;; DSL rule with possible metadata
               (let* ((metadata (extract-metadata-from-dsl expr))
                      (rule (eval-rule-dsl-for-discovery expr))
                      (info (make-rule-info rule metadata file-path)))
                 (loop (read port) (cons info infos))))
              ((and (pair? expr) (eq? (car expr) 'make-lint-rule))
               ;; Direct rule construction
               (let* ((env (make-plugin-environment))
                      (rule (eval expr env))
                      (info (make-rule-info rule (default-metadata) file-path)))
                 (loop (read port) (cons info infos))))
              (else
               (loop (read port) infos))))))))

(define (load-rules-from-directory dir-path source-type)
  "Load all .scm files from directory with metadata"
  (let ((entries (directory-list dir-path)))
    (apply append
           (map (lambda (entry)
                  (let ((path (string-append dir-path "/" entry)))
                    (if (and (>= (string-length entry) 4)
                            (string=? (substring entry
                                               (- (string-length entry) 4)
                                               (string-length entry))
                                     ".scm"))
                        (load-rules-with-metadata path source-type)
                        '())))
                entries))))

(define (extract-metadata-from-dsl dsl-expr)
  "Extract metadata clause from DSL rule"
  (let* ((body (cddr dsl-expr))
         (metadata-clause (assq 'metadata body)))
    (if metadata-clause
        (parse-metadata-clause (cdr metadata-clause))
        (default-metadata))))

(define (parse-metadata-clause metadata-body)
  "Parse metadata clause into rule-metadata record"
  (let ((version (cadr (or (assq 'version metadata-body) '(#f "1.0.0"))))
        (author (cadr (or (assq 'author metadata-body) '(#f "unknown"))))
        (category (cadr (or (assq 'category metadata-body) '(#f style))))
        (tags (cdr (or (assq 'tags metadata-body) '(#f))))
        (enabled (cadr (or (assq 'enabled-by-default metadata-body) '(#f #t))))
        (description (cadr (or (assq 'description metadata-body) '(#f "")))))
    (make-rule-metadata version author category tags enabled description)))

(define (eval-rule-dsl-for-discovery dsl-expr)
  "Evaluate DSL rule for discovery (re-exports from core)"
  (eval-rule-dsl dsl-expr))

;;=============================================================================
;; Rule Filtering

;; filter-rules-by-metadata : list config => list
;;   Filters rule-infos by config enable/disable and metadata.
(define (filter-rules-by-metadata rule-infos cfg)
  (let ((enabled (config-enabled-rules cfg))
        (disabled (config-disabled-rules cfg)))
    (filter
      (lambda (info)
        (let* ((rule (rule-info-rule info))
               (name (lint-rule-name rule))
               (metadata (rule-info-metadata info))
               (enabled-by-default (rule-metadata-enabled-by-default metadata)))
          (and (or (null? enabled)
                   (memq name enabled))
               (not (memq name disabled))
               (or (not (null? enabled))
                   enabled-by-default))))
      rule-infos)))

;;=============================================================================
;; Deduplication

(define (deduplicate-rules-by-name rule-infos)
  "Remove duplicate rules by name, keeping last occurrence"
  (let loop ((infos (reverse rule-infos))
             (seen '())
             (result '()))
    (if (null? infos)
        result
        (let* ((info (car infos))
               (name (lint-rule-name (rule-info-rule info))))
          (if (memq name seen)
              (loop (cdr infos) seen result)
              (loop (cdr infos) (cons name seen) (cons info result)))))))

;;=============================================================================
;; Helpers

(define (get-home-directory)
  "Get user's home directory"
  (or (getenv "HOME")
      "/tmp"))

) ;; end library
