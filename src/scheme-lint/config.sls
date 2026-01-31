;;=============================================================================
;; config.sls - Configuration system for scheme-lint
;;=============================================================================
;; SPDX-License-Identifier: WTFPL

(library (scheme-lint config)
  (export make-config
          config?
          config-min-severity
          config-rules-dirs
          config-enabled-rules
          config-disabled-rules
          config-severity-overrides
          config-ignore-patterns

          load-config
          find-config-file
          merge-configs
          default-config
          parse-config-file)

  (import (rnrs base)
          (rnrs control)
          (rnrs files)
          (rnrs lists)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs exceptions)
          (rnrs records syntactic)
          (only (chezscheme) current-directory
                             directory-separator
                             getenv))

;;=============================================================================
;; Configuration Record

;; config : configuration for linting
(define-record-type config
  (nongenerative)
  (sealed #t)
  (fields min-severity        ;; symbol: error/warning/style
          rules-dirs          ;; list of strings: directories to search for rules
          enabled-rules       ;; list of symbols: rules to enable (empty = all)
          disabled-rules      ;; list of symbols: rules to disable
          severity-overrides  ;; alist: (rule-name . severity)
          ignore-patterns))   ;; list of strings: glob patterns to ignore

;;=============================================================================
;; Default Configuration

;; Return default configuration
(define (default-config)
  (make-config 'style       ;; min-severity
               '()          ;; rules-dirs (empty, will use CLI default)
               '()          ;; enabled-rules (empty = all enabled)
               '()          ;; disabled-rules
               '()          ;; severity-overrides
               '()))        ;; ignore-patterns

;;=============================================================================
;; Config File Parsing

;; parse-config-file : string => config
;;   effects: io/read
;;   Parses S-expression config file.
(define (parse-config-file path)
  (call-with-input-file path
    (lambda (port)
      (let ((expr (read port)))
        (parse-config-expr expr)))))

;; Parse config S-expression into config record
(define (parse-config-expr expr)
  (unless (and (pair? expr) (eq? (car expr) 'config))
    (error 'parse-config-expr "Config must start with (config ...)" expr))

  (let ((body (cdr expr)))
    (make-config
      (parse-min-severity body)
      (parse-rules-dirs body)
      (parse-enabled-rules body)
      (parse-disabled-rules body)
      (parse-severity-overrides body)
      (parse-ignore-patterns body))))

;; Extract min-severity from config body
(define (parse-min-severity body)
  (let ((clause (assq 'min-severity body)))
    (if clause
        (cadr clause)
        'style)))

;; Extract rules-dirs from config body
(define (parse-rules-dirs body)
  (let ((clause (assq 'rules-dirs body)))
    (if clause
        (cdr clause)
        '())))

;; Extract enabled rules from config body
(define (parse-enabled-rules body)
  (let ((rules-clause (assq 'rules body)))
    (if rules-clause
        (let ((enable-clause (assq 'enable (cdr rules-clause))))
          (if enable-clause
              (cdr enable-clause)
              '()))
        '())))

;; Extract disabled rules from config body
(define (parse-disabled-rules body)
  (let ((rules-clause (assq 'rules body)))
    (if rules-clause
        (let ((disable-clause (assq 'disable (cdr rules-clause))))
          (if disable-clause
              (cdr disable-clause)
              '()))
        '())))

;; Extract severity overrides from config body
(define (parse-severity-overrides body)
  (let ((clause (assq 'severity-overrides body)))
    (if clause
        ;; Convert flat list to alist: (rule1 sev1 rule2 sev2) => ((rule1 . sev1) (rule2 . sev2))
        (let parse-pairs ((items (cdr clause)))
          (if (or (null? items) (null? (cdr items)))
              '()
              (cons (cons (car items) (cadr items))
                    (parse-pairs (cddr items)))))
        '())))

;; Extract ignore patterns from config body
(define (parse-ignore-patterns body)
  (let ((clause (assq 'ignore body)))
    (if clause
        (cdr clause)
        '())))

;;=============================================================================
;; Config File Discovery

;; find-config-file : string => string | #f
;;   effects: io/read
;;   Searches for config file starting from dir, walking up to root.
(define (find-config-file start-dir)
  (let search ((dir (normalize-path start-dir)))
    (let ((config-path (string-append dir "/.scheme-lint.scm")))
      (if (file-exists? config-path)
          config-path
          (let ((parent (parent-directory dir)))
            (if (or (not parent) (string=? parent dir))
                #f
                (search parent)))))))

;; Normalize path by removing trailing slashes
(define (normalize-path path)
  (if (and (> (string-length path) 1)
           (char=? (string-ref path (- (string-length path) 1)) #\/))
      (substring path 0 (- (string-length path) 1))
      path))

;; Get parent directory of path
(define (parent-directory path)
  (let loop ((i (- (string-length path) 1)))
    (cond
      ((< i 0) #f)
      ((char=? (string-ref path i) #\/)
       (if (= i 0)
           "/"
           (substring path 0 i)))
      (else (loop (- i 1))))))

;;=============================================================================
;; Config Loading

;; load-config : string => config
;;   effects: io/read
;;   Loads and merges config hierarchy starting from dir.
(define (load-config start-dir)
  (let* ((project-config-path (find-config-file start-dir))
         (user-config-path (string-append (get-home-directory)
                                          "/.config/scheme-lint/config.scm"))
         (configs (list (default-config))))

    ;; Load user config if exists
    (when (file-exists? user-config-path)
      (set! configs (append configs (list (parse-config-file user-config-path)))))

    ;; Load project config if exists
    (when project-config-path
      (set! configs (append configs (list (parse-config-file project-config-path)))))

    ;; Merge all configs (later overrides earlier)
    (fold-left merge-configs (car configs) (cdr configs))))

;; Get user's home directory
(define (get-home-directory)
  (or (getenv "HOME")
      "/tmp"))

;;=============================================================================
;; Config Merging

;; merge-configs : config config => config
;;   Merges two configs, with cfg2 overriding cfg1.
(define (merge-configs cfg1 cfg2)
  (make-config
    ;; min-severity: cfg2 wins if not default
    (if (eq? (config-min-severity cfg2) 'style)
        (config-min-severity cfg1)
        (config-min-severity cfg2))

    ;; rules-dirs: append (cfg2 has priority in search)
    (append (config-rules-dirs cfg1) (config-rules-dirs cfg2))

    ;; enabled-rules: merge (union)
    (merge-rule-lists (config-enabled-rules cfg1) (config-enabled-rules cfg2))

    ;; disabled-rules: merge (union)
    (merge-rule-lists (config-disabled-rules cfg1) (config-disabled-rules cfg2))

    ;; severity-overrides: merge (cfg2 wins for conflicts)
    (merge-alists (config-severity-overrides cfg1) (config-severity-overrides cfg2))

    ;; ignore-patterns: merge (union)
    (merge-string-lists (config-ignore-patterns cfg1) (config-ignore-patterns cfg2))))

;; Merge two lists of rule names, removing duplicates
(define (merge-rule-lists list1 list2)
  (let add-new ((lst list2) (result list1))
    (if (null? lst)
        result
        (if (memq (car lst) result)
            (add-new (cdr lst) result)
            (add-new (cdr lst) (cons (car lst) result))))))

;; Merge two lists of strings, removing duplicates
(define (merge-string-lists list1 list2)
  (let add-new ((lst list2) (result list1))
    (if (null? lst)
        result
        (if (member (car lst) result)
            (add-new (cdr lst) result)
            (add-new (cdr lst) (cons (car lst) result))))))

;; Merge two alists, with alist2 values overriding alist1
(define (merge-alists alist1 alist2)
  (let ((result (fold-left
                  (lambda (acc pair)
                    (if (assq (car pair) alist2)
                        acc
                        (cons pair acc)))
                  '()
                  alist1)))
    (append alist2 result)))

) ;; end library
