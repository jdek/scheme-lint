;;=======================================================================
;; .scheme-lint.scm - Example configuration for scheme-lint
;;=======================================================================

(config
  ;; Minimum severity level to report
  (min-severity warning)

  ;; Directories to search for rules (in addition to defaults)
  (rules-dirs
    "rules/standard")

  ;; Enable/disable specific rules
  (rules
    ;; Only these rules are enabled (comment out to enable all)
    ;; (enable div-not-quotient import-order)

    ;; Disable specific rules
    (disable no-emacs-docstrings))

  ;; Override severity for specific rules
  (severity-overrides
    no-rnrs-all error
    import-order style)

  ;; Files/directories to ignore (glob patterns)
  (ignore
    "tests/fixtures/*"
    "*.backup.scm"))
