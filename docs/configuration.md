# Configuration

scheme-lint supports hierarchical S-expression configuration files for customizing linting behavior.

## Configuration File Format

Configuration files use S-expression syntax:

```scheme
(config
  (min-severity warning)
  (rules-dirs "custom-rules" "team-rules")
  (rules
    (enable rule1 rule2)
    (disable rule3 rule4))
  (severity-overrides
    rule-name new-severity
    another-rule error)
  (ignore
    "generated/*"
    "vendor/**/*.scm"))
```

## Search Hierarchy

scheme-lint searches for configuration in this order:

1. **Project config**: `.scheme-lint.scm` in current directory
2. **Parent directories**: Walks up directory tree to root
3. **User config**: `~/.config/scheme-lint/config.scm`
4. **Default**: Built-in defaults if no config found

Later configs override earlier ones (project > user > default).

## Configuration Options

### `min-severity`

Minimum severity level to report. One of:
- `style` - Report all violations (default)
- `warning` - Report warnings and errors only
- `error` - Report only errors

```scheme
(min-severity warning)
```

### `rules-dirs`

Additional directories to search for rule files. Can be relative or absolute paths.

```scheme
(rules-dirs
  "./project-rules"
  "~/.scheme-lint/custom-rules")
```

### `rules`

Control which rules are enabled:

```scheme
(rules
  ;; Enable only specific rules (leave empty to enable all)
  (enable div-not-quotient import-order)

  ;; Disable specific rules
  (disable no-emacs-docstrings))
```

If `enable` is empty or omitted, all rules are enabled by default (except those in `disable`).

### `severity-overrides`

Change the severity level of specific rules:

```scheme
(severity-overrides
  no-rnrs-all error          ;; Make this an error instead of warning
  import-order style)         ;; Downgrade to style issue
```

### `ignore`

Glob patterns for files to ignore:

```scheme
(ignore
  "generated/*"
  "tests/fixtures/**/*.scm"
  "*.backup.scm")
```

## Example Configurations

### Minimal Config

```scheme
;; .scheme-lint.scm
(config
  (min-severity warning))
```

### Project Config

```scheme
;; .scheme-lint.scm
(config
  (min-severity warning)

  (rules-dirs
    "./lint-rules")

  (rules
    (disable no-emacs-docstrings))

  (severity-overrides
    no-rnrs-all error)

  (ignore
    "tests/fixtures/*"))
```

### User Config

```scheme
;; ~/.config/scheme-lint/config.scm
(config
  (min-severity style)

  (rules-dirs
    "~/.scheme-lint/rules")

  (rules
    (enable
      div-not-quotient
      mod-not-modulo
      import-order)))
```

## CLI Override Options

Command-line flags override configuration file settings:

```bash
# Use specific config file
scheme-lint --config my-config.scm file.scm

# Ignore all config files
scheme-lint --no-config file.scm

# Show effective configuration
scheme-lint --show-config

# Override severity
scheme-lint --severity error file.scm

# Override rules directory
scheme-lint --rules custom-rules/ file.scm
```

## Configuration Precedence

When multiple configs are loaded:

1. **Default config**: Built-in defaults
2. **User config**: `~/.config/scheme-lint/config.scm`
3. **Project config**: `.scheme-lint.scm` (found via upward search)
4. **CLI flags**: Command-line options (highest priority)

### Merging Behavior

- **min-severity**: Latest non-default value wins
- **rules-dirs**: Appended (all directories searched)
- **enabled-rules**: Union of all enabled rules
- **disabled-rules**: Union of all disabled rules
- **severity-overrides**: Latest value wins for each rule
- **ignore-patterns**: Union of all patterns

## Tips

### Per-Project Configuration

Create `.scheme-lint.scm` in your project root:

```scheme
(config
  (min-severity warning)
  (rules-dirs "lint-rules")
  (ignore "generated/*"))
```

### Shareable Team Configuration

Create a shared config file and reference it:

```bash
scheme-lint --config /team/shared/.scheme-lint.scm *.scm
```

Or use a rules directory:

```bash
scheme-lint --rules /team/shared/rules *.scm
```

### Debugging Configuration

Use `--show-config` to see the effective merged configuration:

```bash
$ scheme-lint --show-config
Effective configuration:

  min-severity: warning
  rules-dirs: ("rules/standard")
  enabled-rules: (all)
  disabled-rules:
    no-emacs-docstrings
  severity-overrides:
    no-rnrs-all => error
  ignore-patterns:
    tests/fixtures/*
```

### Disabling Config

To run with defaults only:

```bash
scheme-lint --no-config file.scm
```
