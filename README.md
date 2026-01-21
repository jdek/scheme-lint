# scheme-lint

Pluggable linter for Chez Scheme with pattern-based rules and zero dependencies.

## Features

- **Zero dependencies**: Pure R6RS Scheme + minimal Chez features
- **Pattern matching DSL**: Write concise linting rules with powerful pattern syntax
- **Source location tracking**: Precise file/line/column information for violations
- **Plugin architecture**: Load custom rules from directories
- **Configurable severity**: Filter by error/warning/style
- **Auto-fix framework**: Extensible foundation for automatic fixes

## Quick Start

### Installation

Using Nix flakes:

```bash
# Enter development environment
nix develop github:jdek/scheme-lint

# Or build and install
nix build github:jdek/scheme-lint
```

From source:

```bash
git clone https://github.com/jdek/scheme-lint.git
cd scheme-lint
nix develop  # or manually set CHEZSCHEMELIBDIRS=./src
```

### Basic Usage

```bash
# Lint files
scheme-lint myfile.scm

# Use custom rules directory
scheme-lint --rules my-rules/ *.scm

# Filter by severity
scheme-lint --severity warning myfile.scm

# Show effective configuration
scheme-lint --show-config

# Show version
scheme-lint --version
```

## Configuration

scheme-lint supports S-expression configuration files for project-specific settings:

```scheme
;; .scheme-lint.scm
(config
  (min-severity warning)
  (rules-dirs "custom-rules")
  (rules
    (disable no-emacs-docstrings))
  (severity-overrides
    no-rnrs-all error))
```

Configuration files are searched hierarchically:
1. `.scheme-lint.scm` in current/parent directories
2. `~/.config/scheme-lint/config.scm` (user config)
3. Default configuration

See [Configuration Guide](docs/configuration.md) for complete details.

## Example Rule

The DSL makes it easy to write custom rules:

```scheme
;; rules/my-rules/prefer-let.scm
(rule prefer-let-over-letrec
  (pattern (letrec ((?var ?expr)) . ?body))
  (where #t)
  (severity style)
  (message "Use 'let' instead of 'letrec' for non-recursive bindings"))
```

## Built-in Rules

- **div-not-quotient**: Prefer R6RS `div`/`mod` over `quotient`/`modulo`
- **import-order**: Enforce import ordering (rnrs → local → chezscheme)
- **import-only-chez**: Require explicit `(only ...)` for chezscheme imports
- **no-emacs-docstrings**: Detect invalid docstring syntax
- **no-rnrs-all**: Disallow importing entire `(rnrs)` library

## Project Structure

```
scheme-lint/
├── src/scheme-lint/       # Core libraries
│   ├── core.sls           # Linting engine
│   ├── reader.sls         # Source reader with location tracking
│   ├── matcher.sls        # Pattern matching
│   └── rules/builtin.sls  # Built-in rules
├── scheme-lint.ss         # CLI tool
├── rules/                 # Default rules
│   ├── standard/          # Production rules
│   └── examples/          # Example rules for learning
└── tests/                 # Test suite
```

## Writing Custom Rules

### Simple DSL Rule

```scheme
(rule no-set-car
  (pattern (set-car! ?pair ?value))
  (where #t)
  (severity warning)
  (message "Avoid set-car! in favor of immutable data structures"))
```

### Rule with Where Clause

```scheme
(rule long-parameter-list
  (pattern (define (?name . ?params) . ?body))
  (where
    (let ((params (get-binding bindings '?params)))
      (> (length params) 5)))
  (severity style)
  (message "Function has more than 5 parameters; consider refactoring"))
```

### Pattern Syntax

- `?var` - Capture single expression
- `(?var ...)` - Capture zero or more expressions (splice)
- `(?head . ?rest)` - Capture dotted pair
- Literals - Must match exactly

## Development

This project uses Nix for reproducible builds:

```bash
# Enter dev environment
nix develop

# Run tests
cd tests
CHEZSCHEMELIBDIRS=../src scheme --script test-core.ss

# Lint the linter itself
scheme-lint --rules rules/standard/ src/scheme-lint/*.sls
```

## Documentation

- [Getting Started](docs/getting-started.md) - Installation and first lint
- [Writing Rules](docs/writing-rules.md) - Complete rule authoring guide
- [Configuration](docs/configuration.md) - Config file format (v2.0+)
- [Plugin API](docs/plugin-api.md) - API reference and stability guarantees

## License

WTFPL - see [LICENSE](LICENSE) for details.
