# Development Guide for Claude Code REPL

## Using Eldev for Development

This project uses [Eldev](https://github.com/emacs-eldev/eldev) as its build tool for testing, linting, and package management.

## Installation

### Install Eldev

```bash
# macOS/Linux
curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh

# Or via Homebrew (macOS)
brew install eldev

# Or manually (download to PATH)
wget https://raw.github.com/emacs-eldev/eldev/master/bin/eldev
chmod +x eldev
sudo mv eldev /usr/local/bin/
```

### Verify Installation

```bash
eldev --version
```

## Quick Start

```bash
# Navigate to the claude-code directory
cd /Users/edward/Projects/edmacs/modules/claude-code

# Install dependencies
eldev prepare

# Run tests
eldev test

# Byte-compile the project
eldev compile

# Run all checks (compile + test + lint)
eldev check
```

## Common Commands

### Testing

```bash
# Run all tests
eldev test

# Run tests verbosely
eldev -t test

# Run specific test file
eldev test test/claude-code-process-test.el

# Run tests matching a pattern
eldev test --pattern "process creation"

# Run tests with debugging
eldev -d test
```

### Building

```bash
# Byte-compile all source files
eldev compile

# Clean build artifacts
eldev clean

# Clean everything including dependencies
eldev clean all
```

### Linting

```bash
# Lint all source files
eldev lint

# Lint specific file
eldev lint claude-code-process.el

# Re-lint (force re-check)
eldev lint --force
```

### Custom Commands

```bash
# Run quick demo/smoke test
eldev demo

# Run all checks (compile + test + lint)
eldev check
```

### Dependencies

```bash
# Install/update dependencies
eldev prepare

# List dependencies
eldev dependency-tree

# Upgrade dependencies
eldev upgrade-self
```

### Development Workflow

```bash
# Start development session
eldev emacs

# Evaluate Elisp expression
eldev eval "(+ 1 2)"

# Load project in REPL
eldev exec "(require 'claude-code-core)"
```

## Project Structure

```
claude-code/
├── Eldev                          # Eldev configuration
├── claude-code-process.el         # Core process management
├── claude-code-buffer.el          # Response buffer UI
├── claude-code-core.el            # High-level commands
├── test/
│   ├── test-helper.el            # Test utilities
│   ├── claude-code-process-test.el
│   ├── claude-code-buffer-test.el
│   └── claude-code-integration-test.el
├── README.md                      # User documentation
├── TESTING.md                     # Testing guide
├── DEVELOPMENT.md                 # This file
└── PHASE*_COMPLETE.md            # Implementation docs
```

## Eldev Configuration

The `Eldev` file configures:

### Dependencies

- **Runtime**: `markdown-mode`, `projectile`
- **Testing**: `buttercup`
- **Linting**: `package-lint`

### Test Framework

- Uses Buttercup for BDD-style tests
- Auto-loads `test/test-helper.el` before running tests
- Test files: `test/*.el` (except test-helper.el)

### Build Settings

- Byte-compiles all `.el` files
- Loads dependencies before compilation
- Can treat warnings as errors (strict mode)

### Custom Commands

- `eldev demo` - Quick smoke test
- `eldev check` - Run all checks (compile + test + lint)

## Testing Workflow

### 1. Write Tests

```elisp
;; test/claude-code-feature-test.el
(require 'test-helper)

(describe "My new feature"
  (it "does something useful"
    (expect (my-function) :to-equal expected-value)))
```

### 2. Run Tests

```bash
# Run all tests
eldev test

# Run specific test
eldev test test/claude-code-feature-test.el

# Run with verbose output
eldev -t test
```

### 3. Fix Failures

```bash
# Debug failing test
eldev -d test --pattern "does something useful"
```

### 4. Check Coverage

```bash
# Run all checks
eldev check
```

## Continuous Integration

### Local CI Simulation

```bash
# Simulate CI environment
CI=true eldev check

# Test against different Emacs versions (requires Docker)
eldev -e 28.2 test
eldev -e 29.1 test
```

### GitHub Actions

Create `.github/workflows/test.yml`:

```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        emacs_version:
          - '28.2'
          - '29.1'
          - 'snapshot'

    steps:
      - uses: actions/checkout@v3

      - uses: purcell/setup-emacs@master
        with:
          version: ${{ matrix.emacs_version }}

      - uses: actions/cache@v3
        with:
          path: ~/.cache/eldev
          key: ${{ runner.os }}-eldev-${{ hashFiles('**/Eldev') }}

      - name: Install Eldev
        run: curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh

      - name: Run tests
        run: ~/.local/bin/eldev -t test

      - name: Lint
        run: ~/.local/bin/eldev lint
```

## Development Tips

### Interactive Development

```bash
# Start Emacs with project loaded
eldev emacs

# Then in Emacs:
M-x buttercup-run-discover
```

### Quick Iteration

```bash
# Watch for changes and re-run tests (requires entr or similar)
ls *.el test/*.el | entr eldev test

# Or use Emacs file watches
eldev emacs
# Then: M-x auto-revert-mode
```

### Debugging

```bash
# Run with full stack traces
eldev -d test

# Enable Eldev tracing
eldev -t compile

# Check dependency issues
eldev dependency-tree
```

### Performance

```bash
# Profile a function
eldev exec "(require 'profiler) (profiler-start 'cpu) (my-function) (profiler-report)"

# Benchmark tests
eldev test --benchmark
```

## Package Management

### Adding Dependencies

Edit the `Eldev` file:

```elisp
;; Add runtime dependency
(eldev-add-extra-dependencies 'runtime
  'new-package)

;; Add test dependency
(eldev-add-extra-dependencies 'test
  'new-test-package)
```

Then:

```bash
eldev prepare
```

### Version Constraints

```elisp
;; Require specific version
(eldev-add-extra-dependencies 'runtime
  '(markdown-mode "2.5"))

;; Require minimum version
(eldev-add-extra-dependencies 'runtime
  '(projectile "2.7.0"))
```

## Troubleshooting

### Dependencies Not Found

```bash
# Clean and reinstall
eldev clean all
eldev prepare
```

### Test Failures

```bash
# Run specific test with debug output
eldev -d test --pattern "failing test name"

# Check test isolation
eldev test test/specific-test.el
```

### Byte-Compilation Warnings

```bash
# See full warnings
eldev -t compile

# Fix and recompile
eldev clean
eldev compile
```

### Eldev Issues

```bash
# Update Eldev itself
eldev upgrade-self

# Check Eldev configuration
eldev doctor

# Get help
eldev help
eldev help test
```

## Best Practices

### 1. Run Tests Before Committing

```bash
# Quick check
eldev test

# Full check
eldev check
```

### 2. Keep Dependencies Minimal

Only add dependencies that are truly needed.

### 3. Write Tests for New Features

Always write tests alongside new code.

### 4. Use Consistent Style

```bash
# Check style
eldev lint
```

### 5. Document Public APIs

All public functions should have docstrings.

## Makefile Integration (Optional)

Create a `Makefile` for convenience:

```makefile
.PHONY: test compile lint check clean prepare demo

test:
	eldev test

compile:
	eldev compile

lint:
	eldev lint

check:
	eldev check

clean:
	eldev clean

prepare:
	eldev prepare

demo:
	eldev demo

install-eldev:
	curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh
```

Then use:

```bash
make test
make check
```

## IDE Integration

### VS Code with Emacs extension

Add to `.vscode/tasks.json`:

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Eldev Test",
      "type": "shell",
      "command": "eldev test",
      "group": {
        "kind": "test",
        "isDefault": true
      }
    }
  ]
}
```

### Emacs itself

```elisp
;; In your .emacs or init.el
(defun my-eldev-test ()
  "Run eldev test in project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "eldev test")))

(define-key projectile-mode-map (kbd "C-c p T") #'my-eldev-test)
```

## Resources

- [Eldev Documentation](https://emacs-eldev.github.io/eldev/)
- [Buttercup Documentation](https://github.com/jorgenschaefer/emacs-buttercup)
- [Package Development Handbook](https://alphapapa.github.io/emacs-package-dev-handbook/)
- [Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide)

## Getting Help

- Check `eldev help COMMAND` for command-specific help
- See [Eldev issues](https://github.com/emacs-eldev/eldev/issues)
- Ask in `#emacs` on Libera.Chat IRC

## Next Steps

1. Install Eldev: `curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh`
2. Prepare project: `eldev prepare`
3. Run demo: `eldev demo`
4. Run tests: `eldev test`
5. Start developing! 🚀
