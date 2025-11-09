# edmacs - Instructions for Claude

This document provides guidance for Claude when working on this Emacs configuration repository.

## Repository Overview

This is **edmacs**, a modern, modular Emacs configuration with evil-mode, version-locked packages, and AI assistant integration. The repository contains:

- Top-level Emacs configuration files (`init.el`, `early-init.el`, etc.)
- Multiple configuration modules in `modules/` directory
- A standalone `claude-repl` module in `modules/claude-repl/` (formerly called claude-code)

## Working on the claude-repl Module

The `claude-repl` module (located at `modules/claude-repl/`) is a standalone Emacs package that provides AI assistant integration. When working specifically on the claude-repl module:

### Testing

**IMPORTANT**: The claude-repl module uses [Eldev](https://github.com/doublep/eldev) as its build and test tool.

**You must run eldev commands from within the `modules/claude-repl/` directory.**

#### Running Tests

- **Always use `eldev test` to run tests** - DO NOT create standalone test scripts
- Run all tests: `cd modules/claude-repl && eldev test`
- Run specific test pattern: `cd modules/claude-repl && eldev test <pattern>` (e.g., `eldev test approval`)
- The test framework is [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup)

#### Writing Tests

- All tests belong in the `modules/claude-repl/test/` directory
- Test files must end with `-test.el` (e.g., `claude-repl-approval-test.el`)
- Use the existing test structure and helpers from `test/test-helper.el`
- **NEVER** create temporary test files like `/tmp/test-*.el` - add tests to the appropriate test file in `test/`

Example test structure:
```elisp
(describe "Feature name"
  (it "does something specific"
    (expect (some-function) :to-equal expected-value)))
```

### Compilation

**IMPORTANT**: Use eldev for byte compilation (from within `modules/claude-repl/`).

- Compile the project: `cd modules/claude-repl && eldev compile`
- Clean compiled files: `cd modules/claude-repl && eldev clean`
- **DO NOT** use `emacs --batch` for compilation - use `eldev compile` instead

### Linting

- Lint the code: `cd modules/claude-repl && eldev lint`
- This runs byte-compilation warnings, checkdoc, and other linters

### claude-repl Module Structure

```
modules/claude-repl/
├── claude-repl-core.el      # Core commands
├── claude-repl-process.el   # Process management
├── claude-repl-buffer.el    # Buffer management and UI
├── claude-repl-approval.el  # Tool approval system
├── approval-hook.py         # Approval hook script (handles large responses)
├── test/
│   ├── test-helper.el                    # Test utilities
│   ├── claude-repl-approval-test.el
│   ├── claude-repl-buffer-test.el
│   ├── claude-repl-buffer-autoscroll-test.el
│   ├── claude-repl-core-test.el
│   └── claude-repl-process-test.el
├── Eldev                    # Eldev configuration
└── README.md                # Module documentation
```

## Overall Repository Structure

```
edmacs/
├── early-init.el           # Performance optimizations
├── init.el                 # Main entry point
├── modules/                # Configuration modules
│   ├── core.el            # Basic settings
│   ├── evil-config.el     # Evil mode configuration
│   ├── keybindings.el     # General.el keybindings
│   ├── ui.el              # Theme and appearance
│   ├── completion.el      # Vertico, Corfu, Consult
│   ├── programming.el     # LSP, Flycheck, Apheleia
│   ├── ai.el              # AI assistant integration
│   ├── org-config.el      # Org mode configuration
│   ├── git.el             # Magit and git tools
│   ├── vterm.el           # Terminal configuration
│   ├── claude-repl/       # Standalone claude-repl package
│   ├── languages/         # Language-specific configs
│   └── tiles/             # Tiling window management
├── straight/
│   └── versions/          # Package version lockfiles (committed)
└── README.md              # Main repository documentation
```

### Development Workflow for claude-repl

When working on the claude-repl module:

1. Navigate to the module directory: `cd modules/claude-repl`
2. Make changes to source files
3. Add or update tests in the corresponding test file
4. Run tests: `eldev test`
5. Compile to check for warnings: `eldev compile`
6. Lint the code: `eldev lint`

### Evil Mode Integration

The approval buffer (`claude-repl-approval-mode`) is configured to work with evil-mode:
- Sets initial state to `motion` when evil-mode is available
- Forces motion state when approval buffer is displayed
- This allows vim motions (h/j/k/l, etc.) while keeping approval keybindings (a/d/A/D) active
- Provides the best of both worlds: vim navigation + approval shortcuts
- After making a decision, automatically returns to the buffer you were in before approval

### Message Control

The approval system has a `claude-repl-approval-silent` customization variable:
- Default is `t` (silent mode enabled)
- When enabled, suppresses informational messages to prevent minibuffer growth
- Errors and important notifications are still shown
- Can be set to `nil` for verbose debugging output

## Tool Approval - Safe Tools

The following tools are considered safe and should be auto-approved in hybrid or auto-approve modes:

### Read-Only Tools (Always Safe)
- `Read` - Reading files
- `Grep` - Searching file contents
- `Glob` - Finding files by pattern
- `WebFetch` - Fetching web pages
- `WebSearch` - Web searches

### Todo Management (Safe)
- `TodoWrite` - Managing the todo list for tracking progress

These tools don't modify the filesystem or execute arbitrary code, so they can be safely auto-approved.

### Tools Requiring Approval
- `Write` - Creating/overwriting files
- `Edit` - Modifying existing files
- `Bash` - Executing shell commands
- `Task` - Launching subagents

To configure auto-approval in your Emacs configuration:

```elisp
;; Set hybrid mode (safe tools auto, risky ask)
(setq claude-repl-approval-mode 'hybrid)

;; Or add specific approval rules
(setq claude-repl-approval-rules
      '((tool "TodoWrite" action allow)
        (tool "Read" action allow)
        (tool "Grep" action allow)))
```

## Best Practices

- **Test-driven development**: Add tests before or alongside code changes
- **Use existing test infrastructure**: Don't create ad-hoc test files
- **Follow the module pattern**: Keep related functionality in the appropriate module
- **Byte-compile clean**: Code should compile without warnings
- **Use eldev for all operations**: Consistent build environment across development
- **Use TodoWrite actively**: Track progress on multi-step tasks to help users understand what's being done
