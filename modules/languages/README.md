# Language Modules

Language-specific configurations for edmacs. Each module provides optimized settings, LSP integration, and language-specific tools.

## Overview

| Language | File | Indentation | LSP Support | REPL |
|----------|------|-------------|-------------|------|
| JavaScript/TypeScript | `javascript.el` | 2 spaces | ✅ typescript-language-server | ❌ |
| Clojure/ClojureScript | `clojure.el` | Lisp-style | ✅ clojure-lsp | ✅ CIDER |
| Java | `java.el` | 4 spaces | ✅ Eclipse JDT.LS | ❌ |

---

## JavaScript and TypeScript

**File:** `javascript.el`

### Features

- **JavaScript Support**: `.js`, `.mjs`, `.cjs` files
- **TypeScript Support**: `.ts` files
- **JSX/TSX Support**: `.jsx` and `.tsx` files
- **JSON Support**: `.json` files with formatting
- **2-Space Indentation**: Default for all JS/TS files
- **LSP Integration**: Automatic TypeScript language server
- **Prettier Formatting**: Automatic formatting on save via Apheleia
- **ESLint Support**: Auto-fix on save

### Indentation

All JavaScript and TypeScript files default to 2-space indentation:

- `js-indent-level: 2`
- `typescript-indent-level: 2`
- `json-reformat:indent-width: 2`

### File Associations

| Extension | Mode |
|-----------|------|
| `.js`, `.mjs`, `.cjs` | `js-mode` |
| `.jsx` | `rjsx-mode` |
| `.ts` | `typescript-mode` |
| `.tsx` | `typescript-tsx-mode` |
| `.json` | `json-mode` |

### LSP Configuration

The module configures the TypeScript language server with:

- Auto-imports enabled
- Relative import paths
- Single quotes preference
- ESLint auto-fix on save

### Prerequisites

Install the TypeScript language server:

```bash
npm install -g typescript typescript-language-server
```

Optionally install ESLint:

```bash
npm install -g eslint
```

For formatting, ensure Prettier is available:

```bash
npm install -g prettier
```

### Packages Used

- **typescript-mode**: TypeScript and TSX support
- **rjsx-mode**: Enhanced JSX editing
- **json-mode**: JSON file support
- **lsp-mode**: Language server integration
- **apheleia**: Async formatting with Prettier

---

## Clojure and ClojureScript

**File:** `clojure.el`

### Features

- **Clojure Support**: `.clj` files
- **ClojureScript Support**: `.cljs` files
- **Cross-platform Clojure**: `.cljc` files
- **EDN Support**: `.edn` files
- **CIDER REPL**: Interactive development with nREPL
- **LSP Integration**: clojure-lsp for navigation and refactoring
- **Automatic Indentation**: Lisp-style with align-arguments
- **Parenthesis Management**: smartparens and rainbow-delimiters
- **Refactoring Tools**: clj-refactor integration

### File Associations

| Extension | Mode |
|-----------|------|
| `.clj` | `clojure-mode` |
| `.cljs` | `clojurescript-mode` |
| `.cljc` | `clojurec-mode` |
| `.edn` | `clojure-mode` |

### CIDER Features

- Interactive REPL with syntax highlighting
- Inline evaluation and documentation
- Test runner integration
- Debugger support
- Code completion via company-mode
- Jump to definition/source

### Prerequisites

For Clojure development:

```bash
# Install Clojure CLI tools
brew install clojure/tools/clojure  # macOS
# or download from https://clojure.org/guides/install_clojure

# Install clojure-lsp
brew install clojure-lsp/brew/clojure-lsp-native  # macOS
# or download from https://clojure-lsp.io/installation/
```

For ClojureScript development:

```bash
npm install -g shadow-cljs  # or use other ClojureScript build tools
```

### Keybindings

CIDER provides extensive keybindings under `,` (local leader) in Clojure files:

- `,eb` - Evaluate buffer
- `,ef` - Evaluate function
- `,er` - Evaluate region
- `,ee` - Evaluate last expression
- `,si` - Start REPL (jack-in)
- `,sq` - Quit REPL
- `,tt` - Run tests
- `,tn` - Run tests in namespace

### Packages Used

- **clojure-mode**: Major mode for Clojure
- **cider**: Interactive development environment
- **clj-refactor**: Automated refactoring
- **flycheck-clj-kondo**: Linting with clj-kondo
- **lsp-mode**: Language server integration

---

## Java

**File:** `java.el`

### Features

- **Java Support**: `.java` files
- **LSP Integration**: Eclipse JDT.LS for IntelliSense
- **4-Space Indentation**: Standard Java formatting
- **Build Tool Integration**: Maven and Gradle support
- **DAP Debugging**: Debug Adapter Protocol support
- **Test Runner**: JUnit integration
- **Project Management**: Automatic project detection
- **Import Organization**: Automatic import management

### Indentation

Java files use 4-space indentation:

```elisp
c-basic-offset: 4
tab-width: 4
indent-tabs-mode: nil
```

### LSP Configuration

The Eclipse JDT Language Server provides:

- Code completion with IntelliSense
- Hover documentation
- Go to definition/references
- Code actions and quick fixes
- Refactoring support
- Organize imports
- Format on save

### Prerequisites

The LSP Java package automatically downloads Eclipse JDT.LS on first use. No manual installation required!

Optional tools:

```bash
# Maven (if not already installed)
brew install maven  # macOS

# Gradle (if not already installed)
brew install gradle  # macOS
```

### DAP Debugging

Java debugging is available through DAP mode:

1. Set breakpoints with `SPC c d b`
2. Start debugging with `SPC c d d`
3. Step through code, inspect variables, etc.

### Keybindings

Standard LSP keybindings under `SPC c`:

- `SPC c a` - Code action
- `SPC c r` - Rename symbol
- `SPC c f` - Format buffer
- `SPC c d` - Go to definition
- `SPC c R` - Find references
- `SPC c i` - Organize imports

### Packages Used

- **lsp-java**: Eclipse JDT Language Server integration
- **dap-java**: Debugging support
- **lsp-treemacs**: Project explorer
- **flycheck**: Syntax checking

---

## Adding New Languages

To add a new language module:

1. Create `modules/languages/LANGUAGE.el`
2. Configure the language mode and LSP settings
3. Add `(require 'LANGUAGE)` to `modules/programming.el`

Example structure:

```elisp
;;; python.el --- Python configuration -*- lexical-binding: t -*-

;;; Code:

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred)
  :config
  (setq python-indent-offset 4))

(provide 'python)
;;; python.el ends here
```

## License

This module is part of the edmacs configuration and is licensed under the GNU General Public License v3.0 or later.
