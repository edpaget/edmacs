# Language Modules

Language-specific configurations for edmacs. Each module provides optimized settings, eglot integration, and language-specific tools.

## Overview

| Language | File | Indentation | Language Server | REPL |
|----------|------|-------------|-------------|------|
| JavaScript/TypeScript | `javascript.el` | 2 spaces | ✅ TypeScript 7 `tsc --lsp`, else typescript-language-server | ❌ |
| Clojure/ClojureScript | `clojure.el` | Lisp-style | ✅ clojure-lsp | ✅ CIDER |
| Java | `java.el` | 4 spaces | ✅ eglot + jdtls (navigation-only) | ❌ |

---

## JavaScript and TypeScript

**File:** `javascript.el`

### Features

- **JavaScript Support**: `.js`, `.mjs`, `.cjs` files
- **TypeScript Support**: `.ts` files
- **JSX/TSX Support**: `.jsx` and `.tsx` files
- **JSON Support**: `.json` files with formatting
- **2-Space Indentation**: Default for all JS/TS files
- **Language Server**: eglot, contacting TypeScript 7's own `tsc --lsp` when
  the project's `tsc` is 7 or newer and `typescript-language-server` otherwise
- **Prettier Formatting**: Automatic formatting on save via Apheleia

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

### Language Server Configuration

The module configures the TypeScript language server with:

- Auto-imports enabled
- Relative import paths
- Single quotes preference

ESLint is not wired in: eglot runs one server per project and mode, and the
TypeScript server owns that slot. Tracked as task
`eslint-diagnostics-under-flymake`.

### Prerequisites

Install TypeScript; 7 or newer serves LSP itself, older versions also need
the separate server:

```bash
npm install -g typescript typescript-language-server
```

For formatting, ensure Prettier is available:

```bash
npm install -g prettier
```

### Packages Used

- **typescript-mode**: TypeScript and TSX support
- **rjsx-mode**: Enhanced JSX editing
- **json-mode**: JSON file support
- **eglot**: Language server integration
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
- **Language Server**: clojure-lsp, via eglot, for navigation and refactoring
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
- **eglot**: Language server integration

---

## Java

**File:** `java.el`

### Features

Navigation-only, by deliberate choice (roadmap `edmacs-builtins`, phase 5):
Java gets eglot/jdtls for code intelligence and nothing beyond it. The
jdtls-protocol refactoring/generation commands (organize imports, add
unimplemented methods, generate getters/setters/toString/equals-hashCode,
type hierarchy) and the whole debugger integration that used to live here
are gone, not ported -- see that phase's landing commit for the full list.

- **Java Support**: `.java` files
- **Language Server**: eglot talking to jdtls (Eclipse JDT Language Server)
- **4-Space Indentation**: Standard Java formatting
- **Build Tool Integration**: Maven and Gradle support (`mvn.el` / `gradle-mode`, unrelated to jdtls)
- **Semantic Tokens**: jdtls' semantic highlighting via `eglot-semantic-tokens-mode` (wired for every eglot-managed buffer in `programming.el`)

### Indentation

Java files use 4-space indentation:

```elisp
java-ts-mode-indent-offset: 4
tab-width: 4
indent-tabs-mode: nil
```

### Language Server Configuration

jdtls, via eglot, provides:

- Code completion (through corfu)
- Hover documentation
- Go to definition/references/implementation/type definition
- Code actions and quick fixes
- Rename
- Workspace symbols
- Diagnostics (via flymake)

### Prerequisites

jdtls is **not** bundled and does not auto-install -- install it by hand:

1. Add a `[tools.jdtls]` entry to `~/.config/mise/config.toml` (see the
   file's existing comments for the exact form; jdtls ships tarballs, not
   proper GitHub releases, so it goes through an asdf plugin rather than
   mise's `ubi:`/`github:` backends) and run `mise install`.
2. Confirm it resolves through a **login** shell, not just your current
   one: `$SHELL -l -c 'command -v jdtls'`. This matters because the Emacs
   daemon gets its `PATH` from `exec-path-from-shell` (`modules/core.el`),
   which only sees what a login shell exports -- a jdtls that only
   resolves interactively is invisible to eglot, and the failure mode is a
   silent non-attach with no error.

eglot's bundled `eglot-server-programs` entry already contacts a bare
`jdtls` on `PATH`; no custom entry is needed once the above resolves.

Optional build-tool CLIs (only needed if you don't use the project's own
wrapper script):

```bash
# Maven (if not already installed)
brew install maven  # macOS

# Gradle (if not already installed)
brew install gradle  # macOS
```

### Keybindings

Standard eglot/xref keybindings under `SPC c` (wired globally in
`programming.el`, not by this file):

- `SPC c a` - Code action
- `SPC c r` - Rename symbol
- `SPC c f` - Format buffer
- `SPC c d` - Go to definition
- `SPC c R` - Find references
- `SPC c i` - Go to implementation
- `SPC c t` - Go to type definition
- `SPC c s` - Workspace symbols

Maven and Gradle keep their own local-leader prefixes, `, m` and `, g`.

### Packages Used

- **eglot** (built-in): talks to jdtls over LSP
- **jdtls**: the Java language server, installed via mise (see Prerequisites) -- not an Emacs package
- **mvn**: Maven integration
- **gradle-mode**: Gradle integration
- **flymake** (built-in): diagnostics

---

## Adding New Languages

To add a new language module:

1. Create `modules/languages/LANGUAGE.el`
2. Configure the language mode and its eglot settings
3. Add `(require 'LANGUAGE)` to `modules/programming.el`

Example structure:

```elisp
;;; python.el --- Python configuration -*- lexical-binding: t -*-

;;; Code:

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . eglot-ensure)
  :config
  (setq python-indent-offset 4))

(provide 'python)
;;; python.el ends here
```

## License

This module is part of the edmacs configuration and is licensed under the GNU General Public License v3.0 or later.
