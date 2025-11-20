# claude-repl

[![CI](https://github.com/edpaget/edmacs/actions/workflows/claude-repl.yml/badge.svg)](https://github.com/edpaget/edmacs/actions/workflows/claude-repl.yml)
[![Emacs](https://img.shields.io/badge/Emacs-29.1+-purple.svg)](https://www.gnu.org/software/emacs/)
[![License](https://img.shields.io/badge/license-GPL%20v3-blue.svg)](../../LICENSE)

An Emacs integration for the [Claude Code CLI](https://github.com/anthropics/claude-repl). Provides a REPL-style interface with markdown rendering, syntax highlighting, and conversation continuity.

<img width="1512" height="982" alt="Screenshot 2025-11-06 at 10 47 24 AM 1" src="https://github.com/user-attachments/assets/349eeb78-f58f-4468-85b8-704a04ae44cd" />

<img width="1512" height="982" alt="Screenshot 2025-11-06 at 10 49 35 AM" src="https://github.com/user-attachments/assets/61233d7a-644b-4b98-8e28-09a98dbe0966" />

## Installation

### Prerequisites

- Emacs 29.1 or later
- [Claude Code CLI](https://github.com/anthropics/claude-repl) installed and in PATH
- Required Emacs packages: `projectile`, `markdown-mode`

First, install the Claude Code CLI:

```bash
npm install -g @anthropic/claude-code
```

### Install claude-repl

#### Using Emacs 29+ Built-in Package Manager

Add to your Emacs configuration:

```elisp
(use-package claude-repl
  :vc (:url "https://github.com/edpaget/edmacs"
       :branch "main"
       :lisp-dir "modules/claude-repl")
  :after (projectile markdown-mode)
  :config
  ;; Optional: Configure auto-approval for safe tools
  (setq claude-repl-approval-mode 'hybrid)

  ;; Optional: Set up custom keybindings (see Custom Keybindings section below)
  (global-set-key (kbd "C-c a a") #'claude-repl-ask)
  (global-set-key (kbd "C-c a b") #'claude-repl-open-buffer)
  (global-set-key (kbd "C-c a k") #'claude-repl-process-kill-current-project))
```

Then run `M-x package-vc-install RET` and select `claude-repl`, or evaluate the above configuration and restart Emacs.

#### Using straight.el

```elisp
(use-package claude-repl
  :straight (claude-repl :type git
                         :host github
                         :repo "edpaget/edmacs"
                         :files ("modules/claude-repl/*.el"))
  :after (projectile markdown-mode)
  :config
  ;; Optional: Configure auto-approval for safe tools
  (setq claude-repl-approval-mode 'hybrid)

  ;; Optional: Set up custom keybindings (see Custom Keybindings section below)
  (global-set-key (kbd "C-c a a") #'claude-repl-ask)
  (global-set-key (kbd "C-c a b") #'claude-repl-open-buffer)
  (global-set-key (kbd "C-c a k") #'claude-repl-process-kill-current-project))
```

#### Manual Installation

```bash
# Clone the repository
git clone https://github.com/edpaget/edmacs.git
cd edmacs/modules/claude-repl
```

Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/edmacs/modules/claude-repl")
(require 'claude-repl-core)

;; Optional: Configure auto-approval for safe tools
(setq claude-repl-approval-mode 'hybrid)

;; Optional: Set up custom keybindings (see Custom Keybindings section below)
(global-set-key (kbd "C-c a a") #'claude-repl-ask)
(global-set-key (kbd "C-c a b") #'claude-repl-open-buffer)
(global-set-key (kbd "C-c a k") #'claude-repl-process-kill-current-project)
```

### Configuration Options

#### Tool Approval Modes

Control how tool usage requests are handled:

```elisp
;; Ask for approval on every tool use (default)
(setq claude-repl-approval-mode 'ask)

;; Auto-approve safe read-only tools, ask for risky ones (recommended)
(setq claude-repl-approval-mode 'hybrid)

;; Auto-approve all tools (use with caution)
(setq claude-repl-approval-mode 'allow)
```

Safe auto-approved tools in hybrid mode: `Read`, `Grep`, `Glob`, `WebFetch`, `WebSearch`, `TodoWrite`

#### Custom Keybindings

The claude-repl module doesn't define global keybindings by default. You should set up your own keybindings in your configuration. Here are some examples:

```elisp
;; Basic example - simple global keybindings
(with-eval-after-load 'claude-repl-core
  (global-set-key (kbd "C-c a a") #'claude-repl-ask)
  (global-set-key (kbd "C-c a b") #'claude-repl-open-buffer)
  (global-set-key (kbd "C-c a c") #'claude-repl-clear-buffer)
  (global-set-key (kbd "C-c a s") #'claude-repl-process-start-current-project)
  (global-set-key (kbd "C-c a k") #'claude-repl-process-kill-current-project))

;; Or using use-package :bind
(use-package claude-repl
  :bind (("C-c a a" . claude-repl-ask)
         ("C-c a b" . claude-repl-open-buffer)
         ("C-c a c" . claude-repl-clear-buffer)))
```

For evil-mode/general.el integration (Spacemacs-style), see your editor configuration.

## Features

### Interactive REPL

- Type prompts directly in the buffer and send with `RET`
- Multi-line input with `C-j` for newlines
- Input history navigation with `M-p`/`M-n`
- Session continuity: conversations persist within each buffer using `--resume`
- Real-time response streaming

### Response Buffer

- Markdown rendering with syntax highlighting
- Structured layout for prompts, responses, and tool output
- Code block navigation and copying
- Per-project conversation buffers

### Process Management

- One Claude process per project (uses Projectile)
- Handles `--output-format stream-json` from Claude CLI
- Process lifecycle management (start, stop, monitor)
- Extensible callback system for events

### Navigation

- Jump between prompt/response pairs
- Navigate between code blocks
- Re-send previous prompts
- Search within conversations
- Interaction history tracking

### Tool Approval System

- Interactive approval prompts for file operations and bash commands
- Configurable auto-approval patterns via hooks
- Integration with evil-mode for vim-style navigation in approval buffers

## Quick Start

Start a conversation with `M-x claude-repl-ask` (or use your configured keybinding, e.g., `C-c a a`).

Type your prompt at the `claude> ` prompt and press `RET` to send. Use `C-j` for newlines within multi-line prompts. Responses stream in real-time with markdown formatting and syntax highlighting.

## Keybindings

The claude-repl module provides keybindings in the buffer itself, but doesn't set up global keybindings automatically. You can set up your own keybindings for the main commands (see the Custom Keybindings section above).

### Available Commands

- `claude-repl-ask` - Start a conversation or ask a question
- `claude-repl-interrupt-and-ask` - Interrupt current process and ask new question
- `claude-repl-open-buffer` - Open the conversation buffer
- `claude-repl-clear-buffer` - Clear the conversation buffer
- `claude-repl-process-start-current-project` - Start Claude process for current project
- `claude-repl-process-kill-current-project` - Kill process for current project
- `claude-repl-process-kill-all` - Kill all Claude processes
- `claude-repl-show-processes` - List all running Claude processes
- `claude-repl-process-status-current-project` - Show process status

### Response Buffer Commands

- `q` - Quit window
- `g` - Refresh buffer
- `c` - Copy last response to kill ring
- `C-c C-c` - Copy code block at point

**Input:**
- `RET` - Send input (when in input area)
- `C-c RET` - Send input (works anywhere)
- `C-j` - Insert newline without sending
- `M-p` - Previous input from history
- `M-n` - Next input from history

**Navigation:**
- `C-c C-n` - Next interaction
- `C-c C-p` - Previous interaction
- `C-M-n` - Next code block
- `C-M-p` - Previous code block

**Actions:**
- `C-c C-r` - Re-send prompt at point
- `C-c C-s` - Search in interactions

## Architecture

Each Claude Code process is represented by a `claude-repl-process` struct containing the project root, Emacs process object, session ID, callbacks, and metadata. The module manages one process per project root (determined via Projectile).

JSON events from `--output-format stream-json` are processed line-by-line:
1. Process filter accumulates partial lines
2. Complete lines are parsed as JSON
3. Events are dispatched to registered callbacks
4. Callbacks update the UI and response buffer

Main event types: `message_start`, `content_block_start`, `content_block_delta`, `tool_use`, `message_stop`.

## Project Structure

```
modules/claude-repl/
├── claude-repl.el           # Main entry point
├── claude-repl-core.el      # User commands
├── claude-repl-process.el   # Process management
├── claude-repl-buffer.el    # Buffer management and UI
├── claude-repl-approval.el  # Tool approval system
├── test/
│   ├── test-helper.el                  # Test utilities
│   ├── claude-repl-approval-test.el
│   ├── claude-repl-buffer-test.el
│   ├── claude-repl-core-test.el
│   └── claude-repl-process-test.el
└── Eldev                    # Build configuration
```

## Development

This project uses [Eldev](https://github.com/doublep/eldev) for building and testing.

### Running Tests

```bash
eldev test                  # Run all tests
eldev test approval         # Run tests matching pattern
```

Tests use the [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup) framework. Test files are in `test/` and must end with `-test.el`.

### Compilation and Linting

```bash
eldev compile               # Byte-compile all files
eldev lint                  # Run linters (checkdoc, byte-compile warnings)
eldev clean                 # Remove compiled files
```

### Adding Functionality

1. Add core functions to the appropriate module (`claude-repl-process.el`, `claude-repl-buffer.el`, etc.)
2. Add user-facing commands to `claude-repl-core.el`
3. Add tests to the corresponding test file in `test/`
4. Run `eldev test` and `eldev compile` to verify

Note: Global keybindings should be configured by users in their own configuration (not in the module itself).

## License

GNU General Public License v3.0 or later. See the [LICENSE](../../LICENSE) file for details.
