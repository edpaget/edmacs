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
npm install -g @anthropic/claude-repl
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
  ;; Set up keybindings (Spacemacs-style by default)
  (claude-repl-setup-keybindings)

  ;; Optional: Configure auto-approval for safe tools
  (setq claude-repl-approval-mode 'hybrid))
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
  (claude-repl-setup-keybindings)

  ;; Optional: Configure auto-approval for safe tools
  (setq claude-repl-approval-mode 'hybrid))
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
(claude-repl-setup-keybindings)

;; Optional: Configure auto-approval for safe tools
(setq claude-repl-approval-mode 'hybrid)
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

If you don't use Spacemacs-style keybindings, you can set up your own:

```elisp
(with-eval-after-load 'claude-repl-core
  (global-set-key (kbd "C-c a a") #'claude-repl-start-conversation)
  (global-set-key (kbd "C-c a b") #'claude-repl-show-buffer)
  (global-set-key (kbd "C-c a k") #'claude-repl-kill-process))
```

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

With Spacemacs keybindings:

```
SPC a c a                    # Open Claude Code REPL
```

Type your prompt at the `> ` prompt and press `RET` to send. Use `C-j` for newlines within multi-line prompts. Responses stream in real-time with markdown formatting and syntax highlighting.

## Keybindings

### Main Commands (`SPC a c`)

- `SPC a c a` - Start a conversation
- `SPC a c b` - Open conversation buffer
- `SPC a c c` - Clear buffer
- `SPC a c s` - Start process for current project
- `SPC a c k` - Kill process for current project
- `SPC a c K` - Kill all processes
- `SPC a c l` - List all processes
- `SPC a c i` - Show process status

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
3. Register keybindings in `claude-repl-core-setup-keybindings`
4. Add tests to the corresponding test file in `test/`
5. Run `eldev test` and `eldev compile` to verify

## License

GNU General Public License v3.0 or later. See the [LICENSE](../../LICENSE) file for details.
