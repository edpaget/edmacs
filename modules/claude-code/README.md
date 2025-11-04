# Claude Code REPL Integration

![CI](https://github.com/USERNAME/REPO/workflows/CI/badge.svg)
![Checks](https://github.com/USERNAME/REPO/workflows/Checks/badge.svg)
[![Emacs](https://img.shields.io/badge/Emacs-28.1+-purple.svg)](https://www.gnu.org/software/emacs/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**Status**: Phases 1-4 Complete ✅

A beautiful, **true interactive REPL** for Claude Code in Emacs. Type prompts directly in the buffer and send with RET, just like CIDER. Features markdown rendering, syntax highlighting, structured conversation history, and powerful navigation.

## What's Implemented

### Phase 1: Core Process Management ✅
### Phase 2: Response Buffer UI ✅
### Phase 3: Navigation and History ✅
### Phase 4: Interactive REPL Input ✅

## Features

### 🎨 Beautiful Response Buffer

- **Markdown rendering**: Full markdown support with syntax highlighting
- **Structured layout**: Clear sections for prompts, responses, and tools
- **Real-time streaming**: Watch responses appear as Claude types
- **Custom faces**: Distinct styling for different elements
- **Conversation history**: All interactions in one persistent buffer per project
- **Code block support**: Syntax-highlighted code with copy commands

### ⚙️ Robust Process Management

- **Project-aware**: One process per project root (using Projectile)
- **Streaming JSON**: Handles `--output-format stream-json` responses
- **Lifecycle management**: Start, stop, monitor, and error recovery
- **Callback system**: Extensible event handling
- **Process isolation**: Each project has its own Claude instance

### 🛠️ Developer-Friendly Commands

- **Ask Claude**: Quick question/answer with beautiful output
- **Buffer management**: Open, clear, and navigate response buffers
- **Process control**: Start, stop, and monitor Claude processes
- **Debug tools**: Raw event inspection for troubleshooting

### 🧭 Navigation and History (Phase 3)

- **Interaction navigation**: Jump between prompt/response pairs
- **Code block navigation**: Quick movement between code examples
- **Re-send prompts**: Iterate on previous questions with one command
- **Search conversations**: Find text across all interactions
- **Interaction history**: All completed interactions tracked and accessible

### ⌨️ Interactive REPL Input (Phase 4) **NEW!**

- **Type directly in buffer**: No more minibuffer prompts!
- **RET to send**: Press RET in input area to send prompt (like CIDER)
- **Multi-line input**: Use C-j for newlines, RET to send
- **Input history**: M-p/M-n to navigate previous prompts
- **Smart read-only**: Responses are protected, input area is editable
- **Continuous conversation**: Claude remembers previous prompts in the same buffer!
  - Uses session-based continuity with `--resume`
  - Each buffer maintains its own conversation thread
  - No configuration needed - it just works!

## Quick Start

After reloading your Emacs configuration:

```
SPC a c a                    # Open Claude Code REPL
```

A beautiful REPL buffer opens with a "> " prompt. **Now just type and press RET:**

```
> What is the best way to learn Emacs?
[Press RET]

## Response

The best way to learn Emacs is...

> How do I configure key bindings?
[Press RET]

## Response

To configure key bindings in Emacs...
```

Features you'll see:
- Your prompts with timestamps
- Claude's responses streaming in real-time
- Markdown formatting and syntax highlighting
- Token usage and response duration
- Input history with M-p/M-n

**No more switching to the minibuffer - it's a true REPL!**

## Keybindings

### Main Commands (`SPC a c`)

- `SPC a c a` - **Ask Claude** - Start a conversation
- `SPC a c b` - **Open buffer** - View conversation history
- `SPC a c c` - **Clear buffer** - Start fresh
- `SPC a c s` - Start process for current project
- `SPC a c k` - Kill process for current project
- `SPC a c K` - Kill all processes
- `SPC a c l` - List all processes
- `SPC a c i` - Show process status
- `SPC a c t` - Test prompt (debug mode)

### Buffer Mode Commands

When in a Claude Code response buffer:

**Basic Commands:**
- `q` - Quit window
- `g` - Refresh buffer
- `c` - Copy last response to kill ring
- `C-c C-c` - Copy code block at point

**Interactive REPL (Phase 4):** **NEW!**
- `RET` - Send input (when in input area)
- `C-c RET` - Send input (explicit, works anywhere)
- `C-j` - Insert newline without sending
- `M-p` - Previous input from history
- `M-n` - Next input from history

**Navigation (Phase 3):**
- `C-c C-n` - Next interaction
- `C-c C-p` - Previous interaction
- `C-M-n` - Next code block
- `C-M-p` - Previous code block

**Actions (Phase 3):**
- `C-c C-r` - Re-send prompt at point
- `C-c C-s` - Search in interactions

### Testing Phase 1

After loading your Emacs configuration:

1. **Start a process**:
   ```
   M-x claude-code-process-start-current-project
   ```

2. **Quick test**:
   ```
   M-x claude-code-quick-ask
   Prompt: What is 2+2?
   ```

   This will:
   - Create a new Claude process
   - Send your prompt
   - Display streaming responses in `*claude-code-response*` buffer

3. **Debug test**:
   ```
   M-x claude-code-test-prompt
   Prompt: Hello Claude!
   ```

   This shows raw JSON events in `*claude-code-test-output*` buffer for debugging.

4. **View processes**:
   ```
   M-x claude-code-show-processes
   ```

## Architecture

### Process Object Structure

Each Claude Code process is represented by a `claude-code-process` struct:

```elisp
(cl-defstruct claude-code-process
  project-root          ; Where this process belongs
  process               ; Emacs process object
  session-id            ; Claude session ID (for --resume)
  buffer                ; Process output buffer
  partial-json          ; Accumulated partial JSON
  response-callbacks    ; Functions to call on events
  error-callbacks       ; Functions to call on errors
  status                ; running, stopped, error
  last-prompt           ; Last prompt sent
  last-response         ; Last complete response
  metadata)             ; Additional data (model, timestamps, etc.)
```

### JSON Event Flow

1. Claude outputs JSON events line-by-line
2. Process filter accumulates partial lines
3. Complete lines are parsed as JSON
4. Events are dispatched to registered callbacks
5. Callbacks update UI, buffers, etc.

### Event Types

Based on `--output-format stream-json`:

- `message_start` - New message starting
- `content_block_start` - New content block
- `content_block_delta` - Text chunk (streaming)
- `tool_use` - Claude using a tool
- `message_stop` - Response complete

## API Reference

### Starting/Stopping Processes

```elisp
;; Start a process for a specific project
(claude-code-process-start "/path/to/project")

;; Get existing process or create new one
(claude-code-process-get-or-create "/path/to/project")

;; Get current project's process
(claude-code-process-current)

;; Kill a process
(claude-code-process-kill proc-obj)
```

### Sending Prompts

```elisp
;; Send a prompt to a process
(claude-code-process-send-prompt proc-obj "Explain this code")
```

### Callbacks

```elisp
;; Add a response callback
(claude-code-process-add-response-callback
 proc-obj
 (lambda (event)
   (message "Got event: %s" (alist-get 'type event))))

;; Add an error callback
(claude-code-process-add-error-callback
 proc-obj
 (lambda (error-event)
   (message "Process error: %s" error-event)))
```

## Next Steps (Future Phases)

- **Phase 2**: Response buffer UI with markdown rendering
- **Phase 3**: Navigation and history
- **Phase 4**: Enhanced prompt interface
- **Phase 5**: Inline evaluation
- **Phase 6**: Session management
- **Phase 7**: Tool integration visualization
- **Phase 8**: Advanced features

## Known Limitations (Phase 1)

- Raw JSON output (no pretty formatting yet)
- No conversation history UI
- No session persistence across Emacs restarts
- Each prompt creates a new process (no --continue support yet)
- No inline evaluation
- No response buffer management

These will be addressed in future phases.

## Troubleshooting

### Process won't start

Check that `claude` is in your PATH:
```elisp
M-: (executable-find "claude")
```

### No JSON output

Make sure you're using a recent version of Claude Code CLI that supports `--output-format stream-json`.

### Callbacks not firing

Check the process buffer for raw output:
```elisp
M-x claude-code-show-processes
;; Then visit the buffer shown
```

## Development

To add new functionality:

1. Add core functions to `claude-code-process.el`
2. Add user-facing commands to `claude-code-core.el`
3. Register keybindings in `claude-code-core-setup-keybindings`
4. Test with simple prompts first
