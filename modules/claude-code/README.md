# Claude Code REPL Integration

**Status**: Phases 1-2 Complete ✅

A beautiful, REPL-style Claude Code integration for Emacs with markdown rendering, syntax highlighting, and structured conversation history.

## What's Implemented

### Phase 1: Core Process Management ✅
### Phase 2: Response Buffer UI ✅

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

## Quick Start

After reloading your Emacs configuration:

```
SPC a c a
"What is the best way to learn Emacs?"
```

A beautiful response buffer will open showing:
- Your prompt with timestamp
- Claude's response streaming in real-time
- Markdown formatting and syntax highlighting
- Token usage when complete

Continue the conversation by asking more questions - all interactions stay in the same buffer!

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

- `q` - Quit window
- `g` - Refresh buffer
- `c` - Copy last response to kill ring
- `C-c C-c` - Copy code block at point

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
