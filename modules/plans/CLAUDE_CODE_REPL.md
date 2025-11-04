# Claude Code REPL-Style Integration Plan

## Overview
Replace the current vterm-based Claude Code integration with a REPL-like interface using Claude Code's non-interactive mode (`--print`). The goal is to create an experience similar to Cider (Clojure REPL in Emacs), where users can:
- Send code/prompts to Claude and receive responses inline
- Have a dedicated response buffer with structured output
- Maintain conversation history per project
- Evaluate regions, buffers, or custom prompts
- See streaming responses in real-time
- Navigate between prompts and responses

## Current Implementation Analysis

### Current Features (vterm-based)
- **Project-aware sessions**: Each project has its own Claude Code vterm buffer
- **Prompt composition**: `claude-code-prompt` opens a markdown buffer for composing prompts
- **Region sending**: Send selected code directly to Claude
- **Session management**: Switch between, close, and manage sessions
- **Evil integration**: Proper evil mode states and keybindings
- **Buffer management**: Uses `pop-to-buffer` for sensible buffer placement

### Current Limitations
- Interactive terminal required (vterm overhead)
- No structured response parsing
- Limited history navigation
- No inline evaluation results
- Terminal formatting issues
- Cannot easily extract code from responses
- No markdown rendering of responses

## New Architecture

### Core Components

#### 1. **Claude Code Process Manager** (`claude-code-process.el`)
- Manage non-interactive Claude processes per project
- Handle `--print --output-format stream-json` mode
- Process streaming JSON responses
- Maintain conversation history
- Handle session persistence with `--continue` and `--resume`

```elisp
;; Key functions:
(claude-code-process-start project-root &optional session-id)
(claude-code-process-send-prompt process prompt)
(claude-code-process-kill process)
(claude-code-process-get-or-create project-root)
```

#### 2. **Response Buffer Manager** (`claude-code-buffer.el`)
- Dedicated response buffer per project: `*claude-code: PROJECT-NAME*`
- Markdown rendering with `markdown-mode`
- Structured sections: prompts, responses, tool calls, errors
- Syntax highlighting for code blocks
- Collapsible sections for tool usage
- Navigation between prompt/response pairs

```elisp
;; Buffer structure:
;; ╔═══════════════════════════════════════════╗
;; ║ *claude-code: my-project*                  ║
;; ║ ─────────────────────────────────────────  ║
;; ║ [Prompt 1] 2025-11-02 14:23:45            ║
;; ║ > Explain this function                    ║
;; ║                                            ║
;; ║ [Response 1]                               ║
;; ║ This function does...                      ║
;; ║                                            ║
;; ║ [Tools Used] (click to expand)             ║
;; ║   - Read: src/main.rs                      ║
;; ║   - Grep: pattern="function"               ║
;; ║                                            ║
;; ║ ─────────────────────────────────────────  ║
;; ║ [Prompt 2] 2025-11-02 14:25:12            ║
;; ║ > Add error handling                       ║
;; ║                                            ║
;; ║ [Response 2] (streaming...)                ║
;; ║ Let me add error handling...               ║
;; ╚═══════════════════════════════════════════╝
```

#### 3. **Prompt Interface** (`claude-code-prompt.el`)
- Keep existing prompt buffer for composition
- Enhance with template snippets (code context, ask about error, etc.)
- Add quick-send commands for common queries
- Preview what will be sent (including file context)

#### 4. **Inline Evaluation** (`claude-code-eval.el`)
- Send code/text and show results inline or in popup
- Similar to `cider-eval-last-sexp`
- Overlay support for quick results
- Option to send to main response buffer

#### 5. **Session Management** (`claude-code-session.el`)
- List all sessions across projects
- Resume specific session by ID
- Fork sessions for experimentation
- Export conversation history
- Clear/reset sessions

### JSON Response Handling

Claude Code's `--output-format stream-json` provides:
```json
{
  "type": "message_start",
  "message": {...}
}
{
  "type": "content_block_start",
  "index": 0,
  "content_block": {"type": "text", "text": ""}
}
{
  "type": "content_block_delta",
  "index": 0,
  "delta": {"type": "text_delta", "text": "Here is..."}
}
{
  "type": "tool_use",
  "name": "Read",
  "input": {"file_path": "/path/to/file"}
}
{
  "type": "message_stop"
}
```

The process manager will parse these events and update the response buffer in real-time.

## Implementation Phases

### Phase 1: Core Process Management
**Goal**: Launch Claude in non-interactive mode and capture output

Tasks:
1. Create `claude-code-process.el`
2. Implement `start-process` wrapper for `claude --print --output-format stream-json`
3. Set up process filter to handle JSON streaming
4. Parse JSON responses using `json-read`
5. Maintain process-to-project mapping
6. Handle process lifecycle (start, stop, error recovery)
7. Test with simple prompts

**Success Criteria**: Can send a prompt and receive complete JSON response

### Phase 2: Response Buffer UI
**Goal**: Display responses in a structured, readable format

Tasks:
1. Create `claude-code-buffer.el`
2. Design buffer structure with clear sections
3. Implement markdown rendering
4. Add streaming text updates (append delta chunks)
5. [ ] Format tool use sections with collapsible details
6. Add timestamps and session metadata
7. Implement syntax highlighting for code blocks
8. Create custom faces for different section types

**Success Criteria**: Responses display beautifully with markdown rendering and streaming updates

### Phase 3: Navigation and History
**Goal**: Navigate through conversation history easily

Tasks:
1. Implement prompt/response markers in buffer
2. Add navigation functions:
   - `claude-code-next-prompt`
   - `claude-code-previous-prompt`
   - `claude-code-next-response`
   - `claude-code-previous-response`
3. Jump to tool usage details
4. Copy code blocks
5. Re-send previous prompts
6. Search within conversation

**Success Criteria**: Can navigate history like `cider-repl-previous-input`

### Phase 4: Enhanced Prompt Interface
**Goal**: Make sending prompts more efficient

Tasks:
1. Update `claude-code-prompt.el`
2. Add template system:
   - "Explain this code"
   - "Fix this error"
   - "Add tests for"
   - "Refactor to"
3. Show file context preview
4. Quick-send shortcuts
5. Attach file contents automatically
6. Multi-line editing improvements

**Success Criteria**: Common workflows become one or two keystrokes

### Phase 5: Inline Evaluation
**Goal**: Evaluate code/prompts without switching buffers

Tasks:
1. Create `claude-code-eval.el`
2. Implement inline eval functions:
   - `claude-code-eval-region`
   - `claude-code-eval-buffer`
   - `claude-code-eval-defun`
3. Show results in overlay/popup
4. Option to send results to main buffer
5. Visual feedback during processing
6. Error handling and timeouts

**Success Criteria**: Can evaluate code inline like Cider

### Phase 6: Session Management
**Goal**: Manage multiple conversations and resume sessions

Tasks:
1. Create `claude-code-session.el`
2. Implement session listing UI
3. Resume session by ID (`--resume`)
4. Fork sessions (`--fork-session`)
5. Continue last session (`--continue`)
6. Export history to markdown/org
7. Clear session data

**Success Criteria**: Full control over conversation sessions

### Phase 7: Tool Integration and Visualization
**Goal**: Better visibility into Claude's tool usage

Tasks:
1. Parse tool use from JSON responses
2. Display tool calls in organized format
3. Show tool inputs/outputs
4. Link tool file paths (clickable)
5. Highlight files Claude modified
6. Option to collapse tool details
7. Statistics (tools used, files touched)

**Success Criteria**: Clear visibility into what Claude is doing

### Phase 8: Advanced Features
**Goal**: Add power-user features

Tasks:
1. Diff viewer for Claude's edits
2. Auto-apply edits with confirmation
3. Undo Claude's changes
4. Model selection per prompt
5. Custom system prompts
6. Token usage tracking
7. Response caching
8. Multi-project mode
9. Org-mode integration for exporting conversations

**Success Criteria**: Professional-grade AI coding assistant

## Key Design Decisions

### 1. Process vs. REST API
**Decision**: Use `claude --print` subprocess
**Reasoning**:
- Leverages existing Claude Code CLI
- Automatic auth and configuration
- MCP server support out of the box
- Tool definitions already configured
- Consistent with interactive usage
- No need to reimplement Claude Code logic

### 2. Buffer Strategy
**Decision**: One response buffer per project
**Reasoning**:
- Matches current project-aware design
- Easy to reference past interactions
- Natural conversation flow
- Can have multiple projects open simultaneously

### 3. Streaming vs. Batch
**Decision**: Stream JSON responses
**Reasoning**:
- Real-time feedback
- Better UX for long responses
- Can show progress
- Cancel mid-stream if needed

### 4. Markdown Rendering
**Decision**: Use `markdown-mode` for responses
**Reasoning**:
- Native Emacs rendering
- Syntax highlighting in code blocks
- Familiar for users
- Can leverage existing markdown tools

## REPL-Like Features Comparison

| Feature | Cider (Clojure) | Proposed Claude Code |
|---------|----------------|----------------------|
| Send expression | ✅ `cider-eval-last-sexp` | ✅ `claude-code-eval-region` |
| Inline results | ✅ Overlays | ✅ Overlays/popups |
| REPL buffer | ✅ `*cider-repl*` | ✅ `*claude-code: PROJECT*` |
| History navigation | ✅ M-p/M-n | ✅ Custom navigation |
| Multi-line input | ✅ | ✅ Prompt buffer |
| Load file | ✅ `cider-load-buffer` | ✅ `claude-code-send-buffer` |
| Documentation | ✅ `cider-doc` | ✅ Ask Claude |
| Quit | ✅ `cider-quit` | ✅ `claude-code-quit` |
| Connect | ✅ `cider-jack-in` | ✅ `claude-code-start` |
| Switch REPL | ✅ | ✅ Project-aware switching |

## Keybinding Strategy

### Global Leader (SPC a)
```elisp
SPC a a  - claude-code-start           (Start Claude for project)
SPC a b  - claude-code-buffer          (Switch to response buffer)
SPC a p  - claude-code-prompt          (Open prompt composer)
SPC a q  - claude-code-quit            (Quit session)
SPC a r  - claude-code-resume          (Resume session)
SPC a l  - claude-code-list-sessions   (List all sessions)
```

### Evaluation (like Cider)
```elisp
SPC a e e  - claude-code-eval-region     (Eval selection)
SPC a e b  - claude-code-eval-buffer     (Eval whole buffer)
SPC a e d  - claude-code-eval-defun      (Eval function at point)
SPC a e l  - claude-code-eval-line       (Eval current line)
SPC a e p  - claude-code-eval-paragraph  (Eval paragraph)
```

### Navigation (in response buffer)
```elisp
C-c C-n  - claude-code-next-prompt
C-c C-p  - claude-code-previous-prompt
C-c C-f  - claude-code-next-response
C-c C-b  - claude-code-previous-response
C-c C-j  - claude-code-jump-to-tool
C-c C-c  - claude-code-copy-code-block
C-c C-r  - claude-code-resend-prompt
```

### Prompt Buffer
```elisp
C-c C-c  - claude-code-send-prompt-and-close
C-c C-k  - claude-code-prompt-abort
C-c C-t  - claude-code-insert-template
C-c C-f  - claude-code-attach-file
```

## Migration Path

### Phase 1: Parallel Implementation
1. Create new files alongside existing `ai.el`
2. Use different keybindings (e.g., `SPC a c` for Claude REPL)
3. Keep vterm version available
4. Test thoroughly with real workflows

### Phase 2: Transition
1. Document differences and improvements
2. Create migration guide
3. Default to new implementation
4. Keep old implementation as `claude-code-vterm-*`

### Phase 3: Deprecation
1. Remove vterm version after stable period
2. Clean up old code
3. Update documentation

## Technical Considerations

### Performance
- JSON parsing overhead: Use `json-read` from stream
- Buffer updates: Use markers and `insert-before-markers`
- Long conversations: Implement pagination or limiting

### Error Handling
- Process crashes: Auto-restart with option
- Network errors: Retry logic
- Malformed JSON: Graceful degradation
- Timeout handling: User-configurable

### State Management
- Track active prompts per project
- Handle concurrent requests (queue or block)
- Persist session IDs across Emacs restarts
- Clean up zombie processes

### Testing Strategy
1. Unit tests for JSON parsing
2. Integration tests with mock Claude process
3. Manual testing with real workflows
4. Performance benchmarks for large conversations

## Success Metrics

1. **Usability**: Can complete common tasks faster than vterm
2. **Reliability**: No crashes or hangs during normal use
3. **Performance**: Response rendering feels instant
4. **Discoverability**: New users understand the interface
5. **Power**: Advanced users can customize and extend

## Future Enhancements

1. **Multi-agent support**: Different agents in same buffer
2. **Collaborative sessions**: Share sessions between users
3. **Voice input**: Whisper integration for prompts
4. **Image support**: Display images in responses
5. **Git integration**: Auto-commit Claude's changes
6. **Testing framework**: Claude generates and runs tests
7. **Documentation generation**: Auto-generate docs from code
8. **Code review mode**: Claude reviews diffs
9. **Pair programming mode**: Real-time collaboration UI
10. **Learning mode**: Claude teaches concepts interactively

## Dependencies

### Required
- `markdown-mode`: Response rendering
- `json`: Response parsing (built-in)
- `projectile` or `project.el`: Project detection

### Optional
- `corfu`: Completion in prompt buffer
- `yasnippet`: Template expansion
- `magit`: Git integration
- `org-mode`: Export conversations
- `embark`: Quick actions on responses

## File Structure

```
modules/
├── ai.el                          (Main entry point, kept for compatibility)
├── claude-code/
│   ├── claude-code-core.el        (Core integration layer)
│   ├── claude-code-process.el     (Process management)
│   ├── claude-code-buffer.el      (Response buffer UI)
│   ├── claude-code-prompt.el      (Prompt composition)
│   ├── claude-code-eval.el        (Inline evaluation)
│   ├── claude-code-session.el     (Session management)
│   ├── claude-code-utils.el       (Helper functions)
│   └── claude-code-faces.el       (Custom faces)
```

## Development Timeline

Estimated time per phase (assumes ~10 hrs/week):

- Phase 1: Core Process Management - 2 weeks
- Phase 2: Response Buffer UI - 2 weeks
- Phase 3: Navigation and History - 1 week
- Phase 4: Enhanced Prompt Interface - 1 week
- Phase 5: Inline Evaluation - 1 week
- Phase 6: Session Management - 1 week
- Phase 7: Tool Integration - 1 week
- Phase 8: Advanced Features - 2 weeks

**Total**: ~11 weeks for complete implementation

Could be accelerated by focusing on core features first (Phases 1-4) for an MVP in ~6 weeks.

## Conclusion

This REPL-style integration will transform Claude Code from a terminal-based tool into a first-class Emacs citizen, providing a superior development experience that matches or exceeds modern REPL environments like Cider. The phased approach allows for incremental development and testing while maintaining backwards compatibility during the transition.
