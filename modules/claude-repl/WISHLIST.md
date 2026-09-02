# claude-repl Feature Wishlist

This document tracks desired features for making claude-repl a fully-featured Claude Code porcelain inside Emacs.

## Major Missing Features

### 1. File Context Management

**Priority: CRITICAL**

- [ ] Add support for attaching files to prompts (like `@file` in Claude Code CLI)
- [ ] Visual file picker/selector for adding context
- [ ] Workspace-aware context suggestions
- [ ] Reference specific code ranges or symbols
- [ ] Support for `@folder`, `@url`, and other context types
- [ ] Show attached context in the conversation buffer

**Rationale:** This is fundamental to Claude Code's value proposition. Without file context, users can't effectively use Claude for coding tasks.

### 2. Tool Output Display

**Priority: CRITICAL**

- [ ] Display tool outputs (Read, Grep, Edit results) to the user
- [ ] Show diffs for Edit/Write operations in the main buffer
- [ ] Syntax highlighting in diff previews
- [ ] Collapsible tool output sections
- [ ] Link tool outputs to the corresponding response sections

**Rationale:** Users need to see what Claude actually did, not just what it requested. Essential for understanding and verifying changes.

### 3. Todo/Task List Visibility

**Priority: CRITICAL**

- [ ] Persistent todo list UI (sidebar or dedicated buffer)
- [ ] Real-time todo list updates from TodoWrite events
- [ ] Interactive todo management (mark complete, reorder, edit)
- [ ] Visual progress indicators
- [ ] Todo list filtering and search
- [ ] Export todos to org-mode or other formats

**Rationale:** Essential for tracking multi-step tasks and understanding Claude's progress.

### 4. Session Management

**Priority: HIGH**

- [ ] Browse and select from multiple conversation sessions
- [ ] Session naming and organization
- [ ] Session export/import (JSON, markdown)
- [ ] Fork or branch conversations
- [ ] Auto-save conversations to disk
- [ ] Session search and filtering
- [ ] Session metadata (creation time, message count, etc.)

**Rationale:** Allows users to maintain multiple contexts and preserve important conversations.

### 5. Slash Commands & MCP Integration

**Priority: HIGH**

- [ ] Support for slash commands (`/help`, `/clear`, `/model`, etc.)
- [ ] MCP (Model Context Protocol) server integration
- [ ] Custom tool registration
- [ ] Extension/plugin system
- [ ] Command completion in input area

**Rationale:** Enables advanced workflows and extensibility.

### 6. Image & Multimodal Support

**Priority: MEDIUM**

- [ ] Attach images to prompts
- [ ] Display images in responses
- [ ] Screenshot integration (capture and attach)
- [ ] Clipboard image support
- [ ] Image preview in approval UI
- [ ] Support for other media types (PDFs, etc.)

**Rationale:** Claude supports multimodal input; Emacs users should be able to use it.

### 7. Advanced Navigation

**Priority: MEDIUM**

- [ ] Jump to file at line from responses (clickable `file:line` references)
- [ ] Clickable file paths in tool outputs
- [ ] Navigate between referenced files
- [ ] Back/forward navigation in conversation history
- [ ] Bookmark specific interactions
- [ ] Outline view for long conversations

**Rationale:** Improves workflow when working with multiple files.

### 8. Enhanced Streaming Indicators

**Priority: LOW**

- [ ] Token usage meter during streaming
- [ ] "Thinking" vs "typing" distinction
- [ ] Progress bar for long responses
- [ ] Estimated time remaining
- [ ] Real-time token cost tracking

**Rationale:** Provides better feedback during long-running operations.

### 9. Configuration UI

**Priority: LOW**

- [ ] Interactive settings panel (like `M-x customize`)
- [ ] Model selector UI
- [ ] Per-project configuration
- [ ] Quick toggle for common settings
- [ ] Configuration profiles (e.g., "conservative", "aggressive")

**Rationale:** Makes configuration more accessible to non-elisp users.

### 10. Collaboration Features

**Priority: LOW**

- [ ] Share conversation links
- [ ] Export to markdown/HTML with formatting
- [ ] "Copy as curl" for reproducing requests
- [ ] Collaboration annotations
- [ ] Share approval policies

**Rationale:** Enables team workflows and knowledge sharing.

## Smaller Polish Items

### Code Block Enhancements

- [ ] Run code blocks directly from buffer
- [ ] Copy code block with one keystroke (currently `C-c C-c`)
- [ ] Insert code block at point in another buffer
- [ ] Syntax validation for code blocks
- [ ] Code block templates

### Error Handling

- [ ] Retry failed requests with exponential backoff
- [ ] Better error messages with suggestions
- [ ] Network error recovery
- [ ] Graceful degradation on API errors

### Performance

- [ ] Lazy loading for long conversations
- [ ] Virtual scrolling for large buffers
- [ ] Optimize markdown rendering
- [ ] Background conversation indexing

### Search & Discovery

- [ ] Search across all conversations
- [ ] Semantic search within conversations
- [ ] Tag conversations with topics
- [ ] Related conversation suggestions

### Input Enhancements

- [ ] Voice input support (using whisper.el or similar)
- [ ] Template/snippet expansion in input area
- [ ] Multi-line paste handling
- [ ] Rich text input (bold, italic, code)

### Request Management

- [ ] Request cancellation (distinct from interrupt)
- [ ] Request queuing
- [ ] Concurrent requests (multiple projects)
- [ ] Request history and replay

### Accessibility

- [ ] Screen reader support
- [ ] High contrast themes
- [ ] Font size scaling
- [ ] Keyboard-only navigation

### Integration

- [ ] magit integration (review diffs, commit changes)
- [ ] org-mode integration (export to org, link to headings)
- [ ] projectile integration (enhanced project awareness)
- [ ] eglot/lsp integration (symbol awareness)
- [ ] flycheck integration (show errors in context)

## Implementation Notes

### For File Context Management

The Claude Code CLI supports several context formats:
- `@file:path/to/file.el` - Attach entire file
- `@file:path/to/file.el:10-20` - Attach specific line range
- `@folder:path/to/dir` - Attach directory contents
- `@url:https://example.com` - Fetch and attach URL
- `@symbol:functionName` - Attach symbol definition (via LSP)

Implementation approach:
1. Add input preprocessing to detect `@` syntax
2. Expand `@` references to actual content before sending
3. Show attached context visually in the buffer
4. Provide completion for `@file`, `@folder`, etc.

### For Tool Output Display

Tool outputs are already available in the approval system (approval-hook.py receives them), but they're not shown in the main buffer. Implementation:

1. Modify process event handler to capture tool result events
2. Add new buffer sections for tool outputs
3. Format outputs with appropriate syntax highlighting
4. Make outputs collapsible for better readability

### For Todo List UI

The TodoWrite tool events are already parsed. Implementation:

1. Create a dedicated `*claude-repl-todos*` buffer
2. Update it in real-time as TodoWrite events arrive
3. Add keybindings for todo interaction
4. Sync todo state with conversation buffer

## Priority Ranking

**Must Have (for v1.0):**
1. File Context Management
2. Tool Output Display
3. Todo List Visibility

**Should Have (for v1.5):**
4. Session Management
5. Slash Commands & MCP Integration
6. Advanced Navigation

**Nice to Have (for v2.0):**
7. Image & Multimodal Support
8. Enhanced Streaming Indicators
9. Configuration UI
10. Collaboration Features

## Contributing

If you'd like to work on any of these features, please:
1. Open an issue to discuss the approach
2. Reference this wishlist in your PR
3. Update this file to mark items as complete when merged

---

*Last updated: 2025-11-11*
