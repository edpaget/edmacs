# Implementation Plan: Tool Output Display

**Status**: Phase 1 Complete ✅
**Priority**: CRITICAL
**Date**: 2025-11-12
**Last Updated**: 2025-11-12
**Target Version**: v1.0

## Progress Summary

**Phase 1 (Core Infrastructure)**: ✅ **COMPLETE & TESTED** (2025-11-12)
- Event handling infrastructure implemented and verified working
- Basic Read tool formatter working with live testing ✓
- Comprehensive test suite (215 tests passing, including user event handler)
- All code compiles without warnings
- Discovered and implemented correct event structure (`tool_result` as content block in `user` events)

**Current Status**: READ tool outputs display correctly in live sessions with formatted output, icons, and metadata!

**Next Steps**: Phase 2 (Enhanced Formatting) - Add formatters for Bash, Grep, Edit, Write, and other tool types

## Overview

Implement comprehensive display of tool outputs (Read, Grep, Edit, Write, Bash, etc.) in the main conversation buffer. Users need to see what Claude actually did, not just what it requested. This is essential for understanding and verifying changes, debugging issues, and building trust in the AI's actions.

## Current State

**Phase 1 Complete** - Basic tool output display working:
- ✅ Tool use requests (tool name + parameters)
- ✅ Tool outputs for Read operations (formatted with icons, metadata, boxed content)
- ⏳ Diffs for Edit/Write operations (Phase 3)
- ⏳ Command outputs from Bash operations (Phase 2)
- ⏳ Grep results formatting (Phase 2)

Tool results are captured from the event stream as `tool_result` content blocks within `user` events and displayed with formatting.

## Architecture Components

### 1. Event Stream Enhancement (`claude-repl-process.el`)

Capture and route tool result events.

#### Actual Event Structure (VERIFIED)

**CRITICAL**: Tool results are NOT top-level events. They appear as content blocks within events.

Claude Code CLI's stream-json format uses:
- **Top-level events**: `assistant`, `user`, `result`, `system`
- **Content blocks**: Within `message.content` array, each block has a `type`:
  - `"text"` - Text content
  - `"tool_use"` - Tool request (in `assistant` events)
  - `"tool_result"` - Tool output (in `user` events)

#### Process Flow (ACTUAL)

```
Claude Code CLI → JSON Stream → Process Filter → Event Handler → Content Block Parser
                                                      ↓
                                    assistant event → tool_use block → show tool request
                                    user event → tool_result block → show formatted output
                                    assistant event → text block → show response text
```

#### Implementation Changes

1. **Extend event handler** in `claude-repl-process--handle-json-line`:
   - Detect `tool_result` events
   - Extract tool name, input, and output
   - Call registered tool-result callbacks

2. **Add tool result callback system**:
   ```elisp
   (defun claude-repl-process-add-tool-result-callback (proc-obj callback)
     "Add CALLBACK for tool results.
   CALLBACK receives (tool-name tool-input tool-output).")
   ```

3. **Store tool results** in process structure:
   ```elisp
   (cl-defstruct claude-repl-process
     ...
     tool-results          ; List of tool results for current interaction
     tool-result-callbacks ; Callbacks for tool results
     ...)
   ```

### 2. Tool Output Formatting Module (`claude-repl-tool-output.el`)

New module for formatting different tool outputs.

#### Core Functions

```elisp
(defun claude-repl-tool-output-format (tool-name tool-input tool-output)
  "Format TOOL-OUTPUT for display based on TOOL-NAME.
Returns formatted string ready for insertion.")

(defun claude-repl-tool-output-format-read (tool-input tool-output)
  "Format Read tool output with syntax highlighting.")

(defun claude-repl-tool-output-format-grep (tool-input tool-output)
  "Format Grep tool output with matched lines highlighted.")

(defun claude-repl-tool-output-format-edit (tool-input tool-output)
  "Format Edit tool output as unified diff.")

(defun claude-repl-tool-output-format-write (tool-input tool-output)
  "Format Write tool output showing file preview.")

(defun claude-repl-tool-output-format-bash (tool-input tool-output)
  "Format Bash tool output with command and result.")

(defun claude-repl-tool-output-format-glob (tool-input tool-output)
  "Format Glob tool output as file list.")

(defun claude-repl-tool-output-format-task (tool-input tool-output)
  "Format Task/subagent tool output.")
```

#### Formatting Strategies

**Read Tool**:
```
🔍 Read: modules/claude-repl/claude-repl-core.el

Lines read: 358
File size: 15.4 KB

╭─ Content ─────────────────────────────────────────╮
│ ;;; claude-repl-core.el --- Core Claude Code REPL │
│                                                    │
│ ;; Copyright (C) 2025                             │
│ [... syntax highlighted content ...]              │
╰────────────────────────────────────────────────────╯
```

**Edit Tool**:
```
✏️  Edit: modules/claude-repl/init.el

Replacing 5 lines:

╭─ Diff ────────────────────────────────────────────╮
│ --- init.el                                        │
│ +++ init.el                                        │
│ @@ -10,5 +10,5 @@                                  │
│  (require 'claude-repl-core)                       │
│ -(setq foo "bar")                                  │
│ +(setq foo "baz")                                  │
│  (provide 'init)                                   │
╰────────────────────────────────────────────────────╯

✓ Changed 1 line (+1/-1)
```

**Write Tool**:
```
📝 Write: modules/new-module.el

Creating new file (250 lines, 8.2 KB)

╭─ Preview ─────────────────────────────────────────╮
│ ;;; new-module.el --- Description                  │
│                                                    │
│ [... syntax highlighted preview ...]              │
│                                                    │
│ ... (200 lines truncated)                         │
╰────────────────────────────────────────────────────╯
```

**Bash Tool**:
```
⚡ Bash: npm test

Working directory: /Users/edward/Projects/edmacs

╭─ Output ──────────────────────────────────────────╮
│ > test                                             │
│ > jest                                             │
│                                                    │
│ PASS  src/tests/main.test.js                      │
│   ✓ renders correctly (12ms)                      │
│                                                    │
│ Tests: 1 passed, 1 total                          │
╰────────────────────────────────────────────────────╯

✓ Exit code: 0
⏱ Duration: 2.3s
```

**Grep Tool**:
```
🔎 Grep: "defun.*completion" in modules/

Found 12 matches in 3 files

╭─ Results ─────────────────────────────────────────╮
│ completion.el:192:  (defun corfu-enable-in-...   │
│ completion.el:194:    "Enable Corfu in the...    │
│                                                    │
│ claude-repl-completion.el:45:(defun claude-...   │
│ claude-repl-completion.el:67:(defun claude-...   │
│                                                    │
│ [... more results ...]                            │
╰────────────────────────────────────────────────────╯
```

#### Collapsible Sections

Use text properties and overlays to make tool outputs collapsible:

```elisp
(defun claude-repl-tool-output--make-collapsible (start end)
  "Make region from START to END collapsible.
Adds invisible overlay and toggle button.")

(defun claude-repl-tool-output-toggle-visibility ()
  "Toggle visibility of tool output at point.")
```

Initial state: Large outputs (>20 lines) start collapsed, small outputs expanded.

### 3. Buffer Integration (`claude-repl-buffer.el`)

Display tool outputs in conversation buffer.

#### Display Flow

1. **Tool use event** → Show tool request (current behavior, already working)
2. **Tool result event** → Show tool output (NEW)

#### Implementation Changes

**Add tool output display function**:
```elisp
(defun claude-repl-buffer-add-tool-output (buffer tool-name tool-input tool-output)
  "Add tool output to BUFFER for TOOL-NAME.
TOOL-INPUT is the tool parameters, TOOL-OUTPUT is the result.")
```

**Update interaction structure**:
```elisp
(cl-defstruct claude-repl-interaction
  prompt
  prompt-time
  response-text
  tool-uses            ; List of (tool-name tool-input)
  tool-outputs         ; NEW: List of (tool-name tool-input tool-output)
  start-marker
  end-marker
  status
  metadata)
```

**Modify event handler** in `claude-repl-core.el`:
```elisp
;; Add callback for tool results
(claude-repl-process-add-tool-result-callback
 proc-obj
 (lambda (tool-name tool-input tool-output)
   (claude-repl-buffer-add-tool-output
    response-buffer tool-name tool-input tool-output)))
```

#### Visual Layout

Tool outputs appear inline with conversation:

```
## Prompt

Please check the test files and run them.

## Response

Let me check the test files first.

🔍 Read: test/main-test.el
╭─ Content ─────────────╮
│ [... file content ...] │
╰────────────────────────╯

Now I'll run the tests.

⚡ Bash: eldev test
╭─ Output ──────────────╮
│ [... test output ...] │
╰────────────────────────╯

✓ All tests passed! The code looks good.
```

### 4. Diff Integration

Leverage existing `claude-repl-diff.el` for Edit/Write previews.

#### Current Capabilities

The diff module already supports:
- ✅ Unified diffs for Edit tool
- ✅ File previews for Write tool
- ✅ Syntax highlighting
- ✅ Line count statistics

#### Integration Points

```elisp
(defun claude-repl-tool-output-format-edit (tool-input tool-output)
  "Format Edit tool with inline diff."
  (let* ((diff-buffer (claude-repl-diff-for-edit tool-input))
         (stats (when diff-buffer
                  (claude-repl-diff-get-statistics diff-buffer)))
         (diff-string (when diff-buffer
                        (with-current-buffer diff-buffer
                          (buffer-string)))))
    ;; Format for display
    (format "╭─ Diff ─────╮\n%s\n╰────────────╯\n✓ Changed (+%d/-%d)"
            diff-string
            (plist-get stats :added)
            (plist-get stats :removed))))
```

### 5. Syntax Highlighting

Apply appropriate major modes to tool outputs.

#### Strategy

1. **Detect content type** from file extension or tool type
2. **Apply major mode** in temporary buffer
3. **Extract font-lock properties**
4. **Transfer to conversation buffer**

#### Implementation

```elisp
(defun claude-repl-tool-output--apply-highlighting (content file-path)
  "Apply syntax highlighting to CONTENT based on FILE-PATH.
Returns string with text properties for highlighting."
  (let* ((mode (claude-repl-diff--derive-mode-from-file file-path))
         (temp-buf (generate-new-buffer " *highlight-temp*")))
    (unwind-protect
        (with-current-buffer temp-buf
          (insert content)
          (when mode
            (funcall mode)
            (font-lock-ensure))
          (buffer-string))
      (kill-buffer temp-buf))))
```

### 6. Configuration

Provide customization for display preferences.

```elisp
(defcustom claude-repl-tool-output-show-inline t
  "Show tool outputs inline in conversation buffer.
If nil, tool outputs are not displayed (only tool use requests)."
  :type 'boolean
  :group 'claude-repl-buffer)

(defcustom claude-repl-tool-output-max-lines 50
  "Maximum lines to show in tool output before truncating.
If nil, show full output regardless of length."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max lines"))
  :group 'claude-repl-buffer)

(defcustom claude-repl-tool-output-auto-collapse-threshold 20
  "Auto-collapse tool outputs longer than this many lines.
If nil, never auto-collapse."
  :type '(choice (const :tag "Never auto-collapse" nil)
                 (integer :tag "Line threshold"))
  :group 'claude-repl-buffer)

(defcustom claude-repl-tool-output-show-diffs t
  "Show diffs for Edit and Write tools."
  :type 'boolean
  :group 'claude-repl-buffer)

(defcustom claude-repl-tool-output-syntax-highlight t
  "Apply syntax highlighting to tool outputs."
  :type 'boolean
  :group 'claude-repl-buffer)

(defcustom claude-repl-tool-output-style 'boxed
  "Visual style for tool output display.
- \\='boxed: Draw boxes around outputs
- \\='indented: Indent with background color
- \\='minimal: Plain text with separator"
  :type '(choice (const :tag "Boxed" boxed)
                 (const :tag "Indented" indented)
                 (const :tag "Minimal" minimal))
  :group 'claude-repl-buffer)

(defcustom claude-repl-tool-output-show-metadata t
  "Show metadata (file size, duration, exit codes) for tool outputs."
  :type 'boolean
  :group 'claude-repl-buffer)
```

## Implementation Phases

### Phase 1: Core Infrastructure ✅ COMPLETE

**Goal**: Basic tool result capture and display

**Status**: ✅ Complete (2025-11-12)

**Tasks**:
1. ✅ Add tool result event handling to process layer
2. ✅ Create `claude-repl-tool-output.el` skeleton
3. ✅ Implement basic formatting for Read tool
4. ✅ Display Read tool outputs in buffer
5. ✅ Add tests for tool result parsing

**Deliverables**:
- ✅ Read tool outputs appear in conversation buffer
- ✅ Formatted text display with icons and boxes
- ✅ Event handling infrastructure in place
- ✅ Comprehensive test coverage (12 tests, all passing)

**Implementation Notes**:
- **CRITICAL DISCOVERY**: `tool_result` is a content block type within `user` events, NOT a top-level event
- Event structure follows same pattern as `tool_use`: appears as content block in messages
- Added `tool-result-callbacks` field to `claude-repl-process` structure (kept for API compatibility)
- Created `claude-repl-buffer-handle-user-event` to process user events with tool_result blocks
- Event flow: `user` event → extract content blocks → find `tool_result` → display formatted output
- New module `claude-repl-tool-output.el` provides formatting infrastructure (320 lines)
- Read tool formatter includes:
  - File path display with icon (🔍)
  - Metadata (lines, size)
  - Boxed content display with configurable style (boxed/indented/minimal)
  - Syntax highlighting support
  - Truncation for large outputs
- Buffer integration via `claude-repl-buffer-add-tool-output`
- Tool outputs stored in interaction structure with `tool-use-id` for matching
- Handler wired up in `claude-repl-core.el` for user events
- **TOOL MATCHING**: Implemented `tool_use_id` matching system:
  - Tool uses store their `id` field from `tool_use` blocks
  - Tool results use `tool_use_id` to look up the matching tool use
  - This ensures accurate tool name/input even when not provided in `tool_result`
  - Stored in interaction: `tool-uses` has `:id :tool :input`, `tool-outputs` has `:tool-use-id :tool :input :output`

**Files Modified**:
- `claude-repl-process.el`: Added tool result callback system
- `claude-repl-buffer.el`: Added tool output display function, `tool-outputs` field, and tool matching logic
  - Updated `claude-repl-buffer-add-tool-use` to accept and store `tool-id`
  - Updated `claude-repl-buffer-add-tool-output` to match by `tool-use-id`
  - Modified `claude-repl-buffer-handle-assistant-event` to extract `id` from tool_use blocks
  - Modified `claude-repl-buffer-handle-user-event` to pass `tool-use-id` to output handler
- `claude-repl-core.el`: Wired up tool result callback

**Files Created**:
- `claude-repl-tool-output.el`: Tool output formatting module (320 lines)
- `test/claude-repl-tool-output-test.el`: Comprehensive test suite (12 tests)

**Testing**:
- ✅ All 216 unit tests passing (including user event handler and tool matching tests)
- ✅ Code compiles without warnings
- ✅ Live testing confirmed working - READ tool outputs display correctly!
- ✅ Tool matching test verifies `tool_use_id` lookup works correctly

**Key Learnings**:
1. **Event Structure**: `tool_result` is NOT a top-level event - it's a content block within `user` events
2. **Pattern Matching**: Same structure as `tool_use` (content block) but in different event type
3. **Tool Matching**: The `tool_use_id` field in `tool_result` matches the `id` field in `tool_use`
   - This allows us to correlate results with their original requests
   - Critical for accurate display when `tool_name`/`input` are missing from result
4. **Event Flow**:
   - Assistant event → `tool_use` content block → "Tool: Read" shown (stores ID)
   - User event → `tool_result` content block → Formatted output shown (looks up by ID)
   - Assistant event → `text` content block → Claude's response continues

**Actual Effort**: ~3 hours (including iterative fixes based on actual event structure)

### Phase 2: Enhanced Formatting

**Goal**: Beautiful, informative tool output display

**Tasks**:
1. Implement formatters for all tool types
2. Add syntax highlighting for code content
3. Add boxed/styled output sections
4. Implement metadata display (sizes, durations, etc.)
5. Add icons for different tool types

**Deliverables**:
- All tool types have custom formatters
- Syntax highlighting works for code files
- Consistent visual style across tool outputs
- Metadata shown appropriately

**Testing**:
- Test each tool type individually
- Verify syntax highlighting for various languages
- Check visual consistency

**Estimated Effort**: 4-5 hours

### Phase 3: Diff Integration

**Goal**: Inline diffs for Edit/Write operations

**Tasks**:
1. Integrate `claude-repl-diff.el` with tool output display
2. Show unified diffs for Edit operations
3. Show file previews for Write operations
4. Add statistics (lines changed, files created)
5. Handle edge cases (file not found, permission errors)

**Deliverables**:
- Edit operations show inline diffs
- Write operations show file previews
- Change statistics displayed
- Error cases handled gracefully

**Testing**:
- Test Edit with various file types
- Test Write with new and existing files
- Test error scenarios

**Estimated Effort**: 3-4 hours

### Phase 4: Collapsible Sections

**Goal**: Keep conversation buffer manageable

**Tasks**:
1. Implement collapsible overlays for tool outputs
2. Add toggle keybinding and mouse support
3. Auto-collapse large outputs based on threshold
4. Remember collapse state per interaction
5. Add "Expand All" / "Collapse All" commands

**Deliverables**:
- Large outputs start collapsed
- Click/keypress to toggle visibility
- Buffer remains navigable with many tool outputs

**Testing**:
- Test with very long tool outputs
- Verify toggle works reliably
- Test state persistence during buffer refresh

**Estimated Effort**: 3-4 hours

### Phase 5: Advanced Features

**Goal**: Polish and power features

**Tasks**:
1. Clickable file paths (jump to file)
2. Copy tool output to kill ring
3. Re-run tool with same parameters
4. Export tool outputs
5. Search within tool outputs

**Deliverables**:
- File paths are clickable
- Context menu actions available
- Enhanced navigation and interaction

**Testing**:
- Test all interactive features
- Verify integration with projectile/find-file
- Test with evil-mode

**Estimated Effort**: 4-5 hours

## Data Structures

### Tool Result Event (ACTUAL FORMAT - VERIFIED)

**Actual JSON structure** from Claude Code CLI (tool results come in `user` events):

```json
{
  "type": "user",
  "message": {
    "content": [
      {
        "type": "tool_result",
        "tool_use_id": "toolu_123abc",
        "content": ";;; core.el ...\n(defun foo ())\n...",
        "tool_name": "Read",
        "input": {
          "file_path": "modules/core.el"
        }
      }
    ]
  }
}
```

**Note**: The `tool_name` and `input` fields may not always be present in the `tool_result` block. We now match `tool_use_id` to the previous `tool_use` to get complete information - this ensures accurate display regardless of which fields are included in the result.

### Tool Output Structure

```elisp
(cl-defstruct claude-repl-tool-output
  "Structure representing a tool execution result."
  tool-name       ; String: "Read", "Edit", etc.
  tool-input      ; Alist: tool parameters
  output-content  ; String or alist: the actual output
  output-metadata ; Alist: size, duration, etc.
  status          ; Symbol: 'success, 'error
  timestamp       ; Time when output received
  buffer-range)   ; Cons: (start . end) positions in buffer
```

### Formatter Registry

```elisp
(defvar claude-repl-tool-output-formatters
  '(("Read" . claude-repl-tool-output-format-read)
    ("Grep" . claude-repl-tool-output-format-grep)
    ("Edit" . claude-repl-tool-output-format-edit)
    ("Write" . claude-repl-tool-output-format-write)
    ("Bash" . claude-repl-tool-output-format-bash)
    ("Glob" . claude-repl-tool-output-format-glob)
    ("Task" . claude-repl-tool-output-format-task))
  "Alist mapping tool names to formatter functions.")
```

## Testing Strategy

### Unit Tests (`test/claude-repl-tool-output-test.el`)

```elisp
(describe "Tool output formatting"
  (it "formats Read tool output with syntax highlighting"
    (let* ((input '((file_path . "test.el")))
           (output '((content . ";; test\n(defun foo ())")))
           (formatted (claude-repl-tool-output-format-read input output)))
      (expect formatted :to-match "test.el")
      (expect formatted :to-match "defun foo")))

  (it "formats Edit tool output as diff"
    (let* ((input '((file_path . "test.el")
                    (old_string . "(setq x 1)")
                    (new_string . "(setq x 2)")))
           (output '((status . "success")))
           (formatted (claude-repl-tool-output-format-edit input output)))
      (expect formatted :to-match "[-1]")
      (expect formatted :to-match "[+2]")))

  (it "truncates long outputs"
    (let* ((claude-repl-tool-output-max-lines 10)
           (long-content (string-join (make-list 50 "line") "\n"))
           (input '((file_path . "test.txt")))
           (output `((content . ,long-content)))
           (formatted (claude-repl-tool-output-format-read input output)))
      (expect formatted :to-match "truncated"))))
```

### Integration Tests

```elisp
(describe "Tool output display integration"
  (it "displays Read tool output in conversation buffer"
    (let* ((buffer (claude-repl-buffer-get-or-create "/tmp/test"))
           (tool-input '((file_path . "test.el")))
           (tool-output '((content . ";; test code"))))
      (claude-repl-buffer-add-tool-output buffer "Read" tool-input tool-output)
      (with-current-buffer buffer
        (expect (buffer-string) :to-match "Read: test.el")
        (expect (buffer-string) :to-match "test code"))))

  (it "shows diffs for Edit operations"
    ;; Test diff display integration
    ))
```

### Manual Testing Checklist

- [ ] Read tool shows file contents with syntax highlighting
- [ ] Edit tool shows inline diff with +/- lines
- [ ] Write tool shows file preview
- [ ] Bash tool shows command output
- [ ] Grep tool shows matched lines
- [ ] Long outputs are truncated appropriately
- [ ] Syntax highlighting works for various languages
- [ ] Collapsible sections work with mouse and keyboard
- [ ] File paths are clickable
- [ ] Error outputs are displayed clearly
- [ ] Works with evil-mode keybindings
- [ ] Performance is good with many tool outputs

## Edge Cases and Error Handling

### Error Scenarios

1. **Tool execution failed**
   - Display error message prominently
   - Show tool parameters for debugging
   - Provide suggestion or retry option

2. **Output too large**
   - Truncate with clear indication
   - Offer to save to file or open in new buffer
   - Show size statistics

3. **Binary/non-text output**
   - Detect binary content
   - Show hexdump preview or file info
   - Don't attempt syntax highlighting

4. **Missing/inaccessible files**
   - Show clear error message
   - Display full path for debugging
   - Suggest possible fixes

5. **Slow tool execution**
   - Show "in progress" indicator
   - Allow cancellation
   - Display partial output as it arrives (streaming)

### Handling Strategy

```elisp
(defun claude-repl-tool-output-format (tool-name tool-input tool-output)
  "Format tool output with comprehensive error handling."
  (condition-case err
      (let ((formatter (alist-get tool-name
                                  claude-repl-tool-output-formatters
                                  nil nil #'string=)))
        (if formatter
            (funcall formatter tool-input tool-output)
          ;; Fallback: generic formatting
          (claude-repl-tool-output-format-generic tool-name tool-input tool-output)))
    (error
     ;; Error during formatting - show raw output
     (format "⚠️  Error formatting %s output: %s\n\nRaw output:\n%S"
             tool-name
             (error-message-string err)
             tool-output))))
```

## Performance Considerations

### Optimization Strategies

1. **Lazy rendering**
   - Don't render collapsed sections until expanded
   - Use `jit-lock` for syntax highlighting

2. **Truncation thresholds**
   - Set reasonable defaults (50 lines for inline display)
   - Offer "View full output" button

3. **Async highlighting**
   - Highlight large outputs in idle time
   - Show unhighlighted text immediately

4. **Caching**
   - Cache highlighted buffers for repeated views
   - Invalidate on file changes

5. **Memory management**
   - Clean up temporary buffers after formatting
   - Don't keep raw output in memory after display

## Documentation

### User Documentation

Add section to README.md:

```markdown
## Tool Output Display

Claude-repl automatically displays the results of tools that Claude uses:

### Viewing Tool Outputs

Tool outputs appear inline in the conversation buffer:
- **Read** - File contents with syntax highlighting
- **Edit** - Unified diffs showing changes
- **Write** - File previews for new/modified files
- **Bash** - Command output with exit codes
- **Grep** - Search results with matches highlighted

### Collapsible Outputs

Large tool outputs are automatically collapsed. To expand:
- Click the `[...]` indicator
- Press `TAB` on the tool output header
- Use `C-c C-o` to toggle visibility

### Configuration

Customize tool output display:

\`\`\`elisp
;; Disable tool output display
(setq claude-repl-tool-output-show-inline nil)

;; Adjust truncation threshold
(setq claude-repl-tool-output-max-lines 100)

;; Change visual style
(setq claude-repl-tool-output-style 'indented)
\`\`\`
```

### Inline Documentation

Comprehensive docstrings for all public functions:
- `claude-repl-tool-output-format`
- `claude-repl-buffer-add-tool-output`
- `claude-repl-tool-output-toggle-visibility`
- Configuration variables

## Future Enhancements

Post-v1.0 improvements:

1. **Streaming Tool Output**
   - Show tool output as it arrives
   - Progress indicators for long-running tools

2. **Tool Output History**
   - Browse past tool outputs
   - Compare outputs across interactions

3. **Interactive Tool Results**
   - Edit and re-run failed operations
   - Modify tool parameters and retry

4. **Export Capabilities**
   - Save tool outputs to files
   - Export as org-mode or markdown

5. **Advanced Diff Features**
   - Side-by-side diffs
   - Word-level diff refinement
   - Apply/reject individual hunks

6. **Tool Output Search**
   - Search across all tool outputs in conversation
   - Filter by tool type or status

7. **Performance Profiling**
   - Show timing for each tool execution
   - Identify slow operations

8. **Output Transformation**
   - Format JSON/XML outputs prettily
   - Apply custom transformers per tool type

## Implementation Checklist

### Phase 1: Core Infrastructure ✅
- [x] Add tool result event type to process layer
- [x] Implement tool result callback system
- [x] Create `claude-repl-tool-output.el`
- [x] Implement basic Read formatter
- [x] Display Read outputs in buffer
- [x] Write unit tests for event handling
- [ ] Write integration test for end-to-end (needs live testing)

### Phase 2: Enhanced Formatting
- [ ] Implement formatter for each tool type
- [ ] Add syntax highlighting support
- [ ] Create boxed/styled output sections
- [ ] Add metadata display
- [ ] Add tool-specific icons
- [ ] Test all tool types
- [ ] Verify syntax highlighting

### Phase 3: Diff Integration
- [ ] Integrate claude-repl-diff for Edit
- [ ] Integrate claude-repl-diff for Write
- [ ] Add change statistics display
- [ ] Handle error cases
- [ ] Test various file types
- [ ] Test edge cases

### Phase 4: Collapsible Sections
- [ ] Implement overlay-based collapsing
- [ ] Add toggle keybinding
- [ ] Add mouse support
- [ ] Auto-collapse based on threshold
- [ ] Add expand/collapse all commands
- [ ] Test with large outputs

### Phase 5: Advanced Features
- [ ] Make file paths clickable
- [ ] Add copy-to-kill-ring
- [ ] Implement tool re-run
- [ ] Add export functionality
- [ ] Add search within outputs
- [ ] Full integration testing

## Questions Resolved ✅

1. **Event Format**: What exactly does Claude Code CLI output for tool results?
   - ✅ **RESOLVED**: `tool_result` is a content block within `user` events, not a top-level event
   - Format: `{type: "user", message: {content: [{type: "tool_result", ...}]}}`
   - Matches pattern used for `tool_use` (content block in `assistant` events)

2. **Timing**: When do tool results arrive (before/after assistant response)?
   - ✅ **RESOLVED**: Tool results arrive in separate `user` events between assistant messages
   - Flow: assistant → tool_use → user → tool_result → assistant → text response

3. **Streaming**: Can tool outputs stream incrementally?
   - ⏳ **DEFERRED**: Not needed for Phase 1, tool results arrive as complete blocks
   - Future: Could implement streaming display if needed in Phase 5

4. **Approval Integration**: Should approved/denied tools show differently?
   - ⏳ **DEFERRED**: Phase 1 shows all tool outputs regardless of approval status
   - Future: Could add approval badges in tool output header

5. **Memory**: How to handle conversations with hundreds of tool outputs?
   - ✅ **RESOLVED**: Truncation and collapsible sections (Phase 4) will handle this
   - Phase 1: Simple approach works well, optimize later if needed

## Dependencies

- ✅ `claude-repl-process.el` - Event handling infrastructure exists
- ✅ `claude-repl-buffer.el` - Buffer display system exists
- ✅ `claude-repl-diff.el` - Diff generation exists
- ❌ `claude-repl-tool-output.el` - NEW module to create

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Event format unclear | High | Test with actual CLI early |
| Performance with large outputs | Medium | Implement truncation, lazy rendering |
| Syntax highlighting breaks layout | Low | Extensive testing, fallback to plain text |
| Collision with existing approval display | Medium | Coordinate display locations carefully |
| Memory usage with many outputs | Medium | Implement cleanup, caching strategies |

## Success Criteria

1. **Functionality**
   - ✓ All tool types display outputs correctly
   - ✓ Diffs show for Edit/Write operations
   - ✓ Syntax highlighting works for common languages
   - ✓ Collapsible sections work smoothly

2. **User Experience**
   - ✓ Outputs are visually distinct and readable
   - ✓ Buffer remains navigable with many outputs
   - ✓ Performance is good (no noticeable lag)
   - ✓ Error states are clear and helpful

3. **Quality**
   - ✓ Comprehensive test coverage (>80%)
   - ✓ No memory leaks
   - ✓ Handles edge cases gracefully
   - ✓ Documentation is complete

---

**Next Steps**:
1. Test Claude Code CLI to capture actual tool result events
2. Update plan based on real event format
3. Create feature branch: `feature/tool-output-display`
4. Start Phase 1 implementation (TDD approach)
