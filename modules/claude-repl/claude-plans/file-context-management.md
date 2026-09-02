# Implementation Plan: File Context Management with @-Mentions

**Status**: Planning
**Priority**: CRITICAL
**Date**: 2025-11-12
**Target Version**: v1.0

## Overview

Implement a system where users can type `@` in the claude-repl input area to trigger completion for various context types (files, folders, URLs, symbols), similar to how modern chat applications handle mentions. The context will be attached to prompts before sending to Claude.

## Architecture Components

### 1. Context Types Module (`claude-repl-context.el`)

Create a new module to handle context attachment and expansion.

#### Context Type Registry

Define supported @ types:
- `@file:` - Attach entire files
- `@file:path:10-20` - Attach specific line ranges
- `@folder:` - Attach directory contents (with filtering)
- `@url:` - Fetch and attach web content
- `@symbol:` - Attach symbol definitions (via LSP/imenu)
- `@buffer:` - Attach current or named buffer content

#### Context Expansion Functions

- Parse @-mentions in input text
- Expand each mention type to its actual content
- Handle errors gracefully (missing files, network issues, etc.)
- Format context for Claude (with proper delimiters and metadata)

#### Context Rendering

- Display attached context visually in the buffer
- Show context metadata (file path, line range, size, etc.)
- Make contexts collapsible/expandable in UI

### 2. Completion Integration (`claude-repl-completion.el`)

Implement `completion-at-point` provider for @-mentions.

#### Completion Detection

- Detect when user types `@` in input area
- Identify completion context (what comes after `@`)
- Support partial matching (e.g., `@fil` → `@file:`)

#### Completion Functions

- `claude-repl-completion-at-point`: Main CAPF function
- `claude-repl-completion--context-type`: Complete context types (@file, @folder, etc.)
- `claude-repl-completion--file-path`: Complete file paths using projectile/project.el
- `claude-repl-completion--folder-path`: Complete directory paths
- `claude-repl-completion--buffer-name`: Complete buffer names
- `claude-repl-completion--url`: Provide URL completion (from history/bookmarks)
- `claude-repl-completion--symbol`: Complete symbols via LSP/imenu

#### Enhanced Completion UX

- Use annotations to show context type, file size, etc.
- Support fuzzy matching via orderless
- Show icons for different context types (using nerd-icons)
- Preview file contents in corfu-popupinfo

### 3. Input Processing Pipeline

Modify the prompt sending flow to handle context expansion.

#### Pre-processing Hook (in `claude-repl-core.el`)

- Before sending prompt to Claude, scan for @-mentions
- Expand all mentions to their content
- Build structured context payload
- Optionally show preview of expanded context to user

#### Context Formatting

- Format context as Claude-friendly markdown
- Include metadata (file paths, line numbers, timestamps)
- Handle large contexts (truncation, summaries)
- Respect token limits (warn if context too large)

### 4. UI Enhancements (in `claude-repl-buffer.el`)

Visual representation of attached context.

#### Context Display Section

- Show attached files/context before prompt
- Use collapsible sections with icons
- Syntax highlight file contents
- Make file paths clickable (navigate to file)

#### Input Area Hints

- Show "Type @ to attach context" placeholder
- Real-time preview of what will be attached
- Display token cost estimate for context

## Implementation Phases

### Phase 1: Core Infrastructure (Foundation)

**Goal**: Basic @file: functionality working end-to-end

**Tasks**:
1. Create `claude-repl-context.el` with basic context types
2. Implement context expansion for `@file:` (entire files)
3. Add basic file path completion via CAPF
4. Integrate into prompt sending pipeline
5. Write comprehensive tests for context expansion

**Deliverables**:
- Users can type `@file:init.el` and have it expanded to file contents
- Basic file path completion works with TAB
- Context shown in conversation buffer

**Testing**:
- Unit tests for file reading and expansion
- Integration test: type `@file:foo.el`, send, verify expansion
- Test error handling for missing files

### Phase 2: Enhanced Completion (Polish)

**Goal**: Full completion experience with all basic context types

**Tasks**:
1. Add completion for all context types
2. Implement fuzzy matching and icons
3. Add line range support (`@file:foo.el:10-20`)
4. Add folder context with filtering
5. Integrate with projectile for project-aware completion

**Deliverables**:
- Full @-mention completion with icons and annotations
- Line range syntax working
- Folder attachment with customizable filters

**Testing**:
- Test completion triggering and filtering
- Test line range parsing
- Test folder content gathering with excludes

### Phase 3: Advanced Context Types (Power Features)

**Goal**: Support for symbols, URLs, and buffers

**Tasks**:
1. Implement `@symbol:` via LSP/imenu integration
2. Add `@url:` fetching with caching
3. Add `@buffer:` for attaching buffer contents
4. Implement context size warnings and truncation
5. Add context history/favorites

**Deliverables**:
- Symbol completion from LSP
- URL fetching and caching
- Smart context size management

**Testing**:
- Test LSP integration for symbol lookup
- Test URL fetching and error handling
- Test context size limits

### Phase 4: UI Polish (User Experience)

**Goal**: Beautiful, intuitive context management

**Tasks**:
1. Enhanced context visualization in buffer
2. Collapsible/expandable context sections
3. Clickable file paths and symbols
4. Token cost estimation display
5. Context management commands (remove, edit, preview)

**Deliverables**:
- Beautiful context rendering
- Interactive context management
- Cost transparency

**Testing**:
- Manual UI testing for aesthetics
- Test clickable elements
- Test folding/unfolding

## Key Design Decisions

### Syntax Choice

Use `@type:value` format for consistency:
- `@file:path/to/file.el` - Entire file
- `@file:path/to/file.el:10-20` - Line range
- `@folder:src/` - Directory
- `@url:https://example.com` - URL
- `@symbol:function-name` - Symbol definition
- `@buffer:*scratch*` - Buffer content

**Rationale**:
- Clear, unambiguous syntax
- Easy to parse with regex
- Familiar from other tools (markdown, Slack, etc.)
- Extensible for future context types

### Completion Strategy

1. Type `@` → Show all context types with descriptions
2. Type `@f` → Filter to `@file:`, `@folder:`
3. Select `@file:` → Show project file list with fuzzy search
4. Navigate/filter files → Select file
5. Optionally add `:10-20` for line range

**Implementation Notes**:
- Use two-stage completion: type selection, then value selection
- Hook into `completion-at-point-functions`
- Leverage corfu for popup UI
- Use consult for advanced selection (file lists, symbol lists)

### Context Expansion

Expand contexts inline before sending to maintain transparency.

**Example**:
```markdown
## Prompt

Please review this code:

@file:init.el

--- Expanded Context ---
**File: init.el** (250 lines, 8.2 KB)
```elisp
;;; init.el --- Main configuration
[... file contents ...]
```
--- End Context ---

What improvements would you suggest?
```

**Rationale**:
- User sees exactly what was sent to Claude
- Easy to verify correct files were attached
- Maintains conversation history clarity
- Supports debugging and iteration

### Integration Points

- Hook into `claude-repl-buffer-send-input` to process @-mentions
- Add to `completion-at-point-functions` in `claude-repl-buffer-mode`
- Use `cape` for enhanced CAPF features (file path completion, etc.)
- Integrate with `projectile` for project-aware file completion
- Use `consult` for advanced file/symbol selection

## Data Structures

### Context Object

```elisp
(cl-defstruct claude-repl-context
  "Structure representing an attached context item."
  type              ; Symbol: 'file, 'folder, 'url, 'symbol, 'buffer
  value             ; String: the identifier (path, URL, symbol name, etc.)
  content           ; String: expanded content
  metadata          ; Alist: additional info (size, line-range, etc.)
  start-pos         ; Integer: position in input where @-mention starts
  end-pos)          ; Integer: position in input where @-mention ends
```

### Context Registry

```elisp
(defvar claude-repl-context-type-alist
  '((file . (:prefix "@file:"
             :expander claude-repl-context--expand-file
             :completer claude-repl-context--complete-file
             :icon "nf-md-file"))
    (folder . (:prefix "@folder:"
               :expander claude-repl-context--expand-folder
               :completer claude-repl-context--complete-folder
               :icon "nf-md-folder"))
    ;; ... more types
    )
  "Registry of context types and their handlers.")
```

## Testing Strategy

### Unit Tests (`test/claude-repl-context-test.el`)

- Test context type detection and parsing
- Test each context expansion function
- Test error handling (missing files, network errors)
- Test line range parsing and validation
- Test context formatting

**Example Test**:
```elisp
(describe "Context expansion"
  (it "expands @file: to file contents"
    (let ((temp-file (make-temp-file "test-file" nil ".el" ";; test content\n")))
      (expect (claude-repl-context--expand-file temp-file)
              :to-match ";; test content")
      (delete-file temp-file))))
```

### Integration Tests

- Test completion triggering and filtering
- Test full prompt expansion pipeline
- Test interaction with evil-mode in input area
- Test marker handling with context expansion

### Manual Testing Checklist

- [ ] Type `@` shows completion popup
- [ ] Selecting `@file:` shows project files
- [ ] Fuzzy matching works (typing "init" finds "init.el")
- [ ] Line ranges work correctly
- [ ] Expanded context displays properly in buffer
- [ ] Large files trigger warnings
- [ ] Missing files show error messages
- [ ] Works with evil-mode keybindings
- [ ] History navigation preserves @-mentions

## Configuration API

Provide customization options:

```elisp
(defgroup claude-repl-context nil
  "Context attachment for claude-repl."
  :group 'claude-repl-buffer)

(defcustom claude-repl-context-types
  '(file folder url symbol buffer)
  "Enabled context types for @-mention completion."
  :type '(set (const file) (const folder) (const url)
              (const symbol) (const buffer))
  :group 'claude-repl-context)

(defcustom claude-repl-context-max-size (* 100 1024)
  "Maximum size in bytes for a single context attachment.
Larger contexts will be truncated with a warning."
  :type 'integer
  :group 'claude-repl-context)

(defcustom claude-repl-context-folder-excludes
  '(".git" "node_modules" "__pycache__" "target"
    "dist" "build" ".eldev" "straight")
  "Directories to exclude when attaching folders."
  :type '(repeat string)
  :group 'claude-repl-context)

(defcustom claude-repl-context-show-preview t
  "Show preview of expanded context before sending.
When enabled, displays a confirmation buffer showing what
context will be sent to Claude."
  :type 'boolean
  :group 'claude-repl-context)

(defcustom claude-repl-context-file-extensions
  '("el" "py" "js" "ts" "java" "go" "rs" "c" "h" "cpp"
    "md" "org" "txt" "json" "yaml" "yml" "toml" "xml")
  "File extensions to prioritize in folder context.
When attaching a folder, files with these extensions will
be included first."
  :type '(repeat string)
  :group 'claude-repl-context)

(defcustom claude-repl-context-completion-icon-type 'nerd-icons
  "Type of icons to use in completion.
Options: \\='nerd-icons, \\='emoji, \\='none"
  :type '(choice (const :tag "Nerd Icons" nerd-icons)
                 (const :tag "Emoji" emoji)
                 (const :tag "None" none))
  :group 'claude-repl-context)
```

## Documentation Updates

### README.md

Add section: "Attaching Context with @-Mentions"

```markdown
## Attaching Context with @-Mentions

Claude-repl supports attaching files, folders, and other context to your prompts
using @-mention syntax:

### Basic Usage

Type `@` in the input area to see available context types:
- `@file:` - Attach a file
- `@folder:` - Attach a directory
- `@url:` - Fetch and attach a URL
- `@symbol:` - Attach a symbol definition
- `@buffer:` - Attach buffer contents

### Examples

```
@file:init.el
```
Attaches the entire init.el file to your prompt.

```
@file:src/main.rs:100-150
```
Attaches lines 100-150 from src/main.rs.

```
@folder:src/components/
```
Attaches all files in the src/components/ directory.

### Completion

Press TAB after typing `@` to trigger completion. The completion
system supports fuzzy matching and shows file sizes and types.
```

### Inline Documentation

Add comprehensive docstrings to all public functions:
- `claude-repl-context-attach`
- `claude-repl-context-expand-prompt`
- `claude-repl-completion-at-point`
- etc.

## Future Enhancements

Post-v1.0 improvements:

1. **Context Templates/Snippets**
   - Save frequently-used context combinations
   - Quick insertion via completion

2. **Smart Context Suggestions**
   - Analyze conversation to suggest relevant files
   - ML-based context recommendation

3. **Context Diff Tracking**
   - Show what changed between prompts
   - Version control integration

4. **Multi-file Context Management UI**
   - Dedicated buffer for managing attached context
   - Drag-and-drop interface

5. **Context Search**
   - Search across past conversations by context
   - Find all prompts that referenced a specific file

6. **Org-mode Integration**
   - Export conversations with context to org files
   - Use org links for context references

7. **Context Caching**
   - Cache expanded contexts for faster re-use
   - Smart invalidation on file changes

8. **Semantic Context**
   - `@function:name` - Attach function and dependencies
   - `@class:Name` - Attach class definition and related code
   - `@test:name` - Attach test and implementation

## Implementation Checklist

### Phase 1: Core Infrastructure
- [ ] Create `claude-repl-context.el`
- [ ] Define context type registry
- [ ] Implement `@file:` expansion (whole file)
- [ ] Implement basic file completion
- [ ] Hook into `claude-repl-buffer-send-input`
- [ ] Display expanded context in buffer
- [ ] Write unit tests for file expansion
- [ ] Write integration test for end-to-end flow

### Phase 2: Enhanced Completion
- [ ] Implement completion for all basic types
- [ ] Add icons to completion candidates
- [ ] Add file size annotations
- [ ] Implement line range parsing
- [ ] Add folder expansion with filtering
- [ ] Integrate with projectile
- [ ] Add fuzzy matching support
- [ ] Test all completion scenarios

### Phase 3: Advanced Context Types
- [ ] Implement `@symbol:` with LSP
- [ ] Implement `@url:` with caching
- [ ] Implement `@buffer:` completion
- [ ] Add context size warnings
- [ ] Add context truncation
- [ ] Add context favorites/history
- [ ] Test advanced features

### Phase 4: UI Polish
- [ ] Collapsible context sections
- [ ] Clickable file paths
- [ ] Token cost estimation
- [ ] Context preview command
- [ ] Context removal/editing
- [ ] Custom faces for context display
- [ ] Polish all UI elements

## References

- WISHLIST.md (lines 7-17): Original feature request
- claude-repl-buffer.el (lines 1172-1228): Input area implementation
- claude-repl-core.el (lines 55-115): Prompt sending logic
- modules/completion.el: Existing completion configuration
- Cape documentation: https://github.com/minad/cape
- Corfu documentation: https://github.com/minad/corfu

## Questions to Resolve

1. **Token Counting**: Should we estimate token counts client-side or wait for API response?
   - **Decision**: Implement rough estimate (char count / 4) with warning threshold

2. **Large Context Handling**: Auto-truncate or require user confirmation?
   - **Decision**: Warn but don't block; let user decide

3. **Context Caching**: Cache parsed contexts to avoid re-reading files?
   - **Decision**: Phase 1 will re-read every time; optimize in Phase 3

4. **Folder Recursion**: How deep should `@folder:` recurse?
   - **Decision**: Add `claude-repl-context-folder-max-depth` (default: 3)

5. **Error Display**: Where to show expansion errors (minibuffer, buffer, both)?
   - **Decision**: Show in buffer as part of expanded context section

---

**Next Steps**:
1. Get user feedback on this plan
2. Start Phase 1 implementation
3. Create feature branch: `feature/file-context-management`
4. Implement TDD: tests first, then implementation
