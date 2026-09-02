# Design Document: Diff Viewing and Editing in Tool Approvals

**Author**: Claude
**Date**: 2025-11-08
**Status**: DRAFT - Awaiting Approval
**Version**: 1.0

---

## 1. Executive Summary

This document describes the design for adding diff viewing and interactive editing capabilities to the claude-repl approval system. The enhancement will allow users to:

1. **View diffs** of proposed changes for Edit and Write tools
2. **Edit the proposed changes** before approving/denying
3. **Preview file contents** for Write tool operations
4. **Extend to MCP server tools** that modify files

The design uses Emacs's built-in `diff-mode` for displaying diffs, providing a lightweight, well-integrated solution.

---

## 2. Goals and Non-Goals

### Goals

- Display unified diffs for Edit tool (old_string → new_string)
- Display file previews for Write tool (content to be written)
- Allow interactive editing of proposed changes before approval
- Maintain backward compatibility with existing approval workflow
- Use built-in Emacs functionality (diff-mode)
- Provide extensibility for future tools (MCP servers)
- Preserve existing approval modes (interactive, auto-approve, deny-all, hybrid)

### Non-Goals

- Side-by-side diff viewing (ediff-style)
- Interactive merge conflict resolution
- Automatic application of changes (still requires approval)
- Version control integration beyond simple diffs
- Multi-file diff viewing in single buffer

---

## 3. Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│  Claude Code sends tool request                         │
│  (via approval-hook.py → Unix socket)                   │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│  claude-repl-approval.el                                │
│  ├─ Receives tool request                               │
│  ├─ Checks if tool supports diffs                       │
│  └─ Calls diff generator if applicable                  │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│  claude-repl-diff.el (NEW MODULE)                       │
│  ├─ Generate diff for Edit tool                         │
│  ├─ Generate preview for Write tool                     │
│  ├─ Use diff-no-select for diff generation              │
│  └─ Return formatted diff buffer                        │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│  Enhanced Approval Buffer                               │
│  ├─ Tool info section (existing)                        │
│  ├─ Parameters section (existing)                       │
│  ├─ Diff/Preview section (NEW)                          │
│  │   └─ Embedded diff buffer with syntax highlighting   │
│  └─ Actions section (enhanced)                          │
│      ├─ [a] Allow                                       │
│      ├─ [d] Deny                                        │
│      ├─ [e] Edit (NEW)                                  │
│      ├─ [A] Always Allow                                │
│      ├─ [D] Always Deny                                 │
│      └─ [i] Interrupt                                   │
└─────────────────────────────────────────────────────────┘
```

---

## 4. Component Design

See sections below for detailed component design including:
- claude-repl-diff.el module API
- Enhanced approval buffer
- Edit buffer mode
- Detailed workflows
- Data structures
- UI/UX design

---

## 5. Implementation Plan

### Phase 1: Core Diff Module
1. Create claude-repl-diff.el
2. Implement diff generation for Edit tool
3. Implement preview generation for Write tool
4. Add configuration variables
5. Write unit tests

### Phase 2: Approval Integration
1. Modify approval UI rendering to include diff section
2. Implement diff buffer embedding/display
3. Update approval buffer layout
4. Test with real approval requests

### Phase 3: Interactive Editing
1. Create claude-repl-edit-mode
2. Implement edit action handler
3. Implement confirm/cancel handlers
4. Add diff regeneration after edits
5. Test edit workflow

### Phase 4: Testing and Polish
1. Write integration tests
2. Manual testing with various file types
3. Performance testing
4. Error handling verification
5. Documentation

**Estimated Effort**: 8-12 hours

---

## 6. Key Design Decisions

### Use diff-mode (not ediff)
- Lightweight single-buffer display
- Excellent syntax highlighting
- Easy to embed in approval buffer
- Standard unified diff format
- Built-in refinement for character-level changes

### Edit workflow
- Press 'e' to edit proposed changes
- Opens dedicated edit buffer
- User modifies content
- C-c C-c to confirm, C-c C-k to cancel
- Regenerate diff with modified content
- Then approve/deny

### Extensibility
- Registry for tool-specific diff handlers
- Support for MCP server tools
- Custom diff generators

---

## 7. Success Criteria

- Users can view diffs for Edit tool requests
- Users can view previews for Write tool requests
- Users can edit proposed changes before approving
- All existing approval functionality works unchanged
- Unit tests pass with >90% coverage
- Performance <100ms for typical diffs
- Comprehensive documentation

---

## 8. Open Questions

1. Does Claude Code hook support modifiedInput in responses?
2. How to handle binary files in Write tool?
3. Should we limit size of interactive edits?

---

## Appendix: Detailed Specifications

For complete API specifications, data structures, workflows, error handling, and examples, see the full design document sections.

