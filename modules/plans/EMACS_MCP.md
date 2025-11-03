# Emacs MCP Server Integration Plan

## Goal
Integrate the claude-code-ide.el MCP server functionality into our existing vterm-based Claude Code setup to provide ediff-based diff viewing while maintaining the terminal interface.

## Architecture Overview

```
┌─────────────────┐         ┌──────────────────┐         ┌─────────────┐
│  Claude Code    │ MCP     │  Emacs MCP       │  Elisp  │   Ediff     │
│  (in vterm)     │────────>│  Server          │────────>│   Buffers   │
│                 │<────────│  (ai.el)         │<────────│             │
└─────────────────┘         └──────────────────┘         └─────────────┘
     Terminal                   Same Emacs                  GUI
```

## Implementation Plan

### Phase 1: Understand claude-code-ide.el MCP Implementation

**Tasks:**
1. Clone and study the claude-code-ide.el repository
2. Identify the MCP server components:
   - `claude-code-ide-mcp-server.el` - Server initialization
   - `claude-code-ide-mcp-handlers.el` - Tool handlers (`openDiff`, etc.)
   - MCP protocol communication layer
3. Document the MCP tools provided:
   - `openDiff` - Show diff in ediff
   - Any other IDE integration tools
4. Understand server lifecycle (start/stop/connection management)

### Phase 2: Extract MCP Server Core

**Tasks:**
1. Determine if we can use claude-code-ide.el as a dependency
   - Check if it's available via MELPA/straight
   - Or vendor the relevant files into our modules
2. Identify minimal components needed:
   - MCP protocol handler
   - `openDiff` tool implementation
   - Ediff integration code
3. Create compatibility layer if needed

### Phase 3: Add MCP Server to ai.el

**Tasks:**
1. Add MCP server package dependencies to ai.el:
   ```elisp
   (use-package claude-code-ide
     :straight (:host github :repo "manzaltu/claude-code-ide.el")
     ;; Or vendor specific files
     )
   ```

2. Create MCP server configuration:
   ```elisp
   (defvar claude-code-mcp-server-port 3000
     "Port for the Claude Code MCP server.")

   (defvar-local claude-code-mcp-server-process nil
     "MCP server process for this project.")
   ```

3. Write server lifecycle functions:
   ```elisp
   (defun claude-code-mcp-start ()
     "Start the MCP server for Claude Code ediff integration.")

   (defun claude-code-mcp-stop ()
     "Stop the MCP server.")

   (defun claude-code-mcp-restart ()
     "Restart the MCP server.")
   ```

### Phase 4: Integrate with Existing Functions

**Tasks:**
1. Modify `claude-code-vterm` to:
   - Start MCP server when creating Claude Code session
   - Configure Claude Code to use the MCP server
   - Set environment variables or config for MCP connection

2. Create MCP configuration for Claude Code:
   ```elisp
   (defun claude-code-mcp--create-config (project-root)
     "Create MCP configuration for PROJECT-ROOT."
     ;; Generate ~/.config/claude/mcp.json or project-specific config
     ;; with Emacs MCP server connection details
     )
   ```

3. Modify `claude-code-close` to:
   - Stop MCP server when closing session
   - Clean up any temporary buffers/processes

### Phase 5: Configuration & Preferences

**Tasks:**
1. Add customization variables:
   ```elisp
   (defcustom claude-code-use-ediff t
     "Use ediff for viewing Claude Code diffs."
     :type 'boolean
     :group 'claude-code)

   (defcustom claude-code-mcp-auto-start t
     "Automatically start MCP server with Claude Code session."
     :type 'boolean
     :group 'claude-code)
   ```

2. Create toggle commands:
   ```elisp
   (defun claude-code-toggle-ediff ()
     "Toggle ediff integration on/off.")
   ```

### Phase 6: Enhanced Features

**Optional enhancements:**

1. **Diff History**:
   - Keep track of accepted/rejected diffs
   - Allow reviewing previous diffs

2. **Batch Operations**:
   - Accept/reject multiple diffs at once
   - Queue diffs for review

3. **Integration with Existing Workflows**:
   - Integrate with magit
   - Show diff stats
   - Create commits from accepted diffs

4. **Additional MCP Tools**:
   - File navigation (`jumpToFile`)
   - Symbol lookup
   - Project-wide search integration

### Phase 7: Testing & Documentation

**Tasks:**
1. Test scenarios:
   - Creating new Claude Code session with MCP
   - Multiple concurrent sessions
   - Server restart/recovery
   - Ediff accept/reject workflows

2. Document in ai.el:
   - How MCP integration works
   - Configuration options
   - Troubleshooting common issues

3. Add keybindings:
   ```elisp
   "am" '(claude-code-mcp-start :which-key "start mcp server")
   "aM" '(claude-code-mcp-stop :which-key "stop mcp server")
   "ad" '(claude-code-toggle-ediff :which-key "toggle ediff")
   ```

## Technical Considerations

### MCP Server Communication
- **Protocol**: JSON-RPC over stdio or HTTP
- **Tools**: Must implement at minimum `openDiff`
- **State**: Track active diffs per project/session

### Ediff Integration
- **Buffer management**: Clean up temporary buffers
- **User experience**: Maintain focus/window state
- **Evil integration**: Ensure keybindings work in ediff

### Process Management
- **Lifecycle**: Server per project or global singleton?
- **Error handling**: What if server crashes?
- **Port conflicts**: Handle multiple Emacs instances

### Claude Code Configuration
- **MCP config file**: `~/.config/claude/mcp.json` or project-specific
- **Environment variables**: For connection details
- **Tool discovery**: How Claude Code finds available tools

## Alternative Approaches

### Option A: Full claude-code-ide.el Integration
- Install entire claude-code-ide.el package
- Use its MCP server as-is
- Extend with our vterm integration
- **Pros**: Well-tested, feature-complete
- **Cons**: May conflict with our vterm approach, heavier dependency

### Option B: Minimal MCP Implementation
- Build our own minimal MCP server
- Only implement `openDiff` tool
- Keep it simple and focused
- **Pros**: Full control, minimal dependencies
- **Cons**: More work, may miss features

### Option C: Hybrid Approach (Recommended)
- Use claude-code-ide.el's MCP components as library
- Keep our vterm-based UI
- Best of both worlds
- **Pros**: Reuse proven code, maintain our UX
- **Cons**: Need to understand integration points

## Success Criteria

1. ✅ Claude Code session starts with MCP server
2. ✅ File edits trigger ediff instead of direct modification
3. ✅ User can accept/reject changes in ediff
4. ✅ Accepted changes are applied by Claude Code
5. ✅ Server lifecycle managed automatically
6. ✅ No conflicts with existing vterm workflow
7. ✅ Documentation and keybindings in place

## Open Questions

1. Does claude-code-ide.el support being used as a library, or must we vendor code?
2. What's the best way to configure Claude Code to use our MCP server?
3. Should we support both ediff and terminal diffs, letting users choose?
4. How do we handle multiple Claude Code sessions? One MCP server or multiple?
5. Can we extend this to other IDEs/tools beyond just ediff?

## References

- [claude-code-ide.el GitHub](https://github.com/manzaltu/claude-code-ide.el)
- [Model Context Protocol Specification](https://modelcontextprotocol.io/)
- [Claude Code MCP Documentation](https://docs.claude.com/en/docs/claude-code/mcp.md)
