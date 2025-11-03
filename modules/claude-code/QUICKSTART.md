# Claude Code REPL - Quick Start Guide

## Installation

### 1. Eldev (Build Tool)

```bash
curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh
```

### 2. Project Setup

The Claude Code REPL is already integrated into your Emacs config at:
`/Users/edward/Projects/edmacs/modules/claude-code/`

Reload your Emacs configuration to activate it.

## Basic Usage

### Ask Claude a Question

```
SPC a c a
"What is the best way to learn Emacs?"
```

A beautiful markdown-rendered response buffer will open with:
- Your prompt and timestamp
- Claude's streaming response
- Tool usage details
- Token statistics

### Continue the Conversation

Just ask another question! All interactions stay in the same buffer:

```
SPC a c a
"Can you show me an example?"
```

### View Previous Conversations

```
SPC a c b
```

Opens the response buffer for the current project.

### Start Fresh

```
SPC a c c
```

Clears the conversation history.

## All Commands

| Key | Command | Description |
|-----|---------|-------------|
| `SPC a c a` | Ask | Ask Claude a question |
| `SPC a c b` | Buffer | Open response buffer |
| `SPC a c c` | Clear | Clear conversation |
| `SPC a c s` | Start | Start process |
| `SPC a c k` | Kill | Kill process |
| `SPC a c l` | List | List processes |
| `SPC a c i` | Info | Show status |

### In Response Buffer

| Key | Command |
|-----|---------|
| `q` | Quit window |
| `c` | Copy last response |
| `C-c C-c` | Copy code block at point |

## Development

### Run Tests

```bash
cd /Users/edward/Projects/edmacs/modules/claude-code
eldev test
```

### Run All Checks

```bash
eldev check  # compile + test + lint
```

### Quick Demo

```bash
eldev demo
```

## Troubleshooting

### "Command not found: claude"

Ensure Claude Code CLI is installed and in your PATH:
```bash
which claude
```

### "No response appearing"

Check the process buffer:
```
SPC a c l  # List processes
```

Then inspect the process buffer for errors.

### "Eldev command not found"

Install Eldev (see Installation section above).

## Features

✅ Beautiful markdown-rendered responses
✅ Real-time streaming output
✅ Conversation history per project
✅ Syntax-highlighted code blocks
✅ Tool usage visualization
✅ Token usage statistics
✅ Project-aware (one conversation per project)

## Next Steps

- Read [README.md](README.md) for detailed features
- See [DEVELOPMENT.md](DEVELOPMENT.md) for development workflow
- Check [TESTING.md](TESTING.md) for writing tests
- Review [PHASE1_COMPLETE.md](PHASE1_COMPLETE.md) and [PHASE2_COMPLETE.md](PHASE2_COMPLETE.md) for architecture details

## Quick Reference

### File Structure
```
claude-code/
├── claude-code-process.el   # Process management
├── claude-code-buffer.el    # Response UI
├── claude-code-core.el      # User commands
├── Eldev                    # Build configuration
├── test/                    # Test suite
└── docs/                    # Documentation
```

### Architecture
```
User Command (SPC a c a)
    ↓
claude-code-core (high-level)
    ↓
claude-code-process (manages `claude --print`)
    ↓
claude-code-buffer (beautiful UI)
    ↓
Response Buffer (markdown-rendered)
```

## Tips

1. **One buffer per project**: Each project gets its own conversation buffer
2. **Streaming is live**: Watch responses appear in real-time
3. **Markdown everywhere**: Full markdown syntax including code blocks
4. **Copy-friendly**: Easy commands to copy responses and code
5. **Process-aware**: Check process status anytime with `SPC a c i`

## Support

- Check [Issues](https://github.com/anthropics/claude-code/issues) for Claude Code CLI
- Read the [Plan](plans/CLAUDE_CODE_REPL.md) for future features
- Phases 1-2 complete, Phases 3-8 coming soon!

---

**Happy coding with Claude!** 🚀
