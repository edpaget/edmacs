# edmacs - Instructions for Claude

This document provides guidance for Claude when working on this Emacs configuration repository.

## Repository Overview

This is **edmacs**, a modern, modular Emacs configuration with evil-mode, version-locked packages, and AI assistant integration. The repository contains:

- Top-level Emacs configuration files (`init.el`, `early-init.el`, etc.)
- Multiple configuration modules in `modules/` directory
- A `claude-term` integration (`modules/claude-term.el`, `modules/claude-term-registry.el`) that hosts the interactive Claude CLI in a ghostel terminal buffer

## Working on the Emacs Modules

Everything under `modules/` is a plain Emacs Lisp file loaded by `init.el`.
There is no package manifest, no Eldev, and no CI -- `straight.el` handles
third-party packages and the modules themselves are loaded by path.

### Testing

Tests are plain [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/)
suites living beside the code they cover, as `modules/<module>-test.el`:

- `modules/claude-term-test.el` -- pure-function coverage of `claude-term.el`
- `modules/claude-term-live-test.el` -- kill/restart/exit lifecycle against real subprocesses
- `modules/claude-term-registry-test.el` -- pure-function coverage of the session registry
- `modules/claude-term-registry-live-test.el` -- registry wiring that needs a real spawn

Run a suite in batch from the repository root, loading the modules it
depends on first:

```bash
emacs -Q --batch -l ert -l modules/git-common-dir.el \
      -l modules/claude-term.el \
      -l modules/claude-term-registry.el \
      -l modules/claude-term-registry-test.el \
      -f ert-run-tests-batch-and-exit
```

Each test file's own `;;; Commentary:` header carries its exact
invocation, including the `*-live-test.el` variants and whatever they
need in the environment. Add new tests to the existing `-test.el` file
for the module -- **never** create throwaway files like `/tmp/test-*.el`.

### Compilation

Modules are loaded from source, not byte-compiled as a build step. To check
a module compiles clean:

```bash
emacs -Q --batch -f batch-byte-compile modules/claude-term.el
```

Delete the resulting `.elc` afterwards -- compiled output is not committed.

### Startup check

The cheapest regression test for any module change is a clean batch start:

```bash
emacs --init-directory=$PWD --batch -f kill-emacs 2>&1 \
  | grep -Ei 'error|void-function|Cannot open load file'
```

No output means `init.el` loaded every module without error.

## Overall Repository Structure

```
edmacs/
├── early-init.el           # Performance optimizations
├── init.el                 # Main entry point
├── modules/                # Configuration modules
│   ├── core.el            # Basic settings
│   ├── evil-config.el     # Evil mode configuration
│   ├── keybindings.el     # General.el keybindings
│   ├── ui.el              # Theme and appearance
│   ├── completion.el      # Vertico, Corfu, Consult
│   ├── programming.el     # LSP, Flycheck, Apheleia
│   ├── ai.el              # Markdown/olivetti editor polish
│   ├── claude-term.el     # Claude CLI hosted in a ghostel terminal
│   ├── claude-term-registry.el # Session registry + SPC a keymap
│   ├── git-common-dir.el  # Worktree-aware git dir resolution
│   ├── sessions.el        # desktop.el session persistence
│   ├── org-config.el      # Org mode configuration
│   ├── git.el             # Magit and git tools
│   ├── vterm.el           # Terminal configuration
│   ├── *-test.el          # ERT suites, run in batch (see above)
│   └── languages/         # Language-specific configs
├── straight/
│   └── versions/          # Package version lockfiles (committed)
└── README.md              # Main repository documentation
```

### Development Workflow

1. Edit the module under `modules/`
2. Add or update the matching `modules/<module>-test.el`
3. Run that suite in batch (see Testing above)
4. Run the startup check
5. Reload interactively (`M-x eval-buffer`) or restart Emacs to confirm behaviour

## Tool Approval

Claude sessions in Emacs run the **real interactive CLI** under ghostel, so
the CLI raises its own permission prompts exactly as it does in a terminal.
Emacs adds no approval layer of its own: there is no `PreToolUse` hook, and
no code path spawns `claude` with a generated `--settings` file.

Permission behaviour is therefore governed entirely by
`permissions.defaultMode` in the user's `claude/settings.json` (which lives
in the separate `dotfiles` repository, not here), and is identical for a
terminal session and an Emacs-hosted one. Do not reintroduce an Emacs-side
gate: a `PreToolUse` hook can only *tighten* the decision -- staying silent
never approves -- so one would either double-prompt or silently loosen the
policy.

The hooks that *are* wired into `settings.json` (Notification, PostToolUse,
Stop, UserPromptSubmit, SessionEnd) are observational: they report session
status back into Emacs and make no permission decision.

## Best Practices

- **Test-driven development**: Add tests before or alongside code changes
- **Use existing test infrastructure**: Don't create ad-hoc test files
- **Follow the module pattern**: Keep related functionality in the appropriate module
- **Byte-compile clean**: Code should compile without warnings
- **Check startup**: A batch start with no errors is the minimum bar for any module change
- **Use TodoWrite actively**: Track progress on multi-step tasks to help users understand what's being done
