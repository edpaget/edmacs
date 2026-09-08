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

#### Running everything before landing

```bash
scripts/test-all.sh all          # every suite, every tier
scripts/test-all.sh batch        # just the plain -Q --batch tier
scripts/test-all.sh pty          # just the four suites needing a controlling terminal
scripts/test-all.sh gui          # just the suites needing a real graphical frame
```

This is the single pre-landing gate, driven by the manifest in
`scripts/test-manifest.sh` -- one row per (suite, tier) pair, covering
every file under `modules/*-test.el` across the batch/pty/gui tiers this
document describes below. It reuses `scripts/run-ert-suite.sh` for the
wall-budget check on batch and pty rows, `scripts/pty-ert.sh` to attach a
controlling terminal for the pty tier, and `scripts/gui-ert.sh` for the
gui tier (skipping a gui row gracefully, without failing the run, when no
window server is available to create a frame at all -- distinct from a
real failure inside a frame that WAS created). It prints one line per row
(suite, tier, tests, unexpected, skipped, wall time) plus a final TOTAL,
and exits non-zero on any unexpected result.

The skip bar is per (suite, tier), not per suite: a suite that legitimately
runs in more than one tier (the four pty suites also have a batch row; three
suites have both a batch and a gui row) gets its own `expected_skips_on_main`
per row in the manifest. The four pty suites' documented GUI-only skip counts
below are the accepted baseline ONLY on their batch-tier row -- their
pty-tier row must show zero skips, since attaching a pty is the whole reason
that tier exists; a skip surviving it is a regression, not baseline noise.
This stricter-per-tier bar is enforced only when run from the main checkout
(detected via a populated `straight/build/`) -- a worktree legitimately sees
more skips across the board, so there the script still fails on any
unexpected result but does not additionally enforce the skip bar.

Adding a 25th `modules/*-test.el` file, or a new tier for an existing one,
means adding its row(s) to `scripts/test-manifest.sh` -- that file, and
`modules/test-support.el`'s shared fixtures below, are the two places new
test infrastructure belongs, not a fresh copy-paste into the new file.

Tests are plain [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/)
suites living beside the code they cover, as `modules/<module>-test.el`:

- `modules/claude-term-test.el` -- pure-function coverage of `claude-term.el`
- `modules/claude-term-live-test.el` -- kill/restart/exit lifecycle against real subprocesses
- `modules/claude-term-registry-test.el` -- pure-function coverage of the session registry
- `modules/claude-term-registry-live-test.el` -- registry wiring that needs a real spawn
- `modules/claude-term-approval-parity-live-test.el` -- runs the real `claude`
  binary to prove an Emacs-hosted session resolves the same permission
  policy as a terminal one (see Tool Approval below)
- `modules/windows-test.el` -- master-and-stack layout, popup routing, the
  `SPC w` command set, tab/desktop persistence, the `display-buffer` catch-all
- `modules/ui-test.el`, `modules/sidebar-test.el` -- ui.el and sidebar.el
- `modules/ui-live-test.el` -- term-mode nano-modeline rendering, real
  `nano-modeline` package loaded from the straight build tree; two-tier
  (process-less buffer, then a live `make-term` subprocess via
  `scripts/gui-ert.sh`)
- `modules/window-geometry-live-test.el` -- sidebar side-window width and
  fringe assertions; two-tier (see GUI-only geometry assertions below). The
  standard batch invocation shows 4 of its 10 tests as skipped -- that
  is the documented GUI-only gate, not a regression; run them through
  `scripts/gui-ert.sh`

Run a suite in batch from the repository root, loading the modules it
depends on first:

```bash
emacs -Q --batch -l ert -l modules/test-support.el \
      -l modules/git-common-dir.el \
      -l modules/claude-term.el \
      -l modules/claude-term-registry.el \
      -l modules/claude-term-registry-test.el \
      -f ert-run-tests-batch-and-exit
```

Each test file's own `;;; Commentary:` header carries its exact
invocation, including the `*-live-test.el` variants and whatever they
need in the environment. Add new tests to the existing `-test.el` file
for the module -- **never** create throwaway files like `/tmp/test-*.el`.

`modules/test-support.el` is the shared fixture module for cross-file
test infrastructure: the straight `build`/`repos` root locators, the
`magit-section` dependency-path setup, the second-real-frame-or-skip and
graphical-frame-or-skip helpers, sidebar/agent-state and tab-restore
fixtures, the parameterized wedged-frame builder, and the
`with-hermetic-state` timer/buffer/`tab-bar-mode` snapshot-restore macro.
A new suite that needs one of these loads it with `-l modules/test-support.el`
and calls the shared definition rather than pasting a new local copy --
see that file's own Commentary for the full list and for why (this repo
already relearned the cost of that duplication once).

#### A skipped test is unverified coverage, not a pass

Read the whole `Ran N tests, ...` line, not just `0 unexpected`:

```
Ran 60 tests, 58 results as expected, 0 unexpected, 2 skipped
```

That run is **not** green. A test whose helper module is absent calls
`ert-skip` rather than failing, so a narrower invocation silently drops
coverage while still exiting 0. Treat any non-zero `skipped` as a result
you have not yet obtained: read the skip message (it names what is
missing), add that `-l modules/<dep>.el`, and re-run until the skip count
is zero or you can say why the skip is legitimate.

A file's own header invocation is not automatically the complete one.
Worked example (fixed): `modules/windows-test.el`'s header once omitted
`-l modules/claude-term.el`, and two of its tests silently skipped under
it -- one of which failed once the module *was* loaded, a regression a
green landing run missed entirely. The header now includes that `-l`.
Cross-module tests generally need every module they touch on the command
line, and until a header is corrected the exhaustive invocation may live
in a comment further down the file rather than in the header itself.

This matters most before landing. Skips are also environment-dependent:
tests that load real packages out of `straight/` skip in a worktree, which
has no populated package tree, and only run from the main checkout -- so
the same suite legitimately reports different skip counts in the two
places. Run the suites from the main checkout when the skip count is what
you are trying to drive to zero.

#### `cl-letf` on a subr can silently cost 28 seconds

`cl-letf` on a C subr (`buffer-live-p`, `delete-frame`, `signal-process`,
`completing-read`, ...) forces Emacs to build a native-comp **subr
trampoline** for it -- a synchronous `call-process` out to the native
compiler, ~28s of wall clock with the CPU otherwise idle. The trampoline is
cached on disk once built, keyed by subr signature and shared across the
whole machine (`native-comp-eln-load-path`), so the cost can vanish for
weeks and then reappear the moment the cache is cold (a fresh machine, a
cleared eln-cache, a different Emacs build) or a new un-guarded `cl-letf`
target is introduced. On a build where the eln-cache is unwritable, the
compile can outright fail rather than merely being slow.

Guard any `-test.el` file that `cl-letf`s a subr with this, near the top,
after requires and before the first `ert-deftest` -- copy the guarded form
(the `(when (boundp ...))` wrapper avoids a byte-compiler "assignment to
free variable" warning on a non-native-comp build; an unconditional `setq`
does *not* error there -- `setq` on an unbound symbol is never a runtime
error in Elisp -- several sibling files below already use the unconditional
form and run fine, but the guarded form is the one to copy going forward):

```elisp
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))
```

`modules/sessions-test.el` carries the canonical comment to copy alongside
it. Incident: `modules/windows-test.el` regressed from 4.2s to 282s this
way (5 tests each paying ~28s, one of them 6 times over); the guard above
brought it back to about 0.3s. `modules/claude-term-test.el`,
`modules/claude-term-registry-test.el`, and
`modules/claude-term-registry-live-test.el` had the identical un-guarded
gap and got the same fix.

That incident passed `108/108` the entire time -- ERT has no notion of a
suite taking too long, so a 70x slowdown produced zero failures. Wrap any
ERT batch invocation you want protected against a repeat in
`scripts/run-ert-suite.sh <budget-seconds> <command...>`: it measures the
wrapped command's wall time and fails the run (regardless of the wrapped
command's own exit status) past the budget, and fails fast on a
non-numeric budget rather than silently treating it as zero. All four
files this incident touched (`windows-test.el`, `claude-term-test.el`,
`claude-term-registry-test.el`, `claude-term-registry-live-test.el`) now
route their documented invocation through it; their Commentary blocks show
the wired-in form. This wrapper is opt-in per suite, not a systemic guard
-- adopt it for any other suite you want the same protection on rather
than assuming ERT's exit code already covers it.

#### A second real frame needs a pty, not a window server

Several suites need a *second* frame to test per-frame state. They ask for a
tty frame (`(tty . "/dev/tty")`), so they need a **controlling terminal** --
which `emacs -Q --batch` started from a script, a CI runner, or an agent's
tool call does not have. Those tests call `ert-skip` rather than failing, so
the suite still exits 0 while silently testing less:

| suite | skips a pty clears |
|---|---|
| `sidebar-test.el` | 2 |
| `sidebar-buffers-live-test.el` | 2 |
| `sidebar-agents-live-test.el` | 1 |
| `workspaces-live-test.el` | 3 |

`sessions-test.el` no longer needs a pty: its per-frame restore walk went
single-frame, and the suite runs with zero skips under plain `-Q --batch`.

`scripts/gui-ert.sh` does **not** clear these. It supplies a graphical frame,
not a terminal, so `/dev/tty` is still absent inside it -- it is the right
tool for fringe and width assertions, the wrong one for these.

Attach a pty instead. `script -q /dev/null <cmd>` works from an interactive
shell -- but it fails wherever stdin is not itself a terminal
(`tcgetattr/ioctl: Operation not supported on socket`), which includes most
non-interactive contexts. `scripts/pty-ert.sh` allocates one directly (via
Python's `pty` module, translating its wait status into a real exit code)
and works in both; the four suites in the table above document it as their
pty invocation:

```bash
scripts/pty-ert.sh emacs -Q --batch -l ert -l modules/test-support.el \
      -l modules/git-common-dir.el -l modules/sidebar-test.el \
      -f edmacs-test-support-run-and-exit
```

Expect it to be slower -- `sidebar-test.el` goes from ~1.4s to ~9s -- because
a real terminal is being emulated. Worth it before landing: it is the only
way to drive that skip count to zero. `scripts/pty-ert.sh false; echo $?`
prints non-zero, confirming the wrapper propagates a wrapped failure rather
than the raw `pty.spawn` wait status.

#### GUI-only geometry assertions

Some assertions -- fringe pixels, scroll-bar width, the real gap between
`window-total-width` and `window-body-width` -- are unfalsifiable under
`emacs --batch`: a batch frame accepts `left-fringe`/`right-fringe` and
ignores them, so that gap is always exactly 1 there and 2-5 on a real GUI
frame. `scripts/gui-ert.sh` runs a suite inside an off-screen NS/X frame in
a **throwaway** daemon (its own server name, `edmacs-gui-ert-$$`) to make
those assertions real:

```bash
scripts/gui-ert.sh modules/window-geometry-live-test.el [selector] [-l extra.el ...]
```

It never touches the user's real daemon (server name `server`) and never
sets `--init-directory`, so it cannot bootstrap a second `straight` package
tree -- safe to run from a worktree or the main checkout. The throwaway
daemon is killed on every exit path, including a failure before any test
runs. A test's `:expected-result :failed` marker is honored here the same
way ERT's own batch reporter honors it: an expected failure is not counted
toward the script's exit code, only a genuinely unexpected result is.

#### Batch runs no command loop, so LSP clients never connect

`eglot-ensure` does not connect. It appends a buffer-local
`post-command-hook` entry and returns; the connection happens the next time
the command loop runs a command. `emacs --batch` runs no command loop, so
the obvious verification -- `find-file`, then poll `eglot-managed-p` --
spins to its deadline with **zero** processes spawned and reads as a broken
config. One `(run-hooks 'post-command-hook)` in the buffer fixes it.

Two more of the same shape sit behind it, and each one silently reports "no
diagnostics" rather than failing:

- flymake's first check on a buffer whose `flymake-mode` came up from
  `after-change-major-mode-hook` -- which is every buffer after the first in
  an already-managed project. Force it with `(flymake-start t t)`.
- a language's `lsp-deferred`/`eglot-ensure` hook entry, registered from a
  `with-eval-after-load` or `use-package` `:config` form. `<mode>-hook` is
  empty until the mode's own library is loaded, so `require` it before
  asserting anything about the hook list.

`scripts/go-eglot-check.sh` is the worked example -- Go under eglot and
gopls, end to end in batch, no frame and no screenshot. It builds a
throwaway Go module outside the repo (project.el would otherwise root a
module created inside a worktree at the edmacs repo, and gopls would report
"no packages"), runs it through `startup-check.sh`, and asserts the buffer
is eglot-managed, the server is gopls, `textDocument/definition` answers,
`lsp-mode` never attached, the `:gopls` workspace configuration reached the
server, and `edmacs-modeline-diagnostics` renders flymake's counts:

```bash
scripts/go-eglot-check.sh
```

It re-execs itself through a login shell when `gopls` is missing, because
the toolchain resolves through mise exactly as it does for the daemon via
`exec-path-from-shell`. Copy its shape for the next language moved to
eglot.

### Compilation

Modules are loaded from source, not byte-compiled as a build step. To check
a module compiles clean:

```bash
emacs -Q --batch -f batch-byte-compile modules/claude-term.el
```

Add `-L modules` for any module that `require`s a sibling module --
`sidebar.el`, `vterm.el`, `git.el` and `languages/clojure.el` all
`(require 'windows)`, which bare `-Q` cannot resolve:

```bash
emacs -Q --batch -L modules -f batch-byte-compile modules/git.el
```

Delete the resulting `.elc` afterwards -- compiled output is not committed.

### Linting

`scripts/ambient-reads.el` flags a function that reads ambient state
(`selected-window`, `selected-frame`, `default-directory`, ...) despite
being handed an explicit argument for that same thing -- the lint no
existing tool provides. Run it over every top-level module:

```bash
emacs -Q --batch -l scripts/ambient-reads.el -f edmacs-ambient-reads-batch modules/*.el
```

This is `modules/*.el` only -- it does not recurse into
`modules/languages/`. Findings are two-tier: `ERROR` (the caller supplied
the parameter and the function ignored it anyway) gates the batch exit
code; `WARN` (no such parameter exists -- the read is ambient by
construction, e.g. the `(or PARAM (selected-frame))` idiom) is
informational only. Suppress a legitimate residual finding with a
`;; ambient-reads: ok` comment on the finding's line or the line above --
there is no separate whitelist file. The lint's exit code is the source of
truth for whether every known ERROR is currently fixed, not any specific
function or file named here -- a fix that stands today can regress
silently if the lint is not re-run after a later change touches the same
code. Re-run it (`emacs -Q --batch -l scripts/ambient-reads.el -f
edmacs-ambient-reads-batch modules/*.el`) after any change to a function
that takes a frame/window/buffer/directory parameter, and confirm the
summary line reads `0 error` before trusting that count in isolation.

### Startup check

The cheapest regression test for any module change is a clean batch start.
Run it from the **main checkout only** -- it is the one command here that
loads the real config, and therefore the one that can corrupt the package
tree if pointed at a worktree (see Worktrees below):

```bash
emacs --batch --init-directory="$PWD" -l "$PWD/init.el" -f kill-emacs 2>&1 \
  | grep -Ei 'error|void-function|Cannot open load file'
```

No output means `init.el` loaded every module without error.

`-l init.el` is not redundant: `--batch` implies `-q`, so `--init-directory`
alone sets `user-emacs-directory` without ever loading `init.el`, and the
check silently passes no matter what is broken. Confirm a run really loaded
the config by asking it for something the config sets, e.g. appending
`--eval '(princ (format "%S\n" custom-enabled-themes))'` -- `nil` means init
never ran.

#### Checking a worktree's modules

The command above can only ever test the main checkout, because
`--init-directory` sets `user-emacs-directory` and `init.el` resolves both
straight's bootstrap *and* every `load-module` against it. That leaves a
worktree's changes unverifiable without copying them into the main checkout,
which races every other session working there.

`scripts/startup-check.sh` splits those two roles apart -- module sources from
one checkout, package tree from another:

```bash
scripts/startup-check.sh                  # this checkout's modules, main's packages
scripts/startup-check.sh <module-root> <package-root>
```

Run from a worktree it defaults `package-root` to the sibling `edmacs` main
checkout, and it refuses to start Emacs at all if that resolves somewhere
without a bootstrapped straight tree -- so it cannot bootstrap a second one.
Set `STARTUP_CHECK_EVAL` to an elisp form to assert module state after init;
lines it prints beginning `assert:` are echoed back and excluded from the
error grep:

```bash
STARTUP_CHECK_EVAL='(princ (format "assert: %S\n" window-sides-slots))' \
  scripts/startup-check.sh
```

## Worktrees

rdm roadmaps and tasks get their own git worktree under
`../edmacs__worktrees/<slug>/`, so several Claude sessions edit this config
at once. Module sources are per-worktree; **the package tree is not**.

`.gitignore` ignores `straight/*` except `straight/versions/`, so a fresh
worktree has a lockfile and nothing else -- no `straight/repos/`, no
`straight/build/`. There is exactly one populated package tree, in the main
checkout, and one Emacs daemon, whose `user-emacs-directory` is that main
checkout. A claude-term session opened in a worktree is just a different
project root inside that one daemon, not a second Emacs.

### Never point `--init-directory` at a worktree

`emacs --init-directory=<dir>` makes `<dir>` a full `user-emacs-directory`.
`init.el` then looks for `<dir>/straight/repos/straight.el/bootstrap.el`,
does not find it, and bootstraps a **second** package tree there -- cloning
and rebuilding 100+ packages, and littering the worktree with `eln-cache/`,
`auto-save/`, `backups/`, `recentf.eld`, and `history`. Worse, a build that
runs with `straight-base-dir` set to a worktree can leave the main
checkout's `straight/build/` full of symlinks into
`<worktree>/straight/repos/`, which no longer exists once the worktree is
cleaned or removed. Every package file then dangles and the daemon fails to
start on its next launch.

So: any command that loads the real config -- the bare startup check above,
and every script under `scripts/` except `startup-check.sh` -- runs from the
main checkout, even when the change under test lives in a worktree. Each
`scripts/*.sh` defaults `REPO_ROOT` to its own location, so running a
worktree's copy points it at the worktree. The `verify-*.sh` four take an
explicit root as `$1`; pass the main checkout. `startup-bench.sh` and
`gc-session-bench.sh` do not, so run those from the main checkout after
copying the change over.

Safe from anywhere, because `-Q` skips init entirely and never touches
straight: the ERT suites and `batch-byte-compile`. Those are the normal way
to verify a module change from inside a worktree.
`scripts/startup-check.sh` is safe from anywhere too, for a different reason
-- it keeps `user-emacs-directory` on the main checkout no matter which
checkout supplies the modules, and refuses to run if it cannot find a
bootstrapped straight tree there.

### If the package tree is already poisoned

Symptom: init dies with `Cannot open load file` for a file that plainly
exists under `straight/build/`, often followed by an eager-macro-expansion
failure. Confirm it, from the main checkout:

```bash
find straight/build -type l ! -exec test -e {} \; -print | head
```

Dangling links naming a path under `edmacs__worktrees/` are the signature.
Recovery is offline -- `straight/repos/` holds every clone already, so
nothing is re-fetched:

```bash
rm -rf straight/build straight/build-cache.el
emacs --batch --init-directory="$PWD" -l "$PWD/init.el" -f kill-emacs
```

Rebuilding every package takes a few minutes. Do it when no other session is
running Emacs commands against this checkout, or the next worktree-rooted
run will just poison it again.

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
│   ├── ai.el              # Markdown editor polish
│   ├── claude-term.el     # Claude CLI hosted in a ghostel terminal
│   ├── claude-term-registry.el # Session registry + SPC a keymap
│   ├── git-common-dir.el  # Worktree-aware git dir resolution
│   ├── workspaces.el      # Project/worktree identity on tab-bar groups
│   ├── sidebar.el         # Grouped project/worktree tree, buffers, agents
│   ├── windows.el         # Master-and-stack window management
│   ├── sessions.el        # desktop.el session persistence
│   ├── org-config.el      # Org mode configuration
│   ├── git.el             # Magit and git tools
│   ├── vterm.el           # Terminal configuration
│   ├── test-support.el    # Shared ERT fixtures, loaded with -l
│   ├── *-test.el          # ERT suites, run in batch (see above)
│   └── languages/         # Language-specific configs
├── scripts/
│   ├── test-all.sh        # The pre-landing gate: every suite, every tier
│   ├── test-manifest.sh   # One row per (suite, tier): loads, budget, skips
│   ├── pty-ert.sh         # Runs a suite under a controlling terminal
│   ├── gui-ert.sh         # Runs a suite in a throwaway graphical daemon
│   └── startup-check.sh   # Loads init.el with this checkout's modules
├── straight/
│   └── versions/          # Package version lockfiles (committed)
└── README.md              # Main repository documentation
```

### The tab-group model

One frame per Emacs instance. A project is a tab-bar *group*; a worktree of
that project is a *tab* inside its group, carrying its own root as the
`edmacs-workspace-root` tab parameter. `modules/workspaces.el` owns this
identity model and the `SPC p p` / `SPC T p` (`C-x t p`) entry points that
open a project's group or a worktree's tab; `modules/sidebar.el` renders the
grouped project/worktree tree from it. This replaced an earlier
frame-per-repo model (`frames.el`, since deleted) outright.

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
status back into Emacs and make no permission decision. The first four each
carry *two* commands -- the `emacs-status.sh` call and a pre-existing
`workmux set-window-status` call, which is still in use for tmux-hosted
sessions -- and both are observational, so neither affects the prompt set.

That parity is checked, not just argued:
`modules/claude-term-approval-parity-live-test.el` runs the real `claude`
binary's `auto-mode config` (the resolved allow / soft_deny / hard_deny
policy) through a login shell and through the real `claude-term` spawn
context and asserts the two are byte-identical, plus asserts the spawn argv
carries no permission-affecting flag and the spawn environment contributes
no `CLAUDE*`/`ANTHROPIC*` variable.

The Emacs-side approval layer this replaced (`modules/claude-repl/`, an
approval socket served from Emacs plus a `PreToolUse` hook script) is
archived at the annotated tag `archive/claude-repl`, not lost: `git show
archive/claude-repl` for the removal and its rationale, `git show
archive/claude-repl^:modules/claude-repl/<file>` for any file of it.

## Comments

Keep comments concise. A comment explains **why** only when the reason is not obvious from the code; it never narrates the thinking, history, or measurements that led to the change.

- **Don't restate the code.** `(setq foo t)` does not need `;; set foo to t`.
- **Explain the non-obvious.** A surprising ordering constraint, a workaround for an upstream bug, or a setting that looks wrong but isn't gets one or two lines saying why.
- **No design narratives.** Benchmark numbers, rejected alternatives, what the old code did, roadmap or phase references, and "verified in upstream source" notes belong in the commit message, not the source. A comment block longer than about four lines is a signal to cut.
- **No trailing-comment paragraphs.** A short end-of-line note is fine; wrapping a paragraph across trailing comments is not.

Good:

```elisp
;; Must be set before bootstrap.el loads: it reads this while checking straight's own repo.
(setq straight-check-for-modifications '(check-on-save))
```

Bad: the same setting preceded by a paragraph of benchmark timings, phase references, and a description of what the previous mechanism did.

## Best Practices

- **Test-driven development**: Add tests before or alongside code changes
- **Use existing test infrastructure**: Don't create ad-hoc test files
- **Follow the module pattern**: Keep related functionality in the appropriate module
- **Byte-compile clean**: Code should compile without warnings
- **Check startup**: A batch start with no errors is the minimum bar for any module change
- **Use TodoWrite actively**: Track progress on multi-step tasks to help users understand what's being done
