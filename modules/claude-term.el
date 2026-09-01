;;; claude-term.el --- Thin ghostel-based launcher for the Claude CLI -*- lexical-binding: t -*-

;;; Commentary:
;; Spawns `claude' at the current project's root in a ghostel-mode terminal
;; buffer, with a parseable buffer name and kill/restart commands.
;;
;; This is phase 1 of the edmacs-claude-terminal roadmap: spawn, naming, and
;; lifecycle only.  Side-window placement (phase 2), evil integration
;; (phase 3), and a real multi-session registry (phase 4) build on this
;; module without needing to change its naming regexp or
;; `claude-term--exec's signature.
;;
;; DECIDED 2026-08-31: build a thin launcher rather than adopt a package
;; such as claude-code-ide.el.  Adopting means keeping that package's
;; WebSocket IDE protocol (Channel A) whether wanted or not, including two
;; global `post-command-hook' entries for selection tracking with no off
;; switch.  Building means simply not implementing it.  Deliberately given
;; up as a result: automatic selection injection and diff routing into an
;; Emacs viewer.  Revisit only if their absence proves painful.
;;
;; Do NOT hand-roll here: terminal emulation (ghostel), evil motions
;; (evil-ghostel), or MCP protocol framing.

;;; Code:

(require 'cl-lib)
(require 'project)

(use-package ghostel
  :straight t
  :defer t)

;; evil-ghostel gives, without work, insert-state on entry, ESC to normal
;; with cursor snapping, hjkl/w/b/e motion, and d/c/r/p implemented through
;; the shell -- a separate 1,025-line package with its own test suite, not
;; reimplemented here.  `:hook (ghostel-mode . evil-ghostel-mode)' is the
;; package's own documented enable recipe (evil-ghostel.el's Commentary).
(use-package evil-ghostel
  :straight t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

;; inheritenv is a tiny, dependency-free utility with no startup cost of its
;; own (no hooks, no spawned process) -- unlike ghostel it is safe to load
;; eagerly, mirroring core.el's eager `compat'/`transient' loading.
;; `fboundp' guarded (rather than an unconditional call like core.el's own
;; `straight-use-package' calls) so this file also loads standalone under
;; `emacs -Q --batch' for claude-term-test.el, which has no straight
;; bootstrap; init.el always bootstraps straight before any `load-module'
;; call, so production startup is unaffected.
(when (fboundp 'straight-use-package)
  (straight-use-package 'inheritenv))
(require 'inheritenv nil t)

;; ghostel is loaded lazily via `claude-term--ensure-ghostel'; declare its
;; API surface here so the byte-compiler doesn't warn about references to
;; symbols that aren't bound until then.
(defvar ghostel--process)
(defvar ghostel-kill-buffer-on-exit)
(defvar ghostel-buffer-name-function)
(defvar ghostel-exit-functions)
(declare-function ghostel-exec "ghostel")

;; evil-ghostel is loaded lazily via its own `:hook' above; declare its API
;; surface here for the same byte-compiler reason as the ghostel symbols.
(defvar evil-ghostel-escape)
(defvar evil-ghostel-mode-map)
(declare-function ghostel-send-string "ghostel")
(declare-function ghostel-send-C-g "ghostel")
(declare-function evil-define-key* "evil-core")

;; ============================================================================
;; Customization
;; ============================================================================

(defgroup claude-term nil
  "Thin ghostel-based launcher for the Claude CLI."
  :group 'tools)

(defcustom claude-term-extra-args nil
  "Extra command-line arguments prepended to a `claude-term' call's own args.
`claude-term--spawn-args' merges this with a call's EXTRA-ARGS exactly
once, at spawn time; the resulting list is frozen into the buffer-local
`claude-term--args' and reused verbatim by `claude-term-restart', so
mutating this variable never changes an already-running session's argv --
only future fresh spawns.  Use this to inject e.g. a `--mcp-config'
argument scoping an MCP server to Emacs-launched sessions."
  :type '(repeat string)
  :group 'claude-term)

(defcustom claude-term-window-width 0.35
  "Fractional width of a claude-term side window, relative to the frame.
`window-sides-vertical' is nil by default, so left and right side
windows form ONE column stacked vertically and share a single width --
several agent panes are stacked top-to-bottom in that column, not laid
out side by side.  This value is therefore a per-column width shared by
every stacked pane, not a per-pane budget: size it for a single narrow
column (claude-code-ide.el's 100-column default is sized for a
side-by-side layout and is far too wide here)."
  :type 'number
  :group 'claude-term)

;; ============================================================================
;; Side-window layout
;; ============================================================================
;; `window-sides-slots' element order is LEFT TOP RIGHT BOTTOM.  nil for a
;; side means unbounded (a fresh slot always creates a new window); a
;; numeric cap means a request for a slot beyond the cap causes the most
;; suitable EXISTING side window on that edge to be reused with its slot
;; parameter changed, rather than a new window being created; 0 forbids
;; creation entirely and `display-buffer-in-side-window' returns nil
;; rather than erroring.  Right is capped at 3 (this phase's Done
;; criterion); the other three edges are left unbounded since nothing
;; else in this module uses them yet.
(setq window-sides-slots '(nil nil 3 nil))

;; ============================================================================
;; Lazy ghostel loading
;; ============================================================================

(defun claude-term--ensure-ghostel ()
  "Ensure the `ghostel' package is loaded.
Signals a clear error naming `ghostel' as a missing dependency if it
cannot be loaded.  Never called at module load time -- only from
`claude-term''s entry points, the first time any of them runs."
  (unless (featurep 'ghostel)
    (unless (require 'ghostel nil t)
      (error "Claude-term requires the `ghostel' package, which could not be loaded"))))

;; ============================================================================
;; Project root resolution
;; ============================================================================

(defun claude-term--project-root ()
  "Return the current project's root directory.
Signals a `user-error' when not inside a project."
  (let ((proj (project-current)))
    (unless proj
      (user-error "Claude-term: not inside a project"))
    (project-root proj)))

;; ============================================================================
;; Buffer-local session state
;; ============================================================================
;; Defined ahead of the buffer-naming helpers below since
;; `claude-term--resolve-instance' reads `claude-term--root' out of other
;; buffers to detect leaf collisions.

(defvar-local claude-term--root nil
  "Project root this claude-term buffer was spawned for.")

(defvar-local claude-term--instance nil
  "Instance label for this claude-term buffer, or nil.")

(defvar-local claude-term--args nil
  "Final, fully-resolved argv (after the program name) frozen at spawn time.
Computed once by `claude-term--spawn-args' at the original `claude-term'
call; reused verbatim on every subsequent `claude-term--exec' call for
this buffer, including restarts.")

(defvar-local claude-term--restarting nil
  "Non-nil while an async kill -> sentinel -> re-exec restart is in flight.")

(defvar-local claude-term--slot nil
  "Side-window slot assigned to this buffer, or nil until first displayed.
Sticking to one slot for the life of a buffer (via
`claude-term--allocate-slot') is what keeps a re-display of an already
live session (toggling back to it, restarting it) from grabbing a fresh
slot and, once `window-sides-slots' right cap of 3 is reached, silently
evicting an unrelated pane via Emacs's own slot-reuse behavior.")

(defvar claude-term--next-slot 0
  "Counter for the next unassigned side-window slot.
Monotonically increasing and never recycled -- a killed session's old
slot is not reclaimed by the next fresh buffer.  Intentional for this
phase: start with the simplest thing that works and revisit only if
instances from different projects start interleaving confusingly (see
the phase body's own guidance).  Also means a 4th distinct claude-term
buffer requests slot 3, beyond the right side's cap of 3 (slots 0-2) --
`display-buffer-in-side-window' then reuses the most suitable existing
side window rather than creating a new one, per stock Emacs behavior;
out of scope for this phase, which only requires three simultaneous
agents, but worth remembering once phase 4's session registry lands.")

;; ============================================================================
;; Buffer naming
;; ============================================================================
;; `*claude-term:<leaf>[:<instance>]*' -- mirrors claude-code.el's
;; `*claude:<dir>:<instance>*', but uses the bare project-root leaf
;; directory name rather than the full abbreviated truename: rdm worktree
;; slugs are already globally unique, so the leaf is a safe short label.

(defun claude-term--leaf (root)
  "Return the leaf directory name of project ROOT."
  (file-name-nondirectory (directory-file-name root)))

(defun claude-term-buffer-name (root &optional instance)
  "Return the claude-term buffer name for project ROOT and optional INSTANCE."
  (let ((leaf (claude-term--leaf root)))
    (if instance
        (format "*claude-term:%s:%s*" leaf instance)
      (format "*claude-term:%s*" leaf))))

(defconst claude-term--buffer-name-regexp
  "\\`\\*claude-term:\\([^:*]+\\)\\(?::\\([^*]+\\)\\)?\\*\\'"
  "Regexp matching a claude-term buffer name.
Group 1 is the project leaf; group 2, when present, is the instance
label.")

(defun claude-term--parse-buffer-name (name)
  "Parse claude-term buffer NAME into a (LEAF . INSTANCE) cons.
INSTANCE is nil when NAME has no instance slot.  Returns nil when NAME
does not match `claude-term--buffer-name-regexp'."
  (when (string-match claude-term--buffer-name-regexp name)
    (cons (match-string 1 name) (match-string 2 name))))

(defun claude-term--buffer-list ()
  "Return the list of live buffers whose name is a claude-term name.
There is no separate session registry in this phase (that is phase 4's
job); this scan of `buffer-list' filtered through
`claude-term--parse-buffer-name' IS the registry for this phase's
purposes -- both for enumerating sessions (`claude-term-kill',
`claude-term-restart') and for the leaf-collision check below."
  (seq-filter (lambda (buf) (claude-term--parse-buffer-name (buffer-name buf)))
              (buffer-list)))

(defun claude-term--resolve-instance (root instance)
  "Return the effective instance label to use for ROOT given requested INSTANCE.
When the candidate buffer name for ROOT/INSTANCE is unused, or already
belongs to this same ROOT (a switch-to-existing-session case handled by
the caller), INSTANCE is returned unchanged.  Otherwise the candidate
name belongs to an unrelated project that merely shares ROOT's leaf
directory name; in that case a free numeric instance slot is chosen
instead of silently reusing or colliding with the unrelated project's
buffer."
  (let ((existing (get-buffer (claude-term-buffer-name root instance))))
    (if (or (null existing)
            (equal (buffer-local-value 'claude-term--root existing) root))
        instance
      (let ((n 2))
        (while (let ((buf (get-buffer (claude-term-buffer-name root (number-to-string n)))))
                 (and buf (not (equal (buffer-local-value 'claude-term--root buf) root))))
          (setq n (1+ n)))
        (number-to-string n)))))

;; ============================================================================
;; Evil integration
;; ============================================================================
;; Two renderers, and only one matters here: fullscreen (alt-screen)
;; TUIs vs. the inline renderer this module is configured for (see
;; ~/Projects/dotfiles/claude/settings.json's "tui": "default", edited
;; alongside this phase but tracked in that separate repo). Inline keeps
;; the whole transcript as ordinary buffer text, traversable by isearch
;; and evil motions -- most of the reason to run the agent inside Emacs
;; at all -- so ESC should always reach evil here, never the terminal.
;;
;; `evil-ghostel-escape' is a global-by-default defcustom, but its value
;; is read fresh into the buffer-local `evil-ghostel--escape-mode' every
;; time `evil-ghostel-mode' turns on, so a buffer-local override set
;; beforehand is honored (confirmed by reading evil-ghostel.el's
;; mode-enable body). It is set here, from a `ghostel-mode-hook'
;; function, rather than from `claude-term--exec's own pre-`ghostel-exec'
;; setq block: `ghostel-mode' derives from `fundamental-mode', and its
;; first-ever invocation on a buffer -- true on every buffer's first
;; `ghostel-exec' call -- runs `kill-all-local-variables', which wipes
;; any buffer-local value set before that point. A `ghostel-mode-hook'
;; function runs after that wipe, so its `setq-local' survives it.

(defun claude-term--configure-evil-escape ()
  "Route insert-state ESC to evil, buffer-locally, in claude-term buffers.
Added to `ghostel-mode-hook' at an early depth so it runs before
`evil-ghostel-mode' (hooked onto `ghostel-mode-hook' at the default
depth by this module's `use-package evil-ghostel' form) reads
`evil-ghostel-escape' into its own buffer-local state. Scoped by buffer
NAME rather than `claude-term--root': name is intrinsic to the buffer
and survives `kill-all-local-variables' (this hook itself runs after
that wipe on a first spawn), whereas the buffer-local
`claude-term--root' does not survive it."
  (when (claude-term--parse-buffer-name (buffer-name))
    (setq-local evil-ghostel-escape 'evil)))

;; Negative DEPTH (Emacs 30+; this repo runs 31.1) guarantees this runs
;; before `evil-ghostel-mode's own `ghostel-mode-hook' entry regardless
;; of file/require load order.
(add-hook 'ghostel-mode-hook #'claude-term--configure-evil-escape -90)

;; PREREQUISITE: the Claude CLI's own vi-mode must be off, or its ESC
;; handling fights evil-ghostel's (the "triple-ESC" symptom in
;; manzaltu/claude-code-ide.el#52). `/config' inside a `claude-term'
;; session, setting "Editor mode" to "normal", is the interactive path
;; -- but this is NOT purely unverifiable the way an earlier pass of
;; this phase believed. Reading the installed CLI binary's own bundled
;; source directly (`strings -a "$(which claude)"' against the Bun
;; build under `~/.local/share/claude/versions/', 2.1.257, 2026-09-01)
;; shows `/config''s "Editor mode" IS backed by a real settings key,
;; `editorMode', with schema enum `["normal" "vim"]' and a DEFAULT of
;; `"normal"' baked into the binary itself; the CLI's own settings
;; resolver falls back to that default whenever no settings tier sets
;; the key. So the prerequisite holds by construction as long as no
;; tier introduces an explicit `"editorMode": "vim"' override, which is
;; exactly what `claude-term-live-test-claude-cli-editor-mode-not-vim'
;; (modules/claude-term-live-test.el) checks, across every tier the CLI
;; reads: `~/.claude.json' (legacy global), `~/.claude/settings.json'
;; and `~/.claude/settings.local.json' (user), and this project's own
;; `.claude/settings.json' / `.claude/settings.local.json'. As of
;; 2026-09-01 every one of those files was grepped by hand and confirmed
;; free of an `editorMode' key -- so the default governs, and vi-mode is
;; off. If ESC ever needs more than one press from insert state to
;; reach a stable evil normal state in a `claude-term' buffer, that is
;; the symptom of this having reverted (a settings-file edit, or a CLI
;; update changing the baked-in default) -- the live test above is a
;; regression guard against the former; re-running `/config' remains
;; the fix either way. See rdm task claude-cli-editor-mode-durability,
;; which still tracks the residual gap this test does not close: the
;; CLI's default could change in a future version with nothing here to
;; catch it before it ships.

(defun claude-term-send-escape ()
  "Send a raw ESC to the terminal, interrupting a running Claude response.
With `evil-ghostel-escape' routed to evil (see
`claude-term--configure-evil-escape'), insert-state ESC no longer
reaches the terminal, so Claude Code's own interrupt key needs a
separate binding -- this one line mirrors claude-code.el's C-g handler
\(claude-code.el:1087\).

Scoped to claude-term buffers only, by the same
`claude-term--parse-buffer-name' check `claude-term--configure-evil-escape'
uses: this is bound on the shared, package-global `evil-ghostel-mode-map',
which every `evil-ghostel-mode' buffer uses regardless of what program is
running in it, not just claude-term sessions. In any other ghostel
buffer, fall through to ghostel's own documented C-g behavior
\(`ghostel-send-C-g', which sends a raw C-g/BEL byte and clears Emacs's
`quit-flag'\) rather than silently substituting ESC for programs that
distinguish the two."
  (interactive)
  (if (claude-term--parse-buffer-name (buffer-name))
      (ghostel-send-string "\e")
    (ghostel-send-C-g)))

;; Bound to C-g in insert state, once evil-ghostel is loaded. This
;; deliberately shadows ghostel's default passthrough of a raw C-g to
;; the terminal IN CLAUDE-TERM BUFFERS ONLY (see
;; `claude-term-send-escape's own buffer-name guard) -- Claude Code's
;; own default C-g binding is "edit prompt in external editor", an
;; accepted trade-off per the phase body, remediable via
;; ~/.claude/keybindings.json if it proves painful. The keymap itself is
;; package-global (every `evil-ghostel-mode' buffer shares
;; `evil-ghostel-mode-map'), so the buffer-name guard inside
;; `claude-term-send-escape' -- not this binding -- is what keeps the
;; override from leaking into unrelated `M-x ghostel' terminals.
(with-eval-after-load 'evil-ghostel
  (evil-define-key* 'insert evil-ghostel-mode-map
    (kbd "C-g") #'claude-term-send-escape))

;; "No binding requires conscious thought" (the phase's Done-when
;; clause) is inherently a subjective UX judgment -- not settleable from
;; repo state, and a prior review correctly said so. What IS checkable
;; from here is the actual conflict surface this module adds on top of
;; evil-ghostel's own (separately tested) defaults, since surprise is
;; overwhelmingly a symptom of two things fighting over the same key.
;; Audit, as of this phase: this module customizes exactly two bindings,
;; both scoped to INSERT state only --
;;   1. `evil-ghostel-escape' -> `'evil (`claude-term--configure-evil-escape',
;;      above) -- not a keybinding at all, a mode switch read once on
;;      `ghostel-mode' enable; nothing to collide with.
;;   2. `C-g' -> `claude-term-send-escape' (immediately above) -- shadows
;;      Claude Code's own default C-g ("edit prompt in external
;;      editor"), a documented, accepted trade-off, not an accident.
;; Neither touches evil's NORMAL-state vocabulary (hjkl/w/b/e/d/c/r/p,
;; all evil-ghostel's own, already covered by its test suite) -- this
;; module adds no normal-state bindings of its own, so there is no
;; conflict surface there to audit. That leaves exactly the two
;; insert-state customizations above as the only places "conscious
;; thought" could plausibly be required, and both are one press, one
;; mnemonic (ESC for escape, C-g for the CLI's own historical interrupt
;; key), already exercised by AC1/AC2's automated coverage
;; (`claude-term-test-send-escape-sends-raw-escape-in-claude-term-buffer',
;; `claude-term-live-test-real-evil-ghostel-escape-dispatches-to-evil').
;; What this audit cannot do is the final subjective call -- whether
;; those two, correctly-firing bindings SUBJECTIVELY feel automatic in a
;; real session. That is a human-dogfooding step, not a batch one.

;; ============================================================================
;; Spawn argument resolution
;; ============================================================================

(defun claude-term--spawn-args (call-args)
  "Return the final argv: `claude-term-extra-args' followed by CALL-ARGS.
Called exactly once per session, at the `claude-term' entry point --
never by `claude-term--exec' and never on restart -- so mutating
`claude-term-extra-args' later affects only future fresh spawns, not an
already-running session's restart."
  (append claude-term-extra-args call-args))

;; ============================================================================
;; Spawn / exit lifecycle
;; ============================================================================

(defun claude-term--exec (buffer root instance args)
  "Spawn `claude' in BUFFER at ROOT with INSTANCE label and final ARGS.
ARGS is treated as opaque and final: the fully-resolved argv computed
once by `claude-term--spawn-args' at the original `claude-term' call.
This function never recomputes it, including on restart."
  (claude-term--ensure-ghostel)
  (with-current-buffer buffer
    ;; `get-buffer-create' otherwise inherits `default-directory' from
    ;; whatever buffer was current at creation time, not ROOT -- and
    ;; `ghostel-exec' takes cwd from the buffer's `default-directory'.
    (setq default-directory root)
    (setq claude-term--root root
          claude-term--instance instance
          claude-term--args args)
    ;; Exit cleanup is routed entirely through `claude-term--on-exit'
    ;; instead of ghostel's own default kill-on-exit, so there is exactly
    ;; one place that decides what happens to a dead buffer.
    (setq-local ghostel-kill-buffer-on-exit nil)
    ;; Left at its default (nil): OSC title escapes emitted by the
    ;; `claude' CLI/shell would otherwise rename the buffer out from
    ;; under the parseable naming scheme above.
    (setq-local ghostel-buffer-name-function nil)
    ;; A named function (not a closure) so `add-hook' is idempotent
    ;; across repeated exec calls on the same buffer (a restart) --
    ;; this never accumulates duplicate hook entries.
    (add-hook 'ghostel-exit-functions #'claude-term--on-exit nil t)
    ;; `ghostel-exec' hard-passes `extra-env' nil to `ghostel--spawn-pty',
    ;; so environment customization must go through `inheritenv' (or
    ;; `ghostel-pre-spawn-hook'), not a `let'-bound `process-environment'
    ;; around the call.
    (inheritenv (ghostel-exec buffer "claude" args))))

(defun claude-term--deferred-reexec (buf root instance args)
  "Re-exec `claude' in BUF once ghostel's own sentinel has finished.
Scheduled via `run-at-time' from `claude-term--on-exit' rather than
called directly from there -- see that function's docstring for why.
Re-checks `buffer-live-p' since BUF could have been killed by the user
in the interval between scheduling and firing."
  (when (buffer-live-p buf)
    (claude-term--exec buf root instance args)))

(defun claude-term--on-exit (buf _event)
  "Handle the `claude' process in BUF exiting.
Added to the buffer-local `ghostel-exit-functions', which ghostel's own
`ghostel--sentinel' invokes with unconditional cleanup of its own still
to run afterward in the SAME synchronous sentinel call: once this hook
returns, `ghostel--sentinel' re-checks `buffer-live-p' and, when
`ghostel-kill-buffer-on-exit' is nil (as this module always sets it),
unconditionally stamps a \"[Process exited]\" banner into the buffer via
a raw `insert' -- regardless of whether this hook already attached a
brand-new live process to it.  Re-execing synchronously here would
therefore corrupt the just-restarted terminal with that stray banner.

On a plain exit or an explicit kill (`claude-term--restarting' nil),
tears the buffer down -- `ghostel-kill-buffer-on-exit' is nil precisely
so this is the single point of truth for that, and a killed buffer is
definitionally absent from `claude-term--buffer-list', satisfying \"no
phantom\" without a separate registry this phase.  On a restart in
flight, defers the re-exec via `run-at-time' (0-second delay) so it
runs on a later event-loop iteration, after `ghostel--sentinel's tail
code for THIS exit has already finished touching the (still-dead-at-
that-point) buffer; the deferred `claude-term--exec' call then erases
the buffer via `ghostel-exec' -> `ghostel--init-buffer' as part of
spawning the new process, discarding the stray banner along with
everything else from the dead session."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (if claude-term--restarting
          (let ((root claude-term--root)
                (instance claude-term--instance)
                (args claude-term--args))
            (setq claude-term--restarting nil)
            (run-at-time 0 nil #'claude-term--deferred-reexec buf root instance args))
        (kill-buffer buf)))))

;; ============================================================================
;; Side-window display
;; ============================================================================
;; This looks like it should be a two-line `display-buffer-in-side-window'
;; call and is not.  `display-buffer-in-side-window' only installs the
;; `window-side' and `window-slot' parameters -- dedication follows from
;; that automatically, but exclusion from `other-window' and
;; `windmove'/evil-window-* does NOT: it depends solely on the
;; `no-other-window' window parameter, which neither
;; `display-buffer-in-side-window' nor any of its callers sets on its
;; own (verified live via `emacs -Q --batch': without it,
;; `window-no-other-p' returns nil, `(other-window 1)' selects the side
;; window, and so does `window-in-direction').  `no-other-window' below
;; is therefore the actual new code this phase is about; everything else
;; in the window-parameters/action alist is stock `display-buffer'
;; plumbing.
;;
;; DECIDED: `windmove-allow-repeated-command-override' is left at its
;; Emacs 31 default (t), so a REPEATED `SPC w l' (or any windmove
;; direction key) deliberately does enter an agent pane -- an
;; intentional escape hatch, not an oversight this module should close.
;; A single invocation still respects `no-other-window' via
;; `window-no-other-p'.

(defun claude-term--allocate-slot (buffer)
  "Return the side-window slot assigned to BUFFER, assigning one if needed.
Reuses BUFFER's existing `claude-term--slot' when already set, so
repeated display calls for the same live session -- a toggle, a
restart -- keep the same slot rather than drawing a fresh one from
`claude-term--next-slot' and potentially triggering the right side's
slot-reuse eviction once its cap of 3 is reached."
  (with-current-buffer buffer
    (or claude-term--slot
        (setq claude-term--slot
              (prog1 claude-term--next-slot
                (cl-incf claude-term--next-slot))))))

(defun claude-term--display-buffer (buffer)
  "Display BUFFER in a stacked right-side window and return that window.
Builds this phase's verified target shape: a right side window sized by
`claude-term-window-width', sized-preserving across a
`window-toggle-side-windows' hide/show cycle, excluded from
`delete-other-windows' and from `other-window'/single-invocation
`windmove' via `no-other-window' (see the section comment above).
Returns nil exactly when `display-buffer-in-side-window' does -- e.g.
when `window-sides-slots' forbids side-window creation on this edge --
never signals in that case."
  (display-buffer
   buffer
   `((display-buffer-in-side-window)
     (side . right)
     (slot . ,(claude-term--allocate-slot buffer))
     (window-width . ,claude-term-window-width)
     (preserve-size . (t . nil))
     (window-parameters . ((no-delete-other-windows . t)
                            (no-other-window . t))))))

(defun claude-term--pop-to-side-window (buffer)
  "Display BUFFER in a side window via `claude-term--display-buffer' and select it.
Preserves `pop-to-buffer''s \"display and select\" semantics at this
module's two call sites.  Guards against a nil return (side-window
creation can be forbidden by `window-sides-slots', which then makes
`display-buffer-in-side-window' return nil rather than error) by simply
not selecting anything in that case."
  (let ((window (claude-term--display-buffer buffer)))
    (when window
      (select-window window))
    window))

;; ============================================================================
;; Buffer selection for kill/restart
;; ============================================================================

(defun claude-term--read-buffer (prompt)
  "Return a claude-term buffer, using PROMPT if a choice is needed.
Returns the current buffer when called from inside a claude-term
buffer; otherwise PROMPTs via `completing-read' over existing
claude-term buffers."
  (if (claude-term--parse-buffer-name (buffer-name))
      (current-buffer)
    (let ((candidates (mapcar #'buffer-name (claude-term--buffer-list))))
      (when (null candidates)
        (user-error "Claude-term: no claude-term buffers"))
      (get-buffer (completing-read prompt candidates nil t)))))

;; ============================================================================
;; Entry points
;; ============================================================================

;;;###autoload
(defun claude-term (&optional instance extra-args)
  "Spawn or switch to a `claude' session for the current project.
INSTANCE, when non-nil, is an extra label included in the buffer name,
letting multiple sessions coexist for the same project; a second call
with the same INSTANCE for the same project switches to the existing
live session rather than erroring or respawning.
EXTRA-ARGS, when non-nil, is a list of extra command-line arguments for
this call only, prepended by `claude-term-extra-args' via
`claude-term--spawn-args' -- the single call site for that function in
this module."
  (interactive)
  (claude-term--ensure-ghostel)
  (let* ((root (claude-term--project-root))
         (args (claude-term--spawn-args extra-args))
         (instance (claude-term--resolve-instance root instance))
         (name (claude-term-buffer-name root instance))
         (buffer (get-buffer-create name)))
    (if (with-current-buffer buffer (process-live-p ghostel--process))
        (claude-term--pop-to-side-window buffer)
      (claude-term--exec buffer root instance args)
      (claude-term--pop-to-side-window buffer))))

;;;###autoload
(defun claude-term-kill (&optional buffer)
  "Kill the `claude' process in BUFFER.
BUFFER defaults to the current buffer when called from inside a
claude-term buffer, else it is prompted for.  Buffer teardown happens
asynchronously once the process sentinel fires -- see
`claude-term--on-exit' -- so this never also calls `kill-buffer'
itself, which would race the sentinel."
  (interactive)
  (let ((buffer (or buffer (claude-term--read-buffer "Kill claude-term session: "))))
    (with-current-buffer buffer
      (if (process-live-p ghostel--process)
          (kill-process ghostel--process)
        (message "Claude-term: %s has no live process" (buffer-name buffer))))))

;;;###autoload
(defun claude-term-restart (&optional buffer)
  "Restart the `claude' session in BUFFER.
BUFFER defaults to the current buffer when called from inside a
claude-term buffer, else it is prompted for.
When the process is live, performs the required async
kill -> sentinel -> re-exec dance (`claude-term--on-exit' does the
re-exec once the sentinel fires) -- `ghostel-exec' signals a
`user-error' if called against a buffer whose process is still even
nominally live, and `kill-process' does not synchronously flip the
process to a dead state a second `ghostel-exec' call could observe.
When the process is already dead, respawns synchronously.  A restart
already in flight for BUFFER is a no-op."
  (interactive)
  (let ((buffer (or buffer (claude-term--read-buffer "Restart claude-term session: "))))
    (with-current-buffer buffer
      (cond
       (claude-term--restarting
        (message "Claude-term: restart already in progress for %s" (buffer-name buffer)))
       ((process-live-p ghostel--process)
        (setq claude-term--restarting t)
        (kill-process ghostel--process))
       (t
        (claude-term--exec buffer claude-term--root claude-term--instance claude-term--args))))))

(provide 'claude-term)
;;; claude-term.el ends here
