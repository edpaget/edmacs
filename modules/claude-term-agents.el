;;; claude-term-agents.el --- claude-term adapter for the shared agent table -*- lexical-binding: t -*-

;;; Commentary:
;; Phase 9 of the edmacs-sidebar roadmap: mirrors claude-term-registry.el's
;; create/remove/rename events into the shared `edmacs-agents--table'
;; (agents.el, edmacs-sidebar phase 5) as source `claude-term' rows, so a
;; ghostel-hosted session shows up in the sidebar's ALL AGENTS list and
;; participates in `SPC a TAB' attention cycling.
;;
;; Wires into three swappable extension points added to
;; claude-term-registry.el for this purpose:
;; `claude-term-registry-create-functions' (fired at the end of
;; `claude-term-registry-put', with ROOT INSTANCE BUFFER),
;; `claude-term-registry-remove-functions' (fired at the end of
;; `claude-term-registry-remove', with ROOT INSTANCE), and
;; `claude-term-registry-rename-functions' (fired at the end of
;; `claude-term-registry-rename', with ROOT OLD-INSTANCE NEW-INSTANCE --
;; that function moves a session between two keys by direct
;; `remhash'/`puthash', so neither of the other two hooks fires for it) --
;; mirroring the same swappable-seam convention as that file's own
;; `claude-term-registry-state-accessor' and `claude-term-registry-sort-function'.
;;
;; The key SHAPE matches agents.el's own: `(edmacs-agents--key root
;; instance)' truename-normalizes ROOT exactly as
;; `claude-term-registry--key' does, so this adapter's rows and the
;; registry's own sessions agree on identity without either table
;; depending on the other's internals. `claude-term-session's own ROOT
;; field is documented as NOT truename-normalized, so this file
;; truenames it again for the `edmacs-agent' struct's ROOT field, same
;; as `edmacs-agents--key' does internally for the hash key.
;;
;; Removal is deliberately double-covered, not duplicated logic: this
;; file's own `claude-term-registry-remove-functions' listener is the
;; primary path for an in-Emacs session ending, while
;; `edmacs-agents-set-status's new `remove' status case (added by this
;; same phase) is what makes the ported claude-terminal SessionEnd hook
;; behave correctly for the same row from the other, hook-driven
;; direction -- see that function's docstring.
;;
;; Also composes onto `ghostel-progress-function' (ghostel.el) to fold a
;; live OSC 9;4 progress percentage into the matching row's title while
;; it is `working' -- this specific piece of the original claude-terminal
;; phase 5 body moved here per the `claude-terminal-phase5-split' task's
;; resolution. Installed via `with-eval-after-load' (mirroring
;; claude-term.el's own `claude-term-apply-palette' hookup) since ghostel
;; is loaded lazily, not at this file's load time; chains to whatever
;; handler was already installed (e.g. `ghostel-spinner-progress') rather
;; than clobbering it, and is a no-op for a non-claude-term ghostel
;; buffer or one with no row registered yet.
;;
;; No claude-repl adapter: claude-repl.el is not present anywhere on
;; main or in this worktree -- only the archived `archive/claude-repl'
;; tag carries it -- so that sub-step of the phase body is omitted
;; outright rather than guarded behind a dead `fboundp' check.
;;
;; Loaded last in init.el, after `agents.el': every call this file makes
;; into `claude-term-registry.el' or `agents.el' happens inside a
;; hook-callback body, resolved at hook-fire time, never at this file's
;; own load time -- but placing it last keeps the dependency obvious.
;;
;; Also owns the mode-line half of this adapter (the edmacs-modeline
;; roadmap phase that moved it here): a claude-term buffer's own status
;; glyph, spliced into `nano-modeline-footer' only for `ghostel-mode'
;; buffers. agents.el used to splice a cross-project roll-up into EVERY
;; buffer's mode line; that is gone now that the sidebar header carries
;; a project-scoped roll-up instead, and this section replaces it with a
;; strictly narrower, per-buffer one. The buffer<->row lookup reuses the
;; exact `claude-term--root'/`claude-term-agents--normalize-instance'
;; pattern `claude-term-agents--update-progress-title' above already
;; uses, rather than re-deriving it.
;;
;; Run pure-function tests:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/claude-term.el \
;;         -l modules/claude-term-registry.el \
;;         -l modules/agents.el \
;;         -l modules/claude-term-agents.el \
;;         -l modules/claude-term-agents-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function edmacs-agents--key "agents")
(declare-function edmacs-agents--upsert "agents")
(declare-function edmacs-agents--remove "agents")
(declare-function make-edmacs-agent "agents")
(declare-function edmacs-agent-title "agents")
(declare-function edmacs-agent-status "agents")
(declare-function edmacs-agent-instance "agents")
(declare-function edmacs-agent-key "agents")
(declare-function edmacs-agent-unread "agents")
(defvar edmacs-agents--table)

;; Dynamically bound by nano-modeline around every render; read (never
;; set) by `claude-term-agents-mode-line-segment' so its output matches
;; the line -- mirrors agents.el's former declaration of the same var
;; for the same reason.
(defvar nano-modeline-base-face)

(defvar claude-term-registry-create-functions)
(defvar claude-term-registry-remove-functions)
(defvar claude-term-registry-rename-functions)
(defvar claude-term-registry--default-instance-label)

;; Buffer-local session state, defined in claude-term.el; this file
;; loads strictly after it in the real init order (see init.el), but a
;; bare `defvar' keeps standalone byte-compilation of this file quiet,
;; matching claude-term-registry.el's own `(defvar claude-term--root)'.
(defvar claude-term--root)
(defvar claude-term--instance)

;; ghostel.el is loaded lazily (see claude-term.el's
;; `claude-term--ensure-ghostel'), not required at this file's load
;; time; the bare `defvar' below is byte-compile hygiene only, matching
;; the pattern above.
(defvar ghostel-progress-function)

;; ============================================================================
;; Row lifecycle: mirror registry create/remove into the shared table
;; ============================================================================

(defun claude-term-agents--normalize-instance (instance)
  "Return INSTANCE, or `claude-term-registry--default-instance-label' if nil.
`claude-term-registry-put'/`-remove'/`-rename' pass a session's raw
INSTANCE through unchanged, including nil for the common
non-multi-instance case. But `claude-term-registry--set-instance-env'
never leaves `EDMACS_AGENT_INSTANCE' unset in the child process -- it
always resolves that same nil to this literal label before the session
ever starts -- so the ported claude-terminal status hook this row
exists to serve always calls `edmacs-agents-set-status' with that
non-nil string, never with nil. Every key this file computes from a
registry-side INSTANCE must be normalized the same way, or a
default-instance session's adapter-created row (keyed on nil) and the
hook's status updates (keyed on \"default\") permanently address two
different rows in `edmacs-agents--table'."
  (or instance claude-term-registry--default-instance-label))

(defun claude-term-agents--on-create (root instance buffer)
  "Add/refresh a `claude-term'-sourced row for ROOT/INSTANCE/BUFFER.
Fires on every `claude-term-registry-put' call, including a restart's
re-exec of an already-registered session -- `edmacs-agents--upsert' is
a plain `puthash' on the identical key in that case, a harmless
overwrite rather than a duplicate row. STATUS starts `idle': a freshly
spawned session has had no prompt sent to it yet.

The row's KEY and TITLE are built from INSTANCE run through
`claude-term-agents--normalize-instance', so this row's key agrees with
what `edmacs-agents-set-status' resolves to once the ported status hook
fires for this same session (see that helper's docstring). The
INSTANCE field itself is stored UNNORMALIZED (raw, possibly nil): it is
also what `edmacs-sidebar-agents--claude-term-session' feeds straight
into `claude-term-registry-get' to resolve RET/rename/kill back to the
live session, and that registry's own key
\(`claude-term-registry--key') is never normalized -- a session
launched with no INSTANCE is registered, and stays registered, under a
literal nil, never under the display label. Normalizing this field
too would fix the status-hook lookup but break every other lookup for
the same default-instance row."
  (let* ((truename-root (condition-case nil (file-truename root) (error root)))
         (resolved-instance (claude-term-agents--normalize-instance instance))
         (now (float-time)))
    (edmacs-agents--upsert
     (make-edmacs-agent
      :key (edmacs-agents--key root resolved-instance)
      :root truename-root
      :instance instance
      :status 'idle
      :status-ts now
      :updated-ts now
      :title resolved-instance
      :source 'claude-term
      :locator buffer
      :unread nil))))

(defun claude-term-agents--on-remove (root instance)
  "Remove ROOT/INSTANCE's row from the shared agent table, if any.
INSTANCE is normalized exactly as `claude-term-agents--on-create'
normalizes it for the KEY (never for the stored INSTANCE field, which
this function does not touch), so a remove for a default-instance
session finds the row create actually stored under. A no-op when no
such row exists -- `edmacs-agents--remove' is already idempotent on a
missing key, e.g. a session killed while a status update against the
same row is in flight."
  (edmacs-agents--remove
   (edmacs-agents--key root (claude-term-agents--normalize-instance instance))))

(defun claude-term-agents--on-rename (root old-instance new-instance)
  "Move ROOT/OLD-INSTANCE's row to ROOT/NEW-INSTANCE after a claude-term rename.
Fires on `claude-term-registry-rename-functions', the registry's third
lifecycle-mutating entry point: `claude-term-registry-rename' moves a
session between two keys by direct `remhash'/`puthash', so neither
`claude-term-registry-create-functions' nor
`claude-term-registry-remove-functions' fires for it -- without this
listener, the mirrored row would stay keyed under OLD-INSTANCE forever,
pointing sidebar actions at a registry key the rename already vacated.
OLD-INSTANCE/NEW-INSTANCE are each normalized for the KEY lookup/rebuild
exactly as `claude-term-agents--on-create' normalizes for KEY (see that
function's docstring for why the row's stored INSTANCE field itself
stays raw/unnormalized, mirroring `claude-term-registry-rename' setting
the registry's own session record's INSTANCE to the same raw
NEW-INSTANCE it was called with). Re-keys the EXISTING row in place
(preserving its STATUS/UNREAD/etc, the same way the registry's own
session survives the rename) rather than discarding and recreating it
-- a rename mid-`working' should not silently reset the row to `idle'.
A no-op if no row was registered under OLD-INSTANCE (e.g. a rename
racing ahead of this file's own create listener)."
  (let* ((truename-root (condition-case nil (file-truename root) (error root)))
         (resolved-old (claude-term-agents--normalize-instance old-instance))
         (resolved-new (claude-term-agents--normalize-instance new-instance))
         (old-key (edmacs-agents--key truename-root resolved-old))
         (row (gethash old-key edmacs-agents--table)))
    (when row
      (edmacs-agents--remove old-key)
      (setf (edmacs-agent-instance row) new-instance
            (edmacs-agent-key row) (edmacs-agents--key truename-root resolved-new)
            (edmacs-agent-title row) resolved-new)
      (edmacs-agents--upsert row))))

(add-hook 'claude-term-registry-create-functions #'claude-term-agents--on-create)
(add-hook 'claude-term-registry-remove-functions #'claude-term-agents--on-remove)
(add-hook 'claude-term-registry-rename-functions #'claude-term-agents--on-rename)

;; ============================================================================
;; Ghostel OSC 9;4 progress -> row title suffix
;; ============================================================================

(defconst claude-term-agents--progress-suffix-regexp " [0-9]\\{1,3\\}%\\'"
  "Matches a previously-appended progress suffix on an agent row's title.
Stripped before formatting a new one, so repeated progress updates
replace the suffix instead of accumulating it.")

(defvar claude-term-agents--chained-progress-function nil
  "The `ghostel-progress-function' value in place when this file's wrapper
was installed, chained first so a pre-existing handler (e.g.
`ghostel-spinner-progress') keeps running unmodified.")

(defun claude-term-agents--strip-progress-suffix (title)
  "Return TITLE with any trailing progress-percentage suffix removed."
  (if (string-match claude-term-agents--progress-suffix-regexp title)
      (substring title 0 (match-beginning 0))
    title))

(defun claude-term-agents--update-progress-title (state progress)
  "Fold STATE/PROGRESS into `current-buffer''s agent row title, if any.
No-op for a buffer that is not a claude-term session -- `claude-term--root'
is buffer-local and nil for any other ghostel buffer, so this guards
itself without a separate buffer-name check -- or one with no row
registered yet (e.g. a progress report racing ahead of the registry
`put'). `claude-term--instance' may legitimately be nil (the default,
non-multi-instance session), so it is run through
`claude-term-agents--normalize-instance' before the lookup -- the row
was stored under that normalized key by `claude-term-agents--on-create',
not under a bare nil. Only a `set' report against a row currently
`working' renders a suffix; every other STATE (or a non-`working' row)
strips any existing suffix back to the bare title instead. Pure hash
lookups and one `setf' -- no redraw, no subprocess -- since this runs
synchronously on ghostel's VT-parser callpath."
  (when-let* ((root claude-term--root))
    (let* ((instance (claude-term-agents--normalize-instance claude-term--instance))
           (row (gethash (edmacs-agents--key root instance) edmacs-agents--table)))
      (when row
        (let ((base (claude-term-agents--strip-progress-suffix (edmacs-agent-title row))))
          (setf (edmacs-agent-title row)
                (if (and (eq state 'set)
                         (integerp progress)
                         (eq (edmacs-agent-status row) 'working))
                    (format "%s %d%%" base progress)
                  base)))))))

(defun claude-term-agents--progress-handler (state progress)
  "Chain to the previously-installed progress handler, then update the row.
Installed as `ghostel-progress-function'; see that variable's docstring
in ghostel.el for STATE/PROGRESS's shape. The chained call is wrapped
in its own `condition-case' so an error in a pre-existing handler (e.g.
`ghostel-spinner-progress') never prevents this file's own row update."
  (when claude-term-agents--chained-progress-function
    (condition-case err
        (funcall claude-term-agents--chained-progress-function state progress)
      (error (message "claude-term-agents: chained progress handler error: %s"
                       (error-message-string err)))))
  (claude-term-agents--update-progress-title state progress))

(defun claude-term-agents--install-progress-handler ()
  "Compose this file's progress handler onto `ghostel-progress-function'.
A no-op if already installed -- reloading this file (an interactive
`eval-buffer', or a repeated `with-eval-after-load' firing) must not
chain the wrapper onto itself."
  (unless (eq ghostel-progress-function #'claude-term-agents--progress-handler)
    (setq claude-term-agents--chained-progress-function ghostel-progress-function)
    (setq ghostel-progress-function #'claude-term-agents--progress-handler)))

(with-eval-after-load 'ghostel
  (claude-term-agents--install-progress-handler))

;; ============================================================================
;; Mode-line: this claude-term buffer's own status
;; ============================================================================

(defun claude-term-agents--mode-line-row ()
  "Return `current-buffer''s own row in `edmacs-agents--table', or nil.
No-op (returns nil) for a buffer with no `claude-term--root' at all --
any ghostel buffer that is not a claude-term session, including a
buffer that has not yet had `claude-term--exec' set that buffer-local
\(see this file's Commentary on the `ghostel-mode'-before-buffer-locals
ordering\). `claude-term--instance' is run through
`claude-term-agents--normalize-instance' before the lookup, exactly as
`claude-term-agents--update-progress-title' already does -- the row
for a default, non-multi-instance session is stored keyed under the
normalized label, not under a literal nil."
  (when-let* ((root claude-term--root))
    (gethash (edmacs-agents--key root (claude-term-agents--normalize-instance
                                        claude-term--instance))
              edmacs-agents--table)))

(defun claude-term-agents-mode-line-segment ()
  "Return this claude-term buffer's own status glyph, or \"\" when there
is nothing worth flagging. Nullary: the literal element `apply'd on
every mode-line render by nano-modeline's `:eval' construct.

`working' renders \"[⟳]\", `waiting' renders \"[💬]\", and `done' with
`edmacs-agent-unread' still set renders \"[✓]\" -- the same glyph
vocabulary the removed cross-project roll-up used, now for one row
instead of a count. Every other case -- no row at all (a plain ghostel
buffer, a session whose row was removed, one racing ahead of the
registry `put'), `idle', or an already-read `done' -- renders \"\".

Carries the mode line's own base face, for the same reason the removed
`edmacs-agents-mode-line-segment' did: nano-modeline applies its base
face only to the STRING elements of a line, so an unpropertized
function-element return value would render in the frame's `default'
colours instead of matching the rest of the line."
  (let* ((row (claude-term-agents--mode-line-row))
         (glyph (and row
                     (pcase (edmacs-agent-status row)
                       ('working "⟳")
                       ('waiting "💬")
                       ('done (and (edmacs-agent-unread row) "✓"))))))
    (if glyph
        (let ((s (format "[%s]" glyph)))
          (if (bound-and-true-p nano-modeline-base-face)
              (propertize s 'face nano-modeline-base-face)
            s))
      "")))

(defun claude-term-agents--nano-modeline-footer-filter-args (args)
  "Append this buffer's status segment to `nano-modeline-footer's RIGHT.
ARGS is (LEFT [RIGHT [DEFAULT]]); the two-argument shape is the
mainline one for every claude-term ghostel pane
(`edmacs-modeline-ghostel-mode'). Scoped to `ghostel-mode' buffers by
checking `derived-mode-p' in the buffer being baked, rather than
checking `claude-term--root' non-nil here: claude-term.el sets that
buffer-local strictly AFTER `ghostel-exec' has already turned on
`ghostel-mode' and run its hook (see claude-term.el's own comment on
that ordering), so it is still nil at bake time even for a genuine,
brand-new claude-term buffer -- gating on it here would mean the
element never gets baked in at all. The element itself is checked at
RENDER time instead (`claude-term-agents-mode-line-segment' above),
by which point the buffer-local is set. The element must be a list,
not a bare symbol: `nano-modeline--make' `apply's its car to its cdr.
RIGHT is a shared quoted literal inside nano-modeline, so it is
appended to, never mutated, and the `member' check keeps a re-bake
from doubling it."
  (if (not (derived-mode-p 'ghostel-mode))
      args
    (let ((element '(claude-term-agents-mode-line-segment))
          (right (nth 1 args)))
      (list (nth 0 args)
            (if (member element right) right (append right (list element)))
            (nth 2 args)))))

(defun claude-term-agents--install-mode-line-advice ()
  "Splice this buffer's status segment into every nano-modeline footer.
Targets `nano-modeline-footer' because ui.el binds
`nano-modeline-position' to it; a switch to `nano-modeline-header'
there would silently drop the segment. Not a bare `advice-add':
advising an undefined `nano-modeline-footer' succeeds and defines its
function cell, making `fboundp' lie, so the install stays inside
`with-eval-after-load'."
  (advice-add 'nano-modeline-footer :filter-args
              #'claude-term-agents--nano-modeline-footer-filter-args))

(with-eval-after-load 'nano-modeline
  (claude-term-agents--install-mode-line-advice))

(defun claude-term-agents--refresh-mode-line (&rest _keys)
  "Repaint the mode line so an unfocused claude-term buffer's status
glyph updates promptly on a status change. Wired onto
`edmacs-agents-changed-hook'; ignores the hook's KEYS argument -- the
segment reads `edmacs-agents--table' directly on each render, so there
is no cached aggregate to recompute here, only a repaint to force."
  (force-mode-line-update t))

(add-hook 'edmacs-agents-changed-hook #'claude-term-agents--refresh-mode-line)

(provide 'claude-term-agents)
;;; claude-term-agents.el ends here
