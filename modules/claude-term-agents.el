;;; claude-term-agents.el --- claude-term adapter for the shared agent table -*- lexical-binding: t -*-

;;; Commentary:
;; Phase 9 of the edmacs-sidebar roadmap: mirrors claude-term-registry.el's
;; create/remove/rename events into the shared `edmacs-agents--table'
;; (agents.el, edmacs-sidebar phase 5) as source `claude-term' rows, so a
;; ghostel-hosted session shows up in the sidebar's ALL AGENTS list and
;; `SPC a TAB' attention cycling exactly like a workmux-sourced tmux pane.
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
;; Loaded last in init.el, after `agents.el' and `(edmacs-agents-init)':
;; every call this file makes into `claude-term-registry.el' or
;; `agents.el' happens inside a hook-callback body, resolved at
;; hook-fire time, never at this file's own load time -- but placing it
;; last keeps the dependency obvious.
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
(defvar edmacs-agents--table)

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

(defun claude-term-agents--on-create (root instance buffer)
  "Add/refresh a `claude-term'-sourced row for ROOT/INSTANCE/BUFFER.
Fires on every `claude-term-registry-put' call, including a restart's
re-exec of an already-registered session -- `edmacs-agents--upsert' is
a plain `puthash' on the identical key in that case, a harmless
overwrite rather than a duplicate row. STATUS starts `idle': a freshly
spawned session has had no prompt sent to it yet. TITLE falls back to
`claude-term-registry--default-instance-label' when INSTANCE is nil
\(the default, non-multi-instance session\) -- matching the registry's
own picker/rename/list-mode display convention -- rather than storing
a bare nil title: `claude-term-agents--strip-progress-suffix' calls
`string-match' on it, which signals on nil, and other title-consuming
sites would render the literal string \"nil\". The KEY/INSTANCE fields
themselves stay nil, exactly like the registry's own session record,
since key equality via `equal' treats nil as an ordinary key component."
  (let ((truename-root (condition-case nil (file-truename root) (error root)))
        (now (float-time)))
    (edmacs-agents--upsert
     (make-edmacs-agent
      :key (edmacs-agents--key root instance)
      :root truename-root
      :instance instance
      :status 'idle
      :status-ts now
      :updated-ts now
      :title (or instance claude-term-registry--default-instance-label)
      :source 'claude-term
      :locator buffer
      :unread nil))))

(defun claude-term-agents--on-remove (root instance)
  "Remove ROOT/INSTANCE's row from the shared agent table, if any.
A no-op when no such row exists -- `edmacs-agents--remove' is already
idempotent on a missing key, e.g. a session killed while a status
update against the same row is in flight."
  (edmacs-agents--remove (edmacs-agents--key root instance)))

(defun claude-term-agents--on-rename (root old-instance new-instance)
  "Move ROOT/OLD-INSTANCE's row to ROOT/NEW-INSTANCE after a claude-term rename.
Fires on `claude-term-registry-rename-functions', the registry's third
lifecycle-mutating entry point: `claude-term-registry-rename' moves a
session between two keys by direct `remhash'/`puthash', so neither
`claude-term-registry-create-functions' nor
`claude-term-registry-remove-functions' fires for it -- without this
listener, the mirrored row would stay keyed under OLD-INSTANCE forever,
pointing sidebar actions at a registry key the rename already vacated.
Re-keys the EXISTING row in place (preserving its STATUS/UNREAD/etc, the
same way the registry's own session survives the rename) rather than
discarding and recreating it -- a rename mid-`working' should not
silently reset the row to `idle'. A no-op if no row was registered
under OLD-INSTANCE (e.g. a rename racing ahead of this file's own
create listener)."
  (let* ((truename-root (condition-case nil (file-truename root) (error root)))
         (old-key (edmacs-agents--key truename-root old-instance))
         (row (gethash old-key edmacs-agents--table)))
    (when row
      (edmacs-agents--remove old-key)
      (setf (edmacs-agent-instance row) new-instance
            (edmacs-agent-key row) (edmacs-agents--key truename-root new-instance)
            (edmacs-agent-title row)
            (or new-instance claude-term-registry--default-instance-label))
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
`put'). INSTANCE may legitimately be nil (the default, non-multi-instance
session), so it is bound with a plain `let', not folded into the
`when-let*' chain -- `when-let*' would otherwise treat that nil the
same as a genuinely absent binding and silently drop this whole
feature for the common single-session case. Only a `set' report against
a row currently `working' renders a suffix; every other STATE (or a
non-`working' row) strips any existing suffix back to the bare title
instead. Pure hash lookups and one `setf' -- no redraw, no subprocess --
since this runs synchronously on ghostel's VT-parser callpath."
  (when-let* ((root claude-term--root))
    (let* ((instance claude-term--instance)
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

(provide 'claude-term-agents)
;;; claude-term-agents.el ends here
