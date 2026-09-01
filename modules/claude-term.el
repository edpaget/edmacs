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

(require 'project)

(use-package ghostel
  :straight t
  :defer t)

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
        (pop-to-buffer buffer)
      (claude-term--exec buffer root instance args)
      (pop-to-buffer buffer))))

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
