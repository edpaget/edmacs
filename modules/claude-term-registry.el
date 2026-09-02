;;; claude-term-registry.el --- Session registry and picker for claude-term -*- lexical-binding: t -*-

;;; Commentary:
;; Phase 4 of the edmacs-claude-terminal roadmap: a real session
;; registry on top of claude-term.el's phase 1-3 launcher, a
;; completing-read picker, and the SPC a command surface.
;;
;; Loaded immediately after claude-term.el by init.el, via plain `load'
;; -- no `require' of claude-term from this file is needed since both
;; share one obarray in this codebase's module system (see init.el's
;; `load-module'); the trailing `(provide 'claude-term-registry)' below
;; only mirrors claude-term.el's own end-of-file convention, in case a
;; future module wants to `require' this one explicitly. claude-term.el's
;; own call-site additions into this
;; file's functions (registry put/touch/remove, the kill picker) use
;; `declare-function' stubs there, since this file loads strictly
;; after it and the forward reference resolves fine at call time --
;; never at claude-term.el's own load time. `modules/git-common-dir.el'
;; loads BEFORE this file (init.el's `load-module' order, ahead of
;; claude-term-registry) precisely so this file's repo-name labeling can
;; call its shared, single-implementation `edmacs-git-common-dir'
;; instead of carrying its own second copy of that resolution.
;;
;; Two sharp edges named in the phase body, both fixed here rather than
;; reproduced (see claude-code-ide.el's implementation):
;;   - Sessions are keyed by `file-truename', not `expand-file-name', so
;;     a symlinked path to the same worktree does not register as a
;;     second, distinct session.
;;   - The picker label includes the owning repo name (not just the
;;     worktree leaf), even though rdm worktree slugs are globally
;;     unique today -- that uniqueness is a convention, not an
;;     invariant, and including it costs nothing.
;;
;; Two swappable seams are stubbed here for phase 5 to replace without
;; touching this file's other code: `claude-term-registry-state-accessor'
;; (always `idle' here) and `claude-term-registry-sort-function' (MRU
;; here). Phase 5 also owns the `SPC a TAB' binding this phase
;; deliberately leaves unbound.
;;
;; Run pure-function tests:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/claude-term.el \
;;         -l modules/claude-term-registry.el \
;;         -l modules/claude-term-registry-test.el \
;;         -f ert-run-tests-batch-and-exit
;; Run live-subprocess tests:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/claude-term.el \
;;         -l modules/claude-term-registry.el \
;;         -l modules/claude-term-registry-live-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)

;; claude-term.el is always loaded before this file in this codebase's
;; module system (init.el's `load-module' order, and every test file's
;; own -l ordering documented above); these forward declarations exist
;; only for standalone byte-compilation clarity, never a real forward
;; reference at call time.
(defvar ghostel--process)
(defvar claude-term--root)
(defvar claude-term--instance)
(defvar claude-term--restarting)
(declare-function claude-term--leaf "claude-term")
(declare-function claude-term--parse-buffer-name "claude-term")
(declare-function claude-term--display-buffer "claude-term")
(declare-function claude-term--pop-to-side-window "claude-term")
(declare-function claude-term-buffer-name "claude-term")
(declare-function claude-term "claude-term")

;; `general' is loaded lazily (see modules/keybindings.el's `use-package
;; general'); declared here for the same byte-compiler reason as the
;; ghostel-family declarations in claude-term.el.
(declare-function general-define-key "general")

;; ============================================================================
;; Session struct and hash-table registry
;; ============================================================================

(cl-defstruct claude-term-session
  "One registered claude-term session.
ROOT is the project root exactly as passed to `claude-term-registry-put'
-- untouched, not truename-normalized (see `claude-term-registry--key'
for where that normalization actually happens, and why only the key
needs it). INSTANCE is the instance label, or nil for the default
instance. BUFFER is the live claude-term buffer. PROCESS is BUFFER's
`ghostel--process' as of the last `claude-term-registry-put' (a fresh
spawn or a restart's re-exec both call it, so this stays current; see
`claude-term-registry--process-of') -- present as a literal struct
field to match the phase body's own \"buffer, process, instance name,
and last-used time\" data shape, even though every current call site
(`claude-term-kill', `claude-term-kill-all') still reads the live
process off the buffer-local `ghostel--process' directly rather than
through this field, since that is definitionally always at least as
fresh. LAST-USED is a `float-time' timestamp, updated by
`claude-term-registry-touch'."
  root instance buffer process last-used)

(defun claude-term-registry--process-of (buffer)
  "Return BUFFER's buffer-local `ghostel--process' value, or nil.
Uses `local-variable-p' rather than `buffer-local-value' directly so a
BUFFER with no buffer-local `ghostel--process' at all -- e.g. a bare
test buffer never passed through `claude-term--exec' -- never signals
`void-variable': `ghostel--process' carries no global default value of
its own (see claude-term.el's bare `(defvar ghostel--process)')."
  (and (local-variable-p 'ghostel--process buffer)
       (buffer-local-value 'ghostel--process buffer)))

(defvar claude-term-registry--table (make-hash-table :test #'equal)
  "Hash table of registered claude-term sessions.
Keyed by `claude-term-registry--key', which see -- NOT a plain (root
. instance) cons, so two different-looking paths to the same worktree
collapse to one entry rather than registering as two.")

(defun claude-term-registry--key (root instance)
  "Return the registry key for project ROOT and INSTANCE.
`file-truename' normalizes ROOT so a symlinked path to the same
worktree does not register as a distinct session from the real path --
the sharp edge the phase body names in claude-code-ide.el's own
`string='/`expand-file-name' matching."
  (cons (file-truename root) instance))

(defun claude-term-registry-put (root instance buffer)
  "Register BUFFER as the live session for ROOT/INSTANCE.
Stamps LAST-USED to now -- a freshly spawned session should sort as
most-recently-used from the moment it exists, not only from its first
later `claude-term-registry-touch'. Snapshots BUFFER's current
`ghostel--process' into the PROCESS field via
`claude-term-registry--process-of' -- called again on every fresh spawn
and every restart's re-exec, so this stays current across a restart
without needing its own separate update path."
  (puthash (claude-term-registry--key root instance)
           (make-claude-term-session :root root :instance instance
                                      :buffer buffer
                                      :process (claude-term-registry--process-of buffer)
                                      :last-used (float-time))
           claude-term-registry--table))

(defun claude-term-registry-get (root instance)
  "Return the registered session for ROOT/INSTANCE, or nil."
  (gethash (claude-term-registry--key root instance) claude-term-registry--table))

(defun claude-term-registry-remove (root instance)
  "Remove the registered session for ROOT/INSTANCE, if any."
  (remhash (claude-term-registry--key root instance) claude-term-registry--table))

(defun claude-term-registry-touch (root instance)
  "Update the last-used time of the ROOT/INSTANCE session to now.
No-op when no such session is registered -- e.g. a stale call racing a
buffer that was already killed and reaped out from under it."
  (when-let* ((session (claude-term-registry-get root instance)))
    (setf (claude-term-session-last-used session) (float-time))))

(defun claude-term-registry-sessions ()
  "Return all live registered sessions, dropping (and reaping) dead ones.
Self-healing against a user directly `kill-buffer'-ing a claude-term
buffer, bypassing `claude-term--on-exit' entirely -- without this, a
dead buffer would linger in the table forever and surface as a phantom
picker/list row, or error when something tries to display it.
Collects dead keys during the `maphash' pass and removes them only
afterward, never during -- mutating a hash table while `maphash' is
iterating it is unsupported."
  (let (live dead-keys)
    (maphash
     (lambda (key session)
       (if (buffer-live-p (claude-term-session-buffer session))
           (push session live)
         (push key dead-keys)))
     claude-term-registry--table)
    (dolist (key dead-keys)
      (remhash key claude-term-registry--table))
    live))

;; ============================================================================
;; State accessor and sort comparator -- swappable seams for phase 5
;; ============================================================================

(defun claude-term-registry--stub-state (_session)
  "Stub state accessor: every session reports `idle'.
Phase 5 reassigns `claude-term-registry-state-accessor' to a real
reader over its own attention-state table (waiting/done/working/idle).
This phase ships only the always-`idle' stub -- the swappable seam
itself, not the real state, is the deliverable here."
  'idle)

(defvar claude-term-registry-state-accessor #'claude-term-registry--stub-state
  "Function of one `claude-term-session', returning its attention state.
A swappable seam: phase 5 reassigns this to a real state-table reader.
Callers must always go through this variable rather than calling
`claude-term-registry--stub-state' directly, so reassigning it changes
every caller (picker, list buffer) at once.")

(defun claude-term-registry--sort-mru (a b)
  "Compare sessions A and B for most-recently-used-first order."
  (> (claude-term-session-last-used a) (claude-term-session-last-used b)))

(defvar claude-term-registry-sort-function #'claude-term-registry--sort-mru
  "Comparator used to sort the picker's session candidates.
A swappable seam, named rather than an inline lambda so it can be
reassigned independently: phase 5 replaces this with an
attention-ordered comparator (waiting > done > working > idle) without
touching `claude-term--read-session's body.")

;; ============================================================================
;; Repo name / elapsed time / label
;; ============================================================================
;; The git-common-dir resolution itself (TRAMP-safe `process-file' call,
;; relative/remote path normalization, cached-miss memoization) lives in
;; `modules/git-common-dir.el', shared with `modules/sessions.el's own
;; repo-name-for-tab-naming need, rather than reimplemented here -- both
;; consumers previously carried independent copies of the same
;; algorithm, which this factoring removes. That module loads before
;; this one (init.el's `load-module' order), so no `require' is needed
;; under this codebase's shared-obarray plain-`load' module system; the
;; `declare-function' below exists only for standalone byte-compilation
;; clarity.
(declare-function edmacs-git-common-dir "git-common-dir")

(defun claude-term-registry--repo-name (root)
  "Return the repo name owning ROOT, falling back to ROOT's bare leaf name.
Derives the name from `edmacs-git-common-dir's parent directory -- that
call is itself memoized per ROOT, so no separate cache is needed here
for the cheap string manipulation on top of it. Falls back rather than
erroring when the git lookup fails -- a pruned worktree still lingering
in the registry, or a non-git root reachable only in tests -- so a
single bad entry never takes down the whole picker/list render."
  (let ((common (edmacs-git-common-dir root)))
    (if common
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory (directory-file-name common))))
      (claude-term--leaf root))))

(defun claude-term-registry--elapsed-string (session)
  "Return a short humanized elapsed-time string for SESSION's last-used time.
A just-spawned session's elapsed time is 0 seconds, formatted plainly
as \"0s\" -- no division happens in this implementation, so there is no
divide-by-zero hazard, but the zero case is exercised explicitly by the
test suite as a guard against a future rewrite introducing one."
  (let ((seconds (max 0 (round (- (float-time) (claude-term-session-last-used session))))))
    (cond
     ((< seconds 60) (format "%ds" seconds))
     ((< seconds 3600) (format "%dm" (/ seconds 60)))
     ((< seconds 86400) (format "%dh" (/ seconds 3600)))
     (t (format "%dd" (/ seconds 86400))))))

(defconst claude-term-registry--state-icons
  '((idle . "o") (working . "*") (waiting . "?") (done . "v"))
  "Icon per attention state, keyed by the symbol
`claude-term-registry-state-accessor' returns. Phase 5 owns the real
waiting/done/working states; this phase's stub only ever returns
`idle', so only that icon is exercised here -- the others are
pre-declared so phase 5's swap-in needs no icon-table change of its
own. Plain ASCII rather than unicode glyphs, so the picker/list render
identically in any terminal or font this config runs under.")

(defun claude-term-registry--state-icon (session)
  "Return the icon string for SESSION's current attention state."
  (or (alist-get (funcall claude-term-registry-state-accessor session)
                 claude-term-registry--state-icons)
      "?"))

(defvar claude-term-registry--default-instance-label "default"
  "Env-var / UI label for a claude-term session with no explicit instance.
Used only for `EDMACS_AGENT_INSTANCE' and picker/rename/list-mode text
-- the buffer-local `claude-term--instance' and the registry's own
INSTANCE field both stay nil for the default case, unchanged from
phases 1-3.")

(defun claude-term--session-label (session)
  "Return the picker/list label string for SESSION.
Format: \"<icon> <leaf>[:<instance>] (<repo>) -- <elapsed>\". The
instance segment is included only when SESSION has a non-nil instance,
so two named instances under one root each get a visibly distinct
label satisfying the phase's own Done-when clause; the default
instance's label carries no colon-suffix, matching claude-term.el's
existing bare-leaf buffer-naming convention."
  (let* ((root (claude-term-session-root session))
         (instance (claude-term-session-instance session))
         (leaf (claude-term--leaf root))
         (leaf-label (if instance (format "%s:%s" leaf instance) leaf))
         (repo (claude-term-registry--repo-name root))
         (elapsed (claude-term-registry--elapsed-string session)))
    (format "%s %s (%s) -- %s"
            (claude-term-registry--state-icon session) leaf-label repo elapsed)))

;; ============================================================================
;; Picker
;; ============================================================================

(defun claude-term--read-session (prompt)
  "Return a `claude-term-session' chosen interactively via PROMPT.
Sorts candidates through `claude-term-registry-sort-function' and
labels them via `claude-term--session-label', so both phase 5 swap
points (state accessor, sort comparator) are exercised even though
this phase ships only the stub/MRU behavior. Signals a clear
`user-error' rather than handing `completing-read' an empty candidate
list when no session is registered -- claude-term.el's
`claude-term--read-buffer' (the shared session-selection entry point
for `claude-term-kill' and `claude-term-restart') delegates to this
function for its own cross-project picking, so this is the one place
that empty-list handling lives."
  (let ((sessions (sort (claude-term-registry-sessions) claude-term-registry-sort-function)))
    (when (null sessions)
      (user-error "Claude-term: no registered sessions"))
    (let* ((alist (mapcar (lambda (s) (cons (claude-term--session-label s) s)) sessions))
           (choice (completing-read prompt (mapcar #'car alist) nil t)))
      (cdr (assoc choice alist)))))

;; ============================================================================
;; Launcher contract: EDMACS_AGENT_INSTANCE
;; ============================================================================

(defun claude-term-registry--set-instance-env ()
  "Set `EDMACS_AGENT_INSTANCE' for a spawning claude-term buffer.
Added to the GLOBAL `ghostel-pre-spawn-hook' below, which fires for
every ghostel buffer's spawn, not just claude-term's -- the buffer-name
guard here is therefore load-bearing: it must not leak a value into a
plain, non-claude-term ghostel session someone opens directly.

Reads the instance straight out of the spawning buffer's own NAME
\(already parseable via `claude-term--parse-buffer-name') rather than
through a new dynamic-variable channel: the buffer's name is set by
`get-buffer-create' before `claude-term--exec' ever runs and is
intrinsic to the buffer, so it survives `ghostel-mode's first-run
`kill-all-local-variables' wipe -- this hook can fire on a buffer's
very first spawn and still see the right name.

Runs with `process-environment' dynamically bound to the
about-to-be-spawned child's environment (per `ghostel-pre-spawn-hook's
own docstring in ghostel.el), so `setenv' here is inherited by the real
`claude' process with no further plumbing needed. Renaming a session
later (`claude-term-rename') does NOT retroactively change this
already-running child's environment -- only a subsequent restart,
which re-fires this hook against the (by then renamed) buffer name."
  (when-let* ((parsed (claude-term--parse-buffer-name (buffer-name))))
    (setenv "EDMACS_AGENT_INSTANCE"
            (or (cdr parsed) claude-term-registry--default-instance-label))))

(add-hook 'ghostel-pre-spawn-hook #'claude-term-registry--set-instance-env)

;; ============================================================================
;; Rename
;; ============================================================================

(defun claude-term-registry-rename (root old-instance new-instance)
  "Move the ROOT/OLD-INSTANCE session to key ROOT/NEW-INSTANCE.
Signals a `user-error' when a DIFFERENT session is already registered
under ROOT/NEW-INSTANCE, rather than silently clobbering its registry
entry -- renaming into a same-root collision is a user mistake to
reject, not a merge request to honor. Renaming to an instance label
already used under a DIFFERENT root is unaffected, since the key
includes root."
  (let ((session (claude-term-registry-get root old-instance)))
    (unless session
      (user-error "Claude-term: no session registered for instance %s"
                  (or old-instance claude-term-registry--default-instance-label)))
    (when (and (not (equal old-instance new-instance))
               (claude-term-registry-get root new-instance))
      (user-error "Claude-term: instance %s already exists for this root"
                  (or new-instance claude-term-registry--default-instance-label)))
    (remhash (claude-term-registry--key root old-instance) claude-term-registry--table)
    (setf (claude-term-session-instance session) new-instance)
    (puthash (claude-term-registry--key root new-instance) session claude-term-registry--table)))

(defun claude-term-rename (&optional buffer)
  "Rename claude-term BUFFER's instance label, prompting for a new one.
BUFFER defaults to the current buffer when it is a claude-term buffer,
else it is chosen via `claude-term--read-session'. Migrates the
registry entry via `claude-term-registry-rename' (which rejects a
same-root collision against a DIFFERENT session), updates the
buffer-local `claude-term--instance', and renames the buffer itself to
match -- but does NOT alter `EDMACS_AGENT_INSTANCE' in the already-
running child process; see `claude-term-registry--set-instance-env's
docstring for why.

Also rejects (via `user-error', before touching the registry or
renaming anything) a CROSS-root leaf-name collision: the target buffer
name is computed from ROOT's bare leaf directory name only (see
`claude-term-buffer-name'), so an unrelated project whose root merely
shares that leaf name -- rdm slugs are unique by convention, not by
invariant -- could already own a live buffer under NEW-INSTANCE.
`claude-term-registry-rename's own collision check is scoped to THIS
root, so it cannot see that case; left unhandled, `rename-buffer' would
silently uniquify the on-disk name (e.g. a `<2>' suffix), which no
longer matches `claude-term--buffer-name-regexp' -- breaking every
other claude-term command's `claude-term--parse-buffer-name' buffer-is-
a-claude-term-session test against it, with no error at all. Mirrors
`claude-term--resolve-instance's own numeric-fallback rationale for the
same underlying hazard on fresh spawns, but rejects here rather than
silently substituting a different label, since the user explicitly
chose NEW-INSTANCE by name."
  (interactive)
  (let* ((buffer (or buffer
                      (if (claude-term--parse-buffer-name (buffer-name))
                          (current-buffer)
                        (claude-term-session-buffer
                         (claude-term--read-session "Rename session: ")))))
         (root (buffer-local-value 'claude-term--root buffer))
         (old-instance (buffer-local-value 'claude-term--instance buffer))
         (input (read-string "New instance label: "))
         (new-instance (if (string-empty-p input) nil input))
         (target-name (claude-term-buffer-name root new-instance))
         (colliding (get-buffer target-name)))
    (when (and colliding (not (eq colliding buffer)))
      (user-error "Claude-term: buffer %s already exists (an unrelated project's session sharing this leaf name) -- pick a different instance label"
                  target-name))
    (claude-term-registry-rename root old-instance new-instance)
    (with-current-buffer buffer
      (setq-local claude-term--instance new-instance)
      (rename-buffer target-name t))))

;; ============================================================================
;; SPC a commands
;; ============================================================================

;;;###autoload
(defun claude-term-new-session (&optional instance)
  "Start a new claude-term session, prompting for INSTANCE if not given.
Delegates entirely to the existing `claude-term' entry point; this
command only adds the interactive instance prompt -- an empty answer
means the default (nil) instance, matching phases 1-3's own
convention."
  (interactive
   (list (let ((input (read-string "New session instance (default): ")))
           (if (string-empty-p input) nil input))))
  (claude-term instance))

;;;###autoload
(defun claude-term-jump ()
  "Switch to a claude-term session chosen via `claude-term--read-session'.
Touches the chosen session's last-used time on switch, so a
subsequently reopened picker shows it back at the top of the default
MRU order."
  (interactive)
  (let* ((session (claude-term--read-session "Jump to session: "))
         (buffer (claude-term-session-buffer session)))
    (claude-term--pop-to-side-window buffer)
    (claude-term-registry-touch (claude-term-session-root session)
                                (claude-term-session-instance session))))

(defun claude-term-registry--list-entries ()
  "Return `tabulated-list-entries' for every registered claude-term session."
  (mapcar
   (lambda (session)
     (list session
           (vector (claude-term-registry--state-icon session)
                   (or (claude-term-session-instance session)
                       claude-term-registry--default-instance-label)
                   (claude-term--leaf (claude-term-session-root session))
                   (claude-term-registry--repo-name (claude-term-session-root session))
                   (claude-term-registry--elapsed-string session))))
   (claude-term-registry-sessions)))

(defun claude-term-registry--revert-list (&rest _)
  "`revert-buffer-function' for `claude-term-session-list-mode'.
Re-fetches `claude-term-registry-sessions' rather than caching --
pressing `g' (bound by `tabulated-list-mode' to `revert-buffer') must
always show live state, including reaping any buffer killed directly
since the listing was last drawn."
  (setq tabulated-list-entries (claude-term-registry--list-entries))
  (tabulated-list-print t))

(define-derived-mode claude-term-session-list-mode tabulated-list-mode "Claude-Term-Sessions"
  "Major mode listing every registered claude-term session.
Read-only; `g' refreshes. Deliberately NOT selection-driven -- that is
`claude-term-jump's job; this mode only shows the roster, keeping the
two commands' scopes distinct per the phase body's own separate
listing of them."
  (setq tabulated-list-format
        [("State" 6 t) ("Instance" 12 t) ("Worktree" 20 t) ("Repo" 16 t) ("Last used" 10 t)])
  (setq tabulated-list-padding 2)
  (setq revert-buffer-function #'claude-term-registry--revert-list)
  (tabulated-list-init-header))

;;;###autoload
(defun claude-term-list-sessions ()
  "Display every registered claude-term session in a read-only listing."
  (interactive)
  (let ((buffer (get-buffer-create "*claude-term-sessions*")))
    (with-current-buffer buffer
      (claude-term-session-list-mode)
      (claude-term-registry--revert-list))
    (pop-to-buffer buffer)))

;;;###autoload
(defun claude-term-toggle-pane ()
  "Toggle visibility of every side window, including claude-term panes.
Thin wrapper around the stock `window-toggle-side-windows' -- no new
side-window logic of its own; reuses phase 2's existing column."
  (interactive)
  (window-toggle-side-windows))

;;;###autoload
(defun claude-term-show-all ()
  "Display every registered, still-live claude-term session's buffer.
Calls the existing, non-selecting `claude-term--display-buffer' on
each. Interacts with `window-sides-slots's right-side cap of 3 (set in
claude-term.el phases 1-3): a 4th+ simultaneous session will silently
evict/reuse a slot per Emacs's own side-window slot-reuse behavior --
a pre-existing, accepted limitation this phase does not change."
  (interactive)
  (dolist (session (claude-term-registry-sessions))
    (claude-term--display-buffer (claude-term-session-buffer session))))

;;;###autoload
(defun claude-term-kill-all ()
  "Kill every registered claude-term session's live process.
Explicitly clears an in-flight `claude-term--restarting' flag before
`kill-process' on any session mid-restart -- guards against a restart
racing this global kill and silently resurrecting a session moments
after it was asked to die (see the phase body's own edge case)."
  (interactive)
  (dolist (session (claude-term-registry-sessions))
    (with-current-buffer (claude-term-session-buffer session)
      (when (process-live-p ghostel--process)
        (setq-local claude-term--restarting nil)
        (kill-process ghostel--process)))))

;; ============================================================================
;; SPC a keybindings
;; ============================================================================
;; Letters chosen to avoid every one of modules/ai.el's existing
;; claude-repl bindings at SPC a: a, I, b, c, s, k, K, l, i, t, plus the
;; "p" submenu prefix -- verified live against modules/ai.el, 2026-09-01
;; (claude-repl is retired only in phase 6, so this module must coexist
;; with it until then). Deliberately does NOT bind TAB anywhere under
;; SPC a -- see `claude-term-registry-sort-function's own docstring;
;; phase 5 owns that binding together with the attention-ordered
;; comparator it needs, and binding it here against this phase's stub
;; would ship a key that cannot do what its name says.

(with-eval-after-load 'general
  (general-define-key
   :states 'normal
   :prefix "SPC a"
   "n" '(claude-term-new-session :which-key "new session")
   "j" '(claude-term-jump :which-key "jump to session")
   "L" '(claude-term-list-sessions :which-key "list sessions")
   "w" '(claude-term-toggle-pane :which-key "toggle pane")
   "A" '(claude-term-show-all :which-key "show all sessions")
   "x" '(claude-term-kill :which-key "kill session")
   "X" '(claude-term-kill-all :which-key "kill all sessions")
   "r" '(claude-term-rename :which-key "rename session")))

(provide 'claude-term-registry)
;;; claude-term-registry.el ends here
