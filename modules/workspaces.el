;;; workspaces.el --- Project/worktree identity on tab-bar groups -*- lexical-binding: t -*-

;;; Commentary:
;; The new identity model for roadmap `edmacs-tab-groups': a project is a
;; tab-bar GROUP, a worktree is a TAB inside that group. This module is
;; the one place that answers "what project is this?" and "what worktree
;; is this?", and -- since phase 2 -- the one that drives `SPC p p'
;; (`edmacs-workspaces-open-project') and `SPC T p' / `C-x t p'
;; (`edmacs-workspaces-open-worktree'). The two models still live in
;; separate files, which is what keeps a later phase's deletion of
;; `frames.el' a removal rather than an unpick.
;;
;; ============================================================================
;; Load-time side effects: two hooks
;; ============================================================================
;; This module installs exactly two hook entries at load time, both of
;; which `frames.el' vacated in the same phase:
;;
;; - `edmacs-workspaces--on-tab-post-open' on
;;   `tab-bar-tab-post-open-functions'. STAMP-ONLY. `frames.el' put a
;;   combined stamper/reconciler there; only the reconciler was the
;;   defect (its frame-wide, group-unaware `seq-find' closed the very
;;   tab it had just stamped, because at post-open time the new tab's
;;   window still shows the PREVIOUS buffer). Duplicate prevention moved
;;   into the two entry points, which are group-scoped. What remains
;;   here is the stamper, without which a tab from a plain
;;   `M-x tab-bar-new-tab' / `SPC T n' would carry no root at all.
;; - `edmacs-workspaces--on-window-buffer-change' on
;;   `window-buffer-change-functions', which defers a TAB-scoped
;;   stray-visit sweep onto a zero-delay timer.
;;
;; The tab-bar and desktop facts this module builds on (group needs no
;; explicit stamping to survive a restart, `tab-bar-move-tab-to-group'
;; keeps a group's tabs contiguous for free, ...) are recorded once in
;; the roadmap body under "Verified against Emacs 31.1" -- restated here
;; only where a specific function relies on one of them.
;;
;; ============================================================================
;; Root parameter: `edmacs-workspace-root', not `edmacs-root'
;; ============================================================================
;; `edmacs-workspaces-root-parameter' is a NEW tab parameter, deliberately
;; not `frames.el''s own `edmacs-root'. The two models coexist on the same
;; tab objects under different keys with no interaction: nothing here
;; reads or writes `edmacs-root', and nothing in `frames.el' reads or
;; writes `edmacs-workspace-root'. This module is the sole writer and
;; reader of its own parameter. Reusing `edmacs-root' would have made
;; this phase and `frames.el' contend over one parameter while both
;; models are live, and would have turned phase 5's eventual deletion of
;; `frames.el''s copy into an unpick instead of a clean removal.
;;
;; ============================================================================
;; The symlink question (roadmap dependency)
;; ============================================================================
;; `edmacs-workspaces-group-name' derives a group name via
;; `edmacs-git-common-dir-repo-name', which returns only the FINAL path
;; component of the main worktree root `edmacs-git-common-dir' resolves.
;; Two symlink shapes interact with that differently:
;;
;; - An ancestor symlink (e.g. macOS's /var -> /private/var, or a
;;   symlinked home directory) never reaches the final path component:
;;   `edmacs-git-common-dir-repo-name' -- and so `edmacs-workspaces-group-name'
;;   -- returns the same string regardless of which alias resolved the
;;   ancestor. Unaffected.
;; - A symlink that IS the repo's own worktree directory (the documented
;;   `~/.config/emacs' -> `~/Projects/edmacs' case) changes the final
;;   component itself: from the MAIN worktree, `edmacs-git-common-dir-1'
;;   expands git's relative ".git" answer against ROOT, keeping ROOT's own
;;   symlinked basename ("emacs") rather than the target's ("edmacs") --
;;   so the SAME repo yields two different group names depending on which
;;   alias it was opened through. Affected.
;;
;; So group naming is NOT unconditionally safe: this roadmap has a written
;; dependency on task `git-common-dir-symlink-mismatch', which documents
;; the same underlying skew (there, for the `edmacs-repo' frame parameter
;; rather than a tab-bar group name). See that task for the fix location
;; (`edmacs-git-common-dir-1'/`edmacs-git-common-dir').
;;
;; ============================================================================
;; Enumeration functions take an explicit FRAME
;; ============================================================================
;; Every function below that walks tabs takes an optional FRAME argument
;; defaulting to `(selected-frame)' via the `(or frame (selected-frame))'
;; idiom (ambient-reads discipline, `scripts/ambient-reads.el') rather
;; than reading `selected-frame' unconditionally, so a caller acting on a
;; background frame never accidentally reads the wrong one.
;;
;; ============================================================================
;; The worktree-candidate source is deliberately cache-free
;; ============================================================================
;; `edmacs-workspaces-worktrees' shells out via
;; `vc-git-known-other-working-trees' on every call: no cache, no
;; `file-notify' watch, no polling or debounce timer. (The one
;; `run-at-time' in this file is the stray sweep's zero-delay hop off
;; the redisplay path, which schedules nothing recurring.) This is the direct replacement for the
;; ~225-line discovery layer (`frames.el' worktree cache + debounced
;; file-notify watch) the roadmap's scope decision deletes outright in a
;; later phase -- it must not grow one of its own. `vc-git-known-other-working-trees'
;; excludes ROOT's own worktree by design and returns paths through
;; `abbreviate-file-name' (so a result can read "~/..."), which is why
;; ROOT's own truename is consed back on and every result is passed
;; through `expand-file-name'.
;;
;; TRAMP: `vc-git-known-other-working-trees' inherits whatever TRAMP
;; safety `vc-git' itself has; this module does not re-verify it the way
;; `edmacs-git-common-dir-1' explicitly handles a remote ROOT via
;; `process-file'. Treat remote-root worktree enumeration as unverified.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'tab-bar)
;; For `project-current-directory-override', which `project-switch-project'
;; binds before dispatching to `edmacs-workspaces-open-project'.
(require 'project)
(require 'vc-git)

;; Autoloaded; `frames.el' calls it the same way.
(declare-function dired "dired" (dirname &optional switches))

;; Available via init.el's `load-module' order, not a `require' -- see
;; git-common-dir.el's own commentary on this codebase's shared-obarray
;; plain-`load' module system. Declared here so this file byte-compiles
;; standalone and so `workspaces-test.el' can run under `-Q --batch'.
(declare-function edmacs-git-common-dir "git-common-dir")
(declare-function edmacs-git-common-dir-main-worktree "git-common-dir")
(declare-function edmacs-git-common-dir-repo-name "git-common-dir")

;; ============================================================================
;; Group name derivation
;; ============================================================================

(defun edmacs-workspaces-group-name (root)
  "Return ROOT's project tab-bar group name, or nil when ROOT has none.
Reuses `edmacs-git-common-dir'/`edmacs-git-common-dir-repo-name' rather
than re-deriving a repo name from ROOT's own path directly -- see this
file's Commentary on the symlink dependency those two functions carry."
  (when-let* ((common (edmacs-git-common-dir root)))
    (edmacs-git-common-dir-repo-name common)))

;; ============================================================================
;; Enumeration -- groups, and the tabs inside one
;; ============================================================================

(defun edmacs-workspaces-groups (&optional frame)
  "Return the distinct non-nil group names among FRAME's tabs.
FRAME defaults to the selected frame. A tab created before any grouping
exists carries no `group' parameter at all; such tabs are excluded
entirely rather than contributing a nil \"group\" to the result."
  (delete-dups
   (delq nil (mapcar (lambda (tab) (funcall tab-bar-tab-group-function tab))
                      (tab-bar-tabs (or frame (selected-frame)))))))

(defun edmacs-workspaces-tabs-in-group (group &optional frame)
  "Return the tabs of FRAME (default selected) whose group is GROUP.
GROUP must be non-nil: an ungrouped tab never matches, matching
`edmacs-workspaces-groups' own exclusion of nil groups."
  (when group
    (seq-filter (lambda (tab) (equal (funcall tab-bar-tab-group-function tab) group))
                (tab-bar-tabs (or frame (selected-frame))))))

;; ============================================================================
;; Root classification -- main / roadmap / task / nil
;; ============================================================================

(defun edmacs-workspaces--leaf-and-parent (root)
  "Return (LEAF . PARENT-BASENAME) for ROOT, ignoring any trailing slash.
Pure string manipulation -- no disk access -- so it works the same for a
real path and for a stubbed test fixture that names no real directory."
  (let* ((clean (directory-file-name root))
         (leaf (file-name-nondirectory clean))
         (parent-dir (file-name-directory clean)))
    (cons leaf (and parent-dir (file-name-nondirectory (directory-file-name parent-dir))))))

(defun edmacs-workspaces-classify-root (root)
  "Classify ROOT as the symbol `main', `roadmap', `task', or nil.
`main' when ROOT (truename-compared) is the repo's own main worktree.
Otherwise, `roadmap'/`task' from rdm's `<repo>__worktrees/<slug>'
naming: a parent directory whose basename ends in \"__worktrees\" and a
leaf directory name starting with \"roadmap-\" or \"task-\" respectively.
Nil for anything else, including a ROOT git-common-dir cannot resolve at
all (a pruned worktree, or not a git repo) -- this function never
signals on that, it simply falls through to the naming check and then
to nil."
  (when root
    (let* ((common (edmacs-git-common-dir root))
           (main (and common (edmacs-git-common-dir-main-worktree common))))
      (if (and main (equal (file-truename (file-name-as-directory root))
                           (file-truename (file-name-as-directory main))))
          'main
        (pcase-let ((`(,leaf . ,parent) (edmacs-workspaces--leaf-and-parent root)))
          (cond
           ((and parent (string-suffix-p "__worktrees" parent)
                 (string-prefix-p "roadmap-" leaf))
            'roadmap)
           ((and parent (string-suffix-p "__worktrees" parent)
                 (string-prefix-p "task-" leaf))
            'task)
           (t nil)))))))

;; ============================================================================
;; Per-tab worktree root
;; ============================================================================

(defconst edmacs-workspaces-root-parameter 'edmacs-workspace-root
  "The tab parameter this module owns for a tab's worktree root.
See this file's Commentary for why it is not `frames.el''s `edmacs-root'.")

(defun edmacs-workspaces-tab-root (tab)
  "Return TAB's worktree root, or nil.
A pure read: no derivation, no side effect."
  (alist-get edmacs-workspaces-root-parameter tab))

(defun edmacs-workspaces-set-tab-root (root &optional frame)
  "Stamp ROOT onto FRAME's (default selected) current tab. Returns ROOT.
Mirrors `frames.el''s own `edmacs-frames--stamp-current-tab-root': `setf'
rather than `push', so re-stamping REPLACES the entry -- a push-shadowed
stale cons would survive `tab-bar--tab''s copy-other-parameters
forwarding on every later tab switch and outlive the session via
desktop, exactly the hazard that function's own commentary warns about
for `edmacs-root'."
  (when-let* ((tab (tab-bar--current-tab-find nil frame)))
    (setf (alist-get edmacs-workspaces-root-parameter (cdr tab)) root)
    root))

(defun edmacs-workspaces-find-tab (group root &optional frame)
  "Return the tab of FRAME (default selected) whose group is GROUP and
worktree root is ROOT, or nil."
  (seq-find (lambda (tab)
              (and (equal (funcall tab-bar-tab-group-function tab) group)
                   (equal (edmacs-workspaces-tab-root tab) root)))
            (tab-bar-tabs (or frame (selected-frame)))))

(defun edmacs-workspaces-select-tab (group root &optional frame)
  "Find and select the tab of FRAME matching GROUP and ROOT.
Returns the tab, or nil when none matches -- selection is skipped
entirely in that case. `tab-bar-select-tab' has no FRAME argument of its
own (it always operates on the selected frame), so a non-nil FRAME is
wrapped in `with-selected-frame' -- unconditionally, since wrapping the
already-selected frame is harmless and this avoids re-reading
`selected-frame' merely to compare it against FRAME."
  (let* ((target (or frame (selected-frame)))
         (tab (edmacs-workspaces-find-tab group root target)))
    (when tab
      (let ((number (1+ (tab-bar--tab-index tab (tab-bar-tabs target) target))))
        (if frame
            (with-selected-frame frame
              (tab-bar-select-tab number))
          (tab-bar-select-tab number))))
    tab))

;; ============================================================================
;; Group assignment -- always through `tab-bar-change-tab-group'
;; ============================================================================

(defun edmacs-workspaces-assign-group (group-name &optional tab-number frame)
  "Assign GROUP-NAME to TAB-NUMBER (default the current tab) of FRAME.
The only writer of a tab's `group' parameter in this module. Routed
through `tab-bar-change-tab-group' rather than a raw `alist-get' write,
so `tab-bar-tab-post-change-group-functions' -- whose default,
`tab-bar-move-tab-to-group', is what keeps a group's tabs contiguous --
keeps running. `tab-bar-change-tab-group' has no FRAME argument of its
own (it always operates on the selected frame), so a non-nil FRAME is
wrapped in `with-selected-frame' -- unconditionally, since wrapping the
already-selected frame is harmless and this avoids re-reading
`selected-frame' merely to compare it against FRAME."
  (if frame
      (with-selected-frame frame
        (tab-bar-change-tab-group group-name tab-number))
    (tab-bar-change-tab-group group-name tab-number)))

;; ============================================================================
;; Worktree enumeration -- on demand, no cache
;; ============================================================================

(defun edmacs-workspaces-worktrees (root)
  "Return ROOT's repo worktrees, ROOT's own truename first.
Shells out via `vc-git-known-other-working-trees' on every call -- no
cache, no `file-notify' watch, no timer anywhere in this file. See this
file's Commentary for why, and for the TRAMP caveat.

Falls through to just ROOT's own truename, never signals, when ROOT
does not resolve to a git worktree at all (nonexistent directory, or a
pruned worktree) -- `vc-git-known-other-working-trees' itself signals
`wrong-type-argument' in that case, since it unconditionally
`expand-file-name's `vc-git-root''s nil result rather than checking
it first. Guarding on `vc-git-root' here, before ever calling into
that function, keeps this the caller's contract: matches
`edmacs-workspaces-classify-root''s convention of falling through to
an empty/nil result rather than an internal Lisp error."
  (let* ((default-directory root)
         (mine (file-name-as-directory (file-truename root))))
    ;; `default-directory' is ROOT, let-bound just above -- ambient-reads: ok
    (if (not (vc-git-root default-directory))
        (list mine)
      (let ((others (mapcar (lambda (dir) (file-name-as-directory (expand-file-name dir)))
                             (vc-git-known-other-working-trees))))
        (seq-uniq (cons mine others))))))

;; ============================================================================
;; Stamping a new tab's root -- the sole `tab-bar-tab-post-open-functions' entry
;; ============================================================================

(defvar edmacs-workspaces--pending-tab-root nil
  "The root the tab this module is about to create must carry.
Bound only by `edmacs-workspaces--open-tab', around the one
`tab-bar-new-tab' call in this file, so the post-open hook stamps the
INTENDED root instead of deriving one from the buffer the brand-new tab
inherited from whichever tab was selected a moment before.")

(defun edmacs-workspaces--current-tab (frame)
  "Return FRAME's current tab, without ever creating one.
Reads the `tabs' frame parameter directly: `tab-bar-tabs' creates a
default tab on an unset parameter AND runs
`tab-bar-tab-post-open-functions', re-entering the post-open hook from
inside itself."
  (assq 'current-tab (frame-parameter frame 'tabs)))

(defun edmacs-workspaces--derive-root ()
  "Return the selected window's buffer directory as a normalized root, or nil.
The fallback for a tab created outside this module's own entry points --
`SPC T n', `M-x tab-bar-new-tab', any package's `other-tab-prefix'.
At post-open time the new tab's window still shows the ORIGINATING
tab's buffer, which is exactly the answer wanted here: `SPC T n' from a
worktree tab should stamp that same worktree."
  ;; No frame/window parameter exists to prefer -- ambient-reads: ok
  (when-let* ((buf (window-buffer (selected-window)))
              ;; Read off BUF, not the ambient value -- ambient-reads: ok
              (dir (buffer-local-value 'default-directory buf)))
    (file-name-as-directory (file-truename dir))))

(defun edmacs-workspaces--on-tab-post-open (tab)
  "Stamp TAB's worktree root. This module's only post-open hook entry.
Stamp-only by design: no reconciliation, no tab closing, no group
assignment (`tab-bar-new-tab-group' is t, so TAB already inherited the
originating tab's group, and re-assigning here would re-enter
`tab-bar-tab-post-change-group-functions').

The guard is `frames.el''s, for the same reason: `tab-bar-tabs' also
runs this hook for a default tab it auto-creates on a frame it never
names, and stamping that one from the selected frame's buffer would
reintroduce cross-frame derivation."
  ;; `tab-bar-tab-post-open-functions' calls with (TAB), no frame slot --
  ;; ambient-reads: ok
  (when (eq tab (edmacs-workspaces--current-tab (selected-frame)))
    (when-let* ((root (or edmacs-workspaces--pending-tab-root
                          (edmacs-workspaces--derive-root))))
      ;; `setf', never `push': see `edmacs-workspaces-set-tab-root'.
      (setf (alist-get edmacs-workspaces-root-parameter (cdr tab)) root))))

(add-hook 'tab-bar-tab-post-open-functions #'edmacs-workspaces--on-tab-post-open)

;; ============================================================================
;; Entry points -- open a project's group, open a worktree's tab
;; ============================================================================

(defun edmacs-workspaces--open-tab (root group)
  "Create and return a tab showing worktree ROOT, in project GROUP.
The single `tab-bar-new-tab' caller in this module. Step order is
load-bearing:

- The `display-buffer' override is neutralized first. `other-tab-prefix'
  (armed by `C-x t p', `M-x project-other-tab-command', any
  `other-*-prefix') sets `display-buffer-overriding-action' for the next
  displayed buffer; without this binding the `dired' below is redirected
  into `display-buffer-in-tab', re-firing
  `tab-bar-tab-post-open-functions' before the original override has
  cleared itself -- `frames.el''s documented `excessive-lisp-nesting'.
- `edmacs-workspaces-assign-group' runs LAST, because
  `tab-bar-change-tab-group' fires `tab-bar-move-tab-to-group', which
  reorders the tab list. Every earlier step therefore addresses the tab
  as \"current\" rather than by a captured tab-number; the moved tab
  stays selected, so running the assignment last is safe.

GROUP is assigned unconditionally: `tab-bar-new-tab-group' is t, so the
new tab inherited whatever group the previously selected tab had."
  (let ((display-buffer-overriding-action '(nil . nil))
        (switch-to-buffer-obey-display-actions nil))
    (let ((edmacs-workspaces--pending-tab-root root))
      (tab-bar-new-tab))
    (dired root)
    (tab-bar-rename-tab (file-name-nondirectory (directory-file-name root)))
    (edmacs-workspaces-assign-group group))
  ;; Every step above acts on the ambient frame by design -- ambient-reads: ok
  (edmacs-workspaces--current-tab (selected-frame)))

(defun edmacs-workspaces--main-root (root)
  "Return ROOT's repo's main worktree as a normalized root directory.
Falls through to ROOT's own truename when the repo cannot be resolved,
matching `edmacs-workspaces-classify-root''s fall-through-not-signal
convention."
  (let* ((common (edmacs-git-common-dir root))
         (main (and common (edmacs-git-common-dir-main-worktree common))))
    (file-name-as-directory (file-truename (or main root)))))

(defun edmacs-workspaces-open-project (&optional dir)
  "Select DIR's project tab group, creating it and its main worktree tab.
The `project-switch-commands' entry point: `project-switch-project'
binds `project-current-directory-override' to the chosen directory
before dispatching to the single command named there, and reading it
back is that handoff's documented contract.

When the project already has a group, its tab is selected and nothing
is created. This never makes or selects a frame: a project is a tab
group inside the ambient frame."
  (interactive)
  (let* ((dir (or dir project-current-directory-override))
         (root (and dir (file-name-as-directory (file-truename dir))))
         (group (and root (edmacs-workspaces-group-name root))))
    (unless root
      (user-error "No project directory to open"))
    (unless group
      (user-error "Not inside a git repository: %s" root))
    (let ((tabs (edmacs-workspaces-tabs-in-group group))
          (main (edmacs-workspaces--main-root root)))
      (if tabs
          ;; A group with no main-worktree tab (only linked worktrees are
          ;; open) still selects rather than creating a second tab.
          (or (edmacs-workspaces-select-tab group main)
              (progn (tab-bar-select-tab (1+ (tab-bar--tab-index (car tabs))))
                     (car tabs)))
        (edmacs-workspaces--open-tab main group)))))

(defun edmacs-workspaces--read-worktree (prefix)
  "Read a worktree directory. With PREFIX, read any directory instead.
Candidates come from `edmacs-workspaces-worktrees', which shells out on
every call -- acceptable here precisely because this runs only from an
interactive prompt, never from a hook or the redisplay path."
  (if prefix
      (read-directory-name "Worktree: ")
    ;; An interactive prompt: the ambient frame/directory is the intent.
    (let* ((base (or (edmacs-workspaces-tab-root
                      ;; ambient-reads: ok
                      (edmacs-workspaces--current-tab (selected-frame)))
                     ;; ambient-reads: ok
                     default-directory))
           (candidates (edmacs-workspaces-worktrees base)))
      (completing-read "Worktree: " candidates nil t))))

(defun edmacs-workspaces-open-worktree (dir)
  "Open or select worktree DIR's tab, inside its project's group.
Find-or-create, scoped by (group, root): a tab for the same root in a
DIFFERENT project's group is correctly not treated as a duplicate. ROOT
is normalized exactly as `edmacs-workspaces--open-tab' stamps it
\(truename plus trailing slash) -- the two forms must agree or every
invocation would duplicate a tab instead of finding it."
  (interactive (list (edmacs-workspaces--read-worktree current-prefix-arg)))
  (let* ((root (file-name-as-directory (file-truename dir)))
         (group (edmacs-workspaces-group-name root)))
    (unless group
      (user-error "Not inside a git repository: %s" root))
    (if (edmacs-workspaces-find-tab group root)
        (edmacs-workspaces-select-tab group root)
      (edmacs-workspaces--open-tab root group))))

;; ============================================================================
;; Stray visits -- a buffer displayed in the wrong worktree's tab
;; ============================================================================

(defvar edmacs-workspaces-stray-visit-relocate t
  "When non-nil, relocate a buffer displayed in the wrong worktree's tab.
Set to nil to turn this off entirely.")

(defvar edmacs-workspaces--relocating nil
  "Non-nil while a sweep is running. Re-entrancy guard.
The sweep changes window buffers and selects tabs, both of which re-fire
`window-buffer-change-functions' and re-schedule a sweep; because the
work is on a zero-delay timer, the resulting loop would not even be
visible as a hang in one call stack.")

(defun edmacs-workspaces--buffer-dir (buf)
  "Return BUF's directory as a normalized root, or nil.
Keyed on `(or (buffer-file-name buf) default-directory)': the
`default-directory' fallback is what makes a dired buffer -- which has
no `buffer-file-name' at all -- visible to the sweep."
  (let* ((file (buffer-file-name buf))
         (dir (if file
                  (file-name-directory file)
                ;; Read off BUF, not the ambient value -- ambient-reads: ok
                (buffer-local-value 'default-directory buf))))
    (and dir (file-name-as-directory (file-truename dir)))))

(defun edmacs-workspaces--stray-tab-number (buf &optional frame)
  "Return the 1-based number of FRAME's tab BUF belongs in, or nil.
Nil when BUF belongs in the tab already showing it, and nil when no
tab's root contains it. Longest matching root wins, so a nested
worktree beats the repo containing it.

Only a tab in a project GROUP is ever a target. That excludes the
frame's original ungrouped tab -- the daemon's boot tab, or batch's own
-- which the post-open stamper still gives a root, and whose root is
typically a home directory containing everything: without this, a visit
to any file outside every open worktree would be dragged onto it.

Pure with respect to git: compares against roots already stamped on
tabs, never shelling out, so this is safe on the sweep's hot path."
  (let ((dir (edmacs-workspaces--buffer-dir buf)))
    (when dir
      (let ((number 0) (best nil) (best-length -1) (best-number nil))
        (dolist (tab (tab-bar-tabs (or frame (selected-frame))))
          (setq number (1+ number))
          (let ((root (edmacs-workspaces-tab-root tab)))
            (when (and root
                       (funcall tab-bar-tab-group-function tab)
                       (file-in-directory-p dir root)
                       (> (length root) best-length))
              (setq best tab best-length (length root) best-number number))))
        ;; `tab-bar-tabs' marks the selected tab's car `current-tab'.
        (and best-number (not (eq (car best) 'current-tab)) best-number)))))

(defun edmacs-workspaces--relocate-stray-visits (frame)
  "Move any buffer in FRAME shown in the wrong worktree's tab to its own tab.
Runs from the zero-delay timer `edmacs-workspaces--on-window-buffer-change'
schedules, never synchronously from `window-buffer-change-functions',
which fires mid-redisplay. Only ordinary (non-side, non-dedicated)
windows are considered.

Never creates or selects a frame: relocation is entirely within FRAME,
by selecting the target tab and displaying the buffer there."
  (when (and edmacs-workspaces-stray-visit-relocate
             (not edmacs-workspaces--relocating)
             (frame-live-p frame))
    (let ((edmacs-workspaces--relocating t))
      (with-selected-frame frame
        ;; Every candidate is collected before anything moves: selecting a
        ;; tab replaces the frame's windows wholesale, so a window object
        ;; found after the first relocation would belong to another tab.
        (let ((moves '()))
          (dolist (window (window-list frame 'never))
            (when (and (window-live-p window)
                       (not (window-parameter window 'window-side))
                       (not (window-dedicated-p window)))
              (let* ((buf (window-buffer window))
                     (target (edmacs-workspaces--stray-tab-number buf frame)))
                (when target
                  (push (cons window (cons buf target)) moves)))))
          (dolist (move (nreverse moves))
            (let ((window (car move)))
              (when (window-live-p window)
                (with-selected-window window
                  (switch-to-prev-buffer window)))))
          (dolist (move moves)
            (let ((buf (cadr move))
                  (target (cddr move)))
              (when (buffer-live-p buf)
                (tab-bar-select-tab target)
                (switch-to-buffer buf)))))))))

(defun edmacs-workspaces--on-window-buffer-change (frame)
  "Schedule a stray-visit sweep of FRAME off the redisplay path.
`window-buffer-change-functions' runs mid-redisplay, so no window, tab
or buffer work happens here directly -- only a zero-delay timer."
  (when (and edmacs-workspaces-stray-visit-relocate
             (not edmacs-workspaces--relocating))
    (run-at-time 0 nil #'edmacs-workspaces--relocate-stray-visits frame)))

(add-hook 'window-buffer-change-functions #'edmacs-workspaces--on-window-buffer-change)

(provide 'workspaces)
;;; workspaces.el ends here
