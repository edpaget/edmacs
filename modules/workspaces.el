;;; workspaces.el --- Project/worktree identity on tab-bar groups -*- lexical-binding: t -*-

;;; Commentary:
;; The new identity model for roadmap `edmacs-tab-groups': a project is a
;; tab-bar GROUP, a worktree is a TAB inside that group. This module is
;; the one place that answers "what project is this?" and "what worktree
;; is this?" -- nothing in `frames.el' is touched, so `SPC p p' keeps
;; working exactly as before while this lands (see the roadmap body's
;; phase 1 for why the two models coexist in separate files rather than
;; sharing one, which would turn phase 5's deletion into an unpick).
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
;; `file-notify' watch, no timer. This is the direct replacement for the
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
(require 'vc-git)

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
file's Commentary for why, and for the TRAMP caveat."
  (let* ((default-directory root)
         (mine (file-name-as-directory (file-truename root)))
         (others (mapcar (lambda (dir) (file-name-as-directory (expand-file-name dir)))
                          (vc-git-known-other-working-trees))))
    (seq-uniq (cons mine others))))

(provide 'workspaces)
;;; workspaces.el ends here
