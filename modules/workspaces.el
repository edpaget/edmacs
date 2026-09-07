;;; workspaces.el --- Project/worktree identity on tab-bar groups -*- lexical-binding: t -*-

;;; Commentary:
;; The new identity model for roadmap `edmacs-tab-groups': a project is a
;; tab-bar GROUP, a worktree is a TAB inside that group. This module is
;; the one place that answers "what project is this?" and "what worktree
;; is this?", and -- since phase 2 -- the one that drives `SPC p p'
;; (`edmacs-workspaces-open-project') and `SPC T p' / `C-x t p'
;; (`edmacs-workspaces-open-worktree'). It replaced a frame-per-repo
;; model (the retired `frames.el') outright rather than absorbing it,
;; which is why nothing below carries that model's parameters.
;;
;; ============================================================================
;; Load-time side effects: two hooks
;; ============================================================================
;; This module installs exactly two hook entries at load time:
;;
;; - `edmacs-workspaces--on-tab-post-open' on
;;   `tab-bar-tab-post-open-functions'. STAMP-ONLY. The frames model put
;;   a combined stamper/reconciler there; only the reconciler was the
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
;; not the frames model's own `edmacs-root'. This module is the sole
;; writer and reader of it. The two keys coexisted on the same tab
;; objects, with no interaction, for as long as both models were live;
;; reusing `edmacs-root' would have made them contend over one parameter
;; instead, and would have turned the frames model's removal into an
;; unpick. `edmacs-root' now survives only as legacy DATA in the desktop
;; migration below.
;;
;; ============================================================================
;; Desktop migration: the frames model's saved framesets
;; ============================================================================
;; `edmacs-workspaces-migrate-frameset' converts a frameset written by
;; the frames model -- one frame state per repo, each carrying
;; `edmacs-repo', with tabs carrying the model's `edmacs-root' -- into
;; a single frame state whose tabs carry native `group' parameters and
;; this module's `edmacs-workspace-root'. It is a pure data transform:
;; it reads nothing from the live session, mutates neither its input nor
;; any frame or tab, never signals, and is a fixed point on its own
;; output, so `sessions.el' can leave it in the daemon's boot path
;; permanently instead of gating it on a one-shot flag.
;;
;; It rests on two upstream facts (Emacs 31.1):
;;
;; - `frameset-filter-tabs' strips only `wc wc-point wc-bl wc-bbl
;;   wc-history-back wc-history-forward' when saving, so `group', `ws'
;;   and a custom root parameter all survive a desktop round trip.
;; - `tab-bar-select-tab' falls back to `window-state-put' on a tab's
;;   `ws' whenever its `wc' is not a live window configuration -- which
;;   is every restored tab. That is why a folded frame's whole window
;;   state becomes its tab's `ws', and it is also what carries bufferlo's
;;   per-tab buffer list across a restart, bufferlo reading
;;   `bufferlo-buffer-list' out of `ws' once desktop has stripped
;;   `wc-bl'/`wc-bbl'.
;;
;; It reads `edmacs-root'/`edmacs-repo' as legacy DATA only: they are the
;; shape a desktop file written by the old model has on disk, not
;; anything this session still writes.
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
;; the redisplay path, which schedules nothing recurring.) This replaced
;; the ~310-line discovery layer -- a worktree cache plus a debounced
;; `file-notify' watch -- the roadmap deleted outright, and it must not
;; grow one of its own. `vc-git-known-other-working-trees'
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
;; For `frameset-p'/`frameset-states'/`frameset-timestamp', which the desktop
;; migration below reads.
(require 'frameset)
;; For `project-current-directory-override', which `project-switch-project'
;; binds before dispatching to `edmacs-workspaces-open-project'.
(require 'project)
(require 'vc-git)

;; Autoloaded; called as a plain function from `--open-tab'.
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
See this file's Commentary for why it is not the frames model's
`edmacs-root'.")

(defun edmacs-workspaces--normalize-root (root)
  "Return ROOT as this module's canonical root string, or nil for a non-string.
The single normalizer: every root this module stamps, compares or hands
out goes through it. The truename-plus-trailing-slash form is not
cosmetic: `edmacs-workspaces-find-tab' matches with `equal', so a
migrated tab that is not normalized identically would never be found
and every reopen would duplicate it."
  (and (stringp root) (file-name-as-directory (file-truename root))))

(defun edmacs-workspaces-tab-root (tab)
  "Return TAB's worktree root, or nil.
A pure read: no derivation, no side effect."
  (alist-get edmacs-workspaces-root-parameter tab))

(defun edmacs-workspaces-set-tab-root (root &optional frame)
  "Stamp ROOT onto FRAME's (default selected) current tab. Returns ROOT.
`setf' rather than `push', so re-stamping REPLACES the entry: a
push-shadowed stale cons would survive `tab-bar--tab''s
copy-other-parameters forwarding on every later tab switch and outlive
the session via desktop."
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

(defun edmacs-workspaces-tab-number (tab &optional frame)
  "Return TAB's 1-based `tab-bar' index in FRAME, or nil when it is not there.
The one place this module's `(1+ (tab-bar--tab-index ...))' is written:
every `tab-bar' command that takes a tab takes this number, and each
consumer deriving it itself is a second answer to a question this module
owns."
  (let* ((target (or frame (selected-frame)))
         (index (tab-bar--tab-index tab (tab-bar-tabs target) target)))
    (and index (1+ index))))

(defun edmacs-workspaces-select-tab (group root &optional frame)
  "Find and select the tab of FRAME matching GROUP and ROOT.
Returns the tab, or nil when none matches -- selection is skipped
entirely in that case. `tab-bar-select-tab' has no FRAME argument of its
own (it always operates on the selected frame), so the call is wrapped
in `with-selected-frame' -- unconditionally, since wrapping the
already-selected frame is harmless."
  (let* ((target (or frame (selected-frame)))
         (tab (edmacs-workspaces-find-tab group root target)))
    (when tab
      (with-selected-frame target
        (tab-bar-select-tab (edmacs-workspaces-tab-number tab target))))
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
own (it always operates on the selected frame), so the call is wrapped
in `with-selected-frame' -- unconditionally, since wrapping the
already-selected frame is harmless."
  (with-selected-frame (or frame (selected-frame))
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
         (mine (edmacs-workspaces--normalize-root root)))
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

(defun edmacs-workspaces--worktree-root-of (dir)
  "Return the worktree root containing DIR, normalized, or nil.
DIR is whatever directory a buffer happens to sit in, so it is usually a
*subdirectory* of a worktree; stamping it verbatim would put a root like
`.../edmacs/modules/' on a tab. `edmacs-workspaces--stray-tab-number'
resolves a buffer to the tab with the LONGEST matching root, so such a
tab then captures every buffer under that subdirectory. `vc-git-root'
walks up for the `.git' entry -- stat-only, no subprocess -- and answers
a linked worktree with its OWN root rather than the main checkout, which
is what keeps sibling worktrees distinct. Remote directories are refused
outright: this runs from a core hook and must never touch the network."
  (and dir
       (not (file-remote-p dir))
       (when-let* ((root (ignore-errors (vc-git-root dir))))
         (edmacs-workspaces--normalize-root root))))

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
    (edmacs-workspaces--worktree-root-of dir)))

(defun edmacs-workspaces--on-tab-post-open (tab)
  "Stamp TAB's worktree root. This module's only post-open hook entry.
Stamp-only by design: no reconciliation, no tab closing, no group
assignment (`tab-bar-new-tab-group' is t, so TAB already inherited the
originating tab's group, and re-assigning here would re-enter
`tab-bar-tab-post-change-group-functions').

The guard is the frames model's, for the same reason: `tab-bar-tabs' also
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
;; Frames this config may drive
;; ============================================================================
;; One frame is the model, but the frame LIST is not one long: a daemon
;; also holds its initial tty placeholder, and a completion popup is a
;; child frame. Both must be declined rather than driven.

(defun edmacs-workspaces-gui-frame ()
  "Return this session's graphical frame, or nil when it holds none.
A daemon's frame list also holds its initial tty placeholder, which is
never graphical; the session is meant to hold exactly one graphical
frame, so the first match is the answer rather than an arbitrary pick."
  (seq-find (lambda (f) (and (frame-live-p f) (display-graphic-p f)))
            (frame-list)))

(defun edmacs-workspaces--graphical-session-p ()
  "Return non-nil when this session holds at least one graphical frame."
  (and (edmacs-workspaces-gui-frame) t))

(defun edmacs-workspaces-frame-usable-p (frame)
  "Return non-nil when FRAME is a frame this config may drive.
Excludes a child frame (a corfu-style popup, which must stay the size
its owner gave it) and the daemon's initial tty placeholder -- the frame
`desktop--check-dont-save' already refuses to save, which is never on
screen and must never be given a sidebar or have its tabs stamped.

A non-graphical frame counts as usable only while the session has no
graphical frame at all. That disjunct is what keeps the tty-only batch
test harnesses (and a genuinely terminal-only Emacs) working, while
still excluding the placeholder on the real daemon, which always holds a
GUI boot frame."
  (and (frame-live-p frame)
       (not (frame-parameter frame 'parent-frame))
       (not (and (daemonp) (frame-initial-p frame)))
       (or (display-graphic-p frame)
           (not (edmacs-workspaces--graphical-session-p)))))

;; ============================================================================
;; Stamping a restored frame's tabs
;; ============================================================================

(defun edmacs-workspaces--frame-content-window (frame)
  "Return the window FRAME shows its content in.
Whichever window carries windows.el's `edmacs-main' parameter, else
FRAME's first non-side window, else its selected window. Never
`selected-window': the caller can be acting on a frame that is not the
selected one, which is exactly the mix-up a stamped identity rules out."
  (or (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                (window-list frame 'no-minibuf))
      (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                (window-list frame 'no-minibuf))
      (frame-selected-window frame)))

(defun edmacs-workspaces--derive-frame-root (frame)
  "Return the worktree root FRAME's content window shows, normalized, or nil.
The frame-explicit counterpart of `edmacs-workspaces--derive-root',
which cannot take a frame because its caller is a core hook that
supplies none."
  (when-let* ((window (edmacs-workspaces--frame-content-window frame))
              (buffer (and (window-live-p window) (window-buffer window)))
              (dir (buffer-local-value 'default-directory buffer)))
    (edmacs-workspaces--worktree-root-of dir)))

(defun edmacs-workspaces-stamp-frame-tabs (frame)
  "Stamp every tab of FRAME that carries no worktree root yet.
A background tab has no live window to derive from, so each unstamped
tab is selected in turn, stamped, and the original selection restored.
This is the restore path for a tab that reaches the session without a
root -- one saved by a desktop written before the stamp existed, or one
whose window state named a directory no derivation had seen. Without it
such a tab reads nil forever and the sidebar can file it under no
project at all."
  (when (frame-live-p frame)
    (with-selected-frame frame
      (let ((original (tab-bar--current-tab-index))
            (count (length (tab-bar-tabs frame))))
        (unwind-protect
            (dotimes (i count)
              (unless (edmacs-workspaces-tab-root (nth i (tab-bar-tabs frame)))
                (tab-bar-select-tab (1+ i))
                (when-let* ((root (edmacs-workspaces--derive-frame-root frame)))
                  (edmacs-workspaces-set-tab-root root frame))))
          (when (and original (< original count))
            (tab-bar-select-tab (1+ original))))))))

;; ============================================================================
;; The current tab's identity
;; ============================================================================

(defun edmacs-workspaces-current-tab-root (&optional frame)
  "Return the worktree root stamped on FRAME's current tab, or nil.
FRAME defaults to the selected frame."
  (edmacs-workspaces-tab-root
   (edmacs-workspaces--current-tab (or frame (selected-frame)))))

(defun edmacs-workspaces-current-group (&optional frame)
  "Return the project group name of FRAME's current tab, or nil.
FRAME defaults to the selected frame. The group name is what
`edmacs-workspaces-group-name' derives for the tab's worktree, so this
is the active project's name -- nil on an ungrouped tab (the daemon's
boot tab, or batch's own)."
  (when-let* ((tab (edmacs-workspaces--current-tab (or frame (selected-frame)))))
    (funcall tab-bar-tab-group-function tab)))

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
  cleared itself -- the frames model's documented `excessive-lisp-nesting'.
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

(defun edmacs-workspaces-main-root (root)
  "Return ROOT's repo's main worktree as a normalized root directory.
Falls through to ROOT's own truename when the repo cannot be resolved,
matching `edmacs-workspaces-classify-root''s fall-through-not-signal
convention."
  (let* ((common (edmacs-git-common-dir root))
         (main (and common (edmacs-git-common-dir-main-worktree common))))
    (edmacs-workspaces--normalize-root (or main root))))

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
         (root (edmacs-workspaces--normalize-root dir))
         (group (and root (edmacs-workspaces-group-name root))))
    (unless root
      (user-error "No project directory to open"))
    (unless group
      (user-error "Not inside a git repository: %s" root))
    (let ((main (edmacs-workspaces-main-root root)))
      (or (edmacs-workspaces-select-tab group main)
          ;; Open the main-worktree tab even when the group already holds
          ;; linked-worktree tabs. Selecting a sibling instead leaves the
          ;; main checkout unreachable from `SPC p p' for as long as any
          ;; worktree tab is open, and sends the sidebar's project row --
          ;; which names the main worktree -- somewhere else.
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
  (let* ((root (edmacs-workspaces--normalize-root dir))
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
    (and dir
         ;; Refuse remote paths before `file-truename' touches them: this
         ;; runs from `window-buffer-change-functions' for every buffer in
         ;; every window, and a TRAMP round trip there stalls the sweep.
         (not (file-remote-p dir))
         (edmacs-workspaces--normalize-root dir))))

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

Cheap enough for the sweep's hot path: both operands are already
normalized truenames with a trailing slash, so containment is a string
test. `file-in-directory-p' would be the obvious call and is the wrong
one: it stats DIR and truenames BOTH arguments again (~169us against
~0.1us here), and it requires DIR to exist -- which would stop a deleted
worktree matching its own tab, the state the sidebar renders rather than
one it ignores."
  (let ((dir (edmacs-workspaces--buffer-dir buf)))
    (when dir
      (let ((number 0) (best nil) (best-length -1) (best-number nil))
        (dolist (tab (tab-bar-tabs (or frame (selected-frame))))
          (setq number (1+ number))
          (let ((root (edmacs-workspaces-tab-root tab)))
            (when (and root
                       (funcall tab-bar-tab-group-function tab)
                       (string-prefix-p root dir)
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
          ;; `nreverse' is destructive: reassign, or the second loop walks
          ;; only the last cons and every earlier move is dropped.
          (setq moves (nreverse moves))
          (dolist (move moves)
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

;; ============================================================================
;; Desktop migration -- frames model -> groups and tabs in one frame
;; ============================================================================

(defconst edmacs-workspaces--legacy-root-parameter 'edmacs-root
  "The retired frames model's own per-tab worktree-root parameter.
Read here as legacy DATA out of a saved frameset and never written: no
live tab in this session carries it. Keeping the name local to the
migration is what kept the frames model's removal a removal.")

(defun edmacs-workspaces--tab-root-of (params)
  "Return the worktree root recorded in tab alist PARAMS, or nil.
Prefers this module's own parameter (already normalized when it was
written) and falls back to the legacy one, normalizing that."
  (or (alist-get edmacs-workspaces-root-parameter params)
      (edmacs-workspaces--normalize-root
       (alist-get edmacs-workspaces--legacy-root-parameter params))))

(defun edmacs-workspaces--primary-state (states)
  "Return the frameset state of STATES whose frame survives the fold.
The focused frame when one is marked, else the first state: its
`current-tab' stays the selected tab and its geometry is the geometry
the single surviving frame keeps."
  (or (seq-find (lambda (state) (alist-get 'last-focus-update (car state))) states)
      (car states)))

(defun edmacs-workspaces--state-group (state)
  "Return the project group name for frameset STATE, or nil. Never signals.
Derived through `edmacs-workspaces-group-name' on one of the state's own
tab roots, so the string is `equal' to what the runtime open paths
compute -- anything else would silently duplicate a group the first time
a migrated project is reopened. Falls back to the pure repo-name of the
legacy `edmacs-repo' frame parameter, which keeps a tab whose worktree
directory is gone inside its project rather than dropping it."
  (let* ((params (car state))
         (root (seq-some (lambda (tab) (edmacs-workspaces--tab-root-of (cdr tab)))
                         (alist-get 'tabs params))))
    (or (and root (ignore-errors (edmacs-workspaces-group-name root)))
        (let ((repo (alist-get 'edmacs-repo params)))
          (and (stringp repo)
               (ignore-errors (edmacs-git-common-dir-repo-name repo)))))))

(defun edmacs-workspaces--state-group-if-needed (state)
  "Return STATE's group name only when some tab of STATE still needs one.
Deriving a group reaches git, and this migration runs on every daemon
boot, not once: a frameset already carrying groups must cost nothing."
  (let ((tabs (alist-get 'tabs (car state))))
    (when (or (null tabs)
              (seq-some (lambda (tab) (null (alist-get 'group (cdr tab)))) tabs))
      (edmacs-workspaces--state-group state))))

(defun edmacs-workspaces--migrate-tab (tab group)
  "Return a fresh copy of TAB carrying a `group' and this module's root.
Ensures rather than re-derives: an existing non-nil `group' or
`edmacs-workspace-root' is kept untouched -- and nothing then reaches
git at all -- so this is a fixed point on its own output. The legacy
root parameter is dropped: a migrated tab the new model cannot read is
the whole failure this rename guards against. TAB's car (`tab' vs
`current-tab') is preserved.

A missing group is derived from TAB's OWN root first, and only then from
GROUP, its frame's. The frames model let a tab sit in a frame belonging
to another repo (`sessions.el''s tab namer has a case for exactly that),
and a frame-wide group would file such a tab under the wrong project."
  (let* ((params (cdr tab))
         (had-group (alist-get 'group params))
         (had-root (alist-get edmacs-workspaces-root-parameter params))
         (root (or had-root
                   (edmacs-workspaces--normalize-root
                    (alist-get edmacs-workspaces--legacy-root-parameter params))))
         (group (or had-group
                    (and root (ignore-errors (edmacs-workspaces-group-name root)))
                    group))
         ;; The legacy root always goes; a nil-valued `group'/root
         ;; placeholder goes too, re-appended below with a real value or
         ;; not at all. An entry that already holds a value is kept where
         ;; it is and never re-appended -- appending a second cons would
         ;; shadow nothing but would make this transform grow its output
         ;; on every pass instead of being a fixed point.
         (drop (delq nil (list edmacs-workspaces--legacy-root-parameter
                               (and (null had-group) 'group)
                               (and (null had-root) edmacs-workspaces-root-parameter))))
         (kept (seq-remove (lambda (cell) (memq (car-safe cell) drop)) params)))
    (cons (car tab)
          (append (mapcar (lambda (cell) (cons (car cell) (cdr cell))) kept)
                  (and (null had-group) group (list (cons 'group group)))
                  (and (null had-root) root
                       (list (cons edmacs-workspaces-root-parameter root)))))))

(defun edmacs-workspaces--fold-state (state group time)
  "Return the tabs a non-primary frameset STATE contributes to the fold.
STATE's `current-tab' becomes an ordinary `tab' carrying the frame's
whole window state as its `ws': a `current-tab' has none by construction
\(the frame's own window state is its layout), and `tab-bar-select-tab'
falls back to `window-state-put' on `ws' exactly when the saved `wc' is
not a live window configuration -- which is every restored tab. That
same `ws' is what carries bufferlo's per-tab buffer list across the
restart, since desktop strips `wc-bl'/`wc-bbl' on save and bufferlo
falls back to the `bufferlo-buffer-list' entry inside `ws'.

A state carrying no `tabs' parameter at all contributes one synthesized
tab rather than losing the frame's layout. TIME is the stamp a folded
`current-tab' gains; it is derived from the frameset's own timestamp so
that migrating twice yields `equal' results."
  (let* ((params (car state))
         (window-state (cdr state))
         (tabs (alist-get 'tabs params)))
    (if (null tabs)
        (list (edmacs-workspaces--migrate-tab
               `(tab (name . ,(or (alist-get 'name params) "tab"))
                     (time . ,time)
                     (ws . ,window-state))
               group))
      (mapcar
       (lambda (tab)
         (let ((migrated (edmacs-workspaces--migrate-tab tab group)))
           (if (eq (car tab) 'current-tab)
               (cons 'tab
                     (append (seq-remove (lambda (cell) (memq (car-safe cell) '(ws time)))
                                         (cdr migrated))
                             (list (cons 'time time) (cons 'ws window-state))))
             migrated)))
       tabs))))

(defun edmacs-workspaces--sort-tabs-by-group (tabs)
  "Return TABS stably reordered so each group's tabs are contiguous.
Groups keep first-seen order. `tab-bar-move-tab-to-group' -- which keeps
a group contiguous for a live frame -- is not running while a frameset
is being assembled, so contiguity has to be built in here."
  (let ((order '()) (next 0))
    (dolist (tab tabs)
      (let ((group (alist-get 'group (cdr tab))))
        (unless (assoc group order)
          (push (cons group next) order)
          (setq next (1+ next)))))
    (sort (copy-sequence tabs)
          (lambda (a b)
            (< (cdr (assoc (alist-get 'group (cdr a)) order))
               (cdr (assoc (alist-get 'group (cdr b)) order)))))))

(defun edmacs-workspaces-migrate-frameset (fs)
  "Return FS converted from the frames model to groups and tabs in one frame.
A pure data transform: FS is never mutated, no frame or tab object is
touched, nothing is read from the live session, and the result is a
fixed point -- migrating it again returns an `equal' frameset. That is
what makes this safe to leave permanently in the daemon's boot path
rather than gating it on a one-shot flag: it ensures rather than
detects, so it cannot re-fire on its own output.

Every frame state folds into the one `edmacs-workspaces--primary-state'
picks: each other frame's tabs join it, its `current-tab' demoted to an
ordinary tab carrying that frame's window state as its `ws'. Tabs gain
the project `group' their frame's `edmacs-repo'/root implies and this
module's `edmacs-workspace-root' in place of the old `edmacs-root';
`edmacs-repo'/`edmacs-repo-missing' are dropped from the surviving
frame, which now holds several projects and can no longer name one.

A frameset that is not one, or that carries no states at all, is
returned untouched -- the empty-frameset guard on the restore side must
keep seeing exactly what it sees today."
  (if (or (not (frameset-p fs)) (null (frameset-states fs)))
      fs
    (let* ((states (frameset-states fs))
           (primary (edmacs-workspaces--primary-state states))
           (time (float-time (frameset-timestamp fs)))
           (tabs '())
           (seen '()))
      (dolist (tab (alist-get 'tabs (car primary)))
        (let ((migrated (edmacs-workspaces--migrate-tab
                         tab (edmacs-workspaces--state-group-if-needed primary))))
          (push migrated tabs)
          (push (cons (alist-get 'group (cdr migrated))
                      (alist-get edmacs-workspaces-root-parameter (cdr migrated)))
                seen)))
      (dolist (state states)
        (unless (eq state primary)
          (dolist (tab (edmacs-workspaces--fold-state
                        state (edmacs-workspaces--state-group-if-needed state) time))
            (let ((key (cons (alist-get 'group (cdr tab))
                             (alist-get edmacs-workspaces-root-parameter (cdr tab)))))
              ;; Dedupe only on a real (group, root) pair: two rootless tabs
              ;; are not evidence of the same worktree, and AC2 loses nothing.
              (unless (and (cdr key) (member key seen))
                (push key seen)
                (push tab tabs))))))
      (setq tabs (edmacs-workspaces--sort-tabs-by-group (nreverse tabs)))
      ;; `tab-bar--current-tab-find' is a bare `(assq 'current-tab tabs)', so
      ;; a fold that produced none would leave the frame with no selected tab.
      (when (and tabs (not (assq 'current-tab tabs)))
        (setcar (car tabs) 'current-tab))
      ;; The `tabs' entry is rewritten in place rather than dropped and
      ;; re-appended, so a frameset already in the new shape comes back
      ;; `equal' to its input rather than merely equivalent to it.
      (let* ((had-tabs nil)
             (params (delq nil
                           (mapcar
                            (lambda (cell)
                              (cond
                               ((memq (car-safe cell) '(edmacs-repo edmacs-repo-missing))
                                nil)
                               ((eq (car-safe cell) 'tabs)
                                (setq had-tabs t)
                                (and tabs (cons 'tabs tabs)))
                               (t (cons (car cell) (cdr cell)))))
                            (car primary))))
             (new-fs (copy-sequence fs)))
        (when (and tabs (not had-tabs))
          (setq params (append params (list (cons 'tabs tabs)))))
        (setf (frameset-states new-fs) (list (cons params (cdr primary))))
        new-fs))))

;; ============================================================================
;; Closing the last tab
;; ============================================================================
;; `tab-bar-close-last-tab-choice' is deliberately left at core's nil, so
;; closing the sole tab signals instead of doing something. Under the
;; macOS daemon deleting the last GUI frame drops Emacs out of the Dock;
;; `edmacs-ns-close-frame' (sessions.el) is the sanctioned way out.

;; ============================================================================
;; Fullscreen -- every graphical frame opens fullscreen
;; ============================================================================

;; The load-bearing knob for "one Space per frame": with it nil the NS port
;; fakes fullscreen by resizing the window inside the current Space instead.
;; Already the default; pinned so the contract lives in source.
(defvar ns-use-native-fullscreen)
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t))

(defgroup edmacs-workspaces nil
  "Projects as tab groups, worktrees as tabs."
  :group 'convenience)

(defcustom edmacs-workspaces-fullscreen 'fullboth
  "The `fullscreen' frame parameter every new graphical frame is given.
Nil disables the policy. `fullboth' is the portable value: the NS port
maps it to native macOS fullscreen, the X11/PGTK ports to EWMH's
`_NET_WM_STATE_FULLSCREEN'."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Fullscreen" fullboth)
                 (const :tag "Maximized" maximized)
                 (const :tag "Full width" fullwidth)
                 (const :tag "Full height" fullheight))
  :group 'edmacs-workspaces)

(defun edmacs-workspaces--fullscreen-target (frame)
  "Return the `fullscreen' value FRAME still needs, or nil.
Nil when the policy is off, when FRAME already carries that value, or
when FRAME is one this policy must not touch:

  - a non-graphical frame -- the daemon's own tty placeholder and every
    `emacsclient -t' frame, where `fullscreen' means nothing and is
    mangled by frameset's tty shelving on the way into a desktop file.
    This gate is also why the policy is a hook rather than an entry in
    `default-frame-alist', which those frames read too.
  - a child frame -- a completion popup (corfu's, which already binds
    `after-make-frame-functions' to nil, but posframe-style packages
    generally do not) is a frame by construction and must stay the size
    its owner gave it."
  (and edmacs-workspaces-fullscreen
       (frame-live-p frame)
       (display-graphic-p frame)
       (not (frame-parameter frame 'parent-frame))
       (not (eq (frame-parameter frame 'fullscreen) edmacs-workspaces-fullscreen))
       edmacs-workspaces-fullscreen))

(defun edmacs-workspaces-apply-fullscreen (frame)
  "Put FRAME fullscreen per `edmacs-workspaces-fullscreen'.
Deferred to a zero-delay timer that re-checks the target, for the same
reason `edmacs-sessions--restore-pending-frameset' defers its own work
\(sessions.el): a frame is not fully mapped while its own creation hook
is still running, and the NS port drops a fullscreen toggle sent to an
unmapped window. Never signals -- an error reaching a frameless
daemon's top level exits it 255 (see core.el)."
  (when (edmacs-workspaces--fullscreen-target frame)
    (run-at-time
     0 nil
     (lambda ()
       (condition-case err
           (when-let* ((target (edmacs-workspaces--fullscreen-target frame)))
             (set-frame-parameter frame 'fullscreen target))
         (error
          (display-warning 'edmacs-workspaces
                           (format "could not fullscreen frame: %s" err)
                           :warning)))))))

(add-hook 'after-make-frame-functions #'edmacs-workspaces-apply-fullscreen)

(defun edmacs-workspaces--apply-fullscreen-at-startup ()
  "Apply the fullscreen policy to every already-live graphical frame.
`after-make-frame-functions' never fires for a non-daemon Emacs's
initial frame, which on a plain `emacs' start is the only frame there
is; under the daemon this finds nothing and the hook above covers the
boot frame instead."
  (dolist (frame (frame-list))
    (edmacs-workspaces-apply-fullscreen frame)))

(add-hook 'emacs-startup-hook #'edmacs-workspaces--apply-fullscreen-at-startup)

(provide 'workspaces)
;;; workspaces.el ends here
