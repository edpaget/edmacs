;;; workspaces.el --- Project/worktree identity on tab-bar groups -*- lexical-binding: t -*-

;;; Commentary:
;; Project/worktree identity for this config: a project is a `tab-bar'
;; GROUP, a worktree is a TAB in that group, and there is one frame. This
;; module is the only place that answers "what project is this?" and
;; "what worktree is this?" -- every other module asks it rather than
;; deriving an answer of its own.
;;
;; A tab's worktree ROOT is its whole stored identity. Its group is
;; derived from that root by `edmacs-workspaces-tab-group', installed as
;; `tab-bar-tab-group-function', and its name likewise by sessions.el's
;; `tab-bar-tab-name-function' -- so the two cannot disagree with the
;; root or with each other, and `edmacs-workspaces-find-tab' is keyed on
;; the root alone. Nothing here writes a tab's `group' alist entry.
;;
;; It installs two hooks: the config's SOLE entry on
;; `tab-bar-tab-post-open-functions', which identifies a new tab made by
;; any route (`SPC T n', a package's `other-tab-prefix') -- stamping its
;; root, designating its main window, then running
;; `edmacs-workspaces-tab-post-open-functions' for everyone else, in that
;; order; and a `window-buffer-change-functions' entry that schedules the
;; stray-visit sweep off the redisplay path.
;;
;; Two upstream facts (Emacs 31.1) the desktop migration rests on, neither
;; derivable from the code here:
;;
;; - `frameset-filter-tabs' strips only `wc wc-point wc-bl wc-bbl
;;   wc-history-back wc-history-forward' on save, so `group', `ws' and a
;;   custom root parameter all survive a desktop round trip.
;; - `tab-bar-select-tab' falls back to `window-state-put' on a tab's `ws'
;;   whenever its `wc' is not a live window configuration -- which is every
;;   restored tab. That is what makes a folded frame's window state its
;;   tab's `ws', and what carries bufferlo's per-tab buffer list across a
;;   restart once desktop has stripped `wc-bl'/`wc-bbl'.
;; - `tab-bar-move-tab-to-group' and `tab-bar-change-tab-group' both read
;;   and write the raw `group' alist entry rather than going through
;;   `tab-bar-tab-group-function'. With the entry unwritten, core's
;;   relocation sees every tab as ungrouped and stops keeping a project's
;;   tabs contiguous -- hence `edmacs-workspaces-move-tab-to-group'.
;;
;; That same `ws' is also what a restored background tab's root is derived
;; FROM (`edmacs-workspaces--root-from-ws'), which is why
;; `edmacs-workspaces-stamp-frame-tabs' never selects a tab.
;;
;; One hazard: group names come from `edmacs-git-common-dir-repo-name',
;; which returns only the final component of the resolved main worktree.
;; An ancestor symlink (/var -> /private/var) is harmless, but a symlink
;; that IS the worktree directory -- the `~/.config/emacs' ->
;; `~/Projects/edmacs' case -- yields a different final component per
;; alias, so the same repo can produce two group names. Tracked by task
;; `git-common-dir-symlink-mismatch', which owns the fix in
;; `edmacs-git-common-dir-1'.

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
(declare-function edmacs-windows-main-window-of "windows")
(declare-function edmacs-windows-designate-main "windows")
(declare-function edmacs-windows-ws-main-buffer-names "windows")
(declare-function edmacs-git-common-dir "git-common-dir")
(declare-function edmacs-git-common-dir-main-worktree "git-common-dir")
(declare-function edmacs-git-common-dir-repo-name "git-common-dir")

;; ============================================================================
;; Group name derivation
;; ============================================================================

(defun edmacs-workspaces--group-name-from-path (root)
  "Return ROOT's project group name derived from its PATH alone, or nil.
Pure string work with one `file-directory-p' guard, and the reason a tab
whose worktree directory has been deleted stays inside its project
rather than dropping out of the sidebar tree entirely: git resolution
answers nil for a gone directory, and the tab's stamp IS its identity
\(see `edmacs-sessions--finish-frameset-restore').

Two shapes, in order: rdm's `<repo>__worktrees/<slug>' layout, whose
parent basename names the repo; then ROOT's own leaf basename, but ONLY
for a directory that is not there. A LIVE directory that git could not
resolve is a genuine non-repo -- the daemon's boot tab is stamped with
`~/' -- and must stay ungrouped, or it renders as a phantom project.
Remote roots are refused before the stat: this runs from the group
function's hot path."
  (when (stringp root)
    (pcase-let ((`(,leaf . ,parent) (edmacs-workspaces--leaf-and-parent root)))
      (cond
       ((and parent (string-suffix-p "__worktrees" parent))
        (let ((name (string-remove-suffix "__worktrees" parent)))
          (and (> (length name) 0) name)))
       ((and leaf (> (length leaf) 0)
             (not (file-remote-p root))
             (not (file-directory-p root)))
        leaf)))))

(defun edmacs-workspaces-group-name (root)
  "Return ROOT's project tab-bar group name, or nil when ROOT has none.
Reuses `edmacs-git-common-dir'/`edmacs-git-common-dir-repo-name' rather
than re-deriving a repo name from ROOT's own path directly -- see this
file's Commentary on the symlink dependency those two functions carry --
and falls back to `edmacs-workspaces--group-name-from-path' only when
that resolution answers nothing at all."
  (or (when-let* ((common (edmacs-git-common-dir root)))
        (edmacs-git-common-dir-repo-name common))
      (edmacs-workspaces--group-name-from-path root)))

(defvar edmacs-workspaces--group-memo (make-hash-table :test #'equal)
  "ROOT -> its `edmacs-workspaces-group-name', or the symbol `none'.
A pure root-to-name memo, nothing like the retired worktree-discovery
layer: it installs no watch and no timer, holds no directory listing,
and every entry is a function of its key alone. It exists because
`tab-bar-tab-group-function' is called for every tab on every redisplay
and every sidebar plan, and the git resolution behind a group name is a
`file-truename' walk. `none' rather than nil is stored for an ungrouped
root so a negative answer is memoized too.

Cleared by `edmacs-workspaces-clear-group-memo'; a test that stubs git
resolution MUST call it on entry and on exit, or one test's stubbed
answer is served to the next.")

(defun edmacs-workspaces-clear-group-memo ()
  "Forget every memoized group name. Returns nil."
  (clrhash edmacs-workspaces--group-memo)
  nil)

(defun edmacs-workspaces--group-name-memoized (root)
  "Return ROOT's group name, deriving it at most once per ROOT."
  (let ((hit (gethash root edmacs-workspaces--group-memo 'miss)))
    (if (eq hit 'miss)
        (let ((name (ignore-errors (edmacs-workspaces-group-name root))))
          (puthash root (or name 'none) edmacs-workspaces--group-memo)
          name)
      (and (not (eq hit 'none)) hit))))

(defun edmacs-workspaces-tab-group (tab)
  "Return TAB's project group name, derived from TAB's worktree root.
Installed as `tab-bar-tab-group-function', which makes the root a tab's
single stored identity and the group a pure function of it. TAB's own
`group' alist entry is no longer read by anything in this config, and
nothing here writes one; core's `tab-bar-change-tab-group' (an
interactive `M-x tab-group') still writes that entry, and it is simply
inert -- the derived name wins.

Two things fall out of this. A tab whose stored group disagreed with its
root -- the state `SPC T n' produced, since `tab-bar-new-tab-group' made
a new tab inherit the ORIGINATING tab's group regardless of the root it
was then stamped with -- cannot exist any more. And core's own
`tab-bar-move-tab-to-group' stops working, because it reads the raw
alist rather than this function: `edmacs-workspaces-move-tab-to-group'
is the replacement."
  (when-let* ((root (edmacs-workspaces-tab-root tab)))
    (edmacs-workspaces--group-name-memoized root)))

(setq tab-bar-tab-group-function #'edmacs-workspaces-tab-group)

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

(defun edmacs-workspaces--tab-bar-current-tab (&optional frame)
  "Return FRAME's current tab, or nil.
Wraps tab-bar.el's internal `tab-bar--current-tab-find', a bare
`(assq \='current-tab (tab-bar-tabs FRAME))'. Checked against Emacs 30;
this is the one call site, so a core rename is a one-line fix here.
Distinct from `edmacs-workspaces--current-tab', which reads the `tabs'
frame parameter directly precisely so it cannot create a tab -- go
through that one from anywhere a `tab-bar-tabs' call would re-enter the
post-open hook."
  (tab-bar--current-tab-find nil frame))

(defun edmacs-workspaces-tab-root (tab)
  "Return TAB's worktree root, or nil.
A pure read: no derivation, no side effect."
  (alist-get edmacs-workspaces-root-parameter tab))

(defvar edmacs-workspaces-tab-root-set-functions nil
  "Abnormal hook run with (ROOT FRAME) at the end of
`edmacs-workspaces-set-tab-root', after the stamp lands. workspaces.el
loads before sidebar.el (see init.el's `load-module' order) and has no
sidebar.el function to call directly without an upward, layering-
breaking reference -- this is the same swappable-seam convention
sidebar.el's own `edmacs-sidebar-worktree-section-functions' et al. use,
just owned by this file instead. sidebar.el adds a member that
invalidates FRAME's sidebar, since a project row's label reflects its
main root and a worktree row its own -- a root stamp with no open
buffer-list activity otherwise had no trigger to redraw either.")

(defun edmacs-workspaces-set-tab-root (root &optional frame)
  "Stamp ROOT onto FRAME's (default selected) current tab. Returns ROOT.
`setf' rather than `push', so re-stamping REPLACES the entry: a
push-shadowed stale cons would survive `tab-bar--tab''s
copy-other-parameters forwarding on every later tab switch and outlive
the session via desktop."
  (when-let* ((tab (edmacs-workspaces--tab-bar-current-tab frame)))
    (setf (alist-get edmacs-workspaces-root-parameter (cdr tab)) root)
    (run-hook-with-args 'edmacs-workspaces-tab-root-set-functions root frame)
    root))

(defun edmacs-workspaces-find-tab (root &optional frame)
  "Return the tab of FRAME (default selected) whose worktree root is ROOT.
Nil when FRAME has no such tab. Keyed on ROOT alone: since
`edmacs-workspaces-tab-group' derives a tab's group FROM its root, a
group test here is either redundant (it re-derives what ROOT already
determines) or wrong (it reads a stale stored `group' and hides the very
tab it was asked for, so the caller opens a second one)."
  (seq-find (lambda (tab) (equal (edmacs-workspaces-tab-root tab) root))
            (tab-bar-tabs (or frame (selected-frame)))))

(defun edmacs-workspaces-tab-number (tab &optional frame)
  "Return TAB's 1-based `tab-bar' index in FRAME, or nil when it is not there.
The one place this config wraps tab-bar.el's internal
`tab-bar--tab-index' (checked against Emacs 30): every `tab-bar' command
that takes a tab takes this number, and each consumer deriving it itself
is both a second answer to a question this module owns and a second
place a core change would have to be chased to."
  (let* ((target (or frame (selected-frame)))
         (index (tab-bar--tab-index tab (tab-bar-tabs target) target)))
    (and index (1+ index))))

(defun edmacs-workspaces-select-tab (root &optional frame)
  "Find and select the tab of FRAME whose worktree root is ROOT.
Returns the tab, or nil when none matches -- selection is skipped
entirely in that case. Root-keyed for the reason
`edmacs-workspaces-find-tab' gives. `tab-bar-select-tab' has no FRAME
argument of its own (it always operates on the selected frame), so the
call is wrapped in `with-selected-frame' -- unconditionally, since
wrapping the already-selected frame is harmless."
  (let* ((target (or frame (selected-frame)))
         (tab (edmacs-workspaces-find-tab root target)))
    (when tab
      (with-selected-frame target
        (tab-bar-select-tab (edmacs-workspaces-tab-number tab target))))
    tab))

;; ============================================================================
;; Group contiguity -- keeping a project's tabs adjacent
;; ============================================================================

(defun edmacs-workspaces-move-tab-to-group (&optional tab frame)
  "Relocate TAB (default FRAME's current tab) next to its own group's tabs.
Core's `tab-bar-move-tab-to-group' is the algorithm, reimplemented here
for one reason: it reads a tab's group as `(alist-get \='group tab)',
not through `tab-bar-tab-group-function'. Now that nothing writes the
raw entry (`edmacs-workspaces-tab-group' derives it), core's version
sees every tab as ungrouped and stops relocating anything -- a new
project's tab would simply be appended at the end of the bar, splitting
whichever group already sat there.

A tab whose group is new to FRAME goes to the end; one whose group is
already present moves to that group's near edge; one already inside its
group's bounds is left where it is."
  (with-selected-frame (or frame (selected-frame))
    (let* ((tabs (funcall tab-bar-tabs-function))
           (tab (or tab (tab-bar--current-tab-find tabs)))
           (tab-index (tab-bar--tab-index tab tabs))
           (group (funcall tab-bar-tab-group-function tab))
           (beg (and group
                     (seq-position tabs group
                                   (lambda (other g)
                                     (and (not (eq other tab))
                                          (equal (funcall tab-bar-tab-group-function other)
                                                 g))))))
           (len (when beg
                  (seq-position (nthcdr beg tabs) group
                                (lambda (other g)
                                  (not (equal (funcall tab-bar-tab-group-function other)
                                              g))))))
           (pos (cond
                 ((null tab-index) nil)
                 ((null beg) (and group -1))
                 ((and len (>= tab-index beg) (<= tab-index (+ beg len))) nil)
                 ((and len (> tab-index (+ beg len))) (+ beg len 1))
                 ((< tab-index beg) beg))))
      (when pos
        (tab-bar-move-tab-to pos (1+ tab-index))))))

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

(defvar edmacs-workspaces-tab-post-open-functions nil
  "Abnormal hook run with (TAB FRAME) once a new tab is fully identified.
The seam that makes this module the SOLE entry on core's
`tab-bar-tab-post-open-functions'. Before any member runs, TAB carries
its worktree root and FRAME carries a designated main window -- that
ordering guarantee is the whole reason three independent `add-hook's on
the core variable collapsed into one owner. Their relative order there
was an accident of `add-hook' prepending, and the sidebar's redraw won
it: the first thing drawn for a new tab saw an unstamped tab and filed
it under no project.

Same swappable-seam convention as
`edmacs-workspaces-tab-root-set-functions': workspaces.el loads before
sidebar.el (see init.el's `load-module' order) and has no sidebar.el
function to call directly without an upward reference, so sidebar.el
registers here. windows.el loads FIRST, so its own work is a direct
call rather than a hook member.")

(defun edmacs-workspaces--on-tab-post-open (tab)
  "Identify a brand-new TAB: stamp its root, designate main, run the seam.
This config's only `tab-bar-tab-post-open-functions' entry, and the
order is the contract `edmacs-workspaces-tab-post-open-functions'
documents. Still no reconciliation, no tab closing and no group write --
a tab's group is derived from the root stamped here
\(`edmacs-workspaces-tab-group').

The guard is the frames model's, and every step belongs inside it:
`tab-bar-tabs' also runs this hook for a default tab it auto-creates on
a frame it never names, and stamping that one from the selected frame's
buffer would reintroduce cross-frame derivation. Designation and the
seam run whether or not a root was derivable -- a fresh tab has one
window and no sidebar either way."
  ;; `tab-bar-tab-post-open-functions' calls with (TAB), no frame slot --
  ;; ambient-reads: ok
  (let ((frame (selected-frame)))
    (when (eq tab (edmacs-workspaces--current-tab frame))
      (when-let* ((root (or edmacs-workspaces--pending-tab-root
                            (edmacs-workspaces--derive-root))))
        ;; `setf', never `push': see `edmacs-workspaces-set-tab-root'.
        (setf (alist-get edmacs-workspaces-root-parameter (cdr tab)) root))
      (edmacs-windows-designate-main frame)
      (run-hook-with-args 'edmacs-workspaces-tab-post-open-functions tab frame))))

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
Whichever window windows.el reports as FRAME's main, else FRAME's first
non-side window, else its selected window -- the two fallbacks cover a
tab stamped before the `edmacs-main' convention existed. Never
`selected-window': the caller can be acting on a frame that is not the
selected one, which is exactly the mix-up a stamped identity rules out."
  (or (edmacs-windows-main-window-of frame)
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

(defun edmacs-workspaces--buffer-name-directory (name)
  "Return the directory of the buffer called NAME, or nil.
The live buffer's own file directory (or its `default-directory') when
one exists; otherwise the file `desktop' is going to restore that buffer
from, read out of `desktop-buffer-args-list'.

That second path is not a nicety. sessions.el sets
`desktop-restore-eager' to 10, so at `desktop-after-read-hook' time --
when a restored frame's tabs are stamped -- most of the session's
buffers have not been created yet; a `get-buffer'-only lookup would
answer nil for nearly every background tab and the whole ws derivation
would come back empty. No `require' of desktop: this must stay callable
with desktop.el never loaded, which `bound-and-true-p' gives."
  (when (stringp name)
    (or (when-let* ((buf (get-buffer name)))
          (if-let* ((file (buffer-file-name buf)))
              (file-name-directory file)
            (buffer-local-value 'default-directory buf)))
        ;; Each entry is `desktop-append-buffer-args's own argument list:
        ;; (FILE-VERSION BUFFER-FILE-NAME BUFFER-NAME MAJOR-MODE ...), the
        ;; leading integer included -- so the name is at 2, the file at 1.
        (when-let* ((entry (seq-find (lambda (args) (equal (nth 2 args) name))
                                     (bound-and-true-p desktop-buffer-args-list)))
                    (file (nth 1 entry))
                    ((stringp file)))
          (file-name-directory
           (expand-file-name file (bound-and-true-p desktop-dirname)))))))

(defun edmacs-workspaces--root-from-ws (ws)
  "Return the worktree root a tab's serialized WS shows, normalized, or nil.
WS is the tab's `ws' field verbatim: `window-state-get's raw
\(CONSTRAINTS-ALIST . STATE-TREE) cons, so the tree handed on is its
`cdr'. Passing the whole cons instead would fall through
`edmacs-windows-ws-main-buffer-names' pattern match and read as \"no
root derivable\" rather than as an error.

This is what lets a background tab be stamped without ever being
selected: its main leaf's buffer names, most-specific first, each
resolved to a directory and then to the worktree containing it. Fails
soft everywhere -- a tab whose buffers name nothing resolvable simply
keeps no root, because this runs under `desktop-after-read-hook', where
a signal reaching a frameless daemon's top level exits it 255."
  (when (consp ws)
    (seq-some (lambda (name)
                (when-let* ((dir (edmacs-workspaces--buffer-name-directory name)))
                  (edmacs-workspaces--worktree-root-of dir)))
              (ignore-errors (edmacs-windows-ws-main-buffer-names (cdr ws))))))

(defun edmacs-workspaces--stamp-tab (tab root)
  "Stamp ROOT onto TAB without selecting it. Returns ROOT.
Takes no frame: a tab's parameter alist is the tab object itself, so
there is no frame-relative addressing left to get wrong here.
The frame-and-tab-explicit writer `edmacs-workspaces-set-tab-root' is
not: that one stamps whichever tab is CURRENT, which is exactly the
constraint the restore walk no longer accepts. `setf', never `push',
for the reason `edmacs-workspaces-set-tab-root' documents."
  (setf (alist-get edmacs-workspaces-root-parameter (cdr tab)) root)
  root)

(defun edmacs-workspaces-stamp-frame-tabs (frame)
  "Stamp every tab of FRAME that carries no worktree root yet.
No tab is ever selected. FRAME's CURRENT tab derives its root from the
live window it is showing; every background tab derives its own from the
serialized layout it already carries (`edmacs-workspaces--root-from-ws'),
which is the same information selecting it would have put on screen.

The select loop this replaced ran two tab-select repair advices, a
sidebar redraw and a stray-sweep timer per tab, on a path that runs on
every daemon boot. `edmacs-workspaces-tab-root-set-functions' fires once
per frame after the walk, not once per tab, for the same reason.

This is the restore path for a tab that reaches the session without a
root -- one saved by a desktop written before the stamp existed, or one
whose window state named a directory no derivation had seen. Without it
such a tab reads nil forever and the sidebar can file it under no
project at all."
  (when (frame-live-p frame)
    (let ((current (edmacs-workspaces--current-tab frame))
          (stamped nil))
      (dolist (tab (tab-bar-tabs frame))
        (unless (edmacs-workspaces-tab-root tab)
          (when-let* ((root (if (eq tab current)
                                (edmacs-workspaces--derive-frame-root frame)
                              (edmacs-workspaces--root-from-ws
                               (alist-get 'ws tab)))))
            (edmacs-workspaces--stamp-tab tab root)
            (setq stamped root))))
      (when stamped
        (run-hook-with-args 'edmacs-workspaces-tab-root-set-functions stamped frame))
      stamped)))

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

(defun edmacs-workspaces--open-tab (root _group)
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
- `edmacs-workspaces-move-tab-to-group' runs LAST, because it reorders
  the tab list. Every earlier step therefore addresses the tab as
  \"current\" rather than by a captured tab-number; the moved tab stays
  selected, so running the relocation last is safe.

GROUP is not written anywhere: the new tab's group is derived from the
root stamped on it (`edmacs-workspaces-tab-group'). GROUP is still taken
as an argument because the relocation and the caller's own naming both
speak in terms of it."
  (let ((display-buffer-overriding-action '(nil . nil))
        (switch-to-buffer-obey-display-actions nil))
    ;; The new tab opens directly on ROOT's dired buffer. With the default
    ;; `tab-bar-new-tab-choice' of t it would open on whatever buffer was
    ;; current -- the PREVIOUS project's -- and `bufferlo' records a tab's
    ;; buffers as they are displayed, so that buffer joins this tab's local
    ;; list for good. The sidebar then renders it under this project as a
    ;; `../other-project' row. `tab-bar-new-tab-choice' is applied before
    ;; `tab-bar-tab-post-open-functions', so the stamper still sees ROOT.
    (let ((edmacs-workspaces--pending-tab-root root)
          (tab-bar-new-tab-choice (lambda () (dired-noselect root))))
      (tab-bar-new-tab))
    (dired root)
    (tab-bar-rename-tab (file-name-nondirectory (directory-file-name root)))
    (edmacs-workspaces-move-tab-to-group))
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
      (or (edmacs-workspaces-select-tab main)
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
Find-or-create keyed on ROOT alone: a worktree root belongs to exactly
one repo, so its group is a function of it and adding a group test to
the lookup could only ever hide the tab -- which is precisely what a
stale stored `group' used to do, leaving this creating a second tab for
a worktree already open. ROOT is normalized exactly as
`edmacs-workspaces--open-tab' stamps it \(truename plus trailing slash);
the two forms must agree or every invocation would duplicate a tab
instead of finding it.

The group is still resolved first, and a root with none still
`user-error's: an ungrouped tab is invisible to the sidebar's project
tree."
  (interactive (list (edmacs-workspaces--read-worktree current-prefix-arg)))
  (let* ((root (edmacs-workspaces--normalize-root dir))
         (group (edmacs-workspaces-group-name root)))
    (unless group
      (user-error "Not inside a git repository: %s" root))
    (if (edmacs-workspaces-find-tab root)
        (edmacs-workspaces-select-tab root)
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
             (frame-live-p frame)
             ;; `--stray-tab-number' reaches `tab-bar-tabs', which CREATES a
             ;; default tab on a frame with no `tabs' parameter and re-runs
             ;; `tab-bar-tab-post-open-functions' -- one of which draws a
             ;; sidebar. Never do that to the daemon's tty placeholder.
             (edmacs-workspaces-frame-usable-p frame))
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
                ;; Per-move, because this runs from a timer: a signal here
                ;; strands every relocation still queued behind it.
                (condition-case err
                    (progn
                      (tab-bar-select-tab target)
                      ;; Never a bare `switch-to-buffer'. `tab-bar-select-tab'
                      ;; restores the target tab's OWN selected window, which
                      ;; after RET on a sidebar row is the dedicated sidebar --
                      ;; and `switch-to-buffer' signals there. `pop-to-buffer'
                      ;; goes through `display-buffer', which never reuses a
                      ;; dedicated window and which this config's base action
                      ;; already aims at the main window.
                      (pop-to-buffer buf))
                  (error
                   (message "edmacs-workspaces: could not relocate %s: %s"
                            (buffer-name buf)
                            (error-message-string err))))))))))))

(defvar edmacs-workspaces--stray-sweep-timers (make-hash-table :test #'eq)
  "FRAME -> its pending stray-visit-sweep timer, or absent when none is
pending. A rolling debounce per frame -- canceled and rescheduled on
every new `window-buffer-change-functions' firing for that frame, never
stacked -- so a burst of N firings for the same frame within one command
loop (every window on a frame re-fires this hook on a tab switch, a
buffer kill, ...) collapses onto exactly one queued sweep instead of N
redundant ones. Keyed per frame, like sidebar.el's own
`edmacs-sidebar--resize-debounce-timers', rather than one shared timer:
unlike sidebar-buffers.el's `--redraw-timer' (:757, whose debounced
redraw is frame-independent -- it revisits every frame regardless of
which one changed), `--relocate-stray-visits' only ever acts on the one
FRAME it is passed, so a single shared timer would silently drop a
second frame's sweep if both fired within the same debounce window.")

(defun edmacs-workspaces--run-stray-sweep (frame)
  "Clear FRAME's entry from `edmacs-workspaces--stray-sweep-timers', then
sweep it. The function `edmacs-workspaces--on-window-buffer-change'
actually schedules -- doing the bookkeeping here, not there, keeps a
freshly fired timer from being mistaken for still-pending by a firing
that arrives while this sweep itself is running."
  (remhash frame edmacs-workspaces--stray-sweep-timers)
  (edmacs-workspaces--relocate-stray-visits frame))

(defun edmacs-workspaces--on-window-buffer-change (frame)
  "Schedule (or reschedule) a stray-visit sweep of FRAME off the
redisplay path. `window-buffer-change-functions' runs mid-redisplay, so
no window, tab or buffer work happens here directly -- only a timer.
Canceling any of FRAME's own already-pending timer before scheduling a
fresh one -- see `edmacs-workspaces--stray-sweep-timers' -- means a
burst of firings for FRAME queues exactly one sweep, not one per
firing."
  (when (and edmacs-workspaces-stray-visit-relocate
             (not edmacs-workspaces--relocating))
    (when-let* ((timer (gethash frame edmacs-workspaces--stray-sweep-timers)))
      (cancel-timer timer))
    (puthash frame
             (run-at-time 0 nil #'edmacs-workspaces--run-stray-sweep frame)
             edmacs-workspaces--stray-sweep-timers)))

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

(defun edmacs-workspaces--migrate-tab (tab)
  "Return a fresh copy of TAB carrying this module's worktree root and no
`group'. TAB's car (`tab' vs `current-tab') is preserved.

Two entries are dropped outright. The legacy root parameter goes because
a migrated tab the new model cannot read is the whole failure that
rename guards against. `group' goes because it is no longer part of a
tab's stored identity at all: `edmacs-workspaces-tab-group' derives it
from the root, so a stored one is at best a duplicate of the derived
answer and at worst a stale disagreement -- exactly the state that made
a tab invisible to `edmacs-workspaces-find-tab' and unfindable in the
sidebar's project tree. Dropping it here is what converges an old
desktop on load rather than at some later edit.

The root is ENSURED rather than re-derived: an existing
`edmacs-workspace-root' is kept untouched. A missing one is taken from
the legacy parameter, and failing that derived from TAB's own serialized
`ws' -- which is what lets a restored tab reach the session already
stamped, with nothing selected. Both make this a fixed point on its own
output."
  (let* ((params (cdr tab))
         (had-root (alist-get edmacs-workspaces-root-parameter params))
         (root (or had-root
                   (edmacs-workspaces--normalize-root
                    (alist-get edmacs-workspaces--legacy-root-parameter params))
                   (edmacs-workspaces--root-from-ws (alist-get 'ws params))))
         ;; A nil-valued root placeholder goes too, re-appended below with
         ;; a real value or not at all. An entry that already holds a
         ;; value is kept where it is and never re-appended -- appending a
         ;; second cons would shadow nothing but would make this transform
         ;; grow its output on every pass instead of being a fixed point.
         (drop (delq nil (list edmacs-workspaces--legacy-root-parameter
                               'group
                               (and (null had-root)
                                    edmacs-workspaces-root-parameter))))
         (kept (seq-remove (lambda (cell) (memq (car-safe cell) drop)) params)))
    (cons (car tab)
          (append (mapcar (lambda (cell) (cons (car cell) (cdr cell))) kept)
                  (and (null had-root) root
                       (list (cons edmacs-workspaces-root-parameter root)))))))

(defun edmacs-workspaces--fold-state (state time)
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
                     (ws . ,window-state))))
      (mapcar
       (lambda (tab)
         (let ((migrated (edmacs-workspaces--migrate-tab tab)))
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
  (let ((order '()) (next 0)
        (group-of (lambda (tab) (funcall tab-bar-tab-group-function (cdr tab)))))
    (dolist (tab tabs)
      (let ((group (funcall group-of tab)))
        (unless (assoc group order)
          (push (cons group next) order)
          (setq next (1+ next)))))
    (sort (copy-sequence tabs)
          (lambda (a b)
            (< (cdr (assoc (funcall group-of a) order))
               (cdr (assoc (funcall group-of b) order)))))))

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
this module's `edmacs-workspace-root' in place of the old `edmacs-root'
-- derived from the tab's own `ws' when it carries neither -- and LOSE
any stored `group', which is now derived from that root
\(`edmacs-workspaces-tab-group'); `edmacs-repo'/`edmacs-repo-missing'
are dropped from the surviving frame, which now holds several projects
and can no longer name one.

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
        (let ((migrated (edmacs-workspaces--migrate-tab tab)))
          (push migrated tabs)
          (push (alist-get edmacs-workspaces-root-parameter (cdr migrated)) seen)))
      (dolist (state states)
        (unless (eq state primary)
          (dolist (tab (edmacs-workspaces--fold-state state time))
            (let ((root (alist-get edmacs-workspaces-root-parameter (cdr tab))))
              ;; Dedupe only on a real ROOT -- group is now a function of
              ;; it, so a (group, root) key could only ever agree with this
              ;; one. Two ROOTLESS tabs are not evidence of the same
              ;; worktree, and AC2 loses nothing by keeping both.
              (unless (and root (member root seen))
                (push root seen)
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
