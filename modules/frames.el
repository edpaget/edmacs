;;; frames.el --- One frame per repo -*- lexical-binding: t -*-

;;; Commentary:
;; A frame is a repo. Each repo gets at most one frame, tagged with an
;; `edmacs-repo' parameter holding its git-common-dir (the same value
;; `edmacs-git-common-dir' caches for sessions.el and
;; claude-term-registry.el), titled with the repo's directory name, and
;; showing its sidebar (sidebar.el). Every tab inside that frame is a
;; worktree of that repo, tagged with its own `edmacs-root' parameter so
;; a worktree can be found by directory without re-deriving it from the
;; tab's buffer each time.
;;
;; `SPC p p' (`project-switch-project') lands in a repo's frame via
;; `edmacs-frames-open-project'; `SPC T p' / `C-x t p'
;; (`edmacs-frames-open-worktree-tab', wired from sessions.el) opens or
;; raises a worktree's tab inside that frame. A duplicate tab for the
;; same worktree is prevented before creation by both of those, and
;; folded after the fact -- for any tab created through some other route
;; (the stock `project-other-tab-command' prefix, `M-x tab-bar-new-tab',
;; `other-tab-prefix' from any package) -- by
;; `edmacs-frames--reconcile-tab-after-open' on
;; `tab-bar-tab-post-open-functions'. That reconciliation is also this
;; module's fix for task `sessions-dedupe-advice-closes-live-tab': the
;; old around-advice on `project-other-tab-command' it replaces ran its
;; check before `project-other-tab-command' (a prefix command on Emacs
;; >= 30) had created any tab at all, and could close a live one.
;;
;; `edmacs-frames--relocate-stray-visits' catches a file visited in the
;; wrong repo's frame (e.g. a `find-file' from a stale `default-directory'
;; or a package that ignores the frame model) and moves it to the right
;; one, off the redisplay path and never shelling out -- see its own
;; commentary below.
;;
;; `edmacs-worktrees-for-repo' feeds sidebar.el a repo's full worktree
;; list (tab or not) from a cache populated only at frame-creation time
;; and refreshed by a debounced `file-notify' watch on
;; `<common>/worktrees' -- see that section's own Commentary below for
;; why the redraw path itself never shells out, including on a cache
;; miss.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'tab-bar)
(require 'project)
(require 'vc-git)
(require 'filenotify)

;; Available via init.el's `load-module' order, not a `require' -- see
;; git-common-dir.el's own commentary on this codebase's shared-obarray
;; plain-`load' module system.
(declare-function edmacs-git-common-dir "git-common-dir")
(declare-function edmacs-git-common-dir-main-worktree "git-common-dir")
(declare-function edmacs-git-common-dir-repo-name "git-common-dir")
(declare-function edmacs-sidebar-show "sidebar")
(declare-function edmacs-sidebar--redraw "sidebar")
(defvar edmacs-git-common-dir-cache)

;; `general' loads only in a real init.el session; declared here so the
;; byte-compiler doesn't warn about the forward reference in the `SPC F'
;; block near the end of this file (gated by `with-eval-after-load', like
;; sidebar.el's own `SPC t s').
(declare-function general-define-key "general")

;; ============================================================================
;; Repo resolution
;; ============================================================================

(defun edmacs-frames--repo-of (dir)
  "Return the git-common-dir owning DIR's worktree, or nil.
Resolves DIR to its project root first (falling back to DIR itself when
no project is found) so the `edmacs-git-common-dir' cache is keyed the
same way `edmacs-sessions--tab-name' and
`claude-term-registry--repo-name' already key it, instead of adding a
fresh entry for whatever arbitrary subdirectory DIR happens to be."
  (let* ((proj (project-current nil dir))
         (root (if proj (project-root proj) dir)))
    (edmacs-git-common-dir root)))

(defun edmacs-frames-tab-in-own-repo-p (common)
  "Return non-nil when git-common-dir COMMON is the selected frame's own repo.
Used by `edmacs-sessions--tab-name' to decide whether a tab needs the
repo-name disambiguation prefix at all -- it never does once the frame
itself already names that repo."
  (and common (equal common (frame-parameter nil 'edmacs-repo))))

(defun edmacs-frames-for-repo (common)
  "Return a live frame whose `edmacs-repo' parameter equals COMMON, or nil."
  (seq-find (lambda (frame)
              (and (frame-live-p frame)
                   (equal (frame-parameter frame 'edmacs-repo) common)))
            (frame-list)))

(defun edmacs-frames--frames-for-repo-common (common)
  "Return every live frame whose `edmacs-repo' parameter equals COMMON.
Distinct from `edmacs-frames--repo-frames', which returns every frame
carrying ANY repo -- this is scoped to one exact COMMON, needed because
the usual one-frame-per-repo invariant can be transiently violated by
spare-frame reuse, and a worktree refresh must redraw every such frame,
not just the first one found."
  (seq-filter (lambda (f) (and (frame-live-p f)
                               (equal (frame-parameter f 'edmacs-repo) common)))
              (frame-list)))

(defun edmacs-frames--spare-frame ()
  "Return a live, repo-less frame available to adopt, or nil.
The daemon's own boot frame (made frame-less at `emacs-startup-hook' in
sessions.el) is exactly this shape from birth, as is any frame
`edmacs-frames--close-last-tab' has reset after its last tab closed.
Adopting one instead of always `make-frame'-ing avoids leaving an
orphan empty frame behind after every session's first repo switch."
  (seq-find (lambda (frame)
              (and (frame-live-p frame)
                   (not (frame-parameter frame 'edmacs-repo))))
            (frame-list)))

;; ============================================================================
;; Opening a repo's frame
;; ============================================================================

(defun edmacs-frames--visit-root (root)
  "Visit worktree ROOT in the selected window, as `dired'.
The chosen default for a freshly created tab's initial buffer -- both
this module's own first tab and any new worktree tab -- since the
phase's own `SPC T p' precedent (`project-other-tab-command') has no
single answer once that command is itself a multi-key prefix over
`project-prefix-map'."
  (dired root))

(defmacro edmacs-frames--without-display-override (&rest body)
  "Run BODY with any ambient `display-buffer' override neutralized.
`other-tab-prefix' (`M-x project-other-tab-command', `SPC T n', any
`other-*-prefix') arms `display-buffer-overriding-action' and
`switch-to-buffer-obey-display-actions' for whatever command runs next,
meaning to redirect THAT command's one next displayed buffer into a new
tab/frame/window. When the next command is one of this module's own
frame/tab builders, though, every buffer display inside BODY is this
module's own explicit placement (a `dired' buffer in a brand-new
frame's sole window, then the sidebar in its side window) and must
never be redirected into `display-buffer-in-tab' instead: doing so
re-fires `tab-bar-tab-post-open-functions' (sidebar.el's own hook
included) for each redirected buffer before the ORIGINAL override call
has returned far enough to clear itself, recursing without bound
\(`excessive-lisp-nesting') the first time this module's own sidebar
display gets caught by its own still-armed enclosing override."
  (declare (indent 0))
  `(let ((display-buffer-overriding-action '(nil . nil))
         (switch-to-buffer-obey-display-actions nil))
     ,@body))

(defun edmacs-frames--stamp-current-tab-root (root)
  "Stamp ROOT onto the selected frame's current tab as `edmacs-root'.
Mutates the tab alist's cdr in place via `push', never rebinding the
local variable -- `tab-bar.el' only persists in-place edits to the tab
object it already holds a reference to."
  (when-let* ((tab (tab-bar--current-tab-find)))
    (push (cons 'edmacs-root root) (cdr tab))))

(defun edmacs-frames-open (dir)
  "Raise the frame owning DIR's repo, creating one if none exists yet.
DIR need not be a repo's main worktree -- any worktree, or a file
inside one, resolves to the same repo via `edmacs-frames--repo-of'.
Returns the frame."
  (let* ((common (edmacs-frames--repo-of dir))
         (existing (and common (edmacs-frames-for-repo common))))
    (if existing
        (progn (select-frame-set-input-focus existing) existing)
      (let* ((main (if common
                       (edmacs-git-common-dir-main-worktree common)
                     (file-name-as-directory (expand-file-name dir))))
             (label (if common (edmacs-git-common-dir-repo-name common)
                      (file-name-nondirectory (directory-file-name main))))
             (frame (or (edmacs-frames--spare-frame) (make-frame))))
        (set-frame-parameter frame 'edmacs-repo common)
        (set-frame-parameter frame 'name label)
        (when common
          ;; Before the sidebar's first redraw, so `edmacs-worktrees-for-repo'
          ;; never has to render off a cold cache for this repo's own frame.
          (edmacs-frames--ensure-repo-tracking common))
        (edmacs-frames--without-display-override
          (with-selected-frame frame
            (delete-other-windows)
            (edmacs-frames--visit-root main)
            (edmacs-frames--stamp-current-tab-root (file-truename main))
            (tab-bar-rename-tab label)
            (edmacs-sidebar-show frame)))
        (select-frame-set-input-focus frame)
        frame))))

(defun edmacs-frames-open-project ()
  "`project-switch-commands' entry point: open the chosen project's repo frame.
`project-switch-project' binds `project-current-directory-override' to
the chosen directory before dispatching to the single command named by
`project-switch-commands' -- reading it back here is that handoff's
documented contract."
  (interactive)
  (edmacs-frames-open project-current-directory-override))

;; ============================================================================
;; Worktree tabs -- open/raise a tab for a worktree, in its repo's frame
;; ============================================================================

(defvar edmacs-frames--suppress-reconcile nil
  "Non-nil while a tab this module itself is creating should skip
`edmacs-frames--reconcile-tab-after-open'. Bound only around the brief
window between `tab-bar-new-tab' and this module stamping/visiting the
tab it just asked for -- that tab is not misidentified as a duplicate
of whatever tab was selected a moment before, which the reconciliation
hook would otherwise see (the new tab still shows the previous tab's
buffer until this module visits it).")

(defun edmacs-frames--ws-selected-buffer-name (ws)
  "Return the buffer name of the selected leaf window in WS, or nil.
WS is a `window-state-get' tree, as stored in a (non-current) tab's own
`ws' field. Recurses through `vc'/`hc' combination nodes for the leaf
marked `(selected . t)', falling back to the first leaf found -- a
lone-window tab (this module never creates any other kind) has no
`selected' marker to find."
  (pcase ws
    (`(leaf . ,params)
     (let ((buf (alist-get 'buffer params)))
       (and (consp buf) (car buf))))
    (`(,(or 'vc 'hc) . ,rest)
     (let (found first)
       (dolist (child rest)
         (when (and (consp child) (memq (car child) '(leaf vc hc)))
           (let* ((name (edmacs-frames--ws-selected-buffer-name child))
                  ;; `selected' lives nested inside the leaf's own `buffer'
                  ;; entry -- `(leaf (buffer NAME (selected . t) ...))' --
                  ;; not as a sibling of `buffer' at the leaf's own level.
                  (buf-entry (and (eq (car child) 'leaf)
                                  (alist-get 'buffer (cdr child))))
                  (selected (and buf-entry (alist-get 'selected (cdr buf-entry)))))
             (unless first (setq first name))
             (when selected (setq found name)))))
       (or found first)))
    (_ nil)))

(defun edmacs-frames--tab-window-buffer (tab)
  "Return the live buffer TAB's selected window last showed, or nil.
For the current tab this is simply the selected window's buffer; any
other tab was never switched to, so this walks its own serialized `ws'
\(window-state) field -- the same data `tab-bar-select-tab' itself
restores from -- for its selected leaf's buffer name."
  (if (eq (car tab) 'current-tab)
      (window-buffer (selected-window))
    (let ((name (edmacs-frames--ws-selected-buffer-name (alist-get 'ws tab))))
      (and name (get-buffer name)))))

(defun edmacs-frames--tab-root (tab)
  "Return TAB's `edmacs-root', deriving and stamping it from its buffer if unset.
The stored property is the fast path once any of this module's own
tab-creation routes have stamped it. The fallback covers a tab this
module never touched -- a plain `SPC T n', or one restored by desktop --
by reading its own selected buffer's `default-directory', and stamps
the result so the derivation is not repeated."
  (or (alist-get 'edmacs-root tab)
      (when-let* ((buf (edmacs-frames--tab-window-buffer tab))
                  (dir (buffer-local-value 'default-directory buf)))
        (let ((root (file-truename dir)))
          (push (cons 'edmacs-root root) (cdr tab))
          root))))

(defun edmacs-frames--find-tab-by-root (root &optional frame)
  "Return the tab in FRAME (default selected) whose root equals ROOT, or nil."
  (seq-find (lambda (tab) (equal (edmacs-frames--tab-root tab) root))
            (tab-bar-tabs frame)))

(defun edmacs-frames--tab-for-root (root &optional frame)
  "Return the tab in FRAME (default selected) whose root equals ROOT, or nil.
Public wrapper over `edmacs-frames--find-tab-by-root', for sidebar.el's
worktree-aware redraw to look up a worktree's tab without re-deriving
anything from `project-current'."
  (edmacs-frames--find-tab-by-root root frame))

(defun edmacs-frames--repo-worktrees (common)
  "Return COMMON's main worktree, then its other worktrees.
Runs `vc-git-known-other-working-trees' with `default-directory' bound
to the MAIN worktree throughout -- that function excludes only the
truename of `default-directory' from its results, so calling it from a
non-main worktree of the same repo would wrongly include the main
worktree among the \"other\" ones."
  (let* ((main (edmacs-git-common-dir-main-worktree common))
         (default-directory main))
    (cons main (vc-git-known-other-working-trees))))

;; ============================================================================
;; Worktree list -- cached, refreshed only on frame creation or file-notify
;; ============================================================================
;; `edmacs-worktrees-for-repo' is a strict pure-cache read: it never calls
;; `edmacs-frames--worktrees-compute' or `-refresh' itself, even on a
;; cache miss. Population is guaranteed by call order elsewhere
;; (`edmacs-frames-open' populates synchronously before the first
;; `edmacs-sidebar-show', and the file-notify debounce below repopulates
;; thereafter); a miss simply renders as an empty worktree list for that
;; repo until the next refresh trigger fires. This is the invariant that
;; keeps `process-file' out of the redraw/tab-switch path entirely --
;; `edmacs-frames--worktrees-compute' is the only function in this whole
;; feature reaching `vc-git-known-other-working-trees' (-> `process-file'),
;; and it is only ever invoked from `edmacs-frames--worktrees-refresh'.

(defvar edmacs-frames--worktrees-cache (make-hash-table :test #'equal)
  "COMMON -> list of (NAME . TRUENAME-ROOT), including the main worktree.
Populated only by `edmacs-frames--worktrees-refresh'; never evicted on
its own repo's last-frame teardown -- see
`edmacs-frames--teardown-worktrees-watch'.")

(defvar edmacs-frames--worktree-watches (make-hash-table :test #'equal)
  "COMMON -> file-notify watch descriptor.
Watches COMMON's `worktrees' subdirectory directly once it exists, or
COMMON itself (filtered to that subdirectory's appearance) until then --
see `edmacs-frames--ensure-worktrees-watch'.")

(defvar edmacs-frames--worktree-refresh-timers (make-hash-table :test #'equal)
  "COMMON -> pending debounce timer for
`edmacs-frames--schedule-worktrees-refresh'.")

(defun edmacs-frames--worktrees-compute (common)
  "Return COMMON's worktrees as a list of (NAME . TRUENAME-ROOT).
The only function in this feature reaching `vc-git-known-other-working-trees'
(-> `process-file'), via `edmacs-frames--repo-worktrees'."
  (mapcar (lambda (dir)
            (cons (file-name-nondirectory (directory-file-name dir))
                  (file-truename dir)))
          (edmacs-frames--repo-worktrees common)))

(defun edmacs-worktrees-for-repo (common)
  "Return the cached worktree list for git-common-dir COMMON, or nil.
A strict pure-cache read -- see this section's own Commentary above for
why a miss is a harmless empty render rather than a fallback compute."
  (gethash common edmacs-frames--worktrees-cache))

(defun edmacs-frames--worktrees-refresh (common)
  "Recompute COMMON's worktree list and redraw every frame showing it.
Invoked only from `edmacs-frames-open's first-frame-for-repo branch and
from the debounced file-notify callback below -- never from redraw or
tab-switch."
  (puthash common (edmacs-frames--worktrees-compute common)
           edmacs-frames--worktrees-cache)
  (dolist (frame (edmacs-frames--frames-for-repo-common common))
    (edmacs-sidebar--redraw frame)))

(defun edmacs-frames--worktrees-refresh-safe (common)
  "Call `edmacs-frames--worktrees-refresh' for COMMON, catching any error.
`edmacs-frames--worktrees-compute' shells out via
`vc-git-known-other-working-trees', which can signal on a transient git
failure, an unusual `--separate-git-dir'/bare layout, or a directory
that goes unreadable mid-call. Every other call site touching this
feature (the file-notify watch callbacks below) is already defensive
about that same call; this is the same protection for the refresh
itself, including its use inside `edmacs-frames-open', where an
uncaught signal here would otherwise abort frame setup after
`edmacs-repo' is already stamped and leave a half-built frame that
`edmacs-frames-for-repo' keeps finding on every later lookup. On error
the cache is simply left at its previous value -- nil renders as an
empty worktree list via `edmacs-worktrees-for-repo', never a raw
backtrace."
  (condition-case err
      (edmacs-frames--worktrees-refresh common)
    (error (message "edmacs-frames: worktree refresh failed for %s: %s"
                     common err))))

(defun edmacs-frames--schedule-worktrees-refresh (common)
  "Debounce a worktree refresh for COMMON by 0.5s.
Collapses a burst of file-notify events (e.g. `git worktree add' or
`workmux add' touching several paths under `worktrees/') into a single
recompute, and is the only timer this feature ever registers -- there
is no polling."
  (when-let* ((timer (gethash common edmacs-frames--worktree-refresh-timers)))
    (cancel-timer timer))
  (puthash common
           (run-at-time
            0.5 nil
            (lambda ()
              (remhash common edmacs-frames--worktree-refresh-timers)
              (edmacs-frames--worktrees-refresh-safe common)))
           edmacs-frames--worktree-refresh-timers))

(defun edmacs-frames--worktree-watch-callback (common _event)
  "Schedule a debounced refresh of COMMON in response to a direct-watch EVENT.
Wrapped in `condition-case': this feature has zero file-notify
precedent elsewhere in this codebase, so a backend quirk is treated
defensively rather than allowed to propagate out of the callback."
  (condition-case err
      (edmacs-frames--schedule-worktrees-refresh common)
    (error (message "edmacs-frames: worktree watch callback error for %s: %s"
                     common err))))

(defun edmacs-frames--upgrade-to-worktrees-watch (common)
  "Replace COMMON's parent-dir watch with a direct watch on its `worktrees' dir."
  (when-let* ((old (gethash common edmacs-frames--worktree-watches)))
    (ignore-errors (file-notify-rm-watch old)))
  (puthash common
           (file-notify-add-watch
            (expand-file-name "worktrees" common) '(change)
            (lambda (event) (edmacs-frames--worktree-watch-callback common event)))
           edmacs-frames--worktree-watches))

(defun edmacs-frames--parent-watch-callback (common event)
  "Handle EVENT on COMMON's temporary parent-dir watch.
Filters to the literal filename \"worktrees\" -- `.git/' otherwise
receives frequent unrelated writes (e.g. `.git/index') that would
trigger wasted recomputes before the upgrade below ever needs to
happen. Upgrades to a direct watch and refreshes immediately once
`worktrees' actually appears."
  (condition-case err
      (pcase-let ((`(,_desc ,action ,file) event))
        (when (and (member action '(created changed))
                   (equal (file-name-nondirectory (directory-file-name file))
                          "worktrees"))
          (edmacs-frames--upgrade-to-worktrees-watch common)
          (edmacs-frames--worktrees-refresh-safe common)))
    (error (message "edmacs-frames: worktree parent-watch callback error for %s: %s"
                     common err))))

(defun edmacs-frames--ensure-worktrees-watch (common)
  "Ensure a file-notify watch is active for COMMON's `worktrees' directory.
No-op if a watch descriptor is already stored for COMMON. Otherwise
watches the directory directly if it already exists, else watches
COMMON itself until `worktrees' first appears (see
`edmacs-frames--parent-watch-callback'). `file-notify-add-watch' can
signal on some backends -- wrapped so a watch failure never breaks
frame creation, only leaves that repo's sidebar refreshing on
frame-creation events alone."
  (unless (gethash common edmacs-frames--worktree-watches)
    (condition-case err
        (let ((worktrees-dir (expand-file-name "worktrees" common)))
          (if (file-directory-p worktrees-dir)
              (puthash common
                       (file-notify-add-watch
                        worktrees-dir '(change)
                        (lambda (event)
                          (edmacs-frames--worktree-watch-callback common event)))
                       edmacs-frames--worktree-watches)
            (puthash common
                     (file-notify-add-watch
                      common '(change)
                      (lambda (event)
                        (edmacs-frames--parent-watch-callback common event)))
                     edmacs-frames--worktree-watches)))
      (file-notify-error
       (message "edmacs-frames: could not watch worktrees for %s: %s" common err)))))

(defun edmacs-frames--ensure-repo-tracking (common)
  "Warm COMMON's worktree cache and arm its file-notify watch.
Idempotent: refreshing again is harmless, and
`edmacs-frames--ensure-worktrees-watch' is already a no-op once a watch
exists. Shared by `edmacs-frames-open' (a freshly created repo frame,
whose caches start cold the first time any frame names COMMON) and
`edmacs-sessions--finish-frame-restore' (every repo frame a
daemon-boot frameset restore hands back, whose caches also start cold
-- the daemon's own restart empties both hash tables) -- one function
so the two initialization paths cannot drift apart on what a repo
frame needs before its first sidebar redraw."
  (edmacs-frames--worktrees-refresh-safe common)
  (edmacs-frames--ensure-worktrees-watch common))

(defun edmacs-frames--teardown-worktrees-watch (common)
  "Cancel COMMON's pending debounce timer and remove its file-notify watch.
The cache entry itself (`edmacs-frames--worktrees-cache') is deliberately
left in place -- harmless, since `edmacs-worktrees-for-repo' is already a
safe pure read and nothing draws from it once no frame references COMMON."
  (when-let* ((timer (gethash common edmacs-frames--worktree-refresh-timers)))
    (cancel-timer timer)
    (remhash common edmacs-frames--worktree-refresh-timers))
  (when-let* ((desc (gethash common edmacs-frames--worktree-watches)))
    (ignore-errors (file-notify-rm-watch desc))
    (remhash common edmacs-frames--worktree-watches)))

(defun edmacs-frames--current-frame-common ()
  "Return the git-common-dir the selected frame is on.
Prefers the frame's own `edmacs-repo' parameter; falls back to
resolving the current buffer's own directory for a frame that has not
been claimed by `edmacs-frames-open' yet (e.g. the daemon's boot frame)."
  (or (frame-parameter (selected-frame) 'edmacs-repo)
      (edmacs-frames--repo-of default-directory)))

(defun edmacs-frames--read-worktree (prefix)
  "Read a worktree directory, interactively.
With no PREFIX, offer a `completing-read' over the selected frame's own
repo worktrees -- the main worktree plus `vc-git-known-other-working-trees'
-- since worktrees are deliberately absent from
`project-known-project-roots' (see core.el), making this the only
prompt that reaches them. With PREFIX, fall back to
`read-directory-name' for any directory."
  (if prefix
      (read-directory-name "Open worktree in its repo frame: ")
    (let ((common (edmacs-frames--current-frame-common)))
      (if (not common)
          (read-directory-name "Open worktree in its repo frame: ")
        (completing-read "Worktree: " (edmacs-frames--repo-worktrees common) nil t)))))

(defun edmacs-frames-open-worktree-tab (dir)
  "Open or raise a tab for DIR's worktree, in DIR's repo's frame.
Never touches the calling frame: `edmacs-frames-open' raises or creates
DIR's own repo frame first, and the tab search/creation below happens
entirely inside that frame. A tab already showing DIR is selected, not
duplicated; this is the primary duplicate-prevention rule -- see this
module's Commentary on `edmacs-frames--reconcile-tab-after-open' for
the safety net covering tabs opened through any other route."
  (interactive (list (edmacs-frames--read-worktree current-prefix-arg)))
  (let* ((root (file-truename dir))
         (frame (edmacs-frames-open dir)))
    (edmacs-frames--without-display-override
      (with-selected-frame frame
        (let ((tab (edmacs-frames--find-tab-by-root root frame)))
          (if tab
              (tab-bar-select-tab (1+ (tab-bar--tab-index tab (tab-bar-tabs frame) frame)))
            (let ((edmacs-frames--suppress-reconcile t))
              (tab-bar-new-tab))
            (edmacs-frames--stamp-current-tab-root root)
            (edmacs-frames--visit-root dir)
            (tab-bar-rename-tab (file-name-nondirectory (directory-file-name dir)))))))
    frame))

(defun edmacs-frames--reconcile-tab-after-open (tab)
  "Fold TAB back onto an existing same-root tab in `(selected-frame)'.
The safety net for a tab created through any route other than
`edmacs-frames-open-worktree-tab' -- the stock `project-other-tab-command'
prefix, `M-x tab-bar-new-tab', `other-tab-prefix' from any package.
Stamps TAB's own `edmacs-root' first (deriving it from its buffer, since
it was just created and has none yet), then closes TAB itself if
another tab in the frame already carries that root -- safe because, at
post-open time, TAB is unambiguously the one just added: closing it,
never an older tab, cannot surprise a user mid-edit in some other tab."
  (unless edmacs-frames--suppress-reconcile
    (when-let* ((root (edmacs-frames--tab-root tab))
                (dup (seq-find (lambda (other)
                                 (and (not (eq other tab))
                                      (equal (alist-get 'edmacs-root other) root)))
                               (tab-bar-tabs))))
      ;; Both indices are captured before any selection change: switching
      ;; tabs replaces the outgoing tab's cons with a freshly-built one
      ;; (see `tab-bar-select-tab's own `(from-tab (tab-bar--tab))'), so an
      ;; `eq'-based index lookup on TAB done *after* selecting DUP would
      ;; already be searching for an object no longer in the list -- the
      ;; absolute position itself is unaffected by that swap, so closing
      ;; by the position captured now still closes the right tab.
      (let ((dup-number (1+ (tab-bar--tab-index dup)))
            (tab-number (1+ (tab-bar--tab-index tab))))
        (tab-bar-select-tab dup-number)
        (tab-bar-close-tab tab-number)))))

(add-hook 'tab-bar-tab-post-open-functions #'edmacs-frames--reconcile-tab-after-open)

;; ============================================================================
;; Stray visits -- catch a file opened in the wrong repo's frame
;; ============================================================================

(defvar edmacs-frames-stray-visit-relocate t
  "When non-nil, relocate a file buffer displayed in the wrong repo's frame.
Set to nil to turn this off entirely.")

(defun edmacs-frames--cached-common-dir (dir)
  "Return the cached git-common-dir for DIR, or nil on a cache miss.
Reads `edmacs-git-common-dir-cache' directly via `gethash' -- never
`edmacs-git-common-dir' itself, which shells out on a miss -- and never
walks DIR's ancestors, so a directory nothing else has already looked
up is simply a miss, not a fresh subprocess. Checked under both the
trailing-slash and bare forms of DIR, since different callers key the
cache with whichever form their own ROOT argument happened to have."
  (let* ((miss 'edmacs-frames--miss)
         (hit (let ((v (gethash dir edmacs-git-common-dir-cache miss)))
                (if (eq v miss)
                    (let ((v2 (gethash (file-name-as-directory dir)
                                       edmacs-git-common-dir-cache miss)))
                      (if (eq v2 miss)
                          (gethash (directory-file-name dir)
                                  edmacs-git-common-dir-cache miss)
                        v2))
                  v))))
    (and (not (eq hit miss)) (not (eq hit 'none)) hit)))

(defun edmacs-frames--relocate-stray-visits (frame)
  "Move any file-visiting buffer in FRAME whose repo differs from FRAME's own.
Runs from a zero-delay timer scheduled by
`edmacs-frames--on-window-buffer-change', never synchronously from
`window-buffer-change-functions' itself, which fires mid-redisplay.
Only ordinary (non-side, non-dedicated) windows are considered, and
only via `edmacs-frames--cached-common-dir's cache-hit-only lookup, so
this never shells out on the redisplay path."
  (when (and edmacs-frames-stray-visit-relocate (frame-live-p frame))
    (dolist (window (window-list frame 'never))
      (when (and (window-live-p window)
                 (not (window-parameter window 'window-side))
                 (not (window-dedicated-p window)))
        (let* ((buf (window-buffer window))
               (file (buffer-file-name buf)))
          (when file
            (let ((common (edmacs-frames--cached-common-dir (file-name-directory file))))
              (when (and common (not (equal common (frame-parameter frame 'edmacs-repo))))
                (let ((target (edmacs-frames-open (file-name-directory file))))
                  (when (frame-live-p target)
                    (with-selected-frame target
                      (switch-to-buffer buf))))
                (when (window-live-p window)
                  (with-selected-window window
                    (switch-to-prev-buffer window)))))))))))

(defun edmacs-frames--on-window-buffer-change (frame)
  "Schedule a stray-visit sweep of FRAME off the redisplay path.
`window-buffer-change-functions' runs mid-redisplay, so no window or
buffer work happens here directly -- only a zero-delay idle timer,
matching `edmacs-sidebar--on-tab-pre-close's own deferral for the same
reason."
  (when edmacs-frames-stray-visit-relocate
    (run-at-time 0 nil #'edmacs-frames--relocate-stray-visits frame)))

(add-hook 'window-buffer-change-functions #'edmacs-frames--on-window-buffer-change)

;; ============================================================================
;; Closing a frame's last tab
;; ============================================================================

(defun edmacs-frames--only-frame-p (frame)
  "Return non-nil when FRAME is the only real, user-visible top-level frame.
A naive `frame-live-p' count is fooled by a child frame such as corfu's
completion popup: `corfu--hide-frame' only calls `make-frame-invisible',
never `delete-frame', so that frame stays live -- but invisible, and
parented to the frame it popped up from -- for the rest of the session.
Excluding both invisible frames and any frame with a non-nil
`parent-frame' parameter (as `edmacs-ns-close-frame' in sessions.el
excludes non-graphic/invisible frames from its own \"only frame\" count)
keeps such a popup from ever being mistaken for a second real frame,
without requiring `display-graphic-p' -- this handler also runs under
the tty frames this module's own batch test suite creates."
  (ignore frame)
  (= 1 (length (seq-filter (lambda (f) (and (frame-visible-p f)
                                            (not (frame-parameter f 'parent-frame))))
                           (frame-list)))))

(defun edmacs-frames--maybe-teardown-watch-for-frame (frame)
  "Tear down FRAME's repo's worktree watch if FRAME is that repo's last frame.
Shared by `edmacs-frames--close-last-tab' and the `delete-frame-functions'
hook below, since a repo frame can be lost two ways on this daemon: tabs
closed one-by-one down to zero (routed through
`tab-bar-close-last-tab-choice'), or a direct `delete-frame' call such as
`edmacs-ns-close-frame' in sessions.el, which never consults
`tab-bar-close-last-tab-choice' at all. Both callers run this while
FRAME is still live and its `edmacs-repo' still readable -- required
because `delete-frame-functions' fires before FRAME is actually removed
from `frame-list' -- and only tear down when no OTHER live frame still
carries the same repo (the one-frame-per-repo invariant can be
transiently violated by spare-frame reuse). Idempotent: a repo whose
watch is already torn down is a no-op, so the two call sites racing on
the same frame (`edmacs-frames--close-last-tab' calling `delete-frame',
which then re-fires this via the hook) is harmless."
  (let ((common (frame-parameter frame 'edmacs-repo)))
    (when (and common
               (null (seq-remove (lambda (f) (eq f frame))
                                  (edmacs-frames--frames-for-repo-common common))))
      (edmacs-frames--teardown-worktrees-watch common))))

(add-hook 'delete-frame-functions #'edmacs-frames--maybe-teardown-watch-for-frame)

(defun edmacs-frames--close-last-tab (_tab)
  "`tab-bar-close-last-tab-choice' handler for a repo frame's last tab.
Deletes the frame, unless it is the only frame left in the whole
daemon -- which must keep at least one frame alive to keep running (and,
on macOS, to keep owning the Dock's Emacs.app tile; see
`edmacs-ns-close-frame' in sessions.el). That survivor is reset to a
scratch buffer and stripped of its `edmacs-repo'/name instead, so it
becomes an adoptable spare frame for the next `edmacs-frames-open'
rather than a stale relic still naming a repo with no tabs left.

Either branch means FRAME's repo (if any) has just lost its last frame,
so `edmacs-frames--maybe-teardown-watch-for-frame' runs first, while
`edmacs-repo' is still readable and before `delete-frame'/reset can
race a debounce timer that is already scheduled. The reset branch never
calls `delete-frame' (so the hook above won't fire for it), which is
why this teardown call is not redundant with that hook."
  (let* ((frame (selected-frame)))
    (edmacs-frames--maybe-teardown-watch-for-frame frame)
    (if (edmacs-frames--only-frame-p frame)
        (progn
          (switch-to-buffer (get-buffer-create "*scratch*"))
          (delete-other-windows)
          (set-frame-parameter frame 'edmacs-repo nil)
          (set-frame-parameter frame 'name nil)
          (tab-bar-rename-tab "emacs"))
      (delete-frame))))

(setq tab-bar-close-last-tab-choice #'edmacs-frames--close-last-tab)

;; ============================================================================
;; SPC F - switch between repo frames
;; ============================================================================

(defun edmacs-frames--repo-frames ()
  "Return every live frame carrying a non-nil `edmacs-repo' parameter."
  (seq-filter (lambda (f) (and (frame-live-p f) (frame-parameter f 'edmacs-repo)))
              (frame-list)))

(defun edmacs-frames-switch (&optional name)
  "Select the repo frame named NAME, prompting via `completing-read' when nil."
  (interactive)
  (let* ((frames (edmacs-frames--repo-frames))
         (table (mapcar (lambda (f) (cons (frame-parameter f 'name) f)) frames))
         (chosen (or name (completing-read "Repo frame: " table nil t)))
         (frame (cdr (assoc chosen table))))
    (when frame (select-frame-set-input-focus frame))))

(defun edmacs-frames--cycle (step)
  "Select the repo frame STEP positions from the selected one, wrapping."
  (let ((frames (edmacs-frames--repo-frames)))
    (when frames
      (let* ((pos (or (seq-position frames (selected-frame)) 0))
             (next (mod (+ pos step) (length frames))))
        (select-frame-set-input-focus (nth next frames))))))

(defun edmacs-frames-next-frame ()
  "Select the next repo frame, wrapping."
  (interactive)
  (edmacs-frames--cycle 1))

(defun edmacs-frames-previous-frame ()
  "Select the previous repo frame, wrapping."
  (interactive)
  (edmacs-frames--cycle -1))

(with-eval-after-load 'general
  (general-define-key
   :states 'normal
   :prefix "SPC F"
   "" '(:ignore t :which-key "frames")
   "f" '(edmacs-frames-switch :which-key "switch frame")
   "n" '(edmacs-frames-next-frame :which-key "next frame")
   "p" '(edmacs-frames-previous-frame :which-key "previous frame")
   "d" '(delete-frame :which-key "delete frame")))

(provide 'frames)
;;; frames.el ends here
