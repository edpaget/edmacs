;;; frames.el --- One frame per repo -*- lexical-binding: t -*-

;;; Commentary:
;; A frame is a repo. Each repo gets at most one frame, tagged with an
;; `edmacs-repo' parameter holding its git-common-dir (the same value
;; `edmacs-git-common-dir' caches for sessions.el and
;; claude-term-registry.el), titled with the repo's directory name, and
;; showing its sidebar (sidebar.el). Every tab inside that frame is a
;; worktree of that repo, tagged with its own `edmacs-root' parameter.
;;
;; Identity is STAMPED, never sniffed. `edmacs-root' is written when the
;; tab is created (`edmacs-frames--on-tab-post-open', which covers every
;; tab-creating route including a plain `tab-bar-new-tab'), repaired when
;; an unstamped tab is first selected (`edmacs-frames--on-tab-post-select')
;; and re-stamped by `edmacs-frames-stamp-frame-tabs' during a desktop
;; restore. `edmacs-frames--tab-root' is therefore a pure read: nothing
;; derives a tab's identity from a buffer or from a serialized
;; window-state blob, and the one place a root IS derived from a buffer
;; (`edmacs-frames--derive-root') takes an explicit frame, so a
;; background frame can never inherit the selected frame's answer.
;;
;; The same rule applies one level up: `edmacs-frames-frame-usable-p'
;; keeps the daemon's initial tty placeholder from ever being adopted as
;; a spare or stamped with an `edmacs-repo', mirroring
;; `desktop--check-dont-save's own exclusion of it, and
;; `edmacs-frames-for-repo' reconciles duplicates and returns a healthy
;; frame rather than whichever one `frame-list' happens to yield first.
;;
;; `SPC p p' (`project-switch-project') lands in a repo's frame via
;; `edmacs-frames-open-project'; `SPC T p' / `C-x t p'
;; (`edmacs-frames-open-worktree-tab', wired from sessions.el) opens or
;; raises a worktree's tab inside that frame. A duplicate tab for the
;; same worktree is prevented before creation by both of those, and
;; folded after the fact -- for any tab created through some other route
;; (the stock `project-other-tab-command' prefix, `M-x tab-bar-new-tab',
;; `other-tab-prefix' from any package) -- by
;; `edmacs-frames--on-tab-post-open' on
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
;; Every graphical frame this config opens -- a repo frame, the daemon's
;; boot frame, an `emacsclient -c' frame -- is put fullscreen by
;; `edmacs-frames-apply-fullscreen'. On macOS that is native fullscreen,
;; so each repo frame gets its own Space; a fullscreen frame also has no
;; saved geometry left to replay onto whatever monitor it was last on.
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

;; windows.el loads BEFORE frames.el, so these resolve at real load time;
;; declared for frames-test.el's standalone `-Q --batch' harness.
(declare-function edmacs-main-window "windows")
(declare-function edmacs-windows-repair-frame "windows")
(declare-function edmacs-windows-frame-wedged-p "windows")
(defvar edmacs-git-common-dir-cache)

;; sessions.el also loads before frames.el (init.el), so this resolves at
;; real load time too; `fboundp'-guarded at its one call site for
;; frames-test.el's standalone harness.
(declare-function edmacs-sessions--make-gui-frame "sessions")

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

(defun edmacs-frames-tab-in-own-repo-p (common frame)
  "Return non-nil when git-common-dir COMMON is FRAME's own repo.
Used by `edmacs-sessions--tab-name' to decide whether a tab needs the
repo-name disambiguation prefix at all -- it never does once the frame
itself already names that repo."
  (and common (equal common (frame-parameter frame 'edmacs-repo))))

(defun edmacs-frames--graphical-session-p ()
  "Return non-nil when this session holds at least one graphical frame."
  (seq-some (lambda (f) (and (frame-live-p f) (display-graphic-p f)))
            (frame-list)))

(defun edmacs-frames-frame-usable-p (frame)
  "Return non-nil when FRAME may carry an `edmacs-repo' and show its buffers.
Excludes a child frame (a corfu-style popup, which must stay the size
its owner gave it) and the daemon's initial tty placeholder -- the frame
`desktop--check-dont-save' already refuses to save, which is never on
screen and on which `select-frame-set-input-focus' is a no-op the user
reads as \"opening a worktree did nothing\".

A non-graphical frame counts as usable only while the session has no
graphical frame at all. That disjunct is what keeps the tty-only batch
test harnesses (and a genuinely terminal-only Emacs) working, while
still excluding the placeholder on the real daemon, which always holds a
GUI boot frame."
  (and (frame-live-p frame)
       (not (frame-parameter frame 'parent-frame))
       (not (and (daemonp) (frame-initial-p frame)))
       (or (display-graphic-p frame)
           (not (edmacs-frames--graphical-session-p)))))

(defun edmacs-frames--frame-healthy-p (frame)
  "Return non-nil when FRAME is usable and can actually display a buffer.
Unhealthy is windows.el's wedged shape: every window a side window, so
`display-buffer' has nowhere to put a buffer but another side window."
  (and (edmacs-frames-frame-usable-p frame)
       (not (and (fboundp 'edmacs-windows-frame-wedged-p)
                 (edmacs-windows-frame-wedged-p frame)))))

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

(defun edmacs-frames--demote-frame (frame common)
  "Strip FRAME's repo identity so it stops answering lookups for COMMON.
Non-destructive on purpose: a live frame can hold the user's only copy
of a layout, so a duplicate is un-named rather than deleted. Warns
because a demoted frame becomes adoptable as a spare again, and the next
`edmacs-frames-open' will `dired'-visit over whatever it is showing."
  (set-frame-parameter frame 'edmacs-repo nil)
  (set-frame-parameter frame 'name nil)
  (display-warning
   'edmacs-frames
   (format "demoted a frame that duplicated or could not display %s" common)
   :warning))

(defun edmacs-frames--better-survivor-p (a b)
  "Return non-nil when frame A should outlive frame B as a repo's frame.
Healthy beats wedged; between two equally healthy frames the one holding
more tabs wins, so reconciliation never demotes the frame carrying more
of the user's work. `sort' is stable, so an otherwise equal pair keeps
`frame-list' order."
  (let ((healthy-a (edmacs-frames--frame-healthy-p a))
        (healthy-b (edmacs-frames--frame-healthy-p b)))
    (cond ((not (eq healthy-a healthy-b)) (and healthy-a t))
          (t (> (length (tab-bar-tabs a)) (length (tab-bar-tabs b)))))))

(defun edmacs-frames--reconcile-repo-frames (common)
  "Reduce COMMON's frames to a single survivor and return it, or nil.
First demotes every match that can never display COMMON -- which is
exactly what un-does a tty placeholder wrongly back-filled by a desktop
restore -- then, if several usable frames still claim COMMON, keeps the
best one (`edmacs-frames--better-survivor-p') and demotes the rest.
Idempotent: a COMMON with zero or one usable frame is a no-op."
  (let* ((frames (edmacs-frames--frames-for-repo-common common))
         (usable (seq-filter #'edmacs-frames-frame-usable-p frames)))
    (dolist (frame frames)
      (unless (memq frame usable)
        (edmacs-frames--demote-frame frame common)))
    (when (cdr usable)
      (let ((survivor (car (sort (copy-sequence usable)
                                 #'edmacs-frames--better-survivor-p))))
        (dolist (frame usable)
          (unless (eq frame survivor)
            (edmacs-frames--demote-frame frame common)))
        (setq usable (list survivor))))
    (car usable)))

(defun edmacs-frames-for-repo (common)
  "Return the one frame that owns repo COMMON, or nil.
Never a bare first match: duplicates are reconciled first, and a frame
that can never display COMMON is demoted out of the answer entirely
rather than handed to `select-frame-set-input-focus'. A wedged but
otherwise usable frame IS still returned -- repair is destructive to
slot layout, and windows.el recovers such a frame on its next
`display-buffer' anyway, so a lookup stays side-effect-light."
  (and common (edmacs-frames--reconcile-repo-frames common)))

(defun edmacs-frames--spare-frame ()
  "Return a live, repo-less frame available to adopt, or nil.
The daemon's own boot frame (made frame-less at `emacs-startup-hook' in
sessions.el) is exactly this shape from birth, as is any frame
`edmacs-frames--close-last-tab' has reset after its last tab closed.
Adopting one instead of always `make-frame'-ing avoids leaving an
orphan empty frame behind after every session's first repo switch.

Gated on `edmacs-frames-frame-usable-p': the daemon's initial tty
placeholder is repo-less by construction, so without that gate it is the
first spare every `emacsclient -e'-driven open finds and adopts."
  (seq-find (lambda (frame)
              (and (edmacs-frames-frame-usable-p frame)
                   (not (frame-parameter frame 'edmacs-repo))))
            (frame-list)))

(defun edmacs-frames--make-frame ()
  "Create a frame fit to hold a repo, or nil when none can be made.
Under the daemon a bare `make-frame' names no window system and so
yields another tty placeholder, which is why the frame comes from
sessions.el's GUI maker instead. When that maker cannot produce one
there is no safe fallback left: the placeholder is exactly the frame
spare adoption already refuses, and handing it to `edmacs-frames-open'
to drive dired, the sidebar and input focus onto is what turns a
refused frame into a wedged daemon."
  (if (daemonp)
      (and (fboundp 'edmacs-sessions--make-gui-frame)
           (edmacs-sessions--make-gui-frame))
    (make-frame)))

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

(defun edmacs-frames--stamp-current-tab-root (root &optional frame)
  "Stamp ROOT onto FRAME's (default the selected frame's) current tab.
Mutates the tab alist's cdr in place -- `tab-bar.el' only persists
in-place edits to the tab object it already holds a reference to -- but
via `setf' rather than `push': re-stamping must REPLACE the entry.
A `push'-shadowed stale cons survives, because `tab-bar--tab' copies
every unrecognized tab parameter forward on each tab switch and desktop
then persists the lot, so the duplicate would outlive the session."
  (when-let* ((tab (tab-bar--current-tab-find nil frame)))
    (setf (alist-get 'edmacs-root (cdr tab)) root)
    root))

(defun edmacs-frames-open (dir)
  "Raise the frame owning DIR's repo, creating one if none exists yet.
DIR need not be a repo's main worktree -- any worktree, or a file
inside one, resolves to the same repo via `edmacs-frames--repo-of'.
Returns the frame, or nil when no frame could be created for it."
  (let* ((common (edmacs-frames--repo-of dir))
         (existing (and common (edmacs-frames-for-repo common))))
    (if existing
        ;; `edmacs-frames-for-repo' has already reconciled duplicates and
        ;; rejected anything that cannot display, so focus can no longer
        ;; land on the tty placeholder or on a wedged frame.
        (progn (edmacs-frames-stamp-frame-tabs existing)
               (select-frame-set-input-focus existing)
               existing)
      (let* ((main (if common
                       (edmacs-git-common-dir-main-worktree common)
                     (file-name-as-directory (expand-file-name dir))))
             (label (if common (edmacs-git-common-dir-repo-name common)
                      (file-name-nondirectory (directory-file-name main))))
             (frame (or (edmacs-frames--spare-frame) (edmacs-frames--make-frame))))
        (if (not frame)
            ;; Warn rather than signal: an error reaching a frameless
            ;; daemon's top level exits it 255 (see core.el). Nothing has
            ;; been stamped yet, so the next call retries cleanly.
            (progn
              (display-warning 'edmacs-frames
                               (format "could not open %s: no frame available"
                                       label)
                               :warning)
              nil)
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
          frame)))))

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

(defvar edmacs-frames--pending-tab-root nil
  "The root the tab this module is about to create must carry.
Bound around a `tab-bar-new-tab' this module itself drives, so
`edmacs-frames--on-tab-post-open' stamps the INTENDED root rather than
deriving one from the buffer the brand-new tab inherited from whichever
tab was selected a moment before. It replaces a blanket
\"skip reconciliation entirely\" flag, which disarmed the duplicate
safety net for precisely the call that most needs it.")

(defun edmacs-frames--current-tab (frame)
  "Return FRAME's current tab, without ever creating one.
Reads the `tabs' frame parameter directly: `tab-bar--current-tab-find'
goes through `tab-bar-tabs', which on an unset parameter creates a
default tab AND runs `tab-bar-tab-post-open-functions' -- re-entering
this module's own post-open hook from inside itself."
  (assq 'current-tab (frame-parameter frame 'tabs)))

(defun edmacs-frames--frame-content-window (frame)
  "Return the window FRAME shows its content in.
Whichever window carries windows.el's `edmacs-main' parameter, else
FRAME's first non-side window, else its selected window. Never
`selected-window': every caller can be running for a frame other than
the selected one, and reading the global selection is exactly the
cross-frame mix-up this module's stamped identity exists to rule out."
  (or (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                (window-list frame 'no-minibuf))
      (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                (window-list frame 'no-minibuf))
      (frame-selected-window frame)))

(defun edmacs-frames--derive-root (frame)
  "Return the worktree root FRAME's content window is showing, or nil.
The only place in this module a root is derived from a buffer at all,
and frame-scoped by construction."
  (when-let* ((window (edmacs-frames--frame-content-window frame))
              (buffer (and (window-live-p window) (window-buffer window)))
              (dir (buffer-local-value 'default-directory buffer)))
    (file-truename dir)))

(defun edmacs-frames--tab-root (tab)
  "Return TAB's `edmacs-root', or nil.
A pure read, with no derivation and no side effect. See this module's
Commentary for where the stamp is written; a tab restored from a
desktop file written before the stamp was mandatory simply reads nil
until it is selected once, which is deliberately preferred to guessing."
  (alist-get 'edmacs-root tab))

(defun edmacs-frames-tab-root-live-p (root)
  "Return non-nil when ROOT is a stamp whose directory still exists.
A stamp is not self-validating: a tab restored from a desktop keeps
pointing at whatever worktree it was saved on, and that directory may
since have been removed. `edmacs-git-common-dir' returns nil for a path
that is gone, and `edmacs-sessions--frame-tab-roots' turns a single
unresolvable tab into nil for the whole frame -- which leaves
`edmacs-repo' unset and drops the sidebar to its flat tab list with no
worktrees in it. Treating a dead stamp as no stamp lets
`edmacs-frames-stamp-frame-tabs' re-derive and heal it."
  (and root (file-directory-p root) (edmacs-git-common-dir root) t))

(defun edmacs-frames-stamp-frame-tabs (frame)
  "Stamp every tab of FRAME that carries no `edmacs-root'.
A background tab has no live window to derive from, so each unstamped
tab is selected in turn, stamped, and the original selection restored.
This is the migration path for a desktop written before the stamp was
mandatory: without it every tab reads nil, no repo is resolved,
`edmacs-sessions--backfill-repo-param' leaves `edmacs-repo' unset, and
the sidebar silently falls back to the flat tab list with no worktrees
in it at all."
  (when (frame-live-p frame)
    (with-selected-frame frame
      (let ((original (tab-bar--current-tab-index))
            (count (length (tab-bar-tabs frame))))
        (unwind-protect
            (dotimes (i count)
              (unless (edmacs-frames--tab-root (nth i (tab-bar-tabs frame)))
                (tab-bar-select-tab (1+ i))
                (when-let* ((root (edmacs-frames--derive-root frame)))
                  (edmacs-frames--stamp-current-tab-root root frame))))
          (when (and original (< original count))
            (tab-bar-select-tab (1+ original))))))))

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

(defun edmacs-frames--current-frame-common (frame)
  "Return the git-common-dir FRAME is on.
Prefers FRAME's own `edmacs-repo' parameter; falls back to resolving
the directory of FRAME's own content-window buffer for a frame that
has not been claimed by `edmacs-frames-open' yet (e.g. the daemon's
boot frame). Reads that buffer's `default-directory' via
`buffer-local-value' against FRAME's own content window explicitly --
plain `default-directory' would read whatever buffer happens to be
current in the calling context, and `with-selected-frame' would not
fix that: it does not make FRAME's own selected window's buffer
current (see this file's guard-rail commentary)."
  (or (frame-parameter frame 'edmacs-repo)
      (edmacs-frames--repo-of
       (buffer-local-value 'default-directory
                           (window-buffer (edmacs-frames--frame-content-window frame))))))

(defun edmacs-frames--read-worktree (prefix frame)
  "Read a worktree directory, interactively, for FRAME.
With no PREFIX, offer a `completing-read' over FRAME's own repo
worktrees -- the main worktree plus `vc-git-known-other-working-trees'
-- since worktrees are deliberately absent from
`project-known-project-roots' (see core.el), making this the only
prompt that reaches them. With PREFIX, fall back to
`read-directory-name' for any directory."
  (if prefix
      (read-directory-name "Open worktree in its repo frame: ")
    (let ((common (edmacs-frames--current-frame-common frame)))
      (if (not common)
          (read-directory-name "Open worktree in its repo frame: ")
        (completing-read "Worktree: " (edmacs-frames--repo-worktrees common) nil t)))))

(defun edmacs-frames-open-worktree-tab (dir)
  "Open or raise a tab for DIR's worktree, in DIR's repo's frame.
Returns that frame, or nil when none could be created for DIR's repo.
Never touches the calling frame: `edmacs-frames-open' raises or creates
DIR's own repo frame first, and the tab search/creation below happens
entirely inside that frame. A tab already showing DIR is selected, not
duplicated; this is the primary duplicate-prevention rule -- see this
module's Commentary on `edmacs-frames--reconcile-tab-after-open' for
the safety net covering tabs opened through any other route."
  (interactive (list (edmacs-frames--read-worktree current-prefix-arg (selected-frame))))
  (let* ((root (file-truename dir))
         (frame (edmacs-frames-open dir)))
    ;; `edmacs-frames-open' returns nil (having warned) when no frame could
    ;; be made for DIR's repo; `with-selected-frame' would signal on that,
    ;; and on a frameless daemon that signal exits Emacs 255 (see core.el).
    (when frame
      (edmacs-frames--without-display-override
        (with-selected-frame frame
          (let ((tab (edmacs-frames--find-tab-by-root root frame)))
            (if tab
                (tab-bar-select-tab (1+ (tab-bar--tab-index tab (tab-bar-tabs frame) frame)))
              ;; The post-open hook stamps ROOT from this binding, so
              ;; reconciliation sees the INTENDED root and folds a same-root
              ;; tab the lookup above missed instead of duplicating it.
              (let ((edmacs-frames--pending-tab-root root))
                (tab-bar-new-tab))
              (edmacs-frames--visit-root dir)
              (tab-bar-rename-tab (file-name-nondirectory (directory-file-name dir))))))))
    frame))

(defun edmacs-frames--reconcile-tab-after-open (tab)
  "Fold TAB back onto an existing same-root tab in `(selected-frame)'.
The safety net for a tab created through any route other than
`edmacs-frames-open-worktree-tab' -- the stock `project-other-tab-command'
prefix, `M-x tab-bar-new-tab', `other-tab-prefix' from any package.
TAB is already stamped by the time this runs (see
`edmacs-frames--on-tab-post-open'); this only closes TAB itself when
another tab in the frame already carries that root -- safe because, at
post-open time, TAB is unambiguously the one just added: closing it,
never an older tab, cannot surprise a user mid-edit in some other tab."
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
      (tab-bar-close-tab tab-number))))

(defun edmacs-frames--on-tab-post-open (tab)
  "Stamp TAB's `edmacs-root', then fold it onto an existing same-root tab.
This module's single `tab-bar-tab-post-open-functions' entry. Stamping
has to happen before reconciliation, and hook ordering across frames.el,
windows.el and sidebar.el is not a guarantee, so the two are one
function rather than two entries.

The root comes from `edmacs-frames--pending-tab-root' when this module
asked for the tab, else from the selected frame's own content window --
but only when TAB really IS the selected frame's current tab.
`tab-bar-tabs' also runs this hook for a default tab it auto-creates on
a frame it never names, and stamping that one from the selected frame's
buffer would reintroduce the cross-frame derivation this module removed."
  ;; `tab-bar-tab-post-open-functions' calls with (TAB), no frame slot --
  ;; ambient-reads: ok
  (when (eq tab (edmacs-frames--current-tab (selected-frame)))
    (when-let* ((root (or edmacs-frames--pending-tab-root
                          (edmacs-frames--derive-root (selected-frame))))) ;; ambient-reads: ok
      (setf (alist-get 'edmacs-root (cdr tab)) root)))
  (edmacs-frames--reconcile-tab-after-open tab))

(add-hook 'tab-bar-tab-post-open-functions #'edmacs-frames--on-tab-post-open)

(defun edmacs-frames--on-tab-post-select (_from _to)
  "Stamp the newly selected tab's root when it still carries none.
The repair path for a tab restored from a desktop file written before
the stamp was mandatory. The tab is looked up fresh rather than trusting
the hook's own TO argument: `tab-bar-select-tab' rebuilds the incoming
tab's cons before running this hook, so TO is not the object now in the
frame's tab list."
  ;; `tab-bar-tab-post-select-functions' calls with (FROM TO), no frame
  ;; slot -- ambient-reads: ok
  (let* ((frame (selected-frame))
         (tab (edmacs-frames--current-tab frame)))
    (when (and tab (not (edmacs-frames--tab-root tab)))
      (when-let* ((root (edmacs-frames--derive-root frame)))
        (setf (alist-get 'edmacs-root (cdr tab)) root)))))

(add-hook 'tab-bar-tab-post-select-functions #'edmacs-frames--on-tab-post-select)

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

(defun edmacs-frames--reset-to-spare (frame)
  "Reset FRAME to an adoptable spare: no repo, no name, one scratch window.
Clears the frame state FIRST and rebuilds the windows LAST, so a failure
in the window work can no longer strand `edmacs-repo', the frame name and
the tab name half-cleared.

The window reset goes through `edmacs-windows-repair-frame' rather than
`switch-to-buffer' + `delete-other-windows': called with the sidebar
selected, that pair put *scratch* into a right side window (a
non-interactive `switch-to-buffer' on a dedicated window falls through to
`pop-to-buffer') and then signalled \"Cannot make side window the only
window\". Repairing first guarantees a non-side, undedicated target, which
makes that signal structurally unreachable; the sidebar the repair hook
re-shows survives `delete-other-windows' via `no-delete-other-windows',
leaving FRAME in the normal shape the next `edmacs-frames-open' adopts."
  (set-frame-parameter frame 'edmacs-repo nil)
  (set-frame-parameter frame 'name nil)
  (tab-bar-rename-tab "emacs")
  (condition-case err
      (with-selected-frame frame
        (let ((main (or (edmacs-windows-repair-frame frame) (edmacs-main-window))))
          (when (window-live-p main)
            (set-window-buffer main (get-buffer-create "*scratch*"))
            (select-window main)
            (delete-other-windows main))))
    (error
     (display-warning 'edmacs-frames
                      (format "reset-to-spare: %s" (error-message-string err))
                      :warning))))

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
  ;; `tab-bar-close-last-tab-choice' calls with (TAB), no frame slot --
  ;; ambient-reads: ok
  (let* ((frame (selected-frame)))
    (edmacs-frames--maybe-teardown-watch-for-frame frame)
    (if (edmacs-frames--only-frame-p frame)
        (edmacs-frames--reset-to-spare frame)
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

;; ============================================================================
;; Fullscreen -- every graphical frame opens fullscreen
;; ============================================================================

;; The load-bearing knob for "one Space per frame": with it nil the NS port
;; fakes fullscreen by resizing the window inside the current Space instead.
;; Already the default; pinned so the contract lives in source.
(defvar ns-use-native-fullscreen)
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t))

(defgroup edmacs-frames nil
  "One frame per repo."
  :group 'convenience)

(defcustom edmacs-frames-fullscreen 'fullboth
  "The `fullscreen' frame parameter every new graphical frame is given.
Nil disables the policy. `fullboth' is the portable value: the NS port
maps it to native macOS fullscreen, the X11/PGTK ports to EWMH's
`_NET_WM_STATE_FULLSCREEN'."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Fullscreen" fullboth)
                 (const :tag "Maximized" maximized)
                 (const :tag "Full width" fullwidth)
                 (const :tag "Full height" fullheight))
  :group 'edmacs-frames)

(defun edmacs-frames--fullscreen-target (frame)
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
  (and edmacs-frames-fullscreen
       (frame-live-p frame)
       (display-graphic-p frame)
       (not (frame-parameter frame 'parent-frame))
       (not (eq (frame-parameter frame 'fullscreen) edmacs-frames-fullscreen))
       edmacs-frames-fullscreen))

(defun edmacs-frames-apply-fullscreen (frame)
  "Put FRAME fullscreen per `edmacs-frames-fullscreen'.
Deferred to a zero-delay timer that re-checks the target, for the same
reason `edmacs-sessions--restore-pending-frameset' defers its own work
\(sessions.el): a frame is not fully mapped while its own creation hook
is still running, and the NS port drops a fullscreen toggle sent to an
unmapped window. Never signals -- an error reaching a frameless
daemon's top level exits it 255 (see core.el)."
  (when (edmacs-frames--fullscreen-target frame)
    (run-at-time
     0 nil
     (lambda ()
       (condition-case err
           (when-let* ((target (edmacs-frames--fullscreen-target frame)))
             (set-frame-parameter frame 'fullscreen target))
         (error
          (display-warning 'edmacs-frames
                           (format "could not fullscreen frame: %s" err)
                           :warning)))))))

(add-hook 'after-make-frame-functions #'edmacs-frames-apply-fullscreen)

(defun edmacs-frames--apply-fullscreen-at-startup ()
  "Apply the fullscreen policy to every already-live graphical frame.
`after-make-frame-functions' never fires for a non-daemon Emacs's
initial frame, which on a plain `emacs' start is the only frame there
is; under the daemon this finds nothing and the hook above covers the
boot frame instead."
  (dolist (frame (frame-list))
    (edmacs-frames-apply-fullscreen frame)))

(add-hook 'emacs-startup-hook #'edmacs-frames--apply-fullscreen-at-startup)

(provide 'frames)
;;; frames.el ends here
