;;; sessions.el --- Tabs, layout persistence, and worktree switching -*- lexical-binding: t -*-

;;; Commentary:
;; Replaces the tmux session layer with built-in Emacs 31 primitives:
;;   - `tab-bar-mode': one tab per active rdm worktree.
;;   - `desktop-save-mode': persists each tab's window layout across restarts.
;;     Under a daemon, desktop.el skips frameset restore; see the bridge below.
;;   - `bufferlo': per-tab buffer lists, which desktop.el does not persist.
;;
;; Worktree switching uses vc.el's own `vc-switch-working-tree' and
;; `vc-working-tree-switch-project'. Their `C-x v w ...' chords are shadowed
;; in evil normal state by `evil-numbers/dec-at-pt'; this module registers
;; them through `edmacs-evil-config-add-c-x-chord' rather than redefining
;; `C-x' (which would depend on module load order). `SPC T' and `SPC p w'
;; are the no-delay path to the same commands.
;;
;; A tab is scoped to one frame, and `modules/frames.el' now gives each
;; repo exactly one frame -- checked upstream on 2026-09-01 against the two
;; bugs an earlier design avoided frames over: manzaltu#197 is a bug in
;; claude-code-ide.el's own terminal reflow filter, a package this config
;; never loads, and ghostel#504 was closed as fixed. The constraint that
;; remains is `window-adjust-process-window-size-smallest', which ghostel
;; uses to size its PTY: never show one terminal buffer in two frames at
;; once. Frame-per-repo never does that, so it satisfies the constraint by
;; construction rather than by avoiding frames altogether.

;;; Code:

;; ============================================================================
;; Tab Bar - one tab per active worktree
;; ============================================================================

(require 'tab-bar)

;; Must be nil before `tab-bar-mode' turns on (its :set installs bindings
;; immediately). Keeps C-<tab>/C-S-<tab> free for evil; tab switching is
;; under `SPC T'.
(setq tab-bar-define-keys nil)

;; Available via init.el's `load-module' order, not a `require'.
(declare-function edmacs-git-common-dir "git-common-dir")
(declare-function edmacs-stack-sweep-stale-panes "windows")
(declare-function edmacs-git-common-dir-repo-name "git-common-dir")
(declare-function edmacs-frames-tab-in-own-repo-p "frames")
(declare-function edmacs-frames--tab-root "frames")
(declare-function edmacs-frames--repo-of "frames")
(declare-function edmacs-frames--ensure-repo-tracking "frames")
(declare-function edmacs-frames-frame-usable-p "frames")
(declare-function edmacs-frames-stamp-frame-tabs "frames")
(declare-function edmacs-frames-tab-root-live-p "frames")
(declare-function edmacs-sidebar-show "sidebar")
(declare-function edmacs-sidebar--window "sidebar")

(defun edmacs-sessions--tab-name ()
  "Name the current tab after its project/worktree, falling back sanely.
Uses `project-current' so each tab's label reflects the worktree it
holds; when no project is found (e.g. a scratch tab), falls back to
`tab-bar-tab-name-current' default behavior (buffer name of the
selected window).

Two worktrees of *different* repositories can share a directory
basename (e.g. both named `feature-x', or two rdm worktrees named
`roadmap-foundation' from two different rdm projects), which a bare
basename would render as identical, ambiguous tab names. Disambiguate
by prefixing the owning repository's own directory name, derived from
`edmacs-git-common-dir' (shared by every worktree of one repo, so it
names the repo rather than the worktree) -- except when that repo is
already the one the selected frame itself carries (`modules/frames.el's
`edmacs-repo' parameter): the frame's own title already disambiguates
it, so a tab inside it need only name its worktree. A tab whose repo is
some *other* one -- a foreign-project scratch tab, which `frames.el's
stray-visit relocator should make rare -- still gets the prefix."
  (if-let* ((proj (project-current))
            (root (project-root proj)))
      (let* ((base (file-name-nondirectory (directory-file-name root)))
             (common (edmacs-git-common-dir root)))
        (if (edmacs-frames-tab-in-own-repo-p common)
            base
          (let ((repo (and common (edmacs-git-common-dir-repo-name common))))
            (if (and repo (not (string= repo base)))
                (format "%s/%s" repo base)
              base))))
    (tab-bar-tab-name-current)))

(setq tab-bar-tab-name-function #'edmacs-sessions--tab-name)

(tab-bar-mode 1)

;; ============================================================================
;; Desktop - persist tab/window layout across restarts
;; ============================================================================

(require 'desktop)

(setq desktop-dirname (expand-file-name ".cache/desktop/" user-emacs-directory)
      desktop-path (list desktop-dirname)
      desktop-save t
      desktop-restore-frames t
      desktop-load-locked-desktop t
      ;; bufferlo persists every tab's full buffer list; restoring dozens of
      ;; buried buffers eagerly would block daemon startup.
      desktop-restore-eager 10)

(unless (file-directory-p desktop-dirname)
  (make-directory desktop-dirname t))

;; These buffers front live subprocesses desktop.el cannot reattach; a
;; restored one would be a dead transcript. comint-mode is built in, so it
;; needs no `with-eval-after-load' gate.
(add-to-list 'desktop-modes-not-to-save 'comint-mode)
(with-eval-after-load 'vterm
  (add-to-list 'desktop-modes-not-to-save 'vterm-mode))

;; claude-term sessions front a live `claude' CLI process under ghostel and
;; must not be restored either -- but they cannot be excluded via
;; `desktop-modes-not-to-save', which matches the MAJOR mode. Their major
;; mode is `ghostel-mode', shared with ordinary ghostel shell terminals that
;; ghostel CAN respawn (ghostel sets `desktop-save-buffer' buffer-locally and
;; registers its own `ghostel-desktop-restore-buffer'), so blacklisting
;; `ghostel-mode' would break working restore for those; and the marker minor
;; mode `claude-term-mode' would simply never match. Opt out buffer-locally
;; instead, which scopes the exclusion to exactly the claude-term buffers:
;; `ghostel-mode's body sets `desktop-save-buffer', then run-mode-hooks
;; enables `claude-term-mode', whose hook clears it again.
(add-hook 'claude-term-mode-hook
          (lambda ()
            (when (bound-and-true-p claude-term-mode)
              (setq-local desktop-save-buffer nil))))

;; desktop restores each buffer's minor modes by calling them. mise-mode
;; shells out to mise, and an error during restore in a frameless daemon is
;; fatal (Emacs exits 255). `global-mise-mode' re-enables it anyway.
(add-to-list 'desktop-minor-mode-table '(mise-mode nil))

;; Frame colours come from the theme, never from a saved frameset: a desktop
;; written by a frame that had the wrong colours would otherwise stamp them
;; back over the theme on every restore.
(dolist (param '(background-color foreground-color cursor-color mouse-color))
  (push (cons param :never) frameset-filter-alist))

;; `edmacs-repo' (frames.el) already round-trips with the default
;; pass-through action -- it is simply absent from this alist. Pinned
;; explicitly, alongside the colour filters above, so the contract is
;; visible in source rather than an accident of frameset.el's default.
(push (cons 'edmacs-repo nil) frameset-filter-alist)

;; `edmacs-sidebar-collapsed' (sidebar.el) is a per-frame boolean, not a
;; colour/geometry parameter -- it needs the same explicit pass-through
;; pin as `edmacs-repo' above, not the colour filters' `:never'.
(push (cons 'edmacs-sidebar-collapsed nil) frameset-filter-alist)

;; A restored frame lands on the current display instead of replaying the
;; coordinates of whichever monitor it was saved on; frames.el's fullscreen
;; policy then sizes it there. `width'/`height' need no filter of their own --
;; `frameset--restore-frame' already drops both (and `visibility') from the
;; config of any frame saved carrying a `fullscreen' parameter.
(dolist (param '(left top))
  (push (cons param :never) frameset-filter-alist))

(desktop-save-mode 1)

;; ----------------------------------------------------------------------------
;; Bridge desktop-read's daemon-mode frameset skip to the first client frame.
;; `desktop-restoring-frameset-p' refuses to restore onto the daemon's
;; placeholder frame, nothing retries once a client attaches, and
;; `desktop-read' nils `desktop-saved-frameset' right after
;; `desktop-after-read-hook'. So stash it from that hook and replay it on
;; the first GUI frame; `desktop-restore-reuses-frames' (default t) makes it
;; reuse that frame rather than pop a new one.
(require 'server)

(defvar edmacs-sessions--pending-frameset nil
  "Desktop frameset stashed at daemon boot, awaiting the first client frame.
Non-nil only between a daemon's `desktop-read' (which cannot restore
frames onto its placeholder initial frame) and the first `emacsclient'
frame attaching.")

(defun edmacs-sessions--frameset-has-frames-p (fs)
  "Return non-nil when FS is a frameset carrying at least one frame state.
An *empty* frameset is the poison this guards against, on both the save
and the restore side. `desktop--check-dont-save' deliberately excludes
the daemon's initial frame, so a session ending with no GUI frame writes
a frameset object with zero states -- still a non-nil object, so a bare
non-nil test happily stashes it. Handing that to `frameset-restore' is
destructive rather than inert: `:reuse-frames t' marks every live frame
`:ignored', no state ever reassigns one, and the `:cleanup-frames t'
pass then deletes every ignored frame except the daemon's initial one --
including the frame the restore was handed. The session is left with no
GUI frame, so its own next save is empty too, and the loop perpetuates
itself across restarts."
  (and (frameset-p fs) (consp (frameset-states fs)) t))

(defun edmacs-sessions--stash-frameset-for-daemon ()
  "Stash `desktop-saved-frameset' when daemon boot skipped restoring it.
Runs on `desktop-after-read-hook', which fires after the frameset is
loaded but before `desktop-read' unconditionally nils it back out."
  (when (and (daemonp)
             (edmacs-sessions--frameset-has-frames-p desktop-saved-frameset)
             (not (desktop-restoring-frameset-p)))
    (setq edmacs-sessions--pending-frameset desktop-saved-frameset)))

(add-hook 'desktop-after-read-hook #'edmacs-sessions--stash-frameset-for-daemon)

;; ----------------------------------------------------------------------------
;; Multi-frame finish-up after `frameset-restore': `frameset-restore' (via
;; `desktop-restore-frameset') only reuses/creates frames and replays their
;; window/tab layout -- it knows nothing about `edmacs-repo', frame titles,
;; or sidebars, all of which `edmacs-frames-open' would normally set up for
;; a freshly opened repo frame. This walks every live frame afterward and
;; back-fills each.

(defun edmacs-sessions--frame-tab-roots (frame)
  "Return the list of repos every tab in FRAME resolves to, or nil.
Nil both when FRAME has no tabs and when any tab's root fails to
resolve to a repo -- callers must not treat either case as \"every tab
agrees\".

`edmacs-frames--tab-root' is a pure read of the tab's own stamp and
`tab-bar-tabs' is passed FRAME explicitly, so nothing here consults the
global selection any more; the `with-selected-frame' wrapper is kept
purely defensively, so a future callee that does still sees FRAME
rather than whatever the caller happened to have selected."
  (with-selected-frame frame
    (let (roots)
      (catch 'edmacs-sessions--unresolved
        (dolist (tab (tab-bar-tabs frame))
          (let* ((root (edmacs-frames--tab-root tab))
                 (repo (and root (edmacs-frames--repo-of root))))
            (unless repo (throw 'edmacs-sessions--unresolved nil))
            (push repo roots)))
        (nreverse roots)))))

(defun edmacs-sessions--drop-dead-tab-roots (frame)
  "Clear FRAME's tab stamps that point at a directory which is gone.
A stamp is not self-validating: a tab restored from a desktop keeps
pointing at whatever worktree it was saved on, and that worktree may
since have been removed. `edmacs-git-common-dir' returns nil for a path
that no longer exists, and `edmacs-sessions--frame-tab-roots' turns one
unresolvable tab into nil for the whole frame -- so a single stale tab
leaves `edmacs-repo' unset and drops the sidebar to its flat tab list
with no worktrees in it at all. Clearing the dead stamp lets
`edmacs-frames-stamp-frame-tabs', which runs straight after this,
re-derive a live one from the tab's own window."
  (when (frame-live-p frame)
    (dolist (tab (tab-bar-tabs frame))
      (let ((root (alist-get 'edmacs-root tab)))
        (when (and root (not (edmacs-frames-tab-root-live-p root)))
          (setf (alist-get 'edmacs-root tab nil t) nil))))))

(defun edmacs-sessions--backfill-repo-param (frame)
  "Set FRAME's `edmacs-repo' from its tabs when it has none yet.
Only when every tab's own resolved repo agrees -- a frame with no tabs,
or whose tabs point at different repos, is left alone rather than
guessed at."
  (unless (frame-parameter frame 'edmacs-repo)
    (when-let* ((roots (edmacs-sessions--frame-tab-roots frame))
                (first (car roots)))
      (when (seq-every-p (lambda (r) (equal r first)) roots)
        (set-frame-parameter frame 'edmacs-repo first)))))

(defun edmacs-sessions--regenerate-frame-title (frame)
  "Regenerate FRAME's title from its `edmacs-repo' parameter.
`frameset-filter-alist' marks `name' `:never' (frameset.el's own
`frame-internal-parameters' list), so a saved title is never restored
and must be recomputed here, the same way `edmacs-frames-open' sets it
on first creation. When COMMON's directory is gone, marks the frame
`edmacs-repo-missing' and warns instead of erroring -- see AC3.

Also re-derives the current tab's own label via `edmacs-sessions--tab-name'
-- never hardcoded to the bare repo label, which would be correct only
for a tab on the repo's main worktree and would clobber any other
worktree tab's disambiguating name -- so a tab named before this
frame's `edmacs-repo' was backfilled (and thus still carrying a
now-redundant prefix) gets relabeled consistently with every tab
`frames.el' creates going forward.

`with-selected-frame' alone does not make `edmacs-sessions--tab-name''s
`project-current' read FRAME's own tab: `project-current' resolves via
`default-directory', a buffer-local variable that tracks *current
buffer*, and selecting a frame never changes that (confirmed live: a
frame's selected window keeps showing its own buffer while
`current-buffer' stays whatever the caller's -- here, a restore timer's
-- own buffer happened to be). Multi-frame restore called this once per
restored frame from the same timer callback, so every frame after the
first got its current tab renamed from some OTHER frame's (or the
timer's ambient) project instead of its own -- reproduced live via a
real multi-frame daemon restart, fixed by explicitly making the
frame's own selected window's buffer current too."
  (when-let* ((common (frame-parameter frame 'edmacs-repo)))
    (if (file-directory-p common)
        (progn
          (set-frame-parameter frame 'edmacs-repo-missing nil)
          (set-frame-parameter frame 'name (edmacs-git-common-dir-repo-name common))
          (with-selected-frame frame
            (with-current-buffer (window-buffer (selected-window))
              (ignore-errors (tab-bar-rename-tab (edmacs-sessions--tab-name))))))
      (set-frame-parameter frame 'edmacs-repo-missing t)
      (set-frame-parameter
       frame 'name (format "MISSING: %s" (edmacs-git-common-dir-repo-name common)))
      (display-warning
       'edmacs-sessions
       (format "Restored frame's repo no longer exists: %s" common)
       :warning))))

(defun edmacs-sessions--ensure-sidebar (frame)
  "Show or refresh FRAME's sidebar.
Always calls through to `edmacs-sidebar-show': `display-buffer-in-side-
window' reuses an existing matching side window rather than duplicating
it, so this is cheap even when sidebar.el's own `after-make-frame-
functions' hook already displayed one -- their relative ordering rests
on same-tick `run-at-time 0' registration order, not a documented
guarantee, so this is defense-in-depth either way.

Must not skip the call just because `edmacs-sidebar--window' already
finds one: `edmacs-sidebar--on-desktop-read' (sidebar.el) shows every
frame's sidebar synchronously at desktop-read time, before this
function's caller has regenerated FRAME's real title from its
`edmacs-repo' -- reproduced live, that race first-names the sidebar
buffer after the daemon's generic default frame name, and only
`edmacs-sidebar-show' (via `edmacs-sidebar--ensure-buffer''s rename-if-
stale check) ever revisits it to fix that."
  (edmacs-sidebar-show frame))

(defun edmacs-sessions--ensure-worktree-tracking (frame)
  "Warm FRAME's repo worktree cache and arm its file-notify watch.
`edmacs-frames--worktrees-cache' and `-watches' (frames.el) both start
empty on every daemon boot, so a restored repo frame's sidebar would
otherwise render an empty worktree list and never see a live update
until something else happens to touch that repo -- unlike a frame
`edmacs-frames-open' creates itself, which always warms both as part of
opening. Skipped for a missing repo (`edmacs-repo-missing'): nothing
in `edmacs-frames--ensure-repo-tracking' needs to shell out for a
directory that no longer exists."
  (when-let* ((common (frame-parameter frame 'edmacs-repo)))
    (when (file-directory-p common)
      (edmacs-frames--ensure-repo-tracking common))))

(defun edmacs-sessions--restorable-frame-p (frame)
  "Return non-nil when the restore walk may claim FRAME as a repo frame.
Delegates to `edmacs-frames-frame-usable-p', which mirrors
`desktop--check-dont-save''s own exclusion of the daemon's initial tty
placeholder: that frame is in `frame-list' but in no desktop save, is
never on screen, and must never be stamped with an `edmacs-repo',
renamed after a repo, given a sidebar side window, or made to hold a
`file-notify' worktree watch for a repo it can never display."
  (and (frame-live-p frame)
       (edmacs-frames-frame-usable-p frame)))

(defun edmacs-sessions--finish-frameset-restore ()
  "Back-fill tab roots, `edmacs-repo', title, tracking and sidebar per frame.
Runs synchronously right after `desktop-restore-frameset', by which
point `frameset-restore''s own `:reuse-frames t' (the default) has
already reused/created every saved frame -- this never itself creates or
deletes a frame. Frames `edmacs-sessions--restorable-frame-p' rejects
are skipped entirely.

Tab roots are stamped FIRST: `edmacs-sessions--backfill-repo-param'
resolves a frame's repo from its tabs' own roots, which for a tab
restored from a desktop file written before the stamp was mandatory are
only there once `edmacs-frames-stamp-frame-tabs' has written them."
  (dolist (frame (frame-list))
    (when (edmacs-sessions--restorable-frame-p frame)
      (edmacs-sessions--drop-dead-tab-roots frame)
      (edmacs-frames-stamp-frame-tabs frame)
      (edmacs-sessions--backfill-repo-param frame)
      (edmacs-sessions--regenerate-frame-title frame)
      (edmacs-sessions--ensure-worktree-tracking frame)
      (edmacs-sessions--ensure-sidebar frame))))

(defun edmacs-sessions--gui-frame-parameters ()
  "Parameters `edmacs-sessions--make-gui-frame' creates a frame with.
A daemon's `window-system' is nil, so the window system has to be named
explicitly or `make-frame' produces another tty placeholder rather than
the graphical frame every caller here is asking for. Only the macOS
daemon this config actually runs under needs naming; elsewhere the
ambient default already yields a graphical frame."
  (and (eq system-type 'darwin) '((window-system . ns))))

(defun edmacs-sessions--make-gui-frame ()
  "Create a graphical frame, returning nil rather than signalling on failure.
Never let an error out: under a frameless daemon one reaching top level
exits Emacs 255 (see core.el). The previous `ignore-errors' here made
that failure silent as well as survivable, which is how a boot frame
deleted out from under the daemon went unnoticed -- warn instead."
  (condition-case err
      (make-frame (edmacs-sessions--gui-frame-parameters))
    (error
     (display-warning 'edmacs-sessions
                      (format "could not create a GUI frame: %s" err)
                      :warning)
     nil)))

(defun edmacs-sessions--ensure-gui-frame ()
  "Create a graphical frame when the session has none left.
The net under every path that can end with a frameless daemon -- most
of all `frameset-restore', which deletes the very frame it was handed
when the frameset it replays has no state to reassign to it. A daemon
with no GUI frame saves an empty frameset, which poisons its own next
boot (see `edmacs-sessions--frameset-has-frames-p'), so restoring one
here is what keeps a single bad restore from becoming permanent."
  (unless (seq-find (lambda (f) (and (frame-live-p f) (display-graphic-p f)))
                    (frame-list))
    (edmacs-sessions--make-gui-frame)))

(defun edmacs-sessions--restore-pending-frameset (frame)
  "Restore a daemon-boot-stashed frameset onto FRAME, the first GUI frame.
Runs from `after-make-frame-functions' so it covers the boot frame,
emacsclient frames, and the Dock's reopen event alike.
`desktop-restore-reuses-frames' (default t) reuses FRAME. Deferred by a
timer so the frame is fully created before frameset-restore touches it."
  (when (and (edmacs-sessions--frameset-has-frames-p
              edmacs-sessions--pending-frameset)
             (display-graphic-p frame))
    (let ((frameset edmacs-sessions--pending-frameset))
      (setq edmacs-sessions--pending-frameset nil)
      (run-at-time 0 nil
                   (lambda ()
                     ;; Both the restore and the sweep run guarded: an error
                     ;; escaping here would skip the net below and leave the
                     ;; daemon frameless, which is the state that saves an
                     ;; empty frameset and poisons the next boot.
                     (condition-case err
                         (when (frame-live-p frame)
                           (let ((desktop-saved-frameset frameset))
                             (with-selected-frame frame
                               (desktop-restore-frameset))
                             ;; FRAME is gone whenever `frameset-restore''s
                             ;; cleanup pass found no saved state to reassign
                             ;; to it -- the sweep would signal on it.
                             (when (frame-live-p frame)
                               ;; An agent pane's process cannot survive a
                               ;; restart and a popup's buffer may not have
                               ;; been saved at all; sweep those stale right
                               ;; stack windows rather than show them.
                               (edmacs-stack-sweep-stale-panes frame))))
                       (error
                        (display-warning 'edmacs-sessions
                                         (format "frameset restore failed: %s" err)
                                         :warning)))
                     ;; Outside the guard above, and before the back-fill, so
                     ;; a frame created here to replace one `frameset-restore'
                     ;; deleted gets its sidebar and title like any other.
                     (edmacs-sessions--ensure-gui-frame)
                     (edmacs-sessions--finish-frameset-restore))))))

(add-hook 'after-make-frame-functions #'edmacs-sessions--restore-pending-frameset)

;; ----------------------------------------------------------------------------
;; Keep the daemon owned by the Dock's Emacs.app tile. Emacs becomes a regular
;; Dock app only once it has a visible frame, and drops out again (activation
;; policy Prohibited) when its last NS frame is deleted; after that a Dock
;; click launches a second Emacs. So the daemon opens one frame at boot, and
;; closing the last window hides Emacs (what s-h does) instead of deleting the
;; frame. A Dock click then unhides it with the layout intact.
(defun edmacs-ns-close-frame (&optional frame)
  "Close FRAME, hiding Emacs instead when it is the last visible GUI frame.
Under the daemon a deleted last frame would drop Emacs out of the Dock."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (if (and (daemonp)
             (display-graphic-p frame)
             (= 1 (length (seq-filter (lambda (f) (and (display-graphic-p f)
                                                       (frame-visible-p f)))
                                      (frame-list)))))
        (ns-do-hide-emacs)
      (delete-frame frame t))))

(defgroup edmacs-sessions nil
  "Tabs, layout persistence, and the launchd daemon's lifecycle."
  :group 'convenience)

;; Dock-launch-while-daemon-down decision (daemon-and-Dock-frame phase, AC3):
;; an `emacsclient -c -a ""' wrapper .app was considered and rejected. It
;; would need to replace the Dock's Emacs tile, which moves Dock/LaunchServices
;; identity onto the wrapper's own client processes and defeats Finding 1 --
;; the daemon can no longer piggyback on the OS's native single-instance/
;; hide-show behavior once something else owns the tile. Instead: unconditional
;; `KeepAlive' (below) keeps the daemon relaunching on any exit or crash, and
;; `edmacs-sessions--warn-on-shadow-daemon-process' surfaces -- non-fatally,
;; detection only -- the case that slips through anyway: a Dock click landing
;; between login and the daemon's first `server-start', or during launchd's
;; backoff after a crash-loop.
(defcustom edmacs-sessions-launchd-service "emacs-plus@31"
  "Homebrew service name for the launchd-managed Emacs daemon.
Read only by `edmacs-stop-daemon'.  The plist this names sets
`KeepAlive' unconditionally, which is what makes a plain `kill-emacs' a
restart rather than a quit -- see `edmacs-quit' and
`edmacs-restart-daemon'."
  :type 'string
  :group 'edmacs-sessions)

;; The three commands below exist because `KeepAlive' is unconditional in
;; homebrew.mxcl.emacs-plus@31.plist: launchd relaunches the daemon on ANY
;; exit, so `save-buffers-kill-terminal' -- which reaches
;; `save-buffers-kill-emacs' here, the boot frame having no `client'
;; parameter to send it down the delete-frame branch instead -- behaves as
;; a restart no matter what it is bound to. Each of the three intents
;; therefore needs its own command; they are not interchangeable.

(defun edmacs-quit ()
  "Close the selected frame, leaving the daemon and its session running.
The daemon-native meaning of \"quit\": under launchd nothing here can
end the Emacs process without it being relaunched, and the session is
the point of running a daemon at all. Delegates to
`edmacs-ns-close-frame', so this is the same path the window close
button takes -- the last visible GUI frame hides Emacs rather than being
deleted, and a Dock click brings the layout straight back. Outside a
daemon there is no such distinction, so fall through to the stock
`save-buffers-kill-terminal'."
  (interactive)
  (if (and (daemonp) (display-graphic-p))
      (edmacs-ns-close-frame)
    (save-buffers-kill-terminal)))

(defun edmacs-restart-daemon (&optional arg)
  "Restart the Emacs daemon: save, exit, and let launchd relaunch it.
Under this service's unconditional `KeepAlive', exiting IS the restart --
launchd starts a fresh daemon and `edmacs-sessions--restore-pending-frameset'
puts the layout back, `desktop-save-mode' having written it from
`kill-emacs-hook' on the way out. Prompts to save modified buffers
first; a prefix ARG is passed through to `save-some-buffers' to save
them all without asking. Outside a daemon there is nothing to relaunch,
so defer to `restart-emacs', which spawns a replacement itself."
  (interactive "P")
  (if (daemonp)
      (progn (save-some-buffers arg) (kill-emacs))
    (restart-emacs)))

(defun edmacs-stop-daemon ()
  "Stop the launchd Emacs service so it stays down.
`brew services stop' both signals the daemon and unloads the job, so
launchd does not relaunch it and it does not come back at next login --
the only thing here that really quits. Starting Emacs again then needs
`brew services start' from a terminal, which is why this is the
confirm-first, least-reachable of the three.

Run through `call-process' with DESTINATION 0: that forks the command
without Emacs waiting on or tracking the child, so `brew' survives long
enough to kill us. A tracked `start-process' child would be sent SIGHUP
by `kill-emacs' as the daemon tore down, potentially before the service
was unloaded -- leaving it stopped but still loaded, and back at next
login."
  (interactive)
  (let ((brew (or (executable-find "brew") "/opt/homebrew/bin/brew")))
    (unless (file-executable-p brew)
      (user-error "edmacs-stop-daemon: no `brew' executable found at %s" brew))
    (when (yes-or-no-p
           (format "Stop the %s service? Emacs will not come back on its own. "
                   edmacs-sessions-launchd-service))
      (save-some-buffers)
      (call-process brew nil 0 nil
                    "services" "stop" edmacs-sessions-launchd-service))))

(defun edmacs-ns-handle-delete-frame (event)
  "Handle the window close button EVENT via `edmacs-ns-close-frame'."
  (interactive "e")
  (edmacs-ns-close-frame (posn-window (event-start event))))

(defun edmacs-sessions--shadow-daemon-processes ()
  "Return PIDs of other running processes sharing this daemon's executable.
Finding 1 of the daemon-and-Dock-frame phase: the launchd daemon and a
Dock-launched Emacs.app are the very same binary, so a second process
running it is exactly the silent-drift failure (a Dock click launching a
serverless Emacs instead of raising this one) that phase exists to catch.
Shells out to `pgrep' rather than `list-system-processes' +
`process-attributes', which on macOS cannot read another process's
command line without it being owned by the same user, and even then not
reliably for an app-bundle launch."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (self (number-to-string (emacs-pid)))
         (pgrep (executable-find "pgrep")))
    (when pgrep
      (with-temp-buffer
        (call-process pgrep nil t nil "-f" (regexp-quote exe))
        (seq-remove (lambda (pid) (string= pid self))
                    (split-string (buffer-string) "\n" t))))))

(defun edmacs-sessions--warn-on-shadow-daemon-process ()
  "Warn (non-fatally) if another process shares this daemon's executable.
Detection only, by design: never signals or kills the other process,
since telling a stale/duplicate instance from a legitimate one apart
safely is out of scope for an automatic action -- see the launchd-vs-
wrapper-.app tradeoff recorded above `edmacs-sessions-launchd-service'.
Errors are swallowed rather than surfaced: this is a best-effort warning
running from `emacs-startup-hook', not a boot-critical check."
  (when-let* ((pids (ignore-errors (edmacs-sessions--shadow-daemon-processes))))
    (display-warning
     'edmacs-sessions
     (format "another process (pid%s %s) is running this daemon's own \
executable -- possibly a Dock-launched Emacs.app instead of/alongside \
the launchd daemon; see Finding 1 of the daemon-and-Dock-frame phase"
             (if (cdr pids) "s" "") (string-join pids ", "))
     :warning)))

(defun edmacs-sessions--install-macos-close-frame-bindings ()
  "Install the daemon's macOS close-frame bindings and startup checks.
Extracted from the `when' guard below so a test can call it directly:
`(daemonp)' is nil in every batch test's load environment, so the guard
itself never runs there.

Both the close button (`special-event-map') and `C-x 5 0'/`edmacs-quit'
(`[remap delete-frame]') are wired to `edmacs-ns-close-frame' -- they are
the same intent (\"get this frame off my screen\") and share one
behavior deliberately. `:q'/`:wq'/`:x'/`ZQ'/`C-w q' need no wiring here:
`edmacs-quit-window-or-buffer' (windows.el) already overrides `evil-quit'
to close a window or kill a buffer, never a frame, so they never reach
`delete-frame' at all."
  (define-key global-map [remap delete-frame] #'edmacs-ns-close-frame)
  (define-key special-event-map [delete-frame] #'edmacs-ns-handle-delete-frame)
  (add-hook 'emacs-startup-hook #'edmacs-sessions--ensure-gui-frame)
  (add-hook 'emacs-startup-hook #'edmacs-sessions--warn-on-shadow-daemon-process))

(when (and (daemonp) (eq system-type 'darwin))
  (edmacs-sessions--install-macos-close-frame-bindings))

;; ============================================================================
;; Bufferlo - per-tab buffer lists (desktop.el deliberately omits these)
;; ============================================================================

(use-package bufferlo
  :config
  (bufferlo-mode 1))

;; ============================================================================
;; C-x chords - reach the worktree/tab chords this phase's ACs name
;; ============================================================================
;; Registered via evil-config.el's extension point rather than a competing
;; `define-key' on `C-x', so this works regardless of module load order.
;; "t p" targets `edmacs-frames-open-worktree-tab' (modules/frames.el, loaded
;; after this module) rather than the stock `project-other-tab-command': a
;; plain quoted symbol here carries no forward-reference/compile issue, and
;; is only ever looked up once evil actually dispatches the chord.
(dolist (chord '(("t p" . edmacs-frames-open-worktree-tab)
                  ("v w w" . vc-switch-working-tree)
                  ("v w s" . vc-working-tree-switch-project)
                  ("v w k" . vc-kill-other-working-tree-buffers)
                  ("v w a" . vc-apply-to-other-working-tree)
                  ("v w A" . vc-apply-root-to-other-working-tree)))
  (edmacs-evil-config-add-c-x-chord (car chord) (cdr chord)))

;; ============================================================================
;; Leader-key bindings
;; ============================================================================
;; SPC T - tab lifecycle, with no dispatch grace period (unlike the C-x chords).
(general-define-key
 :states 'normal
 :prefix "SPC T"
 "" '(:ignore t :which-key "tabs")
 "p" '(edmacs-frames-open-worktree-tab :which-key "open worktree in its repo frame")
 "n" '(tab-bar-new-tab :which-key "new tab")
 "d" '(tab-bar-close-tab :which-key "close tab")
 "r" '(tab-bar-rename-tab :which-key "rename tab")
 "]" '(tab-bar-switch-to-next-tab :which-key "next tab")
 "[" '(tab-bar-switch-to-prev-tab :which-key "previous tab")
 "l" '(tab-bar-switch-to-tab :which-key "switch tab by name"))

;; SPC p w - worktree switching, same no-delay path.
(general-define-key
 :states 'normal
 :prefix "SPC p w"
 "" '(:ignore t :which-key "worktree")
 "w" '(vc-switch-working-tree :which-key "visit file in other worktree")
 "s" '(vc-working-tree-switch-project :which-key "switch worktree (project)")
 "k" '(vc-kill-other-working-tree-buffers :which-key "kill other worktree buffers")
 "a" '(vc-apply-to-other-working-tree :which-key "apply to other worktree")
 "A" '(vc-apply-root-to-other-working-tree :which-key "apply root to other worktree"))

;;; sessions.el ends here
