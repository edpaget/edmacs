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
;; The session holds ONE graphical frame: a project is a tab-bar group
;; and a worktree a tab inside it (`modules/workspaces.el'). That
;; satisfies ghostel's `window-adjust-process-window-size-smallest'
;; constraint -- never show one terminal buffer in two frames at once --
;; by construction, there being only one frame to show it in.

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
(declare-function edmacs-stack-sweep-stale-panes "windows")
(declare-function edmacs-sidebar-show "sidebar")
(declare-function edmacs-workspaces-open-worktree "workspaces")
(declare-function edmacs-workspaces-migrate-frameset "workspaces")
(declare-function edmacs-workspaces-gui-frame "workspaces")
(declare-function edmacs-workspaces-frame-usable-p "workspaces")
(declare-function edmacs-workspaces-stamp-frame-tabs "workspaces")
(declare-function edmacs-workspaces-current-tab-root "workspaces")

(defun edmacs-sessions--tab-name ()
  "`tab-bar-tab-name-function' entry point: name the current tab.
Core calls this with zero arguments by its own fixed API, and re-runs it
on every `tab-bar-tabs' read of an unrenamed tab rather than only at
creation. The answer is the leaf directory name of the tab's OWN
stamped worktree root (`edmacs-workspaces-current-tab-root'), a pure
read of the tab -- so it is the same string no matter which buffer
happens to be current when core asks, which the derived-from-the-window
naming this replaced could not promise.

Two worktrees of different repos can share a directory basename, and a
bare leaf name renders both identically. They no longer need a repo
prefix to be told apart: the tab-bar GROUP names the project, so the
disambiguation lives one level up.

An unstamped tab -- batch's own, the daemon's boot tab -- falls through
to core's `tab-bar-tab-name-current'. A tab this config opens is
renamed explicitly by `edmacs-workspaces--open-tab' immediately after
creation, so the nameless moment before the post-open stamper runs is
never the name that sticks."
  ;; Core supplies no frame -- ambient-reads: ok
  (if-let* ((root (edmacs-workspaces-current-tab-root)))
      (file-name-nondirectory (directory-file-name root))
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

;; `edmacs-sidebar-collapsed' (sidebar.el) is a per-frame boolean, not a
;; colour/geometry parameter: it wants frameset.el's default pass-through,
;; not the colour filters' `:never'. That default already applies to any
;; parameter simply absent from this alist; pinned explicitly, alongside
;; those filters, so the contract lives in source rather than being an
;; accident of frameset.el's own default.
(push (cons 'edmacs-sidebar-collapsed nil) frameset-filter-alist)

;; A restored frame lands on the current display instead of replaying the
;; coordinates of whichever monitor it was saved on; workspaces.el's fullscreen
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
loaded but before `desktop-read' unconditionally nils it back out.

What is stashed is the frameset put through
`edmacs-workspaces-migrate-frameset', which folds the frames model's
one-frame-per-repo states into a single state whose tabs carry native
`group' parameters. Because the stashed frameset then holds exactly one
state, `frameset-restore' reuses the boot GUI frame and creates no
second one -- no frame is ever deleted to get there. The migration is a
fixed point, so leaving it in the boot path permanently is a no-op once
the desktop is already in the new shape.

Guarded: an error escaping `desktop-after-read-hook' in a frameless
daemon reaches top level and exits Emacs 255 (see core.el). Falling back
to the unmigrated frameset is degraded -- two frames -- but never
frameless."
  (when (and (daemonp)
             (edmacs-sessions--frameset-has-frames-p desktop-saved-frameset)
             (not (desktop-restoring-frameset-p)))
    (setq edmacs-sessions--pending-frameset
          (condition-case err
              (edmacs-workspaces-migrate-frameset desktop-saved-frameset)
            (error
             (display-warning
              'edmacs-sessions
              (format "desktop frameset migration failed, restoring it unmigrated: %s"
                      err)
              :warning)
             desktop-saved-frameset)))))

(add-hook 'desktop-after-read-hook #'edmacs-sessions--stash-frameset-for-daemon)

;; ----------------------------------------------------------------------------
;; Finish-up after `frameset-restore': it (via `desktop-restore-frameset')
;; only reuses the frame and replays its window/tab layout. It stamps no
;; worktree root on a tab that reaches the session without one, and shows
;; no sidebar; both are back-filled below.

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
function's caller has stamped the restored tabs -- reproduced live,
that race first-names the sidebar buffer after the daemon's generic
default frame name, and only `edmacs-sidebar-show' (via
`edmacs-sidebar--ensure-buffer''s rename-if-stale check) ever revisits
it to fix that."
  (edmacs-sidebar-show frame))

(defun edmacs-sessions--finish-frameset-restore (&optional frame)
  "Stamp FRAME's tab roots and show its sidebar after a frameset restore.
FRAME defaults to `edmacs-workspaces-gui-frame'; with neither, this does
nothing at all rather than guessing at a frame. Runs synchronously right
after `desktop-restore-frameset', by which point `frameset-restore''s
own `:reuse-frames t' (the default) has already reused the saved frame --
this never itself creates or deletes one. A frame
`edmacs-workspaces-frame-usable-p' rejects (the daemon's initial tty
placeholder, most of all) is declined outright.

Single-frame, not a walk over `frame-list': the frameset a daemon
replays is migrated to exactly one state before it is stashed (see
`edmacs-sessions--stash-frameset-for-daemon'), so the session has one
GUI frame holding every project as a tab group.

A tab whose stamped worktree directory is gone deliberately KEEPS its
stamp. Under the frames model a dead stamp was cleared so a live root
could be re-derived for the frame's one repo; under groups the stamp IS
the tab's identity, and clearing it would drop the tab out of its
project's tree instead of rendering it with
`edmacs-sidebar-missing-worktree-face'."
  (when-let* ((frame (or frame (edmacs-workspaces-gui-frame))))
    (when (and (frame-live-p frame)
               (edmacs-workspaces-frame-usable-p frame))
      (edmacs-workspaces-stamp-frame-tabs frame)
      (edmacs-sessions--ensure-sidebar frame))))

(defun edmacs-sessions--gui-frame-parameters ()
  "Parameters `edmacs-sessions--make-gui-frame' creates a frame with.
A daemon's `window-system' is nil, so the window system has to be named
explicitly or `make-frame' produces another tty placeholder rather than
the graphical frame every caller here is asking for. Only the macOS
daemon this config actually runs under needs naming; elsewhere the
ambient default already yields a graphical frame."
  (and (eq system-type 'darwin) '((window-system . ns))))

;; The sole `make-frame' call site in this config: a project is a tab
;; group, never a frame, so nothing else ever asks for one.
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
This is the config's ONE frame-creation path, and it cannot be removed.
Two mechanisms make a frameless daemon self-perpetuating rather than
merely inconvenient:

  - a daemon holding only its initial tty frame writes an EMPTY frameset,
    because `desktop--check-dont-save' excludes that frame -- overwriting
    a good desktop with one that restores nothing (the full account is on
    `edmacs-sessions--frameset-has-frames-p'; it is not restated here).
  - `frameset-restore' with `:reuse-frames t' deletes the very frame it
    was handed whenever the frameset it replays has no state to reassign
    to it.

So a single bad restore would otherwise become permanent. Restoring a
frame here is what breaks that loop."
  (unless (edmacs-workspaces-gui-frame)
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
                     ;; FRAME explicitly, so the frame the restore landed on
                     ;; is the one finished; a frame `--ensure-gui-frame' had
                     ;; to create in its place is picked up by the fallback.
                     (edmacs-sessions--finish-frameset-restore
                      (and (frame-live-p frame) frame)))))))

(add-hook 'after-make-frame-functions #'edmacs-sessions--restore-pending-frameset)

;; ----------------------------------------------------------------------------
;; Keep the daemon owned by the Dock's Emacs.app tile. Emacs becomes a regular
;; Dock app only once it has a visible frame, and drops out again (activation
;; policy Prohibited) when its last NS frame is deleted; after that a Dock
;; click launches a second Emacs. So the daemon opens one frame at boot, and
;; closing the last window hides Emacs (what s-h does) instead of deleting the
;; frame. A Dock click then unhides it with the layout intact.
(defun edmacs-ns-close-frame (frame)
  "Close FRAME, hiding Emacs instead when it is the last visible GUI frame.
Under the daemon a deleted last frame would drop Emacs out of the Dock.
Interactively (including via its `[remap delete-frame]' binding below),
FRAME is always the selected frame; the window-close-button handler
below passes the clicked frame explicitly instead, which need not be
the selected one."
  (interactive (list (selected-frame)))
  (if (and (daemonp)
           (display-graphic-p frame)
           (= 1 (length (seq-filter (lambda (f) (and (display-graphic-p f)
                                                     (frame-visible-p f)))
                                    (frame-list)))))
      (ns-do-hide-emacs)
    (delete-frame frame t)))

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
      (edmacs-ns-close-frame (selected-frame))
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
  "Return PIDs of other Dock-style launches of this daemon's executable.
Finding 1 of the daemon-and-Dock-frame phase: the launchd daemon and a
Dock-launched Emacs.app are the very same binary, so a second process
running it is exactly the silent-drift failure (a Dock click launching a
serverless Emacs instead of raising this one) that phase exists to catch.

Matching the executable alone is far too loose here, because this repo's
own workflow runs `emacs --batch' constantly (ERT suites,
`batch-byte-compile', scripts/startup-check.sh) off that same binary. A
LaunchServices launch is instead identifiable by its argv: double-click,
Dock click and `open -a' all exec the bundle executable with no
arguments at all, while every batch, `--fg-daemon' or `-Q' invocation
carries flags. So only an exact bare-executable command line counts.

Shells out to `ps' rather than `list-system-processes' +
`process-attributes', which on macOS cannot read another process's
command line without it being owned by the same user, and even then not
reliably for an app-bundle launch."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (self (emacs-pid))
         (ps (executable-find "ps")))
    (when ps
      (with-temp-buffer
        (call-process ps nil t nil "-xo" "pid=,command=")
        (goto-char (point-min))
        (let (pids)
          (while (not (eobp))
            (let ((line (string-trim (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))
              (when (string-match "\\`\\([0-9]+\\)[ \t]+\\(.*\\)\\'" line)
                (let ((pid (string-to-number (match-string 1 line)))
                      (command (match-string 2 line)))
                  (when (and (/= pid self) (string= command exe))
                    (push (number-to-string pid) pids)))))
            (forward-line 1))
          (nreverse pids))))))

(defun edmacs-sessions--warn-on-shadow-daemon-process ()
  "Warn (non-fatally) about another Dock-style launch of this executable.
Detection only, by design: never signals or kills the other process,
since telling a stale/duplicate instance from a legitimate one apart
safely is out of scope for an automatic action -- see the launchd-vs-
wrapper-.app tradeoff recorded above `edmacs-sessions-launchd-service'.
Errors are swallowed rather than surfaced: this is a best-effort warning
running from `emacs-startup-hook', not a boot-critical check."
  (when-let* ((pids (ignore-errors (edmacs-sessions--shadow-daemon-processes))))
    (display-warning
     'edmacs-sessions
     (format "another Emacs (pid%s %s) was launched from this daemon's own \
app bundle with no arguments -- a Dock/Finder launch running alongside \
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
;; "t p" targets `edmacs-workspaces-open-worktree' (modules/workspaces.el)
;; rather than the stock `project-other-tab-command': a plain quoted symbol
;; here carries no forward-reference/compile issue, and is only ever looked
;; up once evil actually dispatches the chord.
(dolist (chord '(("t p" . edmacs-workspaces-open-worktree)
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
 "p" '(edmacs-workspaces-open-worktree :which-key "open worktree tab")
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
