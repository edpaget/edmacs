;;; sessions.el --- Tabs, layout persistence, and worktree switching -*- lexical-binding: t -*-

;;; Commentary:
;; Replaces the tmux session layer with built-in Emacs 31 primitives:
;;   - `tab-bar-mode': one tab per active rdm worktree.
;;   - `desktop-save-mode': persists each tab's window layout across restarts.
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

An unstamped tab -- batch's own, or a fresh start's first -- falls through
to core's `tab-bar-tab-name-current'. A tab this config opens is
renamed explicitly by `edmacs-workspaces--open-tab' immediately after
creation, so the nameless moment before the post-open stamper runs is
never the name that sticks."
  ;; Core supplies no frame -- ambient-reads: ok
  (if-let* ((root (edmacs-workspaces-current-tab-root)))
      (file-name-nondirectory (directory-file-name root))
    ;; `tab-bar-tab-name-current' names the tab after the SELECTED window's
    ;; buffer, and the sidebar is a selectable side window -- so an unstamped
    ;; tab read while it held point was named `*sidebar*'. Only that case is
    ;; answered here, from a real window; everything else stays core's.
    (if (window-parameter (selected-window) 'window-side)
        (if-let* ((win (seq-find
                        (lambda (w) (not (window-parameter w 'window-side)))
                        (window-list nil 'no-minibuf))))
            (buffer-name (window-buffer win))
          (tab-bar-tab-name-current))
      (tab-bar-tab-name-current))))
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
      ;; buried buffers eagerly would block startup.
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
;; shells out to mise, and an error there aborts the rest of the restore.
;; `global-mise-mode' re-enables it anyway.
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
;; Restore. `desktop-save-mode' runs `desktop-read' from `after-init-hook',
;; by which point the initial GUI frame exists, so the frameset restores
;; straight onto it; only the tab-group migration beforehand and the
;; finish-up afterwards are ours.

(defun edmacs-sessions--frameset-has-frames-p (fs)
  "Return non-nil when FS is a frameset carrying at least one frame state.
Restoring an empty one is destructive rather than inert: `:reuse-frames
t' marks every live frame `:ignored', no state reassigns one, and the
`:cleanup-frames t' pass then deletes them."
  (and (frameset-p fs) (consp (frameset-states fs)) t))

(defun edmacs-sessions--prepare-frameset ()
  "Migrate `desktop-saved-frameset' just before desktop restores it.
`edmacs-workspaces-migrate-frameset' folds any multi-frame state into one
frame of tab groups and is a fixed point, so it is safe on every read.
An empty frameset is dropped rather than restored.  Never signals: a
failed migration restores the frameset unmigrated."
  (setq desktop-saved-frameset
        (when (edmacs-sessions--frameset-has-frames-p desktop-saved-frameset)
          (condition-case err
              (edmacs-workspaces-migrate-frameset desktop-saved-frameset)
            (error
             (display-warning
              'edmacs-sessions
              (format "desktop frameset migration failed, restoring it unmigrated: %s"
                      err)
              :warning)
             desktop-saved-frameset)))))

(advice-add 'desktop-restore-frameset :before #'edmacs-sessions--prepare-frameset)

(defun edmacs-sessions--restore-selected-tab-buffers (&optional frame)
  "Give FRAME's selected tab back the buffer list it was saved with.
bufferlo reapplies a tab's list only when `window-state-put' leaves the
frame's root window live, which the sidebar split never does, so the
selected tab would otherwise keep every buffer desktop reopened.  FRAME
defaults to `edmacs-workspaces-gui-frame'."
  (when-let* ((frame (or frame (edmacs-workspaces-gui-frame)))
              ((frame-live-p frame))
              ((frameset-p desktop-saved-frameset))
              (state (car (frameset-states desktop-saved-frameset)))
              (names (cadr (assq 'bufferlo-buffer-list (cdr state)))))
    (set-frame-parameter
     frame 'buffer-list
     (delete-dups (append (mapcar #'window-buffer (window-list frame 'nomini))
                          (delq nil (mapcar #'get-buffer names)))))
    (set-frame-parameter frame 'buried-buffer-list nil)))

(advice-add 'desktop-restore-frameset :after #'edmacs-sessions--restore-selected-tab-buffers)

;; desktop re-applies a file buffer's saved major mode over the one
;; `auto-mode-alist' picks, so a file first visited before its mode was
;; installed would stay in `fundamental-mode' across every restart.
(defvar desktop-buffer-major-mode)

(defun edmacs-sessions--keep-auto-mode (fn &rest args)
  "Call FN with ARGS, never forcing a saved `fundamental-mode'."
  (let ((desktop-buffer-major-mode
         (unless (eq desktop-buffer-major-mode 'fundamental-mode)
           desktop-buffer-major-mode)))
    (apply fn args)))

(advice-add 'desktop-restore-file-buffer :around #'edmacs-sessions--keep-auto-mode)

(defun edmacs-sessions--finish-frameset-restore (&optional frame)
  "Sweep, stamp and show the sidebar on FRAME after a frameset restore.
FRAME defaults to `edmacs-workspaces-gui-frame'; with neither, this does
nothing rather than guessing.  Agent panes cannot survive a restart and a
popup's buffer may not have been saved, so their stale stack windows are
swept.  Tabs that reach the session without a worktree root are stamped
from the layout they carry; a tab whose worktree directory is gone keeps
its stamp, so it renders as missing rather than dropping out of its
project's tree.  The sidebar is shown last, after the stamps, so its
buffer is named after the restored tab rather than a default one."
  (when-let* ((frame (or frame (edmacs-workspaces-gui-frame))))
    (when (and (frame-live-p frame)
               (edmacs-workspaces-frame-usable-p frame))
      (edmacs-stack-sweep-stale-panes frame)
      (edmacs-workspaces-stamp-frame-tabs frame)
      (edmacs-sidebar-show frame))))

(defun edmacs-sessions--after-desktop-read ()
  "Run `edmacs-sessions--finish-frameset-restore', warning instead of signalling."
  (condition-case err
      (edmacs-sessions--finish-frameset-restore)
    (error
     (display-warning 'edmacs-sessions
                      (format "finishing the frameset restore failed: %s" err)
                      :warning))))

(add-hook 'desktop-after-read-hook #'edmacs-sessions--after-desktop-read)

;; ----------------------------------------------------------------------------
;; emacsclient: claude-lib's eval channel and the Claude status hooks reach
;; this Emacs through the default server socket.
(require 'server)

(defun edmacs-sessions--start-server ()
  "Start the Emacs server unless one is already answering on its socket.
A second Emacs must not take the socket from the first."
  (unless (or noninteractive (server-running-p))
    (server-start)))

(add-hook 'emacs-startup-hook #'edmacs-sessions--start-server)

(defun edmacs-quit ()
  "Close the selected frame, quitting Emacs when it is the last one."
  (interactive)
  (save-buffers-kill-terminal))


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
;; :global-prefix must be restated as the full sub-prefix here: general.el's
;; global-prefix path concatenates :global-prefix with :infix, never with
;; :prefix, so inheriting leader-def's bare "C-SPC" would collapse every key
;; below onto "C-SPC <key>" instead of "C-SPC T <key>".
(leader-def
 :prefix "SPC T"
 :global-prefix "C-SPC T"
 "" '(:ignore t :which-key "tabs")
 "p" '(edmacs-workspaces-open-worktree :which-key "open worktree tab")
 "n" '(tab-bar-new-tab :which-key "new tab")
 "d" '(tab-bar-close-tab :which-key "close tab")
 "r" '(tab-bar-rename-tab :which-key "rename tab")
 "]" '(tab-bar-switch-to-next-tab :which-key "next tab")
 "[" '(tab-bar-switch-to-prev-tab :which-key "previous tab")
 "l" '(tab-bar-switch-to-tab :which-key "switch tab by name"))

;; SPC p w - worktree switching, same no-delay path.
;; :global-prefix restated for the same reason as the SPC T block above.
(leader-def
 :prefix "SPC p w"
 :global-prefix "C-SPC p w"
 "" '(:ignore t :which-key "worktree")
 "w" '(vc-switch-working-tree :which-key "visit file in other worktree")
 "s" '(vc-working-tree-switch-project :which-key "switch worktree (project)")
 "k" '(vc-kill-other-working-tree-buffers :which-key "kill other worktree buffers")
 "a" '(vc-apply-to-other-working-tree :which-key "apply to other worktree")
 "A" '(vc-apply-root-to-other-working-tree :which-key "apply root to other worktree"))

;;; sessions.el ends here
