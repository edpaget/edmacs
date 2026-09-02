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
names the repo rather than the worktree)."
  (if-let* ((proj (project-current))
            (root (project-root proj)))
      (let* ((base (file-name-nondirectory (directory-file-name root)))
             (common (edmacs-git-common-dir root))
             (repo (and common
                        (file-name-nondirectory
                         (directory-file-name
                          (file-name-directory (directory-file-name common)))))))
        (if (and repo (not (string= repo base)))
            (format "%s/%s" repo base)
          base))
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

(defun edmacs-sessions--stash-frameset-for-daemon ()
  "Stash `desktop-saved-frameset' when daemon boot skipped restoring it.
Runs on `desktop-after-read-hook', which fires after the frameset is
loaded but before `desktop-read' unconditionally nils it back out."
  (when (and (daemonp)
             desktop-saved-frameset
             (not (desktop-restoring-frameset-p)))
    (setq edmacs-sessions--pending-frameset desktop-saved-frameset)))

(add-hook 'desktop-after-read-hook #'edmacs-sessions--stash-frameset-for-daemon)

(defun edmacs-sessions--restore-pending-frameset (frame)
  "Restore a daemon-boot-stashed frameset onto FRAME, the first GUI frame.
Runs from `after-make-frame-functions' so it covers the boot frame,
emacsclient frames, and the Dock's reopen event alike.
`desktop-restore-reuses-frames' (default t) reuses FRAME. Deferred by a
timer so the frame is fully created before frameset-restore touches it."
  (when (and edmacs-sessions--pending-frameset (display-graphic-p frame))
    (let ((frameset edmacs-sessions--pending-frameset))
      (setq edmacs-sessions--pending-frameset nil)
      (run-at-time 0 nil
                   (lambda ()
                     (when (frame-live-p frame)
                       (let ((desktop-saved-frameset frameset))
                         (with-selected-frame frame
                           (desktop-restore-frameset)))))))))

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

(defun edmacs-ns-handle-delete-frame (event)
  "Handle the window close button EVENT via `edmacs-ns-close-frame'."
  (interactive "e")
  (edmacs-ns-close-frame (posn-window (event-start event))))

(when (and (daemonp) (eq system-type 'darwin))
  (define-key global-map [remap delete-frame] #'edmacs-ns-close-frame)
  (define-key special-event-map [delete-frame] #'edmacs-ns-handle-delete-frame)
  (add-hook 'emacs-startup-hook
            (lambda ()
              ;; Never let a headless daemon die here (see core.el on exit 255).
              (ignore-errors (make-frame '((window-system . ns)))))))

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
(dolist (chord '(("t p" . project-other-tab-command)
                  ("v w w" . vc-switch-working-tree)
                  ("v w s" . vc-working-tree-switch-project)
                  ("v w k" . vc-kill-other-working-tree-buffers)
                  ("v w a" . vc-apply-to-other-working-tree)
                  ("v w A" . vc-apply-root-to-other-working-tree)))
  (edmacs-evil-config-add-c-x-chord (car chord) (cdr chord)))

;; ============================================================================
;; One tab per worktree - collapse duplicates from re-opening the same one
;; ============================================================================
;; `project-other-tab-command' always creates a new tab; it has no notion of
;; an existing tab for the same worktree. Advising around it and reconciling
;; afterward is simpler than pre-empting its project resolution.
(defun edmacs-sessions--dedupe-tab-after-open (orig-fn &rest args)
  "Run ORIG-FN (`project-other-tab-command') with ARGS, then dedupe.
If a tab with the new tab's name already existed, close the new one and
switch to the existing one."
  (let ((before-tabs (tab-bar-tabs)))
    (apply orig-fn args)
    (let* ((new-name (funcall tab-bar-tab-name-function))
           (dup (seq-find (lambda (tab) (equal (alist-get 'name tab) new-name))
                           before-tabs))
           (dup-index (and dup (tab-bar--tab-index dup))))
      (when dup-index
        (tab-bar-close-tab nil (1+ dup-index))))))

(advice-add 'project-other-tab-command :around
            #'edmacs-sessions--dedupe-tab-after-open)

;; ============================================================================
;; Leader-key bindings
;; ============================================================================
;; SPC T - tab lifecycle, with no dispatch grace period (unlike the C-x chords).
(general-define-key
 :states 'normal
 :prefix "SPC T"
 "" '(:ignore t :which-key "tabs")
 "p" '(project-other-tab-command :which-key "open worktree in new tab")
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
