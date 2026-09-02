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

(defvar edmacs-sessions--git-common-dir-cache (make-hash-table :test #'equal)
  "Memoized ROOT -> git-common-dir results.
`tab-bar-tab-name-function' runs on nearly every redisplay, so each lookup
would otherwise spawn git. Misses are cached as `none'; a worktree's
common dir never changes while Emacs runs, so nothing is invalidated.")

(defun edmacs-sessions--git-common-dir-1 (root)
  "Uncached implementation of `edmacs-sessions--git-common-dir' for ROOT.
Uses `process-file' so a TRAMP ROOT runs git remotely. Relative output
\(the main worktree prints \".git\") is expanded against ROOT here, while
`default-directory' is still bound to it; on a remote ROOT the TRAMP
prefix is grafted back on, since git prints bare on-host paths. Signals
from a missing git or a pruned ROOT fold into nil so the miss is cached."
  (let ((default-directory root))
    (condition-case nil
        (with-temp-buffer
          (when (zerop (process-file "git" nil t nil "rev-parse" "--git-common-dir"))
            (let ((raw (string-trim (buffer-string)))
                  (remote (file-remote-p root)))
              (cond
               ((not (file-name-absolute-p raw)) (expand-file-name raw root))
               ((and remote (not (file-remote-p raw))) (concat remote raw))
               (t raw)))))
      (file-error nil))))

(defun edmacs-sessions--git-common-dir (root)
  "Return the absolute git common directory for the worktree at ROOT, or nil.
Memoized per ROOT; see `edmacs-sessions--git-common-dir-cache' and
`edmacs-sessions--git-common-dir-1' for why and how."
  (let ((cached (gethash root edmacs-sessions--git-common-dir-cache 'edmacs-sessions--miss)))
    (if (not (eq cached 'edmacs-sessions--miss))
        (and (not (eq cached 'none)) cached)
      (let ((result (edmacs-sessions--git-common-dir-1 root)))
        (puthash root (or result 'none) edmacs-sessions--git-common-dir-cache)
        result))))

(defun edmacs-sessions--tab-name ()
  "Name the current tab after its project/worktree.
Prefixed with the owning repository's directory name (from
`--git-common-dir') so two same-named worktrees from different repos get
distinct tabs. Falls back to `tab-bar-tab-name-current' when there is no
project."
  (if-let* ((proj (project-current))
            (root (project-root proj)))
      (let* ((base (file-name-nondirectory (directory-file-name root)))
             (common (edmacs-sessions--git-common-dir root))
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
(with-eval-after-load 'claude-repl-buffer
  (add-to-list 'desktop-modes-not-to-save 'claude-repl-buffer-mode))

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
;; `server-after-make-frame-hook'; `desktop-restore-reuses-frames' (default t)
;; makes it reuse the client frame rather than pop a new one.
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

(defun edmacs-sessions--restore-pending-frameset-on-client-frame ()
  "Restore a daemon-boot-stashed frameset onto the first client frame.
`server-after-make-frame-hook' selects the client frame before running
this, so `frameset-restore' (with `desktop-restore-reuses-frames'
defaulting to t) reuses it instead of creating a new one. Runs once:
later client frames just get the normal, empty daemon frame."
  (when edmacs-sessions--pending-frameset
    (let ((desktop-saved-frameset edmacs-sessions--pending-frameset))
      (desktop-restore-frameset))
    (setq edmacs-sessions--pending-frameset nil)))

(add-hook 'server-after-make-frame-hook
          #'edmacs-sessions--restore-pending-frameset-on-client-frame)

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
