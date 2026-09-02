;;; core.el --- Core Emacs settings -*- lexical-binding: t -*-

;;; Commentary:
;; Basic Emacs settings, performance tweaks, and quality-of-life improvements.

;;; Code:

;; ============================================================================
;; Transient dependencies
;; ============================================================================
;; compat/cond-let are transient's dependencies; build them here. transient
;; itself is required lazily by magit and combobulate.

(straight-use-package 'compat)
(straight-use-package 'cond-let)
(straight-use-package 'transient)

(require 'compat)
(require 'cond-let)

;; ============================================================================
;; Environment Variables from Shell
;; ============================================================================

;; macOS GUI apps don't inherit the shell environment.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables
        '("PATH"
          "MANPATH"
          "ANTHROPIC_API_KEY"  ; For AI assistant
          "JAVA_HOME"
          "GOPATH"
          "CARGO_HOME"
          "RUSTUP_HOME"
          "PYENV_ROOT"
          "NVM_DIR"))

  ;; Use interactive login shell to get full environment
  (setq exec-path-from-shell-arguments '("-l"))

  (exec-path-from-shell-initialize))

;; ============================================================================
;; Environment Integration (mise)
;; ============================================================================

;; mise.el sets per-project env buffer-locally from the mise config chain.
;; Activated on `after-init' because it shells out to mise several times.
;; Note: first activation writes `experimental = true' to the global mise config.
(use-package mise
  :hook (after-init . global-mise-mode)
  :config
  ;; Prompt before trusting a newly-seen config rather than blanket-trusting.
  ;; Answering yes runs `mise trust --all', which trusts the whole parent chain
  ;; and subdirectories, not just the one file.
  (setq mise-trust 'ask))

(defun edmacs-mise-trust ()
  "Trust the mise config chain for the current buffer's directory."
  (interactive)
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (if (zerop (call-process "mise" nil nil nil "trust"))
        (progn (message "mise: trusted %s" default-directory)
               (when (fboundp 'mise-update-buffer) (mise-update-buffer)))
      (message "mise: trust failed in %s" default-directory))))

(defun edmacs-mise-untrust ()
  "Stop trusting the mise config chain for the current buffer's directory."
  (interactive)
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (if (zerop (call-process "mise" nil nil nil "trust" "--untrust"))
        (progn (message "mise: untrusted %s" default-directory)
               (when (fboundp 'mise-update-buffer) (mise-update-buffer)))
      (message "mise: untrust failed in %s" default-directory))))

;; Keybindings for manual control (using SPC-e prefix for environment).
;; Key strings are unchanged from the envrc bindings these replace.
(with-eval-after-load 'general
  (general-define-key
   :states '(normal visual)
   :prefix "SPC e"
   "e" '(:ignore t :which-key "environment")
   "ea" '(edmacs-mise-trust :which-key "mise trust")
   "ed" '(edmacs-mise-untrust :which-key "mise untrust")
   "er" '(mise-update-buffer :which-key "mise reload buffer")
   "ep" '(mise-update-dir :which-key "mise reload dir")))

;; ============================================================================
;; Basic Settings
;; ============================================================================

;; UTF-8 encoding everywhere
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Better defaults
(setq-default
 indent-tabs-mode nil
 tab-width 4
 fill-column 80
 truncate-lines nil
 word-wrap t
 scroll-margin 0
 scroll-preserve-screen-position t
 require-final-newline t)

;; ============================================================================
;; Performance
;; ============================================================================
;; Cheap redisplay settings that keep per-redisplay overhead from compounding
;; with rainbow-delimiters, flycheck, tree-sitter, diff-hl, etc.

;; Stock limits (160KB/240KB) silently truncate history on a single big
;; change, which apheleia format-on-save and `lsp-rename' produce routinely.
(setq undo-limit (* 3 1024 1024)          ; ~3MB
      undo-strong-limit (* 16 1024 1024)) ; ~16MB

;; so-long neutralizes expensive per-line features in very-long-line buffers.
;; evil isn't in its default minor-mode list and re-enables itself on every
;; major-mode switch, so add it explicitly.
(with-eval-after-load 'evil
  (add-to-list 'so-long-minor-modes 'evil-local-mode))
(global-so-long-mode 1)

;; All code here is left-to-right; skip bidi detection and the bracket-pair
;; algorithm, which is costly in bracket-dense buffers.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Git only; skips per-visit probes for RCS/CVS/SVN/etc.
(setq vc-handled-backends '(Git))

;; Keep fontification off the typing path.
(setq redisplay-skip-fontification-on-input t)

;; Font caches are large here (nerd-icons, runtime face switching); don't
;; compact them on every GC.
(setq inhibit-compacting-font-caches t)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Redundant with early-init.el's frame parameters
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq frame-title-format '("%b - Emacs"))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

;; ============================================================================
;; File Handling
;; ============================================================================

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

(let ((backup-dir (expand-file-name "backups" user-emacs-directory))
      (auto-save-dir (expand-file-name "auto-save" user-emacs-directory)))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;; ============================================================================
;; Recent Files
;; ============================================================================

(use-package recentf
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude
               (expand-file-name "straight" user-emacs-directory))
  (add-to-list 'recentf-exclude
               (expand-file-name "backups" user-emacs-directory))
  (recentf-mode 1))

;; ============================================================================
;; Save History
;; ============================================================================

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;; ============================================================================
;; Save Place
;; ============================================================================

(use-package saveplace
  :config
  (save-place-mode 1))

;; ============================================================================
;; Auto-revert
;; ============================================================================

(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

;; ============================================================================
;; Electric Pair Mode
;; ============================================================================

(use-package elec-pair
  :config
  (electric-pair-mode 1))

;; ============================================================================
;; Show Matching Parentheses
;; ============================================================================

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; ============================================================================
;; Which Key - Show available keybindings
;; ============================================================================

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3
        which-key-separator " → "
        which-key-prefix-prefix "+")
  (which-key-mode 1))

;; ============================================================================
;; Project Management
;; ============================================================================

(use-package project
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-dired "Dired")
          (project-shell "Shell")))

  ;; rdm worktrees vendor their own straight checkouts and caches; never
  ;; remember those as projects.
  (setq project-list-exclude '("/straight/repos/" "/\\.eldev/" "/node_modules/"))
  ;; rdm reaps worktrees on disk; drop entries that no longer exist.
  (when (fboundp 'project-forget-zombie-projects)
    (add-hook 'emacs-startup-hook #'project-forget-zombie-projects))

  ;; Register every rdm worktree with project.el so it appears in
  ;; `project-switch-project' without being visited first. Non-recursive:
  ;; recursing would also register each worktree's vendored straight/repos.
  (defun edmacs--register-project-worktrees ()
    "Register every rdm worktree under ~/Projects/*__worktrees/ with project.el."
    (ignore-errors
      (dolist (worktrees-dir (file-expand-wildcards
                               (expand-file-name "*__worktrees" "~/Projects")))
        (when (file-directory-p worktrees-dir)
          (project-remember-projects-under worktrees-dir)))))
  (add-hook 'emacs-startup-hook #'edmacs--register-project-worktrees))

;; ============================================================================
;; Display Line Numbers
;; ============================================================================

(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; ============================================================================
;; Highlight Current Line
;; ============================================================================

(global-hl-line-mode 1)

;; ============================================================================
;; Better Help
;; ============================================================================

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

;;; core.el ends here
