;;; core.el --- Core Emacs settings -*- lexical-binding: t -*-

;;; Commentary:
;; Basic Emacs settings, performance tweaks, and quality-of-life improvements.

;;; Code:

;; ============================================================================
;; Transient - Load early as it's used by multiple packages
;; ============================================================================
;; Transient is used by magit, combobulate, and other packages.
;; Load it early with all dependencies to avoid load-order issues.

(straight-use-package 'compat)
(straight-use-package 'cond-let)
(straight-use-package 'transient)

(require 'compat)
(require 'cond-let)
(require 'transient)

;; ============================================================================
;; Environment Variables from Shell
;; ============================================================================

;; Ensure Emacs uses the same PATH and environment as your shell
;; This is especially important on macOS where GUI apps don't inherit shell env
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  ;; Copy these environment variables from shell
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
;; Direnv Integration
;; ============================================================================

;; Automatically load environment variables from .envrc files using direnv
;; This allows per-project environment configuration
(use-package envrc
  :config
  ;; Enable envrc-mode globally to auto-load .envrc in all buffers
  (envrc-global-mode)

  ;; Optional: Show direnv status in mode line
  (setq envrc-mode-line-lighter " envrc"))

;; Keybindings for manual control (using SPC-e prefix for environment)
(with-eval-after-load 'general
  (general-define-key
   :states '(normal visual)
   :prefix "SPC e"
   "e" '(:ignore t :which-key "environment")
   "ea" '(envrc-allow :which-key "direnv allow")
   "ed" '(envrc-deny :which-key "direnv deny")
   "er" '(envrc-reload :which-key "direnv reload")
   "ep" '(envrc-reload-all :which-key "direnv reload all")))

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
 ;; Indentation
 indent-tabs-mode nil           ; Use spaces instead of tabs
 tab-width 4                    ; Set tab width to 4 spaces
 fill-column 80                 ; Set fill column to 80 characters

 ;; Line wrapping
 truncate-lines nil             ; Enable line wrapping
 word-wrap t                    ; Wrap at word boundaries

 ;; Scrolling
 scroll-margin 0                ; Scroll margin
 scroll-conservatively 100000   ; Smooth scrolling
 scroll-preserve-screen-position t

 ;; Performance
 redisplay-dont-pause t         ; Don't pause redisplay

 ;; Misc
 require-final-newline t        ; Always end files with newline
 )

;; Disable startup screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Disable some GUI elements (redundant with early-init, but kept for clarity)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Frame title
(setq frame-title-format '("%b - Emacs"))

;; Yes/No prompts become y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable bell
(setq ring-bell-function 'ignore)

;; ============================================================================
;; File Handling
;; ============================================================================

;; Backup and autosave configuration
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

;; Create backup and auto-save directories if they don't exist
(let ((backup-dir (expand-file-name "backups" user-emacs-directory))
      (auto-save-dir (expand-file-name "auto-save" user-emacs-directory)))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

;; Backup settings
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Auto-save settings
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

  ;; No directories are excluded from the project list by default.
  (setq project-list-exclude nil)

  ;; rdm reaps worktrees on disk out from under project.el's memory of
  ;; them; prune entries that no longer exist so stale paths don't
  ;; linger in `project-list-file'. project.el's real symbol for this
  ;; is `project-forget-zombie-projects' (not the
  ;; `project-prune-zombie-projects' name sometimes used informally).
  (when (fboundp 'project-forget-zombie-projects)
    (add-hook 'emacs-startup-hook #'project-forget-zombie-projects))

  ;; One-time walk to register every live rdm worktree so it shows up
  ;; in `project-list-file' (and hence `C-x v w s') without needing to
  ;; be visited interactively first.
  (defun edmacs--register-project-worktrees ()
    "Register every rdm worktree under ~/Projects/*__worktrees/ with project.el."
    (ignore-errors
      (dolist (worktrees-dir (file-expand-wildcards
                               (expand-file-name "*__worktrees" "~/Projects")))
        (when (file-directory-p worktrees-dir)
          (project-remember-projects-under worktrees-dir t)))))
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
