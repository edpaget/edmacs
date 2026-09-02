;;; init.el --- Emacs configuration entry point -*- lexical-binding: t -*-

;;; Commentary:
;; Modern Emacs configuration with evil-mode, straight.el, and modular organization.
;; Configuration is split into modules in the modules/ directory.

;;; Code:

;; ============================================================================
;; Straight.el modification checking
;; ============================================================================

;; Skip straight's find(1) scan of every package repo at startup (~1s here);
;; saves from inside Emacs still mark a package dirty. Must be set before
;; bootstrap.el loads, which reads it.
(setq straight-check-for-modifications '(check-on-save))

;; ============================================================================
;; Bootstrap straight.el
;; ============================================================================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; ============================================================================
;; Performance - GC management after startup
;; ============================================================================
;; gcmh collects on idle instead of on focus change, so it behaves the same
;; for TTY frames and a frameless daemon. Enabled from `emacs-startup-hook'
;; rather than here: enabling it lowers `gc-cons-threshold' immediately, which
;; would end early-init.el's startup GC budget before the modules below load.
(use-package gcmh
  :config
  (setq gcmh-high-cons-threshold (* 100 1024 1024) ; sized for five lsp-mode backends
        gcmh-idle-delay 10)
  (add-hook 'emacs-startup-hook #'gcmh-mode))

;; ============================================================================
;; Module Loading System
;; ============================================================================

(defun load-module (module-name)
  "Load a configuration module from the modules directory.
MODULE-NAME should be the name without the .el extension."
  (let ((module-file (expand-file-name
                      (concat "modules/" module-name ".el")
                      user-emacs-directory)))
    (if (file-exists-p module-file)
        (progn
          (message "Loading module: %s" module-name)
          (load module-file nil 'nomessage))
      (message "Warning: Module not found: %s" module-name))))

(defun load-language-config (language)
  "Load language-specific configuration file.
LANGUAGE should be the name without the .el extension."
  (let ((lang-file (expand-file-name
                    (format "modules/languages/%s.el" language)
                    user-emacs-directory)))
    (when (file-exists-p lang-file)
      (message "Loading language config: %s" language)
      (load lang-file nil 'nomessage))))

;; ============================================================================
;; Load Configuration Modules
;; ============================================================================

;; Order matters: core first, evil before keybindings, keybindings after completion.
(load-module "core")
(load-module "evil-config")
(load-module "ui")
(load-module "windows")
(load-module "completion")
(load-module "keybindings")
(load-module "programming")
(load-module "vterm")
(load-module "ai")
;; Before its consumers below: they only `declare-function' it, so
;; nothing loads it on demand.
(load-module "git-common-dir")
(load-module "claude-term")
(load-module "claude-term-registry")
;; (load-module "org-config")
(load-module "git")
(load-module "sessions")
(load-module "sidebar")

;; ============================================================================
;; Language-specific configurations
;; ============================================================================

(add-to-list 'load-path (expand-file-name "modules/languages" user-emacs-directory))

;; Loaded eagerly; each file defers its packages via :mode/:hook.
(load-language-config "elisp")
(load-language-config "clojure")
(load-language-config "go")
(load-language-config "java")
(load-language-config "javascript")
(load-language-config "rust")

;; ============================================================================
;; Custom file
;; ============================================================================

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ============================================================================
;; Startup message
;; ============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;;; init.el ends here
