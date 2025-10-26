;;; init.el --- Emacs configuration entry point -*- lexical-binding: t -*-

;;; Commentary:
;; Modern Emacs configuration with evil-mode, straight.el, and modular organization.
;; Configuration is split into modules in the modules/ directory.

;;; Code:

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

;; Use straight.el for use-package integration
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t)

;; ============================================================================
;; Performance - Reset GC threshold after startup
;; ============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)))

;; Run GC when Emacs loses focus
(add-hook 'focus-out-hook #'garbage-collect)

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

;; Core settings - must be loaded first
(load-module "core")

;; Evil mode - load early for keybindings
(load-module "evil-config")

;; UI configuration
(load-module "ui")

;; Completion framework
(load-module "completion")

;; Keybindings - load after evil and completion
(load-module "keybindings")

;; Programming tools (general)
(load-module "programming")

;; VTerm terminal emulator
(load-module "vterm")

;; AI assistant integration
(load-module "ai")

;; Org mode
;; (load-module "org-config")

;; Git integration
(load-module "git")

;; ============================================================================
;; Language-specific configurations (loaded on-demand)
;; ============================================================================

;; Language configs are loaded via hooks in their respective mode configurations
;; Example: (add-hook 'clojure-mode-hook (lambda () (load-language-config "clojure")))

;; ============================================================================
;; Custom file
;; ============================================================================

;; Keep customizations in a separate file
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
                     gcs-done)
            ;; Display projectile projects list
            (when (fboundp 'projectile-switch-project)
              (projectile-switch-project))))

;;; init.el ends here
