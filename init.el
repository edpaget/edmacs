;;; init.el --- Emacs configuration entry point -*- lexical-binding: t -*-

;;; Commentary:
;; Modern Emacs configuration with evil-mode, straight.el, and modular organization.
;; Configuration is split into modules in the modules/ directory.

;;; Code:

;; ============================================================================
;; Straight.el modification checking
;; ============================================================================

;; Straight's computed default here is (find-at-startup find-when-checking
;; only-once), which runs a `find(1)` pass over every package repo on every
;; startup -- this alone costs ~0.9-1.0s of boot time on this machine
;; (scripts/startup-bench.sh -n 10: ~1.56-1.62s mean before this setting vs
;; ~0.62-0.64s mean after, reproduced across repeated quiet runs; phase 1's
;; earlier A/B recorded ~0.8s, same ballpark). `check-on-save`
;; drops the boot-time scan and instead marks a package dirty via
;; before-save-hook when a file under straight/repos/<pkg>/ is edited and
;; saved from inside Emacs -- the realistic editing path. It does NOT catch
;; changes made by tooling outside Emacs (e.g. `git checkout` against a stale
;; lockfile). That gap is intentionally left to
;; edmacs-straight-hygiene/phase-2-drift-audit, which as of this change is
;; still not-started -- there is no covering mechanism landed yet.
;;
;; This must be set before straight's own bootstrap.el loads below, since
;; bootstrap.el consults this variable while checking straight.el's own repo,
;; and every straight-use-package call after bootstrap reads it too.
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

;; Run GC when every frame has lost focus.  `focus-out-hook' was obsoleted in
;; 27.1 in favour of `after-focus-change-function' + `frame-focus-state'.
(add-function :after after-focus-change-function
              (lambda ()
                (unless (seq-some #'frame-focus-state (frame-list))
                  (garbage-collect))))

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

;; Tabs, layout persistence, and worktree switching (tmux replacement)
(load-module "sessions")

;; ============================================================================
;; Language-specific configurations
;; ============================================================================

;; Add languages directory to load path for shared modules
(add-to-list 'load-path (expand-file-name "modules/languages" user-emacs-directory))

;; Load language configs eagerly - use-package will defer actual package loading
;; via :mode, :hook, and :after directives in each language config file
(load-language-config "elisp")
(load-language-config "clojure")
(load-language-config "go")
(load-language-config "java")
(load-language-config "javascript")
(load-language-config "rust")

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
                     gcs-done)))

;;; init.el ends here
