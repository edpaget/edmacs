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
;; Performance - GC management after startup
;; ============================================================================
;;
;; edmacs-performance/phase-6-gc-and-unbounded-state: this replaces a
;; hardcoded `emacs-startup-hook' that reset `gc-cons-threshold' to a flat
;; 16MB, plus an `after-focus-change-function' hook that ran a blocking
;; `(garbage-collect)' every time every frame lost focus. On a daemon that
;; has been up for days holding LSP workspaces for five languages plus
;; magit/vterm/claude-repl buffers, that blocking call was a stop-the-world
;; pause of potentially hundreds of milliseconds to seconds taken on every
;; tab-away -- and it silently did nothing for `emacsclient -t' frames,
;; which don't reliably fire focus-change events, or for a daemon with no
;; frames attached at all. 16MB was also undersized for this workload:
;; JSON-RPC deserialisation from five lsp-mode backends is allocation-heavy
;; and was provoking frequent minor GCs at that threshold.
;;
;; gcmh replaces both mechanisms with `pre-command-hook'/`post-command-hook'
;; and Emacs's own idle timer -- no dependency on `frame-focus-state' or
;; `after-focus-change-function' at all, so it behaves identically for GUI
;; and TTY (`emacsclient -t') frames and for a daemon with zero frames
;; attached. `gc-cons-threshold' is kept at `gcmh-high-cons-threshold' during
;; normal use (set immediately on `gcmh-mode' enable, not idle-gated) and
;; dropped to `gcmh-low-cons-threshold' only right after an idle-triggered
;; GC actually runs, so allocation-heavy bursts (LSP parsing, apheleia
;; rewrites) never fight a small threshold mid-operation.
;;
;; `gcmh-mode' is enabled from `emacs-startup-hook', not eagerly here.
;; Enabling it sets `gc-cons-threshold' to `gcmh-high-cons-threshold'
;; synchronously the moment it runs (verified in gcmh.el's
;; `define-minor-mode' body: the mode's enable branch does `(setf
;; gc-cons-threshold gcmh-high-cons-threshold)' directly, with no idle-timer
;; or hook indirection at all) -- and every module below (evil, ui,
;; completion, programming/lsp-mode, vterm, ai, git, sessions, and all
;; per-language configs) still has to load after this point in the file.
;; Calling `(gcmh-mode 1)' here, before that module-loading pass, would cut
;; the generous `most-positive-fixnum' startup budget early-init.el sets up
;; down to 100MB before the heaviest allocation in this config even starts
;; -- reintroducing the startup-time GC pause that budget exists to avoid.
;; `emacs-startup-hook' runs only once the entire init file (this one
;; included) has finished loading, so deferring the enable call to it keeps
;; the `most-positive-fixnum' budget in effect for the whole module-loading
;; pass and drops to `gcmh-high-cons-threshold' only once startup is
;; actually done -- matching early-init.el's own comment on this point.
(use-package gcmh
  :config
  (setq gcmh-high-cons-threshold (* 100 1024 1024) ; 100MB: sized generously
                                                    ; for lsp-mode across
                                                    ; go/java/javascript/
                                                    ; rust/clojure -- too low
                                                    ; here just trades one
                                                    ; perceptible pause for
                                                    ; periodic idle-GC pauses
                                                    ; of similar size
        gcmh-idle-delay 10)                        ; seconds idle before the
                                                    ; deferred GC runs
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
