;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; This file is loaded before package.el and the GUI is initialized.
;; Use it for performance optimizations and disabling unnecessary features.

;;; Code:

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Increase garbage collection threshold during startup
;; This will be reset in init.el after startup (gcmh-mode takes over from
;; there; see init.el).
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Fallback reset for `gc-cons-threshold', independent of `emacs-startup-hook'
;; ever running. On a long-running daemon, an error thrown anywhere between
;; this point and init.el's own post-startup reset (gcmh-mode's enable,
;; which replaces the historical emacs-startup-hook body) -- including the
;; straight.el bootstrap's synchronous `url-retrieve-synchronously' call in
;; init.el -- aborts every remaining top-level form in that file's `load',
;; which means `emacs-startup-hook' never gets a handler registered at all.
;; With no fallback, `gc-cons-threshold' would stay pinned at
;; `most-positive-fixnum' (GC effectively disabled) for the daemon's entire
;; multi-week uptime. This timer is armed here, before straight.el or
;; anything else in init.el has a chance to run, so it survives that failure
;; mode. The `eq' guard makes it a no-op on the happy path once gcmh-mode
;; has already lowered the threshold -- it must never override live GC
;; management.
(run-with-idle-timer
 20 nil
 (lambda ()
   (when (eq gc-cons-threshold most-positive-fixnum)
     (setq gc-cons-threshold (* 64 1024 1024)
           gc-cons-percentage 0.1))))

;; Disable file-name-handler-alist during startup for performance
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file-name-handler-alist after startup.
;;
;; LATENT ISSUE (edmacs-performance/phase-6-gc-and-unbounded-state audit):
;; this is a plain `setq' restore, not a merge -- it silently drops any
;; handler some other code registers onto `file-name-handler-alist' during
;; the nil window between the `setq' above and this hook running. Verified
;; currently latent, not live: everything that loads in that window today
;; (`compat', `cond-let', `transient', `exec-path-from-shell', `envrc')
;; does not touch the list, so there is nothing to drop yet. It becomes
;; live the day something that needs the handler list during that window
;; is added -- most plausibly TRAMP, or an `epa'/GPG workflow, moving into
;; eager startup. If that happens, change this restore to merge
;; (`file-name-handler-alist-original' plus anything added since) instead
;; of overwriting.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Natively compile packages in the background as they are loaded, but keep the
;; async compiler's warnings out of *Warnings* -- they are almost always
;; upstream deprecations in third-party sources, not actionable here.
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-jit-compilation t)

;; Improve startup time by preventing premature loads
(setq frame-inhibit-implied-resize t)

;; Resizing the Emacs frame can be expensive, especially with GUI elements
(setq frame-resize-pixelwise t)

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Raise the per-read chunk size for subprocess output (LSP servers in
;; particular -- rust-analyzer, gopls, jdtls, clojure-lsp are all chatty).
;; The Emacs 31.1 built-in default is 65536, not the 4096 most published
;; advice cites; 1MB is a global process knob, not per-package config, so it
;; lives here rather than in modules/programming.el. Do NOT also set
;; `process-adaptive-read-buffering' -- it is already nil by default in
;; 31.1, so the classic pairing advice is stale and setting it would be a
;; no-op at best.
(setq read-process-output-max (* 1024 1024))

;;; early-init.el ends here
