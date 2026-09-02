;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; This file is loaded before package.el and the GUI is initialized.
;; Use it for performance optimizations and disabling unnecessary features.

;;; Code:

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Effectively disable GC during startup; gcmh-mode (init.el) takes over after.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; If init.el aborts before gcmh-mode is enabled, GC would stay disabled for
;; the daemon's lifetime. Armed before anything else can fail; a no-op once
;; gcmh has lowered the threshold.
(run-with-idle-timer
 20 nil
 (lambda ()
   (when (eq gc-cons-threshold most-positive-fixnum)
     (setq gc-cons-threshold (* 64 1024 1024)
           gc-cons-percentage 0.1))))

;; Disable file-name-handler-alist during startup for performance
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Plain restore, not a merge: anything registered onto the list during the
;; nil window above is dropped. Nothing loaded in that window touches it today;
;; if TRAMP or epa moves into eager startup, merge instead.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Async native-comp warnings are almost always upstream deprecations; keep
;; them out of *Warnings*.
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-jit-compilation t)

;; Improve startup time by preventing premature loads
(setq frame-inhibit-implied-resize t)

;; Resizing the Emacs frame can be expensive, especially with GUI elements
(setq frame-resize-pixelwise t)

(setq load-prefer-newer t)

;; LSP servers are chatty; 1MB per read. Emacs 31's default is already 64KB
;; and `process-adaptive-read-buffering' already nil, so don't add either.
(setq read-process-output-max (* 1024 1024))

;;; early-init.el ends here
