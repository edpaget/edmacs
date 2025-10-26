;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; This file is loaded before package.el and the GUI is initialized.
;; Use it for performance optimizations and disabling unnecessary features.

;;; Code:

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Increase garbage collection threshold during startup
;; This will be reset in init.el after startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable file-name-handler-alist during startup for performance
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file-name-handler-alist after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent unwanted runtime compilation
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation nil)

;; Improve startup time by preventing premature loads
(setq frame-inhibit-implied-resize t)

;; Resizing the Emacs frame can be expensive, especially with GUI elements
(setq frame-resize-pixelwise t)

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;;; early-init.el ends here
