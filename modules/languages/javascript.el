;;; javascript.el --- JavaScript and TypeScript configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Configuration for JavaScript, TypeScript, JSX, and TSX development.

;;; Code:

;; JavaScript Mode
(use-package js
  :straight nil
  :mode (("\\.js\\'" . js-mode)
         ("\\.mjs\\'" . js-mode)
         ("\\.cjs\\'" . js-mode))
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2)
  :hook (js-mode . lsp-deferred)
  :config
  (setq-default js-indent-level 2))

;; TypeScript Mode
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-tsx-mode))
  :custom
  (typescript-indent-level 2)
  :hook ((typescript-mode . lsp-deferred)
         (typescript-tsx-mode . lsp-deferred))
  :config
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript[TSX]")
  (setq-default typescript-indent-level 2))

;; JSX Support
(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :custom
  (js-indent-level 2)
  (sgml-basic-offset 2)
  :hook (rjsx-mode . lsp-deferred)
  :config
  (setq-default js-indent-level 2))

;; JSON Mode
(use-package json-mode
  :mode "\\.json\\'"
  :custom (json-reformat:indent-width 2)
  :hook (json-mode . lsp-deferred))

;; LSP Configuration
(with-eval-after-load 'lsp-mode
  (setq lsp-typescript-suggest-auto-imports t)
  (setq lsp-eslint-auto-fix-on-save t))

;; Prettier Integration
(with-eval-after-load 'apheleia
  (dolist (mode '(js-mode typescript-mode typescript-tsx-mode rjsx-mode json-mode))
    (add-to-list 'apheleia-mode-alist (cons mode 'prettier))))

(provide 'javascript)
;;; javascript.el ends here
