;;; ruby.el --- Ruby language configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Ruby development setup: the built-in ruby-ts-mode, eglot when a Ruby
;; language server is installed, and rubocop through apheleia.

;;; Code:

;; treesit-auto maps .rb, Gemfile, Rakefile and friends to ruby-ts-mode.

(defvar apheleia-mode-alist)
(declare-function smartparens-mode "smartparens")

(defconst edmacs-ruby-language-servers '("ruby-lsp" "solargraph")
  "Executables eglot's built-in ruby entry contacts, in no particular order.")

(defun edmacs-ruby-maybe-eglot ()
  "Start eglot only when a Ruby language server is on `exec-path'.
Unlike gopls or rust-analyzer, none ships with the language, and an
unconditional `eglot-ensure' warns in every Ruby buffer without one."
  (when (seq-some #'executable-find edmacs-ruby-language-servers)
    (eglot-ensure)))

(with-eval-after-load 'ruby-ts-mode
  (add-hook 'ruby-ts-mode-hook #'edmacs-ruby-maybe-eglot)
  (add-hook 'ruby-ts-mode-hook #'smartparens-mode))

;; ruby-mode is treesit-auto's fallback when the grammar is not installed.
(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'edmacs-ruby-maybe-eglot))

;; apheleia defaults both modes to prettier-ruby, which needs a node plugin
;; almost no Ruby project carries.
(with-eval-after-load 'apheleia
  (setf (alist-get 'ruby-mode apheleia-mode-alist) 'rubocop
        (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop))

(provide 'ruby)
;;; ruby.el ends here
