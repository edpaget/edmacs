;;; swift.el --- Swift language configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Swift development setup: swift-mode, eglot against sourcekit-lsp, and
;; swift-format through apheleia.

;;; Code:

;; swift-mode rather than a tree-sitter mode: Emacs ships no swift-ts-mode,
;; and treesit-auto's swift recipe names one without providing it.

(defvar eglot-server-programs)
(defvar apheleia-mode-alist)
(defvar apheleia-formatters)
(declare-function smartparens-mode "smartparens")

(use-package swift-mode
  :mode "\\.swift\\'"
  :interpreter "swift"
  :config
  (add-hook 'swift-mode-hook #'eglot-ensure)
  (add-hook 'swift-mode-hook #'smartparens-mode))

;; eglot has no built-in swift entry.  On macOS /usr/bin/sourcekit-lsp is an
;; xcrun shim into the active toolchain.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(swift-mode "sourcekit-lsp")))

;; `--assume-filename' lets swift-format find the project's .swift-format.
(with-eval-after-load 'apheleia
  (setf (alist-get 'swift-format apheleia-formatters)
        '("swift-format" "format" "--assume-filename" filepath))
  (setf (alist-get 'swift-mode apheleia-mode-alist) 'swift-format))

(provide 'swift)
;;; swift.el ends here
