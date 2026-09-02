;;; rust.el --- Rust language configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Rust development setup with LSP, cargo integration, and tree-sitter support.
;; This file is loaded on-demand when opening Rust files.

;;; Code:

;; ============================================================================
;; Rust Mode (Tree-sitter)
;; ============================================================================

;; treesit-auto maps .rs to rust-ts-mode.

(with-eval-after-load 'rust-ts-mode
  ;; Requires rust-analyzer: rustup component add rust-analyzer
  (add-hook 'rust-ts-mode-hook #'lsp-deferred)

  (add-hook 'rust-ts-mode-hook #'smartparens-mode)

  (setq rust-indent-offset 4))

;; ============================================================================
;; Rustic - Enhanced Rust mode with Cargo integration
;; ============================================================================

(use-package rustic
  ;; `:mode', not `:defer t': rustic's top-level code is what claims `.rs' in
  ;; auto-mode-alist, so without an autoload trigger `.rs' would fall through
  ;; to treesit-auto/rust-mode and rustic would never load.
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-lsp-client 'lsp-mode)

  (setq rustic-flycheck-setup-mode-line-p nil)

  (setq rustic-format-on-save t
        rustic-format-trigger 'on-save)

  (setq rustic-cargo-use-last-stored-arguments t)

  ;; Rustic keybindings with local leader
  (general-define-key
   :states 'normal
   :keymaps 'rustic-mode-map
   :prefix ","
   "" '(:ignore t :which-key "rust")

   ;; Cargo commands
   "c" '(:ignore t :which-key "cargo")
   "cc" '(rustic-cargo-build :which-key "build")
   "cr" '(rustic-cargo-run :which-key "run")
   "ct" '(rustic-cargo-test :which-key "test")
   "cb" '(rustic-cargo-bench :which-key "bench")
   "ck" '(rustic-cargo-check :which-key "check")
   "cl" '(rustic-cargo-clippy :which-key "clippy")
   "cf" '(rustic-cargo-fmt :which-key "format")
   "ca" '(rustic-cargo-add :which-key "add dependency")
   "cu" '(rustic-cargo-upgrade :which-key "upgrade dependencies")
   "co" '(rustic-cargo-outdated :which-key "outdated dependencies")

   ;; Testing
   "t" '(:ignore t :which-key "test")
   "tt" '(rustic-cargo-test-run :which-key "run test")
   "ta" '(rustic-cargo-test :which-key "all tests")
   "tl" '(rustic-cargo-current-test :which-key "current test")

   ;; Documentation
   "d" '(:ignore t :which-key "doc")
   "dd" '(lsp-describe-thing-at-point :which-key "describe")
   "do" '(rustic-cargo-doc :which-key "open docs")

   ;; Repl/Playground
   "p" '(rustic-playground :which-key "playground")))

;; ============================================================================
;; LSP Rust-Analyzer Configuration
;; ============================================================================

(with-eval-after-load 'lsp-mode
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil))

;; ============================================================================
;; Cargo Mode - Additional cargo integration
;; ============================================================================

(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode)
  :config
  (setq cargo-process--command-flags ""))

;; ============================================================================
;; Apheleia - Format on save with rustfmt
;; ============================================================================

(with-eval-after-load 'apheleia
  (add-to-list 'apheleia-mode-alist '(rust-ts-mode . rustfmt))
  (add-to-list 'apheleia-mode-alist '(rustic-mode . rustfmt)))

(provide 'rust)
;;; rust.el ends here
