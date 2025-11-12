;;; rust.el --- Rust language configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Rust development setup with LSP, cargo integration, and tree-sitter support.
;; This file is loaded on-demand when opening Rust files.

;;; Code:

;; ============================================================================
;; Rust Mode (Tree-sitter)
;; ============================================================================

;; rust-ts-mode is automatically enabled by treesit-auto for .rs files
;; No need to explicitly configure mode associations

(with-eval-after-load 'rust-ts-mode
  ;; Enable LSP for Rust
  ;; Requires rust-analyzer: rustup component add rust-analyzer
  (add-hook 'rust-ts-mode-hook #'lsp-deferred)

  ;; Enable smartparens for structural editing
  (add-hook 'rust-ts-mode-hook #'smartparens-mode)

  ;; Rust-specific indentation
  (setq rust-indent-offset 4))

;; ============================================================================
;; Rustic - Enhanced Rust mode with Cargo integration
;; ============================================================================

(use-package rustic
  :config
  ;; Use rust-ts-mode for syntax highlighting instead of rustic's built-in mode
  (setq rustic-lsp-client 'lsp-mode)

  ;; Disable rustic's flycheck in favor of LSP diagnostics
  (setq rustic-flycheck-setup-mode-line-p nil)

  ;; Format on save using rustfmt
  (setq rustic-format-on-save t
        rustic-format-trigger 'on-save)

  ;; Cargo settings
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
  ;; rust-analyzer settings
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
  ;; Ensure rustfmt is used for Rust files
  (add-to-list 'apheleia-mode-alist '(rust-ts-mode . rustfmt))
  (add-to-list 'apheleia-mode-alist '(rustic-mode . rustfmt)))

(provide 'rust)
;;; rust.el ends here
