;;; rust.el --- Rust language configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Rust development setup with eglot, cargo integration, and tree-sitter support.
;; This file is loaded on-demand when opening Rust files.

;;; Code:

;; ============================================================================
;; Rust Mode (Tree-sitter)
;; ============================================================================

;; treesit-auto maps .rs to rust-ts-mode.

;; rustic's own top-level code removes `.rs' from rust-ts-mode's
;; auto-mode-alist entry, so rust-ts-mode never activates and any
;; configuration hung off it is dead. See the `rust-ts-mode-never-activates'
;; task; eglot attaches to `rustic-mode' regardless.

;; ============================================================================
;; Rustic - Enhanced Rust mode with Cargo integration
;; ============================================================================

(use-package rustic
  ;; `:mode', not `:defer t': rustic's top-level code is what claims `.rs' in
  ;; auto-mode-alist, so without an autoload trigger `.rs' would fall through
  ;; to treesit-auto/rust-mode and rustic would never load.
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  ;; `rustic-setup-lsp' dispatches on this and calls `eglot-ensure' directly;
  ;; `rustic-setup-eglot' has already registered rustic-mode with eglot.
  (setq rustic-lsp-client 'eglot)

  ;; rustic's eglot class sends this as an initializationOption; keep it in
  ;; step with the check.command below or the two disagree.
  (setq rustic-lsp-check-command "clippy")

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
   "dd" '(eldoc-doc-buffer :which-key "describe")
   "do" '(rustic-cargo-doc :which-key "open docs")

   ;; Repl/Playground
   "p" '(rustic-playground :which-key "playground")))

;; ============================================================================
;; rust-analyzer Configuration
;; ============================================================================

(defvar eglot-workspace-configuration)

;; rust-analyzer's own settings keys, not the wrapper names of the client
;; this replaced. Global rather than buffer-local, and merged rather than
;; assigned: eglot resolves this in a temp buffer of its own, and every
;; language shares the one plist.
;;
;; Only the settings that differ from rust-analyzer's own defaults are sent.
;; `parameterHints.enable' defaults to t, so switching it off needs
;; `:json-false' -- nil would serialize as null, not false.
(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                (plist-put (default-value 'eglot-workspace-configuration)
                           :rust-analyzer
                           '(:check (:command "clippy")
                             :inlayHints
                             (:lifetimeElisionHints (:enable "skip_trivial")
                              :closureReturnTypeHints (:enable "always")
                              :parameterHints (:enable :json-false))))))

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
