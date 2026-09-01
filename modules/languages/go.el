;;; go.el --- Go language configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Go development setup with LSP, tree-sitter support, and go.mod integration.
;; This file is loaded on-demand when opening Go files.

;;; Code:

;; ============================================================================
;; Go Mode (Tree-sitter)
;; ============================================================================

;; go-ts-mode is automatically enabled by treesit-auto for .go files
;; No need to explicitly configure mode associations

(with-eval-after-load 'go-ts-mode
  ;; Enable LSP for Go
  ;; Requires gopls: go install golang.org/x/tools/gopls@latest
  (add-hook 'go-ts-mode-hook #'lsp-deferred)

  ;; Enable smartparens for structural editing
  (add-hook 'go-ts-mode-hook #'smartparens-mode)

  ;; Go-specific settings
  (setq go-ts-mode-indent-offset 4))

;; ============================================================================
;; go-mode - Additional Go tooling
;; ============================================================================

;; go-mode provides additional utilities beyond what go-ts-mode offers
(use-package go-mode
  :defer t
  :config
  ;; Use goimports for formatting (also handles imports)
  (setq gofmt-command "goimports")

  ;; Common Go keybindings with local leader
  (general-define-key
   :states 'normal
   :keymaps 'go-ts-mode-map
   :prefix ","
   "" '(:ignore t :which-key "go")

   ;; Build/Run
   "c" '(:ignore t :which-key "compile")
   "cc" '(go-mode-compile :which-key "build")
   "cr" '(go-run :which-key "run")

   ;; Testing
   "t" '(:ignore t :which-key "test")
   "tt" '(go-test-current-test :which-key "current test")
   "tf" '(go-test-current-file :which-key "file tests")
   "tp" '(go-test-current-project :which-key "project tests")
   "tb" '(go-test-current-benchmark :which-key "benchmark")
   "tc" '(go-test-current-coverage :which-key "coverage")

   ;; Navigation
   "g" '(:ignore t :which-key "goto")
   "ga" '(go-goto-arguments :which-key "arguments")
   "gd" '(go-goto-docstring :which-key "docstring")
   "gf" '(go-goto-function :which-key "function")
   "gn" '(go-goto-function-name :which-key "function name")
   "gr" '(go-goto-return-values :which-key "return values")
   "gi" '(go-goto-imports :which-key "imports")
   "gm" '(go-goto-method-receiver :which-key "method receiver")

   ;; Imports
   "i" '(:ignore t :which-key "imports")
   "ia" '(go-import-add :which-key "add import")
   "ir" '(go-remove-unused-imports :which-key "remove unused")

   ;; Documentation
   "d" '(:ignore t :which-key "doc")
   "dd" '(lsp-describe-thing-at-point :which-key "describe")
   "dg" '(godoc-at-point :which-key "godoc")

   ;; Fill struct
   "s" '(go-fill-struct :which-key "fill struct")))

;; ============================================================================
;; LSP gopls Configuration
;; ============================================================================

(with-eval-after-load 'lsp-mode
  ;; gopls settings
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (shadow . t)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (useany . t)
                          (unusedvariable . t)))

  ;; Enable inlay hints
  (setq lsp-go-use-placeholders t
        lsp-go-hover-kind "FullDocumentation")

  ;; gopls build flags (customize as needed)
  (setq lsp-go-build-flags []))

;; ============================================================================
;; go-tag - Struct tag management
;; ============================================================================

(use-package go-tag
  :after go-mode
  :config
  (setq go-tag-args '("-transform" "camelcase"))

  (general-define-key
   :states 'normal
   :keymaps 'go-ts-mode-map
   :prefix ","
   "T" '(:ignore t :which-key "tags")
   "Ta" '(go-tag-add :which-key "add tag")
   "Tr" '(go-tag-remove :which-key "remove tag")))

;; ============================================================================
;; go-fill-struct - Fill struct fields
;; ============================================================================

(use-package go-fill-struct
  :after go-mode)

;; ============================================================================
;; go-impl - Generate interface implementations
;; ============================================================================

(use-package go-impl
  :after go-mode
  :config
  (general-define-key
   :states 'normal
   :keymaps 'go-ts-mode-map
   :prefix ","
   "I" '(go-impl :which-key "implement interface")))

;; ============================================================================
;; go-gen-test - Generate test boilerplate
;; ============================================================================

(use-package go-gen-test
  :after go-mode
  :config
  (general-define-key
   :states 'normal
   :keymaps 'go-ts-mode-map
   :prefix ","
   "tg" '(:ignore t :which-key "generate")
   "tgf" '(go-gen-test-dwim :which-key "generate test")
   "tge" '(go-gen-test-exported :which-key "exported functions")
   "tga" '(go-gen-test-all :which-key "all functions")))

;; ============================================================================
;; gotest - Enhanced test running
;; ============================================================================

(use-package gotest
  :after go-mode
  :config
  (setq go-test-verbose t))

;; ============================================================================
;; Apheleia - Format on save with goimports
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; Ensure goimports is used for Go files (handles formatting + imports)
  (add-to-list 'apheleia-mode-alist '(go-ts-mode . goimports))
  (add-to-list 'apheleia-mode-alist '(go-mode . goimports)))

;; ============================================================================
;; go.mod support
;; ============================================================================

(use-package go-mod-mode
  :straight (go-mod-mode :type git :host github :repo "zkry/go-mod-mode")
  :mode "go\\.mod\\'"
  :config
  ;; Enable LSP for go.mod files
  (add-hook 'go-mod-mode-hook #'lsp-deferred))

(provide 'go)
;;; go.el ends here
