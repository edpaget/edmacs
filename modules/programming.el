;;; programming.el --- General programming configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Language-agnostic programming tools: LSP, syntax checking, formatting, etc.
;; Language-specific configurations belong in modules/languages/*.el

;;; Code:

;; ============================================================================
;; LSP Mode - Language Server Protocol
;; ============================================================================

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")

  :config
  ;; Performance tuning
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-snippet t
        lsp-enable-symbol-highlighting t
        lsp-enable-links t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
        lsp-completion-provider :none  ; Use corfu instead
        lsp-headerline-breadcrumb-enable t)

  ;; Configure which-key for LSP commands
  (with-eval-after-load 'which-key
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

  ;; LSP keybindings with general.el
  (general-define-key
   :states 'normal
   :keymaps 'lsp-mode-map
   :prefix "SPC c"
   "a" '(lsp-execute-code-action :which-key "code action")
   "r" '(lsp-rename :which-key "rename")
   "f" '(lsp-format-buffer :which-key "format")
   "d" '(lsp-find-definition :which-key "definition")
   "D" '(lsp-find-declaration :which-key "declaration")
   "i" '(lsp-find-implementation :which-key "implementation")
   "t" '(lsp-find-type-definition :which-key "type definition")
   "R" '(lsp-find-references :which-key "references")
   "s" '(consult-lsp-symbols :which-key "symbols")
   "S" '(consult-lsp-file-symbols :which-key "file symbols")))

;; LSP UI - Enhanced UI for LSP
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.5
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil))

;; Consult-LSP - Consult integration for LSP
(use-package consult-lsp
  :after (consult lsp-mode))

;; ============================================================================
;; Flycheck - Syntax checking
;; ============================================================================

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.3)

  ;; Flycheck keybindings
  (general-define-key
   :states 'normal
   :prefix "SPC c"
   "x" '(:ignore t :which-key "flycheck")
   "xl" '(flycheck-list-errors :which-key "list errors")
   "xn" '(flycheck-next-error :which-key "next error")
   "xp" '(flycheck-previous-error :which-key "previous error")
   "xv" '(flycheck-verify-setup :which-key "verify setup")))

;; ============================================================================
;; Apheleia - Async code formatting
;; ============================================================================

(use-package apheleia
  :config
  (apheleia-global-mode +1)

  ;; Keybinding for manual formatting
  (general-define-key
   :states 'normal
   :prefix "SPC c"
   "F" '(apheleia-format-buffer :which-key "format (apheleia)")))

;; ============================================================================
;; Tree-sitter - Built-in support for Emacs 29+
;; ============================================================================

;; Emacs 29+ has built-in tree-sitter support via treesit
;; treesit-auto automatically uses tree-sitter modes when available
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)  ; Prompt to install missing grammars

  ;; Disable tree-sitter for markdown - it requires a complex split parser setup
  ;; and markdown-mode already provides excellent syntax highlighting
  (setq treesit-auto-langs
        (delete 'markdown (copy-sequence treesit-auto-langs)))

  (global-treesit-auto-mode))

;; ============================================================================
;; Smartparens - Better parenthesis handling
;; ============================================================================

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; ============================================================================
;; Code Folding
;; ============================================================================

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC c"
   "z" '(:ignore t :which-key "fold")
   "zc" '(hs-hide-block :which-key "hide block")
   "zo" '(hs-show-block :which-key "show block")
   "zC" '(hs-hide-all :which-key "hide all")
   "zO" '(hs-show-all :which-key "show all")))

;; ============================================================================
;; Quickrun - Execute code quickly
;; ============================================================================

(use-package quickrun
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC c"
   "r" '(quickrun :which-key "run code")))

;; ============================================================================
;; Comments
;; ============================================================================

;; Better comment/uncomment (works with evil-commentary)
(general-define-key
 :states '(normal visual)
 :prefix "SPC c"
 "c" '(evilnc-comment-or-uncomment-lines :which-key "comment/uncomment"))

;; ============================================================================
;; EditorConfig - Maintain consistent coding styles
;; ============================================================================

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; ============================================================================
;; Projectile - Project management
;; ============================================================================

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)

  ;; Projectile settings
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf)

  ;; Projectile keybindings
  (general-define-key
   :states 'normal
   :prefix "SPC p"
   "" '(:ignore t :which-key "project")
   "p" '(projectile-switch-project :which-key "switch project")
   "f" '(projectile-find-file :which-key "find file")
   "d" '(projectile-find-dir :which-key "find dir")
   "b" '(projectile-switch-to-buffer :which-key "switch buffer")
   "k" '(projectile-kill-buffers :which-key "kill buffers")
   "r" '(projectile-recentf :which-key "recent files")
   "g" '(projectile-grep :which-key "grep")
   "c" '(projectile-compile-project :which-key "compile")
   "t" '(projectile-test-project :which-key "test")
   "i" '(projectile-invalidate-cache :which-key "invalidate cache")
   "R" '(projectile-replace :which-key "replace")
   "x" '(projectile-run-shell-command-in-root :which-key "run shell cmd")))

;; ============================================================================
;; Project.el enhancements (built-in alternative to projectile)
;; ============================================================================

;; Add project keybindings for built-in project.el (as backup)
;; (general-define-key
;;  :states 'normal
;;  :prefix "SPC P"
;;  "c" '(project-compile :which-key "compile")
;;  "k" '(project-kill-buffers :which-key "kill buffers")
;;  "e" '(project-eshell :which-key "eshell"))

;;; programming.el ends here
