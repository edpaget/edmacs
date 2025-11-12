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
;; Evil Tree-Sitter Text Objects
;; ============================================================================

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  ;; Define text objects for various code structures
  ;; Inner/outer function
  (define-key evil-outer-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; Inner/outer class
  (define-key evil-outer-text-objects-map "c"
              (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c"
              (evil-textobj-tree-sitter-get-textobj "class.inner"))

  ;; Inner/outer loop
  (define-key evil-outer-text-objects-map "l"
              (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "l"
              (evil-textobj-tree-sitter-get-textobj "loop.inner"))

  ;; Inner/outer conditional
  (define-key evil-outer-text-objects-map "o"
              (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "o"
              (evil-textobj-tree-sitter-get-textobj "conditional.inner"))

  ;; Inner/outer call (function call)
  (define-key evil-outer-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (define-key evil-inner-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj "call.inner"))

  ;; Inner/outer comment
  (define-key evil-outer-text-objects-map "/"
              (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map "/"
              (evil-textobj-tree-sitter-get-textobj "comment.inner"))

  ;; Parameter/argument text object (special - inner only makes sense)
  (define-key evil-inner-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj "parameter.inner"))

  ;; Navigation with goto-textobj
  ;; Jump to next/previous function
  (general-define-key
   :states '(normal visual)
   "]f" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "function.outer"))
   "[f" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "function.outer" t))

   ;; Jump to next/previous class
   "]c" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "class.outer"))
   "[c" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "class.outer" t))

   ;; Jump to next/previous conditional
   "]o" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "conditional.outer"))
   "[o" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t))))

;; ============================================================================
;; Combobulate - Structural Editing with Tree-Sitter
;; ============================================================================
;; Note: transient (required by combobulate) is loaded early in core.el

(use-package combobulate
  :straight (combobulate :type git
                         :host github
                         :repo "mickeynp/combobulate"
                         :branch "development")
  :after treesit
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)
         (rust-ts-mode . combobulate-mode)
         (clojure-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (html-ts-mode . combobulate-mode))

  :config
  ;; Evil-friendly keybindings for combobulate
  (general-define-key
   :states '(normal visual)
   :keymaps 'combobulate-key-map
   :prefix "SPC k"
   "" '(:ignore t :which-key "combobulate")

   ;; Navigation
   "n" '(combobulate-navigate-next :which-key "next node")
   "p" '(combobulate-navigate-previous :which-key "previous node")
   "u" '(combobulate-navigate-up :which-key "up to parent")
   "d" '(combobulate-navigate-down :which-key "down to child")

   ;; Editing
   "k" '(combobulate-drag-up :which-key "drag up")
   "j" '(combobulate-drag-down :which-key "drag down")
   "r" '(combobulate-splice-up :which-key "splice up")
   "s" '(combobulate-splice-down :which-key "splice down")
   "c" '(combobulate-clone-node-dwim :which-key "clone node")
   "x" '(combobulate-vanish-node :which-key "vanish/delete node")

   ;; Marking/Selection
   "m" '(combobulate-mark-node-dwim :which-key "mark node")
   "e" '(combobulate-envelop-node :which-key "envelop node")

   ;; Transient menu
   "o" '(combobulate :which-key "combobulate menu"))

  ;; Also bind some common operations to more accessible keys
  (general-define-key
   :states 'normal
   :keymaps 'combobulate-key-map
   "M-j" 'combobulate-drag-down
   "M-k" 'combobulate-drag-up
   "M-h" 'combobulate-navigate-previous
   "M-l" 'combobulate-navigate-next))

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

;; ============================================================================
;; YAML Mode - YAML file editing
;; ============================================================================

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :config
  ;; Indentation settings for YAML
  (setq yaml-indent-offset 2)

  ;; Enable flycheck for YAML
  (add-hook 'yaml-mode-hook #'flycheck-mode)

  ;; Enable LSP for YAML (if yaml-language-server is installed)
  ;; Install: npm install -g yaml-language-server
  (add-hook 'yaml-mode-hook #'lsp-deferred))

;;; programming.el ends here
