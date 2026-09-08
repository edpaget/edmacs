;;; programming.el --- General programming configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Language-agnostic programming tools: eglot, flymake, formatting, etc.
;; Language-specific configurations belong in modules/languages/*.el

;;; Code:

;; ============================================================================
;; Eglot - built-in Language Server Protocol client
;; ============================================================================

(use-package eglot
  :straight nil
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        ;; Default 3s blocks the frame that long per `eglot-ensure'; with a
        ;; desktop restore full of Go buffers that adds up. Finish connecting
        ;; asynchronously instead.
        eglot-sync-connect 1
        ;; Default retains 2M characters of JSON per server for nothing.
        eglot-events-buffer-config '(:size 0 :format full)
        eglot-extend-to-xref t)

  ;; Only the verbs eglot itself owns; the client-agnostic diagnostics keys
  ;; live on the global `SPC c' map in the flymake block below.
  (general-define-key
   :states 'normal
   :keymaps 'eglot-mode-map
   :prefix "SPC c"
   "a" '(eglot-code-actions :which-key "code action")
   "r" '(eglot-rename :which-key "rename")
   "f" '(eglot-format-buffer :which-key "format")
   "d" '(xref-find-definitions :which-key "definition")
   "D" '(eglot-find-declaration :which-key "declaration")
   "i" '(eglot-find-implementation :which-key "implementation")
   "t" '(eglot-find-typeDefinition :which-key "type definition")
   "R" '(xref-find-references :which-key "references")
   "S" '(consult-imenu :which-key "file symbols")
   "h" '(eldoc-doc-buffer :which-key "hover doc")))

;; Consult-Eglot - Consult integration for eglot
(use-package consult-eglot
  :after (consult eglot)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'eglot-mode-map
   :prefix "SPC c"
   "s" '(consult-eglot-symbols :which-key "symbols")))

;; ============================================================================
;; Flymake - built-in syntax checking
;; ============================================================================

(use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode)
  ;; `:init', not `:config': the block is deferred until something turns
  ;; flymake on, and both the settings and the keys have to be in place
  ;; before that happens.
  :init
  ;; `flymake-no-changes-timeout' stays at its default: eglot reports a
  ;; pushed diagnostic into the current check only when flymake is on an
  ;; idle timer, so nil there costs every server-sent diagnostic until the
  ;; next explicit check.
  (setq flymake-show-diagnostics-at-end-of-line 'short)

  ;; Global rather than `eglot-mode-map': flymake also runs in elisp and
  ;; shell buffers no language server manages.
  (general-define-key
   :states 'normal
   :prefix "SPC c"
   "w" '(consult-flymake :which-key "workspace diagnostics")
   "W" '(flymake-show-project-diagnostics :which-key "workspace diagnostics tree")
   "x" '(:ignore t :which-key "diagnostics")
   "xl" '(flymake-show-buffer-diagnostics :which-key "list errors")
   "xn" '(flymake-goto-next-error :which-key "next error")
   "xp" '(flymake-goto-prev-error :which-key "previous error")
   "xv" '(flymake-switch-to-log-buffer :which-key "flymake log")))

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

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)  ; Prompt to install missing grammars

  ;; markdown needs a split parser setup and markdown-mode highlights well already.
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
  (define-key evil-outer-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c"
              (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c"
              (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "l"
              (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "l"
              (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "o"
              (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "o"
              (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
  (define-key evil-outer-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (define-key evil-inner-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj "call.inner"))
  (define-key evil-outer-text-objects-map "/"
              (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map "/"
              (evil-textobj-tree-sitter-get-textobj "comment.inner"))
  ;; Parameter: inner only
  (define-key evil-inner-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj "parameter.inner"))

  ;; ]f/[f etc. jump to the next/previous node
  (general-define-key
   :states '(normal visual)
   "]f" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "function.outer"))
   "[f" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "function.outer" t))

   "]c" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "class.outer"))
   "[c" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "class.outer" t))

   "]o" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "conditional.outer"))
   "[o" (lambda ()
          (interactive)
          (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t))))

;; ============================================================================
;; Combobulate - Structural Editing with Tree-Sitter
;; ============================================================================
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
         (go-ts-mode . combobulate-mode)
         (clojure-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (html-ts-mode . combobulate-mode))

  :config
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
  :commands quickrun)

;; Outside use-package so the binding exists before quickrun autoloads.
(general-define-key
 :states 'normal
 :prefix "SPC c"
 "r" '(quickrun :which-key "run code"))

;; ============================================================================
;; Comments
;; ============================================================================

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
;; Project.el enhancements (built-in alternative to projectile)
;; ============================================================================

;; core.el configures project.el and keybindings.el owns SPC p; extras go
;; under SPC P.
(general-define-key
 :states 'normal
 :prefix "SPC P"
 "" '(:ignore t :which-key "project (extra)")
 "c" '(project-compile :which-key "compile")
 "k" '(project-kill-buffers :which-key "kill buffers")
 "e" '(project-eshell :which-key "eshell"))

;; ============================================================================
;; YAML Mode - YAML file editing
;; ============================================================================

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :config
  (setq yaml-indent-offset 2)

  ;; Needs yaml-language-server: npm install -g yaml-language-server.
  ;; Guarded, unlike the unconditional hook this replaced: a bare
  ;; `eglot-ensure' errors visibly on every yaml file when the server is
  ;; not installed.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((yaml-mode yaml-ts-mode) . ("yaml-language-server" "--stdio"))))
  (add-hook 'yaml-mode-hook
            (lambda ()
              (when (executable-find "yaml-language-server")
                (eglot-ensure)))))

;;; programming.el ends here
