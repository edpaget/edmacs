;;; clojure.el --- Clojure language configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Clojure development setup with CIDER and related tools.
;; This file is loaded on-demand when opening Clojure files.
;; Uses clojure-ts-mode for tree-sitter support.

;;; Code:

;; ============================================================================
;; Clojure Mode (Tree-sitter)
;; ============================================================================

(use-package clojure-ts-mode
  :mode (("\\.clj\\'" . clojure-ts-mode)
         ("\\.cljs\\'" . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-mode)
         ("\\.edn\\'" . clojure-ts-mode))
  :config
  ;; Indentation rules
  (setq clojure-ts-indent-style 'align-arguments)

  ;; Enable LSP
  (add-hook 'clojure-ts-mode-hook #'lsp-deferred)

  ;; Enable smartparens for structural editing
  (add-hook 'clojure-ts-mode-hook #'smartparens-strict-mode))

;; ============================================================================
;; CIDER - Clojure Interactive Development Environment
;; ============================================================================

(use-package cider
  :commands (cider-jack-in cider-connect)
  :config
  ;; REPL settings
  (setq cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file (expand-file-name "cider-history" user-emacs-directory)
        cider-repl-wrap-history t
        cider-repl-history-size 3000
        cider-show-eval-spinner t
        cider-overlays-use-font-lock t)

  ;; Evaluation settings
  (setq cider-font-lock-dynamically '(macro core function var)
        cider-prompt-for-symbol nil)

  ;; CIDER keybindings with local leader
  (general-define-key
   :states 'normal
   :keymaps 'clojure-ts-mode-map
   :prefix ","
   "" '(:ignore t :which-key "clojure")

   ;; REPL
   "'" '(cider-jack-in :which-key "jack-in")
   "\"" '(cider-jack-in-cljs :which-key "jack-in-cljs")
   "c" '(cider-connect :which-key "connect")

   ;; Evaluation
   "e" '(:ignore t :which-key "eval")
   "eb" '(cider-eval-buffer :which-key "buffer")
   "ee" '(cider-eval-last-sexp :which-key "sexp")
   "ef" '(cider-eval-defun-at-point :which-key "defun")
   "er" '(cider-eval-region :which-key "region")
   "ep" '(cider-pprint-eval-last-sexp :which-key "pprint last sexp")

   ;; Testing
   "t" '(:ignore t :which-key "test")
   "tt" '(cider-test-run-test :which-key "run test")
   "tn" '(cider-test-run-ns-tests :which-key "run ns tests")
   "tp" '(cider-test-run-project-tests :which-key "run project tests")
   "tr" '(cider-test-rerun-failed-tests :which-key "rerun failed")

   ;; Documentation
   "d" '(:ignore t :which-key "doc")
   "dd" '(cider-doc :which-key "doc")
   "dj" '(cider-javadoc :which-key "javadoc")
   "dc" '(cider-clojuredocs :which-key "clojuredocs")

   ;; Navigation
   "g" '(:ignore t :which-key "goto")
   "gb" '(cider-pop-back :which-key "back")
   "gr" '(cider-find-resource :which-key "find resource")
   "gn" '(cider-find-ns :which-key "find namespace")

   ;; REPL interaction
   "s" '(:ignore t :which-key "repl")
   "ss" '(cider-switch-to-repl-buffer :which-key "switch to repl")
   "sn" '(cider-repl-set-ns :which-key "set ns")
   "sq" '(cider-quit :which-key "quit")
   "sR" '(cider-restart :which-key "restart")

   ;; Refactoring (requires clj-refactor)
   "r" '(:ignore t :which-key "refactor")
   "rr" '(cljr-rename-symbol :which-key "rename")
   "ra" '(cljr-add-require-to-ns :which-key "add require")
   "ri" '(cljr-add-import-to-ns :which-key "add import"))

  ;; Smartparens keybindings for structural editing
  (with-eval-after-load 'smartparens
    (general-define-key
     :states 'normal
     :keymaps 'clojure-ts-mode-map
     :prefix ", k"
     "" '(:ignore t :which-key "smartparens")

     ;; Navigation
     "f" '(sp-forward-sexp :which-key "forward sexp")
     "b" '(sp-backward-sexp :which-key "backward sexp")
     "d" '(sp-down-sexp :which-key "down sexp")
     "u" '(sp-up-sexp :which-key "up sexp")
     "n" '(sp-next-sexp :which-key "next sexp")
     "p" '(sp-previous-sexp :which-key "previous sexp")

     ;; Slurping and barfing (essential for Lisp!)
     ">" '(sp-forward-slurp-sexp :which-key "slurp forward")
     "<" '(sp-forward-barf-sexp :which-key "barf forward")
     "S" '(sp-backward-slurp-sexp :which-key "slurp backward")
     "B" '(sp-backward-barf-sexp :which-key "barf backward")

     ;; Wrapping
     "w" '(:ignore t :which-key "wrap")
     "w(" '(sp-wrap-round :which-key "wrap ()")
     "w[" '(sp-wrap-square :which-key "wrap []")
     "w{" '(sp-wrap-curly :which-key "wrap {}")

     ;; Unwrapping/Splicing
     "s" '(sp-splice-sexp :which-key "splice")
     "r" '(sp-raise-sexp :which-key "raise sexp")
     "j" '(sp-join-sexp :which-key "join sexp")
     "t" '(sp-transpose-sexp :which-key "transpose")

     ;; Killing
     "k" '(sp-kill-sexp :which-key "kill sexp")
     "K" '(sp-backward-kill-sexp :which-key "backward kill sexp")
     "c" '(sp-copy-sexp :which-key "copy sexp")

     ;; Depth changing
     "D" '(sp-splice-sexp-killing-around :which-key "splice killing around")
     "F" '(sp-splice-sexp-killing-forward :which-key "splice killing forward")
     "B" '(sp-splice-sexp-killing-backward :which-key "splice killing backward"))))

;; ============================================================================
;; clj-refactor - Refactoring support
;; ============================================================================

(use-package clj-refactor
  :hook (clojure-ts-mode . clj-refactor-mode)
  :config
  (setq cljr-warn-on-eval nil
        cljr-suppress-middleware-warnings t)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;;; clojure.el ends here
