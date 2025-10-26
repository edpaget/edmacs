;;; clojure.el --- Clojure language configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Clojure development setup with CIDER and related tools.
;; This file is loaded on-demand when opening Clojure files.

;;; Code:

;; ============================================================================
;; Clojure Mode
;; ============================================================================

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.edn\\'" . clojure-mode))
  :config
  ;; Indentation rules
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t)

  ;; Enable LSP
  (add-hook 'clojure-mode-hook #'lsp-deferred)
  (add-hook 'clojurescript-mode-hook #'lsp-deferred)
  (add-hook 'clojurec-mode-hook #'lsp-deferred))

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
   :keymaps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
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
   "ri" '(cljr-add-import-to-ns :which-key "add import")))

;; ============================================================================
;; clj-refactor - Refactoring support
;; ============================================================================

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (setq cljr-warn-on-eval nil
        cljr-suppress-middleware-warnings t)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; ============================================================================
;; Flycheck Clojure - Additional linters
;; ============================================================================

(use-package flycheck-clj-kondo
  :after (flycheck clojure-mode))

;;; clojure.el ends here
