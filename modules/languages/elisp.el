;;; elisp.el --- Emacs Lisp configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Emacs Lisp development configuration with structural editing.
;; Uses smartparens for structural editing (no tree-sitter mode available).

;;; Code:

;; ============================================================================
;; Emacs Lisp Mode (Built-in)
;; ============================================================================

(use-package emacs-lisp-mode
  :straight nil
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("Cask\\'" . emacs-lisp-mode))
  :interpreter ("emacs" . emacs-lisp-mode)
  :config
  ;; Enable smartparens strict mode for structural editing
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

  ;; Enable eldoc for inline documentation
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

  ;; Enable aggressive indent for auto-indentation
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

  ;; Indentation settings
  (setq lisp-indent-offset 2))

;; ============================================================================
;; Language-Specific Keybindings
;; ============================================================================

(general-define-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 :prefix ","
 "" '(:ignore t :which-key "elisp")

 ;; Evaluation
 "e" '(:ignore t :which-key "eval")
 "eb" '(eval-buffer :which-key "eval buffer")
 "ed" '(eval-defun :which-key "eval defun")
 "ee" '(eval-last-sexp :which-key "eval last sexp")
 "er" '(eval-region :which-key "eval region")
 "eS" '(eval-print-last-sexp :which-key "eval and print")

 ;; Macroexpansion
 "m" '(:ignore t :which-key "macro")
 "me" '(macrostep-expand :which-key "expand macro")
 "mc" '(macrostep-collapse :which-key "collapse macro")
 "mn" '(macrostep-next :which-key "next macro")
 "mp" '(macrostep-prev :which-key "previous macro")

 ;; Documentation
 "d" '(:ignore t :which-key "doc")
 "dd" '(describe-function :which-key "describe function")
 "dv" '(describe-variable :which-key "describe variable")
 "dk" '(describe-key :which-key "describe key")
 "dm" '(describe-mode :which-key "describe mode")

 ;; Navigation
 "g" '(:ignore t :which-key "goto")
 "gf" '(find-function :which-key "find function")
 "gv" '(find-variable :which-key "find variable")
 "gl" '(find-library :which-key "find library")

 ;; Testing
 "t" '(:ignore t :which-key "test")
 "tt" '(ert :which-key "run all tests")
 "tf" '(ert-run-tests-interactively :which-key "run tests interactively")
 "td" '(ert-delete-all-tests :which-key "delete all tests")

 ;; Compilation/Byte-compile
 "c" '(:ignore t :which-key "compile")
 "cc" '(emacs-lisp-byte-compile :which-key "byte-compile file")
 "cf" '(emacs-lisp-byte-compile-and-load :which-key "compile and load")
 "cr" '(emacs-lisp-byte-compile-directory :which-key "compile directory")

 ;; Refactoring (using elisp-refs for find usages)
 "r" '(:ignore t :which-key "refactor")
 "rf" '(elisp-refs-function :which-key "find function refs")
 "rv" '(elisp-refs-variable :which-key "find variable refs")
 "rs" '(elisp-refs-symbol :which-key "find symbol refs"))

;; ============================================================================
;; Smartparens - Additional Elisp-specific bindings
;; ============================================================================

;; Add convenient smartparens keybindings for Elisp
(with-eval-after-load 'smartparens
  (general-define-key
   :states 'normal
   :keymaps 'emacs-lisp-mode-map
   :prefix ", s"
   "" '(:ignore t :which-key "smartparens")
   "f" '(sp-forward-sexp :which-key "forward sexp")
   "b" '(sp-backward-sexp :which-key "backward sexp")
   "d" '(sp-down-sexp :which-key "down sexp")
   "u" '(sp-up-sexp :which-key "up sexp")
   "n" '(sp-next-sexp :which-key "next sexp")
   "p" '(sp-previous-sexp :which-key "previous sexp")

   ;; Slurping and barfing
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

   ;; Killing
   "k" '(sp-kill-sexp :which-key "kill sexp")
   "K" '(sp-backward-kill-sexp :which-key "backward kill sexp")))

;; ============================================================================
;; Helpful Packages
;; ============================================================================

;; Macrostep - Interactive macro expansion
(use-package macrostep
  :commands (macrostep-expand))

;; Elisp-refs - Find references to Elisp symbols
(use-package elisp-refs
  :commands (elisp-refs-function elisp-refs-variable elisp-refs-symbol))

;; Aggressive Indent - Auto-indent code as you type
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config
  (setq aggressive-indent-comments-too t))

;; Suggest - Discover Elisp functions by example
(use-package suggest
  :commands suggest)

;; ============================================================================
;; IELM (Emacs Lisp REPL)
;; ============================================================================

(general-define-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 :prefix ", '"
 "" '(ielm :which-key "elisp repl"))

(provide 'elisp)
;;; elisp.el ends here
