;;; lisp-common.el --- Shared configuration for all Lisp languages -*- lexical-binding: t -*-

;;; Commentary:
;; Common configuration for all Lisp-family languages (Emacs Lisp, Clojure, etc.)
;; This includes shared smartparens keybindings and structural editing setup.

;;; Code:

;; ============================================================================
;; Smartparens Keybindings for Lisp Languages
;; ============================================================================

(defun lisp-common-setup-smartparens-keys (mode-map key-prefix)
  "Set up smartparens keybindings for MODE-MAP under KEY-PREFIX.
KEY-PREFIX should be a string like \", s\" or \", k\".
This creates a consistent set of structural editing keybindings for all Lisp modes."
  (with-eval-after-load 'smartparens
    (general-define-key
     :states 'normal
     :keymaps mode-map
     :prefix key-prefix
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

(provide 'lisp-common)
;;; lisp-common.el ends here
