;;; keybindings.el --- Keybinding configuration with general.el -*- lexical-binding: t -*-

;;; Commentary:
;; Self-documenting keybindings using general.el with SPC as leader key.

;;; Code:

;; ============================================================================
;; General - Keybinding framework
;; ============================================================================

(use-package general
  :config
  ;; Set up SPC as the global leader key
  (general-create-definer leader-def
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Set up local leader (for mode-specific bindings)
  (general-create-definer local-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix ","
    :global-prefix "C-,"))

;; ============================================================================
;; Global Leader Key Bindings
;; ============================================================================

(leader-def
  "" nil  ; Unbind SPC in these states

  ;; Simple commands
  "SPC" '(execute-extended-command :which-key "M-x")
  "TAB" '((lambda () (interactive) (switch-to-buffer nil)) :which-key "last buffer")
  "!" '(shell-command :which-key "shell command")
  ":" '(eval-expression :which-key "eval")

  ;; Files
  "f" '(:ignore t :which-key "files")
  "ff" '(find-file :which-key "find file")
  "fr" '(recentf-open-files :which-key "recent files")
  "fs" '(save-buffer :which-key "save file")
  "fS" '(write-file :which-key "save as")
  "fd" '(dired :which-key "dired")
  "fD" '(delete-file :which-key "delete file")
  "fy" '((lambda () (interactive) (kill-new (buffer-file-name))) :which-key "yank path")

  ;; Buffers
  "b" '(:ignore t :which-key "buffers")
  "bb" '(consult-buffer :which-key "switch buffer")
  "bd" '(kill-this-buffer :which-key "kill buffer")
  "bD" '((lambda () (interactive) (kill-buffer)) :which-key "kill buffer (choose)")
  "bs" '(save-buffer :which-key "save buffer")
  "bS" '(save-some-buffers :which-key "save all buffers")
  "br" '(revert-buffer :which-key "revert buffer")
  "bn" '(next-buffer :which-key "next buffer")
  "bp" '(previous-buffer :which-key "previous buffer")
  "bi" '(ibuffer :which-key "ibuffer")

  ;; Windows
  "w" '(:ignore t :which-key "windows")
  "ww" '(other-window :which-key "other window")
  "wd" '(delete-window :which-key "delete window")
  "wD" '(delete-other-windows :which-key "delete other windows")
  "ws" '(split-window-below :which-key "split below")
  "wv" '(split-window-right :which-key "split right")
  "wh" '(evil-window-left :which-key "window left")
  "wj" '(evil-window-down :which-key "window down")
  "wk" '(evil-window-up :which-key "window up")
  "wl" '(evil-window-right :which-key "window right")
  "w=" '(balance-windows :which-key "balance windows")

  ;; Search
  "s" '(:ignore t :which-key "search")
  "ss" '(consult-line :which-key "search buffer")
  "sp" '(consult-ripgrep :which-key "search project")
  "sf" '(consult-find :which-key "find file")
  "si" '(consult-imenu :which-key "imenu")

  ;; Projects
  "p" '(:ignore t :which-key "projects")
  "pf" '(project-find-file :which-key "find file")
  "pp" '(project-switch-project :which-key "switch project")
  "pb" '(consult-project-buffer :which-key "project buffers")
  "pd" '(project-dired :which-key "project dired")
  "ps" '(consult-ripgrep :which-key "search project")

  ;; Git (will be populated by git.el)
  "g" '(:ignore t :which-key "git")

  ;; Code (will be populated by programming.el)
  "c" '(:ignore t :which-key "code")

  ;; AI (will be populated by ai.el)
  "a" '(:ignore t :which-key "ai")

  ;; Org (will be populated by org-config.el)
  "o" '(:ignore t :which-key "org")

  ;; Help
  "h" '(:ignore t :which-key "help")
  "hf" '(describe-function :which-key "describe function")
  "hv" '(describe-variable :which-key "describe variable")
  "hk" '(describe-key :which-key "describe key")
  "hm" '(describe-mode :which-key "describe mode")
  "hp" '(describe-package :which-key "describe package")
  "hF" '(describe-face :which-key "describe face")

  ;; Toggle
  "t" '(:ignore t :which-key "toggle")
  "tl" '(display-line-numbers-mode :which-key "line numbers")
  "tw" '(toggle-truncate-lines :which-key "truncate lines")
  "tv" '(visual-line-mode :which-key "visual line mode")

  ;; Quit/Session
  "q" '(:ignore t :which-key "quit")
  "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
  "qQ" '(kill-emacs :which-key "quit without saving")
  "qr" '(restart-emacs :which-key "restart emacs"))

;; ============================================================================
;; Mode-specific bindings (using local leader)
;; ============================================================================

;; These will be extended by individual modules as needed

;; ============================================================================
;; Evil-specific keybindings
;; ============================================================================

(general-define-key
 :states '(normal visual)
 ;; Better escape
 "C-g" 'evil-normal-state

 ;; Better window navigation
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-k" 'evil-window-up
 "C-l" 'evil-window-right)

;; Visual mode mappings
(general-define-key
 :states 'visual
 "<" 'evil-shift-left
 ">" 'evil-shift-right)

;;; keybindings.el ends here
