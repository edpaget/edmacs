;;; keybindings.el --- Keybinding configuration with general.el -*- lexical-binding: t -*-

;;; Commentary:
;; Self-documenting keybindings using general.el with SPC as leader key.

;;; Code:

;; ============================================================================
;; General - Keybinding framework
;; ============================================================================

(use-package general
  :config
  (general-create-definer leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Set up local leader (for mode-specific bindings)
  (general-create-definer local-leader-def
    :states '(normal visual motion)
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
  "wd" '(edmacs-window-delete-or-demote :which-key "delete/demote window")
  "wD" '(delete-other-windows :which-key "delete other windows")
  "ws" '(split-window-below :which-key "split below")
  "wv" '(split-window-right :which-key "split right")
  "wh" '(evil-window-left :which-key "window left")
  "wj" '(evil-window-down :which-key "window down")
  "wk" '(evil-window-up :which-key "window up")
  "wl" '(evil-window-right :which-key "window right")
  "w=" '(edmacs-stack-balance-center :which-key "rebalance layout")
  "w-" '(edmacs-window-demote :which-key "demote window")
  "w]" '(edmacs-stack-next :which-key "next stack pane")
  "w[" '(edmacs-stack-prev :which-key "previous stack pane")
  "wx" '(edmacs-stack-close :which-key "close pane")
  "w>" '(edmacs-stack-widen :which-key "widen stack")
  "w<" '(edmacs-stack-narrow :which-key "narrow stack")
  "wS" '(edmacs-stack-toggle :which-key "toggle side windows")
  "wr" '(edmacs-windows-repair-frame :which-key "repair frame layout")

  ;; Window rotation (tmux layout replacement)
  "wt" '(:ignore t :which-key "rotate")
  "wtt" '(window-layout-transpose :which-key "transpose layout")
  "wtr" '(rotate-layout :which-key "cycle layout")
  "wtv" '(rotate-main-vertical :which-key "main vertical")
  "wth" '(rotate-main-horizontal :which-key "main horizontal")
  "wtj" '(rotate-window :which-key "swap window")
  "wte" '(edmacs-window-promote :which-key "promote window")

  ;; Master-and-stack moves
  "w RET" '(edmacs-window-promote :which-key "promote to main")
  "wm" '(edmacs-window-pop-buffer-to-main :which-key "pop buffer to main")

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
  "tf" '(toggle-font-size :which-key "toggle font size")

  ;; Quit/Session
  ;; Under the launchd daemon (KeepAlive is unconditional) an Emacs that
  ;; exits is relaunched, so "quit" and "restart" cannot both be an exit --
  ;; see the three commands in modules/sessions.el.
  "q" '(:ignore t :which-key "quit")
  "qq" '(edmacs-quit :which-key "close frame")
  "qr" '(edmacs-restart-daemon :which-key "restart daemon")
  "qQ" '(edmacs-stop-daemon :which-key "stop daemon service"))

;; ============================================================================
;; C-w window prefix -- every state, tmux vocabulary
;; ============================================================================
;; `evil-window-map' is already C-w's prefix in normal/motion state; this
;; section extends it with the tmux pane vocabulary and makes the SAME map
;; reachable from insert, emacs and visual state so window moves never
;; require leaving insert -- the point of the whole section.
;;
;; The reachability mechanism is `evil-define-minor-mode-key' on a global
;; marker mode, NOT `general-define-key :keymaps 'override'.
;; `evil-state-keymaps' (evil-core.el) concatenates keymap buckets in a
;; fixed structural order: intercept, local, MINOR-MODE-MAPS, AUXILIARY-MAPS,
;; overriding maps, then the state map. evil-ghostel binds insert-state C-w
;; to its own terminal word-erase passthrough via `evil-define-key*' on
;; `evil-ghostel-mode-map' -- an AUXILIARY-MAPS entry -- and general's
;; `override' keymap lands in the overriding bucket, BEHIND it. A
;; minor-mode-maps entry is the only one of the three that wins inside a
;; claude-term pane. `ghostel-keymap-exceptions' (modules/claude-term.el)
;; covers the same key for ghostel's non-evil char mode.
;;
;; Deliberately shadowed, following tmux rather than vim: `C-w -' (was
;; decrease-height) and `C-w H/J/K/L' (were move-window-far-*) become split
;; and resize, and `C-w x' (was exchange) closes the pane. Resizing lives on
;; H/J/K/L, so nothing is lost. Insert-state `C-w' (delete-word-backward)
;; and a claude-term pane's own C-w word-erase are the accepted cost of a
;; prefix that works without leaving insert state.

(defvar evil-window-map)
(declare-function evil-define-minor-mode-key "evil-core")
(declare-function evil-window-decrease-width "evil-commands")
(declare-function evil-window-increase-width "evil-commands")
(declare-function evil-window-decrease-height "evil-commands")
(declare-function evil-window-increase-height "evil-commands")

(define-minor-mode edmacs-window-prefix-mode
  "Global marker mode carrying the `C-w' window prefix in every evil state.
Carries no keymap of its own; `evil-define-minor-mode-key' below
associates `evil-window-map' with this mode's symbol directly. Not meant
to be toggled by hand."
  :global t
  :lighter nil
  :group 'windows)

(with-eval-after-load 'evil
  ;; tmux's pane vocabulary, layered onto evil's own window map so both
  ;; `C-w' and `SPC w' reach the same commands.
  (define-key evil-window-map (kbd "|") #'split-window-right)
  (define-key evil-window-map (kbd "-") #'split-window-below)
  (define-key evil-window-map (kbd "H") #'evil-window-decrease-width)
  (define-key evil-window-map (kbd "L") #'evil-window-increase-width)
  (define-key evil-window-map (kbd "J") #'evil-window-increase-height)
  (define-key evil-window-map (kbd "K") #'evil-window-decrease-height)
  (define-key evil-window-map (kbd "RET") #'edmacs-window-promote)
  (define-key evil-window-map (kbd "=") #'edmacs-stack-balance-center)
  (define-key evil-window-map (kbd "x") #'edmacs-stack-close)
  (define-key evil-window-map (kbd "d") #'edmacs-window-delete-or-demote)
  (define-key evil-window-map (kbd "m") #'edmacs-window-pop-buffer-to-main)
  (define-key evil-window-map (kbd "[") #'edmacs-stack-prev)
  (define-key evil-window-map (kbd "]") #'edmacs-stack-next)
  (define-key evil-window-map (kbd "<") #'edmacs-stack-narrow)
  (define-key evil-window-map (kbd ">") #'edmacs-stack-widen)
  (define-key evil-window-map (kbd "S") #'edmacs-stack-toggle)

  (dolist (state '(normal visual insert emacs motion replace operator))
    (evil-define-minor-mode-key state 'edmacs-window-prefix-mode
      (kbd "C-w") evil-window-map))

  (edmacs-window-prefix-mode 1))

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
