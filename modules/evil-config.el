;;; evil-config.el --- Evil mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Vim emulation using Evil mode with additional packages for enhanced functionality.

;;; Code:

;; ============================================================================
;; Undo System - Required by Evil
;; ============================================================================
;; Native `undo-redo' replaces undo-tree, whose per-save history files grew
;; unbounded. Undo history is in-memory only.

;; ============================================================================
;; Evil Mode
;; ============================================================================

(use-package evil
  :init
  ;; Required settings before evil loads
  (setq evil-want-integration t
        evil-want-keybinding nil  ; Required by evil-collection
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump nil
        evil-undo-system 'undo-redo
        evil-respect-visual-line-mode t
        evil-search-module 'evil-search
        evil-split-window-below t
        evil-vsplit-window-right t)

  :config
  (evil-mode 1)

  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (setq evil-want-minibuffer nil))

;; ============================================================================
;; Evil Collection - Evil bindings for many modes
;; ============================================================================

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ============================================================================
;; Evil Surround - Surround text objects
;; ============================================================================

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; ============================================================================
;; Evil Commentary - Comment/uncomment with gc
;; ============================================================================

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

;; ============================================================================
;; Evil Numbers - Increment/decrement numbers
;; ============================================================================
;; `C-x' in normal state is `evil-numbers/dec-at-pt', mirroring vim. Modules
;; needing a `C-x <chord>' register it via `edmacs-evil-config-add-c-x-chord'
;; below, so this file stays the sole owner of that keymap entry regardless
;; of load order.

(defvar edmacs-evil-config--c-x-chords nil
  "Alist of (KEY-STRING . COMMAND) reachable after evil's normal-state `C-x'.
Populated only via `edmacs-evil-config-add-c-x-chord'; consulted every
time the `C-x' binding is rebuilt, so registration order never
matters. KEY-STRING is a `general-key-dispatch'-style continuation
typed after `C-x', e.g. \"t p\" or \"v w s\".")

(defun edmacs-evil-config--rebuild-c-x-binding ()
  "(Re)install normal-state `C-x' from `edmacs-evil-config--c-x-chords'.
With no chords it is the plain `evil-numbers/dec-at-pt' leaf. Otherwise a
`general-key-dispatch' waits briefly for a registered chord and falls
back to decrement, replaying any unmatched keys. Built with `eval'
because `general-key-dispatch' is a macro and the chord list is only
known once later modules register; this also avoids needing `general'
loaded when this file loads."
  (define-key evil-normal-state-map (kbd "C-x")
    (if edmacs-evil-config--c-x-chords
        (eval
         `(general-key-dispatch 'evil-numbers/dec-at-pt
            :timeout 0.4
            :name edmacs-evil-config--c-x-dispatch
            :docstring ,(format "Decrement number at point (vim's `C-x'), \
or run a registered command when `C-x' is followed by one of: %s."
                                 (mapconcat #'car edmacs-evil-config--c-x-chords ", "))
            ,@(mapcan (lambda (entry) (list (car entry) `(quote ,(cdr entry))))
                      edmacs-evil-config--c-x-chords))
         t)
      'evil-numbers/dec-at-pt)))

(defun edmacs-evil-config-add-c-x-chord (key command)
  "Register COMMAND to run when normal-state `C-x' is followed by KEY.
KEY is a string typed after `C-x' (e.g. \"t p\"). Safe to call in any
load order: the entry is recorded now and the keymap rebuilt once
`evil-numbers' has installed its own binding."
  (setf (alist-get key edmacs-evil-config--c-x-chords nil nil #'equal) command)
  (with-eval-after-load 'evil-numbers
    (edmacs-evil-config--rebuild-c-x-binding)))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (edmacs-evil-config--rebuild-c-x-binding))

;; ============================================================================
;; Evil Matchit - Match tags with %
;; ============================================================================

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; ============================================================================
;; Evil Visualstar - Search selected text with *
;; ============================================================================

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode 1))

;; ============================================================================
;; Evil Indent Textobject - Text objects for indentation
;; ============================================================================

(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

;; ============================================================================
;; Additional Evil Keybindings
;; ============================================================================

(with-eval-after-load 'evil
  ;; Use visual line motions even outside visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;;; evil-config.el ends here
