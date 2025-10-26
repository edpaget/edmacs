;;; evil-config.el --- Evil mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Vim emulation using Evil mode with additional packages for enhanced functionality.

;;; Code:

;; ============================================================================
;; Undo System - Required by Evil
;; ============================================================================

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))

  ;; Create undo-tree history directory if it doesn't exist
  (let ((undo-dir (expand-file-name "undo-tree-history" user-emacs-directory)))
    (unless (file-exists-p undo-dir)
      (make-directory undo-dir t)))

  (global-undo-tree-mode 1))

;; ============================================================================
;; Evil Mode
;; ============================================================================

(use-package evil
  :init
  ;; Required settings before evil loads
  (setq evil-want-integration t
        evil-want-keybinding nil  ; Required by evil-collection
        evil-want-C-u-scroll t    ; C-u scrolls up
        evil-want-C-d-scroll t    ; C-d scrolls down
        evil-want-C-i-jump nil    ; Don't use C-i for jump forward
        evil-undo-system 'undo-tree
        evil-respect-visual-line-mode t
        evil-search-module 'evil-search
        evil-split-window-below t
        evil-vsplit-window-right t)

  :config
  ;; Enable evil mode globally
  (evil-mode 1)

  ;; Use Emacs state in certain modes
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Set initial states for some modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  ;; Disable evil in minibuffer
  (setq evil-want-minibuffer nil))

;; ============================================================================
;; Evil Collection - Evil bindings for many modes
;; ============================================================================

(use-package evil-collection
  :after evil
  :config
  ;; Don't set evil-collection-mode-list - let it init all supported modes
  ;; Note: org-roam is not in evil-collection, so it was causing errors
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

(use-package evil-numbers
  :after evil
  :config
  ;; Bind in normal state
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

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
