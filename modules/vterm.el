;;; vterm.el --- VTerm terminal emulator configuration -*- lexical-binding: t -*-

;;; Commentary:
;; VTerm is a fully-featured terminal emulator for Emacs based on libvterm.

;;; Code:

;; ============================================================================
;; VTerm - Terminal Emulator
;; ============================================================================

(use-package vterm
  :commands vterm
  :config
  ;; VTerm settings
  (setq vterm-max-scrollback 10000
        vterm-buffer-name-string "vterm %s"
        vterm-kill-buffer-on-exit t)

  ;; Evil integration - start in insert state (like vim terminal)
  (evil-set-initial-state 'vterm-mode 'insert)

  ;; In vterm-copy-mode, use normal state for navigation
  (evil-set-initial-state 'vterm-copy-mode 'normal)

  ;; VTerm keybindings
  (general-define-key
   :states 'normal
   :prefix "SPC v"
   "" '(:ignore t :which-key "vterm")
   "v" '(vterm :which-key "open vterm")
   "o" '(vterm-other-window :which-key "vterm other window"))

  ;; Key bindings in insert state for vterm
  (evil-define-key 'insert vterm-mode-map
    (kbd "C-c C-t") #'vterm-copy-mode)  ; Enter copy mode

  ;; Key bindings in normal state for vterm-copy-mode
  (evil-define-key 'normal vterm-copy-mode-map
    (kbd "i") #'vterm-copy-mode         ; Exit copy mode back to insert
    (kbd "RET") #'vterm-copy-mode       ; Exit copy mode
    (kbd "q") #'vterm-copy-mode         ; Exit copy mode
    (kbd "p") #'vterm-yank              ; Paste from kill ring
    (kbd "P") #'vterm-yank-pop)         ; Paste previous from kill ring

  ;; Allow Emacs-style keybindings in vterm
  (define-key vterm-mode-map (kbd "C-c C-t") #'vterm-copy-mode)

  ;; Make C-u work in vterm
  (define-key vterm-mode-map (kbd "C-u") #'vterm-send-C-u)

  ;; Paste from kill ring with C-y
  (define-key vterm-mode-map (kbd "C-y") #'vterm-yank))

;; ============================================================================
;; VTerm Anti-Flicker Filter
;; ============================================================================

(use-package vterm-anti-flicker-filter
  :straight (vterm-anti-flicker-filter
             :type git
             :host github
             :repo "martinbaillie/vterm-anti-flicker-filter")
  :after vterm)

;; ============================================================================
;; VTerm Toggle - Toggle vterm buffer
;; ============================================================================

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3)))

  ;; VTerm toggle keybindings
  (general-define-key
   :states 'normal
   :prefix "SPC v"
   "t" '(vterm-toggle :which-key "toggle vterm")
   "n" '(vterm-toggle-forward :which-key "next vterm")
   "p" '(vterm-toggle-backward :which-key "previous vterm")))

;;; vterm.el ends here
