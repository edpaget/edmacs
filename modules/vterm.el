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

  ;; VTerm keybindings
  (general-define-key
   :states 'normal
   :prefix "SPC v"
   "" '(:ignore t :which-key "vterm")
   "v" '(vterm :which-key "open vterm")
   "o" '(vterm-other-window :which-key "vterm other window"))

  ;; Allow Emacs-style keybindings in vterm
  (define-key vterm-mode-map (kbd "C-c C-t") #'vterm-copy-mode))

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
