;;; ui.el --- UI and appearance configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Theme, modeline, fonts, and visual enhancements.

;;; Code:

;; ============================================================================
;; Font Configuration
;; ============================================================================

(defun set-font-if-available (font-name size)
  "Set FONT-NAME at SIZE if it's available on the system."
  (when (find-font (font-spec :name font-name))
    (set-face-attribute 'default nil
                        :font font-name
                        :height (* size 10))
    (set-face-attribute 'fixed-pitch nil
                        :font font-name
                        :height (* size 10))
    (set-face-attribute 'variable-pitch nil
                        :font font-name
                        :height (* size 10))))

;; Try common programming fonts in order of preference
(cond
 ((find-font (font-spec :name "JetBrains Mono"))
  (set-font-if-available "JetBrains Mono" 12))
 ((find-font (font-spec :name "Fira Code"))
  (set-font-if-available "Fira Code" 12))
 ((find-font (font-spec :name "Consolas"))
  (set-font-if-available "Consolas" 12))
 ((find-font (font-spec :name "Menlo"))
  (set-font-if-available "Menlo" 12))
 (t
  (set-font-if-available "Monaco" 12)))

;; ============================================================================
;; Theme - Catppuccin
;; ============================================================================

(use-package catppuccin-theme
  :config
  ;; Catppuccin flavor: latte, frappe, macchiato, or mocha
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

;; ============================================================================
;; Nano Modeline
;; ============================================================================

(use-package nano-modeline
  :config
  ;; Configure nano-modeline style
  (setq nano-modeline-position #'nano-modeline-footer)

  ;; Set text mode as the default modeline
  (nano-modeline-text-mode t)

  ;; Add mode-specific modelines
  (add-hook 'prog-mode-hook #'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook #'nano-modeline-text-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'term-mode-hook #'nano-modeline-term-mode))

;; ============================================================================
;; Icons - Nerd Icons
;; ============================================================================

(use-package nerd-icons
  :config
  ;; The first time you load your configuration on a new machine, you need to
  ;; run M-x nerd-icons-install-fonts to install the icon fonts.
  )

;; Nerd icons for dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Nerd icons for completion frameworks
(use-package nerd-icons-completion
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; ============================================================================
;; Rainbow Delimiters - Colorful parentheses
;; ============================================================================

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================================
;; Highlight TODO/FIXME/NOTE
;; ============================================================================

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FFC0CB")
          ("FIXME"  . "#FF6347")
          ("NOTE"   . "#87CEEB")
          ("HACK"   . "#FFD700")
          ("DEPRECATED" . "#A9A9A9"))))

;; ============================================================================
;; Visual Line Mode for Text
;; ============================================================================

(add-hook 'text-mode-hook 'visual-line-mode)

;; ============================================================================
;; Window Divider
;; ============================================================================

(setq window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode 1)

;; ============================================================================
;; Smooth Scrolling
;; ============================================================================

(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

;; ============================================================================
;; Transparency (optional - commented out by default)
;; ============================================================================

;; (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;;; ui.el ends here
