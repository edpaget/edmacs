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

;; Font size presets
(defvar font-size-standard 18
  "Standard font size in points.")

(defvar font-size-large 28
  "Large font size in points.")

(defvar font-size-current font-size-standard
  "Current font size in use.")

(defun set-iosevka-font (size)
  "Set Iosevka font at SIZE points."
  (when (find-font (font-spec :name "Iosevka"))
    (set-face-attribute 'default nil
                        :font "Iosevka"
                        :height (* size 10))
    (set-face-attribute 'fixed-pitch nil
                        :font "Iosevka"
                        :height (* size 10))
    (set-face-attribute 'variable-pitch nil
                        :font "Iosevka"
                        :height (* size 10))
    (setq font-size-current size)))

(defun toggle-font-size ()
  "Toggle between standard and large font sizes."
  (interactive)
  (if (= font-size-current font-size-standard)
      (progn
        (set-iosevka-font font-size-large)
        (message "Font size: %dpt (large)" font-size-large))
    (progn
      (set-iosevka-font font-size-standard)
      (message "Font size: %dpt (standard)" font-size-standard))))

;; Set initial font
(set-iosevka-font font-size-standard)

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

;; ============================================================================
;; Tiling Window Manager
;; ============================================================================

(use-package tiles
  :straight nil
  :load-path "modules/tiles"
  :commands (tiles-mode tiles-setup tiles-refresh)
  :custom
  (tiles-default-layout 'horizontal)
  (tiles-master-window-ratio 0.5)
  (tiles-auto-balance t)
  :config
  (message "Tiles module loaded"))

;;; ui.el ends here
