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
  "Set Iosevka font at SIZE points.
`default' and `fixed-pitch' use monospace Iosevka.  `variable-pitch'
uses Iosevka Etoile (Iosevka's proportional slab sibling) so that
`variable-pitch-mode' is not a no-op; if Etoile is not installed on
this machine, `variable-pitch' is left alone rather than failing the
whole function."
  (when (find-font (font-spec :name "Iosevka"))
    (set-face-attribute 'default nil
                        :font "Iosevka"
                        :height (* size 10))
    (set-face-attribute 'fixed-pitch nil
                        :font "Iosevka"
                        :height (* size 10))
    (when (find-font (font-spec :name "Iosevka Etoile"))
      (set-face-attribute 'variable-pitch nil
                          :font "Iosevka Etoile"
                          :height (* size 10)))
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

;; A daemon has no display at load time, so `find-font' fails there; apply
;; the font once the first graphical frame exists instead.
(defun edmacs--set-font-on-first-frame (frame)
  "Apply the Iosevka font when FRAME is the first graphical frame."
  (when (display-graphic-p frame)
    (with-selected-frame frame (set-iosevka-font font-size-current))
    (remove-hook 'after-make-frame-functions #'edmacs--set-font-on-first-frame)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'edmacs--set-font-on-first-frame)
  (set-iosevka-font font-size-standard))

;; ============================================================================
;; Theme - Solarized Dark
;; ============================================================================

;; The palette Ghostty is configured with (`theme = iTerm2 Solarized Dark'),
;; so an Emacs-hosted Claude session reads like a terminal one.  Unlike
;; modus-themes, solarized also themes the `ansi-color-*' faces that ghostel
;; derives its terminal palette from -- claude-term.el pins the handful of
;; slots solarized maps differently.
;;
;; The `:init' settings must land before `load-theme' reads them; scaling and
;; variable-pitch headings are off to keep the previous modus-vivendi
;; proportions.
(use-package solarized-theme
  :straight t
  :init
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil
        solarized-scale-outline-headlines nil)
  :config
  ;; `solarized-light' is the light one.
  (load-theme 'solarized-dark :no-confirm))

;; solarized resolves its palette against the display it is loaded on; a
;; daemon loads it on a tty and GUI frames then come up black-on-white.
;; Reload it on the first GUI frame, at a depth that runs before
;; nano-modeline's face refresh reads the theme.
(defun edmacs--reload-theme-on-first-frame (frame)
  "Reload the theme when FRAME is the first graphical frame."
  (when (display-graphic-p frame)
    (remove-hook 'after-make-frame-functions #'edmacs--reload-theme-on-first-frame)
    (with-selected-frame frame
      (load-theme 'solarized-dark :no-confirm))))

(when (daemonp)
  (add-hook 'after-make-frame-functions #'edmacs--reload-theme-on-first-frame -10))

;; ============================================================================
;; Nano Modeline
;; ============================================================================

(use-package nano-modeline
  :config
  (setq nano-modeline-position #'nano-modeline-footer)

  (nano-modeline-text-mode t)

  (add-hook 'prog-mode-hook #'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook #'nano-modeline-text-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'term-mode-hook #'nano-modeline-term-mode))

;; nano-modeline bakes theme colors into its faces at load time, and a daemon
;; has no display then, so they come out as tty fallbacks. Re-declare them
;; once the first GUI frame exists; `defface' only re-evaluates a face whose
;; `face-defface-spec' property has been cleared.
(defun edmacs--nano-modeline-refresh-faces (frame)
  "Re-derive nano-modeline's faces on FRAME, the first graphical frame."
  (when (display-graphic-p frame)
    (remove-hook 'after-make-frame-functions #'edmacs--nano-modeline-refresh-faces)
    (with-selected-frame frame
      (dolist (face (face-list))
        (when (string-prefix-p "nano-modeline" (symbol-name face))
          (put face 'face-defface-spec nil)))
      (load-library "nano-modeline"))))

(when (daemonp)
  (add-hook 'after-make-frame-functions #'edmacs--nano-modeline-refresh-faces))

;; ============================================================================
;; Icons - Nerd Icons
;; ============================================================================

;; Run M-x nerd-icons-install-fonts once on a new machine.
;;
;; Not deferred: `nerd-icons-completion' below gates on it loading, and with
;; no autoload trigger of its own it would never load in a session that
;; opens no dired buffer, silently losing completion icons. It is cheap.
(use-package nerd-icons)

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

(setq scroll-conservatively 10000
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

;; ghostel forwards wheel events it does not send to the terminal on to
;; whatever scroll package is configured, so this also governs how a
;; claude-term pane scrolls.
(pixel-scroll-precision-mode 1)

;; ============================================================================
;; Transparency (optional - commented out by default)
;; ============================================================================

;; (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; ============================================================================
;; Window Rotation (tmux layout replacement)
;; ============================================================================

;; Master-and-stack window management (main window, stack column, the
;; windmove/rotate advice) lives in modules/windows.el; only the rotate.el
;; package declaration itself stays here.
(use-package rotate
  :commands (rotate-layout rotate-window rotate-main-vertical rotate-main-horizontal)
  ;; `:init' + `setq', not `:custom': rotate is autoloaded, so `:custom' would
  ;; leave the variable unbound until the first rotate command runs. Matches
  ;; the upstream default; explicit so a default change can't start rotating
  ;; dedicated windows (sidebars) silently.
  :init
  (setq rotate-skip-dedicated-windows t))

;;; ui.el ends here
