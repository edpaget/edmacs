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
;; Window Rotation (tmux layout replacement)
;; ============================================================================

;; Native Emacs 31 `window.el': let `C-x w t' / `SPC w t t' transpose a
;; layout even when a dedicated window is part of it. Nothing in this
;; config currently marks a window dedicated -- vterm-toggle's own notion
;; of "dedicated" (`vterm-toggle--dedicated-p') is an unrelated
;; buffer-selection flag, not core window dedication -- so this has no
;; effect today, but it heads off a `user-error' the moment a real
;; dedicated window (a sidebar, or a future `(dedicated . t)'
;; display-buffer-alist action) becomes part of the layout.
(setq transpose-dedicated-windows t)

(use-package rotate
  :commands (rotate-layout rotate-window rotate-main-vertical rotate-main-horizontal)
  ;; `:init' (plain `setq'), not `:custom': `rotate' is autoloaded via
  ;; `:commands' above, so `rotate-skip-dedicated-windows' has no
  ;; `defcustom' yet at this point in the load sequence. `:custom' records
  ;; the value on a `use-package' custom theme that only takes effect once
  ;; `rotate.el' itself defines the variable, which would leave it unbound
  ;; from boot until the first rotate command runs. Setting it here with
  ;; plain `setq' binds it immediately; `defcustom' honors an
  ;; already-bound value when `rotate.el' later loads, so the intent
  ;; (matches the upstream default; explicit here so a future default
  ;; change doesn't silently start sweeping dedicated windows, e.g.
  ;; treemacs/dired-sidebar, into a rotate) is preserved either way.
  :init
  (setq rotate-skip-dedicated-windows t))

;; rotate.el reassigns buffers to windows via plain `set-window-buffer'
;; (`rotate-window') or by deleting windows outright and rebuilding via
;; `split-window' + `set-window-buffer' (`rotate-main-vertical' /
;; `rotate-main-horizontal' / `rotate-layout', through
;; `rotate--refresh-window') -- never `window-swap-states' -- so a
;; window-parameter attached to a window (e.g. Embark's
;; `(mode-line-format . none)' from modules/completion.el's
;; `display-buffer-alist' entry) does not travel with the buffer it was
;; styling: it is left behind on the now-repurposed window, or deleted
;; along with the window that carried it. Snapshot each window's
;; parameters keyed by its buffer before the rotate and reapply them to
;; whichever window ends up showing that buffer afterward. Also wraps
;; `window-layout-transpose' as a cheap safety net, since it too rebuilds
;; part of the window subtree rather than merely reshaping existing
;; windows. These `advice-add' calls are deliberately top-level (not
;; inside `use-package rotate''s `:config') so `window-layout-transpose'
;; -- native to Emacs, not part of the `rotate' package -- is protected
;; even when invoked before `rotate' has been autoloaded; advice on the
;; still-autoloaded `rotate-*' symbols is honored the same way.
(defun edmacs--rotate-capture-window-parameters ()
  "Return an alist of (BUFFER . PARAMETERS) for the selected frame's windows."
  (let (captured)
    (dolist (w (window-list nil nil (minibuffer-window)))
      (let ((params (window-parameters w)))
        (when params
          (push (cons (window-buffer w) params) captured))))
    captured))

(defun edmacs--rotate-restore-window-parameters (captured)
  "Reapply CAPTURED window-parameters to windows now showing that buffer.
Fills in only the specific parameter keys a window doesn't already carry
-- checking per key, not per window, since Emacs itself stamps an
unrelated `quit-restore' parameter onto nearly every window, and an
all-or-nothing per-window check would see that and skip restoration."
  (dolist (w (window-list nil nil (minibuffer-window)))
    (dolist (param (alist-get (window-buffer w) captured))
      (unless (window-parameter w (car param))
        (set-window-parameter w (car param) (cdr param))))))

(defun edmacs--rotate-preserve-window-parameters (orig-fn &rest args)
  "Preserve per-buffer window-parameters across a rotate/transpose ORIG-FN."
  (let ((captured (edmacs--rotate-capture-window-parameters)))
    (prog1 (apply orig-fn args)
      (edmacs--rotate-restore-window-parameters captured))))

(dolist (fn '(rotate-window rotate-main-vertical rotate-main-horizontal
              rotate-layout window-layout-transpose))
  (advice-add fn :around #'edmacs--rotate-preserve-window-parameters))

;;; ui.el ends here
