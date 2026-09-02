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

;; Master-and-stack moves on top of rotate.el's main layouts. The main window
;; is the top-left non-side window, which is where `rotate-main-vertical' and
;; `rotate-main-horizontal' both put it.
(defun edmacs--main-window ()
  "Return the frame's main window: the top-left window that is not a side window."
  (seq-find (lambda (w) (not (window-parameter w 'window-side)))
            (window-list nil 'no-minibuf (frame-first-window))))

(defun edmacs--swap-window-buffers (w1 w2)
  "Exchange the buffers shown in W1 and W2.
Uses `window-swap-states' for ordinary windows; a side window keeps its
side and slot and only trades buffers. A non-claude buffer moved into a
side window clears `no-other-window' so window navigation still reaches it."
  (if (or (window-parameter w1 'window-side) (window-parameter w2 'window-side))
      (let ((b1 (window-buffer w1)) (b2 (window-buffer w2)))
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (dolist (w (list w1 w2))
          (when (and (window-parameter w 'window-side)
                     (not (string-prefix-p "*claude-term" (buffer-name (window-buffer w)))))
            (set-window-parameter w 'no-other-window nil))))
    (window-swap-states w1 w2)))

(defun edmacs-window-promote (&optional window)
  "Swap WINDOW's buffer into the main window and select the main window.
Like dwm's zoom or tmux's promote. Side windows (the right-hand column
claude-term and *Warnings* use) count as stack windows: promoting from
one puts its buffer in main and the old main buffer in that pane. From
the main window itself, swap with the first stack window."
  (interactive)
  (let* ((window (or window (selected-window)))
         (main (edmacs--main-window))
         (other (if (eq window main)
                    (seq-find (lambda (w) (not (eq w main)))
                              (window-list nil 'no-minibuf main))
                  window)))
    (when other
      (edmacs--swap-window-buffers main other))
    (select-window main)))

(defun edmacs-window-pop-buffer-to-main (buffer)
  "Show BUFFER in the main window and select it.
If BUFFER is already visible in a stack window, swap it into main; if
it is in a side window, close that side window."
  (interactive (list (read-buffer "Pop to main: " (other-buffer) t)))
  (let ((window (get-buffer-window buffer)))
    (if (and window (not (eq window (edmacs--main-window))))
        (edmacs-window-promote window)
      (let ((main (edmacs--main-window)))
        (set-window-buffer main buffer)
        (select-window main)))))

;; Nothing marks a window dedicated today, but this heads off a `user-error'
;; from `window-layout-transpose' the moment something does.
(setq transpose-dedicated-windows t)

;; claude-term panes set `no-other-window' to stay out of `other-window'
;; cycling; directional moves (SPC w h/j/k/l) should still reach them.
(setq windmove-allow-all-windows t)

;; Stack *Warnings* in the right-hand column beside claude-term panes (slot -1
;; puts it above them). Width matches `claude-term-window-width'.
(add-to-list 'display-buffer-alist
             '("\\`\\*Warnings\\*\\'"
               (display-buffer-in-side-window)
               (side . right)
               (slot . -1)
               (window-width . 0.4)
               (preserve-size . (t . nil))))

(use-package rotate
  :commands (rotate-layout rotate-window rotate-main-vertical rotate-main-horizontal)
  ;; `:init' + `setq', not `:custom': rotate is autoloaded, so `:custom' would
  ;; leave the variable unbound until the first rotate command runs. Matches
  ;; the upstream default; explicit so a default change can't start rotating
  ;; dedicated windows (sidebars) silently.
  :init
  (setq rotate-skip-dedicated-windows t))

;; rotate.el moves buffers between windows with `set-window-buffer' or by
;; rebuilding windows, never `window-swap-states', so window parameters that
;; style a buffer (e.g. Embark's `(mode-line-format . none)' from
;; completion.el) stay behind on the old window. Snapshot them before the
;; rotate and move them to whichever window shows the buffer afterward,
;; stripping stale ones from windows that changed buffer. Advised at top
;; level so `window-layout-transpose', which is native, is covered even
;; before rotate autoloads.
;;
;; Parameters that describe the window slot itself (`quit-restore' and
;; friends) must not travel with the buffer: swapping them makes `q' restore
;; the wrong thing.
(defconst edmacs--rotate-window-identity-parameters
  '(quit-restore quit-restore-prev window-side window-slot no-other-window clone-of)
  "Window parameters describing the window slot, not buffer styling.
Left untouched by `edmacs--rotate-preserve-window-parameters' rather
than migrated along with whatever buffer happens to occupy the window
when a rotate/transpose runs.")

(defun edmacs--rotate-capture-window-parameters ()
  "Snapshot window-parameters for the selected frame's windows.
Returns (BY-BUFFER . BY-WINDOW): the same parameter alists keyed by each
window's buffer (to reapply) and by the window itself (to strip). Keys in
`edmacs--rotate-window-identity-parameters' are omitted."
  (let (by-buffer by-window)
    (dolist (w (window-list nil nil (minibuffer-window)))
      (let (params)
        (dolist (param (window-parameters w))
          (unless (memq (car param) edmacs--rotate-window-identity-parameters)
            (push param params)))
        (when params
          (push (cons (window-buffer w) params) by-buffer)
          (push (cons w params) by-window))))
    (cons by-buffer by-window)))

(defun edmacs--rotate-restore-window-parameters (captured)
  "Transfer CAPTURED window-parameters with the buffer they styled.
CAPTURED is from `edmacs--rotate-capture-window-parameters'. For each
live window, strip parameters it carried whose value its current buffer
is not entitled to, then fill in any the buffer had captured. Both steps
work per key, since `set-window-buffer' never clears a window's own
parameters."
  (let ((by-buffer (car captured))
        (by-window (cdr captured)))
    (dolist (w (window-list nil nil (minibuffer-window)))
      (let ((current-params (alist-get (window-buffer w) by-buffer)))
        (dolist (param (alist-get w by-window))
          (unless (equal (cdr param) (alist-get (car param) current-params))
            (set-window-parameter w (car param) nil)))
        (dolist (param current-params)
          (unless (window-parameter w (car param))
            (set-window-parameter w (car param) (cdr param))))))))

(defun edmacs--rotate-preserve-window-parameters (orig-fn &rest args)
  "Preserve per-buffer window-parameters across a rotate/transpose ORIG-FN."
  (let ((captured (edmacs--rotate-capture-window-parameters)))
    (prog1 (apply orig-fn args)
      (edmacs--rotate-restore-window-parameters captured))))

(dolist (fn '(rotate-window rotate-main-vertical rotate-main-horizontal
              rotate-layout window-layout-transpose))
  (advice-add fn :around #'edmacs--rotate-preserve-window-parameters))

;;; ui.el ends here
