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

;; Set initial font
(set-iosevka-font font-size-standard)

;; ============================================================================
;; Theme - Modus Themes
;; ============================================================================

;; Adopted in place of catppuccin-theme so that fixed-pitch inheritance is
;; correct by construction rather than patched face-by-face.  catppuccin
;; redefines several markdown-mode (and org-mode) faces with only
;; :foreground, overriding markdown-mode's own upstream default of
;; `:inherit fixed-pitch' on its code/table faces -- under
;; `variable-pitch-mode' that meant code blocks, inline code, and tables
;; inherited proportional metrics instead of staying monospaced, and only
;; markdown was patchable without an exhaustive per-theme face audit.
;; modus-themes (bundled with Emacs since 28, so no straight package or
;; lockfile entry needed) applies a single `modus-themes-fixed-pitch'
;; face to 50+ faces -- the whole markdown, markdown-treesitter, and org
;; set (org-block/org-code/org-table included, ready for whenever
;; org-config is re-enabled) -- so nothing needs a manual `user'-theme
;; override here.  ef-themes covers the same ground and was the other
;; candidate named by this phase; modus-themes was picked because it
;; ships in Emacs core, so this swap adds zero new dependencies.  There
;; is no solarized theme in either family (bbatsov/solarized-emacs is
;; itself only partially correct on this front), so solarized was not
;; considered further.  task/theme-fixed-pitch-coverage-audit, filed
;; while catppuccin was still in place to track auditing it against this
;; exact face list, is now moot and has been closed accordingly.
;;
;; `modus-themes-mixed-fonts' is what actually wires
;; `modus-themes-fixed-pitch' to `:inherit fixed-pitch' -- it defaults to
;; nil, in which case markdown/org/table faces that inherit
;; `modus-themes-fixed-pitch' get NO font-family override at all and
;; would render proportionally under `variable-pitch-mode' despite the
;; face list above looking exhaustive.  Must be set before `load-theme'
;; runs, since it is consulted while the theme's face list is built.
(setq modus-themes-mixed-fonts t)

;; modus-vivendi is the dark variant, closest in spirit to the catppuccin
;; mocha flavor this replaces.  Swap to `modus-operandi' (or one of the
;; other bundled modus/ef variants) for a light theme.
(load-theme 'modus-vivendi :no-confirm)

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
;; parameters keyed by both its buffer and the window object itself
;; before the rotate, then transfer them to whichever window ends up
;; showing that buffer afterward -- and strip them from any window that
;; kept the parameter but lost the buffer it belonged to. A plain
;; `set-window-buffer' swap (exactly what `rotate-window' does) never
;; touches a window's own parameter slot, so restoring only additively
;; would leave the departing window's old parameters in place to bleed
;; onto whatever buffer now occupies it -- e.g. swapping an Embark
;; Collect window (styled with `(mode-line-format . none)') with a plain
;; buffer window would correctly give the Embark buffer's new window a
;; hidden mode-line, but would also leave the plain buffer's new window
;; with a hidden mode-line it never asked for, inherited from the window
;; object it now occupies. Also wraps `window-layout-transpose' as a
;; cheap safety net, since it too rebuilds part of the window subtree
;; rather than merely reshaping existing windows. These `advice-add'
;; calls are deliberately top-level (not inside `use-package rotate''s
;; `:config') so `window-layout-transpose' -- native to Emacs, not part
;; of the `rotate' package -- is protected even when invoked before
;; `rotate' has been autoloaded; advice on the still-autoloaded
;; `rotate-*' symbols is honored the same way.
;;
;; Not every key `window-parameters' returns is buffer-owned styling,
;; though. `quit-restore' (what `quit-window' / `q' consults to decide
;; what to do with a window afterward), `window-side' and `window-slot'
;; (side-window placement), `no-other-window', and `clone-of' all
;; describe the window SLOT itself -- bookkeeping Emacs attaches to a
;; particular window, not something that belongs to whatever buffer is
;; currently showing in it. Migrating those along with a buffer during a
;; swap would corrupt each window's own history: verified via direct
;; reproduction, two windows each carrying a distinct `quit-restore'
;; value, after an advised `rotate-window' swap, each ends up carrying
;; the OTHER window's `quit-restore' instead of its own, so `q' would
;; restore the wrong thing or delete the wrong window. `quit-restore-prev'
;; is set by that same window.el code path for that same purpose -- it
;; picks between the two keys depending on whether `quit-restore' is
;; already set -- so it is window identity on identical grounds and
;; belongs here beside it. Exclude these window-identity keys from the
;; buffer-following transfer entirely.
(defconst edmacs--rotate-window-identity-parameters
  '(quit-restore quit-restore-prev window-side window-slot no-other-window clone-of)
  "Window parameters describing the window slot, not buffer styling.
Left untouched by `edmacs--rotate-preserve-window-parameters' rather
than migrated along with whatever buffer happens to occupy the window
when a rotate/transpose runs.")

(defun edmacs--rotate-capture-window-parameters ()
  "Snapshot window-parameters for the selected frame's windows.
Returns a cons (BY-BUFFER . BY-WINDOW): BY-BUFFER maps each window's
buffer to its parameter alist, used to reapply parameters onto
whichever window ends up showing that buffer; BY-WINDOW maps each
window object itself to the same alist, used to detect and strip a
parameter a window no longer needs once it shows a different buffer.
Keys in `edmacs--rotate-window-identity-parameters' are omitted from
both, since they describe the window slot rather than the buffer."
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
CAPTURED is the (BY-BUFFER . BY-WINDOW) cons produced by
`edmacs--rotate-capture-window-parameters', which already excludes
`edmacs--rotate-window-identity-parameters'.

For every window still live after the rotate, first strip any
parameter key the window carried before whose value no longer matches
what the window's *current* buffer is entitled to -- this is the half
a purely additive restore misses, since `set-window-buffer' never
clears a window's own parameters when the buffer it shows changes.
Then fill in any parameter key the current buffer had captured but the
window doesn't yet carry. Both steps check per key, not per window,
since Emacs itself stamps an unrelated `quit-restore' parameter onto
nearly every window, and an all-or-nothing per-window check would see
that and skip restoration -- `quit-restore' itself never reaches this
function at all, since capture already excluded it."
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
