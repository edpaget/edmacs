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

(defconst edmacs-mono-family "Iosevka Term"
  "Monospace family for `default' and `fixed-pitch'.
Iosevka's `Term' cut, not the plain family: plain Iosevka draws the
markers Claude's CLI emits (⏺ ✻ ✓ ✔ ✗ → … ▪ ★) two cells wide, and
ghostel rescales any glyph overflowing its cell, so claude-term renders
them at half size.  The Term cut sizes every symbol to one cell, so
ghostel leaves them alone.")

;; Font size presets.  Both are divisible by 4, which `edmacs-emoji-pin-size'
;; requires to land emoji on the cell grid.
(defvar font-size-standard 16
  "Standard font size in points.")

(defvar font-size-large 20
  "Large font size in points.")

(defvar font-size-current font-size-standard
  "Current font size in use.")

(defun edmacs-emoji-pin-size (size)
  "Emoji font size that fills exactly two cells at text SIZE.
Apple Color Emoji advances at 4/3 of its requested size, and an Iosevka
Term cell is half the text size, so two cells are covered exactly when
the emoji is pinned to three quarters of SIZE.  Whole-numbered only when
SIZE is divisible by 4; other sizes land a pixel or two off."
  (/ (* size 3) 4))

(defun set-iosevka-font (size)
  "Set `edmacs-mono-family' at SIZE points.
`default' and `fixed-pitch' use it directly.  `variable-pitch' uses
Iosevka Etoile (Iosevka's proportional slab sibling) so that
`variable-pitch-mode' is not a no-op; if Etoile is not installed on
this machine, `variable-pitch' is left alone rather than failing the
whole function.  Emoji are pinned rather than left to their natural
advance, which is wider than two cells at every size."
  (when (find-font (font-spec :name edmacs-mono-family))
    (set-face-attribute 'default nil
                        :font edmacs-mono-family
                        :height (* size 10))
    (set-face-attribute 'fixed-pitch nil
                        :font edmacs-mono-family
                        :height (* size 10))
    (when (find-font (font-spec :name "Iosevka Etoile"))
      (set-face-attribute 'variable-pitch nil
                          :font "Iosevka Etoile"
                          :height (* size 10)))
    (when (find-font (font-spec :family "Apple Color Emoji"))
      (set-fontset-font t 'emoji
                        (font-spec :family "Apple Color Emoji"
                                   :size (edmacs-emoji-pin-size size))))
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

;; solarized gives `nano-modeline-active', `nano-modeline-inactive' and
;; `default' all the same background (#002b36), so the mode line is
;; invisible against its own buffer. Shade it one step up; the sidebar is
;; shaded one step DOWN (`edmacs-sidebar-background-face'), which puts the
;; three surfaces on distinct levels.
(defun edmacs--apply-modeline-contrast (&rest _)
  "Give the nano-modeline faces a background distinct from `default'.
Re-applied from `enable-theme-functions' because `load-theme' resets
faces: ui.el reloads the theme on the first graphical frame, which would
otherwise discard these the moment a GUI frame appears."
  (when (eq (frame-parameter nil 'background-mode) 'dark)
    ;; The box is what solarized uses to fake padding, but its colour is the
    ;; OLD background (#002b36) -- invisible before, a 1px outline once the
    ;; background moved to #073642. Keep the box for its height, recoloured
    ;; to the background so it stops drawing a line; drop the theme's
    ;; overline/underline, which draw one across the top and bottom edges.
    (dolist (spec '((nano-modeline-active . "#93a1a1")
                    (nano-modeline-inactive . "#586e75")))
      (set-face-attribute (car spec) nil
                          :background "#073642" :foreground (cdr spec)
                          :box '(:line-width 1 :color "#073642")
                          :overline nil :underline nil))
    ;; nano renders through its own faces, but Emacs still paints the row
    ;; with the stock ones -- their overline/underline show through as the
    ;; same stray line.
    (dolist (face '(mode-line mode-line-inactive))
      (set-face-attribute face nil
                          :background "#073642"
                          :box '(:line-width 1 :color "#073642")
                          :overline nil :underline nil))))

(defvar nano-modeline-faces)

(defun edmacs--apply-modeline-inactive-status-faces ()
  "Give the buffer-status badge an inactive face, so it survives defocus.
`nano-modeline-faces' ships `status-RW-active' / `-RO-active' /
`status-**-active' with no `-inactive' counterpart, and its own docstring
says a state left undefined falls back to defaults -- which for the
status badge means no background at all. The badge is two `display'-raised padding
spaces around the label, so losing its background does not merely dim it:
the raised spaces render as a floating artifact. Mirror each active entry
so an unfocused window keeps the same badge."
  (dolist (entry `((status-RW-inactive . (nano-modeline-status))
                   (status-RO-inactive . (nano-modeline-status))
                   (status-**-inactive . (nano-modeline-status
                                          ,(when (facep 'nano-popout-i)
                                             'nano-popout-i)))))
    (unless (assq (car entry) nano-modeline-faces)
      (push entry nano-modeline-faces))))

(with-eval-after-load 'nano-modeline
  (edmacs--apply-modeline-contrast)
  (edmacs--apply-modeline-inactive-status-faces)
  (add-hook 'enable-theme-functions #'edmacs--apply-modeline-contrast))

;; ============================================================================
;; Modeline content
;; ============================================================================
;; nano-modeline renders one `:eval' form and never consults
;; `mode-line-format's standard constructs, `global-mode-string' included.
;; Two things follow, and this section exists for both:
;;
;;   - Anything that reports through `global-mode-string' is invisible.
;;     `lsp-modeline-diagnostics-enable' (modules/programming.el) pushes
;;     onto exactly that, so its counts were being computed and rendered
;;     nowhere; `edmacs-modeline-diagnostics' below puts them back in view,
;;     reading whichever checker is live in the buffer -- flymake for
;;     eglot-managed buffers, flycheck for the rest -- rather than through
;;     that dead channel.
;;   - Every line is a plain list of (FUNCTION ARGS...) forms, so replacing
;;     one element means replacing the line. The `edmacs-modeline-*-mode'
;;     constructors below are nano's own with two elements swapped, not a
;;     reimplementation of its layout, alignment or truncation.

(defgroup edmacs-modeline nil
  "What the modeline shows, and what it leaves out."
  :group 'convenience)

;; nano-modeline is loaded by the `use-package' form below, i.e. after these
;; definitions; declare the API surface they use so the byte-compiler does
;; not warn about references that resolve fine at redisplay time.
(defvar nano-modeline-position)
(defvar nano-modeline-padding)
(defvar term-raw-map)
(declare-function nano-modeline-buffer-name "nano-modeline")
(declare-function nano-modeline-face "nano-modeline")
(declare-function nano-modeline-buffer-status "nano-modeline")
(declare-function nerd-icons-icon-for-mode "nerd-icons")
(declare-function nerd-icons-mdicon "nerd-icons")

(defcustom edmacs-modeline-name-filters
  '(("\\`\\*claude-term:\\(.*\\)\\*\\'" . "\\1")
    ("\\`\\*magit-\\([a-z]+\\): \\(.*\\)\\*\\'" . "\\2 \\1")
    ("\\`\\*helpful [^:]*: \\(.*\\)\\*\\'" . "\\1")
    ("\\`\\*cider-repl \\(.*\\)\\*\\'" . "\\1 repl"))
  "Rewrites applied to the buffer name the modeline displays.
An alist of (REGEXP . REPLACEMENT); the FIRST entry whose REGEXP matches
the whole buffer name wins, and REPLACEMENT is expanded by
`replace-regexp-in-string' (so \\\\1, \\\\2, ... refer to its groups).
Only the displayed string changes -- the buffer keeps its real name, so
`switch-to-buffer', `claude-term--parse-buffer-name' and everything else
that matches on it are unaffected.

Anchor each REGEXP with \\\\` and \\\\' unless a partial match is really
intended: an unanchored pattern will happily rewrite the middle of an
unrelated buffer's name."
  :type '(alist :key-type regexp :value-type string)
  :group 'edmacs-modeline)

(defun edmacs-modeline-filter-name (name)
  "Return NAME rewritten by the first matching `edmacs-modeline-name-filters'.
Returns NAME unchanged when nothing matches, and never returns an empty
string -- a filter that would erase the name entirely is ignored, since a
nameless modeline is strictly worse than a noisy one."
  (or (seq-some (lambda (filter)
                  (when (string-match-p (car filter) name)
                    (let ((filtered (replace-regexp-in-string
                                     (car filter) (cdr filter) name)))
                      (unless (string-empty-p filtered) filtered))))
                edmacs-modeline-name-filters)
      name))

(defun edmacs-modeline-buffer-name ()
  "Buffer name, filtered by `edmacs-modeline-name-filters'.
Re-applies the narrowing suffix `nano-modeline-buffer-name' adds on its
own no-argument path: passing it an explicit NAME takes that branch away,
and losing the narrowing indicator is not part of the trade."
  (let ((name (edmacs-modeline-filter-name (buffer-name))))
    (nano-modeline-buffer-name
     (if (buffer-narrowed-p) (format "%s [narrow]" name) name))))

(defcustom edmacs-modeline-diagnostics-format "E%d"
  "Format string for the error count in `edmacs-modeline-diagnostics'."
  :type 'string
  :group 'edmacs-modeline)

(defcustom edmacs-modeline-diagnostics-warning-format "W%d"
  "Format string for the warning count in `edmacs-modeline-diagnostics'."
  :type 'string
  :group 'edmacs-modeline)

(defvar flycheck-mode)
(defvar flycheck-current-errors)
(defvar flymake-mode)
(declare-function flycheck-count-errors "flycheck")
(declare-function flymake-diagnostics "flymake")
(declare-function flymake-diagnostic-type "flymake")

(defun edmacs-modeline--flycheck-counts ()
  "Flycheck's (ERRORS . WARNINGS) for this buffer, or nil when it is not live.
`info'-level results are noise in a modeline and are dropped."
  (when (and (bound-and-true-p flycheck-mode)
             (fboundp 'flycheck-count-errors))
    (let ((counts (flycheck-count-errors flycheck-current-errors)))
      (cons (or (alist-get 'error counts) 0)
            (or (alist-get 'warning counts) 0)))))

(defun edmacs-modeline--flymake-severity (type)
  "Numeric flymake severity for diagnostic TYPE.
Follows `flymake-category' before reading `severity': eglot's own
`eglot-error'/`eglot-warning'/`eglot-note' symbols -- and flymake's
`:error'/`:warning'/`:note' -- carry the property only on the category
they point at. An unregistered TYPE counts as an error, matching
flymake's own fallback."
  (or (get type 'severity)
      (let ((category (get type 'flymake-category)))
        (and category (get category 'severity)))
      3))

(defun edmacs-modeline--flymake-counts ()
  "Flymake's (ERRORS . WARNINGS) for this buffer, or nil when it is not live.
Notes are dropped, mirroring the flycheck side's treatment of `info'."
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics))
    (let ((errors 0) (warnings 0))
      (dolist (d (flymake-diagnostics))
        (pcase (edmacs-modeline--flymake-severity (flymake-diagnostic-type d))
          ((pred (<= 3)) (setq errors (1+ errors)))
          (2 (setq warnings (1+ warnings)))))
      (cons errors warnings))))

(defun edmacs-modeline-diagnostics ()
  "Error and warning counts, or \"\" when there is nothing to say.
Reports from flymake when it is on and from flycheck otherwise -- an
eglot-managed buffer carries the LSP diagnostics on flymake, and summing
both backends would report the same problem twice.

Silent -- not zero -- when no checker is on, still checking, or clean, so
the segment costs no width in the overwhelmingly common case. Uses the
stock `error'/`warning' faces rather than a `nano-modeline-face', which
carries no severity distinction; those two are defined by every theme."
  (let ((counts (or (edmacs-modeline--flymake-counts)
                    (edmacs-modeline--flycheck-counts))))
    (if (null counts)
        ""
      (let ((parts (delq nil
                         (list (when (> (car counts) 0)
                                 (propertize (format edmacs-modeline-diagnostics-format
                                                     (car counts))
                                             'face 'error))
                               (when (> (cdr counts) 0)
                                 (propertize (format edmacs-modeline-diagnostics-warning-format
                                                     (cdr counts))
                                             'face 'warning))))))
        (if parts (concat (string-join parts " ") " ") "")))))

(defcustom edmacs-modeline-force-text-glyphs nil
  "Non-nil forces the plain text status glyphs everywhere in the modeline
\(the buffer-status mode icon and the ghostel agent icon\), even when
`nerd-icons' is loaded. Useful for a terminal frame where nerd-icons's
private-use-area glyphs render as unreadable boxes."
  :type 'boolean
  :group 'edmacs-modeline)

(defcustom edmacs-modeline-modified-marker "*"
  "String appended to the buffer-status mode icon when the buffer has
unsaved changes and is not read-only. Keeps the modified state legible
even though the icon itself no longer spells out RW/RO/**."
  :type 'string
  :group 'edmacs-modeline)

(defun edmacs-modeline--mode-icon ()
  "Return a `nerd-icons' glyph for the current buffer's major mode, or nil.
Nil whenever icons are forced off, `nerd-icons' is not loaded, or the
lookup itself fails for any reason -- never signals, so a broken or
missing icon degrades to `nano-modeline-buffer-status's own default
text rather than breaking the modeline."
  (and (not edmacs-modeline-force-text-glyphs)
       (featurep 'nerd-icons)
       (fboundp 'nerd-icons-icon-for-mode)
       (condition-case nil
           (let ((icon (nerd-icons-icon-for-mode major-mode)))
             (and (stringp icon) icon))
         (error nil))))

(defun edmacs-modeline--restore-icon-face (result icon)
  "Re-merge ICON's own face onto its span inside RESULT, in place.
`nano-modeline-buffer-status' and `edmacs-modeline-fixed-status' both
build RESULT as a single space (top pad), the STATUS string, then a
single space (bottom pad), and `propertize' the whole span with one
uniform badge face -- which REPLACES, rather than merges with, whatever
face ICON's own characters already carried. A `nerd-icons' glyph's face
is not decoration: it names the patched PUA font family and a height
correction for that font's metrics, both required for the private-use
codepoint to render as the intended glyph instead of a box. Losing them
degrades the icon silently, so `add-face-text-property' -- which
composes onto the existing face list instead of overwriting it -- puts
them back over the exact span ICON occupies (index 1, right after the
1-char top pad, per the layout both callers share). No-op when ICON is
nil (nothing to restore) or carries no face of its own."
  (let ((icon-face (and icon (get-text-property 0 'face icon))))
    (when (and icon-face (> (length result) (length icon)))
      (add-face-text-property 1 (+ 1 (length icon)) icon-face nil result)))
  result)

(defun edmacs-modeline-buffer-status ()
  "`nano-modeline-buffer-status', with a major-mode icon in place of text.
Read-only vs. read-write is still encoded exactly as before, via the
badge's background face -- `nano-modeline-buffer-status' picks that
purely from `buffer-read-only'/`buffer-modified-p', unaffected by the
STATUS string passed in here. Modified state, which the icon alone
would drop, gets `edmacs-modeline-modified-marker' appended, but only
when the buffer is actually writable: a read-only+modified buffer
keeps the plain icon with no marker, matching
`nano-modeline-buffer-status's own read-only-wins precedence. When no
icon is available, passes nil through unchanged, i.e. exactly today's
plain RO/**/RW text. The icon's own font-family/height face, which the
badge's uniform `propertize' would otherwise clobber, is restored by
`edmacs-modeline--restore-icon-face' afterwards."
  (let* ((icon (edmacs-modeline--mode-icon))
         (status (when icon
                   (if (and (buffer-modified-p) (not buffer-read-only))
                       (concat icon edmacs-modeline-modified-marker)
                     icon))))
    (edmacs-modeline--restore-icon-face
     (nano-modeline-buffer-status status)
     icon)))

(defun edmacs-modeline-fixed-status (status &optional face)
  "Nano-styled STATUS box in FACE, independent of buffer state.
`nano-modeline-buffer-status' picks its face from `buffer-read-only' and
`buffer-modified-p'. A terminal buffer is modified by essentially every
chunk of output, so that would strobe the box's color while an agent
works. Same padding/raise treatment, fixed face."
  (let* ((padding nano-modeline-padding)
         (top (propertize " " 'display `(raise ,(car padding))))
         (bot (propertize " " 'display `(raise ,(- (cdr padding))))))
    (propertize (concat top status bot)
                'face (or face (nano-modeline-face 'status-RO)))))

(defun edmacs-modeline--agent-icon ()
  "Return a `nerd-icons' robot glyph for an agent session, or nil.
Same availability guard as `edmacs-modeline--mode-icon'."
  (and (not edmacs-modeline-force-text-glyphs)
       (featurep 'nerd-icons)
       (fboundp 'nerd-icons-mdicon)
       (condition-case nil
           (let ((icon (nerd-icons-mdicon "nf-md-robot")))
             (and (stringp icon) icon))
         (error nil))))

(defun edmacs-modeline-ghostel-status ()
  "Status glyph for a ghostel terminal buffer: a robot for claude-term,
plain \">_\" for everything else. `claude-term-mode' is the buffer-local
marker minor mode claude-term.el already turns on for exactly its own
panes; read the same way `modules/sessions.el' already reads it
cross-module, with no load-order dependency. Restores the robot icon's
own face the same way `edmacs-modeline-buffer-status' does, so its font
family/height survive `edmacs-modeline-fixed-status's uniform badge
`propertize'."
  (let* ((agent-icon (and (bound-and-true-p claude-term-mode)
                           (edmacs-modeline--agent-icon)))
         (status (cond (agent-icon agent-icon)
                       ((bound-and-true-p claude-term-mode) "AI")
                       (t ">_"))))
    (edmacs-modeline--restore-icon-face
     (edmacs-modeline-fixed-status status)
     agent-icon)))

(defun edmacs-modeline-prog-mode (&optional default)
  "Nano line for prog mode, with filtered name and diagnostics.
`nano-modeline-prog-mode' with two changes: the buffer name goes through
`edmacs-modeline-name-filters', and `edmacs-modeline-diagnostics' leads
the right side. Can be made DEFAULT mode."
  (funcall nano-modeline-position
           '((edmacs-modeline-buffer-status) " "
             (edmacs-modeline-buffer-name) " "
             (nano-modeline-git-info))
           '((edmacs-modeline-diagnostics)
             (nano-modeline-cursor-position)
             (nano-modeline-window-dedicated))
           default))

(defun edmacs-modeline-text-mode (&optional default)
  "Nano line for text mode. See `edmacs-modeline-prog-mode'.
Kept identical to the prog line rather than trimmed: this one is also
installed as the DEFAULT, so it is what every buffer with no line of its
own falls back to -- flycheck runs in plenty of those."
  (funcall nano-modeline-position
           '((edmacs-modeline-buffer-status) " "
             (edmacs-modeline-buffer-name) " "
             (nano-modeline-git-info))
           '((edmacs-modeline-diagnostics)
             (nano-modeline-cursor-position)
             (nano-modeline-window-dedicated))
           default))

(defun edmacs-modeline-ghostel-mode ()
  "Nano line for a ghostel terminal buffer, claude-term agent panes included.
ghostel-mode has no line of its own upstream, so these buffers fell
through to the DEFAULT text line and showed a file buffer's furniture: an
`RW' read-write box, the raw `*claude-term:leaf:instance*' name, and a
cursor position that means nothing in a terminal. This drops all three --
the name is filtered to the session label alone (see
`edmacs-modeline-name-filters') and the right side carries the working
directory instead."
  (funcall nano-modeline-position
           '((edmacs-modeline-ghostel-status) " "
             (edmacs-modeline-buffer-name))
           '((nano-modeline-default-directory) " "
             (nano-modeline-window-dedicated))))

(defun edmacs-modeline-term-shell-mode ()
  "Nano's `nano-modeline-term-shell-mode' with its macro call inlined.
nano-modeline.el is byte-compiled by straight without `term' loaded, so its
call to the `term-in-char-mode' macro compiles as an ordinary function call
and signals `invalid-function' at render time, killing the whole `:eval'
and leaving every term-mode buffer with an apparently-empty modeline. The
sibling `nano-modeline-eat-shell-mode' reads `eat--semi-char-mode' and
friends as plain variables, never calls them, so it cannot hit this bug --
and this repo has no `eat' package or hook wired in regardless."
  (propertize (if (eq (current-local-map) term-raw-map)
                  "(char mode)"
                "(line mode)")
              'face (nano-modeline-face 'primary)))

;; ============================================================================
;; Nano Modeline
;; ============================================================================

(use-package nano-modeline
  :config
  (setq nano-modeline-position #'nano-modeline-footer)

  (edmacs-modeline-text-mode t)

  (add-hook 'prog-mode-hook #'edmacs-modeline-prog-mode)
  (add-hook 'text-mode-hook #'edmacs-modeline-text-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'term-mode-hook #'nano-modeline-term-mode)
  (advice-add 'nano-modeline-term-shell-mode :override #'edmacs-modeline-term-shell-mode)
  ;; ghostel-mode has no nano line of its own, so a claude-term pane
  ;; otherwise falls through to the default text line -- see
  ;; `edmacs-modeline-ghostel-mode'.
  (add-hook 'ghostel-mode-hook #'edmacs-modeline-ghostel-mode))

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
