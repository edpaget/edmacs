;;; sidebar.el --- Per-frame tab list in a left side window -*- lexical-binding: t -*-

;;; Commentary:
;; One `magit-section-mode' buffer per frame, shown in a left side window at
;; slot 0, listing the frame's tabs (`tab-bar-tabs') as top-level sections.
;; `tab-bar-mode' stays on as the model -- only its strip (`tab-bar-show') is
;; hidden -- so every `SPC T' binding sessions.el already provides keeps
;; working unchanged; this module only adds the visual list and an
;; `SPC t s' toggle.
;;
;; This is the foundation phase of the edmacs-sidebar roadmap: tab-row
;; rendering is kept in its own `magit-insert-section' block inside
;; `edmacs-sidebar--redraw' so a later phase can append further sections
;; (e.g. agent-pane status) without restructuring the per-frame buffer/hook
;; plumbing built here.
;;
;; A repo frame (one carrying an `edmacs-repo' parameter -- see frames.el)
;; instead renders one section per worktree of that repo, via
;; `edmacs-sidebar--redraw-worktrees': open ones as an ordinary tab row,
;; tab-less ones dimmed with a "no tab" hint, and any tab whose own
;; worktree has since disappeared kept with a warning face. `RET'
;; (`edmacs-sidebar-activate') opens/raises through frames.el's own
;; find-or-create path; `d' (`edmacs-sidebar-close-worktree') closes an
;; open one's tab and no-ops on a tab-less row. A repo-less frame (the
;; daemon's boot/spare frame) keeps the original flat tab list.
;;
;; The sidebar claims the LEFT element of `window-sides-slots' (cap 1)
;; through `modules/windows.el's `edmacs-windows-claim-side', the single
;; writer of that variable.
;;
;; Phase 8's "no subprocess frames on the render path" AC is checked two
;; ways. `sidebar-test.el's `edmacs-sidebar-test-redraw-and-hooks-never-
;; shell-out' is the automated, CI-equivalent guard (advises every
;; subprocess primitive to signal instead of run, then drives every
;; redraw/command path many times over) and is the actual evidence relied
;; on. A literal `M-x profiler-start' pass was also attempted on this
;; machine (GNU Emacs 31.1, emacs-plus@31/Homebrew) driving that same
;; loop for a real 60-second wall-clock run, in `--batch', in `--batch'
;; under a `script'-attached pty, and with a plain CPU-bound loop
;; containing no sidebar code at all as a control: `profiler-cpu-log' and
;; `profiler-memory-log' both come back as an empty hash table (0
;; samples) in every case, including the control -- this build's sampling
;; profiler records nothing under any non-GUI invocation on this machine,
;; not something specific to sidebar.el. A real sample-bearing profiler
;; run therefore needs an actual interactive GUI frame, outside what an
;; unattended worktree/batch pass can drive; the advice-guard test is
;; this codebase's substitute, per the same "automate the underlying
;; property, not the literal manual step" approach used elsewhere in this
;; codebase.
;;
;; One more of phase 8's ACs is met only partially, deliberately, and by
;; design (not oversight):
;;
;; - "Faces ... text fallback renders sensibly in a terminal frame" has
;;   two halves. Which glyph table gets SELECTED (nerd-icons vs. plain
;;   Unicode, including the force-text override and the nerd-icons-
;;   errors-at-runtime fallback) is fully unit-tested. Whether the
;;   plain-Unicode glyphs (● ○ ⋯ for tabs; * ? v o for agent status)
;;   read as sensible once actually painted in a real tty is a
;;   rendered-appearance judgment no `-Q --batch' ERT run can make on
;;   its own -- but it has now had an actual terminal spot-check: a real
;;   `emacs -Q -nw' tty frame, attached to a `tmux' pane under this
;;   machine's normal `en_US.UTF-8' locale, rendered every fallback
;;   glyph from both `edmacs-sidebar--fallback-glyphs' and
;;   `edmacs-sidebar-agents--fallback-glyphs' captured via
;;   `tmux capture-pane' -- every glyph came through as its intended
;;   character (no tofu boxes, no terminal-coding-system mangling to
;;   `?'). That check is manual/interactive by nature (a `-Q --batch'
;;   run has no tty to paint into at all), so it is not wired into the
;;   ERT suite and must be re-run by hand if these glyph tables change;
;;   it is otherwise the same "spot-check once, document the result"
;;   treatment the no-shellout AC's literal-profiler attempt above got.

;;; Code:

(require 'tab-bar)
(require 'desktop)
(require 'cl-lib)
(require 'windows)

;; `modules/git.el's `use-package magit :commands (...)' only activates
;; magit's autoloads file, which does not autoload `magit-section-mode',
;; `magit-insert-section', or `magit-insert-heading' (confirmed against
;; straight/build/magit-section/magit-section-autoloads.el) -- so without
;; this, `define-derived-mode' below parses fine but throws void-function
;; the first time `edmacs-sidebar-mode' actually turns on. This require
;; resolves cleanly at real init.el load time because straight already
;; puts a built package's directory (and its transitive deps: compat,
;; cond-let, llama, transient, seq) on `load-path' at build/registration
;; time, independent of magit's own `:commands' deferral. Under `-Q
;; --batch' (this module's own test harness), sidebar-test.el fixes
;; `load-path' before loading this file.
(require 'magit-section)

;; `general' loads only in a real init.el session (see the `SPC t s'
;; binding near the end of this file, gated by `with-eval-after-load');
;; declared here so the byte-compiler doesn't warn about that forward
;; reference.
(declare-function general-define-key "general")

;; frames.el loads AFTER sidebar.el (see init.el's `load-module' order),
;; so these forward references are needed for the byte-compiler even
;; though the shared-obarray runtime calls resolve fine once both
;; modules have loaded -- mirroring frames.el's own
;; `(declare-function edmacs-sidebar-show "sidebar")' in the other
;; direction.
(declare-function edmacs-worktrees-for-repo "frames")
(declare-function edmacs-frames--tab-for-root "frames")
(declare-function edmacs-frames--tab-root "frames")
(declare-function edmacs-frames-open-worktree-tab "frames")

;; git-common-dir.el loads BEFORE sidebar.el (init.el's `load-module'
;; order), so this one resolves at real load time too; declared anyway
;; for this file's own standalone `-Q --batch' test harness, which does
;; not always load it first.
(declare-function edmacs-git-common-dir-repo-name "git-common-dir")

;; sidebar-agents.el (phase 6) loads AFTER this file (init.el's
;; `load-module' order); these commands are only ever reached
;; through the keymap below, resolved at keypress time.
(declare-function edmacs-sidebar-agents-visit "sidebar-agents")
(declare-function edmacs-sidebar-agents-toggle-all "sidebar-agents")
(declare-function edmacs-sidebar-agents-rename "sidebar-agents")
(declare-function edmacs-sidebar-agents-kill "sidebar-agents")

;; Autoloaded by Emacs 29+ core (`describe-keymap.el'); declared here so
;; the byte-compiler has no forward-reference warning on a build whose
;; autoloads have not yet been regenerated.
(declare-function which-key-show-full-keymap "which-key")

;; agents.el loads AFTER this file too; used only by
;; `edmacs-sidebar--point-identity'/`--find-agent-section' below to key
;; point-preservation on an agent row by its stable struct field rather
;; than its rendered (and frequently-changing) label text.
(declare-function edmacs-agent-key "agents")

;; sidebar-buffers.el (phase 7) loads AFTER this file; these three
;; commands are only ever reached through the keymap below or the RET/d
;; dispatch, resolved at keypress time.
(declare-function edmacs-sidebar-buffers-visit "sidebar-buffers")
(declare-function edmacs-sidebar-buffers-kill "sidebar-buffers")
(declare-function edmacs-sidebar-buffers-next "sidebar-buffers")
(declare-function edmacs-sidebar-buffers-prev "sidebar-buffers")
(declare-function edmacs-sidebar-buffers-toggle-flat "sidebar-buffers")

;; ============================================================================
;; Extension points for sidebar-agents.el (phase 6)
;; ============================================================================
;; Kept here, rather than sidebar.el reaching into sidebar-agents.el
;; directly, so this file stays agent-agnostic -- exactly the same
;; swappable-seam convention claude-term-registry.el uses for
;; `claude-term-registry-state-accessor'.

(defvar edmacs-sidebar-worktree-label-suffix-function #'ignore
  "Function of one argument, a worktree ROOT (truename), returning a
string to append to that worktree's own row label, or nil.
sidebar-agents.el reassigns this to append its per-worktree agent
count.")

(defvar edmacs-sidebar-worktree-section-functions nil
  "Hook run with (ROOT HAS-TAB FRAME TAB-NUMBER) right after each worktree
row is inserted in `edmacs-sidebar--redraw-worktrees' -- ROOT is that
worktree's truename, HAS-TAB is non-nil when an open tab row was
inserted (nil for a tab-less row), FRAME is the frame being redrawn,
and TAB-NUMBER is that tab's 1-based `tab-bar-tabs' index (nil when
HAS-TAB is nil). Lets sidebar-agents.el append its own `agents' child
section, and sidebar-buffers.el its own `buffers' child section,
immediately after the row, without this file needing to know anything
about agents or buffers.")

(defvar edmacs-sidebar-extra-section-functions nil
  "Hook run with FRAME at the end of `edmacs-sidebar--redraw', after
every other section. Lets sidebar-agents.el append its own
frame-independent ALL AGENTS section.")

(defvar edmacs-sidebar-header-line-function #'ignore
  "Function of one argument, a FRAME, returning a suffix string to
append to that frame's own repo-name header line, or nil.
sidebar-agents.el reassigns this to append a repo-wide agent-status
roll-up -- the same swappable-seam convention
`edmacs-sidebar-worktree-label-suffix-function' uses.")

;; ============================================================================
;; Faces
;; ============================================================================

(defgroup edmacs-sidebar nil
  "Per-frame tab/worktree list in a left side window."
  :group 'convenience)

(defface edmacs-sidebar-worktree-closed-face
  '((t :inherit shadow))
  "Face for a worktree row with no open tab."
  :group 'edmacs-sidebar)

(defface edmacs-sidebar-missing-worktree-face
  '((t :inherit warning))
  "Face for an open tab whose worktree directory no longer exists."
  :group 'edmacs-sidebar)

(defface edmacs-sidebar-missing-repo-face
  '((t :inherit warning :weight bold))
  "Face for the warning row shown when FRAME's whole repo is gone.
See `edmacs-repo-missing', set by `modules/sessions.el's frameset
restore bridge."
  :group 'edmacs-sidebar)

(defface edmacs-sidebar-current-tab-face
  '((t :inherit magit-section-heading))
  "Face for the current tab's own row label."
  :group 'edmacs-sidebar)

(defface edmacs-sidebar-header-face
  '((t :inherit magit-section-heading))
  "Face for the sidebar buffer's header line."
  :group 'edmacs-sidebar)

;; ============================================================================
;; Customization: width, glyphs
;; ============================================================================

(defcustom edmacs-sidebar-width 32
  "Default width, in columns, of the sidebar side window.
Overridden per frame once the user manually resizes the window -- see
`edmacs-sidebar--on-window-size-change'. Always subject to
`edmacs-sidebar--clamp-width' before it reaches a live window."
  :type 'integer
  :group 'edmacs-sidebar)

(defcustom edmacs-sidebar-max-width-fraction 0.33
  "Maximum fraction of the frame's total width the sidebar may occupy.
Enforced by `edmacs-sidebar--clamp-width' both when a width is stashed
into `edmacs-sidebar-remembered-width' and when one is read back out in
`edmacs-sidebar-show' -- so a value already poisoned in a live frame
parameter or a restored desktop self-heals on the next show rather than
persisting forever."
  :type 'float
  :group 'edmacs-sidebar)

(defvar edmacs-sidebar--min-width 15
  "Floor, in columns, below which a glyph plus a truncated name no
longer fits usefully. Wins over `edmacs-sidebar-max-width-fraction' on a
pathologically narrow frame, where the fraction-derived cap would be
smaller than this floor. A plain `defvar', not `defcustom', so a test
can shrink it -- mirrors `edmacs-sidebar-resize-debounce-seconds's own
convention.")

(defun edmacs-sidebar--clamp-width (width frame)
  "Clamp WIDTH to `edmacs-sidebar-max-width-fraction' of FRAME's width,
floored at `edmacs-sidebar--min-width'."
  (max edmacs-sidebar--min-width
       (min width (floor (* (frame-width frame) edmacs-sidebar-max-width-fraction)))))

(defcustom edmacs-sidebar-force-text-glyphs nil
  "Non-nil forces the plain text/Unicode marker glyphs everywhere in the
sidebar, even when `nerd-icons' is loaded. Useful for a terminal frame
where nerd-icons's private-use-area glyphs render as unreadable boxes."
  :type 'boolean
  :group 'edmacs-sidebar)

;; ============================================================================
;; Major mode
;; ============================================================================

(define-derived-mode edmacs-sidebar-mode magit-section-mode "Sidebar"
  "Major mode listing the current frame's tabs in a side window."
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'edmacs-sidebar-mode 'motion)))

;; Plain `define-key' on the mode's own local map is not enough by
;; itself: evil installs its state keymaps via `emulation-mode-map-alists',
;; which key lookup consults BEFORE the buffer's local map, and
;; `evil-motion-state-map' already binds RET to `evil-ret' -- so with
;; only this, RET in a motion-state sidebar buffer resolves to
;; `evil-ret', never reaching `edmacs-sidebar-activate' (verified live:
;; `(key-binding (kbd "RET"))' in a motion-state sidebar buffer returned
;; `evil-ret' despite `(lookup-key edmacs-sidebar-mode-map (kbd "RET"))'
;; correctly showing the intended binding present but unreachable). `q'
;; happens to work today only because `evil-motion-state-map' has no
;; binding for it (`evil-record-macro' on `q' lives in
;; `evil-normal-state-map', not motion) -- a coincidence, not a
;; mechanism, so it gets the same real fix below rather than relying on
;; that continuing to hold.
;;
;; `edmacs-sidebar-mode-map' is this mode's own dedicated map (not a
;; shared package-global one), so `evil-define-key' on it -- landing in
;; evil's AUXILIARY-MAPS bucket, which wins over the main motion-state
;; map -- is exactly git.el's `git-timemachine-mode-map' pattern, safely
;; scoped to sidebar buffers only.
(define-key edmacs-sidebar-mode-map (kbd "RET") #'edmacs-sidebar-visit-at-point)
(define-key edmacs-sidebar-mode-map (kbd "q") #'edmacs-sidebar-hide)
(define-key edmacs-sidebar-mode-map (kbd "d") #'edmacs-sidebar-kill-at-point)
(define-key edmacs-sidebar-mode-map (kbd "a") #'edmacs-sidebar-agents-toggle-all)
(define-key edmacs-sidebar-mode-map (kbd "[") #'edmacs-sidebar-buffers-prev)
(define-key edmacs-sidebar-mode-map (kbd "]") #'edmacs-sidebar-buffers-next)
(define-key edmacs-sidebar-mode-map (kbd "s") #'edmacs-sidebar-buffers-toggle-flat)
(define-key edmacs-sidebar-mode-map (kbd "J") #'edmacs-sidebar-move-to-next-worktree)
(define-key edmacs-sidebar-mode-map (kbd "K") #'edmacs-sidebar-move-to-prev-worktree)
(define-key edmacs-sidebar-mode-map (kbd "r") #'edmacs-sidebar-rename-at-point)
(define-key edmacs-sidebar-mode-map (kbd "g r") #'edmacs-sidebar-redraw)
(define-key edmacs-sidebar-mode-map (kbd "?") #'edmacs-sidebar-help)
;; TAB and `C-i' are the same event in a non-GUI/tty keymap lookup, and
;; `evil-motion-state-map' binds `C-i' to `evil-jump-forward' regardless
;; of `evil-want-C-i-jump' (that variable only governs whether evil
;; claims plain `TAB' too under a GUI frame, where the two differ) -- so
;; TAB needs the same dual-binding override as RET/q/K/? once measured
;; live, even though `magit-section-mode-map' already binds it and a
;; GUI frame alone would not have shown the shadow. Bound to
;; `edmacs-sidebar-toggle-at-point', not plain `magit-section-toggle',
;; so a leaf row (agent/buffer) folds its enclosing group instead of
;; toggling its own bodyless heading as a no-op -- see the design
;; table's `TAB' row and that command's docstring.
(define-key edmacs-sidebar-mode-map (kbd "TAB") #'edmacs-sidebar-toggle-at-point)

(with-eval-after-load 'evil
  (evil-define-key 'motion edmacs-sidebar-mode-map
    (kbd "RET") #'edmacs-sidebar-visit-at-point
    (kbd "q") #'edmacs-sidebar-hide
    (kbd "d") #'edmacs-sidebar-kill-at-point
    (kbd "a") #'edmacs-sidebar-agents-toggle-all
    (kbd "[") #'edmacs-sidebar-buffers-prev
    (kbd "]") #'edmacs-sidebar-buffers-next
    (kbd "s") #'edmacs-sidebar-buffers-toggle-flat
    (kbd "J") #'edmacs-sidebar-move-to-next-worktree
    (kbd "K") #'edmacs-sidebar-move-to-prev-worktree
    (kbd "r") #'edmacs-sidebar-rename-at-point
    (kbd "g r") #'edmacs-sidebar-redraw
    (kbd "?") #'edmacs-sidebar-help
    (kbd "TAB") #'edmacs-sidebar-toggle-at-point))

;; ============================================================================
;; Per-frame buffer management
;; ============================================================================

(defun edmacs-sidebar--buffer (frame)
  "Return FRAME's sidebar buffer, or nil if it has none."
  (frame-parameter frame 'edmacs-sidebar-buffer))

(defun edmacs-sidebar--ensure-buffer (frame)
  "Return a live, freshly redrawn sidebar buffer for FRAME.
Creates one, lazily, the first time FRAME needs it -- not eagerly for
every frame at load time. Renames an already-live buffer whose name has
drifted from FRAME's current `name' parameter: on a daemon-boot
frameset restore, `edmacs-sidebar--on-desktop-read' shows every frame's
sidebar synchronously, before `edmacs-sessions--finish-frameset-restore'
(deferred a tick later) has regenerated that frame's real title from its
`edmacs-repo' -- so a restored frame's sidebar buffer was reproduced
live coming back permanently mis-named after the daemon's generic
default frame name (e.g. \"*sidebar: F1*\") instead of its repo."
  (let* ((buf (edmacs-sidebar--buffer frame))
         (expected (format "*sidebar: %s*" (frame-parameter frame 'name))))
    (if (buffer-live-p buf)
        (unless (equal (buffer-name buf) expected)
          (with-current-buffer buf (rename-buffer expected t)))
      (setq buf (generate-new-buffer expected))
      (set-frame-parameter frame 'edmacs-sidebar-buffer buf)
      (with-current-buffer buf
        (edmacs-sidebar-mode)))
    (edmacs-sidebar--redraw frame)
    buf))

(defun edmacs-sidebar--cleanup-frame (frame)
  "Kill FRAME's sidebar buffer, if any, when FRAME is deleted.
Scoped to exactly FRAME's own buffer/parameter -- every other frame's
sidebar buffer is untouched."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(add-hook 'delete-frame-functions #'edmacs-sidebar--cleanup-frame)

;; ============================================================================
;; Rendering
;; ============================================================================
;; Tab names are read verbatim from each tab alist's own precomputed `name'
;; field -- never recomputed via `edmacs-sessions--tab-name'/`project-current'
;; -- so redraw does no subprocess or directory-stat work.

(defun edmacs-sidebar--map-sections (section fn)
  "Call FN on SECTION and, recursively, on every descendant."
  (when section
    (funcall fn section)
    (dolist (child (oref section children))
      (edmacs-sidebar--map-sections child fn))))

(defun edmacs-sidebar--find-agent-section (key)
  "Return the `edmacs-sidebar-agent' section (anywhere in the current
buffer's section tree) whose agent's KEY matches, or nil."
  (catch 'edmacs-sidebar--found-agent-section
    (edmacs-sidebar--map-sections
     magit-root-section
     (lambda (section)
       (when (and (eq (oref section type) 'edmacs-sidebar-agent)
                  (slot-boundp section 'value)
                  (equal (edmacs-agent-key (oref section value)) key))
         (throw 'edmacs-sidebar--found-agent-section section))))
    nil))

(defun edmacs-sidebar--find-buffer-section (name)
  "Return the `edmacs-sidebar-buffers-file'/`-special' section (anywhere
in the current buffer's section tree) whose buffer's name is NAME, or nil."
  (catch 'edmacs-sidebar--found-buffer-section
    (edmacs-sidebar--map-sections
     magit-root-section
     (lambda (section)
       (when (and (memq (oref section type) '(edmacs-sidebar-buffers-file edmacs-sidebar-buffers-special))
                  (slot-boundp section 'value)
                  (buffer-live-p (oref section value))
                  (equal (buffer-name (oref section value)) name))
         (throw 'edmacs-sidebar--found-buffer-section section))))
    nil))

(defun edmacs-sidebar--point-identity ()
  "Return an identity for the row at point, preserved across a redraw.
An `edmacs-sidebar-agent' row is identified by its agent's own stable
KEY field rather than its rendered label: unlike a tab row, an agent
row's label text (elapsed-time string, title on a heartbeat refresh)
routinely changes between one redraw and the next even though it is
still \"the same row\" as far as the user sitting on it is concerned.
A `edmacs-sidebar-buffers-file'/`-special' row (sidebar-buffers.el,
phase 7) is likewise identified by its buffer's own name rather than
its rendered label, which changes with recency-based reordering.
Every other row keeps the original rendered-label identity. Returns
nil when point is on no recognized row."
  (let ((section (magit-current-section)))
    (cond
     ((and section (eq (oref section type) 'edmacs-sidebar-agent)
           (slot-boundp section 'value))
      (cons 'agent (edmacs-agent-key (oref section value))))
     ((and section
           (memq (oref section type) '(edmacs-sidebar-buffers-file edmacs-sidebar-buffers-special))
           (slot-boundp section 'value)
           (buffer-live-p (oref section value)))
      (cons 'buffer (buffer-name (oref section value))))
     (t (save-excursion
          (goto-char (line-beginning-position))
          ;; One non-space marker glyph (a plain Unicode dot/circle, or a
          ;; nerd-icons private-use-area glyph -- see `--glyph' below)
          ;; followed by a space, then the row's own text.
          (when (looking-at "\\S-+ \\(.*\\)$")
            (cons 'tab (match-string 1))))))))

(defun edmacs-sidebar--goto-identity (identity)
  "Move point to the row named by IDENTITY (from `--point-identity'),
or `point-min' if it can no longer be found."
  (goto-char (point-min))
  (pcase identity
    (`(agent . ,key)
     (let ((section (edmacs-sidebar--find-agent-section key)))
       (when section
         (goto-char (oref section start)))))
    (`(buffer . ,name)
     (let ((section (edmacs-sidebar--find-buffer-section name)))
       (when section
         (goto-char (oref section start)))))
    (`(tab . ,name)
     (re-search-forward (concat "^\\S-+ " (regexp-quote name) "$") nil t))))

;; ============================================================================
;; Glyphs: nerd-icons with a plain-text fallback
;; ============================================================================

(defconst edmacs-sidebar--fallback-glyphs
  '((current-tab . "●") (open-tab . "○") (no-tab . "⋯"))
  "Plain-Unicode fallback marker glyph per row KIND.")

(declare-function nerd-icons-octicon "nerd-icons")

(defun edmacs-sidebar--nerd-icon (kind)
  "Return KIND's nerd-icons glyph, or nil when unavailable.
Never signals: an icon lookup failing falls back to plain text exactly
as if nerd-icons were not loaded at all -- mirrors
`edmacs-sidebar-agents--nerd-icon's own convention."
  (and (not edmacs-sidebar-force-text-glyphs)
       (featurep 'nerd-icons)
       (condition-case nil
           (pcase kind
             ('current-tab (and (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-arrow_right")))
             ('open-tab (and (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-circle")))
             ('no-tab (and (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-dash")))
             (_ nil))
         (error nil))))

(defun edmacs-sidebar--glyph (kind)
  "Return the display glyph for KIND: a nerd-icon if available and not
forced off by `edmacs-sidebar-force-text-glyphs', else the plain-Unicode
fallback from `edmacs-sidebar--fallback-glyphs'."
  (or (edmacs-sidebar--nerd-icon kind)
      (alist-get kind edmacs-sidebar--fallback-glyphs "?")))

;; ============================================================================
;; Ellipsis truncation to the sidebar window's live width
;; ============================================================================

(defun edmacs-sidebar--truncate-label (label &optional frame)
  "Truncate LABEL with a trailing … to fit FRAME's sidebar window width.
FRAME defaults to the selected frame. Falls back to the frame's
remembered width, or `edmacs-sidebar-width', when the sidebar has no
live window yet (e.g. the very first redraw of a freshly created
buffer, before `display-buffer' has shown it) -- there is no live width
to measure against yet, but this is still a reasonable estimate,
consistent with what `edmacs-sidebar-show' is about to use. That
fallback is run through `edmacs-sidebar--clamp-width' just like every
other read of the same frame parameter (`edmacs-sidebar-show',
`edmacs-sidebar--remember-width'): otherwise a poisoned or merely
larger-than-clamp remembered width would render an untruncated label on
this first pass, only to be truncated correctly from the next redraw on
once a live, clamped window exists to measure."
  (let* ((frame (or frame (selected-frame)))
         (window (edmacs-sidebar--window frame))
         (width (if (window-live-p window)
                    (window-width window)
                  (edmacs-sidebar--clamp-width
                   (or (frame-parameter frame 'edmacs-sidebar-remembered-width)
                       edmacs-sidebar-width)
                   frame))))
    (if (> (length label) width)
        (concat (substring label 0 (max 0 (1- width))) "…")
      label)))

(defun edmacs-sidebar--tab-label (tab)
  "Return TAB's marker-prefixed display label, unpropertized."
  (concat (edmacs-sidebar--glyph (if (eq (car tab) 'current-tab) 'current-tab 'open-tab))
          " " (alist-get 'name tab)))

(defun edmacs-sidebar--insert-tab-row (tab tabs frame &optional root stale)
  "Insert a row for TAB, an element of TABS in FRAME.
With ROOT, the section value is `(ROOT . TAB-NUMBER)' (the worktree-aware
shape `edmacs-sidebar-activate' dispatches on); without it, the section
value is the bare 1-based TAB-NUMBER (the repo-less flat-list shape).
STALE renders the label with `edmacs-sidebar-missing-worktree-face' --
TAB's own worktree directory has disappeared from the fresh worktree
list (phase body Steps item 6); otherwise the current tab's row gets
`edmacs-sidebar-current-tab-face'."
  ;; `tabs'/`frame' passed explicitly: the 0-arg form of
  ;; `tab-bar--tab-index' defaults to `(selected-frame)' and would
  ;; silently return nil for a tab belonging to a non-selected frame.
  (let* ((tab-number (1+ (tab-bar--tab-index tab tabs frame)))
         (suffix (and root (funcall edmacs-sidebar-worktree-label-suffix-function root)))
         (label (edmacs-sidebar--truncate-label
                 (concat (edmacs-sidebar--tab-label tab) (or suffix "")) frame))
         (value (if root (cons root tab-number) tab-number)))
    (magit-insert-section (edmacs-sidebar-tab value)
      (magit-insert-heading
        (cond
         (stale (propertize label 'face 'edmacs-sidebar-missing-worktree-face))
         ((eq (car tab) 'current-tab) (propertize label 'face 'edmacs-sidebar-current-tab-face))
         (t label))))))

(defun edmacs-sidebar--insert-no-tab-row (entry frame)
  "Insert a dimmed, tab-less row for worktree ENTRY, a (NAME . ROOT) pair,
in FRAME's sidebar."
  (let* ((suffix (funcall edmacs-sidebar-worktree-label-suffix-function (cdr entry)))
         (label (edmacs-sidebar--truncate-label
                 (concat (edmacs-sidebar--glyph 'no-tab) " " (car entry) (or suffix "") " (no tab)")
                 frame)))
    (magit-insert-section (edmacs-sidebar-tab (cons (cdr entry) nil))
      (magit-insert-heading
        (propertize label 'face 'edmacs-sidebar-worktree-closed-face)))))

(defun edmacs-sidebar--redraw-tabs (frame)
  "Render FRAME's tabs as a flat list -- the repo-less fallback.
Unchanged from before worktree-awareness: used only for a frame with no
`edmacs-repo' parameter (the daemon's boot/spare frame)."
  (let ((tabs (tab-bar-tabs frame)))
    (dolist (tab tabs)
      (edmacs-sidebar--insert-tab-row tab tabs frame))))

(defun edmacs-sidebar--redraw-worktrees (frame common)
  "Render one top-level section per COMMON worktree, tab or not.
Reads `edmacs-worktrees-for-repo' -- a pure cache read, never a
subprocess call -- so this never shells out even on a cache miss.
A real, populated worktree list always includes at least the main
worktree, so an empty/nil result is a cache miss, not a real repo with
zero worktrees -- rendered as zero worktree sections, including no
stale-tab rows, rather than guessing at anything. Open worktrees render
as an ordinary tab row; tab-less ones dimmed with a dotted marker and a
\"no tab\" hint; a tab whose own worktree directory has since
disappeared from a genuinely fresh (non-empty) list is kept, rendered
with a warning face."
  (let ((worktrees (edmacs-worktrees-for-repo common)))
    (when worktrees
      (let ((roots (mapcar #'cdr worktrees))
            (tabs (tab-bar-tabs frame)))
        (dolist (entry worktrees)
          (let* ((root (cdr entry))
                 (tab (edmacs-frames--tab-for-root root frame))
                 (tab-number (and tab (1+ (tab-bar--tab-index tab tabs frame)))))
            (if tab
                (edmacs-sidebar--insert-tab-row tab tabs frame root nil)
              (edmacs-sidebar--insert-no-tab-row entry frame))
            (run-hook-with-args 'edmacs-sidebar-worktree-section-functions
                                 root (and tab t) frame tab-number)))
        (dolist (tab tabs)
          (let ((root (edmacs-frames--tab-root tab)))
            (unless (member root roots)
              (edmacs-sidebar--insert-tab-row tab tabs frame root t))))))))

(defun edmacs-sidebar--insert-missing-repo-warning ()
  "Insert a warning heading for a frame whose whole repo is gone.
Only ever shown when `edmacs-sessions--regenerate-frame-title' has set
FRAME's `edmacs-repo-missing' parameter -- see AC3."
  (magit-insert-section (edmacs-sidebar-warning)
    (magit-insert-heading
      (propertize "repo missing" 'face 'edmacs-sidebar-missing-repo-face))))

(defun edmacs-sidebar--header-line-name (frame)
  "Return FRAME's own identity string for the header line: its repo's
bare leaf directory name if it carries an `edmacs-repo' parameter, else
its frame `name' parameter (the repo-less flat-tab-list case)."
  (let ((common (frame-parameter frame 'edmacs-repo)))
    (if common
        (edmacs-git-common-dir-repo-name common)
      (or (frame-parameter frame 'name) ""))))

(defun edmacs-sidebar--header-line (frame)
  "Return FRAME's sidebar header-line string: its own repo/frame
identity plus whatever suffix `edmacs-sidebar-header-line-function'
supplies (sidebar-agents.el's repo-wide roll-up, by default none)."
  (let ((suffix (funcall edmacs-sidebar-header-line-function frame)))
    (propertize (concat (edmacs-sidebar--header-line-name frame) (or suffix ""))
                'face 'edmacs-sidebar-header-face)))

(defun edmacs-sidebar--redraw (frame)
  "Redraw FRAME's sidebar buffer from its current `tab-bar-tabs'.
No-ops when FRAME has no live sidebar buffer -- callers such as the
tab-bar hooks below fire for every frame regardless of whether that
frame's sidebar has ever been shown. Point is preserved on the same
row when possible; falls back to `point-min' otherwise. Branches on
FRAME's `edmacs-repo' parameter: a repo frame gets the worktree-aware
render, everything else keeps the original flat tab list. A frame
carrying `edmacs-repo-missing' (its repo directory vanished since it
was saved) gets a warning section ahead of everything else. Also
(re)sets the buffer's `header-line-format' -- see `--header-line'."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (common (frame-parameter frame 'edmacs-repo))
               (point-identity (edmacs-sidebar--point-identity)))
          (erase-buffer)
          (magit-insert-section (edmacs-sidebar-root)
            (when (frame-parameter frame 'edmacs-repo-missing)
              (edmacs-sidebar--insert-missing-repo-warning))
            (if common
                (edmacs-sidebar--redraw-worktrees frame common)
              (edmacs-sidebar--redraw-tabs frame))
            (run-hook-with-args 'edmacs-sidebar-extra-section-functions frame))
          (setq header-line-format (edmacs-sidebar--header-line frame))
          (edmacs-sidebar--goto-identity point-identity))))))

;; ============================================================================
;; Commands
;; ============================================================================

(defun edmacs-sidebar-activate ()
  "Act on the section at point: switch to its tab, or open/create one.
An integer section value (the repo-less flat tab list) is already the
1-based tab-number `tab-bar-select-tab' expects -- it treats 0 as a
\"reselect current tab\" sentinel, so redraw stores `(1+ index)', never
the raw 0-based index. A `(ROOT . TAB-NUMBER)' value (the worktree-aware
list) selects TAB-NUMBER when non-nil; when nil -- no tab yet for that
worktree -- opens one via `edmacs-frames-open-worktree-tab', which
performs its own find-or-create dance, so a second activation of what
is now an open row takes the tab-number branch instead and simply
reselects, never duplicating."
  (interactive)
  (when-let* ((section (magit-current-section))
              (value (and (slot-boundp section 'value) (oref section value))))
    (cond
     ((integerp value) (tab-bar-select-tab value))
     ((consp value)
      (if (cdr value)
          (tab-bar-select-tab (cdr value))
        (edmacs-frames-open-worktree-tab (car value)))))))

(defun edmacs-sidebar-visit-at-point ()
  "Act on the section at point, dispatched by its magit-section TYPE.
An `edmacs-sidebar-agent' row (sidebar-agents.el, phase 6) is visited
via `edmacs-sidebar-agents-visit'; a `edmacs-sidebar-buffers-file'/
`-special' row (sidebar-buffers.el, phase 7) via
`edmacs-sidebar-buffers-visit'; a tab/worktree row (or any other,
unrecognized section type) keeps the original `edmacs-sidebar-activate'
behavior unchanged. Every section type known to have no RET action at
all -- the root wrapper, the missing-repo warning row, sidebar-agents.el's
group/ALL-AGENTS headings, and sidebar-buffers.el's directory-node
headings -- and no section at all, signal `user-error' instead of
silently doing nothing."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((and section (eq (oref section type) 'edmacs-sidebar-agent))
      (edmacs-sidebar-agents-visit))
     ((and section (memq (oref section type) '(edmacs-sidebar-buffers-file edmacs-sidebar-buffers-special)))
      (edmacs-sidebar-buffers-visit))
     ((or (null section)
          (memq (oref section type)
                '(edmacs-sidebar-root edmacs-sidebar-warning
                  edmacs-sidebar-agents-group edmacs-sidebar-agents-all
                  edmacs-sidebar-buffers-dir)))
      (user-error "Nothing to do on this row"))
     (t (edmacs-sidebar-activate)))))

(defun edmacs-sidebar--top-level-section-at (section)
  "Return the top-level (direct child of `magit-root-section') ancestor
of SECTION, or nil when SECTION is nil or is `magit-root-section' itself."
  (while (and section (oref section parent) (not (eq (oref section parent) magit-root-section)))
    (setq section (oref section parent)))
  (and section (not (eq section magit-root-section)) section))

(defun edmacs-sidebar--move-to-worktree (delta)
  "Move point DELTA positions along the top-level rows (direct children
of `magit-root-section' -- tab/worktree rows, the warning row, and the
agents-all section are all direct children today). A no-op past either
end: DELTA is +1 for `edmacs-sidebar-move-to-next-worktree', -1 for
`edmacs-sidebar-move-to-prev-worktree'. With no current top-level row
under point (e.g. point at `point-min' before any row), DELTA > 0 moves
to the first row and DELTA < 0 is a no-op."
  (let* ((children (oref magit-root-section children))
         (current (edmacs-sidebar--top-level-section-at (magit-current-section)))
         (index (and current (cl-position current children)))
         (target (cond (index (+ index delta))
                       ((> delta 0) 0))))
    (when (and target (>= target 0) (< target (length children)))
      (goto-char (oref (nth target children) start)))))

;;;###autoload
(defun edmacs-sidebar-move-to-next-worktree ()
  "Move point to the next top-level row. A no-op past the last row."
  (interactive)
  (edmacs-sidebar--move-to-worktree 1))

;;;###autoload
(defun edmacs-sidebar-move-to-prev-worktree ()
  "Move point to the previous top-level row. A no-op before the first row."
  (interactive)
  (edmacs-sidebar--move-to-worktree -1))

(defun edmacs-sidebar--section-tab-number (section)
  "Return the open tab-number for tab-row SECTION's value, or nil.
An integer value (the repo-less flat tab list) is itself always an open
tab's number; a `(ROOT . TAB-NUMBER)' value's TAB-NUMBER may be nil for
a tab-less worktree row -- same shape `edmacs-sidebar-close-worktree'
already dispatches on."
  (let ((value (and section (slot-boundp section 'value) (oref section value))))
    (cond ((integerp value) value)
          ((consp value) (cdr value)))))

;;;###autoload
(defun edmacs-sidebar-rename-at-point ()
  "Rename the row at point: `tab-bar-rename-tab' on a tab row with an
open tab, `edmacs-sidebar-agents-rename' (sidebar-agents.el) on an agent
row. Every other row -- a tab-less worktree row, or no row at all --
signals `user-error' instead.

Always renames the tab whose row is under point, never the frame's
currently-selected tab: `tab-bar-rename-tab' called interactively
defaults TAB-NUMBER to the selected tab, which is wrong once J/K have
moved point onto a background tab's row, so this passes the row's own
tab-number through explicitly instead of using `call-interactively'."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((and section (eq (oref section type) 'edmacs-sidebar-tab))
      (if-let* ((tab-number (edmacs-sidebar--section-tab-number section)))
          (let* ((tabs (funcall tab-bar-tabs-function))
                 (tab-name (alist-get 'name (nth (1- tab-number) tabs)))
                 (new-name (read-from-minibuffer
                            "New name for tab (leave blank for automatic naming): "
                            nil nil nil nil tab-name)))
            (tab-bar-rename-tab new-name tab-number))
        (user-error "No tab to rename")))
     ((and section (eq (oref section type) 'edmacs-sidebar-agent) (slot-boundp section 'value))
      (edmacs-sidebar-agents-rename (oref section value)))
     (t (user-error "Nothing to rename here")))))

;;;###autoload
(defun edmacs-sidebar-toggle-at-point ()
  "Fold/unfold the section at point, matching the design table's `TAB'
row: a section with its own children (a tab row, an agents/buffers
group heading) folds itself via `magit-section-toggle'; a leaf row
(an agent, or a buffer file/special row) has no body of its own to
fold, so this folds its enclosing group instead -- the parent section
-- exactly as the table's colspan cell for `On an agent'/`On a buffer'
specifies. Falls back to toggling SECTION itself when it has neither
children nor a non-root parent, matching plain `magit-section-toggle's
own no-op/error behavior for the root and unparented sections."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((or (null section) (eq section magit-root-section))
      (magit-section-toggle section))
     ((oref section children) (magit-section-toggle section))
     ((let ((parent (oref section parent)))
        (and parent (not (eq parent magit-root-section))))
      (magit-section-toggle (oref section parent)))
     (t (magit-section-toggle section)))))

;;;###autoload
(defun edmacs-sidebar-redraw ()
  "Force a redraw of the selected frame's sidebar from cached data.
Never re-runs `edmacs-frames--worktrees-refresh' (a subprocess call) --
this only rebuilds the section tree from data already cached, so it is
always safe to bind to a bare key."
  (interactive)
  (edmacs-sidebar--redraw (selected-frame))
  (message "sidebar redrawn"))

;;;###autoload
(defun edmacs-sidebar-help ()
  "Show a cheat sheet of every binding in `edmacs-sidebar-mode-map'.
Prefers `which-key-show-full-keymap' when available; falls back to the
Emacs 29+ core `describe-keymap', which is always present. Emacs 31
ships which-key's autoloads by default, so `fboundp' alone is true even
under `-Q --batch' with no which-key loaded -- exercising the
`describe-keymap' branch in the ERT suite needs that autoload stub
unbound first, per
`edmacs-sidebar-test-help-falls-back-to-describe-keymap-for-real'."
  (interactive)
  (if (fboundp 'which-key-show-full-keymap)
      (which-key-show-full-keymap 'edmacs-sidebar-mode-map)
    (describe-keymap 'edmacs-sidebar-mode-map)))

(defun edmacs-sidebar-close-worktree ()
  "Close the open tab represented by the section at point.
A no-op on a tab-less worktree row -- worktree removal itself stays
with workmux/rdm, never this key (phase body Steps item 5)."
  (interactive)
  (when-let* ((section (magit-current-section))
              (value (and (slot-boundp section 'value) (oref section value))))
    (let ((tab-number (cond ((integerp value) value)
                             ((consp value) (cdr value)))))
      (when tab-number
        (tab-bar-close-tab tab-number)))))

(defun edmacs-sidebar-kill-at-point ()
  "Act on the section at point: kill a buffer row (sidebar-buffers.el,
phase 7), kill an agent session (sidebar-agents.el, phase 8, after
confirming), else close the worktree row's tab exactly as before."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((and section (memq (oref section type) '(edmacs-sidebar-buffers-file edmacs-sidebar-buffers-special)))
      (edmacs-sidebar-buffers-kill))
     ((and section (eq (oref section type) 'edmacs-sidebar-agent) (slot-boundp section 'value))
      (edmacs-sidebar-agents-kill (oref section value)))
     (t (edmacs-sidebar-close-worktree)))))

(defun edmacs-sidebar--window (frame)
  "Return FRAME's visible sidebar window, or nil.
Matches on buffer identity alone, so this also finds the sidebar buffer
sitting in an ordinary window -- a window the sidebar does not own and
must not delete. Use `edmacs-sidebar--side-window' where side-ness, not
mere presence, is the question."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (buffer-live-p buf)
      (seq-find (lambda (w) (eq (window-buffer w) buf))
                 (window-list frame 'never)))))

(defun edmacs-sidebar--side-window (frame)
  "Return FRAME's sidebar window only when it really is a left side window."
  (let ((window (edmacs-sidebar--window frame)))
    (when (and window (eq (window-parameter window 'window-side) 'left))
      window)))

;; ============================================================================
;; Manual resize survives a hide/show cycle (AC3)
;; ============================================================================
;; `preserve-size's `(t . nil)' parameter (below) blocks only AUTOMATIC
;; resizing -- `balance-windows', `fit-window-to-buffer' -- not an
;; explicit user mouse-drag or `C-x {'/`}', which keep working exactly
;; as before. What mouse-resize does NOT survive on its own is a
;; hide/show cycle: `edmacs-sidebar-show' would otherwise always fall
;; back to the `edmacs-sidebar-width' defcustom. So the frame's actual
;; window width is mirrored into a frame parameter here, debounced per
;; frame (mirroring `edmacs-frames--worktree-refresh-timers's shape) so
;; a mouse drag's stream of intermediate sizes doesn't thrash.

(defvar edmacs-sidebar-resize-debounce-seconds 0.2
  "Seconds a frame's sidebar-window-width changes coalesce into one
remembered-width update. A plain `defvar', not `defcustom', so a test
can shrink it -- mirrors `edmacs-sidebar-agents-coalesce-seconds's own
convention.")

(defvar edmacs-sidebar--resize-debounce-timers (make-hash-table :test #'eq)
  "FRAME -> pending debounce timer for `edmacs-sidebar--on-window-size-change'.")

(defun edmacs-sidebar--remember-width (frame)
  "Stash the `window-width' value that reproduces FRAME's current
sidebar window width the next time `edmacs-sidebar-show' creates a
fresh side window. `display-buffer-in-side-window's own `window-width'
action-alist entry consistently yields an actual window one column
narrower than requested on a fresh split -- confirmed live, both for
the plain `edmacs-sidebar-width' default and after a manual resize --
so `1+' compensates for that offset; a plain `window-resize' (an
already-live window, not a fresh split) has no such offset, which is
why `--on-window-size-change's own measurement below has to go through
this same compensation rather than stashing the raw width.

Refuses to stash unless WINDOW is genuinely a side window with at
least one sibling window in the frame -- a bare `window-width' read at
a moment the sidebar is effectively the frame's only live window (e.g.
`delete-other-windows', or mid-frameset-restore before other windows
exist) is not a real sidebar width and must never be persisted. The
stashed value itself is clamped via `edmacs-sidebar--clamp-width'."
  (remhash frame edmacs-sidebar--resize-debounce-timers)
  (when (frame-live-p frame)
    (let ((window (edmacs-sidebar--side-window frame)))
      (when (and (window-live-p window)
                 (> (length (window-list frame 'never)) 1))
        (set-frame-parameter
         frame 'edmacs-sidebar-remembered-width
         (edmacs-sidebar--clamp-width (1+ (window-width window)) frame))))))

(defun edmacs-sidebar--on-window-size-change (frame)
  "Registered on `window-size-change-functions': debounce-stash FRAME's
sidebar window width, if it currently has one shown. A no-op for a
frame with no live sidebar window at all -- most redisplay-triggering
size changes are unrelated windows."
  (when (and (frame-live-p frame) (edmacs-sidebar--window frame))
    (when-let* ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
      (cancel-timer timer))
    (puthash frame
             (run-at-time edmacs-sidebar-resize-debounce-seconds nil
                           #'edmacs-sidebar--remember-width frame)
             edmacs-sidebar--resize-debounce-timers)))

(add-hook 'window-size-change-functions #'edmacs-sidebar--on-window-size-change)

(defun edmacs-sidebar-show (&optional frame)
  "Show FRAME's sidebar window, creating and redrawing its buffer first.
Guarantees the result is either FRAME's left side window or nil --
never a window on any other edge. Calls
`display-buffer-in-side-window' DIRECTLY rather than handing a
one-function action list to `display-buffer': `display-buffer'
concatenates that action with `display-buffer-alist',
`display-buffer-base-action' and `display-buffer-fallback-action' into
one list and keeps trying entries after any one of them returns nil,
so when the left slot can't be created (`window-sides-slots'
forbidding it, or the frame too small) `display-buffer' would fall
through to its own base/fallback actions -- which split whatever
window is widest, landing the sidebar on the right on a wide frame.
Bypassing `display-buffer' itself is what stops that fallthrough; a
`display-buffer-overriding-action' wrapper alone would not; only
inspecting the returned window afterward would be too late; and this
call site is exactly where a nil `display-buffer-in-side-window'
result is otherwise turned into an empty-not-nil action list, so the
placement guarantee and the width clamp below share this same call by
necessity, not convenience.

As a second line of defense, if the call somehow still returns a
live, non-nil window that is not a left side window, that window is
deleted and treated as nil for the rest of this function -- nothing is
dedicated. This mirrors `claude-term--pop-to-window's own nil
guard for the ordinary `display-buffer-in-side-window' returns-nil
case (slot exhausted, frame too small). A frame with no non-side window
is repaired first, since otherwise the existing slot-0 left window is
simply reused and the frame stays without a main window.

Uses FRAME's remembered width (see above) when it has one, else
`edmacs-sidebar-width', so a manual resize survives a hide/show cycle.
Either way the width is passed through `edmacs-sidebar--clamp-width'
here, at read time -- this is what makes a value already poisoned in a
live frame parameter or a restored desktop self-heal on the very next
show, rather than only ever being prevented on write."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    ;; A frame with no non-side window would otherwise just have its
    ;; existing slot-0 left window reused, leaving it wedged.
    (when (edmacs-windows-frame-wedged-p frame)
      (edmacs-windows-repair-frame frame))
    (let* ((buf (edmacs-sidebar--ensure-buffer frame))
           (width (edmacs-sidebar--clamp-width
                   (or (frame-parameter frame 'edmacs-sidebar-remembered-width)
                       edmacs-sidebar-width)
                   frame))
           (window (with-selected-frame frame
                     (display-buffer-in-side-window
                      buf
                      `((side . left)
                        (slot . 0)
                        (window-width . ,width)
                        (preserve-size . (t . nil))
                        (window-parameters . ((no-delete-other-windows . t)
                                               (no-other-window . t)
                                               (mode-line-format . none))))))))
      (cond
       ((null window) nil)
       ((not (eq (window-parameter window 'window-side) 'left))
        ;; This call is what produced WINDOW, so deleting it is the right
        ;; cleanup -- except on the one shape `delete-window' refuses, a
        ;; window with no parent, which is released in place instead.
        (if (window-parent window)
            (delete-window window)
          (edmacs-sidebar--release-window window frame))
        nil)
       (t
        (set-window-dedicated-p window t)
        window)))))

(defun edmacs-sidebar--release-window (window frame)
  "Stop WINDOW on FRAME being the sidebar's, without ever signalling.
`delete-window' is only correct for a side window that has a parent.
The sidebar buffer in an ordinary window sits in a window the sidebar
does not own, and a window with no parent is the frame's sole window,
whose `delete-window' signals \"Attempt to delete minibuffer or sole
ordinary window\" -- both are released in place instead: side and
protection parameters cleared, dedication dropped, and another buffer
shown. Returns WINDOW when it survived, nil when it was deleted."
  (when (window-live-p window)
    (if (and (window-parameter window 'window-side) (window-parent window))
        (progn (delete-window window) nil)
      (dolist (parameter '(window-side window-slot
                           no-other-window no-delete-other-windows
                           mode-line-format))
        (set-window-parameter window parameter nil))
      (set-window-dedicated-p window nil)
      (let* ((sidebar (window-buffer window))
             (other (other-buffer sidebar t frame)))
        (set-window-buffer window
                           (if (eq other sidebar)
                               (get-buffer-create "*scratch*")
                             other)))
      window)))

(defun edmacs-sidebar-hide (&optional frame)
  "Hide FRAME's sidebar window, if shown.
Deletes the window when the sidebar genuinely owns one; otherwise
releases it in place rather than signalling -- see
`edmacs-sidebar--release-window'. Returns the surviving window, or nil."
  (interactive)
  (let* ((frame (or frame (selected-frame)))
         (window (edmacs-sidebar--window frame)))
    (when window
      (edmacs-sidebar--release-window window frame))))

;; The repaired frame has a main window again but no sidebar; this is the
;; hook `edmacs-windows-repair-frame' runs to put one back. Safe as a hook
;; member because `edmacs-sidebar-show' reaches
;; `display-buffer-in-side-window' directly rather than through
;; `display-buffer' -- see its docstring.
(add-hook 'edmacs-windows-frame-repaired-functions #'edmacs-sidebar-show)

;;;###autoload
(defun edmacs-sidebar-toggle ()
  "Hide the selected frame's sidebar window if shown, else show it."
  (interactive)
  (if (edmacs-sidebar--window (selected-frame))
      (edmacs-sidebar-hide)
    (edmacs-sidebar-show)))

;; ============================================================================
;; Redraw triggers
;; ============================================================================

(defun edmacs-sidebar--on-tab-select (_from-tab _to-tab)
  "Redraw the selected frame's sidebar; moves the current-tab marker."
  (edmacs-sidebar--redraw (selected-frame)))

(add-hook 'tab-bar-tab-post-select-functions #'edmacs-sidebar--on-tab-select)

(defun edmacs-sidebar--on-tab-open (_tab)
  "Re-show the sidebar in a new tab -- a fresh tab drops the side window."
  (edmacs-sidebar-show (selected-frame)))

(add-hook 'tab-bar-tab-post-open-functions #'edmacs-sidebar--on-tab-open)

(defun edmacs-sidebar--on-tab-pre-close (_tab _last-tab-p)
  "Redraw after the closing tab is actually removed from `tab-bar-tabs'.
`tab-bar-tab-pre-close-functions' fires BEFORE that removal, so a
synchronous redraw here would still show the closing tab; deferred one
tick instead. `frame-live-p' is checked because the last-tab-p
`delete-frame' branch can run and destroy the frame between this hook
firing and the timer executing."
  (let ((frame (selected-frame)))
    (run-at-time 0 nil
                 (lambda ()
                   (when (frame-live-p frame)
                     (edmacs-sidebar--redraw frame))))))

(add-hook 'tab-bar-tab-pre-close-functions #'edmacs-sidebar--on-tab-pre-close)

;; `tab-bar-rename-tab' has no dedicated hook; it always targets the
;; current tab of the current frame, so the advice has nothing to key off
;; besides the selected frame.
(advice-add 'tab-bar-rename-tab :after
            (lambda (&rest _) (edmacs-sidebar--redraw (selected-frame))))

;; ============================================================================
;; Hide the tab-bar strip; the sidebar is the model's only visible list
;; ============================================================================
;; Hides the strip without disabling `tab-bar-mode' -- `SPC T' stays intact.

(setq tab-bar-show nil)

;; ============================================================================
;; window-sides-slots: claim LEFT, cap 1
;; ============================================================================
;; One sidebar per frame, so the left column holds exactly one slot.

(edmacs-windows-claim-side 'left 1 'sidebar)

;; ============================================================================
;; SPC t s - toggle
;; ============================================================================
;; Populates keybindings.el's existing "toggle" ignore-stub, the way
;; git.el populates "SPC g" -- see modules/keybindings.el's "t" prefix.
;; Guarded by `with-eval-after-load' (unlike git.el/sessions.el, which load
;; only in a real init.el session) so this file stays loadable standalone
;; under `-Q --batch', which has no `general' -- see sidebar-test.el.

(with-eval-after-load 'general
  (general-define-key
   :states 'normal
   :prefix "SPC t"
   "s" '(edmacs-sidebar-toggle :which-key "toggle sidebar")))

;; ============================================================================
;; Desktop - exclude the buffer, regenerate a live one after restore
;; ============================================================================
;; Exclusion alone would leave a desktop-restored window pointing at
;; nothing; regeneration alone would race a still-live window pointing at
;; whatever desktop.el left behind. Both are needed.

(add-to-list 'desktop-modes-not-to-save 'edmacs-sidebar-mode)

(defun edmacs-sidebar--on-desktop-read ()
  "Regenerate every live frame's sidebar after a desktop restore.
A named function, not an anonymous lambda, so tests can invoke exactly
what `desktop-after-read-hook' runs: `desktop-read' itself is a no-op
under `-Q --batch' (\"This function is a no-op when Emacs is running in
batch mode\", per its own docstring), so this is the only way to exercise
this half of the restore path in the ERT suite -- see sidebar-test.el."
  (dolist (f (frame-list))
    (edmacs-sidebar-show f)))

(add-hook 'desktop-after-read-hook #'edmacs-sidebar--on-desktop-read)

;; Under the daemon, `sessions.el's `edmacs-sessions--restore-pending-frameset'
;; restores a stashed frameset from `after-make-frame-functions', deferred
;; one tick, which lands AFTER `desktop-after-read-hook' already fired at
;; boot -- so this needs its own entry on the same hook, deferred the same
;; way. Appended (depth 100, rather than the default which prepends) so it
;; runs after sessions.el's entry (added at load time, before this module
;; loads) and actually observes the frameset having landed on FRAME rather
;; than racing ahead of it.
(defun edmacs-sidebar--regenerate-after-frame (frame)
  "Show FRAME's sidebar once any pending frameset restore has landed on it."
  (run-at-time 0 nil
               (lambda ()
                 (when (frame-live-p frame)
                   (edmacs-sidebar-show frame)))))

(add-hook 'after-make-frame-functions #'edmacs-sidebar--regenerate-after-frame 100)

(provide 'sidebar)
;;; sidebar.el ends here
