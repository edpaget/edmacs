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
;; `window-sides-slots' LEFT element is bumped to 1 below; the RIGHT
;; element (reserved for edmacs-claude-terminal's agent panes) is read back
;; and preserved verbatim, never overwritten.

;;; Code:

(require 'tab-bar)
(require 'desktop)

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

;; sidebar-agents.el (phase 6) loads AFTER this file (init.el's
;; `load-module' order); these two commands are only ever reached
;; through the keymap below, resolved at keypress time.
(declare-function edmacs-sidebar-agents-visit "sidebar-agents")
(declare-function edmacs-sidebar-agents-toggle-all "sidebar-agents")

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
  "Hook run with (ROOT HAS-TAB) right after each worktree row is
inserted in `edmacs-sidebar--redraw-worktrees' -- ROOT is that
worktree's truename, HAS-TAB is non-nil when an open tab row was
inserted (nil for a tab-less row). Lets sidebar-agents.el append its
own `agents' child section immediately after the row, without this
file needing to know anything about agents.")

(defvar edmacs-sidebar-extra-section-functions nil
  "Hook run with FRAME at the end of `edmacs-sidebar--redraw', after
every other section. Lets sidebar-agents.el append its own
frame-independent ALL AGENTS section.")

;; ============================================================================
;; Faces
;; ============================================================================

(defgroup edmacs-sidebar nil
  "Per-frame tab/worktree list in a left side window."
  :group 'convenience)

(defface edmacs-sidebar-no-tab-face
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
(define-key edmacs-sidebar-mode-map (kbd "d") #'edmacs-sidebar-close-worktree)
(define-key edmacs-sidebar-mode-map (kbd "a") #'edmacs-sidebar-agents-toggle-all)

(with-eval-after-load 'evil
  (evil-define-key 'motion edmacs-sidebar-mode-map
    (kbd "RET") #'edmacs-sidebar-visit-at-point
    (kbd "q") #'edmacs-sidebar-hide
    (kbd "d") #'edmacs-sidebar-close-worktree
    (kbd "a") #'edmacs-sidebar-agents-toggle-all))

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

(defun edmacs-sidebar--point-tab-name ()
  "Return the row label displayed on the line at point, or nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (looking-at "[●○⋯] \\(.*\\)$")
      (match-string 1))))

(defun edmacs-sidebar--goto-tab-name (name)
  "Move point to the row for label NAME, or `point-min' if not found."
  (goto-char (point-min))
  (unless (and name
               (re-search-forward
                (concat "^[●○⋯] " (regexp-quote name) "$") nil t))
    (goto-char (point-min))))

(defun edmacs-sidebar--tab-label (tab)
  "Return TAB's marker-prefixed display label, unpropertized."
  (concat (if (eq (car tab) 'current-tab) "● " "○ ")
          (alist-get 'name tab)))

(defun edmacs-sidebar--insert-tab-row (tab tabs frame &optional root stale)
  "Insert a row for TAB, an element of TABS in FRAME.
With ROOT, the section value is `(ROOT . TAB-NUMBER)' (the worktree-aware
shape `edmacs-sidebar-activate' dispatches on); without it, the section
value is the bare 1-based TAB-NUMBER (the repo-less flat-list shape).
STALE renders the label with `edmacs-sidebar-missing-worktree-face' --
TAB's own worktree directory has disappeared from the fresh worktree
list (phase body Steps item 6)."
  ;; `tabs'/`frame' passed explicitly: the 0-arg form of
  ;; `tab-bar--tab-index' defaults to `(selected-frame)' and would
  ;; silently return nil for a tab belonging to a non-selected frame.
  (let* ((tab-number (1+ (tab-bar--tab-index tab tabs frame)))
         (suffix (and root (funcall edmacs-sidebar-worktree-label-suffix-function root)))
         (label (concat (edmacs-sidebar--tab-label tab) (or suffix "")))
         (value (if root (cons root tab-number) tab-number)))
    (magit-insert-section (edmacs-sidebar-tab value)
      (magit-insert-heading
        (if stale (propertize label 'face 'edmacs-sidebar-missing-worktree-face)
          label)))))

(defun edmacs-sidebar--insert-no-tab-row (entry)
  "Insert a dimmed, tab-less row for worktree ENTRY, a (NAME . ROOT) pair."
  (let* ((suffix (funcall edmacs-sidebar-worktree-label-suffix-function (cdr entry)))
         (label (concat "⋯ " (car entry) (or suffix "") " (no tab)")))
    (magit-insert-section (edmacs-sidebar-tab (cons (cdr entry) nil))
      (magit-insert-heading
        (propertize label 'face 'edmacs-sidebar-no-tab-face)))))

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
                 (tab (edmacs-frames--tab-for-root root frame)))
            (if tab
                (edmacs-sidebar--insert-tab-row tab tabs frame root nil)
              (edmacs-sidebar--insert-no-tab-row entry))
            (run-hook-with-args 'edmacs-sidebar-worktree-section-functions root (and tab t))))
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

(defun edmacs-sidebar--redraw (frame)
  "Redraw FRAME's sidebar buffer from its current `tab-bar-tabs'.
No-ops when FRAME has no live sidebar buffer -- callers such as the
tab-bar hooks below fire for every frame regardless of whether that
frame's sidebar has ever been shown. Point is preserved on the same
row when possible; falls back to `point-min' otherwise. Branches on
FRAME's `edmacs-repo' parameter: a repo frame gets the worktree-aware
render, everything else keeps the original flat tab list. A frame
carrying `edmacs-repo-missing' (its repo directory vanished since it
was saved) gets a warning section ahead of everything else."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (common (frame-parameter frame 'edmacs-repo))
               (point-tab-name (edmacs-sidebar--point-tab-name)))
          (erase-buffer)
          (magit-insert-section (edmacs-sidebar-root)
            (when (frame-parameter frame 'edmacs-repo-missing)
              (edmacs-sidebar--insert-missing-repo-warning))
            (if common
                (edmacs-sidebar--redraw-worktrees frame common)
              (edmacs-sidebar--redraw-tabs frame))
            (run-hook-with-args 'edmacs-sidebar-extra-section-functions frame))
          (edmacs-sidebar--goto-tab-name point-tab-name))))))

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
via `edmacs-sidebar-agents-visit'; every other section type (tab,
worktree, root, warning, agents-group) keeps the original
`edmacs-sidebar-activate' behavior unchanged."
  (interactive)
  (let ((section (magit-current-section)))
    (if (and section (eq (oref section type) 'edmacs-sidebar-agent))
        (edmacs-sidebar-agents-visit)
      (edmacs-sidebar-activate))))

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

(defun edmacs-sidebar--window (frame)
  "Return FRAME's visible sidebar window, or nil."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (buffer-live-p buf)
      (seq-find (lambda (w) (eq (window-buffer w) buf))
                 (window-list frame 'never)))))

(defun edmacs-sidebar-show (&optional frame)
  "Show FRAME's sidebar window, creating and redrawing its buffer first.
Guards against `display-buffer-in-side-window' returning nil -- e.g.
`window-sides-slots' forbidding creation on this edge -- by simply not
dedicating anything in that case, mirroring
`claude-term--pop-to-window's own nil guard."
  (interactive)
  (let* ((frame (or frame (selected-frame)))
         (buf (edmacs-sidebar--ensure-buffer frame))
         (window (with-selected-frame frame
                   (display-buffer
                    buf
                    '((display-buffer-in-side-window)
                      (side . left)
                      (slot . 0)
                      (window-width . 32)
                      (preserve-size . (t . nil))
                      (window-parameters . ((no-delete-other-windows . t)
                                             (no-other-window . t))))))))
    (when window
      (set-window-dedicated-p window t))
    window))

(defun edmacs-sidebar-hide (&optional frame)
  "Hide FRAME's sidebar window, if shown. Only the window is deleted."
  (interactive)
  (let* ((frame (or frame (selected-frame)))
         (window (edmacs-sidebar--window frame)))
    (when window
      (delete-window window))))

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
;; window-sides-slots: bump LEFT to 1, leave everything else untouched
;; ============================================================================
;; Rebuilt as a fresh list rather than `setcar'-mutated in place, to avoid
;; any shared-structure/byte-compiled-literal mutation hazard on the
;; `'(nil nil 3 nil)' literal `claude-term.el' installs -- and to guarantee
;; the right side's cap of 3 (reserved for edmacs-claude-terminal's agent
;; panes) survives verbatim.

(setq window-sides-slots
      (list 1 (nth 1 window-sides-slots) (nth 2 window-sides-slots)
            (nth 3 window-sides-slots)))

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
