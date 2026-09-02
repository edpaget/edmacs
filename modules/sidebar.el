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

;; ============================================================================
;; Major mode
;; ============================================================================

(define-derived-mode edmacs-sidebar-mode magit-section-mode "Sidebar"
  "Major mode listing the current frame's tabs in a side window."
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'edmacs-sidebar-mode 'motion)))

(define-key edmacs-sidebar-mode-map (kbd "RET") #'edmacs-sidebar-visit-tab)
(define-key edmacs-sidebar-mode-map (kbd "q") #'edmacs-sidebar-hide)

;; ============================================================================
;; Per-frame buffer management
;; ============================================================================

(defun edmacs-sidebar--buffer (frame)
  "Return FRAME's sidebar buffer, or nil if it has none."
  (frame-parameter frame 'edmacs-sidebar-buffer))

(defun edmacs-sidebar--ensure-buffer (frame)
  "Return a live, freshly redrawn sidebar buffer for FRAME.
Creates one, lazily, the first time FRAME needs it -- not eagerly for
every frame at load time."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (unless (buffer-live-p buf)
      (setq buf (generate-new-buffer
                 (format "*sidebar: %s*" (frame-parameter frame 'name))))
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
  "Return the tab name displayed on the line at point, or nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (looking-at "[●○] \\(.*\\)$")
      (match-string 1))))

(defun edmacs-sidebar--goto-tab-name (name)
  "Move point to the row for tab NAME, or `point-min' if not found."
  (goto-char (point-min))
  (unless (and name
               (re-search-forward
                (concat "^[●○] " (regexp-quote name) "$") nil t))
    (goto-char (point-min))))

(defun edmacs-sidebar--redraw (frame)
  "Redraw FRAME's sidebar buffer from its current `tab-bar-tabs'.
No-ops when FRAME has no live sidebar buffer -- callers such as the
tab-bar hooks below fire for every frame regardless of whether that
frame's sidebar has ever been shown. Point is preserved on the same
tab's row when possible; falls back to `point-min' when the
previously-pointed-at tab was closed or renamed."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (tabs (tab-bar-tabs frame))
               (point-tab-name (edmacs-sidebar--point-tab-name)))
          (erase-buffer)
          (magit-insert-section (edmacs-sidebar-root)
            (dolist (tab tabs)
              ;; `tabs'/`frame' passed explicitly: the 0-arg form of
              ;; `tab-bar--tab-index' defaults to `(selected-frame)' and
              ;; would silently return nil when redrawing a non-selected
              ;; frame (e.g. the desktop-after-read-hook loop below).
              (let ((tab-number (1+ (tab-bar--tab-index tab tabs frame))))
                (magit-insert-section (edmacs-sidebar-tab tab-number)
                  (magit-insert-heading
                    (concat (if (eq (car tab) 'current-tab) "● " "○ ")
                            (alist-get 'name tab)))))))
          (edmacs-sidebar--goto-tab-name point-tab-name))))))

;; ============================================================================
;; Commands
;; ============================================================================

(defun edmacs-sidebar-visit-tab ()
  "Switch to the tab represented by the section at point.
The section value is already the 1-based tab-number `tab-bar-select-tab'
expects -- `tab-bar-select-tab' treats 0 as a \"reselect current tab\"
sentinel, so redraw stores `(1+ index)', not the raw 0-based index."
  (interactive)
  (when-let* ((section (magit-current-section))
              (n (and (slot-boundp section 'value) (oref section value))))
    (when (integerp n)
      (tab-bar-select-tab n))))

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
`claude-term--pop-to-side-window's own nil guard."
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

(add-hook 'desktop-after-read-hook
          (lambda ()
            (dolist (f (frame-list))
              (edmacs-sidebar-show f))))

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
