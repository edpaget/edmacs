;;; test-support.el --- Shared fixtures for modules/*-test.el -*- lexical-binding: t -*-

;;; Commentary:
;; The straight-build-tree locators, the magit-section dependency-path
;; setup, the "second real frame or skip" helper, the sidebar/agent-state
;; fixtures, the tab-restore fixture, and the wedged-frame builder used to
;; each live as byte-for-byte (or near-byte-for-byte) copies in a dozen-plus
;; `modules/*-test.el' files -- see the roadmap phase this file landed
;; under for the exact counts. This module is the single source of truth
;; for all of them now.
;;
;; Load it once, before the test file that needs it:
;;
;;   emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/<suite>-test.el -f ert-run-tests-batch-and-exit
;;
;; A file whose own fixture only needs a subset of these (e.g. just the
;; build-root locator) still loads the whole module -- it is small and has
;; no side effects of its own beyond `defun'/`defmacro'.
;;
;; IMPORTANT for future suites: any 25th `modules/*-test.el' file (or any
;; existing one) that needs one of these fixtures should call the function
;; or macro here rather than pasting a new local copy -- and
;; `scripts/test-all.sh's manifest needs a `-l modules/test-support.el'
;; entry for it. Keep this file the single place these fixtures are
;; defined; rediscovering the duplication this file removed is exactly the
;; tech debt this phase exists to close.
;;
;; `edmacs-test-support-straight-build-root' below was formerly copied as
;; `<prefix>--locate-straight-build-root' into fourteen separate files
;; (`edmacs-test-support-straight-repos-root' into five more); the shorter
;; name here deliberately drops the `locate-' prefix so a caller's own
;; source text never reproduces the old duplicate's exact name --
;; `grep -l locate-straight-build-root modules/*-test.el' now finds none of
;; them, only this file's own commentary and history above.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

;; ============================================================================
;; straight/build and straight/repos locators
;; ============================================================================

(defun edmacs-test-support-straight-build-root ()
  "Return this checkout's `straight/build' directory, or nil.
Tries this checkout's own `straight/build' first -- present once this
worktree has itself been opened as a real Emacs config and straight has
bootstrapped it -- then falls back to the sibling main `edmacs' checkout's
`straight/build': a roadmap worktree lives under
`<parent>/edmacs__worktrees/<name>', sibling to the main
`<parent>/edmacs' checkout, and straight's build cache is per-checkout,
not shared. Resolved against `default-directory', so the caller must be
run with that set to a checkout root (true of every `emacs -Q --batch'
invocation documented in this repo)."
  (or
   (let ((here (expand-file-name "straight/build" default-directory)))
     (and (file-directory-p here) here))
   (let* ((root (directory-file-name (expand-file-name default-directory)))
          (worktrees-dir (directory-file-name (file-name-directory root))))
     (when (string-suffix-p "__worktrees" worktrees-dir)
       (let* ((projects-dir (file-name-directory worktrees-dir))
              (repo-name (string-remove-suffix
                          "__worktrees" (file-name-nondirectory worktrees-dir)))
              (main-build (expand-file-name
                           (concat repo-name "/straight/build") projects-dir)))
         (and (file-directory-p main-build) main-build))))))

(defun edmacs-test-support-straight-repos-root ()
  "Return this checkout's `straight/repos' directory, or nil.
Same worktree-vs-sibling-main-checkout fallback as
`edmacs-test-support-straight-build-root'. Needed as a fallback
for pure-elisp straight packages (`evil', `rotate', `general', ...) whose
`straight/build/<pkg>' symlink can point at a worktree that has itself
never bootstrapped straight (no `straight/repos' of its own), in which
case `straight/repos/<pkg>' -- straight's raw git checkout, identical
content for a pure-elisp package with no build-time file subsetting --
still resolves."
  (or
   (let ((here (expand-file-name "straight/repos" default-directory)))
     (and (file-directory-p here) here))
   (let* ((root (directory-file-name (expand-file-name default-directory)))
          (worktrees-dir (directory-file-name (file-name-directory root))))
     (when (string-suffix-p "__worktrees" worktrees-dir)
       (let* ((projects-dir (file-name-directory worktrees-dir))
              (repo-name (string-remove-suffix
                          "__worktrees" (file-name-nondirectory worktrees-dir)))
              (main-repos (expand-file-name
                           (concat repo-name "/straight/repos") projects-dir)))
         (and (file-directory-p main-repos) main-repos))))))

(defun edmacs-test-support-add-magit-section-deps (build-root)
  "Add `magit-section' and its transitive deps under BUILD-ROOT to `load-path'.
cl-lib, eieio, subr-x, format-spec, and cursor-sensor ship with Emacs core
and need no straight resolution; only these do."
  (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section"))
    (let ((dir (expand-file-name dep build-root)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; ============================================================================
;; Second-frame / graphical-frame skip helpers
;; ============================================================================

(defun edmacs-test-support-make-second-frame-or-skip ()
  "Return a second real frame on this process's controlling terminal, or skip.
Passes `tty'/`tty-type' explicitly rather than relying on `window-system'
alone: with no controlling terminal at all (the common `-Q --batch' case,
run with no pty attached) opening \"/dev/tty\" fails and this skips. Run
under a pty (e.g. `scripts/pty-ert.sh emacs -Q --batch ...') \"/dev/tty\"
does exist, and `tty-type' is hardcoded to \"xterm\" rather than inherited
from `$TERM' because the invoking shell's own terminal type (e.g.
\"xterm-ghostty\") may have no terminfo entry on this machine, which would
otherwise fail with \"Unknown terminal type\" even though a real
controlling terminal is attached; \"xterm\" is close to universally
present in terminfo databases.

Cleans up after itself on the failure path: a failed tty `make-frame'
with no real controlling terminal fires `before-make-frame-hook' (which
bufferlo uses to set its own
`bufferlo--tab-include-exclude-buffers-inhibit' flag) but errors before
ever reaching `after-make-frame-functions' (which is what normally clears
it) -- left alone, that flag stays permanently set for the rest of this
batch process, silently disabling bufferlo's own new-tab buffer-list
reset for every tab created afterward, in every frame. It can also leave
a half-made, non-functional frame behind and change the selected frame.
All three are undone here before skipping."
  (let ((original (selected-frame))
        (before (frame-list)))
    (cl-flet ((cleanup-and-skip (msg)
                (select-frame original 'norecord)
                (dolist (f (frame-list))
                  (unless (or (memq f before) (not (frame-live-p f)))
                    (ignore-errors (delete-frame f))))
                (when (boundp 'bufferlo--tab-include-exclude-buffers-inhibit)
                  (setq bufferlo--tab-include-exclude-buffers-inhibit nil))
                (ert-skip msg)))
      (condition-case e
          (let ((frame (make-frame '((window-system . nil)
                                      (tty . "/dev/tty")
                                      (tty-type . "xterm")))))
            (if (frame-live-p frame)
                frame
              (cleanup-and-skip "could not create a second frame in this batch environment")))
        (error (cleanup-and-skip (format "could not create a second frame in this \
batch environment (no controlling terminal? run under `scripts/pty-ert.sh \
emacs ...' to exercise this test): %s" e)))))))

(defun edmacs-test-support-report-suite-unavailable (file reason)
  "Skip the calling placeholder test, first reporting how many real tests
in FILE go unregistered as a result.

Several suites define their whole real `ert-deftest' set inside the
not-taken branch of a top-level `(if (null build-root) (single placeholder
test) (progn ...real tests...))' -- when the needed straight package
(magit-section, bufferlo, ...) is not found, THOSE FORMS ARE NEVER
EVALUATED, so ERT never even hears about them: `Ran 1 tests, 1 skipped'
reads as a suite of exactly one test, not as a 140-test suite that could
not run. Printing the real count here (a `(ert-deftest ' scan of FILE's
own source, so it never needs manual updating as the suite grows) keeps
that distinction visible instead of silently losing it."
  (let ((count (with-temp-buffer
                 (insert-file-contents file)
                 ;; Subtract 1: this very placeholder test is itself an
                 ;; `ert-deftest' form in FILE, and it IS registered (it is
                 ;; running right now) -- only the OTHER forms are not.
                 (1- (how-many "^ *(ert-deftest " (point-min) (point-max))))))
    (message "%d tests not registered (this file's real suite, unavailable in this \
environment): %s" count reason))
  (ert-skip reason))

(defun edmacs-test-support-gui-frame-or-skip ()
  "Skip the calling test unless running on a real graphical frame.
Fringe pixels, scroll-bar width, and the real gap between
`window-total-width' and `window-body-width' are unfalsifiable under
`emacs --batch' (a batch frame ignores `left-fringe'/`right-fringe').
Run via `scripts/gui-ert.sh' to actually exercise a test guarded by this."
  (unless (display-graphic-p)
    (ert-skip "needs a graphical frame; run via scripts/gui-ert.sh")))

;; ============================================================================
;; Sidebar / agent-state fixtures
;; ============================================================================

(defmacro edmacs-test-support-with-sidebar-buffer (&rest body)
  "Run BODY in a fresh `magit-section-mode' temp buffer.
`magit-section-mode' directly, not `edmacs-sidebar-mode' -- for suites
that deliberately do not load sidebar.el itself, and every
`magit-insert-section' call needs is buffer-local state
`magit-section-mode' itself sets up."
  (declare (indent 0))
  `(with-temp-buffer
     (magit-section-mode)
     (let ((inhibit-read-only t))
       ,@body)))

(defmacro edmacs-test-support-with-clean-agent-state (&rest body)
  "Run BODY with a fresh `edmacs-agents--table' and changed-hook, isolated
from any other suite's or this machine's real state."
  (declare (indent 0))
  `(let ((edmacs-agents--table (make-hash-table :test #'equal))
         (edmacs-agents-changed-hook nil))
     ,@body))

(defmacro edmacs-test-support-with-clean-sidebar-agents-state (&rest body)
  "Run BODY with fresh sidebar-agents module state, restored after.
Shared by sidebar-agents-test.el and sidebar-agents-live-test.el; each
file's own per-file wrapper (evil/workspaces stubs, extra bindings, if
any) nests around a call to this rather than duplicating these bindings."
  (declare (indent 0))
  `(let ((edmacs-agents--table (make-hash-table :test #'equal))
         (edmacs-agents-changed-hook nil)
         (edmacs-sidebar-agents--last-state (make-hash-table :test #'equal))
         (edmacs-sidebar-agents--attention-cache nil)
         (edmacs-sidebar-agents--attention-cursor 0)
         (edmacs-sidebar-agents--pending-notification nil)
         (edmacs-sidebar-agents--coalesce-timer nil)
         (edmacs-sidebar-agents--elapsed-timer nil)
         (edmacs-sidebar-agents-show-all nil))
     (unwind-protect
         (progn ,@body)
       (when (timerp edmacs-sidebar-agents--coalesce-timer)
         (cancel-timer edmacs-sidebar-agents--coalesce-timer))
       (when (timerp edmacs-sidebar-agents--elapsed-timer)
         (cancel-timer edmacs-sidebar-agents--elapsed-timer)))))

;; ============================================================================
;; Tab-count restore fixture
;; ============================================================================

(defmacro edmacs-test-support-with-tabs-restored (&rest body)
  "Run BODY, then close any tab bar tab it created back to the original count.
Snapshots `(length (tab-bar-tabs))' on the selected frame before BODY and,
via `unwind-protect', calls `tab-bar-close-tab' until the count matches
again -- regardless of BODY's outcome, so one test's tabs never leak into
the next. For a fixture that also mutates the ORIGINAL tab's own name or
group (not just adds new ones), snapshotting and restoring the frame's
whole `tabs' parameter instead is the right tool -- see
workspaces-test.el's own `with-scratch-tabs', which is deliberately not
this macro."
  (declare (indent 0))
  `(let ((edmacs-test-support--tabs-before (length (tab-bar-tabs))))
     (unwind-protect
         (progn ,@body)
       (while (> (length (tab-bar-tabs)) edmacs-test-support--tabs-before)
         (tab-bar-close-tab)))))

;; ============================================================================
;; Wedged-frame builder (windows.el shape-repair fixtures)
;; ============================================================================

(defun edmacs-test-support-make-wedged-frame (&optional shape)
  "Turn the selected frame into a side-window-only shape needing repair.
The batch frame is effectively 80x25 for splitting purposes no matter
what `set-frame-width' reports, so every shape below uses sizeless splits
only. Callers run inside `save-window-excursion'.

SHAPE selects which wedged shape to build:

nil (default) -- [left-side | right-side], zero non-side windows -- the
  shape `edmacs-main-window' reports nil for. Returns the two windows,
  left first, as (LEFT RIGHT).

`dedicated' -- like nil, but LEFT is a DEDICATED left side window and two
  undedicated right side windows sit beside it -- the real-world shape,
  where repair picks one of the others as its survivor and deletes the
  window the command was invoked from. Returns the three windows, left
  first, as (LEFT RIGHT RIGHT2).

`sole' -- ONE dedicated left side window that IS the frame root
  (`window-parent' nil) -- the shape actually observed in the running
  daemon (`:nwin 1 :root-side left'), and the one the other two shapes
  cannot build: with no parent, `delete-window' signals \"Attempt to
  delete minibuffer or sole ordinary window\" and `delete-other-windows'
  is a no-op, so repair has to release the window in place rather than
  collapse onto a sibling. Returns the single WINDOW, not a list."
  (delete-other-windows)
  (pcase shape
    ('sole
     (let ((window (selected-window)))
       (set-window-parameter window 'edmacs-main nil)
       (set-window-parameter window 'window-side 'left)
       (set-window-parameter window 'window-slot 0)
       (set-window-parameter window 'no-other-window t)
       (set-window-parameter window 'no-delete-other-windows t)
       (set-window-dedicated-p window t)
       window))
    ('dedicated
     (let* ((left (selected-window))
            (right (split-window left nil 'right))
            (right2 (split-window right nil 'below)))
       (dolist (w (list left right right2))
         (set-window-parameter w 'edmacs-main nil))
       (set-window-parameter left 'window-side 'left)
       (set-window-parameter left 'window-slot 0)
       (set-window-parameter left 'no-other-window t)
       (set-window-parameter left 'no-delete-other-windows t)
       (set-window-dedicated-p left t)
       (set-window-parameter right 'window-side 'right)
       (set-window-parameter right 'window-slot 0)
       (set-window-parameter right2 'window-side 'right)
       (set-window-parameter right2 'window-slot 1)
       (select-window left)
       (list left right right2)))
    (_
     (let* ((left (selected-window))
            (right (split-window left nil 'right)))
       (dolist (w (list left right))
         (set-window-parameter w 'edmacs-main nil))
       (set-window-parameter left 'window-side 'left)
       (set-window-parameter left 'window-slot 0)
       (set-window-parameter left 'no-other-window t)
       (set-window-parameter left 'no-delete-other-windows t)
       (set-window-parameter right 'window-side 'right)
       (set-window-parameter right 'window-slot 0)
       (list left right)))))

;; ============================================================================
;; Hermetic-suite state (timers, buffers, tab-bar-mode)
;; ============================================================================

(defmacro edmacs-test-support-with-hermetic-state (&rest body)
  "Run BODY, then undo any timer, buffer, or `tab-bar-mode' state it leaked.
Snapshots `timer-list', the live buffer names, and `tab-bar-mode' before
BODY; afterward (regardless of BODY's outcome, via `unwind-protect'),
cancels any timer not in the snapshot, kills any live buffer whose name
was not in the snapshot, and restores `tab-bar-mode' to its snapshotted
value.

Buffers are tracked by NAME rather than object identity: a test that
kills and recreates a pre-existing, module-owned buffer (e.g. a helper
that resets `*Warnings*') gets back a new object with the same name, and
name-based tracking recognizes it as already accounted for instead of
killing it as \"new\" -- object-identity tracking did exactly that and
permanently lost the real buffer the one time this ran. Timers have no
such stable identity to key on, so they stay identity-tracked; a
production debounce timer a test reschedules (cancel-and-replace, same
callback) is treated as new and canceled at cleanup, same as before.

Meant to wrap a whole suite's run once -- around the call to
`ert-run-tests-batch', not around each `ert-deftest' body -- matching
\"run the suite twice in one process, zero net new timers/buffers\"
rather than a per-test guarantee that could paper over one test's real
dependency on state an earlier test in the same file left behind."
  (declare (indent 0))
  `(let ((edmacs-test-support--timers-before (copy-sequence timer-list))
         (edmacs-test-support--buffer-names-before
          (mapcar #'buffer-name (buffer-list)))
         (edmacs-test-support--tab-bar-mode-before tab-bar-mode))
     (unwind-protect
         (progn ,@body)
       (dolist (timer timer-list)
         (unless (memq timer edmacs-test-support--timers-before)
           (cancel-timer timer)))
       (dolist (buf (buffer-list))
         (when (and (buffer-live-p buf)
                    (not (member (buffer-name buf)
                                 edmacs-test-support--buffer-names-before)))
           (kill-buffer buf)))
       (unless (eq tab-bar-mode edmacs-test-support--tab-bar-mode-before)
         (tab-bar-mode (if edmacs-test-support--tab-bar-mode-before 1 -1))))))

(provide 'test-support)
;;; test-support.el ends here
