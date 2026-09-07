;;; workspaces-live-test.el --- workspaces.el tests needing a real frame -*- lexical-binding: t -*-

;;; Commentary:
;; The two assertions in workspaces.el's coverage that a SECOND real frame
;; is the whole point of: that a background frame's tab is never stamped
;; from the selected frame's buffer, and that the fullscreen policy leaves
;; a tty frame alone when it runs through the real, unstubbed
;; `after-make-frame-functions' entry.
;;
;; A second frame here is a tty frame (`(tty . "/dev/tty")'), so this
;; process needs a CONTROLLING TERMINAL -- which `emacs -Q --batch' run
;; from a script or an agent's tool call does not have. Both tests
;; `ert-skip' rather than fail in that case, so a plain batch run of this
;; file still exits 0 while testing nothing. `scripts/gui-ert.sh' does not
;; help: it supplies a graphical frame, not a terminal.
;;
;; Run with (a pty allocated directly, which works whether or not stdin is
;; already a terminal -- `script -q /dev/null' does not):
;;   python3 -c 'import pty,sys; pty.spawn(sys.argv[1:])' \
;;     emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;           -l modules/workspaces.el -l modules/workspaces-live-test.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; No `straight/build' dependency: nothing here loads sidebar.el or
;; magit-section, so this suite runs identically from a worktree and from
;; the main checkout.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'tab-bar)
(require 'dired)

;; `cl-letf' on a C subr forces a synchronous native-comp trampoline build.
;; Nothing here stubs one today; the guard keeps that true cheaply. See
;; CLAUDE.md's "`cl-letf' on a subr can silently cost 28 seconds".
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

;; `tab-bar-new-tab-to's default (t) picks a new tab's initial window state
;; through a split-then-delete dance that needs real width a pty in a
;; sandboxed environment may not report. `clone' reuses `window-state-put'
;; with no split; no test here depends on the choice.
(setq tab-bar-new-tab-choice 'clone)

;; The ambient frame is never passed through `set-frame-size' below, so
;; widen it too rather than let a narrow one wedge `tab-bar-new-tab'.
(ignore-errors (set-frame-size (selected-frame) 200 50))

(defun edmacs-workspaces-live-test--make-frame-or-skip ()
  "Return a new real frame on this process's controlling terminal, or skip."
  (condition-case e
      (let ((frame (make-frame '((window-system . nil)
                                 (tty . "/dev/tty")
                                 (tty-type . "xterm")))))
        (unless (frame-live-p frame)
          (ert-skip "could not create a second frame in this batch environment"))
        (ignore-errors (set-frame-size frame 200 50))
        frame)
    (error (ert-skip (format "could not create a second frame in this batch \
environment (no controlling terminal? see this file's Commentary for the pty \
invocation): %s" e)))))

(defmacro edmacs-workspaces-live-test--with-frames (frames &rest body)
  "Bind FRAMES (a list of symbols) to fresh real frames, run BODY, clean up."
  (declare (indent 1))
  `(let ,(mapcar (lambda (f) (list f '(edmacs-workspaces-live-test--make-frame-or-skip)))
                 frames)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (f) `(when (frame-live-p ,f) (delete-frame ,f))) frames))))

(defmacro edmacs-workspaces-live-test--with-sandbox (var &rest body)
  "Bind VAR to a fresh temp directory for BODY, deleting it after."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "edmacs-workspaces-test-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

;; ============================================================================
;; A background frame's tab is never stamped from the selected frame
;; ============================================================================

(ert-deftest edmacs-workspaces-live-test-background-frame-tab-root-never-derives-from-selected-frame ()
  "Two real frames: F1 selected showing /repo-a, F2 in the background
showing /repo-b with an unstamped current tab. Nothing may hand F2's tab
F1's answer -- the cross-frame derivation the stamped identity exists to
rule out."
  (edmacs-workspaces-live-test--with-sandbox sandbox
    (let ((dir-a (expand-file-name "repo-a" sandbox))
          (dir-b (expand-file-name "repo-b" sandbox))
          ;; The sweep would move these dired buffers between tabs off a
          ;; zero-delay timer; irrelevant here and only a source of noise.
          (edmacs-workspaces-stray-visit-relocate nil))
      (make-directory dir-a t)
      (make-directory dir-b t)
      ;; Real repos, not bare directories: root derivation normalizes a
      ;; buffer's directory to its WORKTREE root, so a non-repo sandbox
      ;; stamps nothing and the assertion below would prove nothing.
      (dolist (d (list dir-a dir-b))
        (let ((default-directory d))
          (call-process "git" nil nil nil "init" "--quiet")))
      (edmacs-workspaces-live-test--with-frames (f1 f2)
        (with-selected-frame f2 (dired dir-b))
        ;; F1 is selected for the rest of the test on purpose: `make-frame'
        ;; leaves the LAST frame it made selected in this harness.
        (select-frame f1)
        (dired dir-a)
        (should (eq (selected-frame) f1))
        ;; `tab-bar-tabs' creates F2's default tab and runs the post-open
        ;; hook for it while F1 is selected -- the exact auto-creation path
        ;; whose tab must NOT be stamped from F1's buffer.
        (tab-bar-tabs f2)
        (let ((tab (assq 'current-tab (frame-parameter f2 'tabs))))
          (should tab)
          (should-not (edmacs-workspaces-tab-root tab))
          ;; Invoking the hook directly with F2's tab, F1 still selected,
          ;; stamps nothing at all.
          (edmacs-workspaces--on-tab-post-open tab)
          (should-not (edmacs-workspaces-tab-root tab))
          ;; The frame-scoped entry point stamps F2's OWN directory.
          (edmacs-workspaces-stamp-frame-tabs f2)
          (should (equal (edmacs-workspaces-tab-root tab)
                         (file-name-as-directory (file-truename dir-b)))))))))

;; ============================================================================
;; Fullscreen policy -- a real frame, through the real unstubbed hook
;; ============================================================================

(ert-deftest edmacs-workspaces-live-test-fullscreen-leaves-tty-frames-alone ()
  "The half of the policy a real frame can prove in this harness.
Every frame this suite can open is a tty frame -- the same shape as the
daemon's own placeholder and as every `emacsclient -t' frame -- where
`fullscreen' is meaningless and gets mangled by frameset's tty shelving
on the way into a desktop file. Nothing is stubbed: the frame is made by
the suite's own helper, so the real `after-make-frame-functions' entry
runs, and `sit-for' drains the zero-delay timer it would have scheduled.

The positive case needs a window system this batch harness cannot open;
`modules/workspaces-test.el' covers
`edmacs-workspaces--fullscreen-target''s graphical branch directly."
  (should edmacs-workspaces-fullscreen)
  (should (memq #'edmacs-workspaces-apply-fullscreen after-make-frame-functions))
  (edmacs-workspaces-live-test--with-frames (frame)
    (should-not (display-graphic-p frame))
    (sit-for 0.2)
    (should-not (frame-parameter frame 'fullscreen))))

(provide 'workspaces-live-test)
;;; workspaces-live-test.el ends here
