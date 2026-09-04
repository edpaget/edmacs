;;; sessions-test.el --- Tests for sessions.el -*- lexical-binding: t -*-

;;; Commentary:
;; sessions.el cannot be `-l'-loaded standalone under plain `-Q --batch':
;; it makes an unconditional, non-deferred call to
;; `edmacs-evil-config-add-c-x-chord' (evil-config.el, never loaded here)
;; and unconditional `general-define-key' calls (the real `general',
;; loaded only in a real init.el session). So this file, like
;; sidebar-test.el, fixes up the environment before loading sessions.el
;; itself rather than taking it as a `-l' argument:
;;
;;   - `edmacs-evil-config-add-c-x-chord' is stubbed as a no-op.
;;   - the real `general' is pulled off this checkout's (or its sibling
;;     main checkout's) `straight/build', mirroring
;;     `edmacs-sidebar-test--locate-straight-build-root''s fallback.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/sessions-test.el -f ert-run-tests-batch-and-exit
;;
;; Also covers the daemon lifecycle commands `SPC q' dispatches to. They
;; exist because homebrew.mxcl.emacs-plus@31.plist sets `KeepAlive'
;; unconditionally: launchd relaunches the daemon on ANY exit, so the three
;; intents behind `SPC q q'/`q r'/`q Q' cannot share one command, and each
;; one's daemon-vs-not branch is exactly the kind of dispatch that regresses
;; silently. Every one of those tests stubs the process-ending call rather
;; than making it -- nothing here exits Emacs, kills a frame, or shells out
;; to `brew'.
;;
;; Loading sessions.el this way prints one benign "Cannot load bufferlo"
;; use-package notice (straight is not bootstrapped in this bare batch
;; harness) -- the same harmless failure claude-term-test.el's own
;; Commentary documents for `use-package ghostel'.
;;
;; Everything here stubs the frames.el side of the boundary, so the walk's
;; own eligibility gate and call ORDER are what this file pins -- not what a
;; restored tab ends up carrying. `modules/sessions-live-test.el' covers
;; that end to end, driving a real `desktop-restore-frameset' with the real
;; frames.el loaded.
;;
;; None of frames.el or sidebar.el is loaded here: the functions under
;; test call across that module boundary
;; (`edmacs-frames--tab-root'/`edmacs-frames--repo-of'/
;; `edmacs-sidebar-show'/`edmacs-sidebar--window'), and every test below
;; stubs them via `cl-letf' rather than pulling in the real modules --
;; matching frames-test.el's own no-real-frames-module-loaded style for
;; its `git-common-dir'-boundary calls.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; `cl-letf' on a C primitive (`yes-or-no-p', `call-process',
;; `file-executable-p' below) makes Emacs build a native trampoline so
;; already-native callers see the redefinition, and that compile fails under
;; `-Q' when `user-emacs-directory''s eln-cache is not writable. Nothing here
;; needs one: sessions.el is loaded from source, so the caller under test is
;; interpreted and reads the stub straight out of the symbol's function cell.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defun edmacs-sessions-test--locate-straight-build-root ()
  "Return this checkout's `straight/build' directory, or nil.
Tries this checkout's own `straight/build' first, then falls back to
the sibling main `edmacs' checkout's -- the same
worktree-vs-sibling-main-checkout fallback
`edmacs-sidebar-test--locate-straight-build-root' uses."
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

(defvar edmacs-sessions-test--build-root
  (edmacs-sessions-test--locate-straight-build-root)
  "This checkout's (or its sibling main checkout's) `straight/build' root.")

(if (null edmacs-sessions-test--build-root)

    (ert-deftest edmacs-sessions-test-general-unavailable ()
      (ert-skip "general's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (add-to-list 'load-path (expand-file-name "general" edmacs-sessions-test--build-root))
    (require 'general)

    (unless (fboundp 'edmacs-evil-config-add-c-x-chord)
      (defun edmacs-evil-config-add-c-x-chord (&rest _)
        "Stub for tests: sessions.el's real target lives in evil-config.el."
        nil))

    (load (expand-file-name "modules/sessions.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Test helpers
    ;; ==========================================================================

    (defun edmacs-sessions-test--make-second-frame-or-skip ()
      "Return a second real frame on this process's controlling terminal, or skip.
Mirrors `edmacs-sidebar-test--make-second-frame-or-skip'."
      (condition-case e
          (let ((frame (make-frame '((window-system . nil)
                                      (tty . "/dev/tty")
                                      (tty-type . "xterm")))))
            (unless (frame-live-p frame)
              (ert-skip "could not create a second frame in this batch environment"))
            frame)
        (error (ert-skip (format "could not create a second frame in this \
batch environment (no controlling terminal? run under `script -q /dev/null \
emacs ...' to exercise this test): %s" e)))))

    (defmacro edmacs-sessions-test--with-clean-frame-params (frame params &rest body)
      "Run BODY, then reset each of FRAME's PARAMS to nil afterward."
      (declare (indent 2))
      `(unwind-protect
           (progn ,@body)
         (dolist (p ,params) (set-frame-parameter ,frame p nil))))

    ;; ==========================================================================
    ;; edmacs-sessions--frame-tab-roots
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-frame-tab-roots-nil-for-zero-tabs ()
      "A frame with no tabs must not vacuously report \"every tab agrees\"."
      (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) nil)))
        (should-not (edmacs-sessions--frame-tab-roots (selected-frame)))))

    (ert-deftest edmacs-sessions-test-frame-tab-roots-nil-when-a-tab-root-unresolved ()
      (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) '(tab1 tab2)))
                ((symbol-function 'edmacs-frames--tab-root)
                 (lambda (tab) (and (eq tab 'tab1) "/root1/")))
                ((symbol-function 'edmacs-frames--repo-of)
                 (lambda (_) "/repo/.git")))
        (should-not (edmacs-sessions--frame-tab-roots (selected-frame)))))

    (ert-deftest edmacs-sessions-test-frame-tab-roots-all-resolve ()
      (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) '(tab1 tab2)))
                ((symbol-function 'edmacs-frames--tab-root)
                 (lambda (tab) (symbol-name tab)))
                ((symbol-function 'edmacs-frames--repo-of)
                 (lambda (_) "/repo/.git")))
        (should (equal (edmacs-sessions--frame-tab-roots (selected-frame))
                        '("/repo/.git" "/repo/.git")))))

    (ert-deftest edmacs-sessions-test-frame-tab-roots-selects-target-frame ()
      "Deriving a background FRAME's tab roots must never read whatever
frame the caller happens to have selected -- the cross-frame mix-up a
daemon-boot multi-frame restore hits for every frame but whichever one
is globally selected at the time. `edmacs-frames--tab-root' is a pure
read of the tab's own stamp now, so this pins the `with-selected-frame'
wrapper that keeps the guarantee for any callee that is not."
      (let* ((f1 (selected-frame))
             (f2 (edmacs-sessions-test--make-second-frame-or-skip)))
        (unwind-protect
            (with-selected-frame f1
              (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) '(tab1)))
                        ((symbol-function 'edmacs-frames--tab-root)
                         (lambda (_) (if (eq (selected-frame) f2)
                                         "/f2/root/" "/wrong-frame/")))
                        ((symbol-function 'edmacs-frames--repo-of) (lambda (root) root)))
                (should (equal (edmacs-sessions--frame-tab-roots f2) '("/f2/root/")))
                (should (eq (selected-frame) f1))))
          (when (frame-live-p f2) (delete-frame f2)))))

    ;; ==========================================================================
    ;; The restore walk's own eligibility gate
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-restorable-frame-p-delegates-to-frames ()
      "One shared predicate, so the restore walk and spare-frame adoption
cannot drift apart on what counts as a frame a repo may live on."
      (cl-letf (((symbol-function 'frame-live-p) (lambda (f) (memq f '(gui f1))))
                ((symbol-function 'edmacs-frames-frame-usable-p)
                 (lambda (f) (eq f 'gui))))
        (should (edmacs-sessions--restorable-frame-p 'gui))
        (should-not (edmacs-sessions--restorable-frame-p 'f1))
        (should-not (edmacs-sessions--restorable-frame-p 'dead))))

    (ert-deftest edmacs-sessions-test-finish-frameset-restore-skips-tty-placeholder ()
      "The daemon's initial tty frame is in `frame-list' but in no desktop
save, and the walk must not stamp it, rename it, give it a sidebar or
arm a `file-notify' watch on it. Observed live as a second frame named
after a repo it could never display. The eligible frame is processed in
full, with the tab stamp FIRST -- back-fill resolves a frame's repo from
its tabs' own roots, which a pre-stamp desktop file does not carry until
`edmacs-frames-stamp-frame-tabs' has written them."
      (let (calls)
        (cl-letf (((symbol-function 'frame-list) (lambda () '(f1 gui)))
                  ((symbol-function 'frame-live-p) (lambda (f) (memq f '(f1 gui))))
                  ((symbol-function 'daemonp) (lambda (&rest _) t))
                  ((symbol-function 'display-graphic-p)
                   (lambda (&optional f) (eq f 'gui)))
                  ((symbol-function 'frame-initial-p) (lambda (f) (eq f 'f1)))
                  ;; frames.el is not loaded in this harness (see Commentary);
                  ;; this is `edmacs-frames-frame-usable-p''s own shape, and
                  ;; frames-test.el covers the real predicate directly.
                  ((symbol-function 'edmacs-frames-frame-usable-p)
                   (lambda (f) (and (not (and (daemonp) (frame-initial-p f)))
                                    (display-graphic-p f))))
                  ((symbol-function 'edmacs-frames-stamp-frame-tabs)
                   (lambda (f) (push (cons 'stamp f) calls)))
                  ((symbol-function 'edmacs-sessions--backfill-repo-param)
                   (lambda (f) (push (cons 'backfill f) calls)))
                  ((symbol-function 'edmacs-sessions--regenerate-frame-title)
                   (lambda (f) (push (cons 'title f) calls)))
                  ((symbol-function 'edmacs-sessions--ensure-worktree-tracking)
                   (lambda (f) (push (cons 'tracking f) calls)))
                  ((symbol-function 'edmacs-sessions--ensure-sidebar)
                   (lambda (f) (push (cons 'sidebar f) calls))))
          (edmacs-sessions--finish-frameset-restore))
        (setq calls (nreverse calls))
        (should-not (seq-find (lambda (c) (eq (cdr c) 'f1)) calls))
        (should (equal (mapcar #'car calls)
                       '(stamp backfill title tracking sidebar)))))

    ;; ==========================================================================
    ;; AC1 -- edmacs-sessions--backfill-repo-param
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-backfill-repo-param-uses-non-selected-frames-own-tabs ()
      "The same cross-frame mix-up, one level up: backfilling a background
frame's `edmacs-repo' must resolve from ITS OWN tabs, not whichever
frame the caller happens to have globally selected -- the exact defect
`edmacs-sessions--finish-frameset-restore' would otherwise hit for
every restored frame but the one already selected."
      (let* ((f1 (selected-frame))
             (f2 (edmacs-sessions-test--make-second-frame-or-skip)))
        (unwind-protect
            (edmacs-sessions-test--with-clean-frame-params f2 '(edmacs-repo)
              (set-frame-parameter f2 'edmacs-repo nil)
              (with-selected-frame f1
                (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) '(tab1)))
                          ((symbol-function 'edmacs-frames--tab-root)
                           (lambda (_) (if (eq (selected-frame) f2)
                                           "/f2/root/" "/f1/root/")))
                          ((symbol-function 'edmacs-frames--repo-of)
                           (lambda (root) (if (equal root "/f2/root/")
                                               "/f2/repo/.git" "/f1/repo/.git"))))
                  (edmacs-sessions--backfill-repo-param f2)
                  (should (equal (frame-parameter f2 'edmacs-repo) "/f2/repo/.git")))))
          (when (frame-live-p f2) (delete-frame f2)))))

    (ert-deftest edmacs-sessions-test-backfill-repo-param-when-all-tabs-agree ()
      (let ((frame (selected-frame)))
        (edmacs-sessions-test--with-clean-frame-params frame '(edmacs-repo)
          (set-frame-parameter frame 'edmacs-repo nil)
          (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) '(tab1 tab2)))
                    ((symbol-function 'edmacs-frames--tab-root)
                     (lambda (tab) (symbol-name tab)))
                    ((symbol-function 'edmacs-frames--repo-of)
                     (lambda (_) "/repo/.git")))
            (edmacs-sessions--backfill-repo-param frame)
            (should (equal (frame-parameter frame 'edmacs-repo) "/repo/.git"))))))

    (ert-deftest edmacs-sessions-test-backfill-repo-param-skips-when-tabs-disagree ()
      (let ((frame (selected-frame)))
        (edmacs-sessions-test--with-clean-frame-params frame '(edmacs-repo)
          (set-frame-parameter frame 'edmacs-repo nil)
          (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) '(tab1 tab2)))
                    ((symbol-function 'edmacs-frames--tab-root)
                     (lambda (tab) (symbol-name tab)))
                    ((symbol-function 'edmacs-frames--repo-of)
                     (lambda (root) (if (equal root "tab1") "/repo-a/.git" "/repo-b/.git"))))
            (edmacs-sessions--backfill-repo-param frame)
            (should-not (frame-parameter frame 'edmacs-repo))))))

    (ert-deftest edmacs-sessions-test-backfill-repo-param-leaves-existing-param-alone ()
      (let ((frame (selected-frame)))
        (edmacs-sessions-test--with-clean-frame-params frame '(edmacs-repo)
          (set-frame-parameter frame 'edmacs-repo "/already/set/.git")
          (cl-letf (((symbol-function 'tab-bar-tabs)
                     (lambda (&rest _) (error "must not consult tabs when edmacs-repo is already set"))))
            (edmacs-sessions--backfill-repo-param frame)
            (should (equal (frame-parameter frame 'edmacs-repo) "/already/set/.git"))))))

    ;; ==========================================================================
    ;; AC1/AC3 -- edmacs-sessions--regenerate-frame-title
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-regenerate-title-sets-name-for-existing-repo ()
      (let* ((frame (selected-frame))
             (dir (file-name-as-directory (make-temp-file "edmacs-sessions-test-" t))))
        (unwind-protect
            (edmacs-sessions-test--with-clean-frame-params
                frame '(edmacs-repo edmacs-repo-missing name)
              (set-frame-parameter frame 'edmacs-repo dir)
              (set-frame-parameter frame 'edmacs-repo-missing t)
              (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                        ((symbol-function 'tab-bar-rename-tab) (lambda (&rest _) nil)))
                (edmacs-sessions--regenerate-frame-title frame))
              (should (equal (frame-parameter frame 'name)
                              (edmacs-git-common-dir-repo-name dir)))
              (should-not (frame-parameter frame 'edmacs-repo-missing)))
          (delete-directory dir t))))

    (ert-deftest edmacs-sessions-test-regenerate-title-noop-without-edmacs-repo ()
      (let ((frame (selected-frame)))
        (edmacs-sessions-test--with-clean-frame-params frame '(edmacs-repo name)
          (set-frame-parameter frame 'edmacs-repo nil)
          (set-frame-parameter frame 'name "untouched")
          (edmacs-sessions--regenerate-frame-title frame)
          (should (equal (frame-parameter frame 'name) "untouched")))))

    (ert-deftest edmacs-sessions-test-regenerate-title-missing-repo-warns ()
      (let* ((frame (selected-frame))
             (dir "/no/such/edmacs-sessions-test/path/.git/")
             (warnings nil))
        (edmacs-sessions-test--with-clean-frame-params
            frame '(edmacs-repo edmacs-repo-missing name)
          (set-frame-parameter frame 'edmacs-repo dir)
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings))))
            (edmacs-sessions--regenerate-frame-title frame))
          (should (eq (frame-parameter frame 'edmacs-repo-missing) t))
          (should (string-prefix-p "MISSING: " (frame-parameter frame 'name)))
          (should (= 1 (length warnings)))
          (should (eq (nth 0 (car warnings)) 'edmacs-sessions))
          (should (string-match-p (regexp-quote dir) (nth 1 (car warnings))))
          (should (eq (nth 2 (car warnings)) :warning)))))

    (ert-deftest edmacs-sessions-test-regenerate-title-uses-frames-own-buffer-not-ambient-current-buffer ()
      "`with-selected-frame' alone does not change `current-buffer' --
confirmed live against a real daemon restart: a restore timer's own
ambient buffer stayed current while the frame being processed kept
showing its own buffer in its own selected window. `edmacs-sessions--
tab-name' reads `default-directory', a buffer-local variable that
tracks *current buffer*, not the selected window -- so deriving it
without first making FRAME's own window buffer current renamed every
restored frame's tab from whatever buffer the timer happened to have
current instead of that frame's own, corrupting tab names across a
multi-frame restore (reproduced live before this fix)."
      (let* ((frame (selected-frame))
             (dir (file-name-as-directory (make-temp-file "edmacs-sessions-test-" t)))
             (frame-buf (generate-new-buffer "edmacs-sessions-test-frame-buffer"))
             (ambient-buf (generate-new-buffer "edmacs-sessions-test-ambient-buffer"))
             (renamed-with nil))
        (unwind-protect
            (edmacs-sessions-test--with-clean-frame-params frame '(edmacs-repo)
              (set-frame-parameter frame 'edmacs-repo dir)
              (set-window-buffer (frame-selected-window frame) frame-buf)
              (with-current-buffer ambient-buf
                (cl-letf (((symbol-function 'edmacs-sessions--tab-name)
                           (lambda () (buffer-name (current-buffer))))
                          ((symbol-function 'tab-bar-rename-tab)
                           (lambda (name &rest _) (setq renamed-with name))))
                  (edmacs-sessions--regenerate-frame-title frame))
                (should (equal renamed-with "edmacs-sessions-test-frame-buffer"))))
          (delete-directory dir t)
          (kill-buffer frame-buf)
          (kill-buffer ambient-buf))))

    ;; ==========================================================================
    ;; AC1 -- edmacs-sessions--ensure-worktree-tracking
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-ensure-worktree-tracking-warms-cache-for-existing-repo ()
      "A daemon restart starts frames.el's worktree cache and watch table
empty, so a restored repo frame must have both warmed here -- unlike a
frame `edmacs-frames-open' creates itself, which always does this as
part of opening."
      (let* ((frame (selected-frame))
             (dir (file-name-as-directory (make-temp-file "edmacs-sessions-test-" t)))
             (tracked nil))
        (unwind-protect
            (edmacs-sessions-test--with-clean-frame-params frame '(edmacs-repo)
              (set-frame-parameter frame 'edmacs-repo dir)
              (cl-letf (((symbol-function 'edmacs-frames--ensure-repo-tracking)
                         (lambda (common) (push common tracked))))
                (edmacs-sessions--ensure-worktree-tracking frame)
                (should (equal tracked (list dir)))))
          (delete-directory dir t))))

    (ert-deftest edmacs-sessions-test-ensure-worktree-tracking-skips-missing-repo ()
      "AC3: nothing in the restore path may shell out for a repo whose
directory no longer exists."
      (let ((frame (selected-frame))
            (tracked nil))
        (edmacs-sessions-test--with-clean-frame-params frame '(edmacs-repo)
          (set-frame-parameter frame 'edmacs-repo "/no/such/edmacs-sessions-test/path/.git/")
          (cl-letf (((symbol-function 'edmacs-frames--ensure-repo-tracking)
                     (lambda (common) (push common tracked))))
            (edmacs-sessions--ensure-worktree-tracking frame)
            (should-not tracked)))))

    (ert-deftest edmacs-sessions-test-ensure-worktree-tracking-noop-without-edmacs-repo ()
      (let ((frame (selected-frame))
            (tracked nil))
        (edmacs-sessions-test--with-clean-frame-params frame '(edmacs-repo)
          (set-frame-parameter frame 'edmacs-repo nil)
          (cl-letf (((symbol-function 'edmacs-frames--ensure-repo-tracking)
                     (lambda (common) (push common tracked))))
            (edmacs-sessions--ensure-worktree-tracking frame)
            (should-not tracked)))))

    ;; ==========================================================================
    ;; Frame position (`left'/`top') is deliberately NOT restored
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-frameset-filter-drops-frame-position ()
      "Supersedes the earlier AC1, which wanted saved positions honoured.
Every graphical frame now opens fullscreen (frames.el's
`edmacs-frames-fullscreen'), so there is no position worth replaying --
only the hazard that `frameset-filter-alist''s default action for
`left'/`top' (`frameset-filter-shelve-param', which passes both through
verbatim on a GUI-to-GUI restore) puts a restored frame back at the
coordinates of whichever monitor it was saved on, which may not be
attached any more. `:never' makes it land on the current display and
fullscreen there instead.

`width'/`height' need no filter of their own: `frameset--restore-frame'
already drops both, and `visibility', from the config of any frame
saved carrying a `fullscreen' parameter."
      (dolist (param '(left top))
        (should (eq (cdr (assq param frameset-filter-alist)) :never))))

    ;; ==========================================================================
    ;; Empty-frameset guard -- the self-perpetuating frameless-daemon loop
    ;; ==========================================================================

    (defun edmacs-sessions-test--frameset (states)
      "Return a frameset carrying STATES, shaped the way desktop.el writes one."
      (frameset--make :version 1 :timestamp '(0 0 0 0)
                      :app '(desktop . "208") :name "test"
                      :states states))

    (defvar edmacs-sessions-test--one-state
      (list (cons (list (cons 'name "a-frame")) nil))
      "One (FRAME-PARAMETERS . WINDOW-STATE) item -- a frameset with a frame in it.")

    (ert-deftest edmacs-sessions-test-frameset-has-frames-p-rejects-empty ()
      "The literal shape found in a poisoned `.emacs.desktop':
`[frameset 1 TIMESTAMP (desktop . \"208\") NAME nil nil nil]' -- a
non-nil object whose `states' list is empty, which every bare non-nil
test happily accepts."
      (should-not (edmacs-sessions--frameset-has-frames-p
                   (edmacs-sessions-test--frameset nil)))
      (should-not (edmacs-sessions--frameset-has-frames-p nil))
      (should-not (edmacs-sessions--frameset-has-frames-p 'not-a-frameset)))

    (ert-deftest edmacs-sessions-test-frameset-has-frames-p-accepts-populated ()
      (should (edmacs-sessions--frameset-has-frames-p
               (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))))

    (ert-deftest edmacs-sessions-test-stash-skips-empty-frameset ()
      "Stashing an empty frameset is what closes the loop: it is replayed on
the boot frame, `frameset-restore''s cleanup pass deletes that frame
because no state ever claims it, and the frameless session then saves
another empty frameset for the next boot to find."
      (let ((edmacs-sessions--pending-frameset nil)
            (desktop-saved-frameset (edmacs-sessions-test--frameset nil)))
        (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
                  ((symbol-function 'desktop-restoring-frameset-p) (lambda () nil)))
          (edmacs-sessions--stash-frameset-for-daemon)
          (should-not edmacs-sessions--pending-frameset))))

    (ert-deftest edmacs-sessions-test-stash-takes-populated-frameset ()
      (let* ((fs (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))
             (edmacs-sessions--pending-frameset nil)
             (desktop-saved-frameset fs))
        (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
                  ((symbol-function 'desktop-restoring-frameset-p) (lambda () nil)))
          (edmacs-sessions--stash-frameset-for-daemon)
          (should (eq edmacs-sessions--pending-frameset fs)))))

    (ert-deftest edmacs-sessions-test-restore-pending-ignores-empty-frameset ()
      "The same guard on the restore side, so an empty frameset that reached
`edmacs-sessions--pending-frameset' by any other route still never gets
as far as `frameset-restore''s frame-deleting cleanup pass."
      (let ((edmacs-sessions--pending-frameset (edmacs-sessions-test--frameset nil))
            (scheduled 0) (restored nil))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) (setq scheduled (1+ scheduled)) nil))
                  ((symbol-function 'desktop-restore-frameset)
                   (lambda (&rest _) (setq restored t))))
          (edmacs-sessions--restore-pending-frameset (selected-frame))
          (should (= scheduled 0))
          (should-not restored))))

    (defun edmacs-sessions-test--run-restore-timer (restore-fn)
      "Drive the deferred body `edmacs-sessions--restore-pending-frameset\='
schedules, with RESTORE-FN standing in for `desktop-restore-frameset\='.
Returns (ORDER . WARNINGS): the sequence of steps the timer reached and
any `display-warning\=' text it produced. RESTORE-FN is called with the
frame handed to the sweep so a stub can simulate `frameset-restore\='
deleting it."
      (let ((edmacs-sessions--pending-frameset
             (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))
            (deferred nil) (order nil) (warnings nil))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest _) (setq deferred fn) nil))
                  ((symbol-function 'desktop-restore-frameset)
                   (lambda (&rest _) (push 'restore order) (funcall restore-fn)))
                  ((symbol-function 'edmacs-stack-sweep-stale-panes)
                   (lambda (&rest _) (push 'sweep order)))
                  ((symbol-function 'edmacs-sessions--ensure-gui-frame)
                   (lambda () (push 'ensure order)))
                  ((symbol-function 'edmacs-sessions--finish-frameset-restore)
                   (lambda () (push 'finish order)))
                  ((symbol-function 'display-warning)
                   (lambda (_type msg &rest _) (push msg warnings))))
          (edmacs-sessions--restore-pending-frameset (selected-frame))
          (should deferred)
          (should-not edmacs-sessions--pending-frameset)
          (funcall deferred))
        (cons (nreverse order) (nreverse warnings))))

    (ert-deftest edmacs-sessions-test-restore-pending-runs-the-gui-frame-net ()
      "The net runs inside the same timer body, after the restore and before
the back-fill -- so a frame it creates to replace one `frameset-restore\='
deleted still gets its title, worktree tracking, and sidebar."
      (let ((result (edmacs-sessions-test--run-restore-timer #'ignore)))
        (should (equal (car result) '(restore sweep ensure finish)))
        (should-not (cdr result))))

    (ert-deftest edmacs-sessions-test-restore-pending-skips-sweep-on-deleted-frame ()
      "`frameset-restore\=' deletes the very frame it was handed when its
cleanup pass finds no saved state to reassign to it. The sweep opens
with `with-selected-frame\=', so calling it on that dead frame signals
`(wrong-type-argument frame-live-p ...)\=' out of the timer and skips the
net -- leaving the daemon with only its tty placeholder, which is the
frameless state that goes on to save an empty frameset."
      (let* ((target (selected-frame))
             (deleted nil)
             (result
              (cl-letf (((symbol-function 'frame-live-p)
                         (lambda (f) (if (eq f target) (not deleted) t))))
                (edmacs-sessions-test--run-restore-timer
                 (lambda () (setq deleted t))))))
        (should (equal (car result) '(restore ensure finish)))
        (should-not (memq 'sweep (car result)))
        (should-not (cdr result))))

    (ert-deftest edmacs-sessions-test-restore-pending-nets-a-signalling-restore ()
      "Any error out of the restore still leaves a GUI frame behind: the
guard warns and the net runs anyway, rather than the timer aborting
into a frameless daemon."
      (let ((result (edmacs-sessions-test--run-restore-timer
                     (lambda () (error "boom")))))
        (should (equal (car result) '(restore ensure finish)))
        (should (= (length (cdr result)) 1))
        (should (string-match-p "boom" (car (cdr result))))))

    ;; ==========================================================================
    ;; edmacs-sessions--ensure-gui-frame / --make-gui-frame
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-ensure-gui-frame-noop-when-one-exists ()
      (let ((made 0))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'edmacs-sessions--make-gui-frame)
                   (lambda () (setq made (1+ made)))))
          (edmacs-sessions--ensure-gui-frame)
          (should (= made 0)))))

    (ert-deftest edmacs-sessions-test-ensure-gui-frame-creates-one-when-none ()
      "A daemon left with only its tty placeholder -- `display-graphic-p' nil
on every live frame -- is exactly the state that saves an empty
frameset and poisons the next boot."
      (let ((made 0))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
                  ((symbol-function 'edmacs-sessions--make-gui-frame)
                   (lambda () (setq made (1+ made)))))
          (edmacs-sessions--ensure-gui-frame)
          (should (= made 1)))))

    (ert-deftest edmacs-sessions-test-gui-frame-parameters-name-the-window-system ()
      "A daemon's `window-system' is nil, so a bare `make-frame' would hand
back another tty placeholder rather than the GUI frame asked for."
      (let ((system-type 'darwin))
        (should (equal (edmacs-sessions--gui-frame-parameters)
                       '((window-system . ns)))))
      (let ((system-type 'gnu/linux))
        (should-not (edmacs-sessions--gui-frame-parameters))))

    (ert-deftest edmacs-sessions-test-make-gui-frame-passes-those-parameters ()
      (let (passed)
        (cl-letf (((symbol-function 'make-frame)
                   (lambda (&optional params) (setq passed params) 'a-frame)))
          (should (eq (edmacs-sessions--make-gui-frame) 'a-frame))
          (should (equal passed (edmacs-sessions--gui-frame-parameters))))))

    (ert-deftest edmacs-sessions-test-make-gui-frame-warns-instead-of-signalling ()
      "Never signal: an error at a frameless daemon's top level exits it 255
\(see core.el). The `ignore-errors' this replaced made the failure
invisible as well as survivable, which is how a boot frame deleted out
from under the daemon went unnoticed for so long."
      (let ((warnings nil))
        (cl-letf (((symbol-function 'make-frame)
                   (lambda (&rest _) (error "no window system")))
                  ((symbol-function 'display-warning)
                   (lambda (&rest args) (push args warnings))))
          (should-not (edmacs-sessions--make-gui-frame))
          (should (= 1 (length warnings)))
          (should (eq (car (car warnings)) 'edmacs-sessions)))))

    ;; ==========================================================================
    ;; AC1/AC2 -- edmacs-sessions--ensure-sidebar / --finish-frameset-restore
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-ensure-sidebar-always-shows-even-with-window ()
      "A stale sidebar buffer -- mis-named because `edmacs-sidebar--on-
desktop-read' (sidebar.el) shows every frame's sidebar synchronously at
desktop-read time, before this frame's title has been regenerated from
its `edmacs-repo' -- is only ever fixed by `edmacs-sidebar-show' (via
`edmacs-sidebar--ensure-buffer''s rename-if-stale check). Skipping the
call whenever a window already exists (the old behavior) meant that fix
never ran for an already-windowed frame; reproduced live via a real
multi-frame daemon restart, where such a frame's sidebar buffer kept
the wrong name forever."
      (let ((show-calls nil))
        (cl-letf (((symbol-function 'edmacs-sidebar--window) (lambda (_) 'a-window))
                  ((symbol-function 'edmacs-sidebar-show) (lambda (f) (push f show-calls))))
          (edmacs-sessions--ensure-sidebar (selected-frame))
          (should (equal show-calls (list (selected-frame)))))))

    (ert-deftest edmacs-sessions-test-ensure-sidebar-shows-frame-without-window ()
      (let ((show-calls nil))
        (cl-letf (((symbol-function 'edmacs-sidebar--window) (lambda (_) nil))
                  ((symbol-function 'edmacs-sidebar-show) (lambda (f) (push f show-calls))))
          (edmacs-sessions--ensure-sidebar (selected-frame))
          (should (equal show-calls (list (selected-frame)))))))

    (ert-deftest edmacs-sessions-test-finish-restore-shows-sidebar-for-every-live-frame ()
      "Every live frame gets `edmacs-sidebar-show' called on it, regardless
of whether `edmacs-sidebar--window' already finds one -- see
`edmacs-sessions-test-ensure-sidebar-always-shows-even-with-window' for
why skipping an already-windowed frame is wrong."
      (let* ((f1 (selected-frame))
             (f2 (edmacs-sessions-test--make-second-frame-or-skip))
             (show-calls nil))
        (unwind-protect
            (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) nil))
                      ((symbol-function 'edmacs-frames--tab-root) (lambda (_) nil))
                      ((symbol-function 'edmacs-frames--repo-of) (lambda (_) nil))
                      ((symbol-function 'edmacs-frames-frame-usable-p) (lambda (_) t))
                      ((symbol-function 'edmacs-frames-stamp-frame-tabs) #'ignore)
                      ((symbol-function 'edmacs-sidebar--window)
                       (lambda (frame) (if (eq frame f2) 'has-window nil)))
                      ((symbol-function 'edmacs-sidebar-show)
                       (lambda (frame) (push frame show-calls))))
              (edmacs-sessions--finish-frameset-restore)
              (should (member f1 show-calls))
              (should (member f2 show-calls)))
          (when (frame-live-p f2) (delete-frame f2)))))

    (ert-deftest edmacs-sessions-test-finish-restore-tracks-worktrees-for-every-repo-frame ()
      "Every live frame carrying a resolvable `edmacs-repo' after backfill
gets its worktree cache/watch warmed, not just its sidebar shown --
otherwise a restored frame's sidebar renders an empty worktree list
until some unrelated event happens to trigger a refresh."
      (let* ((f1 (selected-frame))
             (f2 (edmacs-sessions-test--make-second-frame-or-skip))
             (dir1 (file-name-as-directory (make-temp-file "edmacs-sessions-test-f1-" t)))
             (tracked nil))
        (unwind-protect
            (edmacs-sessions-test--with-clean-frame-params f1 '(edmacs-repo)
              (set-frame-parameter f1 'edmacs-repo dir1)
              (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) nil))
                        ((symbol-function 'edmacs-frames--tab-root) (lambda (_) nil))
                        ((symbol-function 'edmacs-frames--repo-of) (lambda (_) nil))
                        ((symbol-function 'edmacs-frames-frame-usable-p) (lambda (_) t))
                        ((symbol-function 'edmacs-frames-stamp-frame-tabs) #'ignore)
                        ((symbol-function 'edmacs-sidebar--window) (lambda (_) t))
                        ((symbol-function 'edmacs-sidebar-show) (lambda (_) nil))
                        ((symbol-function 'edmacs-frames--ensure-repo-tracking)
                         (lambda (common) (push common tracked))))
                (edmacs-sessions--finish-frameset-restore)
                (should (member dir1 tracked))
                (should (= 1 (length tracked)))))
          (delete-directory dir1 t)
          (when (frame-live-p f2) (delete-frame f2)))))

    (ert-deftest edmacs-sessions-test-finish-restore-never-calls-make-frame ()
      "`frameset-restore's own `:reuse-frames t' owns all frame creation/reuse;
this orchestrator only mutates already-live frames."
      (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) nil))
                ((symbol-function 'edmacs-frames--tab-root) (lambda (_) nil))
                ((symbol-function 'edmacs-frames--repo-of) (lambda (_) nil))
                ((symbol-function 'edmacs-frames-frame-usable-p) (lambda (_) t))
                ((symbol-function 'edmacs-frames-stamp-frame-tabs) #'ignore)
                ((symbol-function 'edmacs-sidebar--window) (lambda (_) t))
                ((symbol-function 'edmacs-sidebar-show) (lambda (_) nil))
                ((symbol-function 'make-frame)
                 (lambda (&rest _)
                   (error "edmacs-sessions--finish-frameset-restore must never call make-frame"))))
        (edmacs-sessions--finish-frameset-restore)))

    ;; ============================================================================
    ;; edmacs-quit -- close the frame under a daemon, kill the terminal otherwise
    ;; ============================================================================

    (ert-deftest edmacs-sessions-test-quit-closes-the-frame-under-a-daemon ()
      "`SPC q q' must not end the process under launchd: an exit is relaunched,
    so quitting means closing the frame and leaving the session up."
      (let (closed killed)
        (cl-letf (((symbol-function 'daemonp) (lambda () t))
                  ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'edmacs-ns-close-frame)
                   (lambda (&rest _) (setq closed t)))
                  ((symbol-function 'save-buffers-kill-terminal)
                   (lambda (&rest _) (setq killed t))))
          (edmacs-quit)
          (should closed)
          (should-not killed))))

    (ert-deftest edmacs-sessions-test-quit-kills-terminal-outside-a-daemon ()
      "Without a daemon there is no relaunch to work around, so the stock
    behavior is the correct one."
      (let (closed killed)
        (cl-letf (((symbol-function 'daemonp) (lambda () nil))
                  ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'edmacs-ns-close-frame)
                   (lambda (&rest _) (setq closed t)))
                  ((symbol-function 'save-buffers-kill-terminal)
                   (lambda (&rest _) (setq killed t))))
          (edmacs-quit)
          (should killed)
          (should-not closed))))

    (ert-deftest edmacs-sessions-test-quit-kills-terminal-on-a-tty-daemon-frame ()
      "A non-graphical daemon frame has no Dock tile to keep owned, so the
    hide-the-last-frame dance does not apply to it."
      (let (closed killed)
        (cl-letf (((symbol-function 'daemonp) (lambda () t))
                  ((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
                  ((symbol-function 'edmacs-ns-close-frame)
                   (lambda (&rest _) (setq closed t)))
                  ((symbol-function 'save-buffers-kill-terminal)
                   (lambda (&rest _) (setq killed t))))
          (edmacs-quit)
          (should killed)
          (should-not closed))))

    ;; ============================================================================
    ;; edmacs-restart-daemon -- exiting IS the restart under KeepAlive
    ;; ============================================================================

    (ert-deftest edmacs-sessions-test-restart-daemon-saves-then-exits ()
      "Under a daemon the restart is `kill-emacs' plus launchd, and buffers are
    offered for saving BEFORE the exit -- `desktop-save-mode' writes the layout
    from `kill-emacs-hook' on the way out, but modified buffers are not its job."
      (let (saved exited restarted)
        (cl-letf (((symbol-function 'daemonp) (lambda () t))
                  ((symbol-function 'save-some-buffers)
                   (lambda (&rest _) (setq saved t)))
                  ((symbol-function 'kill-emacs)
                   (lambda (&rest _) (should saved) (setq exited t)))
                  ((symbol-function 'restart-emacs)
                   (lambda (&rest _) (setq restarted t))))
          (edmacs-restart-daemon)
          (should saved)
          (should exited)
          (should-not restarted))))

    (ert-deftest edmacs-sessions-test-restart-daemon-passes-prefix-to-save-some-buffers ()
      "A prefix arg reaches `save-some-buffers' as its ARG, so `C-u SPC q r'
    saves every modified buffer without prompting."
      (let (save-arg)
        (cl-letf (((symbol-function 'daemonp) (lambda () t))
                  ((symbol-function 'save-some-buffers)
                   (lambda (&optional arg &rest _) (setq save-arg arg)))
                  ((symbol-function 'kill-emacs) (lambda (&rest _) nil)))
          (edmacs-restart-daemon t)
          (should (eq save-arg t)))))

    (ert-deftest edmacs-sessions-test-restart-daemon-defers-to-restart-emacs-otherwise ()
      "Outside a daemon nothing relaunches the process, so exiting would be a
    quit, not a restart -- `restart-emacs' spawns the replacement itself."
      (let (exited restarted)
        (cl-letf (((symbol-function 'daemonp) (lambda () nil))
                  ((symbol-function 'kill-emacs)
                   (lambda (&rest _) (setq exited t)))
                  ((symbol-function 'restart-emacs)
                   (lambda (&rest _) (setq restarted t))))
          (edmacs-restart-daemon)
          (should restarted)
          (should-not exited))))

    ;; ============================================================================
    ;; edmacs-stop-daemon -- the only one that really quits
    ;; ============================================================================

    (ert-deftest edmacs-sessions-test-stop-daemon-runs-brew-services-stop-detached ()
      "DESTINATION 0 is load-bearing: `brew' has to outlive the Emacs it is
    killing, which a process Emacs tracks would not, so assert the argument
    rather than only the command line."
      (let (call)
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/opt/homebrew/bin/brew"))
                  ((symbol-function 'file-executable-p) (lambda (&rest _) t))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
                  ((symbol-function 'call-process)
                   (lambda (&rest args) (setq call args) 0)))
          (let ((edmacs-sessions-launchd-service "emacs-plus@31"))
            (edmacs-stop-daemon))
          (should call)
          (should (equal (nth 0 call) "/opt/homebrew/bin/brew"))
          (should (equal (nth 2 call) 0))
          (should (equal (nthcdr 4 call) '("services" "stop" "emacs-plus@31"))))))

    (ert-deftest edmacs-sessions-test-stop-daemon-honors-the-service-variable ()
      (let (call)
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/opt/homebrew/bin/brew"))
                  ((symbol-function 'file-executable-p) (lambda (&rest _) t))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
                  ((symbol-function 'call-process)
                   (lambda (&rest args) (setq call args) 0)))
          (let ((edmacs-sessions-launchd-service "emacs-plus@42"))
            (edmacs-stop-daemon))
          (should (equal (nthcdr 4 call) '("services" "stop" "emacs-plus@42"))))))

    (ert-deftest edmacs-sessions-test-stop-daemon-declining-runs-nothing ()
      "Answering no leaves the service alone AND leaves buffers unsaved -- the
    confirmation gates the whole command, not just the `brew' call."
      (let (call saved)
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/opt/homebrew/bin/brew"))
                  ((symbol-function 'file-executable-p) (lambda (&rest _) t))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                  ((symbol-function 'save-some-buffers) (lambda (&rest _) (setq saved t)))
                  ((symbol-function 'call-process)
                   (lambda (&rest args) (setq call args) 0)))
          (edmacs-stop-daemon)
          (should-not call)
          (should-not saved))))

    (ert-deftest edmacs-sessions-test-stop-daemon-errors-without-brew ()
      "Fails loudly rather than silently doing nothing when `brew' is absent --
    the user asked for the daemon to stay down and has to know it will not."
      (let (call)
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
                  ((symbol-function 'file-executable-p) (lambda (&rest _) nil))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'call-process)
                   (lambda (&rest args) (setq call args) 0)))
          (should-error (edmacs-stop-daemon) :type 'user-error)
          (should-not call))))))

;;; sessions-test.el ends here
