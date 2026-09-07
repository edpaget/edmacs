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
;;         -l modules/workspaces.el -l modules/sessions-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; `workspaces.el' is on that line because the daemon stash hook now puts
;; `desktop-saved-frameset' through `edmacs-workspaces-migrate-frameset'
;; before stashing it; without it two tests here would exercise a stub of
;; their own subject.
;;
;; Every test runs under plain `-Q --batch' -- 52/52, zero skipped, no
;; controlling terminal required. The second-frame skip path this suite
;; used to carry went with the per-frame restore walk.
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
;; The restore finish-up's own eligibility gate and call ORDER are what
;; this file pins -- not what a restored tab ends up carrying.
;; `modules/sessions-live-test.el' covers that end to end, driving a real
;; `desktop-restore-frameset'.
;;
;; sidebar.el is not loaded here (`edmacs-sidebar-show'/`--window' are
;; stubbed via `cl-letf'). workspaces.el IS loaded, so
;; `edmacs-workspaces-frame-usable-p'/`-stamp-frame-tabs'/
;; `-current-tab-root' are the real functions except where a test
;; deliberately stubs one to isolate sessions.el's own logic.

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

    (defmacro edmacs-sessions-test--with-clean-frame-params (frame params &rest body)
      "Run BODY, then reset each of FRAME's PARAMS to nil afterward."
      (declare (indent 2))
      `(unwind-protect
           (progn ,@body)
         (dolist (p ,params) (set-frame-parameter ,frame p nil))))

    ;; ==========================================================================
    ;; The restore finish-up's own eligibility gate
    ;; ==========================================================================

    (defmacro edmacs-sessions-test--with-stubbed-steps (calls &rest body)
      "Run BODY with each restore step recording into CALLS.
Each entry is (STEP . FRAME), pushed in call order. workspaces.el's own
stamper is stubbed here alongside sessions.el's sidebar step so the
order this function fixes is what is asserted, not either callee's
behaviour."
      (declare (indent 1))
      `(cl-letf (((symbol-function 'edmacs-workspaces-stamp-frame-tabs)
                  (lambda (f) (push (cons 'stamp f) ,calls)))
                 ((symbol-function 'edmacs-sessions--ensure-sidebar)
                  (lambda (f) (push (cons 'sidebar f) ,calls))))
         ,@body))

    (ert-deftest edmacs-sessions-test-finish-frameset-restore-declines-unusable-frame ()
      "The daemon's initial tty frame is in `frame-list' but in no desktop
save, and it must never have its tabs stamped or be given a sidebar --
observed live as a second frame named after a repo it could never
display. Handed that frame explicitly, the
finish-up declines it and does no per-frame work at all; it must not
fall back to scanning for some other frame either, or an explicit
argument would stop meaning anything."
      (let (calls)
        (cl-letf (((symbol-function 'frame-live-p) (lambda (f) (memq f '(f1 gui))))
                  ((symbol-function 'daemonp) (lambda (&rest _) t))
                  ((symbol-function 'display-graphic-p)
                   (lambda (&optional f) (eq f 'gui)))
                  ((symbol-function 'frame-initial-p) (lambda (f) (eq f 'f1)))
                  ;; `edmacs-workspaces-frame-usable-p''s own shape;
                  ;; workspaces-test.el covers the real predicate directly.
                  ((symbol-function 'edmacs-workspaces-frame-usable-p)
                   (lambda (f) (and (not (and (daemonp) (frame-initial-p f)))
                                    (display-graphic-p f))))
                  ((symbol-function 'edmacs-sessions--gui-frame)
                   (lambda () (ert-fail "an explicit FRAME must not be second-guessed"))))
          (edmacs-sessions-test--with-stubbed-steps calls
            (edmacs-sessions--finish-frameset-restore 'f1)))
        (should-not calls)))

    (ert-deftest edmacs-sessions-test-finish-frameset-restore-runs-every-step-in-order ()
      "The frame it was given is processed in full, tabs stamped BEFORE the
sidebar is shown: the sidebar files a tab under a project by that tab's
own stamped root, so a sidebar drawn first would render a restored tab
under no project at all.

There is deliberately no dead-root sweep here. Under the frames model a
tab whose worktree directory was gone had its stamp cleared, so a live
root could be re-derived for the frame's one repo; under groups the
stamp IS the tab's identity, and clearing it would drop the tab out of
its project's tree rather than mark it missing."
      (let (calls)
        (cl-letf (((symbol-function 'frame-live-p) (lambda (f) (memq f '(f1 gui))))
                  ((symbol-function 'edmacs-workspaces-frame-usable-p)
                   (lambda (f) (eq f 'gui))))
          (edmacs-sessions-test--with-stubbed-steps calls
            (edmacs-sessions--finish-frameset-restore 'gui)))
        (setq calls (nreverse calls))
        (should (seq-every-p (lambda (c) (eq (cdr c) 'gui)) calls))
        (should (equal (mapcar #'car calls) '(stamp sidebar)))))

    (ert-deftest edmacs-sessions-test-finish-frameset-restore-falls-back-to-the-gui-frame ()
      "With no FRAME -- and with no GUI frame at all -- it does nothing
rather than guessing. The fallback matters because
`edmacs-sessions--ensure-gui-frame' may have had to create a replacement
for a frame `frameset-restore' deleted, and the caller then has only a
dead frame to hand over."
      (let (calls)
        (cl-letf (((symbol-function 'frame-live-p) (lambda (_f) t))
                  ((symbol-function 'edmacs-workspaces-frame-usable-p) (lambda (_f) t))
                  ((symbol-function 'edmacs-sessions--gui-frame) (lambda () 'replacement)))
          (edmacs-sessions-test--with-stubbed-steps calls
            (edmacs-sessions--finish-frameset-restore nil)))
        (should (seq-every-p (lambda (c) (eq (cdr c) 'replacement)) calls))
        (should (= 2 (length calls))))
      (let (calls)
        (cl-letf (((symbol-function 'edmacs-sessions--gui-frame) (lambda () nil)))
          (edmacs-sessions-test--with-stubbed-steps calls
            (edmacs-sessions--finish-frameset-restore nil)))
        (should-not calls)))

    (ert-deftest edmacs-sessions-test-gui-frame-finds-only-a-live-graphic-frame ()
      "`edmacs-sessions--gui-frame' is the one place that answers \"which
frame is the session's\", shared by the finish-up's fallback and
`edmacs-sessions--ensure-gui-frame' -- so the two cannot drift apart on
what counts."
      (cl-letf (((symbol-function 'frame-list) (lambda () '(dead tty gui)))
                ((symbol-function 'frame-live-p) (lambda (f) (memq f '(tty gui))))
                ((symbol-function 'display-graphic-p) (lambda (&optional f) (eq f 'gui))))
        (should (eq (edmacs-sessions--gui-frame) 'gui)))
      (cl-letf (((symbol-function 'frame-list) (lambda () '(tty)))
                ((symbol-function 'frame-live-p) (lambda (_f) t))
                ((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
        (should-not (edmacs-sessions--gui-frame))))

    ;; ==========================================================================
    ;; edmacs-sessions--tab-name
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-tab-name-reads-the-tabs-own-stamped-root ()
      "The regression the old, buffer-derived namer produced: a tab whose
frame correctly showed one worktree was named after some OTHER buffer
that happened to be current when tab-bar recomputed the name (core
re-runs `tab-bar-tab-name-function' on every `tab-bar-tabs' read of an
unrenamed tab, not just at creation). Reading the tab's OWN stamped
root cannot depend on ambient state, which is what this pins: an
unrelated buffer is deliberately current throughout."
      (let* ((frame (selected-frame))
             (saved (frame-parameter frame 'tabs))
             (ambient (generate-new-buffer "edmacs-sessions-test-ambient")))
        (unwind-protect
            (with-current-buffer ambient
              (setq default-directory "/some/other/place/")
              (set-frame-parameter
               frame 'tabs
               (list (list 'current-tab
                           (cons 'edmacs-workspace-root "/repo__worktrees/roadmap-x/"))))
              (should (equal (edmacs-sessions--tab-name) "roadmap-x")))
          (set-frame-parameter frame 'tabs saved)
          (kill-buffer ambient))))

    (ert-deftest edmacs-sessions-test-tab-name-falls-back-on-an-unstamped-tab ()
      "The daemon's boot tab and batch's own carry no root; core's
`tab-bar-tab-name-current' is the documented answer there."
      (let* ((frame (selected-frame))
             (saved (frame-parameter frame 'tabs)))
        (unwind-protect
            (progn
              (set-frame-parameter frame 'tabs (list (list 'current-tab)))
              (cl-letf (((symbol-function 'tab-bar-tab-name-current)
                         (lambda () "core-fallback")))
                (should (equal (edmacs-sessions--tab-name) "core-fallback"))))
          (set-frame-parameter frame 'tabs saved))))

    (ert-deftest edmacs-sessions-test-tab-name-is-the-tab-bar-name-function ()
      (should (eq tab-bar-tab-name-function #'edmacs-sessions--tab-name)))

    ;; ==========================================================================
    ;; Frame position (`left'/`top') is deliberately NOT restored
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-frameset-filter-drops-frame-position ()
      "Supersedes the earlier AC1, which wanted saved positions honoured.
Every graphical frame now opens fullscreen (workspaces.el's
`edmacs-workspaces-fullscreen'), so there is no position worth replaying --
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
      "The guard's positive case. The migration is stubbed to identity here
so this stays about the guard; the handoff to the real one is pinned by
`edmacs-sessions-test-stash-migrates-the-frameset'."
      (let* ((fs (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))
             (edmacs-sessions--pending-frameset nil)
             (desktop-saved-frameset fs))
        (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
                  ((symbol-function 'desktop-restoring-frameset-p) (lambda () nil))
                  ((symbol-function 'edmacs-workspaces-migrate-frameset) #'identity))
          (edmacs-sessions--stash-frameset-for-daemon)
          (should (eq edmacs-sessions--pending-frameset fs)))))

    (ert-deftest edmacs-sessions-test-stash-migrates-the-frameset ()
      "What the daemon stashes is the MIGRATED frameset: one frame state
holding every project as a tab group. That is the whole mechanism behind
\"a restart produces exactly one GUI frame\" -- `frameset-restore' reuses
the boot frame and never creates a second one, rather than a second one
being created and then deleted."
      (let* ((fs (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))
             (migrated (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))
             (seen nil)
             (edmacs-sessions--pending-frameset nil)
             (desktop-saved-frameset fs))
        (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
                  ((symbol-function 'desktop-restoring-frameset-p) (lambda () nil))
                  ((symbol-function 'edmacs-workspaces-migrate-frameset)
                   (lambda (arg) (setq seen arg) migrated)))
          (edmacs-sessions--stash-frameset-for-daemon)
          (should (eq seen fs))
          (should (eq edmacs-sessions--pending-frameset migrated)))))

    (ert-deftest edmacs-sessions-test-stash-survives-a-signalling-migration ()
      "An error out of `desktop-after-read-hook' in a frameless daemon
reaches top level and exits Emacs 255 (see core.el). A failing migration
must therefore warn and stash the frameset unmigrated -- degraded, two
frames, but never nil and never frameless."
      (let* ((fs (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))
             (warnings nil)
             (edmacs-sessions--pending-frameset nil)
             (desktop-saved-frameset fs))
        (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
                  ((symbol-function 'desktop-restoring-frameset-p) (lambda () nil))
                  ((symbol-function 'edmacs-workspaces-migrate-frameset)
                   (lambda (_arg) (error "boom")))
                  ((symbol-function 'display-warning)
                   (lambda (_type msg &rest _) (push msg warnings))))
          (edmacs-sessions--stash-frameset-for-daemon)
          (should (eq edmacs-sessions--pending-frameset fs))
          (should (= 1 (length warnings)))
          (should (string-match-p "boom" (car warnings))))))

    (ert-deftest edmacs-sessions-test-stash-uses-the-real-migration ()
      "The real `edmacs-workspaces-migrate-frameset' is what the hook calls,
not a stub of it -- so `workspaces.el' belongs on this suite's own
invocation line (see Commentary). Two frame states in, one out."
      (let* ((fs (edmacs-sessions-test--frameset
                  (list (cons '((last-focus-update . t)
                                (tabs (current-tab (name . "a"))))
                              nil)
                        (cons '((name . "b") (tabs (current-tab (name . "b"))))
                              nil))))
             (edmacs-sessions--pending-frameset nil)
             (desktop-saved-frameset fs))
        (unless (fboundp 'edmacs-workspaces-migrate-frameset)
          (ert-skip "workspaces.el is not loaded; add -l modules/workspaces.el"))
        (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
                  ((symbol-function 'desktop-restoring-frameset-p) (lambda () nil)))
          (edmacs-sessions--stash-frameset-for-daemon)
          (should (= 1 (length (frameset-states edmacs-sessions--pending-frameset))))
          (should (= 2 (length (alist-get 'tabs (car (car (frameset-states
                                                           edmacs-sessions--pending-frameset))))))))))

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

    (defvar edmacs-sessions-test--finish-frame 'unset
      "The FRAME argument the restore timer passed to the finish-up step.
Recorded out of band by `edmacs-sessions-test--run-restore-timer' so the
step ORDER that helper returns stays a flat list of symbols.")

    (defun edmacs-sessions-test--run-restore-timer (restore-fn)
      "Drive the deferred body `edmacs-sessions--restore-pending-frameset\='
schedules, with RESTORE-FN standing in for `desktop-restore-frameset\='.
Returns (ORDER . WARNINGS): the sequence of steps the timer reached and
any `display-warning\=' text it produced. RESTORE-FN is called with the
frame handed to the sweep so a stub can simulate `frameset-restore\='
deleting it."
      (setq edmacs-sessions-test--finish-frame 'unset)
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
                   (lambda (&optional frame)
                     (push 'finish order)
                     (setq edmacs-sessions-test--finish-frame frame)))
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
        (should-not (cdr result))
        ;; The frame the restore landed on is handed over explicitly, so the
        ;; finish-up never has to guess which frame it is finishing.
        (should (eq edmacs-sessions-test--finish-frame (selected-frame)))))

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
        (should-not (cdr result))
        ;; A dead frame is passed as nil, not passed on: the finish-up then
        ;; falls back to whatever frame `--ensure-gui-frame' left behind.
        (should-not edmacs-sessions-test--finish-frame)))

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
desktop-read time, before this frame's tabs have been stamped -- is only
ever fixed by `edmacs-sidebar-show' (via
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

    (ert-deftest edmacs-sessions-test-finish-restore-shows-sidebar-for-the-frame-it-was-given ()
      "The frame handed over gets `edmacs-sidebar-show' called on it,
regardless of whether `edmacs-sidebar--window' already finds one -- see
`edmacs-sessions-test-ensure-sidebar-always-shows-even-with-window' for
why skipping an already-windowed frame is wrong. No second frame is
needed to pin this any more: the frameset the daemon replays is migrated
to a single state, so there is one frame to finish."
      (let ((frame (selected-frame))
            (show-calls nil))
        (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) nil))
                  ((symbol-function 'edmacs-workspaces-frame-usable-p) (lambda (_) t))
                  ((symbol-function 'edmacs-workspaces-stamp-frame-tabs) #'ignore)
                  ((symbol-function 'edmacs-sidebar--window) (lambda (_) 'has-window))
                  ((symbol-function 'edmacs-sidebar-show)
                   (lambda (f) (push f show-calls))))
          (edmacs-sessions--finish-frameset-restore frame)
          (should (equal show-calls (list frame))))))

    (ert-deftest edmacs-sessions-test-finish-restore-never-calls-make-frame ()
      "`frameset-restore's own `:reuse-frames t' owns all frame creation/reuse;
this orchestrator only mutates the already-live frame it is handed."
      (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) nil))
                ((symbol-function 'edmacs-workspaces-frame-usable-p) (lambda (_) t))
                ((symbol-function 'edmacs-workspaces-stamp-frame-tabs) #'ignore)
                ((symbol-function 'edmacs-sidebar--window) (lambda (_) t))
                ((symbol-function 'edmacs-sidebar-show) (lambda (_) nil))
                ((symbol-function 'make-frame)
                 (lambda (&rest _)
                   (error "edmacs-sessions--finish-frameset-restore must never call make-frame"))))
        (edmacs-sessions--finish-frameset-restore (selected-frame))))

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
          (should-not call))))

    ;; ============================================================================
    ;; edmacs-ns-close-frame -- the hide-vs-delete branch itself.
    ;; Live-verified 2026-09-04 against a real `--fg-daemon' (see the
    ;; daemon-and-Dock-frame task's live verification log for the NS-level
    ;; hide/lsappinfo transition an ERT batch run cannot exercise); these
    ;; pin the pure branch logic so a regression there fails in CI, not just
    ;; on a manual pass.
    ;; ============================================================================

    (unless (fboundp 'ns-do-hide-emacs)
      (defun ns-do-hide-emacs ()
        "Stub for a non-NS batch build: the real primitive is NS-only."))

    (ert-deftest edmacs-ns-close-frame-test-hides-the-last-visible-graphic-frame ()
      "Under a daemon, closing the sole visible graphic frame hides Emacs
instead of deleting it -- a deleted last frame drops the Dock tile."
      (let (hidden deleted (frame 'the-frame))
        (cl-letf (((symbol-function 'daemonp) (lambda () t))
                  ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-visible-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-list) (lambda () (list frame)))
                  ((symbol-function 'ns-do-hide-emacs) (lambda () (setq hidden t)))
                  ((symbol-function 'delete-frame) (lambda (&rest _) (setq deleted t))))
          (edmacs-ns-close-frame frame)
          (should hidden)
          (should-not deleted))))

    (ert-deftest edmacs-ns-close-frame-test-deletes-when-another-graphic-frame-remains ()
      "A second visible graphic frame means this one isn't the Dock tile's
last hold on life, so the ordinary `delete-frame' applies."
      (let (hidden deleted (frame 'the-frame))
        (cl-letf (((symbol-function 'daemonp) (lambda () t))
                  ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-visible-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-list) (lambda () (list frame 'other-frame)))
                  ((symbol-function 'ns-do-hide-emacs) (lambda () (setq hidden t)))
                  ((symbol-function 'delete-frame) (lambda (&rest _) (setq deleted t))))
          (edmacs-ns-close-frame frame)
          (should deleted)
          (should-not hidden))))

    (ert-deftest edmacs-ns-close-frame-test-deletes-outside-a-daemon ()
      "No daemon means no Dock tile to protect, so hide never applies."
      (let (hidden deleted (frame 'the-frame))
        (cl-letf (((symbol-function 'daemonp) (lambda () nil))
                  ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-visible-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-list) (lambda () (list frame)))
                  ((symbol-function 'ns-do-hide-emacs) (lambda () (setq hidden t)))
                  ((symbol-function 'delete-frame) (lambda (&rest _) (setq deleted t))))
          (edmacs-ns-close-frame frame)
          (should deleted)
          (should-not hidden))))

    (ert-deftest edmacs-ns-close-frame-test-deletes-a-non-graphic-frame ()
      "A tty frame has no Dock tile either, even under a daemon."
      (let (hidden deleted (frame 'the-frame))
        (cl-letf (((symbol-function 'daemonp) (lambda () t))
                  ((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
                  ((symbol-function 'frame-visible-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-list) (lambda () (list frame)))
                  ((symbol-function 'ns-do-hide-emacs) (lambda () (setq hidden t)))
                  ((symbol-function 'delete-frame) (lambda (&rest _) (setq deleted t))))
          (edmacs-ns-close-frame frame)
          (should deleted)
          (should-not hidden))))

    ;; ============================================================================
    ;; edmacs-sessions--install-macos-close-frame-bindings -- extracted so it is
    ;; directly callable regardless of the ambient `daemonp', which is nil in
    ;; every test's batch load environment.
    ;; ============================================================================

    (ert-deftest edmacs-sessions-test-install-macos-close-frame-bindings ()
      "Installs the close-button and remapped-delete-frame bindings."
      (let ((prior-remap (lookup-key global-map [remap delete-frame]))
            (prior-special (lookup-key special-event-map [delete-frame]))
            (emacs-startup-hook nil))
        (unwind-protect
            (progn
              (edmacs-sessions--install-macos-close-frame-bindings)
              (should (eq (lookup-key global-map [remap delete-frame])
                          #'edmacs-ns-close-frame))
              (should (eq (lookup-key special-event-map [delete-frame])
                          #'edmacs-ns-handle-delete-frame))
              (should (memq #'edmacs-sessions--ensure-gui-frame emacs-startup-hook))
              (should (memq #'edmacs-sessions--warn-on-shadow-daemon-process
                            emacs-startup-hook)))
          (define-key global-map [remap delete-frame] prior-remap)
          (define-key special-event-map [delete-frame] prior-special))))

    ;; ============================================================================
    ;; edmacs-sessions--warn-on-shadow-daemon-process -- detection-only, never
    ;; touches the other process.
    ;; ============================================================================

    (ert-deftest edmacs-sessions-test-shadow-daemon-warns-when-found ()
      "Surfaces a non-fatal warning naming the other PID(s) it found."
      (let (warned)
        (cl-letf (((symbol-function 'edmacs-sessions--shadow-daemon-processes)
                   (lambda () '("4242")))
                  ((symbol-function 'display-warning)
                   (lambda (type message &rest _)
                     (setq warned (cons type message)))))
          (edmacs-sessions--warn-on-shadow-daemon-process)
          (should warned)
          (should (eq (car warned) 'edmacs-sessions))
          (should (string-match-p "4242" (cdr warned))))))

    (ert-deftest edmacs-sessions-test-shadow-daemon-silent-when-alone ()
      "No other process sharing the executable means no warning at all."
      (let (warned)
        (cl-letf (((symbol-function 'edmacs-sessions--shadow-daemon-processes)
                   (lambda () nil))
                  ((symbol-function 'display-warning)
                   (lambda (&rest _) (setq warned t))))
          (edmacs-sessions--warn-on-shadow-daemon-process)
          (should-not warned))))

    ;; ============================================================================
    ;; edmacs-sessions--shadow-daemon-processes -- only a bare-argv (Dock/Finder)
    ;; launch counts. This repo's own workflow runs `emacs --batch' off the very
    ;; same binary all day, so matching the executable alone would fire on every
    ;; ERT run, `batch-byte-compile' and scripts/startup-check.sh.
    ;; ============================================================================

    (defconst edmacs-sessions-test--shadow-exe
      "/opt/homebrew/Cellar/emacs-plus@31/31.1/Emacs.app/Contents/MacOS/Emacs")

    (defun edmacs-sessions-test--shadow-pids (ps-output)
      "Run the shadow probe against synthetic PS-OUTPUT, as pid 100."
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "/bin/ps"))
                ((symbol-function 'emacs-pid) (lambda () 100))
                ((symbol-function 'call-process)
                 (lambda (_program &optional _infile _dest _display &rest _args)
                   (insert ps-output)
                   0)))
        (let ((invocation-name "Emacs")
              (invocation-directory
               (file-name-directory edmacs-sessions-test--shadow-exe)))
          (edmacs-sessions--shadow-daemon-processes))))

    (ert-deftest edmacs-sessions-test-shadow-daemon-finds-bare-launch ()
      "A no-argument launch of the same bundle executable is the shadow case."
      (should (equal (edmacs-sessions-test--shadow-pids
                      (format "  100 %s --fg-daemon\n 2023 %s\n"
                              edmacs-sessions-test--shadow-exe
                              edmacs-sessions-test--shadow-exe))
                     '("2023"))))

    (ert-deftest edmacs-sessions-test-shadow-daemon-ignores-batch-processes ()
      "Batch runs of the same binary are this repo's normal workflow, not shadows.
`emacs --batch' ERT suites, `batch-byte-compile' and
scripts/startup-check.sh all exec the daemon's own executable, and all
carry arguments -- unlike a Dock launch."
      (should-not (edmacs-sessions-test--shadow-pids
                   (format (concat "  100 %s --fg-daemon\n"
                                   " 2001 %s -Q --batch -l ert -l modules/sessions-test.el\n"
                                   " 2002 %s -Q --batch -f batch-byte-compile modules/ui.el\n"
                                   " 2003 %s --batch --init-directory=/Users/e/Projects/edmacs\n"
                                   " 2004 %s --fg-daemon\n")
                           edmacs-sessions-test--shadow-exe
                           edmacs-sessions-test--shadow-exe
                           edmacs-sessions-test--shadow-exe
                           edmacs-sessions-test--shadow-exe
                           edmacs-sessions-test--shadow-exe))))

    (ert-deftest edmacs-sessions-test-shadow-daemon-ignores-self-and-others ()
      "Skips its own PID and any process running a different executable."
      (should-not (edmacs-sessions-test--shadow-pids
                   (format (concat "  100 %s\n"
                                   " 3001 /Applications/Emacs.app/Contents/MacOS/Emacs\n"
                                   " 3002 /opt/homebrew/bin/emacsclient -c\n")
                           edmacs-sessions-test--shadow-exe))))

    (ert-deftest edmacs-sessions-test-shadow-daemon-without-ps ()
      "No `ps' on PATH means no probe and no error."
      (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
        (should-not (edmacs-sessions--shadow-daemon-processes))))

    (ert-deftest edmacs-sessions-test-shadow-daemon-errors-swallowed ()
      "A failure probing for other processes must not escape as an error --
    this runs from `emacs-startup-hook' and must never block boot."
      (let (warned)
        (cl-letf (((symbol-function 'edmacs-sessions--shadow-daemon-processes)
                   (lambda () (error "ps exploded")))
                  ((symbol-function 'display-warning)
                   (lambda (&rest _) (setq warned t))))
          (edmacs-sessions--warn-on-shadow-daemon-process)
          (should-not warned))))

    ;; ==========================================================================
    ;; sessions.el's own Commentary and the sole GUI-frame maker
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-commentary-does-not-relitigate-frames ()
      "The Commentary must not carry the old tabs-over-frames rationale
back: that argument was retired once upstream fixed the bugs it rested
on, and the model is now one frame regardless. What it must record is
the constraint that actually survives -- ghostel's PTY sizing."
      (let ((text (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "modules/sessions.el" default-directory))
                    (buffer-string))))
        (should-not (string-match-p "per-frame is where manzaltu" text))
        (should-not (string-match-p "explicitly tab-aware" text))
        (should (string-match-p "window-adjust-process-window-size-smallest" text))))

    (ert-deftest edmacs-sessions-test-sole-gui-frame-maker-documents-the-hazard ()
      "The config's one frame-creation path, and the docstring saying why
it cannot be removed -- naming both mechanisms that make a frameless
daemon self-perpetuating."
      (should (fboundp 'edmacs-sessions--make-gui-frame))
      (let ((doc (documentation 'edmacs-sessions--ensure-gui-frame)))
        (should doc)
        (should (string-match-p "cannot be removed" doc))
        (should (string-match-p "EMPTY frameset" doc))
        (should (string-match-p "frameset-restore" doc)))
      (let ((text (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "modules/sessions.el" default-directory))
                    (buffer-string)))
            (count 0)
            (start 0))
        (while (string-match "(make-frame" text start)
          (setq count (1+ count) start (match-end 0)))
        (should (= count 1))))))

;;; sessions-test.el ends here
