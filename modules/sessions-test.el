;;; sessions-test.el --- Tests for sessions.el -*- lexical-binding: t -*-

;;; Commentary:
;; sessions.el cannot be `-l'-loaded standalone under plain `-Q --batch':
;; it makes an unconditional, non-deferred call to
;; `edmacs-evil-config-add-c-x-chord' (evil-config.el, never loaded here)
;; and unconditional `leader-def' calls (a real macro `modules/keybindings.el'
;; defines, which in turn needs the real `evil'/`general', loaded only in a
;; real init.el session). So this file, like sidebar-test.el, fixes up the
;; environment before loading sessions.el itself rather than taking it as a
;; `-l' argument, via `edmacs-test-support-load-sessions-stack'
;; (`modules/test-support.el'), the same helper
;; `modules/sessions-live-test.el' and `modules/keybindings-test.el' use:
;;
;;   - `edmacs-evil-config-add-c-x-chord' is stubbed as a no-op.
;;   - the real `evil' and `general' are pulled off this checkout's (or its
;;     sibling main checkout's) `straight/build', mirroring
;;     `edmacs-test-support-straight-build-root''s fallback.
;;   - `modules/keybindings.el' is loaded (once) for its `leader-def' macro
;;     before `modules/sessions.el' itself.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/git-common-dir.el -l modules/windows.el \
;;         -l modules/workspaces.el -l modules/sessions-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; `windows.el' is on that line because the migration sanitizes every
;; saved window state through `edmacs-windows-ws-ensure-main', and
;; `workspaces.el' because `edmacs-sessions--prepare-frameset' puts
;; `desktop-saved-frameset' through `edmacs-workspaces-migrate-frameset';
;; without it two tests here would exercise a stub of their own subject.
;;
;; Every test runs under plain `-Q --batch', zero skipped, no controlling
;; terminal required. Nothing here exits Emacs, kills a frame, or starts a
;; real server: the process-ending and socket-opening calls are stubbed.
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

(defvar edmacs-sessions-test--build-root
  (edmacs-test-support-straight-build-root)
  "This checkout's (or its sibling main checkout's) `straight/build' root.")

(defconst edmacs-sessions-test--self-file (or load-file-name buffer-file-name))

(if (null edmacs-sessions-test--build-root)

    (ert-deftest edmacs-sessions-test-general-unavailable ()
      (edmacs-test-support-report-suite-unavailable
       edmacs-sessions-test--self-file
       "general's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    ;; See `edmacs-test-support-load-sessions-stack''s docstring for the
    ;; full bootstrap recipe (evil/general, the chord stub, `leader-def'
    ;; via keybindings.el, then git-common-dir/windows/workspaces/sessions).
    (edmacs-test-support-load-sessions-stack)

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
    ;; The restore finish-up
    ;; ==========================================================================

    (defmacro edmacs-sessions-test--with-stubbed-steps (calls &rest body)
      "Run BODY with each restore step recording into CALLS.
Each entry is (STEP . FRAME), pushed in call order, so the order the
finish-up fixes is what is asserted, not any callee's behaviour."
      (declare (indent 1))
      `(cl-letf (((symbol-function 'edmacs-stack-sweep-stale-panes)
                  (lambda (f) (push (cons 'sweep f) ,calls)))
                 ((symbol-function 'edmacs-workspaces-stamp-frame-tabs)
                  (lambda (f) (push (cons 'stamp f) ,calls)))
                 ((symbol-function 'edmacs-sidebar-show)
                  (lambda (f) (push (cons 'sidebar f) ,calls))))
         ,@body))

    (ert-deftest edmacs-sessions-test-finish-frameset-restore-declines-unusable-frame ()
      "Handed a frame `edmacs-workspaces-frame-usable-p' rejects, the
finish-up does no per-frame work, and does not fall back to scanning for
another frame -- or an explicit argument would stop meaning anything."
      (let (calls)
        (cl-letf (((symbol-function 'frame-live-p) (lambda (f) (memq f '(tty gui))))
                  ((symbol-function 'edmacs-workspaces-frame-usable-p)
                   (lambda (f) (eq f 'gui)))
                  ((symbol-function 'edmacs-workspaces-gui-frame)
                   (lambda () (ert-fail "an explicit FRAME must not be second-guessed"))))
          (edmacs-sessions-test--with-stubbed-steps calls
            (edmacs-sessions--finish-frameset-restore 'tty)))
        (should-not calls)))

    (ert-deftest edmacs-sessions-test-finish-frameset-restore-runs-every-step-in-order ()
      "Stale panes are swept, then tabs stamped, then the sidebar shown: the
sidebar files a tab under a project by that tab's own stamped root, so a
sidebar drawn first would render a restored tab under no project."
      (let (calls)
        (cl-letf (((symbol-function 'frame-live-p) (lambda (f) (memq f '(tty gui))))
                  ((symbol-function 'edmacs-workspaces-frame-usable-p)
                   (lambda (f) (eq f 'gui))))
          (edmacs-sessions-test--with-stubbed-steps calls
            (edmacs-sessions--finish-frameset-restore 'gui)))
        (setq calls (nreverse calls))
        (should (seq-every-p (lambda (c) (eq (cdr c) 'gui)) calls))
        (should (equal (mapcar #'car calls) '(sweep stamp sidebar)))))

    (ert-deftest edmacs-sessions-test-finish-frameset-restore-falls-back-to-the-gui-frame ()
      "With no FRAME it finishes the session's GUI frame; with no GUI frame
either it does nothing rather than guessing."
      (let (calls)
        (cl-letf (((symbol-function 'frame-live-p) (lambda (_f) t))
                  ((symbol-function 'edmacs-workspaces-frame-usable-p) (lambda (_f) t))
                  ((symbol-function 'edmacs-workspaces-gui-frame) (lambda () 'gui)))
          (edmacs-sessions-test--with-stubbed-steps calls
            (edmacs-sessions--finish-frameset-restore nil)))
        (should (seq-every-p (lambda (c) (eq (cdr c) 'gui)) calls))
        (should (= 3 (length calls))))
      (let (calls)
        (cl-letf (((symbol-function 'edmacs-workspaces-gui-frame) (lambda () nil)))
          (edmacs-sessions-test--with-stubbed-steps calls
            (edmacs-sessions--finish-frameset-restore nil)))
        (should-not calls)))

    (ert-deftest edmacs-sessions-test-finish-restore-never-calls-make-frame ()
      "`frameset-restore''s own `:reuse-frames t' owns all frame creation;
the finish-up only mutates the already-live frame it is handed."
      (cl-letf (((symbol-function 'edmacs-workspaces-frame-usable-p) (lambda (_) t))
                ((symbol-function 'edmacs-stack-sweep-stale-panes) #'ignore)
                ((symbol-function 'edmacs-workspaces-stamp-frame-tabs) #'ignore)
                ((symbol-function 'edmacs-sidebar-show) #'ignore)
                ((symbol-function 'make-frame)
                 (lambda (&rest _)
                   (error "edmacs-sessions--finish-frameset-restore must never call make-frame"))))
        (edmacs-sessions--finish-frameset-restore (selected-frame))))

    (ert-deftest edmacs-sessions-test-after-desktop-read-warns-instead-of-signalling ()
      "An error out of `desktop-after-read-hook' would abort the rest of the
hook, the sidebar's regeneration among it."
      (let ((warnings nil))
        (cl-letf (((symbol-function 'edmacs-sessions--finish-frameset-restore)
                   (lambda (&rest _) (error "boom")))
                  ((symbol-function 'display-warning)
                   (lambda (_type msg &rest _) (push msg warnings))))
          (edmacs-sessions--after-desktop-read)
          (should (= 1 (length warnings)))
          (should (string-match-p "boom" (car warnings))))))

    (ert-deftest edmacs-sessions-test-after-desktop-read-is-hooked ()
      (should (memq #'edmacs-sessions--after-desktop-read desktop-after-read-hook)))

    (ert-deftest edmacs-sessions-test-gui-frame-finds-only-a-live-graphic-frame ()
      "`edmacs-workspaces-gui-frame' is the one place that answers \"which
frame is the session's\"; sessions.el is a consumer, not a second
implementation."
      (cl-letf (((symbol-function 'frame-list) (lambda () '(dead tty gui)))
                ((symbol-function 'frame-live-p) (lambda (f) (memq f '(tty gui))))
                ((symbol-function 'display-graphic-p) (lambda (&optional f) (eq f 'gui))))
        (should (eq (edmacs-workspaces-gui-frame) 'gui)))
      (cl-letf (((symbol-function 'frame-list) (lambda () '(tty)))
                ((symbol-function 'frame-live-p) (lambda (_f) t))
                ((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
        (should-not (edmacs-workspaces-gui-frame))))


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

    (ert-deftest edmacs-sessions-test-tab-name-never-names-a-tab-after-a-side-window ()
      "Core's `tab-bar-tab-name-current' answers from the SELECTED window's
buffer, and the sidebar is a selectable side window -- so an unstamped tab
read while the sidebar held point was named `*sidebar*', and that name stuck
in the tab alist and rendered as a phantom row in the sidebar's own tree."
      (let* ((frame (selected-frame))
             (saved (frame-parameter frame 'tabs))
             (real (generate-new-buffer "est-real-window-buffer"))
             (side-buf (generate-new-buffer "est-side-window-buffer")))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (set-frame-parameter frame 'tabs (list (list 'current-tab)))
              (set-window-buffer (selected-window) real)
              (let ((side (display-buffer-in-side-window
                           side-buf '((side . left) (slot . 0)))))
                (select-window side)
                (should (equal (buffer-name (window-buffer (selected-window)))
                               "est-side-window-buffer"))
                (should (equal (edmacs-sessions--tab-name)
                               "est-real-window-buffer"))))
          (set-frame-parameter frame 'tabs saved)
          (kill-buffer real)
          (kill-buffer side-buf))))

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

    ;; ==========================================================================
    ;; edmacs-sessions--prepare-frameset -- migrate before desktop restores
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-prepare-drops-empty-frameset ()
      "Restoring an empty frameset deletes frames rather than doing nothing
(see `edmacs-sessions--frameset-has-frames-p'), so it is dropped."
      (let ((desktop-saved-frameset (edmacs-sessions-test--frameset nil)))
        (edmacs-sessions--prepare-frameset)
        (should-not desktop-saved-frameset)))

    (ert-deftest edmacs-sessions-test-prepare-migrates-the-frameset ()
      "What desktop restores is the MIGRATED frameset: one frame state
holding every project as a tab group, so `frameset-restore' reuses the
initial frame and never creates a second one."
      (let* ((fs (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))
             (migrated (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))
             (seen nil)
             (desktop-saved-frameset fs))
        (cl-letf (((symbol-function 'edmacs-workspaces-migrate-frameset)
                   (lambda (arg) (setq seen arg) migrated)))
          (edmacs-sessions--prepare-frameset)
          (should (eq seen fs))
          (should (eq desktop-saved-frameset migrated)))))

    (ert-deftest edmacs-sessions-test-prepare-survives-a-signalling-migration ()
      "A failing migration warns and restores the frameset unmigrated --
degraded, but never an error out of `desktop-read'."
      (let* ((fs (edmacs-sessions-test--frameset edmacs-sessions-test--one-state))
             (warnings nil)
             (desktop-saved-frameset fs))
        (cl-letf (((symbol-function 'edmacs-workspaces-migrate-frameset)
                   (lambda (_arg) (error "boom")))
                  ((symbol-function 'display-warning)
                   (lambda (_type msg &rest _) (push msg warnings))))
          (edmacs-sessions--prepare-frameset)
          (should (eq desktop-saved-frameset fs))
          (should (= 1 (length warnings)))
          (should (string-match-p "boom" (car warnings))))))

    (ert-deftest edmacs-sessions-test-prepare-uses-the-real-migration ()
      "The real `edmacs-workspaces-migrate-frameset' is what runs, not a
stub of it -- so `workspaces.el' belongs on this suite's own invocation
line (see Commentary). Two frame states in, one out."
      (let ((desktop-saved-frameset
             (edmacs-sessions-test--frameset
              (list (cons '((last-focus-update . t)
                            (tabs (current-tab (name . "a"))))
                          nil)
                    (cons '((name . "b") (tabs (current-tab (name . "b"))))
                          nil)))))
        (unless (fboundp 'edmacs-workspaces-migrate-frameset)
          (ert-skip "workspaces.el is not loaded; add -l modules/workspaces.el"))
        (unless (fboundp 'edmacs-windows-ws-ensure-main)
          (ert-skip "windows.el is not loaded; add -l modules/windows.el"))
        (edmacs-sessions--prepare-frameset)
        (should (= 1 (length (frameset-states desktop-saved-frameset))))
        (should (= 2 (length (alist-get 'tabs (car (car (frameset-states
                                                         desktop-saved-frameset)))))))))

    (ert-deftest edmacs-sessions-test-prepare-runs-before-desktop-restore-frameset ()
      "`desktop-read' calls `desktop-restore-frameset' directly, before
`desktop-after-read-hook', so the migration has to ride on that call."
      (should (advice-member-p #'edmacs-sessions--prepare-frameset
                               'desktop-restore-frameset)))

    ;; ==========================================================================
    ;; Server and quit
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-start-server-starts-when-none-answers ()
      (let ((started 0))
        (cl-letf (((symbol-function 'server-running-p) (lambda (&rest _) nil))
                  ((symbol-function 'server-start)
                   (lambda (&rest _) (setq started (1+ started)))))
          (let ((noninteractive nil))
            (edmacs-sessions--start-server))
          (should (= started 1)))))

    (ert-deftest edmacs-sessions-test-start-server-leaves-a-running-one-alone ()
      "A second Emacs must not take the socket from the first."
      (cl-letf (((symbol-function 'server-running-p) (lambda (&rest _) t))
                ((symbol-function 'server-start)
                 (lambda (&rest _) (ert-fail "server-start must not run"))))
        (let ((noninteractive nil))
          (edmacs-sessions--start-server))))

    (ert-deftest edmacs-sessions-test-start-server-skips-batch ()
      (cl-letf (((symbol-function 'server-start)
                 (lambda (&rest _) (ert-fail "server-start must not run in batch"))))
        (let ((noninteractive t))
          (edmacs-sessions--start-server))))

    (ert-deftest edmacs-sessions-test-start-server-is-hooked ()
      (should (memq #'edmacs-sessions--start-server emacs-startup-hook)))

    (ert-deftest edmacs-sessions-test-quit-kills-the-terminal ()
      "`SPC q q' is the stock quit: the last frame quits Emacs."
      (let ((killed 0))
        (cl-letf (((symbol-function 'save-buffers-kill-terminal)
                   (lambda (&rest _) (setq killed (1+ killed)))))
          (edmacs-quit)
          (should (= killed 1)))))

    (ert-deftest edmacs-sessions-test-no-close-frame-remap ()
      "Closing a frame is stock again: nothing remaps `delete-frame' or
intercepts the window close button."
      (should-not (command-remapping 'delete-frame nil (list global-map)))
      (should (eq (lookup-key special-event-map [delete-frame]) 'handle-delete-frame)))

    ;; ==========================================================================
    ;; sessions.el's own source
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-commentary-does-not-relitigate-frames ()
      "The Commentary must not carry the old tabs-over-frames rationale
back. What it must record is the constraint that actually survives --
ghostel's PTY sizing."
      (let ((text (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "modules/sessions.el" default-directory))
                    (buffer-string))))
        (should-not (string-match-p "per-frame is where manzaltu" text))
        (should-not (string-match-p "explicitly tab-aware" text))
        (should (string-match-p "window-adjust-process-window-size-smallest" text))))

    (ert-deftest edmacs-sessions-test-no-daemon-code-paths ()
      "Emacs runs as a plain app: sessions.el neither creates frames nor
branches on `daemonp'."
      (let ((text (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "modules/sessions.el" default-directory))
                    (buffer-string))))
        (should-not (string-match-p "(make-frame" text))
        (should-not (string-match-p "(daemonp)" text))))))

;;; sessions-test.el ends here
