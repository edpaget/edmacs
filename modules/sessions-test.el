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
      "`edmacs-frames--tab-root''s fallback for a tab this module never
stamped resolves via the globally selected window (frames.el's
`edmacs-frames--tab-window-buffer'), so deriving a background FRAME's
tab roots must select FRAME first -- otherwise it would silently read
whatever frame the caller happens to have selected instead, exactly
the cross-frame mix-up a daemon-boot multi-frame restore hits for
every frame but whichever one is globally selected at the time."
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
    ;; AC1 -- saved frame position (`left'/`top') must survive restore
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-frameset-filter-preserves-frame-position ()
      "AC1 requires saved window positions to be honoured on restore.
`frameset-restore' owns that: `frameset-filter-alist''s own default
action for `left'/`top' is `frameset-filter-shelve-param', which only
shelves a parameter when switching a GUI frame to a tty (never a plain
GUI-to-GUI restore, confirmed by reading its docstring) -- unlike
`name', which frameset.el marks `:never' outright. This guards against
a future edit to this file's own `frameset-filter-alist' pushes (the
colour `:never's, the `edmacs-repo' pin) accidentally widening to catch
`left'/`top' too, the way they deliberately do for frame colours."
      (dolist (param '(left top))
        (should-not (eq (cdr (assq param frameset-filter-alist)) :never))))

    ;; ==========================================================================
    ;; AC1/AC2 -- edmacs-sessions--ensure-sidebar / --finish-frameset-restore
    ;; ==========================================================================

    (ert-deftest edmacs-sessions-test-ensure-sidebar-skips-frame-with-window ()
      (let ((show-calls nil))
        (cl-letf (((symbol-function 'edmacs-sidebar--window) (lambda (_) 'a-window))
                  ((symbol-function 'edmacs-sidebar-show) (lambda (f) (push f show-calls))))
          (edmacs-sessions--ensure-sidebar (selected-frame))
          (should-not show-calls))))

    (ert-deftest edmacs-sessions-test-ensure-sidebar-shows-frame-without-window ()
      (let ((show-calls nil))
        (cl-letf (((symbol-function 'edmacs-sidebar--window) (lambda (_) nil))
                  ((symbol-function 'edmacs-sidebar-show) (lambda (f) (push f show-calls))))
          (edmacs-sessions--ensure-sidebar (selected-frame))
          (should (equal show-calls (list (selected-frame)))))))

    (ert-deftest edmacs-sessions-test-finish-restore-shows-sidebar-for-every-live-frame ()
      (let* ((f1 (selected-frame))
             (f2 (edmacs-sessions-test--make-second-frame-or-skip))
             (show-calls nil))
        (unwind-protect
            (cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&rest _) nil))
                      ((symbol-function 'edmacs-frames--tab-root) (lambda (_) nil))
                      ((symbol-function 'edmacs-frames--repo-of) (lambda (_) nil))
                      ((symbol-function 'edmacs-sidebar--window)
                       (lambda (frame) (if (eq frame f2) 'has-window nil)))
                      ((symbol-function 'edmacs-sidebar-show)
                       (lambda (frame) (push frame show-calls))))
              (edmacs-sessions--finish-frameset-restore)
              (should (member f1 show-calls))
              (should-not (member f2 show-calls)))
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
