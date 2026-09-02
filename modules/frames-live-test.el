;;; frames-live-test.el --- Tests for frames.el needing real frames/git -*- lexical-binding: t -*-

;;; Commentary:
;; Two independent things this suite needs but can't always have, each
;; following an existing convention in this codebase:
;;
;;   - A second real frame, for AC2/AC3/AC4/AC7/AC8: `-Q --batch' with no
;;     controlling terminal has no way to open one, so
;;     `edmacs-frames-live-test--make-frame-or-skip' (copied from
;;     sidebar-test.el's own `edmacs-sidebar-test--make-second-frame-or-skip',
;;     a second, independent optional dependency) skips cleanly under a
;;     plain invocation. Run under `script -q /dev/null emacs -Q --batch
;;     ...' to actually exercise these.
;;   - `sidebar.el' itself, via `edmacs-frames-open''s call to
;;     `edmacs-sidebar-show': needs `magit-section', whose straight build
;;     this worktree may not have (per this repo's own CLAUDE.md, only the
;;     main checkout has a populated `straight/build'). Falls back to the
;;     sibling main checkout's build tree exactly as sidebar-test.el does;
;;     reports a single skip if neither has one.
;;
;; AC1/AC5 (the picker-seeding registrar in core.el) additionally load
;; core.el itself, standalone, with `straight-use-package' stubbed to a
;; no-op (core.el's own compat/cond-let/transient dependencies are pulled
;; from the same sibling straight/build). `user-emacs-directory' is
;; rebound to a fresh temp directory *before* `core.el' (and therefore
;; `project.el') ever loads, and every test in that section asserts
;; `project-list-file' actually resolved under that sandbox before
;; touching `project-known-project-roots' -- otherwise a stray subprocess
;; git repo could get written into the real user's own project list.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/frames-live-test.el -f ert-run-tests-batch-and-exit
;;
;; (frames.el and sidebar.el are NOT passed on the command line -- this
;; file fixes `load-path' against the straight build tree and loads them
;; itself, below, mirroring sidebar-test.el's own convention.)

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; Same native-comp-trampoline hazard `sidebar-test.el' disables this for;
;; see its own commentary.
(setq native-comp-enable-subr-trampolines nil)

;; `tab-bar-new-tab-to's default (t) choice of a brand-new tab's initial
;; window state goes through a split-then-delete dance
;; (`(split-window nil window-safe-min-width t)') purely to shed stale
;; window parameters -- which needs real width a `script'-attached pty in
;; this sandboxed environment may not report. `clone' reuses
;; `window-state-put' with no split, sidestepping that environment limit
;; without touching what any test here actually checks (none depend on
;; `tab-bar-new-tab-choice's value).
(setq tab-bar-new-tab-choice 'clone)

;; The suite's own ambient frame (there is always at least one, even
;; under `-Q --batch') is just as adoptable a "spare" as any frame this
;; file creates on purpose (see `edmacs-frames--spare-frame'), and unlike
;; those it is never passed through `set-frame-size' below -- widen it
;; too, or a test whose sandbox has no decoy frame available can adopt a
;; too-narrow one and hang inside `tab-bar-new-tab's window-splitting.
(ignore-errors (set-frame-size (selected-frame) 200 50))

(defun edmacs-frames-live-test--locate-straight-build-root ()
  "Return this checkout's (or its sibling main checkout's) `straight/build'."
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

(defvar edmacs-frames-live-test--build-root
  (edmacs-frames-live-test--locate-straight-build-root))

(defun edmacs-frames-live-test--add-dep (dep)
  "Add DEP's directory under the located `straight/build' to `load-path'."
  (when edmacs-frames-live-test--build-root
    (let ((dir (expand-file-name dep edmacs-frames-live-test--build-root)))
      (when (file-directory-p dir) (add-to-list 'load-path dir)))))

(if (null edmacs-frames-live-test--build-root)

    (ert-deftest edmacs-frames-live-test-straight-build-unavailable ()
      (ert-skip "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section"))
      (edmacs-frames-live-test--add-dep dep))
    (load (expand-file-name "modules/sidebar.el" default-directory) nil t)
    (load (expand-file-name "modules/frames.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Test helpers
    ;; ==========================================================================

    (defun edmacs-frames-live-test--make-frame-or-skip ()
      "Return a new real frame on this process's controlling terminal, or skip.
See this file's Commentary; identical rationale and mechanism to
`edmacs-sidebar-test--make-second-frame-or-skip'."
      (condition-case e
          (let ((frame (make-frame '((window-system . nil)
                                      (tty . "/dev/tty")
                                      (tty-type . "xterm")))))
            (unless (frame-live-p frame)
              (ert-skip "could not create a second frame in this batch environment"))
            ;; `script's pty often reports a tiny size with no real
            ;; terminal behind it; widen it so the sidebar (32 columns)
            ;; plus a main window still leaves `tab-bar-new-tab' enough
            ;; room to split without erroring.
            (ignore-errors (set-frame-size frame 200 50))
            frame)
        (error (ert-skip (format "could not create a second frame in this \
batch environment (no controlling terminal? run under `script -q /dev/null \
emacs ...' to exercise this test): %s" e)))))

    (defun edmacs-frames-live-test--make-git-repo (dir)
      "Init a real, one-commit git repo at DIR."
      (make-directory dir t)
      (let ((default-directory (file-name-as-directory dir)))
        (call-process "git" nil nil nil "init" "-q")
        (call-process "git" nil nil nil "config" "user.email" "t@t.com")
        (call-process "git" nil nil nil "config" "user.name" "t")
        (write-region "x" nil (expand-file-name "f.txt" dir))
        (call-process "git" nil nil nil "add" "-A")
        (call-process "git" nil nil nil "commit" "-q" "-m" "init")))

    (defun edmacs-frames-live-test--main-window (frame)
      "Return FRAME's non-side (main editing) window.
`frame-root-window' is often an INTERNAL window once the sidebar splits
the frame in two, and `window-buffer' on an internal window is always
nil -- this finds the actual leaf window content lives in instead."
      (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                (window-list frame 'never)))

    (defun edmacs-frames-live-test--add-worktree (repo wt-dir)
      "Add a real git worktree of REPO at WT-DIR."
      (let ((default-directory (file-name-as-directory repo)))
        (call-process "git" nil nil nil "worktree" "add" wt-dir)))

    (defmacro edmacs-frames-live-test--with-frames (frames &rest body)
      "Bind FRAMES (a list of symbols) to fresh real frames, run BODY, clean up.
Each frame is deleted afterward if still live; `edmacs-frames-open''s own
sidebar buffer for it is cleaned up too."
      (declare (indent 1))
      `(let ,(mapcar (lambda (f) (list f '(edmacs-frames-live-test--make-frame-or-skip)))
                     frames)
         (unwind-protect (progn ,@body)
           ,@(mapcar (lambda (f)
                       `(when (frame-live-p ,f)
                          (let ((buf (edmacs-sidebar--buffer ,f)))
                            (when (buffer-live-p buf) (kill-buffer buf)))
                          (delete-frame ,f)))
                     frames))))

    (defmacro edmacs-frames-live-test--with-sandbox (var &rest body)
      "Bind VAR to a fresh temp directory for BODY, deleting it after."
      (declare (indent 1))
      `(let ((,var (file-name-as-directory (make-temp-file "edmacs-frames-test-" t))))
         (unwind-protect (progn ,@body)
           (delete-directory ,var t))))

    ;; ==========================================================================
    ;; AC2 -- create-or-raise a repo frame
    ;; ==========================================================================

    (ert-deftest edmacs-frames-live-test-open-creates-then-raises-same-frame ()
      "`edmacs-frames-open' may adopt a spare (repo-less) frame instead of
always `make-frame'-ing (see `edmacs-frames--spare-frame') -- exercised
here by a decoy frame that must stay untouched, so what matters is that
SOME live frame ends up correctly configured for repo A, and a second
call raises that exact same one rather than creating another."
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo (expand-file-name "repoA" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo)
          (edmacs-frames-live-test--with-frames (decoy)
            (let ((count-before (length (frame-list)))
                  (frame nil))
              (unwind-protect
                  (progn
                    (setq frame (edmacs-frames-open repo))
                    (should (frame-live-p frame))
                    (should (frame-parameter frame 'edmacs-repo))
                    (should (equal (frame-parameter frame 'name) "repoA"))
                    (should (= 1 (length (tab-bar-tabs frame))))
                    (should (with-selected-frame frame (edmacs-sidebar--window frame)))
                    ;; Opening again raises the SAME frame -- no new one.
                    (let ((frame2 (edmacs-frames-open repo)))
                      (should (eq frame2 frame))
                      (should (= count-before (length (frame-list))))
                      (should (eq (selected-frame) frame))))
                (when (and (frame-live-p frame) (not (eq frame decoy)))
                  (let ((buf (edmacs-sidebar--buffer frame)))
                    (when (buffer-live-p buf) (kill-buffer buf)))
                  (delete-frame frame))))))))

    ;; ==========================================================================
    ;; AC3 -- SPC T p routes a worktree tab to its OWN repo's frame
    ;; ==========================================================================

    (ert-deftest edmacs-frames-live-test-worktree-tab-routes-and-dedupes ()
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo-a (expand-file-name "repoA" sandbox))
               (repo-b (expand-file-name "repoB" sandbox))
               (wt-b (expand-file-name "repoB-wt" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo-a)
          (edmacs-frames-live-test--make-git-repo repo-b)
          (edmacs-frames-live-test--add-worktree repo-b wt-b)
          (edmacs-frames-live-test--with-frames (frame-a frame-c)
            (let (frame-b)
              (unwind-protect
                  (progn
                    (with-selected-frame frame-a (edmacs-frames-open repo-a))
                    (setq frame-a (edmacs-frames-for-repo (edmacs-frames--repo-of repo-a)))
                    (let ((a-tab-count (length (tab-bar-tabs frame-a))))
                      ;; From frame A, open a tab for repo B's worktree.
                      (with-selected-frame frame-a
                        (setq frame-b (edmacs-frames-open-worktree-tab wt-b)))
                      (should (not (eq frame-b frame-a)))
                      (should (frame-live-p frame-b))
                      ;; A gained no tab.
                      (should (= a-tab-count (length (tab-bar-tabs frame-a))))
                      ;; B has exactly one tab for wt-b (plus its own main-worktree tab).
                      (should (edmacs-frames--find-tab-by-root (file-truename wt-b) frame-b))
                      (let ((b-tab-count (length (tab-bar-tabs frame-b)))
                            (closed-before (length tab-bar-closed-tabs)))
                        ;; Repeating from a third frame C: still exactly one tab in B.
                        (with-selected-frame frame-c
                          (edmacs-frames-open-worktree-tab wt-b))
                        (should (= b-tab-count (length (tab-bar-tabs frame-b))))
                        (should (= closed-before (length tab-bar-closed-tabs))))))
                (when (and frame-b (frame-live-p frame-b) (not (eq frame-b frame-a))
                           (not (eq frame-b frame-c)))
                  (let ((buf (edmacs-sidebar--buffer frame-b)))
                    (when (buffer-live-p buf) (kill-buffer buf)))
                  (delete-frame frame-b))
                (when (and frame-a (frame-live-p frame-a) (not (memq frame-a (list frame-c))))
                  (let ((buf (edmacs-sidebar--buffer frame-a)))
                    (when (buffer-live-p buf) (kill-buffer buf)))
                  (delete-frame frame-a))))))))

    ;; ==========================================================================
    ;; AC4 -- reconciliation folds a duplicate tab opened via any other route
    ;; ==========================================================================

    (ert-deftest edmacs-frames-live-test-reconcile-folds-duplicate-tab ()
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo (expand-file-name "repoA" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo)
          (edmacs-frames-live-test--with-frames (frame)
            (with-selected-frame frame
              (edmacs-frames-open repo)
              (let ((tab-count (length (tab-bar-tabs frame)))
                    (closed-before (length tab-bar-closed-tabs)))
                ;; A brand-new tab (as `SPC T n' or `other-tab-prefix' would
                ;; make) that happens to already show a buffer in this same
                ;; worktree gets folded back onto the existing tab, not kept.
                (let ((default-directory (file-name-as-directory repo)))
                  (dired repo)
                  (tab-bar-new-tab))
                (should (= tab-count (length (tab-bar-tabs frame))))
                (should (= (1+ closed-before) (length tab-bar-closed-tabs)))
                ;; A brand-new tab for an UNRELATED directory is kept.
                (let* ((other (expand-file-name "unrelated" sandbox)))
                  (make-directory other t)
                  (let ((default-directory (file-name-as-directory other)))
                    (dired other)
                    (tab-bar-new-tab))
                  (should (= (1+ tab-count) (length (tab-bar-tabs frame)))))))))))

    ;; ==========================================================================
    ;; AC1/AC5 -- picker seeding registrar (core.el), sandboxed
    ;; ==========================================================================

    (defvar edmacs-frames-live-test--core-registrar-available
      (and edmacs-frames-live-test--build-root
           (dolist (dep '("compat" "cond-let" "transient") t)
             (edmacs-frames-live-test--add-dep dep)))
      "Non-nil once the deps `core.el' needs to load standalone are on `load-path'.")

    (defmacro edmacs-frames-live-test--with-registrar-sandbox (vars &rest body)
      "Load core.el's registrar into a sandboxed `user-emacs-directory', run BODY.
VARS binds (SANDBOX PROJECTS-DIR) for BODY's use. `user-emacs-directory'
is rebound *before* `core.el' (and therefore `project.el') ever loads in
this process, and `project-list-file' is asserted to have resolved
inside SANDBOX before BODY runs -- refusing to continue, rather than
touching the real user's own project list, if that ever fails."
      (declare (indent 1))
      (cl-destructuring-bind (sandbox-var projects-var) vars
        `(if (not edmacs-frames-live-test--core-registrar-available)
             (ert-skip "compat/cond-let/transient's straight build was not found; \
bootstrap straight once (open this worktree in a real Emacs session) to enable \
this test")
           (let* ((,sandbox-var (file-name-as-directory (make-temp-file "edmacs-core-test-" t)))
                  (,projects-var (expand-file-name "Projects/" ,sandbox-var))
                  ;; `project.el' may already be loaded in this process (by
                  ;; `frames.el' itself, earlier in this same suite), which
                  ;; freezes `project-list-file's *value* at its first-load
                  ;; `user-emacs-directory' -- rebinding that variable alone
                  ;; would not move it. Bind `project-list-file' (and
                  ;; `project--list', so a previously-read real list is
                  ;; never reused) directly instead.
                  (user-emacs-directory (expand-file-name "emacsd/" ,sandbox-var))
                  (project-list-file (expand-file-name "projects.eld" user-emacs-directory))
                  (project--list 'unset))
             (unwind-protect
                 (progn
                   (make-directory ,projects-var t)
                   (unless (fboundp 'straight-use-package)
                     (fset 'straight-use-package (lambda (&rest _) nil)))
                   (unless (fboundp 'edmacs--register-projects-under-projects-dir)
                     (load (expand-file-name "modules/core.el" default-directory) nil t))
                   (unless (string-prefix-p (expand-file-name ,sandbox-var)
                                            (expand-file-name project-list-file))
                     (error "sandbox guard failed: project-list-file is %s, not under %s"
                            project-list-file ,sandbox-var))
                   ,@body)
               (delete-directory ,sandbox-var t))))))

    (defun edmacs-frames-live-test--call-registrar-against (projects-dir)
      "Run the real registrar with `directory-files' redirected to PROJECTS-DIR
for its one `~/Projects' lookup, leaving every other call untouched."
      (let ((orig (symbol-function 'directory-files)))
        (cl-letf (((symbol-function 'directory-files)
                   (lambda (dir &rest args)
                     (if (equal (expand-file-name dir) (expand-file-name "~/Projects"))
                         (apply orig projects-dir args)
                       (apply orig dir args)))))
          (edmacs--register-projects-under-projects-dir))))

    (ert-deftest edmacs-frames-live-test-registrar-lists-repos-not-worktrees ()
      (edmacs-frames-live-test--with-registrar-sandbox (sandbox projects)
        (let* ((repo1 (expand-file-name "repo1" projects))
               (repo2 (expand-file-name "repo2" projects))
               (wt-dir (expand-file-name "repo1__worktrees" projects))
               (wt-a (expand-file-name "wt-a" wt-dir)))
          (edmacs-frames-live-test--make-git-repo repo1)
          (edmacs-frames-live-test--make-git-repo repo2)
          (make-directory wt-dir t)
          (edmacs-frames-live-test--add-worktree repo1 wt-a)
          (edmacs-frames-live-test--call-registrar-against projects)
          (let ((roots (project-known-project-roots)))
            (should (member (file-name-as-directory repo1) roots))
            (should (member (file-name-as-directory repo2) roots))
            (should-not (seq-find (lambda (r) (string-match-p "__worktrees/" r)) roots))
            ;; AC5: visiting a file inside the worktree via plain find-file
            ;; must not cause it to be remembered either.
            (let ((buf (find-file-noselect (expand-file-name "f.txt" wt-a))))
              (unwind-protect
                  (with-current-buffer buf
                    (call-interactively #'project-remember-project))
                (kill-buffer buf)))
            (should-not (seq-find (lambda (r) (string-match-p "__worktrees/" r))
                                  (project-known-project-roots)))))))

    (ert-deftest edmacs-frames-live-test-registrar-idempotent-after-worktree-visit ()
      "AC1's second half: re-running the registrar after a worktree visit \
still excludes it."
      (edmacs-frames-live-test--with-registrar-sandbox (sandbox projects)
        (let* ((repo1 (expand-file-name "repo1" projects))
               (wt-dir (expand-file-name "repo1__worktrees" projects))
               (wt-a (expand-file-name "wt-a" wt-dir)))
          (edmacs-frames-live-test--make-git-repo repo1)
          (make-directory wt-dir t)
          (edmacs-frames-live-test--add-worktree repo1 wt-a)
          (edmacs-frames-live-test--call-registrar-against projects)
          (find-file-noselect (expand-file-name "f.txt" wt-a))
          (edmacs-frames-live-test--call-registrar-against projects)
          (should-not (seq-find (lambda (r) (string-match-p "__worktrees/" r))
                                (project-known-project-roots))))))

    ;; ==========================================================================
    ;; AC6 -- stray-visit relocation, cache-hit-only, never shells out
    ;; ==========================================================================

    (ert-deftest edmacs-frames-live-test-stray-visit-relocates-and-restores ()
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo-a (expand-file-name "repoA" sandbox))
               (repo-b (expand-file-name "repoB" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo-a)
          (edmacs-frames-live-test--make-git-repo repo-b)
          ;; Pre-populate the cache directly -- no live git needed for the
          ;; relocator itself, which must never call `edmacs-git-common-dir-1'.
          (puthash (file-name-as-directory repo-a)
                   (expand-file-name ".git" repo-a) edmacs-git-common-dir-cache)
          (puthash (file-name-as-directory repo-b)
                   (expand-file-name ".git" repo-b) edmacs-git-common-dir-cache)
          ;; A properly-sized decoy so the relocator's own `edmacs-frames-open'
          ;; call for repo B adopts it (see `edmacs-frames--spare-frame')
          ;; instead of a bare `(make-frame)' with no tty/size parameters.
          (edmacs-frames-live-test--with-frames (frame-a _decoy)
            (unwind-protect
                (progn
                  (with-selected-frame frame-a (edmacs-frames-open repo-a))
                  (let* ((frame-a (edmacs-frames-for-repo (expand-file-name ".git" repo-a)))
                         (prior-buf (with-selected-frame frame-a
                                      (window-buffer (edmacs-frames-live-test--main-window frame-a))))
                         (edmacs-frames-stray-visit-relocate nil)
                         (b-file (expand-file-name "f.txt" repo-b))
                         (frame-b nil))
                    (unwind-protect
                        (progn
                          ;; Guard nil: find-file'ing a foreign repo's file
                          ;; in A's own window must never move it.
                          (with-selected-frame frame-a
                            (find-file b-file))
                          (edmacs-frames--relocate-stray-visits frame-a)
                          (should (equal (buffer-file-name
                                          (window-buffer (edmacs-frames-live-test--main-window frame-a)))
                                         b-file))
                          ;; Guard t: it relocates, and A's window reverts.
                          (let ((edmacs-frames-stray-visit-relocate t)
                                (edmacs-git-common-dir-1-calls 0))
                            (cl-letf (((symbol-function 'edmacs-git-common-dir-1)
                                       (lambda (&rest _)
                                         (setq edmacs-git-common-dir-1-calls
                                               (1+ edmacs-git-common-dir-1-calls))
                                         (error "must not shell out"))))
                              (edmacs-frames--relocate-stray-visits frame-a))
                            (should (= 0 edmacs-git-common-dir-1-calls))
                            (setq frame-b (edmacs-frames-for-repo
                                           (expand-file-name ".git" repo-b)))
                            (should (frame-live-p frame-b))
                            (should (not (eq frame-b frame-a)))
                            (should (equal (buffer-file-name
                                            (with-selected-frame frame-b
                                              (window-buffer (edmacs-frames-live-test--main-window frame-b))))
                                          b-file))
                            (should (eq (window-buffer (edmacs-frames-live-test--main-window frame-a))
                                       prior-buf))))
                      (when (and frame-b (frame-live-p frame-b))
                        (let ((buf (edmacs-sidebar--buffer frame-b)))
                          (when (buffer-live-p buf) (kill-buffer buf)))
                        (delete-frame frame-b))
                      (let ((b (find-buffer-visiting b-file)))
                        (when b (kill-buffer b))))))
              nil)))))

    ;; ==========================================================================
    ;; AC7 -- SPC F switch/cycle between repo frames
    ;; ==========================================================================

    (ert-deftest edmacs-frames-live-test-switch-and-cycle ()
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo-a (expand-file-name "repoA" sandbox))
               (repo-b (expand-file-name "repoB" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo-a)
          (edmacs-frames-live-test--make-git-repo repo-b)
          (edmacs-frames-live-test--with-frames (fa fb)
            (with-selected-frame fa (edmacs-frames-open repo-a))
            (with-selected-frame fb (edmacs-frames-open repo-b))
            (let* ((real-a (edmacs-frames-for-repo (edmacs-frames--repo-of repo-a)))
                   (real-b (edmacs-frames-for-repo (edmacs-frames--repo-of repo-b))))
              (edmacs-frames-switch (frame-parameter real-a 'name))
              (should (eq (selected-frame) real-a))
              (edmacs-frames-switch (frame-parameter real-b 'name))
              (should (eq (selected-frame) real-b))
              ;; Cycling twice through a 2-frame set returns to the start.
              (let ((start (selected-frame)))
                (edmacs-frames-next-frame)
                (edmacs-frames-next-frame)
                (should (eq (selected-frame) start))))))))

    ;; ==========================================================================
    ;; AC8 -- closing a frame's last tab
    ;; ==========================================================================

    (ert-deftest edmacs-frames-live-test-close-last-tab-deletes-non-last-frame ()
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo-a (expand-file-name "repoA" sandbox))
               (repo-b (expand-file-name "repoB" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo-a)
          (edmacs-frames-live-test--make-git-repo repo-b)
          (edmacs-frames-live-test--with-frames (fa fb)
            (with-selected-frame fa (edmacs-frames-open repo-a))
            (with-selected-frame fb (edmacs-frames-open repo-b))
            (let ((count-before (length (frame-list)))
                  (real-b (edmacs-frames-for-repo (edmacs-frames--repo-of repo-b))))
              (with-selected-frame real-b (tab-bar-close-tab))
              (should (= (1- count-before) (length (frame-list)))))))))

    (ert-deftest edmacs-frames-live-test-close-last-tab-resets-sole-frame ()
      "On the daemon's only frame, closing its last tab leaves a scratch tab."
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo (expand-file-name "repoA" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo)
          (cl-letf (((symbol-function 'frame-list) (lambda () (list (selected-frame)))))
            (edmacs-frames-open repo)
            (should (= 1 (length (tab-bar-tabs))))
            (tab-bar-close-tab)
            (should (= 1 (length (tab-bar-tabs))))
            (should-not (frame-parameter nil 'edmacs-repo))
            (should (equal (buffer-name (window-buffer (selected-window))) "*scratch*"))
            (let ((buf (edmacs-sidebar--buffer (selected-frame))))
              (when (buffer-live-p buf) (kill-buffer buf)))))))

    (provide 'frames-live-test)))
;;; frames-live-test.el ends here
