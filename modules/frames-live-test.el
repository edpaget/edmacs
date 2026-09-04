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
;; A third, real-file-notify worktree-discovery test self-preflights a
;; trivial watch before doing any real `git worktree add'/`remove' work: on
;; this suite's own development machine, `--batch' mode's kqueue backend
;; accepts a watch but never actually delivers a callback for a real
;; filesystem change (confirmed independently of this feature's own code),
;; while the identical watch fires immediately under a real `emacs
;; --daemon'. The preflight distinguishes that known `--batch'/kqueue gap
;; from an actual regression, and skips (rather than fails) when it can't
;; be exercised here. Unlike the AC2/AC3/AC4/AC7/AC8 tests above, this one
;; does NOT go through `edmacs-frames-live-test--with-frames': it only
;; ever needs one frame, and forcing a second `(tty . "/dev/tty")' frame
;; would fail outright under the very `emacs --daemon' environment the
;; preflight requires (a daemon has no controlling terminal to open) --
;; it uses the suite's ambient/selected frame instead, which
;; `edmacs-frames-open' adopts as a spare. Manually verified green against
;; a real, throwaway `emacs --daemon' (`emacsclient -e' driving
;; `ert-run-test' directly): `git worktree add'/`remove' on a sandbox repo
;; are reflected in `edmacs-worktrees-for-repo' within the 0.5s debounce
;; window with `edmacs-frames--worktree-refresh-timers' draining back to
;; empty, and no leftover watch after `edmacs-frames--teardown-worktrees-watch'.
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
    ;; windows.el first: sidebar.el `require's it for `edmacs-windows-claim-side'.
    (load (expand-file-name "modules/windows.el" default-directory) nil t)
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

    (defun edmacs-frames-live-test--remove-worktree (repo wt-dir)
      "Remove real git worktree WT-DIR from REPO."
      (let ((default-directory (file-name-as-directory repo)))
        (call-process "git" nil nil nil "worktree" "remove" "--force" wt-dir)))

    (defun edmacs-frames-live-test--wait-until (predicate timeout)
      "Pump the event loop until PREDICATE is non-nil or TIMEOUT seconds pass.
Returns PREDICATE's own final value. `sit-for' (not `sleep-for') is what
actually lets a pending `file-notify' callback run -- it processes
input/timer/subprocess events, `sleep-for' merely blocks."
      (let ((deadline (+ (float-time) timeout)))
        (while (and (< (float-time) deadline) (not (funcall predicate)))
          (sit-for 0.05))
        (funcall predicate)))

    (defun edmacs-frames-live-test--file-notify-delivers-p ()
      "Return non-nil iff a real `file-notify' watch actually delivers here.
`--batch' mode's own event loop does not service every backend the same
way an interactive/daemon session's does -- confirmed on this suite's own
kqueue backend (macOS): `file-notify-add-watch' succeeds and returns a
live descriptor, but a real filesystem change underneath it never
invokes the callback no matter how long `sit-for'/`accept-process-output'
is pumped, while the identical watch fires immediately against a real
`emacs --daemon'. Preflighting this trivial case, rather than skipping
only on `file-notify-add-watch' itself signaling, is what actually
distinguishes \"this backend cannot deliver under `--batch' here\" from
a genuine regression in this feature's own watch plumbing."
      (let* ((dir (make-temp-file "edmacs-frames-fnprobe-" t))
             (probe (expand-file-name "probe" dir))
             (fired nil)
             (desc (ignore-errors
                     (file-notify-add-watch dir '(change) (lambda (_ev) (setq fired t))))))
        (unwind-protect
            (progn
              (when desc
                (write-region "x" nil probe nil 'silent)
                (edmacs-frames-live-test--wait-until (lambda () fired) 2.0))
              fired)
          (when desc (ignore-errors (file-notify-rm-watch desc)))
          (delete-directory dir t))))

    (defun edmacs-frames-live-test--current-tab-display-name ()
      "Return the selected frame's current tab's actually-displayed name.
`tab-bar-tab-name-current' (despite its name) is only one possible VALUE
of `tab-bar-tab-name-function' -- calling it directly ignores an
explicitly-renamed tab's own stored name and instead derives one fresh
from the selected window's buffer, exactly the wrong thing to assert
against here. This mirrors tab-bar.el's own rendering rule instead:
an explicitly-named tab's stored `name' wins; otherwise the configured
`tab-bar-tab-name-function' is consulted live."
      (let ((tab (tab-bar--current-tab)))
        (if (alist-get 'explicit-name tab)
            (alist-get 'name tab)
          (funcall tab-bar-tab-name-function))))

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
                    (should (equal (with-selected-frame frame
                                     (edmacs-frames-live-test--current-tab-display-name))
                                   "repoA"))
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
                ;; A brand-new tab for an UNRELATED directory is kept, and
                ;; carries `edmacs-root' -- the reconciliation hook stamps
                ;; every tab it inspects, whether or not it ends up folding it.
                (let* ((other (expand-file-name "unrelated" sandbox)))
                  (make-directory other t)
                  (let ((default-directory (file-name-as-directory other)))
                    (dired other)
                    (tab-bar-new-tab))
                  (should (= (1+ tab-count) (length (tab-bar-tabs frame))))
                  (should (equal (edmacs-frames--tab-root (tab-bar--current-tab))
                                 (file-truename (file-name-as-directory other)))))))))))

    (ert-deftest edmacs-frames-live-test-ac4-other-tab-command-then-switch-project ()
      "Literally drives `M-x project-other-tab-command' followed by `p'
\(`project-switch-project', really bound to it in `project-prefix-map')
through this config's actual `project-switch-commands' value,
`edmacs-frames-open-project'. Because a frame is a repo, \"switching into
the current worktree\" here means switching into the CURRENT FRAME's own
repo -- the only kind of root `project-switch-project' can ever offer once
`__worktrees/' roots are excluded from `project-known-project-roots' (see
AC1/AC5 in core.el) -- which `edmacs-frames-open' resolves to the frame
already selected: a no-op that must touch no tab. Switching into a
brand-new repo instead raises/creates THAT repo's own frame with one tab,
never adding a tab to the frame the command was invoked from."
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo-a (expand-file-name "repoA" sandbox))
               (repo-z (expand-file-name "repoZ" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal))
               ;; Never touch the real user's own project list.
               (project-list-file (expand-file-name "projects.eld" sandbox))
               (project--list 'unset)
               ;; This file loads `frames.el' alone, not `core.el' -- the
               ;; module that actually wires this in a real session. Left
               ;; at `project.el's own stock alist default, dispatch would
               ;; prompt for a further keystroke and hang batch Emacs.
               (project-switch-commands #'edmacs-frames-open-project))
          (edmacs-frames-live-test--make-git-repo repo-a)
          (edmacs-frames-live-test--make-git-repo repo-z)
          (cl-macrolet
              ((other-tab-command-then-switch-project (target-dir)
                 ;; `other-tab-prefix' (armed by `project-other-tab-command')
                 ;; installs its transient keymap and its one-shot
                 ;; `display-buffer' override as raw global state, both meant
                 ;; to self-clear via `pre-command-hook'/`post-command-hook'
                 ;; in the real command loop once "the next command" runs --
                 ;; here, that next command is exactly the `call-interactively'
                 ;; below, so a `let' scoped to just this one arm-then-dispatch
                 ;; pair reproduces that same one-command boundary: unwound
                 ;; before the NEXT pair's own `project-other-tab-command' can
                 ;; stack a second live override on top of a first one nothing
                 ;; ever consumed (which is what a real command loop's
                 ;; intervening `post-command-hook' run prevents, and what a
                 ;; single shared `let' around both pairs previously failed to
                 ;; -- two stacked overrides both firing on the second pair's
                 ;; `dired' + `edmacs-sidebar-show' calls, each re-entering the
                 ;; other before either could clear itself: `excessive-lisp-nesting').
                 `(let ((overriding-terminal-local-map nil)
                        (display-buffer-overriding-action (cons nil nil))
                        (switch-to-buffer-obey-display-actions nil))
                    (call-interactively #'project-other-tab-command)
                    (let ((project-prompter (lambda () ,target-dir)))
                      (call-interactively #'project-switch-project)))))
            ;; DECOY is never used for anything -- it exists purely so the
            ;; batch process always has a third live frame in reserve.
            ;; `edmacs-frames-open''s spare-frame adoption (see
            ;; `edmacs-frames--spare-frame') may claim ANY repo-less frame,
            ;; including the batch process's own pre-existing ambient one --
            ;; with only FRAME-A on hand, both `edmacs-frames-open' calls
            ;; below (repo-a, then repo-z) between them can exhaust every
            ;; repo-less frame there is, and this test's own cleanup then
            ;; deleting the resulting repo-z frame would try to delete the
            ;; LAST live frame in the whole process -- an error, not a
            ;; tab/frame-routing bug -- when nothing else survives to hold
            ;; the line.
            (edmacs-frames-live-test--with-frames (frame-a decoy)
              (with-selected-frame frame-a
                (edmacs-frames-open repo-a))
              ;; Never assume the ORIGINAL `frame-a' object is the one
              ;; `edmacs-frames-open' actually claimed for repo-a -- spare-frame
              ;; adoption may have picked a different repo-less frame instead
              ;; (the ambient one, or DECOY); re-resolve by repo, matching
              ;; `edmacs-frames-live-test-worktree-tab-routes-and-dedupes's own
              ;; idiom just above.
              (setq frame-a (edmacs-frames-for-repo (edmacs-frames--repo-of repo-a)))
              (with-selected-frame frame-a
                ;; A second, unrelated tab, so the frame really has two open.
                (let ((default-directory (file-name-as-directory repo-a)))
                  (tab-bar-new-tab))
                (let ((tab-count (length (tab-bar-tabs frame-a)))
                      (closed-before (length tab-bar-closed-tabs))
                      frame-z)
                  (unwind-protect
                      (progn
                        (other-tab-command-then-switch-project repo-a)
                        (should (= tab-count (length (tab-bar-tabs frame-a))))
                        (should (= closed-before (length tab-bar-closed-tabs)))
                        (should (eq (selected-frame) frame-a))
                        (other-tab-command-then-switch-project repo-z)
                        (setq frame-z (edmacs-frames-for-repo (edmacs-frames--repo-of repo-z)))
                        (should (frame-live-p frame-z))
                        (should (not (eq frame-z frame-a)))
                        (should (= 1 (length (tab-bar-tabs frame-z))))
                        (should (edmacs-frames--tab-root (car (tab-bar-tabs frame-z))))
                        (should (= tab-count (length (tab-bar-tabs frame-a))))
                        (should (= closed-before (length tab-bar-closed-tabs))))
                    (when (and frame-z (frame-live-p frame-z)
                               (not (eq frame-z frame-a)) (not (eq frame-z decoy)))
                      (let ((buf (edmacs-sidebar--buffer frame-z)))
                        (when (buffer-live-p buf) (kill-buffer buf)))
                      (delete-frame frame-z))))))))))

    ;; ==========================================================================
    ;; AC9 -- tab names inside a repo frame show only the worktree name
    ;; ==========================================================================
    ;; `edmacs-frames-open-project's AC2 test above already asserts the
    ;; EXPLICIT-rename path (this module's own `tab-bar-rename-tab' calls).
    ;; This covers the other one: `edmacs-sessions--tab-name' itself, the
    ;; DYNAMIC namer a tab with no explicit name (a plain `SPC T n', or one
    ;; restored by desktop) falls back to.

    (defvar edmacs-frames-live-test--sessions-available
      (and edmacs-frames-live-test--build-root
           (dolist (dep '("general" "bufferlo") t)
             (edmacs-frames-live-test--add-dep dep)))
      "Non-nil once sessions.el's own straight deps are on `load-path'.")

    (defmacro edmacs-frames-live-test--with-sessions-tab-namer (&rest body)
      "Load sessions.el's real `edmacs-sessions--tab-name' standalone, run BODY.
Stubs the top-level forms sessions.el uses that this suite has no
business actually running: `use-package' (bufferlo is a real per-tab
buffer-list minor mode with no bearing on tab NAMING) is faked as a
macro that expands to nil, and the two leader/chord registrars
\(`general-define-key', `edmacs-evil-config-add-c-x-chord') -- which
would otherwise need evil-config.el's own much larger dependency tree
-- as plain no-op functions (safe here because every call site passes
already-quoted literals, never something requiring real evaluation).
`user-emacs-directory' is rebound to a fresh sandbox first, exactly as
the AC1/AC5 registrar sandbox below does for core.el, so
`desktop-dirname's directory-creation side effect never touches the
real user's `~/.config/emacs'."
      (declare (indent 0))
      `(if (not edmacs-frames-live-test--sessions-available)
           (ert-skip "general/bufferlo's straight build was not found; \
bootstrap straight once (open this worktree in a real Emacs session) to \
enable this test")
         (let ((user-emacs-directory
                (file-name-as-directory (make-temp-file "edmacs-sessions-test-" t))))
           (cl-letf (((symbol-function 'use-package) (cons 'macro (lambda (&rest _) nil)))
                     ((symbol-function 'general-define-key) (lambda (&rest _) nil))
                     ((symbol-function 'edmacs-evil-config-add-c-x-chord) (lambda (&rest _) nil)))
             (load (expand-file-name "modules/sessions.el" default-directory) nil t)))
         ,@body))

    (ert-deftest edmacs-frames-live-test-tab-name-drops-repo-prefix-in-own-frame ()
      (edmacs-frames-live-test--with-sessions-tab-namer
        (edmacs-frames-live-test--with-sandbox sandbox
          (let* ((repo-a (expand-file-name "repoA" sandbox))
                 (repo-b (expand-file-name "repoB" sandbox))
                 (wt-b (expand-file-name "repoB-wt" sandbox))
                 (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
            (edmacs-frames-live-test--make-git-repo repo-a)
            (edmacs-frames-live-test--make-git-repo repo-b)
            (edmacs-frames-live-test--add-worktree repo-b wt-b)
            (edmacs-frames-live-test--with-frames (frame)
              (with-selected-frame frame
                (edmacs-frames-open repo-a)
                ;; A plain new tab (no explicit rename, unlike this module's
                ;; own tab-creation routes) visiting the SAME repo the frame
                ;; itself carries drops the repo/ prefix.
                (let ((default-directory (file-name-as-directory repo-a)))
                  (tab-bar-new-tab)
                  (dired repo-a))
                (should (equal (edmacs-frames-live-test--current-tab-display-name) "repoA"))
                ;; A plain new tab visiting a worktree of a DIFFERENT repo
                ;; keeps the disambiguating repo/ prefix.
                (let ((default-directory (file-name-as-directory wt-b)))
                  (tab-bar-new-tab)
                  (dired wt-b))
                (should (equal (edmacs-frames-live-test--current-tab-display-name)
                               "repoB/repoB-wt"))))))))

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
      "On the daemon's only frame, closing its last tab leaves a scratch tab.
Driven from the SIDEBAR window, which is the shape that used to abandon
the reset half-done: a non-interactive `switch-to-buffer' against that
dedicated window popped *scratch* into a right side window, and the
`delete-other-windows' that followed signalled from a side window."
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo (expand-file-name "repoA" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo)
          (cl-letf (((symbol-function 'frame-list) (lambda () (list (selected-frame)))))
            (edmacs-frames-open repo)
            (should (= 1 (length (tab-bar-tabs))))
            (let ((sidebar (edmacs-sidebar-show (selected-frame))))
              (should (window-live-p sidebar))
              (select-window sidebar))
            (tab-bar-close-tab)
            (should (= 1 (length (tab-bar-tabs))))
            (should-not (frame-parameter nil 'edmacs-repo))
            (let ((main (edmacs-main-window)))
              (should (window-live-p main))
              (should-not (window-parameter main 'window-side))
              (should (equal (buffer-name (window-buffer main)) "*scratch*")))
            (let ((buf (edmacs-sidebar--buffer (selected-frame))))
              (when (buffer-live-p buf) (kill-buffer buf)))))))

    (ert-deftest edmacs-frames-live-test-direct-delete-frame-tears-down-watch ()
      "`edmacs-ns-close-frame' in sessions.el deletes a repo's frame with a
plain `delete-frame' call, never going through `tab-bar-close-tab'/
`tab-bar-close-last-tab-choice' at all -- so the watch teardown must
also fire from the `delete-frame-functions' hook, not only from
`edmacs-frames--close-last-tab'. This drives that exact bare
`delete-frame' path against a repo's last frame and asserts the
watch/timer bookkeeping is gone afterward."
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo-a (expand-file-name "repoA" sandbox))
               (repo-b (expand-file-name "repoB" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo-a)
          (edmacs-frames-live-test--make-git-repo repo-b)
          (edmacs-frames-live-test--with-frames (fa fb)
            (with-selected-frame fa (edmacs-frames-open repo-a))
            (with-selected-frame fb (edmacs-frames-open repo-b))
            (let* ((real-b (edmacs-frames-for-repo (edmacs-frames--repo-of repo-b)))
                   (common-b (edmacs-frames--repo-of repo-b)))
              (unless (gethash common-b edmacs-frames--worktree-watches)
                (ert-skip "no file-notify watch was established for the \
sandbox repo in this environment (backend unavailable?) -- cannot exercise \
teardown-on-delete-frame"))
              ;; Bypass `tab-bar-close-tab' entirely -- exactly what
              ;; `edmacs-ns-close-frame' does.
              (delete-frame real-b t)
              (should-not (gethash common-b edmacs-frames--worktree-watches))
              (should-not (gethash common-b edmacs-frames--worktree-refresh-timers)))))))

    ;; ==========================================================================
    ;; Worktree discovery (edmacs-sidebar roadmap phase 3) -- real
    ;; `workmux add'/`remove' reflected via the real file-notify backend
    ;; ==========================================================================
    ;; Every worktree/file-notify test in frames-test.el stubs
    ;; `file-notify-add-watch'/`-rm-watch' entirely -- appropriate for unit
    ;; coverage of the debounce/upgrade logic, but none of it proves the
    ;; real OS backend actually delivers an event for a real
    ;; `git worktree add'/`remove' the way this feature assumes. This is
    ;; that end-to-end proof, mirroring claude-term-live-test.el's own
    ;; precedent of driving a real subprocess for a claim unit tests can't
    ;; make on their own. `workmux add'/`remove' themselves are a thin
    ;; wrapper over exactly these two git subcommands (per the phase body),
    ;; so exercising `git worktree add'/`remove' directly is the same
    ;; file-notify-visible event this feature actually depends on.

    (ert-deftest edmacs-frames-live-test-real-file-notify-reflects-worktree-add-remove ()
      "A real `git worktree add' (what `workmux add' wraps) populates
`edmacs-worktrees-for-repo' within a couple of seconds via the real
`file-notify' backend and the 0.5s debounce -- no polling. `git worktree
remove' (what `workmux remove' wraps) then removes it the same way."
      (unless (edmacs-frames-live-test--file-notify-delivers-p)
        (ert-skip "the real file-notify backend in this batch environment \
never delivers a callback for an actual filesystem change (confirmed via a \
trivial probe watch, independent of this feature's own code) -- run this \
suite from a real `emacs --daemon' or interactive session to exercise it"))
      (edmacs-frames-live-test--with-sandbox sandbox
        (let* ((repo (expand-file-name "repoA" sandbox))
               (wt (expand-file-name "wt-test" sandbox))
               (edmacs-git-common-dir-cache (make-hash-table :test #'equal))
               (edmacs-frames--worktrees-cache (make-hash-table :test #'equal))
               (edmacs-frames--worktree-watches (make-hash-table :test #'equal))
               (edmacs-frames--worktree-refresh-timers (make-hash-table :test #'equal)))
          (edmacs-frames-live-test--make-git-repo repo)
          ;; Deliberately NOT `edmacs-frames-live-test--with-frames': this
          ;; test only ever needs ONE frame, and that macro's `make-frame'
          ;; call with an explicit `(tty . "/dev/tty")' fails outright under
          ;; a real `emacs --daemon' (no controlling terminal to open) --
          ;; exactly the environment this test's own preflight is written to
          ;; require. The suite's ambient/selected frame (there is always
          ;; one; widened to 200x50 at load time above) is adopted as a
          ;; spare by `edmacs-frames-open' instead, sidestepping the need
          ;; for a second real terminal entirely.
          (let ((frame (selected-frame)) common)
            (unwind-protect
                (progn
                  (with-selected-frame frame (edmacs-frames-open repo))
                  (setq common (edmacs-frames--repo-of repo))
                  (unless (gethash common edmacs-frames--worktree-watches)
                    (ert-skip "no file-notify watch was established for the \
sandbox repo in this environment (backend unavailable?) -- cannot exercise \
real add/remove reflection"))
                  ;; Baseline: only the main worktree, from the synchronous
                  ;; refresh `edmacs-frames-open' already ran.
                  (should (= 1 (length (edmacs-worktrees-for-repo common))))
                  (edmacs-frames-live-test--add-worktree repo wt)
                  (should (edmacs-frames-live-test--wait-until
                           (lambda () (assoc "wt-test" (edmacs-worktrees-for-repo common)))
                           5.0))
                  (should (= 2 (length (edmacs-worktrees-for-repo common))))
                  ;; `git worktree add' writes several files under the new
                  ;; worktree's `worktrees/<name>/' directory, so more than
                  ;; one file-notify event -- and so more than one
                  ;; reschedule of the debounce timer -- can land after the
                  ;; `assoc' above already went true off an earlier one;
                  ;; each reschedule still collapses to a single pending
                  ;; timer (`edmacs-frames--schedule-worktrees-refresh'
                  ;; cancels the old one first), so this settles to zero
                  ;; shortly after, never accumulating a second entry --
                  ;; the concrete "no polling timer" proof.
                  (should (edmacs-frames-live-test--wait-until
                           (lambda () (= 0 (hash-table-count
                                             edmacs-frames--worktree-refresh-timers)))
                           2.0))
                  (should (<= (hash-table-count edmacs-frames--worktree-refresh-timers) 1))
                  (edmacs-frames-live-test--remove-worktree repo wt)
                  (should (edmacs-frames-live-test--wait-until
                           (lambda () (not (assoc "wt-test" (edmacs-worktrees-for-repo common))))
                           5.0))
                  (should (= 1 (length (edmacs-worktrees-for-repo common)))))
              (when common
                (ignore-errors (edmacs-frames--teardown-worktrees-watch common)))
              (when (frame-live-p frame)
                (let ((buf (edmacs-sidebar--buffer frame)))
                  (when (buffer-live-p buf) (kill-buffer buf)))
                (ignore-errors (set-frame-parameter frame 'edmacs-repo nil))))))))

    ;; ==========================================================================
    ;; Fullscreen policy -- a real frame, through the real unstubbed hook
    ;; ==========================================================================

    (ert-deftest edmacs-frames-live-test-fullscreen-leaves-tty-frames-alone ()
      "The half of the policy a real frame can prove in this harness.
Every frame this suite can open is a tty frame -- the same shape as the
daemon's own placeholder and as every `emacsclient -t' frame -- where
`fullscreen' is meaningless and gets mangled by frameset's tty shelving
on the way into a desktop file. Nothing is stubbed here: the frame is
made by the suite's own helper, so the real
`after-make-frame-functions' entry runs, and `sit-for' drains the
zero-delay timer it would have scheduled.

The positive case needs a window system this batch harness has no way
to open; `modules/frames-test.el' covers
`edmacs-frames--fullscreen-target''s graphical branch directly instead."
      (should edmacs-frames-fullscreen)
      (should (memq #'edmacs-frames-apply-fullscreen after-make-frame-functions))
      (edmacs-frames-live-test--with-frames (frame)
        (should-not (display-graphic-p frame))
        (sit-for 0.2)
        (should-not (frame-parameter frame 'fullscreen))))

    (provide 'frames-live-test)))
;;; frames-live-test.el ends here
