;;; sidebar-test.el --- Tests for sidebar.el -*- lexical-binding: t -*-

;;; Commentary:
;; Unlike claude-term-test.el, sidebar.el's parent mode (`magit-section-mode')
;; is genuinely load-bearing to even parse the file under `-Q --batch':
;; `define-derived-mode' errors if `magit-section-mode' is undefined, and
;; sidebar.el's own `(require 'magit-section)' errors even earlier if
;; `load-path' lacks it. So this file carries its own, different
;; invocation:
;;
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/sidebar-test.el -f ert-run-tests-batch-and-exit
;;
;; Note sidebar.el is NOT passed on the command line -- this file fixes
;; `load-path' against the straight build tree and loads sidebar.el itself,
;; below, so sidebar.el's own `(require 'magit-section)' succeeds. If
;; neither this checkout nor its sibling main `edmacs' checkout has ever
;; bootstrapped straight, the whole suite reports a single skip rather than
;; erroring out on file load.
;;
;; `edmacs-sidebar-test-per-frame-buffers-distinct-and-delete-frame-scoped'
;; (AC2: per-frame buffers, delete-frame scoping) needs a second real
;; frame, which needs a controlling terminal to attach to -- plain `-Q
;; --batch' with no pty has none, so that one test skips cleanly under
;; the invocation above. To actually exercise it, wrap the same
;; invocation in `script' to attach a pty:
;;
;;   script -q /dev/null emacs -Q --batch -l ert \
;;         -l modules/git-common-dir.el -l modules/sidebar-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; This draws real terminal escape sequences to that pty as a side
;; effect (the second frame is a live tty frame) -- harmless, but expect
;; screen-clear/cursor codes in the raw output.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; Disabled file-wide, before anything below loads magit-section (and its
;; five transitive deps) or evil: on a natively-compiled Emacs,
;; `advice-add' on a primitive subr (`select-window', `use-global-map',
;; `read-key-sequence', `set-window-buffer', etc. -- exactly the sort of
;; thing evil/magit-section/this suite's own tests do) makes Emacs spawn
;; a whole second `emacs -Q --batch -l <trampoline>.el' subprocess on the
;; spot to compile a native "trampoline" for it, so the advice still
;; takes effect from already-native-compiled callers. That is a real
;; `call-process' invocation, but Emacs's own internal compiler
;; plumbing, not sidebar.el's -- confirmed via a live backtrace showing
;; `comp-subr-trampoline-install' as the caller, entirely independent of
;; `native-comp-jit-compilation' (which only gates compiling freshly
;; loaded .el source and does not affect this).
(setq native-comp-enable-subr-trampolines nil)

(defun edmacs-sidebar-test--locate-straight-build-root ()
  "Return this checkout's `straight/build' directory, or nil.
Tries this checkout's own `straight/build' first -- present once this
worktree has itself been opened as a real Emacs config and straight has
bootstrapped it -- then falls back to the sibling main `edmacs' checkout's
`straight/build', the same worktree-vs-sibling-main-checkout fallback
`claude-term-test--locate-real-rotate' uses: a roadmap worktree lives
under `<parent>/edmacs__worktrees/<name>', sibling to the main
`<parent>/edmacs' checkout, and straight's build cache is per-checkout,
not shared."
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

(defun edmacs-sidebar-test--add-magit-section-deps (build-root)
  "Add `magit-section' and its transitive deps under BUILD-ROOT to `load-path'.
cl-lib, eieio, subr-x, format-spec, and cursor-sensor ship with Emacs core
and need no straight resolution; only these do."
  (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section"))
    (let ((dir (expand-file-name dep build-root)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(defvar edmacs-sidebar-test--build-root
  (edmacs-sidebar-test--locate-straight-build-root)
  "This checkout's (or its sibling main checkout's) `straight/build' root.
Also reused by the rotate.el lookup below -- a second, independent
optional straight dependency.")

(if (null edmacs-sidebar-test--build-root)

    (ert-deftest edmacs-sidebar-test-magit-section-unavailable ()
      (ert-skip "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (edmacs-sidebar-test--add-magit-section-deps edmacs-sidebar-test--build-root)
    (load (expand-file-name "modules/sidebar.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Test helpers
    ;; ==========================================================================

    (defmacro edmacs-sidebar-test--with-extra-tab (&rest body)
      "Run BODY after adding one tab, restoring the original tab count after.
Cleanup runs via `unwind-protect' regardless of BODY's outcome -- every
test in this file shares the same real frame, so a failing assertion
must never leave stray tabs behind for a later test."
      (declare (indent 0))
      `(let ((edmacs-sidebar-test--tab-count-before (length (tab-bar-tabs))))
         (unwind-protect
             (progn (tab-bar-new-tab) ,@body)
           (while (> (length (tab-bar-tabs)) edmacs-sidebar-test--tab-count-before)
             (tab-bar-close-tab)))))

    (defun edmacs-sidebar-test--cleanup-sidebar (frame)
      "Hide and kill FRAME's sidebar window/buffer, if any."
      (edmacs-sidebar-hide frame)
      (let ((buf (edmacs-sidebar--buffer frame)))
        (when (buffer-live-p buf)
          (kill-buffer buf))
        (set-frame-parameter frame 'edmacs-sidebar-buffer nil)))

    ;; ==========================================================================
    ;; AC1 -- redraw content, marker, RET-driven visit, 1-based numbering,
    ;; frame-explicit tab-index lookups
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-redraw-lists-both-tabs-with-marker ()
      (edmacs-sidebar-test--with-extra-tab
        (unwind-protect
            (progn
              (edmacs-sidebar-show (selected-frame))
              (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                (let ((text (buffer-string)))
                  (should (= 2 (length (split-string text "\n" t))))
                  (should (= 1 (cl-count ?● text)))
                  (should (= 1 (cl-count ?○ text))))))
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))

    (ert-deftest edmacs-sidebar-test-first-tab-section-value-is-one-not-zero ()
      (edmacs-sidebar-test--with-extra-tab
        (unwind-protect
            (progn
              (edmacs-sidebar-show (selected-frame))
              (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                (goto-char (point-min))
                (should (= 1 (oref (magit-current-section) value)))))
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))

    (ert-deftest edmacs-sidebar-test-visit-tab-selects-and-moves-marker ()
      (edmacs-sidebar-test--with-extra-tab
        (unwind-protect
            (progn
              (edmacs-sidebar-show (selected-frame))
              ;; The newly-added tab is current, at index 1.
              (should (= 1 (tab-bar--current-tab-index)))
              (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                (goto-char (point-min))
                (edmacs-sidebar-visit-tab))
              ;; RET on the first (non-current) row actually selected it --
              ;; not a no-op under `tab-bar-select-tab's 0-as-sentinel
              ;; semantics, and not off-by-one to the tab before it.
              (should (= 0 (tab-bar--current-tab-index)))
              (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                (goto-char (point-min))
                (should (looking-at-p "●"))))
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))

    (defun edmacs-sidebar-test--locate-straight-repos-root ()
      "Return this checkout's `straight/repos' directory, or its sibling
main checkout's -- the same fallback `edmacs-sidebar-test--locate-straight-build-root'
uses for `straight/build'. Needed only as a fallback for `evil' below,
whose `straight/build/evil' symlink can point at a worktree that has
itself never bootstrapped straight (no `straight/repos' of its own),
in which case `straight/repos/evil' -- straight's raw git checkout,
identical content for a pure-elisp package with no build-time file
subsetting -- still resolves."
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

    (defun edmacs-sidebar-test--locate-real-evil ()
      "Return the directory holding the real `evil.el', or nil.
Tries `straight/build/evil' first (file-exists-p follows a working
symlink); falls back to `straight/repos/evil' when that symlink is
broken or the build tree was never generated."
      (or
       (let* ((root (or edmacs-sidebar-test--build-root
                         (edmacs-sidebar-test--locate-straight-build-root)))
              (path (and root (expand-file-name "evil/evil.el" root))))
         (and path (file-exists-p path) (file-name-directory path)))
       (let* ((root (edmacs-sidebar-test--locate-straight-repos-root))
              (path (and root (expand-file-name "evil/evil.el" root))))
         (and path (file-exists-p path) (file-name-directory path)))))

    (defun edmacs-sidebar-test--ensure-real-evil ()
      "Load the real `evil', skipping the calling test if unavailable.
A second, independent optional straight dependency from `magit-section'
and `rotate.el' -- pure elisp, no external deps beyond stock Emacs."
      (unless (featurep 'evil)
        (let ((dir (edmacs-sidebar-test--locate-real-evil)))
          (unless dir
            (ert-skip "evil's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this test"))
          (let ((load-path (cons dir load-path)))
            (require 'evil)))))

    (ert-deftest edmacs-sidebar-test-ret-and-q-resolve-through-real-evil-keymaps ()
      "Regression test for the RET-shadowed-by-evil-motion-state fix.
A plain `define-key' on `edmacs-sidebar-mode-map' alone is invisible to
real key lookup in motion state: evil's state keymaps are installed via
`emulation-mode-map-alists', consulted BEFORE the buffer's local map, and
`evil-motion-state-map' already binds RET to `evil-ret'. Calling
`edmacs-sidebar-visit-tab'/`edmacs-sidebar-hide' directly as Lisp
functions (as the tests above do) cannot catch this -- only dispatching
through the real, active keymaps the way a keypress does can."
      (edmacs-sidebar-test--ensure-real-evil)
      (unwind-protect
          (progn
            (evil-mode 1)
            (with-temp-buffer
              (edmacs-sidebar-mode)
              (evil-motion-state)
              (should (eq evil-state 'motion))
              (should (eq (key-binding (kbd "RET")) #'edmacs-sidebar-visit-tab))
              (should (eq (key-binding (kbd "q")) #'edmacs-sidebar-hide))))
        (evil-mode -1)))

    (ert-deftest edmacs-sidebar-test-redraw-passes-tabs-and-frame-explicitly ()
      "Regression test for the frame-mismatch fix.
Every `tab-bar--tab-index' call inside redraw must pass TABS/FRAME
explicitly, never rely on the 0-arg form's `(selected-frame)' default --
the 0-arg form would silently return nil for a tab belonging to a
non-selected frame, making that frame's rows non-selectable via RET."
      (edmacs-sidebar-test--with-extra-tab
        (unwind-protect
            (let ((calls nil))
              (cl-letf* ((orig (symbol-function 'tab-bar--tab-index))
                         ((symbol-function 'tab-bar--tab-index)
                          (lambda (tab &optional tabs frame)
                            (push (cons tabs frame) calls)
                            (funcall orig tab tabs frame))))
                (edmacs-sidebar-show (selected-frame)))
              (should calls)
              (dolist (call calls)
                (should (car call))
                (should (cdr call))))
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))

    ;; ==========================================================================
    ;; AC2 -- per-frame buffers; delete-frame kills only that frame's buffer
    ;; ==========================================================================
    ;; `-Q --batch' generally cannot open a second real frame (no controlling
    ;; terminal to attach it to) -- an environment limitation, not a missing
    ;; optional package, but the resulting `ert-skip' follows the same
    ;; "second, independent thing this suite depends on but can't always
    ;; have" convention `claude-term-test--ensure-real-rotate' uses for
    ;; rotate.el.

    (defun edmacs-sidebar-test--make-second-frame-or-skip ()
      "Return a second real frame on this process's controlling terminal, or skip.
Passes `tty'/`tty-type' explicitly rather than relying on `window-system'
alone: with no controlling terminal at all (the common `-Q --batch' case,
run with no pty attached) opening \"/dev/tty\" fails and this skips, same
as before. But run under a pty (e.g. `script -q /dev/null emacs -Q
--batch ...') \"/dev/tty\" does exist, and `tty-type' is hardcoded to
\"xterm\" rather than inherited from `$TERM' because the invoking shell's
own terminal type (e.g. \"xterm-ghostty\") may have no terminfo entry on
this machine, which would otherwise fail with \"Unknown terminal type\"
even though a real controlling terminal is attached; \"xterm\" is close
to universally present in terminfo databases."
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

    (ert-deftest edmacs-sidebar-test-per-frame-buffers-distinct-and-delete-frame-scoped ()
      (let* ((f1 (selected-frame))
             (f2 (edmacs-sidebar-test--make-second-frame-or-skip)))
        (unwind-protect
            (progn
              (edmacs-sidebar-show f1)
              (with-selected-frame f2 (edmacs-sidebar-show f2))
              (let ((buf1 (edmacs-sidebar--buffer f1))
                    (buf2 (edmacs-sidebar--buffer f2)))
                (should (buffer-live-p buf1))
                (should (buffer-live-p buf2))
                (should-not (eq buf1 buf2))
                ;; RET on f2's (non-selected-relative-to-f1) buffer still
                ;; selects the right tab on f2 -- exercises the same
                ;; frame-explicit lookup as the regression test above,
                ;; against a genuinely different frame.
                (with-current-buffer buf2
                  (goto-char (point-min))
                  (edmacs-sidebar-visit-tab))
                (should (= 0 (with-selected-frame f2 (tab-bar--current-tab-index))))
                (delete-frame f2)
                (should-not (buffer-live-p buf2))
                (should (buffer-live-p buf1))))
          (edmacs-sidebar-test--cleanup-sidebar f1)
          (when (frame-live-p f2) (delete-frame f2)))))

    ;; ==========================================================================
    ;; AC3 -- SPC T n/d/r wiring
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-post-open-shows-sidebar-in-new-tab ()
      (edmacs-sidebar-test--with-extra-tab
        (unwind-protect
            (progn
              (edmacs-sidebar--on-tab-open nil)
              (should (edmacs-sidebar--window (selected-frame))))
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))

    (ert-deftest edmacs-sidebar-test-pre-close-redraw-removes-closed-tab-row ()
      (edmacs-sidebar-test--with-extra-tab
        (unwind-protect
            (progn
              (edmacs-sidebar-show (selected-frame))
              (should (= 2 (length (tab-bar-tabs))))
              ;; Closes the current (newly-added) tab.
              (tab-bar-close-tab)
              ;; `sit-for' alone does not run pending timers under `-Q
              ;; --batch'; a real sleep is needed to let the deferred
              ;; `run-at-time 0' redraw actually fire.
              (sleep-for 0.2)
              (sit-for 0)
              (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                (should (= 1 (length (tab-bar-tabs))))
                (should (= 1 (length (split-string (buffer-string) "\n" t))))))
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))

    (ert-deftest edmacs-sidebar-test-rename-advice-redraws ()
      (unwind-protect
          (progn
            (edmacs-sidebar-show (selected-frame))
            (tab-bar-rename-tab "edmacs-sidebar-test-renamed")
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (should (string-match-p "edmacs-sidebar-test-renamed" (buffer-string)))))
        (ignore-errors (tab-bar-rename-tab ""))
        (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))

    ;; ==========================================================================
    ;; AC4 -- window never selected/deleted; rotate-layout leaves it
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-window-parameters-block-other-window-and-c-x-1 ()
      (unwind-protect
          (let ((win (edmacs-sidebar-show (selected-frame)))
                (ordinary (selected-window)))
            (should win)
            (should (window-parameter win 'no-other-window))
            (should (window-parameter win 'no-delete-other-windows))
            (should (window-dedicated-p win))
            (select-window ordinary)
            (other-window 1)
            (should-not (eq (selected-window) win))
            (delete-other-windows)
            (should (window-live-p win)))
        (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))

    (defun edmacs-sidebar-test--locate-real-rotate ()
      "Return the path to the real `rotate.el' straight build, or nil.
Reuses `edmacs-sidebar-test--build-root's own worktree-vs-sibling-main-
checkout resolution -- rotate.el is a second, independent optional
straight dependency from magit-section. Falls back to
`straight/repos/emacs-rotate' (the package's repo name differs from its
feature name) the same way `edmacs-sidebar-test--locate-real-evil' falls
back to `straight/repos/evil', for the same broken-build-symlink case."
      (or
       (let ((root (or edmacs-sidebar-test--build-root
                        (edmacs-sidebar-test--locate-straight-build-root))))
         (when root
           (let ((path (expand-file-name "rotate/rotate.el" root)))
             (and (file-exists-p path) path))))
       (let ((root (edmacs-sidebar-test--locate-straight-repos-root)))
         (when root
           (let ((path (expand-file-name "emacs-rotate/rotate.el" root)))
             (and (file-exists-p path) path))))))

    (defun edmacs-sidebar-test--ensure-real-rotate ()
      "Load the real `rotate.el', skipping the calling test if unavailable."
      (unless (featurep 'rotate)
        (let ((path (edmacs-sidebar-test--locate-real-rotate)))
          (unless path
            (ert-skip "rotate.el's straight build was not found in this \
checkout or its sibling main checkout; bootstrap straight once (open this \
worktree in a real Emacs session) to enable this test"))
          (load path nil t))))

    (ert-deftest edmacs-sidebar-test-rotate-layout-preserves-sidebar-window ()
      (edmacs-sidebar-test--ensure-real-rotate)
      (unwind-protect
          (let ((rotate-skip-dedicated-windows t)
                (main-buf (generate-new-buffer "edmacs-sidebar-test-rotate-main"))
                (other-buf (generate-new-buffer "edmacs-sidebar-test-rotate-other")))
            (unwind-protect
                (progn
                  (delete-other-windows)
                  (set-window-buffer (selected-window) main-buf)
                  (let* ((main-win (selected-window))
                         (other-win (split-window main-win nil 'below)))
                    (set-window-buffer other-win other-buf)
                    (select-window main-win)
                    (let ((side-win (edmacs-sidebar-show (selected-frame))))
                      (should side-win)
                      (select-window main-win)
                      (rotate-window)
                      (should (window-live-p side-win))
                      (should (eq (window-parameter side-win 'window-side) 'left)))))
              (kill-buffer main-buf)
              (kill-buffer other-buf)))
        (edmacs-sidebar-test--cleanup-sidebar (selected-frame))
        (delete-other-windows)))

    ;; ==========================================================================
    ;; AC5 -- toggle hides/reshows at the same width
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-toggle-preserves-width ()
      (unwind-protect
          (let* ((win (edmacs-sidebar-show (selected-frame)))
                 (width (window-width win)))
            (edmacs-sidebar-toggle)
            (should-not (edmacs-sidebar--window (selected-frame)))
            (edmacs-sidebar-toggle)
            (let ((win2 (edmacs-sidebar--window (selected-frame))))
              (should win2)
              (should (<= (abs (- (window-width win2) width)) 1))))
        (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))

    ;; ==========================================================================
    ;; AC6 -- top strip gone; SPC T l still works
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-tab-bar-show-nil-mode-untouched ()
      (should (null tab-bar-show))
      (should (fboundp 'tab-bar-switch-to-tab)))

    (ert-deftest edmacs-sidebar-test-switch-to-tab-still-works ()
      (edmacs-sidebar-test--with-extra-tab
        (unwind-protect
            (let ((first-name (alist-get 'name (car (tab-bar-tabs)))))
              (tab-bar-switch-to-tab first-name)
              (should (= 0 (tab-bar--current-tab-index))))
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))

    ;; ==========================================================================
    ;; AC7 -- desktop/daemon restore regenerates a live sidebar
    ;; ==========================================================================
    ;; A genuine `desktop-save'/`desktop-read' round trip cannot run here:
    ;; `desktop-read' is unconditionally a no-op under `-Q --batch' --
    ;; "This function is a no-op when Emacs is running in batch mode",
    ;; straight from its own docstring, confirmed empirically (it prints
    ;; "Not reloading the desktop" and never fires
    ;; `desktop-after-read-hook' regardless of lock state or
    ;; `desktop-file-modtime'). So these tests instead call the exact,
    ;; named functions sidebar.el registers on the two restore hooks
    ;; directly -- the same functions a real restore would invoke -- and
    ;; assert they leave each frame with a live, freshly-populated
    ;; sidebar rather than none/a stale one. This is real coverage of
    ;; sidebar.el's own regeneration logic, even though the surrounding
    ;; desktop.el/daemon machinery itself stays a manual M-x checklist
    ;; (see the commit body) per the phase's own step 9 fallback.

    (ert-deftest edmacs-sidebar-test-desktop-excludes-sidebar-mode ()
      (should (memq 'edmacs-sidebar-mode desktop-modes-not-to-save)))

    (ert-deftest edmacs-sidebar-test-desktop-after-read-hook-regenerates-sidebar ()
      "Simulates the state a real restore leaves a frame in -- no live
sidebar buffer yet, since `edmacs-sidebar-mode' is excluded from the
saved desktop -- and asserts the function registered on
`desktop-after-read-hook' produces a fresh, correctly-populated one.
`edmacs-sidebar-test--cleanup-sidebar' forces that starting state
explicitly rather than assuming it: AC3's own post-open hook
(`edmacs-sidebar--on-tab-open') already auto-shows the sidebar as soon
as `edmacs-sidebar-test--with-extra-tab's `tab-bar-new-tab' runs, so a
live buffer already exists by this point and must be torn down first to
model \"freshly restored, buffer excluded from the save\" rather than
\"already showing\"."
      (edmacs-sidebar-test--with-extra-tab
        (unwind-protect
            (progn
              (edmacs-sidebar-test--cleanup-sidebar (selected-frame))
              (should-not (edmacs-sidebar--buffer (selected-frame)))
              (edmacs-sidebar--on-desktop-read)
              (let ((buf (edmacs-sidebar--buffer (selected-frame))))
                (should (buffer-live-p buf))
                (with-current-buffer buf
                  (should (= 2 (length (split-string (buffer-string) "\n" t)))))))
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))

    (ert-deftest edmacs-sidebar-test-regenerate-after-frame-shows-sidebar-once-deferred ()
      "Direct regression test for the daemon-restart path's own function
\(`edmacs-sidebar--regenerate-after-frame', registered on
`after-make-frame-functions' at depth 100). Unlike sessions.el's own
frameset-restore hook, this one is not gated on `display-graphic-p' -- it
must show a fresh sidebar once its `run-at-time 0' fires, even on a
non-graphical batch frame."
      (unwind-protect
          (progn
            (should-not (edmacs-sidebar--buffer (selected-frame)))
            (edmacs-sidebar--regenerate-after-frame (selected-frame))
            ;; Deferred -- must not have run synchronously.
            (should-not (edmacs-sidebar--buffer (selected-frame)))
            (sleep-for 0.2)
            (sit-for 0)
            (should (buffer-live-p (edmacs-sidebar--buffer (selected-frame)))))
        (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))

    ;; ==========================================================================
    ;; AC8 -- no subprocess work during redraw/hook activity
    ;; ==========================================================================
    ;; The phase's own step 9 fallback treats "M-x profiler-start over a
    ;; minute of interactive tab switching" as not ERT-automatable, but the
    ;; underlying property it checks -- sidebar.el never shells out -- is:
    ;; advise every subprocess primitive to signal instead of run, then
    ;; drive every redraw/hook path here many times over. This is a
    ;; stronger guarantee than the manual profiler pass (it catches an
    ;; indirect call through a variable, not just a `grep'-visible
    ;; literal) and needs neither a display nor real wall-clock time.
    ;; (`native-comp-enable-subr-trampolines' is disabled file-wide,
    ;; above, so `advice-add' on a primitive below can't spawn Emacs's
    ;; own trampoline-compiler subprocess and get misattributed to
    ;; sidebar.el.)

    (ert-deftest edmacs-sidebar-test-redraw-and-hooks-never-shell-out ()
      (edmacs-sidebar-test--with-extra-tab
        (let ((violations nil)
              (guarded '(call-process call-process-region process-file
                         start-process start-file-process make-process)))
          (unwind-protect
              (progn
                (dolist (fn guarded)
                  (advice-add fn :before
                              (lambda (&rest _) (push fn violations))
                              `((name . ,(intern (format "edmacs-sidebar-test--guard-%s" fn))))))
                (edmacs-sidebar-show (selected-frame))
                (dotimes (_ 50)
                  (edmacs-sidebar--redraw (selected-frame))
                  (edmacs-sidebar--on-tab-select nil nil)
                  (edmacs-sidebar--on-tab-open nil)
                  (edmacs-sidebar--on-tab-pre-close nil nil)
                  (tab-bar-rename-tab "edmacs-sidebar-test-shellout-check"))
                (sleep-for 0.2)
                (sit-for 0)
                (should-not violations))
            (dolist (fn guarded)
              (advice-remove fn (intern (format "edmacs-sidebar-test--guard-%s" fn))))
            (ignore-errors (tab-bar-rename-tab ""))
            (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))

    )) ; end of build-root-found branch

;;; sidebar-test.el ends here
