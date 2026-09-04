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
;; below, so sidebar.el's own `(require 'magit-section)' succeeds. It loads
;; `modules/windows.el' first, which sidebar.el now `require's for
;; `edmacs-windows-claim-side'; that also installs windows.el's global side
;; effects (`display-buffer-base-action', the `quit-restore-window' advice,
;; the `tab-bar-tab-post-open-functions' hook) into this batch session. If
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
    ;; windows.el first: sidebar.el `require's it for `edmacs-windows-claim-side'.
    (load (expand-file-name "modules/windows.el" default-directory) nil t)
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
                (edmacs-sidebar-activate))
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
`edmacs-sidebar-activate'/`edmacs-sidebar-hide' directly as Lisp
functions (as the tests above do) cannot catch this -- only dispatching
through the real, active keymaps the way a keypress does can.
RET now resolves to `edmacs-sidebar-visit-at-point' (edmacs-sidebar
roadmap phase 6's type-dispatching generalization) rather than directly
to `edmacs-sidebar-activate' -- see
`edmacs-sidebar-test-visit-at-point-dispatches-by-section-type' below
for coverage of the dispatch itself."
      (edmacs-sidebar-test--ensure-real-evil)
      (unwind-protect
          (progn
            (evil-mode 1)
            (with-temp-buffer
              (edmacs-sidebar-mode)
              (evil-motion-state)
              (should (eq evil-state 'motion))
              (should (eq (key-binding (kbd "RET")) #'edmacs-sidebar-visit-at-point))
              (should (eq (key-binding (kbd "q")) #'edmacs-sidebar-hide))))
        (evil-mode -1)))

    (ert-deftest edmacs-sidebar-test-visit-at-point-dispatches-by-section-type ()
      "`edmacs-sidebar-visit-at-point' calls `edmacs-sidebar-agents-visit'
on an `edmacs-sidebar-agent' section, and `edmacs-sidebar-activate' on
every other section type (a plain tab row here) -- sidebar-agents.el
itself is not loaded by this suite, so the agent-visit command is
stubbed."
      (let ((activate-calls 0) (agent-visit-calls 0))
        (cl-letf (((symbol-function 'edmacs-sidebar-activate)
                   (lambda () (setq activate-calls (1+ activate-calls))))
                  ((symbol-function 'edmacs-sidebar-agents-visit)
                   (lambda () (setq agent-visit-calls (1+ agent-visit-calls)))))
          (with-temp-buffer
            (edmacs-sidebar-mode)
            (let ((inhibit-read-only t))
              ;; Both rows nested inside one outer wrapper: an
              ;; unwrapped top-level `magit-insert-section' call
              ;; becomes `magit-root-section' itself and is skipped by
              ;; `magit-section--set-section-properties' (see
              ;; magit-section.el's `magit-insert-section--finish'),
              ;; so two sibling top-level calls here would leave
              ;; neither row's own text actually tagged with its
              ;; section -- exactly the real shape `--redraw' always
              ;; produces via its own wrapping `edmacs-sidebar-root'.
              (magit-insert-section (edmacs-sidebar-root)
                (magit-insert-section (edmacs-sidebar-tab 1)
                  (magit-insert-heading "a tab row"))
                (magit-insert-section (edmacs-sidebar-agent "fake-agent")
                  (magit-insert-heading "an agent row"))))
            (goto-char (point-min))
            (edmacs-sidebar-visit-at-point)
            (should (= 1 activate-calls))
            (should (= 0 agent-visit-calls))
            (goto-char (point-max))
            (forward-line -1)
            (edmacs-sidebar-visit-at-point)
            (should (= 1 activate-calls))
            (should (= 1 agent-visit-calls))))))

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
    ;; Worktree discovery (edmacs-sidebar roadmap phase 3) -- AC1/AC2/AC4
    ;; ==========================================================================
    ;; frames.el is NOT loaded by this suite's invocation (see this file's own
    ;; Commentary), so every frames.el symbol these tests touch --
    ;; `edmacs-worktrees-for-repo', `edmacs-frames--tab-for-root',
    ;; `edmacs-frames--tab-root', `edmacs-frames-open-worktree-tab' -- is
    ;; stubbed via `cl-letf' rather than real; `sidebar.el' only ever calls
    ;; them through its own `declare-function' forward references.

    (defmacro edmacs-sidebar-test--with-repo-frame (common &rest body)
      "Run BODY with the selected frame's `edmacs-repo' set to COMMON.
Restores it to nil afterward, regardless of BODY's outcome -- the
selected frame is shared across this whole test file."
      (declare (indent 1))
      `(unwind-protect
           (progn (set-frame-parameter (selected-frame) 'edmacs-repo ,common)
                  ,@body)
         (set-frame-parameter (selected-frame) 'edmacs-repo nil)))

    (defmacro edmacs-sidebar-test--stub-worktree-lookup (root-alist-var &rest body)
      "Run BODY with frames.el's worktree/tab-root lookups stubbed.
ROOT-ALIST-VAR names a lexical variable holding an alist of
\(TAB . ROOT) associations standing in for `edmacs-root' stamps a real
`edmacs-frames-open-worktree-tab' would have made."
      (declare (indent 1))
      `(cl-letf (((symbol-function 'edmacs-frames--tab-root)
                  (lambda (tab) (cdr (assq tab ,root-alist-var))))
                 ((symbol-function 'edmacs-frames--tab-for-root)
                  (lambda (root &optional frame)
                    (seq-find (lambda (tab)
                                (equal (cdr (assq tab ,root-alist-var)) root))
                              (tab-bar-tabs frame)))))
         ,@body))

    (ert-deftest edmacs-sidebar-test-redraw-worktrees-open-and-tabless-shape ()
      "3 worktree sections, exactly 1 open (the current tab), 2 tab-less."
      (let* ((current (tab-bar--current-tab-find))
             (root-alist (list (cons current "/repo/wt-b/")))
             (worktrees '(("repo" . "/repo/")
                          ("wt-a" . "/repo/wt-a/")
                          ("wt-b" . "/repo/wt-b/"))))
        (edmacs-sidebar-test--stub-worktree-lookup root-alist
          (cl-letf (((symbol-function 'edmacs-worktrees-for-repo)
                     (lambda (_common) worktrees)))
            (edmacs-sidebar-test--with-repo-frame "/repo/.git"
              (unwind-protect
                  (progn
                    (edmacs-sidebar-show (selected-frame))
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      (let ((text (buffer-string)))
                        (should (= 3 (length (split-string text "\n" t))))
                        (should (= 1 (cl-count ?● text)))
                        (should (= 0 (cl-count ?○ text)))
                        (should (= 2 (cl-count ?⋯ text)))
                        (should (string-match-p "no tab" text)))))
                (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))))

    (ert-deftest edmacs-sidebar-test-redraw-worktrees-cache-miss-renders-empty ()
      "A cache miss (nil from `edmacs-worktrees-for-repo') renders zero
sections -- never an error, and never a fallback compute."
      (let ((root-alist nil))
        (edmacs-sidebar-test--stub-worktree-lookup root-alist
          (cl-letf (((symbol-function 'edmacs-worktrees-for-repo) (lambda (_common) nil)))
            (edmacs-sidebar-test--with-repo-frame "/repo/.git"
              (unwind-protect
                  (progn
                    (edmacs-sidebar-show (selected-frame))
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      (should (= 0 (length (split-string (buffer-string) "\n" t))))))
                (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))))

    (ert-deftest edmacs-sidebar-test-redraw-worktrees-stale-tab-gets-warning-face ()
      "A tab whose root has dropped out of a fresh, non-empty worktree list
is kept, rendered with the missing-worktree warning face."
      (let* ((current (tab-bar--current-tab-find))
             (root-alist (list (cons current "/repo/gone/")))
             (worktrees '(("repo" . "/repo/"))))
        (edmacs-sidebar-test--stub-worktree-lookup root-alist
          (cl-letf (((symbol-function 'edmacs-worktrees-for-repo)
                     (lambda (_common) worktrees)))
            (edmacs-sidebar-test--with-repo-frame "/repo/.git"
              (unwind-protect
                  (progn
                    (edmacs-sidebar-show (selected-frame))
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      (let ((text (buffer-string)))
                        ;; "repo" (tab-less) + the stale tab itself.
                        (should (= 2 (length (split-string text "\n" t))))
                        (should (string-match-p "no tab" text))
                        (should (text-property-any
                                 (point-min) (point-max)
                                 'face 'edmacs-sidebar-missing-worktree-face)))))
                (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))))

    (ert-deftest edmacs-sidebar-test-activate-tabless-row-opens-once-then-reselects ()
      "RET on a tab-less row opens a tab exactly once; RET again reselects
rather than opening a second (AC2's core duplicate-prevention claim).
A second worktree entry is stamped onto the frame's own (only) real tab
so that tab is never mistaken for a stale row here -- this test is about
the SECOND entry, \"wt\", which starts with no tab of its own."
      (let* ((current (tab-bar--current-tab-find))
             (root-alist (list (cons current "/repo/main/")))
             (worktrees '(("main" . "/repo/main/") ("wt" . "/repo/wt/")))
             (open-calls 0)
             (select-calls nil))
        (edmacs-sidebar-test--stub-worktree-lookup root-alist
          (cl-letf (((symbol-function 'edmacs-worktrees-for-repo) (lambda (_common) worktrees))
                    ((symbol-function 'edmacs-frames-open-worktree-tab)
                     (lambda (root)
                       (setq open-calls (1+ open-calls))
                       ;; Simulate the real effect: the current tab now
                       ;; also carries ROOT (a single-tab frame, as here).
                       (push (cons current root) root-alist)))
                    ((symbol-function 'tab-bar-select-tab)
                     (lambda (n) (push n select-calls))))
            (edmacs-sidebar-test--with-repo-frame "/repo/.git"
              (unwind-protect
                  (progn
                    (edmacs-sidebar-show (selected-frame))
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      ;; Row order follows `worktrees': "main" then "wt".
                      (goto-char (point-min))
                      (forward-line 1)
                      (edmacs-sidebar-activate))
                    (should (= 1 open-calls))
                    (should-not select-calls)
                    ;; Redraw now sees the (stubbed) newly-open tab.
                    (edmacs-sidebar--redraw (selected-frame))
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      (goto-char (point-min))
                      (forward-line 1)
                      (edmacs-sidebar-activate))
                    (should (= 1 open-calls))
                    (should select-calls))
                (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))))

    (ert-deftest edmacs-sidebar-test-close-worktree-open-row-closes-tabless-row-noops ()
      (let* ((current (tab-bar--current-tab-find))
             (root-alist (list (cons current "/repo/wt/")))
             (worktrees '(("wt" . "/repo/wt/") ("wt2" . "/repo/wt2/")))
             (closed nil))
        (edmacs-sidebar-test--stub-worktree-lookup root-alist
          (cl-letf (((symbol-function 'edmacs-worktrees-for-repo) (lambda (_common) worktrees))
                    ((symbol-function 'tab-bar-close-tab) (lambda (n) (push n closed))))
            (edmacs-sidebar-test--with-repo-frame "/repo/.git"
              (unwind-protect
                  (progn
                    (edmacs-sidebar-show (selected-frame))
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      ;; First row: "wt", open (current tab).
                      (goto-char (point-min))
                      (edmacs-sidebar-close-worktree)
                      (should closed)
                      ;; Second row: "wt2", tab-less -- a no-op.
                      (setq closed nil)
                      (forward-line 1)
                      (edmacs-sidebar-close-worktree)
                      (should-not closed)))
                (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))))

    ;; ==========================================================================
    ;; AC4 (worktree redraw path) -- no subprocess work through the
    ;; worktree-aware render/activate/close surface
    ;; ==========================================================================
    ;; `edmacs-sidebar-test-redraw-and-hooks-never-shell-out' above (the
    ;; phase-2 regression test) only ever drives a repo-LESS frame -- it
    ;; never sets `edmacs-repo', so it exercises `edmacs-sidebar--redraw-tabs'
    ;; but never `edmacs-sidebar--redraw-worktrees', `edmacs-sidebar-activate'
    ;; on an already-open row, or `edmacs-sidebar-close-worktree'. This is
    ;; the direct regression test for THIS phase's own no-shellout claim,
    ;; covering both a populated cache and a cache miss.

    (ert-deftest edmacs-sidebar-test-redraw-worktrees-never-shell-out ()
      (let* ((current (tab-bar--current-tab-find))
             (root-alist (list (cons current "/repo/")))
             (worktrees '(("repo" . "/repo/")
                          ("wt-a" . "/repo/wt-a/")
                          ("wt-b" . "/repo/wt-b/")))
             (violations nil)
             (guarded '(call-process call-process-region process-file
                        start-process start-file-process make-process)))
        (edmacs-sidebar-test--stub-worktree-lookup root-alist
          (cl-letf (((symbol-function 'edmacs-worktrees-for-repo)
                     (lambda (_common) worktrees))
                    ;; Correctness of RET's open-vs-select dispatch is
                    ;; already covered by the AC2 tests above; this stub only
                    ;; needs to do nothing, so the tab-less row's activation
                    ;; below cannot itself register a (real) subprocess call.
                    ((symbol-function 'edmacs-frames-open-worktree-tab)
                     (lambda (_root) nil))
                    ;; Real `tab-bar-close-tab' would actually close the one
                    ;; live tab this whole suite shares -- recorded instead,
                    ;; since all that matters here is that closing an
                    ;; already-open row never reaches a subprocess primitive.
                    ((symbol-function 'tab-bar-close-tab) (lambda (&optional _n) nil)))
            (edmacs-sidebar-test--with-repo-frame "/repo/.git"
              (unwind-protect
                  (progn
                    (dolist (fn guarded)
                      (advice-add fn :before
                                  (lambda (&rest _) (push fn violations))
                                  `((name . ,(intern (format "edmacs-sidebar-test--guard-wt-%s" fn))))))
                    (edmacs-sidebar-show (selected-frame))
                    (dotimes (_ 50)
                      (edmacs-sidebar--redraw (selected-frame))
                      (edmacs-sidebar--redraw-worktrees (selected-frame) "/repo/.git")
                      (edmacs-sidebar--on-tab-select nil nil)
                      (edmacs-sidebar--on-tab-open nil)
                      (edmacs-sidebar--on-tab-pre-close nil nil)
                      (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                        ;; First row ("repo") is the open one (stamped onto
                        ;; the current tab above) -- activating/closing it
                        ;; stays on the tab-number branch.
                        (goto-char (point-min))
                        (edmacs-sidebar-activate)
                        (edmacs-sidebar-close-worktree)
                        ;; Second row ("wt-a") is tab-less -- both must no-op
                        ;; rather than reach for a subprocess.
                        (goto-char (point-min))
                        (forward-line 1)
                        (edmacs-sidebar-activate)
                        (edmacs-sidebar-close-worktree)))
                    (sleep-for 0.2)
                    (sit-for 0)
                    (should-not violations))
                (dolist (fn guarded)
                  (advice-remove fn (intern (format "edmacs-sidebar-test--guard-wt-%s" fn))))
                (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))))

    (ert-deftest edmacs-sidebar-test-redraw-worktrees-cache-miss-never-shell-out ()
      "The cache-miss render path (nil from `edmacs-worktrees-for-repo') is
just as much a no-shellout surface as the populated-cache one -- it must
never fall back to a compute/subprocess call, only an empty render."
      (let ((violations nil)
            (guarded '(call-process call-process-region process-file
                       start-process start-file-process make-process)))
        (cl-letf (((symbol-function 'edmacs-worktrees-for-repo) (lambda (_common) nil)))
          (edmacs-sidebar-test--with-repo-frame "/repo/.git"
            (unwind-protect
                (progn
                  (dolist (fn guarded)
                    (advice-add fn :before
                                (lambda (&rest _) (push fn violations))
                                `((name . ,(intern (format "edmacs-sidebar-test--guard-wtmiss-%s" fn))))))
                  (edmacs-sidebar-show (selected-frame))
                  (dotimes (_ 50)
                    (edmacs-sidebar--redraw (selected-frame))
                    (edmacs-sidebar--redraw-worktrees (selected-frame) "/repo/.git"))
                  (sleep-for 0.2)
                  (sit-for 0)
                  (should-not violations))
              (dolist (fn guarded)
                (advice-remove fn (intern (format "edmacs-sidebar-test--guard-wtmiss-%s" fn))))
              (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))))

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
                  (edmacs-sidebar-activate))
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
      "Short tab name deliberately: `edmacs-sidebar-max-width-fraction'
(AC4) can clamp the default sidebar width below what a long literal
test name needs to render untruncated -- this test's own concern is
that the rename advice triggers a redraw at all, not truncation, which
has its own dedicated coverage below."
      (unwind-protect
          (progn
            (edmacs-sidebar-show (selected-frame))
            (tab-bar-rename-tab "renamed-tab")
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (should (string-match-p "renamed-tab" (buffer-string)))))
        (ignore-errors (tab-bar-rename-tab ""))
        (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))

    ;; ==========================================================================
    ;; AC4 -- window never selected/deleted; rotate-layout leaves it
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-window-parameters-block-other-window-and-c-x-1 ()
      "Behavioural half only: this asserts the parameters at creation time.
Whether they survive a `window-state-get'/`window-state-put' round trip --
what a daemon restart does to a background tab -- is windows.el's owned
set, covered by `edmacs-windows-test-layout-parameters-survive-a-state-
round-trip'."
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

    (ert-deftest edmacs-sidebar-test-ensure-buffer-renames-stale-buffer-name ()
      "A restored frame's sidebar buffer can be created before the frame's
real title is regenerated -- exactly the daemon-boot frameset-restore
race `edmacs-sessions--ensure-sidebar' (sessions.el) documents, and
reproduced live via a real multi-frame daemon restart: the sidebar
buffer's own name locked in as the frame's stale/generic name and never
caught up once the frame was correctly retitled. `edmacs-sidebar--
ensure-buffer' must rename an already-live buffer to match, not just
leave a stale name on an otherwise-correct buffer."
      (let ((frame (selected-frame))
            (original-name (frame-parameter (selected-frame) 'name)))
        (unwind-protect
            (progn
              (edmacs-sidebar--ensure-buffer frame)
              (with-current-buffer (edmacs-sidebar--buffer frame)
                (rename-buffer "*sidebar: stale-name*" t))
              (set-frame-parameter frame 'name "real-repo-name")
              (edmacs-sidebar--ensure-buffer frame)
              (should (equal (buffer-name (edmacs-sidebar--buffer frame))
                              "*sidebar: real-repo-name*")))
          (set-frame-parameter frame 'name original-name)
          (edmacs-sidebar-test--cleanup-sidebar frame))))

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
      "Extended for phase 8: also drives every new command (J/K/gr/rename/
help) through the same guard. This phase adds no new *expected*
subprocess call -- the guard's expectation stays 'zero', not 'zero
except N'. The two real, already-documented exceptions elsewhere in
this codebase (`edmacs-frames--worktrees-refresh' [phase 3] and
sidebar-agents.el's tmux-jump `start-process' calls [phase 6]) are
never reached by this loop: it never creates a frame or fires an
agent jump. This automated guard is the primary, CI-equivalent check;
the phase body's own 'one minute of `profiler-start' over mixed tab
switching/buffer opening/agent state changes' is a documented,
non-automated interactive checklist pass for the implementer/reviewer
to run once before marking the phase reviewed.

Also poisons `edmacs-sidebar-remembered-width' up front and re-drives
`edmacs-sidebar-show', `--remember-width', and `--on-desktop-read'
through the same loop (item 4b's clamp path), so the width-clamp code
added for AC4 is exercised under this same zero-subprocess guarantee,
not only under its own dedicated AC4 tests."
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
                (set-frame-parameter (selected-frame) 'edmacs-sidebar-remembered-width
                                      (* 2 (frame-width (selected-frame))))
                (cl-letf (((symbol-function 'read-from-minibuffer)
                           (lambda (&rest _) "edmacs-sidebar-test-renamed-tab")))
                  (dotimes (_ 50)
                    (edmacs-sidebar--redraw (selected-frame))
                    (edmacs-sidebar--on-tab-select nil nil)
                    (edmacs-sidebar--on-tab-open nil)
                    (edmacs-sidebar--on-tab-pre-close nil nil)
                    (tab-bar-rename-tab "edmacs-sidebar-test-shellout-check")
                    (edmacs-sidebar-redraw)
                    (edmacs-sidebar-show (selected-frame))
                    (edmacs-sidebar--remember-width (selected-frame))
                    (edmacs-sidebar--on-desktop-read)
                    ;; `describe-keymap' is real Emacs 29+ core, exercised for
                    ;; real by `edmacs-sidebar-test-help-falls-back-to-describe-keymap-for-real'
                    ;; below; stubbed here to a no-op -- this loop's only
                    ;; concern is that `edmacs-sidebar-help's own dispatch
                    ;; never shells out, not that the real help/which-key UI
                    ;; can coexist with this frame's dedicated,
                    ;; `no-other-window' sidebar side window without wedging
                    ;; `display-buffer'.
                    (cl-letf (((symbol-function 'describe-keymap) (lambda (&rest _) nil))
                              ((symbol-function 'which-key-show-full-keymap) (lambda (&rest _) nil)))
                      (let ((inhibit-message t))
                        (edmacs-sidebar-help)))
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      (goto-char (point-min))
                      (edmacs-sidebar-move-to-next-worktree)
                      (edmacs-sidebar-move-to-prev-worktree)
                      (edmacs-sidebar-rename-at-point))))
                (sleep-for 0.2)
                (sit-for 0)
                (should-not violations))
            (dolist (fn guarded)
              (advice-remove fn (intern (format "edmacs-sidebar-test--guard-%s" fn))))
            (ignore-errors (tab-bar-rename-tab ""))
            (when (get-buffer "*Help*") (kill-buffer "*Help*"))
            (set-frame-parameter (selected-frame) 'edmacs-sidebar-remembered-width nil)
            (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))

    ;; ==========================================================================
    ;; Frameset restore (edmacs-sidebar roadmap phase 4) -- AC2/AC3
    ;; ==========================================================================
    ;; sessions.el is not loaded here (see this file's own module-boundary
    ;; convention); the `edmacs-repo-missing' frame parameter it sets is
    ;; poked directly.

    (ert-deftest edmacs-sidebar-test-redraw-shows-missing-repo-warning-row ()
      (unwind-protect
          (progn
            (set-frame-parameter (selected-frame) 'edmacs-repo-missing t)
            (edmacs-sidebar-show (selected-frame))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (should (string-match-p "repo missing" (buffer-string)))))
        (set-frame-parameter (selected-frame) 'edmacs-repo-missing nil)
        (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))

    (ert-deftest edmacs-sidebar-test-redraw-omits-missing-repo-warning-row-when-unset ()
      (unwind-protect
          (progn
            (set-frame-parameter (selected-frame) 'edmacs-repo-missing nil)
            (edmacs-sidebar-show (selected-frame))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (should-not (string-match-p "repo missing" (buffer-string)))))
        (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))

    (ert-deftest edmacs-sidebar-test-show-replaces-scratch-in-existing-side-window ()
      "Regression: a restored tab whose window-state names a dead sidebar
buffer leaves its side window showing some substitute buffer (a
placeholder such as `*scratch*' stands in for whatever
`window-state-put' actually leaves there). `edmacs-sidebar-show' must
reuse that window -- never open a second side window -- and end up
showing the frame's live sidebar buffer, never `*scratch*'."
      (let* ((frame (selected-frame))
             (placeholder-window
              (display-buffer (get-buffer-create "*scratch*")
                               '((display-buffer-in-side-window)
                                 (side . left) (slot . 0) (window-width . 32)))))
        (unwind-protect
            (progn
              (should (window-live-p placeholder-window))
              (should (eq (window-buffer placeholder-window) (get-buffer "*scratch*")))
              (edmacs-sidebar-show frame)
              (let ((side-windows
                     (seq-filter (lambda (w) (eq (window-parameter w 'window-side) 'left))
                                 (window-list frame 'never))))
                (should (= 1 (length side-windows)))
                (should (eq (window-buffer (car side-windows))
                            (edmacs-sidebar--buffer frame)))
                (should-not (eq (window-buffer (car side-windows)) (get-buffer "*scratch*")))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    ;; ==========================================================================
    ;; Phase 8 -- J/K/r/gr/? bindings, RET user-errors, faces, resize, header-line
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-j-k-r-gr-help-resolve-through-real-evil-keymaps ()
      "Regression test mirroring `-ret-and-q-resolve-...' above, for this
phase's own bindings: J/K/r/gr/? must resolve through real evil
motion-state keymaps to this file's own commands, not evil's own
K/?/... bindings (`evil-lookup', `evil-search-backward', ...) --
per evil-maps.el:234,294. TAB is checked too: contrary to the phase
body's own expectation that `evil-want-C-i-jump' being nil leaves TAB
unclaimed, this live check found `evil-motion-state-map' still binding
`C-i' (the same event as plain TAB outside a GUI frame) to
`evil-jump-forward' regardless of that variable, shadowing
`magit-section-mode-map's own binding -- so TAB got the same
dual-binding override as RET/q/K/? (see sidebar.el's own comment at
its `TAB' binding). Resolves to `edmacs-sidebar-toggle-at-point', the
fold-dispatch wrapper, not bare `magit-section-toggle' -- see the
dedicated fold-dispatch tests below for its enclosing-group behavior."
      (edmacs-sidebar-test--ensure-real-evil)
      (unwind-protect
          (progn
            (evil-mode 1)
            (with-temp-buffer
              (edmacs-sidebar-mode)
              (evil-motion-state)
              (should (eq evil-state 'motion))
              (should (eq (key-binding (kbd "J")) #'edmacs-sidebar-move-to-next-worktree))
              (should (eq (key-binding (kbd "K")) #'edmacs-sidebar-move-to-prev-worktree))
              (should (eq (key-binding (kbd "r")) #'edmacs-sidebar-rename-at-point))
              (should (eq (key-binding (kbd "g r")) #'edmacs-sidebar-redraw))
              (should (eq (key-binding (kbd "?")) #'edmacs-sidebar-help))
              (should (eq (key-binding (kbd "TAB")) #'edmacs-sidebar-toggle-at-point))))
        (evil-mode -1)))

    (ert-deftest edmacs-sidebar-test-help-falls-back-to-describe-keymap-for-real ()
      "`edmacs-sidebar-help' actually takes its real `describe-keymap'
branch and produces a `*Help*' buffer listing the mode's own bindings
-- unlike the never-shell-out loop above, which stubs both branches to
no-ops and only guards against a subprocess call.
Emacs 31 ships which-key's autoloads by default, so
`(fboundp 'which-key-show-full-keymap)' is already true under plain
`-Q --batch' before which-key.el itself ever loads; unbind that
autoload stub here to simulate which-key genuinely being absent, which
is the actual precondition `edmacs-sidebar-help's fallback branch is
for."
      (let ((which-key-was-bound (fboundp 'which-key-show-full-keymap))
            (which-key-def (and (fboundp 'which-key-show-full-keymap)
                                 (symbol-function 'which-key-show-full-keymap))))
        (unwind-protect
            (progn
              (fmakunbound 'which-key-show-full-keymap)
              (when (get-buffer "*Help*") (kill-buffer "*Help*"))
              (let ((inhibit-message t)) (edmacs-sidebar-help))
              (let ((help-buf (get-buffer "*Help*")))
                (should help-buf)
                (with-current-buffer help-buf
                  (should (string-match-p "edmacs-sidebar-mode-map" (buffer-string)))
                  (should (string-match-p "edmacs-sidebar-redraw" (buffer-string))))))
          (when which-key-was-bound
            (fset 'which-key-show-full-keymap which-key-def))
          (when (get-buffer "*Help*") (kill-buffer "*Help*")))))

    (ert-deftest edmacs-sidebar-test-toggle-at-point-folds-self-when-has-children ()
      "TAB (`edmacs-sidebar-toggle-at-point') on a row with its own
children -- a tab row with an agents group nested underneath -- folds
its own body, per the design table's `On a tab' -> `Fold / unfold' cell."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let (tab-section)
          (let ((inhibit-read-only t))
            (magit-insert-section (edmacs-sidebar-root)
              (setq tab-section
                    (magit-insert-section (edmacs-sidebar-tab 1)
                      (magit-insert-heading "tab row")
                      (magit-insert-section (edmacs-sidebar-agents-group nil)
                        (magit-insert-heading "  agents group"))))))
          (goto-char (point-min))
          (should (eq (magit-current-section) tab-section))
          (should (eq nil (oref tab-section hidden)))
          (edmacs-sidebar-toggle-at-point)
          (should (eq t (oref tab-section hidden)))
          (edmacs-sidebar-toggle-at-point)
          (should (eq nil (oref tab-section hidden))))))

    (ert-deftest edmacs-sidebar-test-toggle-at-point-folds-enclosing-group-for-agent-leaf ()
      "TAB on an agent leaf row folds its enclosing group (the agents
group heading above it) instead of the leaf itself -- an agent section
has no body of its own, so toggling it directly would be an invisible
no-op. Matches the design table's colspan cell for `On an agent'/`On a
buffer': \"Fold / unfold the enclosing group\"."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let (group-section agent-section)
          (let ((inhibit-read-only t))
            (magit-insert-section (edmacs-sidebar-root)
              (magit-insert-section (edmacs-sidebar-tab 1)
                (magit-insert-heading "tab row")
                (setq group-section
                      (magit-insert-section (edmacs-sidebar-agents-group nil)
                        (magit-insert-heading "  agents group")
                        (setq agent-section
                              (magit-insert-section (edmacs-sidebar-agent "fake-agent")
                                (magit-insert-heading "    an agent row"))))))))
          (goto-char (point-max))
          (forward-line -1)
          (should (eq (magit-current-section) agent-section))
          (should (eq nil (oref group-section hidden)))
          (should (eq nil (oref agent-section hidden)))
          (edmacs-sidebar-toggle-at-point)
          (should (eq t (oref group-section hidden)))
          (should (eq nil (oref agent-section hidden))))))

    (ert-deftest edmacs-sidebar-test-toggle-at-point-folds-self-with-no-parent-group ()
      "A leaf row directly under the root wrapper (no intervening group
section -- e.g. a scratch frame's flat agent-less layout) has no
non-root parent to defer to, so TAB falls back to toggling itself,
exactly like plain `magit-section-toggle' would (a no-op here, since
the leaf has no body, but never an error)."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let (agent-section)
          (let ((inhibit-read-only t))
            (magit-insert-section (edmacs-sidebar-root)
              (setq agent-section
                    (magit-insert-section (edmacs-sidebar-agent "fake-agent")
                      (magit-insert-heading "an agent row")))))
          (goto-char (point-min))
          (should (eq (magit-current-section) agent-section))
          (should (eq nil (oref agent-section hidden)))
          (edmacs-sidebar-toggle-at-point)
          (should (eq t (oref agent-section hidden))))))

    (ert-deftest edmacs-sidebar-test-visit-at-point-user-errors-with-no-section ()
      "No section at all (an `edmacs-sidebar-mode' buffer with nothing
ever inserted into it -- `magit-root-section' stays nil) user-errors."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (should-error (edmacs-sidebar-visit-at-point) :type 'user-error)))

    (ert-deftest edmacs-sidebar-test-visit-at-point-user-errors-on-empty-root ()
      "Point on the root wrapper itself (no children at all) user-errors."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)))
        (goto-char (point-min))
        (should-error (edmacs-sidebar-visit-at-point) :type 'user-error)))

    (ert-deftest edmacs-sidebar-test-visit-at-point-user-errors-on-inactionable-types ()
      "warning/agents-group/agents-all/buffers-dir all user-error on RET."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)
            (magit-insert-section (edmacs-sidebar-warning)
              (magit-insert-heading "warning row"))
            (magit-insert-section (edmacs-sidebar-agents-group nil)
              (magit-insert-heading "agents group"))
            (magit-insert-section (edmacs-sidebar-agents-all)
              (magit-insert-heading "all agents"))
            (magit-insert-section (edmacs-sidebar-buffers-dir "src" nil)
              (magit-insert-heading "src/"))))
        (goto-char (point-min))
        (should-error (edmacs-sidebar-visit-at-point) :type 'user-error)
        (forward-line 1)
        (should-error (edmacs-sidebar-visit-at-point) :type 'user-error)
        (forward-line 1)
        (should-error (edmacs-sidebar-visit-at-point) :type 'user-error)
        (forward-line 1)
        (should-error (edmacs-sidebar-visit-at-point) :type 'user-error)))

    (ert-deftest edmacs-sidebar-test-kill-at-point-closes-scratch-tab-in-flat-list ()
      "`d' (`edmacs-sidebar-kill-at-point') already closes a tab in the
repo-less flat-tab-list frame (the daemon's scratch/spare frame) --
regression coverage for phase 1/3's `(integerp value)' branch; no code
change needed for this phase."
      (edmacs-sidebar-test--with-extra-tab
        (let ((closed nil))
          (cl-letf (((symbol-function 'tab-bar-close-tab) (lambda (&optional n) (push n closed))))
            (unwind-protect
                (progn
                  (edmacs-sidebar-show (selected-frame))
                  (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                    (goto-char (point-min))
                    (edmacs-sidebar-kill-at-point))
                  (should closed))
              (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))))

    (ert-deftest edmacs-sidebar-test-kill-at-point-dispatches-to-agents-kill ()
      "`d' on an `edmacs-sidebar-agent' row calls
`edmacs-sidebar-agents-kill' (sidebar-agents.el) with the section's
agent value, per the design's key table (\"Kill the agent session
[confirm]\"), instead of falling through to
`edmacs-sidebar-close-worktree'."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)
            (magit-insert-section (edmacs-sidebar-agent 'fake-agent-value)
              (magit-insert-heading "agent row"))))
        (goto-char (point-min))
        (let ((kill-calls nil))
          (cl-letf (((symbol-function 'edmacs-sidebar-agents-kill)
                     (lambda (agent) (push agent kill-calls))))
            (edmacs-sidebar-kill-at-point))
          (should (equal kill-calls '(fake-agent-value))))))

    (ert-deftest edmacs-sidebar-test-move-to-worktree-top-level-only ()
      "J/K move only among top-level (direct root children) rows, skipping
over a nested child section entirely, and no-op past either end."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)
            (magit-insert-section (edmacs-sidebar-tab 1)
              (magit-insert-heading "row one")
              (magit-insert-section (edmacs-sidebar-agents-group nil)
                (magit-insert-heading "  nested agent group")))
            (magit-insert-section (edmacs-sidebar-tab 2)
              (magit-insert-heading "row two"))
            (magit-insert-section (edmacs-sidebar-tab 3)
              (magit-insert-heading "row three"))))
        (cl-flet ((line () (buffer-substring (line-beginning-position) (line-end-position))))
          (goto-char (point-min))
          (forward-line 1)
          (should (equal "  nested agent group" (line)))
          (edmacs-sidebar-move-to-next-worktree)
          (should (equal "row two" (line)))
          (edmacs-sidebar-move-to-next-worktree)
          (should (equal "row three" (line)))
          (edmacs-sidebar-move-to-next-worktree)
          (should (equal "row three" (line)))
          (edmacs-sidebar-move-to-prev-worktree)
          (should (equal "row two" (line)))
          (edmacs-sidebar-move-to-prev-worktree)
          (should (equal "row one" (line)))
          (edmacs-sidebar-move-to-prev-worktree)
          (should (equal "row one" (line))))))

    (ert-deftest edmacs-sidebar-test-rename-at-point-dispatches-and-user-errors ()
      "`r' calls `tab-bar-rename-tab' on an open tab row -- through the
real `call-interactively' dispatch, so `tab-bar-rename-tab's own
Lisp-form interactive spec actually runs too, exactly as it would from
a keypress; that spec calls `read-from-minibuffer' via a normal
(stubbable) Lisp call, unlike a bare string interactive spec's C-level
argument reading, which `fset'/`cl-letf' cannot intercept -- confirmed
against a real tab's name actually changing, not a mock call count.
`r' user-errors on a tab-less worktree row, and with no recognized row
at all."
      (let* ((current (tab-bar--current-tab-find))
             (root-alist (list (cons current "/repo/wt-open/")))
             (worktrees '(("wt-open" . "/repo/wt-open/") ("wt-closed" . "/repo/wt-closed/"))))
        (edmacs-sidebar-test--stub-worktree-lookup root-alist
          (cl-letf (((symbol-function 'edmacs-worktrees-for-repo) (lambda (_common) worktrees))
                    ((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _) "edmacs-sidebar-test-renamed")))
            (edmacs-sidebar-test--with-repo-frame "/repo/.git"
              (unwind-protect
                  (progn
                    (edmacs-sidebar-show (selected-frame))
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      (goto-char (point-min))
                      (edmacs-sidebar-rename-at-point)
                      (should (equal "edmacs-sidebar-test-renamed"
                                      (alist-get 'name (tab-bar--current-tab-find))))
                      (forward-line 1)
                      (should-error (edmacs-sidebar-rename-at-point) :type 'user-error)))
                (ignore-errors (tab-bar-rename-tab ""))
                (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))))
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (should-error (edmacs-sidebar-rename-at-point) :type 'user-error)))

    (ert-deftest edmacs-sidebar-test-rename-at-point-renames-background-tab-not-current ()
      "`r' on a *background* (non-current) tab's row renames that tab,
never the frame's currently-selected tab -- the exact interaction J/K
navigation exists to enable. `tab-bar-rename-tab' called interactively
defaults TAB-NUMBER to the selected tab regardless of which row
triggered the command, so this only passes if
`edmacs-sidebar-rename-at-point' threads the row's own tab-number
through explicitly instead of `call-interactively'-ing blind."
      (edmacs-sidebar-test--with-extra-tab
        (unwind-protect
            (progn
              (edmacs-sidebar-show (selected-frame))
              ;; The newly-added tab is current, at index 1; point-min
              ;; is the original (background) tab's row, at index 0.
              (should (= 1 (tab-bar--current-tab-index)))
              (let ((current-name-before (alist-get 'name (tab-bar--current-tab-find))))
                (cl-letf (((symbol-function 'read-from-minibuffer)
                           (lambda (&rest _) "edmacs-sidebar-test-bg-renamed")))
                  (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                    (goto-char (point-min))
                    (edmacs-sidebar-rename-at-point)))
                ;; The background tab (index 0) got the new name...
                (should (equal "edmacs-sidebar-test-bg-renamed"
                                (alist-get 'name (nth 0 (tab-bar-tabs)))))
                ;; ...and the still-current tab (index 1) is untouched.
                (should (= 1 (tab-bar--current-tab-index)))
                (should (equal current-name-before
                                (alist-get 'name (tab-bar--current-tab-find))))))
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame)))))

    (ert-deftest edmacs-sidebar-test-current-tab-and-worktree-closed-faces ()
      (let* ((current (tab-bar--current-tab-find))
             (root-alist (list (cons current "/repo/wt-open/")))
             (worktrees '(("wt-open" . "/repo/wt-open/") ("wt-closed" . "/repo/wt-closed/"))))
        (edmacs-sidebar-test--stub-worktree-lookup root-alist
          (cl-letf (((symbol-function 'edmacs-worktrees-for-repo) (lambda (_common) worktrees)))
            (edmacs-sidebar-test--with-repo-frame "/repo/.git"
              (unwind-protect
                  (progn
                    (edmacs-sidebar-show (selected-frame))
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      (goto-char (point-min))
                      (should (eq (get-text-property (point) 'face) 'edmacs-sidebar-current-tab-face))
                      (forward-line 1)
                      (should (eq (get-text-property (point) 'face) 'edmacs-sidebar-worktree-closed-face))))
                (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))))

    (ert-deftest edmacs-sidebar-test-resize-survives-toggle ()
      "A manual resize survives `edmacs-sidebar-toggle' twice (hide, then
show): the restored width comes from the frame-parameter stash, not
the `edmacs-sidebar-width' default. `--remember-width' (the debounced
timer's own callback) is called directly rather than through
`--on-window-size-change' + a real wait -- a live window's own
automatic `window-size-change-functions' firing during the wait would
keep re-arming the debounce timer out from under a fixed `sleep-for',
and this test's own concern is the stash-and-restore behavior, not the
debounce timing (which has no dedicated assertion here)."
      (let ((frame (selected-frame)))
        (unwind-protect
            (progn
              (edmacs-sidebar-show frame)
              (let ((window (edmacs-sidebar--window frame)))
                (window-resize window -5 t)
                (edmacs-sidebar--remember-width frame))
              (let ((resized (window-width (edmacs-sidebar--window frame))))
                (should (/= resized edmacs-sidebar-width))
                (edmacs-sidebar-toggle)
                (edmacs-sidebar-toggle)
                (should (= resized (window-width (edmacs-sidebar--window frame))))))
          (let ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
            (when (timerp timer) (cancel-timer timer)))
          (remhash frame edmacs-sidebar--resize-debounce-timers)
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-on-window-size-change-debounces-and-stashes ()
      "`--on-window-size-change' schedules a debounced call to
`--remember-width', which stashes the CURRENT window width (`1+', to
compensate for `display-buffer-in-side-window's own fresh-split
off-by-one -- see `--remember-width's docstring) once it fires; a
no-op for a frame with no live sidebar window shown."
      (let ((frame (selected-frame))
            (edmacs-sidebar-resize-debounce-seconds 0.05))
        (unwind-protect
            (progn
              (should-not (edmacs-sidebar--window frame))
              (edmacs-sidebar--on-window-size-change frame)
              (should-not (gethash frame edmacs-sidebar--resize-debounce-timers))
              (edmacs-sidebar-show frame)
              (window-resize (edmacs-sidebar--window frame) -3 t)
              (edmacs-sidebar--on-window-size-change frame)
              (should (timerp (gethash frame edmacs-sidebar--resize-debounce-timers)))
              (let ((deadline (+ (float-time) 2)))
                (while (and (< (float-time) deadline)
                            (not (frame-parameter frame 'edmacs-sidebar-remembered-width)))
                  (sit-for 0.1)))
              (should (= (1+ (window-width (edmacs-sidebar--window frame)))
                          (frame-parameter frame 'edmacs-sidebar-remembered-width))))
          (let ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
            (when (timerp timer) (cancel-timer timer)))
          (remhash frame edmacs-sidebar--resize-debounce-timers)
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    ;; ==========================================================================
    ;; AC4 -- the sidebar never exceeds `edmacs-sidebar-max-width-fraction'
    ;; of the frame width (item 4b: unbounded width, reported live as the
    ;; sidebar intermittently occupying ~50% of the frame)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-clamp-width-caps-and-floors ()
      "`--clamp-width' caps at the fraction of FRAME's width, floors at
`edmacs-sidebar--min-width', and passes a mid-range width through
unchanged."
      (cl-letf (((symbol-function 'frame-width) (lambda (_frame) 100)))
        (let ((edmacs-sidebar-max-width-fraction 0.33)
              (edmacs-sidebar--min-width 15))
          ;; Fraction cap wins: floor(100 * 0.33) = 33.
          (should (= 33 (edmacs-sidebar--clamp-width 90 'fake-frame)))
          ;; Floor wins: below the minimum usable width.
          (should (= 15 (edmacs-sidebar--clamp-width 5 'fake-frame)))
          ;; Mid-range: passes through unchanged.
          (should (= 25 (edmacs-sidebar--clamp-width 25 'fake-frame))))))

    (ert-deftest edmacs-sidebar-test-remember-width-refuses-as-sole-window ()
      "Measuring the sidebar while it is the frame's only live window must
not stash that width -- e.g. mid-frameset-restore before other windows
exist. `window-list' is stubbed to report the sidebar as the frame's
only window rather than literally deleting every sibling: Emacs's own
side-window invariant (a frame keeps at least one main window whenever
a side window exists) makes that real layout unreachable by deletion,
so the guard is exercised by controlling exactly what it inspects."
      (let ((frame (selected-frame)))
        (unwind-protect
            (progn
              (edmacs-sidebar-show frame)
              (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
              (let ((sidebar-window (edmacs-sidebar--window frame)))
                (should (window-live-p sidebar-window))
                (cl-letf (((symbol-function 'window-list)
                           (lambda (&rest _) (list sidebar-window))))
                  (edmacs-sidebar--remember-width frame)))
              (should-not (frame-parameter frame 'edmacs-sidebar-remembered-width)))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-remember-width-refuses-non-side-window ()
      "A sidebar buffer displayed in an ordinary (non-side) window must not
have its width stashed -- `--window' can still find it by buffer
identity, but `window-parameter ... window-side' is nil there."
      (let* ((frame (selected-frame))
             (buf (edmacs-sidebar--ensure-buffer frame))
             (original-window (selected-window))
             (split nil))
        (unwind-protect
            (progn
              (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
              (setq split (split-window original-window))
              (set-window-buffer split buf)
              (let ((window (edmacs-sidebar--window frame)))
                (should (window-live-p window))
                (should-not (window-parameter window 'window-side)))
              (edmacs-sidebar--remember-width frame)
              (should-not (frame-parameter frame 'edmacs-sidebar-remembered-width)))
          (when (window-live-p split) (delete-window split))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (let ((b (edmacs-sidebar--buffer frame)))
            (when (buffer-live-p b) (kill-buffer b)))
          (set-frame-parameter frame 'edmacs-sidebar-buffer nil))))

    (ert-deftest edmacs-sidebar-test-remember-width-clamps-stash ()
      "A genuinely live side window measuring wider than the fraction cap
gets the CLAMPED value stashed, not the raw `(1+ (window-width window))'."
      (let* ((frame (selected-frame))
             (fw (frame-width frame))
             (oversized (max 40 (- fw 10))))
        (unwind-protect
            (let ((edmacs-sidebar-max-width-fraction 1.0)
                  (edmacs-sidebar-width oversized))
              (edmacs-sidebar-show frame)
              (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
              (let* ((edmacs-sidebar-max-width-fraction 0.2)
                     (edmacs-sidebar--min-width 5)
                     (window (edmacs-sidebar--window frame))
                     (measured (window-width window))
                     (expected (edmacs-sidebar--clamp-width (1+ measured) frame)))
                ;; The scenario is only meaningful if the live window is
                ;; actually wider than the shrunk cap.
                (should (> (1+ measured) expected))
                (edmacs-sidebar--remember-width frame)
                (should (= expected (frame-parameter frame 'edmacs-sidebar-remembered-width)))))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-show-clamps-poisoned-remembered-width ()
      "A frame parameter already poisoned to (at or above) the frame's full
width still yields a clamped window from `edmacs-sidebar-show' -- the
reported bug of the sidebar coming back at ~50% of the frame."
      (let* ((frame (selected-frame))
             (fw (frame-width frame)))
        (unwind-protect
            (progn
              (set-frame-parameter frame 'edmacs-sidebar-remembered-width (+ fw 50))
              (edmacs-sidebar-show frame)
              (let ((window (edmacs-sidebar--window frame)))
                (should (<= (window-width window)
                             (floor (* fw edmacs-sidebar-max-width-fraction))))))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-desktop-restore-clamps-poisoned-width ()
      "`--on-desktop-read' (this file's documented stand-in for a real
`desktop-read' round trip under `-Q --batch') brings a frame carrying a
poisoned remembered-width back clamped, not full-frame-wide."
      (let* ((frame (selected-frame))
             (fw (frame-width frame)))
        (unwind-protect
            (progn
              (set-frame-parameter frame 'edmacs-sidebar-remembered-width (* fw 2))
              (edmacs-sidebar--on-desktop-read)
              (let ((window (edmacs-sidebar--window frame)))
                (should (<= (window-width window)
                             (floor (* fw edmacs-sidebar-max-width-fraction))))))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    ;; ==========================================================================
    ;; AC4 -- the sidebar is always a left side window, never a wrong-edge
    ;; or split window, even with the left slot exhausted at slot 0 (item
    ;; 4c: reported live as the sidebar intermittently popping up on the
    ;; far right of the frame)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-show-refuses-when-left-slot-exhausted ()
      "Drives the exhausted-left-slot path through the REAL
`window-sides-slots' global (not a mock of `edmacs-sidebar-show's
internals): with the left element forced to 0, no left side window can
be created at all, so `display-buffer-in-side-window' itself returns
nil per its own docstring. `edmacs-sidebar-show' must return nil and
leave the frame's window layout (count and buffer identities) exactly
as it found it -- never falling back to a wrong-edge or split window
the way a plain `display-buffer' call with a one-function action list
would."
      (let* ((frame (selected-frame))
             (before-buffers (mapcar #'window-buffer (window-list frame 'never)))
             (before-count (length before-buffers)))
        (unwind-protect
            (let ((window-sides-slots (list 0 (nth 1 window-sides-slots)
                                             (nth 2 window-sides-slots)
                                             (nth 3 window-sides-slots))))
              (should (null (edmacs-sidebar-show frame)))
              (should-not (seq-find (lambda (w) (window-parameter w 'window-side))
                                     (window-list frame 'never)))
              (should-not (seq-find #'window-dedicated-p (window-list frame 'never)))
              (should (= before-count (length (window-list frame 'never))))
              (should (equal before-buffers (mapcar #'window-buffer (window-list frame 'never)))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-show-cleans-up-non-left-window-from-placement ()
      "Belt-and-suspenders branch: even if `display-buffer-in-side-window'
itself returned a live window that is NOT a left side window (stubbed
here via `cl-letf' to fabricate an ordinary split, independent of
whatever real side-window semantics the exhausted-slot test above
relies on), `edmacs-sidebar-show' must delete that window and return
nil rather than dedicating and keeping it."
      (let* ((frame (selected-frame))
             (before-buffers (mapcar #'window-buffer (window-list frame 'never)))
             (before-count (length before-buffers))
             (stub-window nil))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'display-buffer-in-side-window)
                         (lambda (buffer _alist)
                           (setq stub-window (split-window (selected-window)))
                           (set-window-buffer stub-window buffer)
                           stub-window)))
                (should (null (edmacs-sidebar-show frame))))
              (should-not (window-live-p stub-window))
              (should (= before-count (length (window-list frame 'never))))
              (should (equal before-buffers (mapcar #'window-buffer (window-list frame 'never)))))
          (when (window-live-p stub-window) (delete-window stub-window))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-header-line-shows-repo-name ()
      (cl-letf (((symbol-function 'edmacs-worktrees-for-repo) (lambda (_common) nil)))
        (edmacs-sidebar-test--with-repo-frame "/repo/.git"
          (unwind-protect
              (progn
                (edmacs-sidebar-show (selected-frame))
                (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                  (should (equal "repo" (substring-no-properties header-line-format)))))
            (edmacs-sidebar-test--cleanup-sidebar (selected-frame))))))

    (ert-deftest edmacs-sidebar-test-header-line-shows-frame-name-when-repo-less ()
      (let ((frame (selected-frame))
            (original-name (frame-parameter (selected-frame) 'name)))
        (unwind-protect
            (progn
              (set-frame-parameter frame 'name "edmacs-sidebar-test-boot-frame")
              (edmacs-sidebar-show frame)
              (with-current-buffer (edmacs-sidebar--buffer frame)
                (should (equal "edmacs-sidebar-test-boot-frame"
                                (substring-no-properties header-line-format)))))
          (set-frame-parameter frame 'name original-name)
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    ;; ==========================================================================
    ;; Tab/worktree marker glyphs: nerd-icons with a plain-text fallback
    ;; (phase 8, AC2) -- same coverage pattern as
    ;; `edmacs-sidebar-agents-test-glyph-*' and
    ;; `edmacs-sidebar-buffers-test-visible-glyph-*' for their own glyphs.
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-glyph-fallback-plain-unicode ()
      "With no `nerd-icons', every kind falls back to its plain marker."
      (should (equal "●" (edmacs-sidebar--glyph 'current-tab)))
      (should (equal "○" (edmacs-sidebar--glyph 'open-tab)))
      (should (equal "⋯" (edmacs-sidebar--glyph 'no-tab)))
      (should (equal "?" (edmacs-sidebar--glyph 'some-unknown-kind))))

    (ert-deftest edmacs-sidebar-test-glyph-prefers-nerd-icons-when-available ()
      "When `nerd-icons' is (simulated) present, its icon wins over the
plain fallback."
      (cl-letf (((symbol-function 'nerd-icons-octicon)
                 (lambda (name) (format "NERD-%s" name)))
                ((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons))))
        (should (equal "NERD-nf-oct-arrow_right" (edmacs-sidebar--glyph 'current-tab)))
        (should (equal "NERD-nf-oct-circle" (edmacs-sidebar--glyph 'open-tab)))
        (should (equal "NERD-nf-oct-dash" (edmacs-sidebar--glyph 'no-tab)))))

    (ert-deftest edmacs-sidebar-test-glyph-nerd-icon-error-falls-back ()
      "A `nerd-icons' call that errors falls back to plain text, never signals."
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'fboundp) (lambda (f) (eq f 'nerd-icons-octicon)))
                ((symbol-function 'nerd-icons-octicon) (lambda (_name) (error "boom"))))
        (should (equal "●" (edmacs-sidebar--glyph 'current-tab)))))

    (ert-deftest edmacs-sidebar-test-glyph-force-text-overrides-nerd-icons ()
      (cl-letf (((symbol-function 'nerd-icons-octicon) (lambda (_name) "NERD-ARROW"))
                ((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'fboundp) (lambda (f) (eq f 'nerd-icons-octicon)))
                (edmacs-sidebar-force-text-glyphs t))
        (should (equal "●" (edmacs-sidebar--glyph 'current-tab)))))

    ;; ==========================================================================
    ;; Ellipsis truncation to the sidebar window's live width (phase 8, AC2/AC5)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-truncate-label-boundary ()
      "A label exactly as wide as the window is left untouched; one
character over gets truncated with a trailing … so the result is still
exactly WIDTH characters wide; no live sidebar window falls back to the
frame's remembered width run through `edmacs-sidebar--clamp-width', then
`edmacs-sidebar-width' likewise clamped. `edmacs-sidebar--min-width' is
lowered here so the small widths this test drives (5, 10) exercise the
truncation math itself rather than the floor -- the floor's own
interaction with the fallback is covered separately below."
      (let ((frame (selected-frame))
            (edmacs-sidebar--min-width 1))
        (should-not (edmacs-sidebar--window frame))
        (unwind-protect
            (let ((edmacs-sidebar-width 10))
              (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
              (should (equal "0123456789" (edmacs-sidebar--truncate-label "0123456789" frame)))
              (should (equal "012345678…" (edmacs-sidebar--truncate-label "0123456789X" frame)))
              (should (= 10 (length (edmacs-sidebar--truncate-label "0123456789X" frame))))
              (set-frame-parameter frame 'edmacs-sidebar-remembered-width 5)
              (should (equal "0123…" (edmacs-sidebar--truncate-label "0123456789" frame))))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil))))

    (ert-deftest edmacs-sidebar-test-truncate-label-fallback-clamps-poisoned-width ()
      "The no-live-window fallback in `edmacs-sidebar--truncate-label' must
run the remembered width through `edmacs-sidebar--clamp-width', exactly
like `edmacs-sidebar-show' and `edmacs-sidebar--remember-width' already
do for the same frame parameter. Without that, a poisoned remembered
width (the reported ~50%-of-frame bug) renders a full, untruncated
label on the very first pre-window redraw -- self-healing only once a
live, clamped window exists on the next redraw."
      (let ((frame (selected-frame)))
        (should-not (edmacs-sidebar--window frame))
        (unwind-protect
            (let* ((edmacs-sidebar--min-width 5)
                   (edmacs-sidebar-max-width-fraction 0.33)
                   (clamped (edmacs-sidebar--clamp-width most-positive-fixnum frame))
                   (long (make-string (+ clamped 20) ?x)))
              (set-frame-parameter frame 'edmacs-sidebar-remembered-width most-positive-fixnum)
              (should (= clamped (length (edmacs-sidebar--truncate-label long frame)))))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil))))

    (ert-deftest edmacs-sidebar-test-truncate-label-uses-live-window-width ()
      "Once the sidebar window is live, truncation keys off its current
width, not the static `edmacs-sidebar-width' default -- a widened
sidebar must not keep truncating to the old default."
      (let ((frame (selected-frame)))
        (unwind-protect
            (progn
              (edmacs-sidebar-show frame)
              (let* ((window (edmacs-sidebar--window frame))
                     (width (window-width window))
                     (long (make-string (+ width 5) ?x)))
                (should (= width (length (edmacs-sidebar--truncate-label long frame))))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))


    ;; ==========================================================================
    ;; The sidebar is never a frame's sole or root window
    ;; ==========================================================================

    (defun edmacs-sidebar-test--make-sole-sidebar-window (frame)
      "Leave FRAME with one window: a dedicated left side window showing the
sidebar buffer. That is the shape whose `delete-window' signals \"Attempt
to delete minibuffer or sole ordinary window\", because the window has no
parent -- reproduced by hand rather than via `edmacs-sidebar-show', which
now repairs it."
      (delete-other-windows)
      (let ((window (selected-window)))
        (set-window-buffer window (edmacs-sidebar--ensure-buffer frame))
        (set-window-parameter window 'edmacs-main nil)
        (set-window-parameter window 'window-side 'left)
        (set-window-parameter window 'window-slot 0)
        (set-window-parameter window 'no-other-window t)
        (set-window-parameter window 'no-delete-other-windows t)
        (set-window-dedicated-p window t)
        window))

    (ert-deftest edmacs-sidebar-test-hide-on-sole-window-frame-does-not-signal ()
      "`edmacs-sidebar-hide' used to call `delete-window' unconditionally, so
the frame's sole window signalled. It now releases the window in place,
leaving the frame with a real main window rather than a wedged one."
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (let ((window (edmacs-sidebar-test--make-sole-sidebar-window frame)))
                (should (edmacs-windows-frame-wedged-p frame))
                (should-not (edmacs-main-window))
                (should (eq (edmacs-sidebar-hide frame) window))
                (should (window-live-p window))
                (should-not (eq (window-buffer window)
                                (edmacs-sidebar--buffer frame)))
                (should-not (window-dedicated-p window))
                (dolist (parameter '(window-side window-slot
                                     no-other-window no-delete-other-windows))
                  (should-not (window-parameter window parameter)))
                (should-not (edmacs-sidebar--window frame))
                (should-not (edmacs-windows-frame-wedged-p frame))
                (should (eq (edmacs-main-window) window))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-hide-in-ordinary-window-keeps-the-window ()
      "`edmacs-sidebar--window' matches on buffer identity, so it also finds
the sidebar buffer in an ordinary window -- a window the sidebar does not
own and must not delete."
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (let* ((main (selected-window))
                     (other (split-window main nil 'below)))
                (set-window-buffer other (edmacs-sidebar--ensure-buffer frame))
                (should (eq (edmacs-sidebar-hide frame) other))
                (should (window-live-p other))
                (should (window-live-p main))
                (should-not (eq (window-buffer other)
                                (edmacs-sidebar--buffer frame)))
                (should (edmacs-main-window))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-hide-deletes-a-real-side-window ()
      "The normal case is unchanged: a side window with a parent is deleted."
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (let ((window (edmacs-sidebar-show frame)))
                (should (window-live-p window))
                (should (window-parent window))
                (should-not (edmacs-sidebar-hide frame))
                (should-not (window-live-p window))
                (should-not (edmacs-sidebar--window frame))
                (should (edmacs-main-window))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-show-into-mainless-frame-yields-side-window-and-main ()
      "Without the repair, `display-buffer-in-side-window' just reuses the
existing slot-0 left window and the frame stays wedged. The unchanged
frame count is what proves repair rebuilt this frame rather than
escaping to a new one."
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (let ((frames (length (frame-list))))
                (edmacs-sidebar-test--make-sole-sidebar-window frame)
                (should (edmacs-windows-frame-wedged-p frame))
                (should-not (edmacs-main-window))
                (let ((window (edmacs-sidebar-show frame)))
                  (should (window-live-p window))
                  (should (eq (window-parameter window 'window-side) 'left))
                  (should (window-dedicated-p window))
                  (should-not (edmacs-windows-frame-wedged-p frame))
                  (should (= (length (frame-list)) frames))
                  (let ((main (edmacs-main-window)))
                    (should (window-live-p main))
                    (should-not (eq main window))
                    (should-not (window-parameter main 'window-side))))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-show-returns-nil-when-the-left-slot-is-forbidden ()
      "With no left slot available `display-buffer-in-side-window' returns
nil, and `edmacs-sidebar-show' must return nil rather than fall through
to splitting the widest window -- which is what would put the sidebar on
the right of a wide frame. The frame keeps its main window either way."
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (edmacs-window-set-main (selected-window))
              (let ((window-sides-slots '(0 nil nil nil)))
                (should-not (edmacs-sidebar-show frame)))
              (should-not (edmacs-sidebar--side-window frame))
              (should (window-live-p (edmacs-main-window)))
              (should-not (edmacs-windows-frame-wedged-p frame)))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-hide-twice-is-idempotent ()
      "The second call finds no window at all -- `edmacs-sidebar--window'
matches on buffer identity and the buffer is gone from the frame -- so
it returns nil without signalling or re-wedging."
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (edmacs-sidebar-test--make-sole-sidebar-window frame)
              (should (edmacs-sidebar-hide frame))
              (should-not (edmacs-sidebar-hide frame))
              (should-not (edmacs-windows-frame-wedged-p frame))
              (should (window-live-p (edmacs-main-window))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-release-window-deletes-a-parented-side-window ()
      "The one shape `delete-window' is correct for."
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (let ((window (edmacs-sidebar-show frame)))
                (should (window-parent window))
                (should-not (edmacs-sidebar--release-window window frame))
                (should-not (window-live-p window))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-release-window-releases-an-ordinary-window-in-place ()
      "A window the sidebar does not own is never deleted, only handed back."
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (let* ((main (selected-window))
                     (other (split-window main nil 'below))
                     (sidebar (edmacs-sidebar--ensure-buffer frame)))
                (set-window-buffer other sidebar)
                (set-window-dedicated-p other t)
                (should (eq (edmacs-sidebar--release-window other frame) other))
                (should (window-live-p other))
                (should-not (window-dedicated-p other))
                (should-not (eq (window-buffer other) sidebar))
                (dolist (parameter '(window-side window-slot
                                     no-other-window no-delete-other-windows))
                  (should-not (window-parameter other parameter)))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-release-window-falls-back-to-scratch ()
      "When `other-buffer' can only offer the sidebar buffer back, the
released window must not simply re-show it."
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (let* ((window (edmacs-sidebar-test--make-sole-sidebar-window frame))
                     (sidebar (window-buffer window)))
                (cl-letf (((symbol-function 'other-buffer)
                           (lambda (&rest _) sidebar)))
                  (should (eq (edmacs-sidebar--release-window window frame) window)))
                (should (equal (buffer-name (window-buffer window)) "*scratch*"))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    (ert-deftest edmacs-sidebar-test-show-is-registered-on-the-repaired-hook ()
      "Repair hands the frame back a main window but no sidebar; this hook
membership is what puts one back."
      (should (memq #'edmacs-sidebar-show edmacs-windows-frame-repaired-functions)))

    (ert-deftest edmacs-sidebar-test-side-window-accessor-ignores-ordinary-windows ()
      (let ((frame (selected-frame)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (let ((other (split-window (selected-window) nil 'below)))
                (set-window-buffer other (edmacs-sidebar--ensure-buffer frame))
                (should (eq (edmacs-sidebar--window frame) other))
                (should-not (edmacs-sidebar--side-window frame))))
          (edmacs-sidebar-test--cleanup-sidebar frame))))

    )) ; end of build-root-found branch

;;; sidebar-test.el ends here
