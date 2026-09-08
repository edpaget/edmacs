;;; sidebar-test.el --- Tests for sidebar.el -*- lexical-binding: t -*-

;;; Commentary:
;; Unlike claude-term-test.el, sidebar.el's parent mode (`magit-section-mode')
;; is genuinely load-bearing to even parse the file under `-Q --batch':
;; `define-derived-mode' errors if `magit-section-mode' is undefined, and
;; sidebar.el's own `(require 'magit-section)' errors even earlier if
;; `load-path' lacks it. So this file carries its own, different invocation
;; -- and sidebar.el is NOT passed on the command line, because this file
;; fixes `load-path' against the straight build tree and loads sidebar.el
;; (and windows.el, which sidebar.el `require's) itself, below. Loading
;; windows.el also installs its global side effects
;; (`display-buffer-base-action', the `quit-restore-window' advice, the
;; `tab-bar-tab-post-open-functions' hook) into this batch session. With no
;; bootstrapped straight tree in this checkout or its sibling main one, the
;; whole suite reports a single skip rather than erroring on file load.
;;
;;   emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/git-common-dir.el \
;;         -l modules/sidebar-test.el -f edmacs-test-support-run-and-exit
;;
;; Two tests need a second real frame, which needs a controlling terminal:
;; plain `-Q --batch' with no pty has none, so
;; `-singleton-buffer-shared-across-frames' and
;; `-anchor-region-pulls-point-forward-on-unselected-frame' skip cleanly
;; under the invocation above. `scripts/pty-ert.sh' allocates a pty
;; directly (unlike `script', which needs stdin to be a terminal itself)
;; and drives them for real, at the cost of raw escape codes in the
;; output.
;;
;;   scripts/pty-ert.sh emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/git-common-dir.el -l modules/sidebar-test.el -f edmacs-test-support-run-and-exit

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

(defvar edmacs-sidebar-test--build-root
  ;; Formerly this file's own `edmacs-sidebar-test--locate-straight-build-root';
  ;; consolidated into modules/test-support.el's `edmacs-test-support-straight-build-root'.
  (edmacs-test-support-straight-build-root)
  "This checkout's (or its sibling main checkout's) `straight/build' root.
Also reused by the rotate.el lookup below -- a second, independent
optional straight dependency.")

(defconst edmacs-sidebar-test--self-file (or load-file-name buffer-file-name))

(if (null edmacs-sidebar-test--build-root)

    (ert-deftest edmacs-sidebar-test-magit-section-unavailable ()
      (edmacs-test-support-report-suite-unavailable
       edmacs-sidebar-test--self-file
       "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (edmacs-test-support-add-magit-section-deps edmacs-sidebar-test--build-root)
    ;; windows.el first: sidebar.el `require's it for `edmacs-windows-claim-side'.
    (load (expand-file-name "modules/windows.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Test helpers
    ;; ==========================================================================

    (defmacro edmacs-sidebar-test--with-extra-tab (&rest body)
      "Run BODY after adding one tab, restoring the original tab count after
via test-support.el's shared fixture -- every test in this file shares
the same real frame, so a failing assertion must never leave stray tabs
behind for a later test."
      (declare (indent 0))
      `(edmacs-test-support-with-tabs-restored
         (tab-bar-new-tab)
         ,@body))

    (defun edmacs-sidebar-test--cleanup-sidebar (frame)
      "Hide and kill FRAME's sidebar window/buffer, if any."
      (edmacs-sidebar-hide frame)
      (let ((buf (edmacs-sidebar--buffer frame)))
        (when (buffer-live-p buf)
          (kill-buffer buf))
        (set-frame-parameter frame 'edmacs-sidebar-buffer nil)))

    (defmacro edmacs-sidebar-test--with-frame (bindings &rest body)
      "Run BODY over the selected frame, then clean its sidebar up again.
The frame is bound to FRAME -- deliberately anaphoric, since that is the
name the bodies collapsed onto this macro already used.  BINDINGS are
extra `let*' bindings, evaluated after FRAME and visible to BODY: the
`let' head each of these tests used to open by hand.  Forms in BODY after
a `:cleanup' keyword run during the unwind, ahead of
`edmacs-sidebar-test--cleanup-sidebar', which runs whatever BODY did.

Every test in this file shares one real frame, so that teardown is not
optional: a failing assertion must not leave a sidebar window or buffer
behind for the next test to trip over."
      (declare (indent 1) (debug (sexp body)))
      (let* ((tail (memq :cleanup body))
             (main (if tail (butlast body (length tail)) body)))
        `(let* ((frame (selected-frame)) ,@bindings)
           (unwind-protect
               (progn ,@main)
             ,@(cdr tail)
             (edmacs-sidebar-test--cleanup-sidebar frame)))))

    (defun edmacs-sidebar-test--reset-redraw-queue ()
      "Empty `edmacs-sidebar--dirty-frames' and cancel any pending flush."
      (setq edmacs-sidebar--dirty-frames nil)
      (when (timerp edmacs-sidebar--redraw-timer)
        (cancel-timer edmacs-sidebar--redraw-timer))
      (setq edmacs-sidebar--redraw-timer nil))

    (defmacro edmacs-sidebar-test--with-clean-redraw-queue (&rest body)
      "Run BODY over an empty coalescing redraw queue, emptied again after.
One test's pending idle redraw must not fire inside the next one, and a
test asserting on the queue must not inherit a dirty frame from a
previous one."
      (declare (indent 0))
      `(unwind-protect
           (progn (edmacs-sidebar-test--reset-redraw-queue) ,@body)
         (edmacs-sidebar-test--reset-redraw-queue)))

    (defmacro edmacs-sidebar-test--with-no-shellout (&rest body)
      "Run BODY, then assert it reached no subprocess primitive at all.
Every process-spawning entry point is advised to RECORD the call rather
than block it, so a violation names the exact primitive.  The
expectation is zero, never \"zero except N\"; the trailing
`sleep-for'/`sit-for' gives an asynchronous spawn a chance to land
before the assertion reads the tally."
      (declare (indent 0))
      `(let ((violations nil)
             (guarded '(call-process call-process-region process-file
                        start-process start-file-process make-process)))
         (unwind-protect
             (progn
               (dolist (fn guarded)
                 (advice-add fn :before (lambda (&rest _) (push fn violations))
                             (list (cons 'name (edmacs-sidebar-test--guard-name fn)))))
               ,@body
               (sleep-for 0.2)
               (sit-for 0)
               (should-not violations))
           (dolist (fn guarded)
             (advice-remove fn (edmacs-sidebar-test--guard-name fn))))))

    (defun edmacs-sidebar-test--guard-name (fn)
      "Name the `edmacs-sidebar-test--with-no-shellout' advice on FN."
      (intern (format "edmacs-sidebar-test--guard-%s" fn)))

    ;; ==========================================================================
    ;; AC1 -- redraw content, marker, RET-driven visit, 1-based numbering,
    ;; frame-explicit tab-index lookups
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-visit-tab-selects-and-moves-marker ()
      (edmacs-sidebar-test--with-extra-tab
        (edmacs-sidebar-test--with-frame ()
          (edmacs-sidebar-show (selected-frame))
          ;; The newly-added tab is current, at index 1.
          (should (= 1 (tab-bar--current-tab-index)))
          (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
            (goto-char (point-min))
            (edmacs-sidebar-activate))
          ;; `--on-tab-select' now routes through `edmacs-sidebar-
          ;; invalidate' (a deferred idle-0 redraw), not a synchronous
          ;; `--redraw' -- flush it manually, since idle timers never
          ;; fire under `sit-for' in `--batch'.
          (edmacs-sidebar--flush-dirty-frames)
          ;; RET on the first (non-current) row actually selected it --
          ;; not a no-op under `tab-bar-select-tab's 0-as-sentinel
          ;; semantics, and not off-by-one to the tab before it.
          (should (= 0 (tab-bar--current-tab-index)))
          (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
            (goto-char (point-min))
            (should (looking-at-p "●"))))))

    (defun edmacs-sidebar-test--locate-real-evil ()
      "Return the directory holding the real `evil.el', or nil.
Tries `straight/build/evil' first (file-exists-p follows a working
symlink); falls back to `straight/repos/evil' when that symlink is
broken or the build tree was never generated."
      (or
       (let* ((root (or edmacs-sidebar-test--build-root
                         (edmacs-test-support-straight-build-root)))
              (path (and root (expand-file-name "evil/evil.el" root))))
         (and path (file-exists-p path) (file-name-directory path)))
       (let* ((root (edmacs-test-support-straight-repos-root))
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

    (defmacro edmacs-sidebar-test--with-real-evil-motion (&rest body)
      "Load real evil, enter motion state in a fresh sidebar buffer, run BODY.
`evil-mode' is a global minor mode, so it is switched back off after
BODY however BODY ends -- a left-on evil would change key lookup for
every later test in this file."
      (declare (indent 0))
      `(progn
         (edmacs-sidebar-test--ensure-real-evil)
         (unwind-protect
             (progn
               (evil-mode 1)
               (with-temp-buffer
                 (edmacs-sidebar-mode)
                 (evil-motion-state)
                 (should (eq evil-state 'motion))
                 ,@body))
           (evil-mode -1))))

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
      (edmacs-sidebar-test--with-real-evil-motion
        (should (eq (key-binding (kbd "RET")) #'edmacs-sidebar-visit-at-point))
        (should (eq (key-binding (kbd "q")) #'edmacs-sidebar-hide))))

    (ert-deftest edmacs-sidebar-test-visit-at-point-dispatches-by-section-type ()
      "`edmacs-sidebar-visit-at-point' calls `edmacs-sidebar-agents-visit'
on an `edmacs-sidebar-agent' section, `edmacs-sidebar-buffers-visit' on
an `edmacs-sidebar-buffers-file' section, and `edmacs-sidebar-activate'
on every other section type (a plain tab row here) -- sidebar-agents.el
and sidebar-buffers.el are not loaded by this suite, so both real visit
commands are stubbed. This is the proof that RET on an agent or buffer
row never reaches `edmacs-sidebar-activate' at all: the row-type-specific
report coverage for those two (does the real, routed-to command signal
`user-error' instead of silently doing nothing on an actionless row)
lives with the real functions, in sidebar-agents-test.el's
`edmacs-sidebar-agents-test-visit-no-section-reports'/
`-visit-nil-value-reports' and sidebar-buffers-test.el's
`edmacs-sidebar-buffers-test-visit-no-section-reports'/
`-visit-file-row-without-root-reports'."
      (let ((activate-calls 0) (agent-visit-calls 0) (buffer-visit-calls 0))
        (cl-letf (((symbol-function 'edmacs-sidebar-activate)
                   (lambda () (setq activate-calls (1+ activate-calls))))
                  ((symbol-function 'edmacs-sidebar-agents-visit)
                   (lambda () (setq agent-visit-calls (1+ agent-visit-calls))))
                  ((symbol-function 'edmacs-sidebar-buffers-visit)
                   (lambda () (setq buffer-visit-calls (1+ buffer-visit-calls)))))
          (with-temp-buffer
            (edmacs-sidebar-mode)
            (let ((inhibit-read-only t))
              ;; All three rows nested inside one outer wrapper: an
              ;; unwrapped top-level `magit-insert-section' call
              ;; becomes `magit-root-section' itself and is skipped by
              ;; `magit-section--set-section-properties' (see
              ;; magit-section.el's `magit-insert-section--finish'),
              ;; so sibling top-level calls here would leave none of the
              ;; rows' own text actually tagged with its section --
              ;; exactly the real shape `--redraw' always produces via
              ;; its own wrapping `edmacs-sidebar-root'.
              (magit-insert-section (edmacs-sidebar-root)
                (magit-insert-section (edmacs-sidebar-tab 1)
                  (magit-insert-heading "a tab row"))
                (magit-insert-section (edmacs-sidebar-agent "fake-agent")
                  (magit-insert-heading "an agent row"))
                (magit-insert-section (edmacs-sidebar-buffers-file "fake-buffer")
                  (magit-insert-heading "a buffer row"))))
            (goto-char (point-min))
            (edmacs-sidebar-visit-at-point)
            (should (= 1 activate-calls))
            (should (= 0 agent-visit-calls))
            (should (= 0 buffer-visit-calls))
            (forward-line 1)
            (edmacs-sidebar-visit-at-point)
            (should (= 1 activate-calls))
            (should (= 1 agent-visit-calls))
            (should (= 0 buffer-visit-calls))
            (goto-char (point-max))
            (forward-line -1)
            (edmacs-sidebar-visit-at-point)
            (should (= 1 activate-calls))
            (should (= 1 agent-visit-calls))
            (should (= 1 buffer-visit-calls))))))

    ;; ==========================================================================
    ;; The render plan -- pure structure, no frame and no tab fixtures
    ;; ==========================================================================
    ;; `edmacs-sidebar--plan' is a pure function of frame/tab-bar state, so a
    ;; render's whole SHAPE (row kinds, labels, faces, section values,
    ;; nesting, hook arguments) is assertable without opening a real tab,
    ;; priming the git-common-dir cache, or unwinding anything. The fixture
    ;; below is `cl-letf' only, deliberately.

    (defun edmacs-sidebar-test--window-call (&rest _)
      "Signal: a code path that must be pure reached a window primitive."
      (error "window call from a pure path"))

    (defun edmacs-sidebar-test--plan-records (spec)
      "Expand SPEC into an ordered list of (TAB GROUP ROOT KIND) records.
SPEC is a list of plists, one per tab-bar group: :group (nil for a
group-less, flat frame), :main-root (what
`edmacs-sidebar-main-root-function' answers), :collapsed, and :tabs, a
list of (NAME ROOT KIND CURRENT-P) tab specs in order."
      (let (records)
        (dolist (entry spec (nreverse records))
          (let ((group (plist-get entry :group)))
            (dolist (tab-spec (plist-get entry :tabs))
              (push (list (append (list (if (nth 3 tab-spec) 'current-tab 'tab))
                                  (list (cons 'name (nth 0 tab-spec)))
                                  (when group (list (cons 'group group))))
                          group (nth 1 tab-spec) (nth 2 tab-spec))
                    records))))))

    (defmacro edmacs-sidebar-test--with-plan-fixture (spec &rest body)
      "Run BODY with every workspaces lookup `edmacs-sidebar--plan' makes
stubbed from the literal SPEC (see `edmacs-sidebar-test--plan-records').
`cl-letf' only: no real tab, no frame, no primed git-common-dir cache,
nothing to unwind. FRAME is nil in most callers -- the planner never
dereferences it, only hands it on. A :collapsed entry makes
`frame-parameter' answer that flag while every other parameter reads
through; glyphs are forced to the plain-text table; and every root is
live unless BODY rebinds `edmacs-sidebar-worktree-live-p-function'."
      (declare (indent 1))
      `(let* ((records (edmacs-sidebar-test--plan-records ,spec))
              (tabs (mapcar #'car records))
              (groups (delete-dups (delq nil (mapcar (lambda (r) (nth 1 r)) records))))
              (current-group (car (delq nil
                                        (mapcar (lambda (r)
                                                  (and (eq (car (nth 0 r)) 'current-tab)
                                                       (nth 1 r)))
                                                records))))
              (main-roots (mapcar (lambda (e) (cons (plist-get e :group)
                                                    (plist-get e :main-root)))
                                  ,spec))
              (collapsed (seq-some (lambda (e) (plist-get e :collapsed)) ,spec))
              (edmacs-sidebar-worktree-live-p-function #'always)
              (edmacs-sidebar-force-text-glyphs t)
              (edmacs-sidebar-main-root-function
               (lambda (group-tabs)
                 (alist-get (nth 1 (assq (car group-tabs) records))
                            main-roots nil nil #'equal))))
         (cl-letf* ((frame-parameter-orig (symbol-function 'frame-parameter))
                    ((symbol-function 'frame-parameter)
                     (lambda (frame parameter)
                       (if (eq parameter 'edmacs-sidebar-collapsed)
                           collapsed
                         (funcall frame-parameter-orig frame parameter))))
                    ((symbol-function 'tab-bar-tabs) (lambda (&optional _f) tabs))
                    ((symbol-function 'edmacs-workspaces-groups) (lambda (&optional _f) groups))
                    ((symbol-function 'edmacs-workspaces-current-group)
                     (lambda (&optional _f) current-group))
                    ((symbol-function 'edmacs-workspaces-tabs-in-group)
                     (lambda (group &optional _f)
                       (mapcar #'car (seq-filter (lambda (r) (equal group (nth 1 r))) records))))
                    ((symbol-function 'edmacs-workspaces-tab-root)
                     (lambda (tab) (nth 2 (assq tab records))))
                    ((symbol-function 'edmacs-workspaces-classify-root)
                     (lambda (root)
                       (nth 3 (seq-find (lambda (r) (equal root (nth 2 r))) records))))
                    ((symbol-function 'edmacs-workspaces-tab-number)
                     (lambda (tab &optional _f)
                       (when-let* ((i (seq-position tabs tab #'eq))) (1+ i)))))
           ,@body)))

    (defconst edmacs-sidebar-test--two-project-spec
      '((:group "repoA" :main-root "/repoA/main/"
         :tabs (("main" "/repoA/main/" main t)
                ("roadmap-foo" "/repoA__worktrees/roadmap-foo/" roadmap nil)))
        (:group "repoB" :main-root "/repoB/main/"
         :tabs (("main" "/repoB/main/" main nil))))
      "repoA with a current main tab plus one roadmap worktree; repoB with
only its main tab.")

    (defun edmacs-sidebar-test--rows-of-kind (kind rows)
      "Return the members of ROWS whose :kind is KIND."
      (seq-filter (lambda (row) (eq (plist-get row :kind) kind)) rows))

    (ert-deftest edmacs-sidebar-test-plan-grouped-shape ()
      "AC1: one `project' row per group, worktrees only under their own
project, the count matching the child rows, no row for a never-opened
worktree, a `roadmap-'-stripped child name, the `(GROUP . ROOT)' :value
`--capture-positions' keys point and fold state on, the active group's
current-tab face vs. an inactive group's nil face vs. the worktree hue,
and the trailing extra-section and :anchor'd bottom-anchor hook rows."
      (edmacs-sidebar-test--with-plan-fixture edmacs-sidebar-test--two-project-spec
        (let* ((rows (edmacs-sidebar--plan nil))
               (projects (edmacs-sidebar-test--rows-of-kind 'project rows))
               (a (nth 0 projects))
               (b (nth 1 projects))
               (children (edmacs-sidebar-test--rows-of-kind 'worktree (plist-get a :children)))
               (tail (last rows 2)))
          (should (= 2 (length projects)))
          (should (equal "● repoA [1]" (plist-get a :label)))
          (should (equal "○ repoB [0]" (plist-get b :label)))
          (should (equal (cons "repoA" "/repoA/main/") (plist-get a :value)))
          (should (equal (cons "repoB" "/repoB/main/") (plist-get b :value)))
          (should (= 1 (length children)))
          (should (equal "  ◆ foo" (plist-get (car children) :label)))
          (should (equal (cons "repoA" "/repoA__worktrees/roadmap-foo/")
                         (plist-get (car children) :value)))
          (should-not (edmacs-sidebar-test--rows-of-kind 'worktree (plist-get b :children)))
          (should (eq 'edmacs-sidebar-current-tab-face (plist-get a :face)))
          (should-not (plist-get b :face))
          (should (eq 'edmacs-sidebar-worktree-child-face (plist-get (car children) :face)))
          (should (equal 'edmacs-sidebar-extra-section-functions (plist-get (nth 0 tail) :hook)))
          (should-not (plist-get (nth 0 tail) :anchor))
          (should (equal 'edmacs-sidebar-bottom-anchor-section-functions
                         (plist-get (nth 1 tail) :hook)))
          (should (equal '(nil) (plist-get (nth 1 tail) :args)))
          (should (plist-get (nth 1 tail) :anchor)))))

    (ert-deftest edmacs-sidebar-test-plan-and-render-flat-tabs ()
      "A frame carrying no tab-bar group at all (the daemon's boot/spare
frame) plans a flat list of `tab' rows keyed on the bare 1-BASED tab
number -- 0 is `tab-bar-select-tab's sentinel, so a 0 would make the
first row unselectable. Only the current tab takes the current-tab glyph
and face; the render is one unindented row each."
      (edmacs-sidebar-test--with-plan-fixture
          '((:group nil :tabs (("one" nil nil nil) ("two" nil nil t))))
        (let ((rows (edmacs-sidebar-test--rows-of-kind 'tab (edmacs-sidebar--plan nil))))
          (should (equal '(1 2) (mapcar (lambda (r) (plist-get r :value)) rows)))
          (should (equal '("○ one" "● two") (mapcar (lambda (r) (plist-get r :label)) rows)))
          (should-not (plist-get (nth 0 rows) :face))
          (should (eq 'edmacs-sidebar-current-tab-face (plist-get (nth 1 rows) :face))))
        (should (equal "○ one\n● two\n"
                       (substring-no-properties
                        (car (edmacs-sidebar-test--render-to-string
                              (edmacs-sidebar--plan nil) 20)))))))

    (ert-deftest edmacs-sidebar-test-plan-flat-tab-with-a-root-carries-its-identity ()
      "A flat-list tab that DOES carry a worktree root gets the same
`(GROUP . ROOT)' value a project/worktree row does, so RET resolves it
by identity instead of by a position a reorder can invalidate. The
frame has no group at all here, so the GROUP half is nil -- that is the
documented shape, not a defect. A rootless sibling keeps its bare tab
number: it has no identity to key on, and `(nil . nil)' would turn RET
on the daemon's boot tab into a `user-error'."
      (edmacs-sidebar-test--with-plan-fixture
          '((:group nil :tabs (("boot" nil nil nil)
                               ("wt" "/repoZ/wt/" roadmap t))))
        (let ((rows (edmacs-sidebar-test--rows-of-kind 'tab (edmacs-sidebar--plan nil))))
          (should (equal (list 1 (cons nil "/repoZ/wt/"))
                         (mapcar (lambda (r) (plist-get r :value)) rows))))))

    (ert-deftest edmacs-sidebar-test-plan-collapsed ()
      "A collapsed frame plans exactly the two collapsed hook rows -- no
project, worktree or tab row at all -- with :anchor on the second and
the `edmacs-sidebar--width' sentinel in place of the width only
`edmacs-sidebar--render' can supply."
      (edmacs-sidebar-test--with-plan-fixture
          (append edmacs-sidebar-test--two-project-spec '((:collapsed t)))
        (let ((rows (edmacs-sidebar--plan nil)))
          (should (equal '(hook hook) (mapcar (lambda (r) (plist-get r :kind)) rows)))
          (should (equal 'edmacs-sidebar-collapsed-section-functions
                         (plist-get (nth 0 rows) :hook)))
          (should (equal 'edmacs-sidebar-collapsed-bottom-anchor-section-functions
                         (plist-get (nth 1 rows) :hook)))
          (should-not (plist-get (nth 0 rows) :anchor))
          (should (plist-get (nth 1 rows) :anchor))
          (should (equal (list nil edmacs-sidebar--width) (plist-get (nth 0 rows) :args)))
          (should (equal (list nil edmacs-sidebar--width) (plist-get (nth 1 rows) :args))))))

    (ert-deftest edmacs-sidebar-test-plan-marks-a-missing-root ()
      "A row whose stamped root is gone from disk carries a trailing
\" (missing)\" and `edmacs-sidebar-missing-worktree-face', which
OUTRANKS both the current-tab face and the worktree-child hue, on a
worktree row and a project row alike. A live sibling is untouched."
      (edmacs-sidebar-test--with-plan-fixture
          '((:group "repoM" :main-root "/repoM/main/"
             :tabs (("main" "/repoM/main/" main nil)
                    ("gone" "/repoM__worktrees/gone/" nil t)
                    ("alive" "/repoM__worktrees/alive/" nil nil))))
        (let* ((edmacs-sidebar-worktree-live-p-function
                (lambda (root) (not (equal root "/repoM__worktrees/gone/"))))
               (project (car (edmacs-sidebar-test--rows-of-kind
                              'project (edmacs-sidebar--plan nil))))
               (children (edmacs-sidebar-test--rows-of-kind
                          'worktree (plist-get project :children))))
          ;; "gone" is also the frame's current tab -- the marker still wins.
          (should (equal "  · gone (missing)" (plist-get (nth 0 children) :label)))
          (should (eq 'edmacs-sidebar-missing-worktree-face (plist-get (nth 0 children) :face)))
          (should (equal "  · alive" (plist-get (nth 1 children) :label)))
          (should (eq 'edmacs-sidebar-worktree-child-face (plist-get (nth 1 children) :face)))
          (should (eq 'edmacs-sidebar-current-tab-face (plist-get project :face)))
          (let ((edmacs-sidebar-worktree-live-p-function #'ignore))
            (should (eq 'edmacs-sidebar-missing-worktree-face
                        (plist-get (car (edmacs-sidebar-test--rows-of-kind
                                         'project (edmacs-sidebar--plan nil)))
                                   :face)))))))

    (ert-deftest edmacs-sidebar-test-plan-hook-rows-carry-root-has-tab-frame-and-tab-number ()
      "`edmacs-sidebar-worktree-section-functions' keeps its published
\(ROOT HAS-TAB FRAME TAB-NUMBER\) tuple, materialized as data on a
`hook' row nested inside the row it belongs to: a project row whose main
tab IS open reports HAS-TAB non-nil and that tab's own 1-based number, a
project row whose main tab is NOT open reports nil for both, and a
worktree row always reports HAS-TAB t. That second group also covers the
derived main root: a group with only a linked worktree open still plans
ONE project row, at the root `edmacs-sidebar-main-root-function' derives
-- never a second, tabless project row, and never crashing."
      (edmacs-sidebar-test--with-plan-fixture
          '((:group "repoJ" :main-root "/repoJ/main/"
             :tabs (("main" "/repoJ/main/" main t)
                    ("roadmap-wt" "/repoJ__worktrees/roadmap-wt/" roadmap nil)))
            (:group "repoK" :main-root "/repoK/main/"
             :tabs (("roadmap-wt" "/repoK__worktrees/roadmap-wt/" roadmap nil))))
        (let* ((projects (edmacs-sidebar-test--rows-of-kind
                          'project (edmacs-sidebar--plan nil)))
               (hook-args (lambda (row)
                            (plist-get (car (edmacs-sidebar-test--rows-of-kind
                                             'hook (plist-get row :children)))
                                       :args))))
          (should (equal 'edmacs-sidebar-worktree-section-functions
                         (plist-get (car (plist-get (nth 0 projects) :children)) :hook)))
          (should (equal (list "/repoJ/main/" t nil 1) (funcall hook-args (nth 0 projects))))
          (should (equal (list "/repoJ__worktrees/roadmap-wt/" t nil 2)
                         (funcall hook-args
                                  (car (edmacs-sidebar-test--rows-of-kind
                                        'worktree (plist-get (nth 0 projects) :children))))))
          (should (equal (list "/repoK/main/" nil nil nil)
                         (funcall hook-args (nth 1 projects))))
          (should (= 2 (length projects)))
          (should (equal (cons "repoK" "/repoK/main/") (plist-get (nth 1 projects) :value)))
          (should (equal "○ repoK [1]" (plist-get (nth 1 projects) :label)))
          (should (= 1 (length (edmacs-sidebar-test--rows-of-kind
                                'worktree (plist-get (nth 1 projects) :children))))))))

    (ert-deftest edmacs-sidebar-test-plan-passes-frame-explicitly ()
      "Regression test for the frame-mismatch fix: every workspaces lookup
the planner makes must be handed FRAME explicitly. The 0-arg form's
`(selected-frame)' default silently returns nil for a tab belonging to a
non-selected frame, making that frame's rows unselectable via RET."
      (edmacs-sidebar-test--with-plan-fixture edmacs-sidebar-test--two-project-spec
        (let (seen)
          (cl-letf* ((number-orig (symbol-function 'edmacs-workspaces-tab-number))
                     (group-orig (symbol-function 'edmacs-workspaces-tabs-in-group))
                     ((symbol-function 'edmacs-workspaces-tab-number)
                      (lambda (tab &optional frame)
                        (push frame seen) (funcall number-orig tab frame)))
                     ((symbol-function 'edmacs-workspaces-tabs-in-group)
                      (lambda (group &optional frame)
                        (push frame seen) (funcall group-orig group frame))))
            (edmacs-sidebar--plan 'test-frame))
          (should seen)
          (dolist (frame seen) (should (eq 'test-frame frame))))))

    (ert-deftest edmacs-sidebar-test-plan-and-truncate-make-no-window-calls ()
      "AC5: the planner and `edmacs-sidebar--truncate-label' are pure of
window measurement. Every window primitive either could reach signals --
`window-list' included, since `edmacs-sidebar--window' reaches a window
through it, so stubbing only `window-width'/`get-buffer-window' would
miss a regression there."
      (edmacs-sidebar-test--with-plan-fixture edmacs-sidebar-test--two-project-spec
        (cl-letf (((symbol-function 'window-width) #'edmacs-sidebar-test--window-call)
                  ((symbol-function 'window-body-width) #'edmacs-sidebar-test--window-call)
                  ((symbol-function 'window-body-size) #'edmacs-sidebar-test--window-call)
                  ((symbol-function 'get-buffer-window) #'edmacs-sidebar-test--window-call)
                  ((symbol-function 'get-buffer-window-list) #'edmacs-sidebar-test--window-call)
                  ((symbol-function 'window-list) #'edmacs-sidebar-test--window-call))
          (let ((projects (edmacs-sidebar-test--rows-of-kind
                           'project (edmacs-sidebar--plan nil))))
            (should (= 2 (length projects)))
            (should (equal "● repoA [1]" (plist-get (car projects) :label))))
          (should (equal "a-long-…" (edmacs-sidebar--truncate-label "a-long-label" 8))))))

    ;; A small golden set: the renderer turns plan rows into exactly this
    ;; buffer text, with exactly these text properties.

    (defun edmacs-sidebar-test--render-to-string (rows width)
      "Render ROWS at WIDTH into a fresh sidebar-mode buffer, returning
\(TEXT . ANCHOR-REGION)."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let* ((inhibit-read-only t)
               (region (edmacs-sidebar--render rows width)))
          (cons (buffer-string) region))))

    (ert-deftest edmacs-sidebar-test-render-grouped-golden ()
      "The grouped plan inserts one heading line per row, children indented
under their project, propertizes a label only where the row carries a
face, and truncates to the width it is HANDED, never one it measures."
      (edmacs-sidebar-test--with-plan-fixture edmacs-sidebar-test--two-project-spec
        (let* ((rendered (edmacs-sidebar-test--render-to-string
                          (edmacs-sidebar--plan nil) 20))
               (text (car rendered)))
          (should (equal "● repoA [1]\n  ◆ foo\n○ repoB [0]\n"
                         (substring-no-properties text)))
          (should (eq 'edmacs-sidebar-current-tab-face (get-text-property 0 'face text)))
          (should-not (get-text-property (string-match "○ repoB" text) 'face text))
          ;; No row was marked :anchor, so no anchored region came back.
          (should-not (cdr rendered))
          (should (equal "● rep…\n  ◆ f…\n○ rep…\n"
                         (substring-no-properties
                          (car (edmacs-sidebar-test--render-to-string
                                (edmacs-sidebar--plan nil) 6))))))))

    (ert-deftest edmacs-sidebar-test-render-collapsed-golden-substitutes-the-width ()
      "The collapsed plan inserts only what its two hooks insert, the
`edmacs-sidebar--width' sentinel is replaced by the renderer's own WIDTH
argument, and the returned region brackets the second hook's insertion."
      (edmacs-sidebar-test--with-plan-fixture
          (append edmacs-sidebar-test--two-project-spec '((:collapsed t)))
        (let* ((widths nil)
               (edmacs-sidebar-collapsed-section-functions
                (list (lambda (_frame width) (push width widths) (insert "strip\n"))))
               (edmacs-sidebar-collapsed-bottom-anchor-section-functions
                (list (lambda (_frame width) (push width widths) (insert "usage\n"))))
               (rendered (edmacs-sidebar-test--render-to-string
                          (edmacs-sidebar--plan nil) 4)))
          (should (equal '(4 4) widths))
          (should (equal "strip\nusage\n" (substring-no-properties (car rendered))))
          (should (equal (cons 7 13) (cdr rendered))))))

    (ert-deftest edmacs-sidebar-test-render-hook-row-nests-inside-its-own-row ()
      "A function on `edmacs-sidebar-worktree-section-functions' inserts
its section from inside the row's own `magit-insert-section' body: the
contributed section's PARENT is that row's own `edmacs-sidebar-tab'
section, not `magit-root-section' and not the project row above it."
      (edmacs-sidebar-test--with-plan-fixture
          '((:group "repoJ" :main-root "/repoJ/main/"
             :tabs (("main" "/repoJ/main/" main t)
                    ("roadmap-wt" "/repoJ__worktrees/roadmap-wt/" roadmap nil))))
        (let* (sections
               (edmacs-sidebar-worktree-section-functions
                (list (lambda (root _has-tab _frame _tab-number)
                        (push (cons root
                                    (magit-insert-section (edmacs-sidebar-test-child nil)
                                      (magit-insert-heading "    test child")))
                              sections)))))
          (edmacs-sidebar-test--render-to-string (edmacs-sidebar--plan nil) 40)
          (dolist (expected '(("/repoJ/main/" . ("repoJ" . "/repoJ/main/"))
                              ("/repoJ__worktrees/roadmap-wt/"
                               . ("repoJ" . "/repoJ__worktrees/roadmap-wt/"))))
            (let ((parent (oref (cdr (assoc (car expected) sections)) parent)))
              (should (eq (oref parent type) 'edmacs-sidebar-tab))
              (should (equal (cdr expected) (oref parent value))))))))

    ;; ==========================================================================
    ;; Workspaces model fixtures (edmacs-tab-groups phase 3)
    ;; ==========================================================================
    ;; workspaces.el itself is not loaded by this suite's invocation (see
    ;; this file's Commentary) -- sidebar.el only ever calls it through its
    ;; own `declare-function' forward references. Rather than `cl-letf'-
    ;; stubbing each one per test, the small, pure lookups sidebar.el's
    ;; grouped-tree render/activate path calls are defined here for REAL,
    ;; as exact copies of workspaces.el's logic over real `tab-bar.el'
    ;; primitives. A test that never assigns a tab-bar `group' parameter
    ;; therefore falls through to the flat tab list with no stub at all.
    ;; `edmacs-workspaces-classify-root' is real too, over the real
    ;; `edmacs-git-common-dir-cache' -- which is why every fixture ROOT is
    ;; pre-warmed into that cache by `edmacs-sidebar-test--with-project',
    ;; since an uncached one would shell out to git from inside a redraw.
    ;; `-open-project'/`-open-worktree' (real `dired'/`tab-bar-new-tab'/
    ;; `vc-git' side effects) are stubbed per-test instead.
    ;;
    ;; Structure-only assertions do NOT belong on this fixture -- the plan
    ;; tests above cover those. What is left needs real tab-bar state:
    ;; activation, close, rename, and the survival tripwires.

    (defconst edmacs-sidebar-test--root-parameter 'edmacs-workspace-root)

    (defun edmacs-workspaces-groups (&optional frame)
      (delete-dups (delq nil (mapcar (lambda (tab) (funcall tab-bar-tab-group-function tab))
                                       (tab-bar-tabs (or frame (selected-frame)))))))

    (defun edmacs-workspaces-tabs-in-group (group &optional frame)
      (when group
        (seq-filter (lambda (tab) (equal (funcall tab-bar-tab-group-function tab) group))
                    (tab-bar-tabs (or frame (selected-frame))))))

    (defun edmacs-workspaces-tab-root (tab)
      (alist-get edmacs-sidebar-test--root-parameter tab))

    (defun edmacs-workspaces-current-group (&optional frame)
      (when-let* ((tab (assq 'current-tab
                             (frame-parameter (or frame (selected-frame)) 'tabs))))
        (funcall tab-bar-tab-group-function tab)))

    (defun edmacs-workspaces-set-tab-root (root &optional frame)
      (when-let* ((tab (tab-bar--current-tab-find nil frame)))
        (setf (alist-get edmacs-sidebar-test--root-parameter (cdr tab)) root)
        root))

    (defun edmacs-workspaces-tab-number (tab &optional frame)
      (let* ((target (or frame (selected-frame)))
             (index (tab-bar--tab-index tab (tab-bar-tabs target) target)))
        (and index (1+ index))))

    ;; Root-keyed, exactly as workspaces.el is: a tab's group is derived
    ;; FROM its root there, so a group test could only ever hide the tab.
    (defun edmacs-workspaces-find-tab (root &optional frame)
      (seq-find (lambda (tab) (equal (edmacs-workspaces-tab-root tab) root))
                (tab-bar-tabs (or frame (selected-frame)))))

    (defun edmacs-workspaces-select-tab (root &optional frame)
      (let* ((target (or frame (selected-frame)))
             (tab (edmacs-workspaces-find-tab root target)))
        (when tab
          (let ((number (1+ (tab-bar--tab-index tab (tab-bar-tabs target) target))))
            (if frame (with-selected-frame frame (tab-bar-select-tab number))
              (tab-bar-select-tab number))))
        tab))

    (defun edmacs-workspaces-frame-usable-p (frame)
      (and (frame-live-p frame)
           (not (frame-parameter frame 'parent-frame))
           (not (and (daemonp) (frame-initial-p frame)))
           (or (display-graphic-p frame)
               (not (seq-some (lambda (f) (and (frame-live-p f) (display-graphic-p f)))
                              (frame-list))))))

    (defun edmacs-workspaces-main-root (root)
      (let ((main (when-let* ((common (edmacs-git-common-dir root)))
                    (edmacs-git-common-dir-main-worktree common))))
        (file-name-as-directory (file-truename (or main root)))))

    (defun edmacs-workspaces-classify-root (root)
      (when root
        (let* ((common (edmacs-git-common-dir root))
               (main (and common (edmacs-git-common-dir-main-worktree common))))
          (if (and main (equal (file-truename (file-name-as-directory root))
                                (file-truename (file-name-as-directory main))))
              'main
            (let* ((clean (directory-file-name root))
                   (leaf (file-name-nondirectory clean))
                   (parent-dir (file-name-directory clean))
                   (parent (and parent-dir (file-name-nondirectory (directory-file-name parent-dir)))))
              (cond
               ((and parent (string-suffix-p "__worktrees" parent) (string-prefix-p "roadmap-" leaf)) 'roadmap)
               ((and parent (string-suffix-p "__worktrees" parent) (string-prefix-p "task-" leaf)) 'task)
               (t nil)))))))

    (defmacro edmacs-sidebar-test--with-project (spec &rest body)
      "Create real tab-bar tabs per SPEC, run BODY, then close every tab
this macro created (restoring the original tab count) and remove any
`edmacs-git-common-dir-cache' entries it primed.

SPEC is a list of (GROUP MAIN-ROOT MAIN-COMMON . CHILDREN) entries, one
per project group -- MAIN-COMMON is the git-common-dir MAIN-ROOT's own
repo resolves to, primed into the REAL `edmacs-git-common-dir-cache' so
`edmacs-workspaces-classify-root'/sidebar.el's own main-root derivation
never shells out for a fixture path that names no real directory.
CHILDREN is a list of (ROOT NAME) pairs, each opened as its own real
tab in GROUP (via `tab-bar-new-tab'/`tab-bar-rename-tab'/
`tab-bar-change-tab-group'), stamped with worktree root ROOT via
`edmacs-workspaces-set-tab-root'. MAIN-ROOT itself is NOT opened as a
tab unless it also appears in CHILDREN -- modeling \"no open main
tab\" by default, since that is this phase's own new edge case."
      (declare (indent 1))
      ;; Fixture roots ("/repoA/main/" and friends) name no real
      ;; directory, so the real `file-directory-p' probe would render
      ;; every row as a missing worktree. Bound to `always' here: these
      ;; tests are about shape, glyph and current-tab hue, not staleness
      ;; -- `edmacs-sidebar-test-missing-worktree-row-is-marked' below
      ;; drives the probe deliberately, against real directories.
      `(let ((edmacs-sidebar-worktree-live-p-function #'always)
             (edmacs-sidebar-test--primed-roots nil))
         (unwind-protect
             (edmacs-test-support-with-tabs-restored
               (dolist (entry ,spec)
                 (let* ((group (nth 0 entry)) (main-root (nth 1 entry)) (main-common (nth 2 entry))
                        (children (nthcdr 3 entry)))
                   (push main-root edmacs-sidebar-test--primed-roots)
                   (puthash main-root main-common edmacs-git-common-dir-cache)
                   (dolist (child children)
                     (let ((root (nth 0 child)) (name (nth 1 child)))
                       (push root edmacs-sidebar-test--primed-roots)
                       (puthash root main-common edmacs-git-common-dir-cache)
                       (tab-bar-new-tab)
                       (tab-bar-rename-tab name)
                       (edmacs-workspaces-set-tab-root root)
                       (tab-bar-change-tab-group group)))))
               ,@body)
           (dolist (root edmacs-sidebar-test--primed-roots)
             (remhash root edmacs-git-common-dir-cache)))))

    (ert-deftest edmacs-sidebar-test-plan-files-a-tab-by-the-group-function-not-the-stored-param ()
      "A tab whose stored `group' disagrees with its root is filed under the
root's project, because every bucketing step the planner takes --
`edmacs-workspaces-groups', `-tabs-in-group', `-current-group' -- reads
`tab-bar-tab-group-function', never `(alist-get \\='group tab)'.
Production installs `edmacs-workspaces-tab-group' there, which derives
the group from the root; this test installs an equivalent root-deriving
lambda so the assertion needs no git resolution at all.

The pre-phase bug this pins: `SPC T n' made a tab inherit the
ORIGINATING tab's group and then stamped it with its own root, so the
sidebar bucketed it by group and classified it by root -- and it
rendered as a kind-nil child under the wrong project."
      (edmacs-test-support-with-tabs-restored
        (let* ((root "/repoN__worktrees/roadmap-n/")
               (common "/repoN/main/.git")
               (tab-bar-tab-group-function
                (lambda (tab)
                  (when-let* ((r (edmacs-workspaces-tab-root tab)))
                    (if (string-match-p "repoN" r) "repoN" "other")))))
          (puthash root common edmacs-git-common-dir-cache)
          (puthash "/repoN/main/" common edmacs-git-common-dir-cache)
          (unwind-protect
              (progn
                (tab-bar-new-tab)
                (tab-bar-rename-tab "roadmap-n")
                (edmacs-workspaces-set-tab-root root)
                ;; The disagreement: a stored group naming another project.
                (setf (alist-get 'group (cdr (tab-bar--current-tab-find))) "other")
                (let* ((edmacs-sidebar-force-text-glyphs t)
                       (edmacs-sidebar-main-root-function (lambda (_tabs) "/repoN/main/"))
                       (rows (edmacs-sidebar--plan (selected-frame)))
                       (projects (seq-filter
                                  (lambda (r) (eq (plist-get r :kind) 'project)) rows))
                       (repo-n (seq-find (lambda (r) (equal (plist-get r :group) "repoN"))
                                         projects))
                       (other (seq-find (lambda (r) (equal (plist-get r :group) "other"))
                                        projects)))
                  (should repo-n)
                  (should-not other)
                  (should (member root
                                  (mapcar (lambda (r) (plist-get r :root))
                                          (seq-filter
                                           (lambda (r) (eq (plist-get r :kind) 'worktree))
                                           (plist-get repo-n :children)))))))
            (remhash root edmacs-git-common-dir-cache)
            (remhash "/repoN/main/" edmacs-git-common-dir-cache)))))

    (ert-deftest edmacs-sidebar-test-activate-project-row-opens-once-then-reselects ()
      "RET on a project row whose main tab isn't open calls
`edmacs-workspaces-open-project' exactly once; RET again finds the
now-open main tab and reselects rather than opening a second -- the
regression test for AC5's stale `edmacs-root' lookup phase 2 left
behind, and for AC7's \"never `edmacs-workspaces-open-worktree' on a
project row\" claim."
      (edmacs-sidebar-test--with-project
          '(("repoD" "/repoD/main/" "/repoD/main/.git"
             ("/repoD__worktrees/roadmap-y/" "roadmap-y")))
        (let ((open-calls 0) (worktree-open-calls 0))
          (cl-letf (((symbol-function 'edmacs-workspaces-open-project)
                     (lambda (root)
                       (setq open-calls (1+ open-calls))
                       (tab-bar-new-tab)
                       (tab-bar-rename-tab "main")
                       (edmacs-workspaces-set-tab-root root)
                       (tab-bar-change-tab-group "repoD")))
                    ((symbol-function 'edmacs-workspaces-open-worktree)
                     (lambda (_root) (setq worktree-open-calls (1+ worktree-open-calls)))))
            (edmacs-sidebar-test--with-frame ()
              (edmacs-sidebar-show (selected-frame))
              (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                (goto-char (point-min))
                (edmacs-sidebar-activate))
              (should (= 1 open-calls))
              (let ((before (length (tab-bar-tabs))))
                (edmacs-sidebar--redraw (selected-frame))
                (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                  (goto-char (point-min))
                  (edmacs-sidebar-activate))
                (should (= 1 open-calls))
                (should (= before (length (tab-bar-tabs)))))
              (should (= 0 worktree-open-calls)))))))

    (ert-deftest edmacs-sidebar-test-close-worktree-project-row-noop-child-row-closes ()
      "`d' on a project row whose main tab isn't open is a no-op (never
opens one); `d' on an open worktree child row closes its tab."
      (edmacs-sidebar-test--with-project
          '(("repoE" "/repoE/main/" "/repoE/main/.git"
             ("/repoE__worktrees/roadmap-x/" "roadmap-x")))
        (let (closed)
          (cl-letf (((symbol-function 'tab-bar-close-tab) (lambda (n) (push n closed))))
            (edmacs-sidebar-test--with-frame ()
              (edmacs-sidebar-show (selected-frame))
              (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                ;; First (top-level) row: the project, main tab closed.
                (goto-char (point-min))
                (edmacs-sidebar-close-worktree)
                (should-not closed)
                ;; Its child row: "x", open.
                (forward-line 1)
                (edmacs-sidebar-close-worktree)
                (should closed)))))))

    ;; ==========================================================================
    ;; edmacs-sidebar-activate direct-call coverage, one per row-type in the
    ;; phase body's table -- every non-acting path must report via
    ;; `user-error' rather than silently doing nothing.
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-activate-worktree-row-tab-opens-tab ()
      "A `(GROUP . ROOT)' row value that `edmacs-workspaces-find-tab'
resolves to an open tab selects it via `edmacs-workspaces-select-tab',
never `edmacs-workspaces-open-project'/`-open-worktree' -- and passes
GROUP through, so a same-root match in a different group could never
be mistaken for this one (AC4)."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)
            (magit-insert-section (edmacs-sidebar-tab (cons "g1" "/repo/wt/"))
              (magit-insert-heading "wt row"))))
        (goto-char (point-min))
        (let (select-calls open-project-calls open-worktree-calls found-calls)
          (cl-letf (((symbol-function 'edmacs-workspaces-select-tab)
                     (lambda (root) (push root select-calls)))
                    ((symbol-function 'edmacs-workspaces-open-project)
                     (lambda (root) (push root open-project-calls)))
                    ((symbol-function 'edmacs-workspaces-open-worktree)
                     (lambda (root) (push root open-worktree-calls)))
                    ((symbol-function 'edmacs-workspaces-find-tab)
                     (lambda (root) (push root found-calls) 'fake-tab)))
            (edmacs-sidebar-activate))
          ;; The ROW's GROUP half is never passed: lookup is keyed on ROOT.
          (should (equal found-calls '("/repo/wt/")))
          (should (equal select-calls '("/repo/wt/")))
          (should-not open-project-calls)
          (should-not open-worktree-calls))))

    (ert-deftest edmacs-sidebar-test-activate-worktree-row-no-match-main-opens-project ()
      "A `(GROUP . ROOT)' row whose ROOT classifies `main' but has no
matching open tab opens the whole project via
`edmacs-workspaces-open-project', never `-open-worktree' -- the
defensive fallback AC7 says only a project row can reach."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)
            (magit-insert-section (edmacs-sidebar-tab (cons "g1" "/repo/main/"))
              (magit-insert-heading "project row"))))
        (goto-char (point-min))
        (let (select-calls open-project-calls open-worktree-calls)
          (cl-letf (((symbol-function 'edmacs-workspaces-select-tab)
                     (lambda (root) (push root select-calls)))
                    ((symbol-function 'edmacs-workspaces-open-project)
                     (lambda (root) (push root open-project-calls)))
                    ((symbol-function 'edmacs-workspaces-open-worktree)
                     (lambda (root) (push root open-worktree-calls)))
                    ((symbol-function 'edmacs-workspaces-find-tab) (lambda (_root) nil))
                    ((symbol-function 'edmacs-workspaces-classify-root) (lambda (_root) 'main)))
            (edmacs-sidebar-activate))
          (should (equal open-project-calls '("/repo/main/")))
          (should-not open-worktree-calls)
          (should-not select-calls))))

    (ert-deftest edmacs-sidebar-test-activate-worktree-row-no-match-non-main-reports ()
      "A `(GROUP . ROOT)' row with no matching open tab and a non-`main'
classification (the never-should-happen defensive case for a worktree
child row, whose value is always drawn from an already-open tab)
signals `user-error' rather than opening anything."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)
            (magit-insert-section (edmacs-sidebar-tab (cons "g1" "/repo/wt/"))
              (magit-insert-heading "wt row"))))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'edmacs-workspaces-find-tab) (lambda (_root) nil))
                  ((symbol-function 'edmacs-workspaces-classify-root) (lambda (_root) 'roadmap)))
          (should-error (edmacs-sidebar-activate) :type 'user-error))))

    (ert-deftest edmacs-sidebar-test-activate-flat-tab-row-selects-tab ()
      "A repo-less flat tab row's bare integer value selects that tab
directly (no worktree root involved)."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)
            (magit-insert-section (edmacs-sidebar-tab 3)
              (magit-insert-heading "flat tab row"))))
        (goto-char (point-min))
        (let (select-calls)
          (cl-letf (((symbol-function 'tab-bar-select-tab)
                     (lambda (n) (push n select-calls))))
            (edmacs-sidebar-activate))
          (should (equal select-calls '(3))))))

    (ert-deftest edmacs-sidebar-test-activate-called-directly-on-agent-value-reports ()
      "A synthetic, direct call to `edmacs-sidebar-activate' -- NOT the real
RET path, which `edmacs-sidebar-test-visit-at-point-dispatches-by-section-type'
above proves routes an `edmacs-sidebar-agent' row to
`edmacs-sidebar-agents-visit' before `edmacs-sidebar-activate' is ever
reached. This is defensive coverage for `edmacs-sidebar-activate' itself
staying honest if it is ever called some other way (bound to a key
directly, called from a future extension point, etc.): an agent row's
value -- neither `integerp' nor `consp' -- has nothing for it to act on
and reports rather than silently doing nothing. The real, RET-reachable
report coverage for an actionless agent row lives in
sidebar-agents-test.el's `edmacs-sidebar-agents-test-visit-no-section-reports'
and `-visit-nil-value-reports', against the real
`edmacs-sidebar-agents-visit'."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)
            (magit-insert-section (edmacs-sidebar-agent "fake-agent")
              (magit-insert-heading "agent row"))))
        (goto-char (point-min))
        (should-error (edmacs-sidebar-activate) :type 'user-error)))

    (ert-deftest edmacs-sidebar-test-activate-called-directly-on-buffer-value-reports ()
      "A synthetic, direct call to `edmacs-sidebar-activate' -- NOT the real
RET path, which `edmacs-sidebar-test-visit-at-point-dispatches-by-section-type'
above proves routes an `edmacs-sidebar-buffers-file' row to
`edmacs-sidebar-buffers-visit' before `edmacs-sidebar-activate' is ever
reached. This is defensive coverage for `edmacs-sidebar-activate' itself:
a buffers-file row's value is a real buffer object -- neither `integerp'
nor `consp' -- and has nothing for it to act on, so it reports rather
than silently doing nothing. The real, RET-reachable report coverage
for an actionless buffer row lives in sidebar-buffers-test.el's
`edmacs-sidebar-buffers-test-visit-no-section-reports' and
`-visit-file-row-without-root-reports', against the real
`edmacs-sidebar-buffers-visit'."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t)
              (buf (get-buffer-create "edmacs-sidebar-test-activate-buf")))
          (unwind-protect
              (progn
                (magit-insert-section (edmacs-sidebar-root)
                  (magit-insert-section (edmacs-sidebar-buffers-file buf)
                    (magit-insert-heading "buffer row")))
                (goto-char (point-min))
                (should-error (edmacs-sidebar-activate) :type 'user-error))
            (kill-buffer buf)))))

    (ert-deftest edmacs-sidebar-test-activate-usage-row-reports ()
      "A usage-meter row's section value stays nil (no value form is
passed at the real insertion site in claude-usage.el, matching the
class's nil initform) -- `edmacs-sidebar-activate' now reports on it
via `user-error' instead of silently no-opping."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (edmacs-sidebar-root)
            (magit-insert-section (claude-usage-sidebar-meter)
              (insert "usage row\n"))))
        (goto-char (point-min))
        (should-error (edmacs-sidebar-activate) :type 'user-error)))

    (ert-deftest edmacs-sidebar-test-activate-no-section-reports ()
      "No section at all (an `edmacs-sidebar-mode' buffer with nothing ever
inserted into it) reports rather than silently doing nothing -- the
reachable form of the phase body's \"value slot unbound\" case, since a
real EIEIO `magit-section' instance always has its `value' slot bound
(to nil) via its `:initform'."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (should-error (edmacs-sidebar-activate) :type 'user-error)))

    ;; ==========================================================================
    ;; AC4 (worktree redraw path) -- no subprocess work through the
    ;; worktree-aware render/activate/close surface
    ;; ==========================================================================
    ;; `edmacs-sidebar-test-redraw-and-hooks-never-shell-out' above (the
    ;; phase-2 regression test) only ever drives an UNGROUPED frame -- it
    ;; opens no project group, so it exercises the flat-tab plan but never
    ;; `edmacs-sidebar--plan-projects', `edmacs-sidebar-activate' on an
    ;; already-open row, or `edmacs-sidebar-close-worktree'. This is
    ;; the direct regression test for THIS phase's own no-shellout claim,
    ;; covering both a populated cache and a cache miss.

    (ert-deftest edmacs-sidebar-test-redraw-projects-never-shell-out ()
      "The grouped-tree render/activate/close path -- including the
`edmacs-git-common-dir'-based classify-root/main-root derivation this
phase adds -- never shells out, PROVIDED every root it touches is
already cached (`edmacs-sidebar-test--with-project' pre-warms
`edmacs-git-common-dir-cache' for exactly that reason; see this file's
own Commentary on the workspaces fixtures above)."
      (edmacs-sidebar-test--with-project
          '(("repoG" "/repoG/main/" "/repoG/main/.git"
             ("/repoG/main/" "main") ("/repoG__worktrees/roadmap-z/" "roadmap-z")))
        (cl-letf (((symbol-function 'edmacs-workspaces-open-project) (lambda (_root) nil))
                  ;; `edmacs-workspaces-find-tab' stays real (it is the
                  ;; lookup this test's own classify-root/main-root claim
                  ;; is about), but a REAL `edmacs-workspaces-select-tab'
                  ;; would call the real `tab-bar-select-tab' and re-fire
                  ;; `edmacs-sidebar--on-tab-select''s nested redraw from
                  ;; inside this loop's own `with-current-buffer' --
                  ;; recorded instead, since all that matters here is that
                  ;; activating an already-open row never reaches a
                  ;; subprocess primitive.
                  ((symbol-function 'edmacs-workspaces-select-tab) (lambda (&rest _) nil))
                  ((symbol-function 'tab-bar-close-tab) (lambda (&optional _n) nil)))
          (edmacs-sidebar-test--with-frame ()
            (edmacs-sidebar-test--with-no-shellout
              (edmacs-sidebar-show (selected-frame))
              (dotimes (_ 50)
                (edmacs-sidebar--redraw (selected-frame))
                (edmacs-sidebar--render (edmacs-sidebar--plan (selected-frame)) 32)
                (edmacs-sidebar--on-tab-select nil nil)
                (edmacs-sidebar--on-tab-open nil)
                (edmacs-sidebar--on-tab-pre-close nil nil)
                (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                  ;; First (top-level) row: the project, main tab open.
                  (goto-char (point-min))
                  (edmacs-sidebar-activate)
                  (edmacs-sidebar-close-worktree)
                  ;; Its child row: "z", open too.
                  (forward-line 1)
                  (edmacs-sidebar-activate)
                  (edmacs-sidebar-close-worktree))))))))

    ;; ==========================================================================
    ;; AC2 -- per-frame buffers; delete-frame kills only that frame's buffer
    ;; ==========================================================================
    ;; `-Q --batch' generally cannot open a second real frame (no controlling
    ;; terminal to attach it to) -- an environment limitation, not a missing
    ;; optional package, but the resulting `ert-skip' follows the same
    ;; "second, independent thing this suite depends on but can't always
    ;; have" convention `claude-term-test--ensure-real-rotate' uses for
    ;; rotate.el.

    (ert-deftest edmacs-sidebar-test-singleton-buffer-shared-across-frames ()
      "Since edmacs-tab-groups phase 3's buffer collapse, two frames'
sidebars resolve to the `eq' SAME buffer object -- there is no more
per-frame `*sidebar: <repo>*' name to be distinct. Deleting the
NON-last frame showing it leaves the buffer alive for the remaining
frame; deleting the LAST frame showing it kills the buffer."
      (let* ((f1 (selected-frame))
             (f2 (edmacs-test-support-make-second-frame-or-skip)))
        (unwind-protect
            (progn
              (edmacs-sidebar-show f1)
              (with-selected-frame f2 (edmacs-sidebar-show f2))
              (let ((buf (edmacs-sidebar--buffer f1)))
                (should (buffer-live-p buf))
                (should (eq buf (edmacs-sidebar--buffer f2)))
                ;; RET on f2's window still selects the right tab on f2 --
                ;; exercises the same frame-explicit lookup as the
                ;; regression test above, against a genuinely different
                ;; frame, even though the buffer itself is shared.
                (with-selected-frame f2
                  (with-current-buffer buf
                    (goto-char (point-min))
                    (edmacs-sidebar-activate)))
                (should (= 0 (with-selected-frame f2 (tab-bar--current-tab-index))))
                ;; f1 still shows the buffer -- deleting f2 (not the last
                ;; frame showing it) must not kill it out from under f1.
                (delete-frame f2)
                (should (buffer-live-p buf))
                ;; f1 is this whole suite's shared frame and can't actually
                ;; be deleted -- hide its window (so no frame shows BUF any
                ;; more) and call the delete-frame hook directly, exactly
                ;; as `delete-frame' would invoke it on f1's own deletion.
                (edmacs-sidebar-hide f1)
                (edmacs-sidebar--cleanup-frame f1)
                (should-not (buffer-live-p buf))))
          (edmacs-sidebar-test--cleanup-sidebar f1)
          (when (frame-live-p f2) (delete-frame f2)))))

    ;; ==========================================================================
    ;; AC3 -- SPC T n/d/r wiring
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-post-open-shows-sidebar-in-new-tab ()
      (edmacs-sidebar-test--with-extra-tab
        (edmacs-sidebar-test--with-frame ()
          (edmacs-sidebar--on-tab-open nil)
          (should (edmacs-sidebar--window (selected-frame))))))

    (ert-deftest edmacs-sidebar-test-pre-close-redraw-removes-closed-tab-row ()
      (edmacs-sidebar-test--with-extra-tab
        (edmacs-sidebar-test--with-frame ()
          (edmacs-sidebar-show (selected-frame))
          (should (= 2 (length (tab-bar-tabs))))
          ;; Closes the current (newly-added) tab.
          (tab-bar-close-tab)
          ;; `sit-for' alone does not run pending (`run-at-time')
          ;; timers under `-Q --batch'; a real sleep is needed to let
          ;; the deferred `run-at-time 0' callback actually fire. That
          ;; callback now calls `edmacs-sidebar-invalidate', not
          ;; `--redraw' directly -- its own idle-0 timer never fires
          ;; under `sit-for' in `--batch' either, so flush it manually.
          (sleep-for 0.2)
          (sit-for 0)
          (edmacs-sidebar--flush-dirty-frames)
          (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
            (should (= 1 (length (tab-bar-tabs))))
            (should (= 1 (length (split-string (buffer-string) "\n" t))))))))

    (ert-deftest edmacs-sidebar-test-rename-advice-redraws ()
      "Short tab name deliberately: `edmacs-sidebar-max-width-fraction'
(AC4) can clamp the default sidebar width below what a long literal
test name needs to render untruncated -- this test's own concern is
that the rename advice triggers a redraw at all, not truncation, which
has its own dedicated coverage below."
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-show (selected-frame))
        (tab-bar-rename-tab "renamed-tab")
        ;; The rename advice now routes through `edmacs-sidebar-
        ;; invalidate' (deferred idle-0 redraw) rather than calling
        ;; `--redraw' directly -- flush it manually.
        (edmacs-sidebar--flush-dirty-frames)
        (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
          (should (string-match-p "renamed-tab" (buffer-string))))
        :cleanup
        (ignore-errors (tab-bar-rename-tab ""))))

    ;; ==========================================================================
    ;; AC4 -- window never selected/deleted; rotate-layout leaves it
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-window-parameters-block-other-window-and-c-x-1 ()
      "Behavioural half only: this asserts the parameters at creation time.
Whether they survive a `window-state-get'/`window-state-put' round trip --
what a daemon restart does to a background tab -- is windows.el's owned
set, covered by `edmacs-windows-test-layout-parameters-survive-a-state-
round-trip'."
      (edmacs-sidebar-test--with-frame ()
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
          (should (window-live-p win)))))

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
                        (edmacs-test-support-straight-build-root))))
         (when root
           (let ((path (expand-file-name "rotate/rotate.el" root)))
             (and (file-exists-p path) path))))
       (let ((root (edmacs-test-support-straight-repos-root)))
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
      (edmacs-sidebar-test--with-frame ()
        (let* ((win (edmacs-sidebar-show (selected-frame)))
               (width (window-width win)))
          (edmacs-sidebar-toggle (selected-frame))
          (should-not (edmacs-sidebar--window (selected-frame)))
          (edmacs-sidebar-toggle (selected-frame))
          (let ((win2 (edmacs-sidebar--window (selected-frame))))
            (should win2)
            (should (<= (abs (- (window-width win2) width)) 1))))))

    ;; ==========================================================================
    ;; AC6 -- top strip gone; SPC T l still works
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-tab-bar-show-nil-mode-untouched ()
      (should (null tab-bar-show))
      (should (fboundp 'tab-bar-switch-to-tab)))

    ;; The variable reading nil is not the same as the strip being gone: only
    ;; `tab-bar-show''s setter pushes `tab-bar-lines' to 0 on existing frames.
    (ert-deftest edmacs-sidebar-test-tab-bar-show-clears-existing-frame-lines ()
      (let* ((frame (selected-frame))
             (orig-lines (frame-parameter frame 'tab-bar-lines))
             (orig-show tab-bar-show))
        (unwind-protect
            (progn
              ;; init.el loads sessions.el -- which enables `tab-bar-mode' --
              ;; before sidebar.el, so the strip is already up on every live
              ;; frame by the time sidebar.el goes to hide it.
              (set-frame-parameter frame 'tab-bar-lines 1)
              ;; A bare `setq' is inert here -- this is the regression.
              (setq tab-bar-show nil)
              (should (equal 1 (frame-parameter frame 'tab-bar-lines)))
              ;; The setter's nil branch calls `tab-bar--update-tab-bar-lines'
              ;; with FORCE, which is what actually drops the strip.
              (customize-set-variable 'tab-bar-show nil)
              (should (equal 0 (frame-parameter frame 'tab-bar-lines))))
          (customize-set-variable 'tab-bar-show orig-show)
          (set-frame-parameter frame 'tab-bar-lines orig-lines))))

    ;; ... and sidebar.el must be the caller that goes through the setter.
    (ert-deftest edmacs-sidebar-test-tab-bar-show-hidden-via-setter ()
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name "modules/sidebar.el" default-directory))
        (goto-char (point-min))
        (should-not (re-search-forward "^(setq tab-bar-show" nil t))
        (goto-char (point-min))
        (should (re-search-forward "^(customize-set-variable 'tab-bar-show nil)"
                                   nil t))))

    (ert-deftest edmacs-sidebar-test-switch-to-tab-still-works ()
      (edmacs-sidebar-test--with-extra-tab
        (edmacs-sidebar-test--with-frame ()
          (let ((first-name (alist-get 'name (car (tab-bar-tabs)))))
            (tab-bar-switch-to-tab first-name)
            (should (= 0 (tab-bar--current-tab-index)))))))

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
        (edmacs-sidebar-test--with-frame ()
          (edmacs-sidebar-test--cleanup-sidebar (selected-frame))
          (should-not (edmacs-sidebar--buffer (selected-frame)))
          (edmacs-sidebar--on-desktop-read)
          (let ((buf (edmacs-sidebar--buffer (selected-frame))))
            (should (buffer-live-p buf))
            (with-current-buffer buf
              (should (= 2 (length (split-string (buffer-string) "\n" t)))))))))

    (ert-deftest edmacs-sidebar-test-ensure-buffer-is-always-the-singleton-name ()
      "Since edmacs-tab-groups phase 3's buffer collapse, there is no
per-frame title-derived name to drift out of sync any more (the race
`-ensure-buffer-renames-stale-buffer-name' used to cover) -- FRAME's
`name' parameter changing has no bearing on the buffer's own name at
all, which stays the fixed singleton `*sidebar*'."
      (edmacs-sidebar-test--with-frame ((original-name (frame-parameter (selected-frame) 'name)))
        (edmacs-sidebar--ensure-buffer frame)
        (should (equal (buffer-name (edmacs-sidebar--buffer frame)) "*sidebar*"))
        (set-frame-parameter frame 'name "real-repo-name")
        (edmacs-sidebar--ensure-buffer frame)
        (should (equal (buffer-name (edmacs-sidebar--buffer frame)) "*sidebar*"))
        :cleanup
        (set-frame-parameter frame 'name original-name)))

    (ert-deftest edmacs-sidebar-test-regenerate-after-frame-shows-sidebar-once-deferred ()
      "Direct regression test for the daemon-restart path's own function
\(`edmacs-sidebar--regenerate-after-frame', registered on
`after-make-frame-functions' at depth 100). Unlike sessions.el's own
frameset-restore hook, this one is not gated on `display-graphic-p' -- it
must show a fresh sidebar once its `run-at-time 0' fires, even on a
non-graphical batch frame."
      (edmacs-sidebar-test--with-frame ()
        (should-not (edmacs-sidebar--buffer (selected-frame)))
        (edmacs-sidebar--regenerate-after-frame (selected-frame))
        ;; Deferred -- must not have run synchronously.
        (should-not (edmacs-sidebar--buffer (selected-frame)))
        (sleep-for 0.2)
        (sit-for 0)
        (should (buffer-live-p (edmacs-sidebar--buffer (selected-frame))))))

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
      "The flat-tab render, every tab-bar hook, and every command
(J/K/gr/rename/help) drive zero subprocess calls -- the expectation is
'zero', not 'zero except N'. sidebar-agents.el's tmux-jump
`start-process' is the one documented exception in this codebase and is
never reached here: this loop fires no agent jump. A poisoned
`edmacs-sidebar-remembered-width' is set up front and `--show',
`--remember-width' and `--on-desktop-read' re-driven through the same
loop, so the width-clamp path is exercised under the same guarantee."
      (edmacs-sidebar-test--with-extra-tab
        (edmacs-sidebar-test--with-frame ()
          (edmacs-sidebar-test--with-no-shellout
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
                (edmacs-sidebar-redraw (selected-frame))
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
                  (edmacs-sidebar-rename-at-point)))))
          :cleanup
          (ignore-errors (tab-bar-rename-tab ""))
          (when (get-buffer "*Help*") (kill-buffer "*Help*"))
          (set-frame-parameter (selected-frame) 'edmacs-sidebar-remembered-width nil))))

    ;; ==========================================================================
    ;; Frameset restore
    ;; ==========================================================================
    (ert-deftest edmacs-sidebar-test-show-replaces-scratch-in-existing-side-window ()
      "Regression: a restored tab whose window-state names a dead sidebar
buffer leaves its side window showing some substitute buffer (a
placeholder such as `*scratch*' stands in for whatever
`window-state-put' actually leaves there). `edmacs-sidebar-show' must
reuse that window -- never open a second side window -- and end up
showing the frame's live sidebar buffer, never `*scratch*'."
      (edmacs-sidebar-test--with-frame
          ((placeholder-window
            (display-buffer (get-buffer-create "*scratch*")
                            '((display-buffer-in-side-window)
                              (side . left) (slot . 0) (window-width . 32)))))
        (should (window-live-p placeholder-window))
        (should (eq (window-buffer placeholder-window) (get-buffer "*scratch*")))
        (edmacs-sidebar-show frame)
        (let ((side-windows
               (seq-filter (lambda (w) (eq (window-parameter w 'window-side) 'left))
                           (window-list frame 'never))))
          (should (= 1 (length side-windows)))
          (should (eq (window-buffer (car side-windows))
                      (edmacs-sidebar--buffer frame)))
          (should-not (eq (window-buffer (car side-windows)) (get-buffer "*scratch*"))))))

    ;; ==========================================================================
    ;; Phase 8 -- J/K/r/gr/? bindings, RET user-errors, faces, resize, header-line
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-z-resolves-through-real-evil-keymap-to-toggle-collapse ()
      "`z' -- like RET/q/J/K/r/gr/?/TAB before it -- needs the same
dual-binding override: `evil-motion-state-map' claims `z' as a prefix
key (the `zz'/`zt' scrolling family), so only a real key-lookup check
proves this reaches `edmacs-sidebar-toggle-collapse' rather than
evil's own prefix map."
      (edmacs-sidebar-test--with-real-evil-motion
        (should (eq (key-binding (kbd "z")) #'edmacs-sidebar-toggle-collapse))))

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
      (edmacs-sidebar-test--with-real-evil-motion
        (should (eq (key-binding (kbd "J")) #'edmacs-sidebar-move-to-next-worktree))
        (should (eq (key-binding (kbd "K")) #'edmacs-sidebar-move-to-prev-worktree))
        (should (eq (key-binding (kbd "r")) #'edmacs-sidebar-rename-at-point))
        (should (eq (key-binding (kbd "g r")) #'edmacs-sidebar-redraw))
        (should (eq (key-binding (kbd "?")) #'edmacs-sidebar-help))
        (should (eq (key-binding (kbd "TAB")) #'edmacs-sidebar-toggle-at-point))))

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
          (edmacs-sidebar-toggle-at-point (selected-frame))
          (should (eq t (oref tab-section hidden)))
          (edmacs-sidebar-toggle-at-point (selected-frame))
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
          (edmacs-sidebar-toggle-at-point (selected-frame))
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
          (edmacs-sidebar-toggle-at-point (selected-frame))
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
            (edmacs-sidebar-test--with-frame ()
              (edmacs-sidebar-show (selected-frame))
              (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                (goto-char (point-min))
                (edmacs-sidebar-kill-at-point (selected-frame)))
              (should closed))))))

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
            (edmacs-sidebar-kill-at-point (selected-frame)))
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
`r' user-errors on a project row whose main tab isn't open, and with no
recognized row at all."
      (edmacs-sidebar-test--with-project
          '(("repoH" "/repoH/main/" "/repoH/main/.git"
             ("/repoH__worktrees/roadmap-wt/" "roadmap-wt")))
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) "edmacs-sidebar-test-renamed")))
          (edmacs-sidebar-test--with-frame ()
            (edmacs-sidebar-show (selected-frame))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              ;; First (top-level) row: the project, main tab not open.
              (goto-char (point-min))
              (should-error (edmacs-sidebar-rename-at-point) :type 'user-error)
              ;; Its child row: "wt", open.
              (forward-line 1)
              (edmacs-sidebar-rename-at-point)
              (should (equal "edmacs-sidebar-test-renamed"
                             (alist-get 'name (tab-bar--current-tab-find)))))
            :cleanup
            (ignore-errors (tab-bar-rename-tab "")))))
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
        (edmacs-sidebar-test--with-frame ()
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
                           (alist-get 'name (tab-bar--current-tab-find))))))))

    ;; ==========================================================================
    ;; A stamped worktree root that has been deleted from disk
    ;; ==========================================================================
    ;; The per-row replacement for the frames model's whole-frame "repo
    ;; missing" warning row (`edmacs-sidebar--insert-missing-repo-warning',
    ;; deleted with `edmacs-repo-missing'). How the marker and face reach a
    ;; ROW is a plan property, covered fixture-free by
    ;; `edmacs-sidebar-test-plan-marks-a-missing-root' and
    ;; `-plan-missing-root-outranks-current-tab-face'; what is left here is
    ;; the probe itself, driven against REAL directories through the real
    ;; `file-directory-p' default.

    (ert-deftest edmacs-sidebar-test-root-missing-p-probes-a-real-directory ()
      "The default probe is a real `file-directory-p' stat: a root that
exists on disk is live, the same root is missing once deleted, and a nil
root is never missing -- there is no stamp to be stale."
      (let ((alive (file-name-as-directory (make-temp-file "edmacs-sidebar-test-alive-" t)))
            (gone (file-name-as-directory (make-temp-file "edmacs-sidebar-test-gone-" t))))
        (unwind-protect
            (progn
              (should-not (edmacs-sidebar--root-missing-p alive))
              (should-not (edmacs-sidebar--root-missing-p gone))
              (delete-directory gone t)
              (should (edmacs-sidebar--root-missing-p gone))
              (should-not (edmacs-sidebar--root-missing-p nil)))
          (dolist (dir (list alive gone))
            (when (file-directory-p dir) (delete-directory dir t))))))

    (ert-deftest edmacs-sidebar-test-missing-worktree-probe-skips-remote-roots ()
      "A remote root is never probed: `file-directory-p' on one blocks on
the network, which the redisplay-path redraw cannot afford. The row
renders as an ordinary live one, and the probe function is not called."
      (let* ((called nil)
             (edmacs-sidebar-worktree-live-p-function
              (lambda (root) (push root called) nil)))
        (should-not (edmacs-sidebar--root-missing-p "/ssh:host:/srv/repo/"))
        (should-not called)
        ;; A local root of the same shape still goes through the probe.
        (should (edmacs-sidebar--root-missing-p "/srv/repo/"))
        (should (equal called '("/srv/repo/")))))

    (ert-deftest edmacs-sidebar-test-reapply-width-restores-a-resized-sidebar ()
      "`edmacs-sidebar-reapply-width' resizes a drifted sidebar back to target.
`SPC w =' reaches it through `edmacs-windows-rebalance-functions': a side
window keeps the absolute width it was created at, so a sidebar sized for
one display stays that width on the next one."
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-show frame)
        (let ((window (edmacs-sidebar--window frame)))
          (window-resize window -5 t)
          (should (/= (window-total-width window)
                      (edmacs-sidebar--target-width frame)))
          (edmacs-sidebar-reapply-width frame)
          (should (= (window-total-width window)
                     (edmacs-sidebar--target-width frame))))))

    (ert-deftest edmacs-sidebar-test-reapply-width-never-shows-a-hidden-sidebar ()
      "A frame with no sidebar window is left without one."
      (let ((frame (selected-frame)))
        (edmacs-sidebar-hide frame)
        (edmacs-sidebar-reapply-width frame)
        (should (null (edmacs-sidebar--window frame)))))

    (ert-deftest edmacs-sidebar-test-reapply-width-joins-the-rebalance-hook ()
      "The command in keybindings.el reaches this file through that hook."
      (should (memq #'edmacs-sidebar-reapply-width
                    edmacs-windows-rebalance-functions)))

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
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-show frame)
        (let ((window (edmacs-sidebar--window frame)))
          (window-resize window -5 t)
          (edmacs-sidebar--remember-width frame t))
        (let ((resized (window-width (edmacs-sidebar--window frame))))
          (should (/= resized edmacs-sidebar-width))
          (edmacs-sidebar-toggle (selected-frame))
          (edmacs-sidebar-toggle (selected-frame))
          (should (= resized (window-width (edmacs-sidebar--window frame)))))
        :cleanup
        (let ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
          (when (timerp timer) (cancel-timer timer)))
        (remhash frame edmacs-sidebar--resize-debounce-timers)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-on-window-size-change-debounces-and-stashes ()
      "`--on-window-size-change' schedules a debounced call to
`--remember-width', which stashes the CURRENT window's
`window-total-width' (see `--remember-width's docstring) once it
fires; a no-op for a frame with no live sidebar window shown. `this-command' is
bound to an allowlisted `edmacs-sidebar--interactive-resize-commands'
member throughout, modeling a genuine user-driven resize -- the
allowlist gate itself is covered by the two tests below."
      (edmacs-sidebar-test--with-frame ((edmacs-sidebar-resize-debounce-seconds 0.05)
                                        (this-command 'evil-window-decrease-width))
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
        (should (= (window-total-width (edmacs-sidebar--window frame))
                   (frame-parameter frame 'edmacs-sidebar-remembered-width)))
        :cleanup
        (let ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
          (when (timerp timer) (cancel-timer timer)))
        (remhash frame edmacs-sidebar--resize-debounce-timers)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    ;; ==========================================================================
    ;; edmacs-sidebar-polish -- only a deliberate, interactive resize is
    ;; remembered; a reset command clears an already-poisoned value (U4)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-on-window-size-change-refuses-non-interactive-resize ()
      "Reproduces the U4 poisoning path: the sidebar window ends up wide
for a reason that is not the user directly resizing it (here modeled
by widening it with a raw `window-resize', standing in for e.g.
another window's layout change pushing the sidebar wide), and
`--on-window-size-change' fires with a `this-command' that is not in
`edmacs-sidebar--interactive-resize-commands'. The width must not be
stashed, even after the debounce fires."
      (edmacs-sidebar-test--with-frame ((edmacs-sidebar-resize-debounce-seconds 0.05)
                                        (this-command 'tab-bar-new-tab))
        ;; Keep the test honest: the command used to model a non-deliberate
        ;; resize must not itself be on the allowlist.
        (should-not (memq this-command edmacs-sidebar--interactive-resize-commands))
        (edmacs-sidebar-show frame)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
        (window-resize (edmacs-sidebar--window frame) 10 t)
        (edmacs-sidebar--on-window-size-change frame)
        (let ((deadline (+ (float-time) 2)))
          (while (and (< (float-time) deadline)
                      (gethash frame edmacs-sidebar--resize-debounce-timers))
            (sit-for 0.1)))
        (should-not (frame-parameter frame 'edmacs-sidebar-remembered-width))
        :cleanup
        (let ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
          (when (timerp timer) (cancel-timer timer)))
        (remhash frame edmacs-sidebar--resize-debounce-timers)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-on-window-size-change-still-stashes-interactive-resize ()
      "Positive counterpart to the refusal test above: a `this-command'
that IS in `edmacs-sidebar--interactive-resize-commands' still gets its
width stashed once the debounce fires -- guards against the gate
becoming so broad it silently breaks genuine manual resizes."
      (edmacs-sidebar-test--with-frame ((edmacs-sidebar-resize-debounce-seconds 0.05)
                                        (this-command 'evil-window-increase-width))
        (edmacs-sidebar-show frame)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
        (window-resize (edmacs-sidebar--window frame) -3 t)
        (edmacs-sidebar--on-window-size-change frame)
        (let ((deadline (+ (float-time) 2)))
          (while (and (< (float-time) deadline)
                      (not (frame-parameter frame 'edmacs-sidebar-remembered-width)))
            (sit-for 0.1)))
        (should (= (window-total-width (edmacs-sidebar--window frame))
                   (frame-parameter frame 'edmacs-sidebar-remembered-width)))
        :cleanup
        (let ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
          (when (timerp timer) (cancel-timer timer)))
        (remhash frame edmacs-sidebar--resize-debounce-timers)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-reset-width-clears-parameter ()
      "`edmacs-sidebar-reset-width' clears an already-poisoned remembered
width and, since the sidebar is shown, immediately reflows the live
window back near `edmacs-sidebar-width' rather than waiting for the
next hide/show cycle."
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-show frame)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width
                             (+ edmacs-sidebar-width 50))
        (edmacs-sidebar-reset-width frame)
        (should-not (frame-parameter frame 'edmacs-sidebar-remembered-width))
        ;; `edmacs-sidebar-width', like `edmacs-sidebar-remembered-width',
        ;; is a TOTAL-width target: `edmacs-sidebar-show' hands it to
        ;; `edmacs-sidebar--enforce-width', which resizes the live
        ;; window's `window-total-width' to match exactly, regardless
        ;; of how many columns of that total the window's own chrome
        ;; (a vertical border here in batch; fringes/scroll bar on a
        ;; real GUI frame) then costs `window-body-width'.
        (should (= (edmacs-sidebar--clamp-width edmacs-sidebar-width frame)
                   (window-total-width (edmacs-sidebar--window frame))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-reset-width-when-not-shown-only-clears-parameter ()
      "Calling `edmacs-sidebar-reset-width' when the sidebar has no live
window on FRAME must not error trying to hide/show a nonexistent
window -- it only clears the frame parameter."
      (edmacs-sidebar-test--with-frame ()
        (should-not (edmacs-sidebar--window frame))
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width
                             (+ edmacs-sidebar-width 50))
        (edmacs-sidebar-reset-width frame)
        (should-not (frame-parameter frame 'edmacs-sidebar-remembered-width))
        (should-not (edmacs-sidebar--window frame))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

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

    ;; ==========================================================================
    ;; edmacs-sidebar-polish -- collapse to a strip (width branch, not hide)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-collapse-expand-round-trips-width ()
      "Collapsing narrows the live window to
`edmacs-sidebar--collapsed-width'; expanding restores exactly the
pre-collapse remembered width -- possible only because
`--remember-width' (below) refuses to stash anything while collapsed."
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-show frame)
        (let ((window (edmacs-sidebar--window frame)))
          (window-resize window -5 t)
          (edmacs-sidebar--remember-width frame t))
        (let ((pre-collapse (window-width (edmacs-sidebar--window frame))))
          (should (/= pre-collapse edmacs-sidebar--collapsed-width))
          (edmacs-sidebar-collapse frame)
          ;; `display-buffer-in-side-window' yields an actual window one
          ;; column narrower than requested on a re-ask, exactly like
          ;; `edmacs-sidebar-reset-width's own live-width assertion --
          ;; see `edmacs-sidebar--remember-width's docstring.
          (should (= edmacs-sidebar--collapsed-width
                     (window-width (edmacs-sidebar--window frame))))
          (edmacs-sidebar-expand frame)
          (should (= pre-collapse (window-width (edmacs-sidebar--window frame)))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)
        (let ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
          (when (timerp timer) (cancel-timer timer)))
        (remhash frame edmacs-sidebar--resize-debounce-timers)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-collapse-when-not-shown-only-sets-parameter ()
      "Collapsing a frame with no live sidebar window at all must not
error, and must still flag the frame so the next real
`edmacs-sidebar-show' (e.g. a tab-open hook) opens directly at the
collapsed width instead of full width followed by a flash-resize."
      (edmacs-sidebar-test--with-frame ()
        (should-not (edmacs-sidebar--window frame))
        (edmacs-sidebar-collapse frame)
        (should (frame-parameter frame 'edmacs-sidebar-collapsed))
        (should (= edmacs-sidebar--collapsed-width
                   (window-width (edmacs-sidebar--window frame))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-toggle-collapse-flips-both-ways ()
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-show frame)
        (should-not (frame-parameter frame 'edmacs-sidebar-collapsed))
        (edmacs-sidebar-toggle-collapse frame)
        (should (frame-parameter frame 'edmacs-sidebar-collapsed))
        (edmacs-sidebar-toggle-collapse frame)
        (should-not (frame-parameter frame 'edmacs-sidebar-collapsed))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-remember-width-noops-while-collapsed ()
      "A resize event firing while the sidebar is collapsed (the live
window is at `edmacs-sidebar--collapsed-width', not a value the user
chose) must not clobber the real remembered width stashed before the
collapse."
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-show frame)
        (let ((window (edmacs-sidebar--window frame)))
          (window-resize window -5 t)
          (edmacs-sidebar--remember-width frame t))
        (let ((remembered (frame-parameter frame 'edmacs-sidebar-remembered-width)))
          (edmacs-sidebar-collapse frame)
          (edmacs-sidebar--remember-width frame t)
          (should (equal remembered (frame-parameter frame 'edmacs-sidebar-remembered-width))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-fit-uses-string-width-not-length ()
      "A double-width glyph counts as two columns, so a label carrying one
truncates a column earlier than a same-length ASCII label at the same
WIDTH would -- `length' alone would never notice the difference."
      (let ((wide "字bcd")    ; string-width 5, length 4
            (ascii "wbcd"))   ; string-width 4, length 4
        (should (= 4 (length wide)))
        (should (= 5 (string-width wide)))
        (should (= 4 (length ascii)))
        (should (= 4 (string-width ascii)))
        ;; The ASCII label fits WIDTH 4 exactly and is untouched...
        (should (equal ascii (edmacs-sidebar--fit ascii 4)))
        ;; ...but the equal-length wide one overflows it and gets truncated.
        (should-not (equal wide (edmacs-sidebar--fit wide 4)))
        (should (<= (string-width (edmacs-sidebar--fit wide 4)) 4))))

    (ert-deftest edmacs-sidebar-test-collapse-expand-adds-no-new-timer ()
      "Collapsing/expanding calls only `set-frame-parameter',
`edmacs-sidebar-show', and `edmacs-sidebar--redraw' -- no new timer is
armed as a direct result; the pre-existing debounce table is
unmodified by this feature."
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-show frame)
        (let ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
          (when (timerp timer) (cancel-timer timer)))
        (remhash frame edmacs-sidebar--resize-debounce-timers)
        (edmacs-sidebar-collapse frame)
        (should-not (gethash frame edmacs-sidebar--resize-debounce-timers))
        (edmacs-sidebar-expand frame)
        (should-not (gethash frame edmacs-sidebar--resize-debounce-timers))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)
        (let ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
          (when (timerp timer) (cancel-timer timer)))
        (remhash frame edmacs-sidebar--resize-debounce-timers)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-toggle-at-point-expands-when-collapsed ()
      "TAB on a collapsed sidebar expands it instead of folding a section
-- there is nothing meaningful to fold in the collapsed strip's render."
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-collapse frame)
        (with-current-buffer (edmacs-sidebar--buffer frame)
          (edmacs-sidebar-toggle-at-point (selected-frame)))
        (should-not (frame-parameter frame 'edmacs-sidebar-collapsed))
        (should (> (window-width (edmacs-sidebar--window frame))
                   edmacs-sidebar--collapsed-width))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-remember-width-refuses-as-sole-window ()
      "Measuring the sidebar while it is the frame's only live window must
not stash that width -- e.g. mid-frameset-restore before other windows
exist. `window-list' is stubbed to report the sidebar as the frame's
only window rather than literally deleting every sibling: Emacs's own
side-window invariant (a frame keeps at least one main window whenever
a side window exists) makes that real layout unreachable by deletion,
so the guard is exercised by controlling exactly what it inspects."
      (edmacs-sidebar-test--with-frame ()
        (edmacs-sidebar-show frame)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
        (let ((sidebar-window (edmacs-sidebar--window frame)))
          (should (window-live-p sidebar-window))
          (cl-letf (((symbol-function 'window-list)
                     (lambda (&rest _) (list sidebar-window))))
            (edmacs-sidebar--remember-width frame t)))
        (should-not (frame-parameter frame 'edmacs-sidebar-remembered-width))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

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
              (edmacs-sidebar--remember-width frame t)
              (should-not (frame-parameter frame 'edmacs-sidebar-remembered-width)))
          (when (window-live-p split) (delete-window split))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (let ((b (edmacs-sidebar--buffer frame)))
            (when (buffer-live-p b) (kill-buffer b)))
          (set-frame-parameter frame 'edmacs-sidebar-buffer nil))))

    (ert-deftest edmacs-sidebar-test-remember-width-clamps-stash ()
      "A genuinely live side window measuring wider than the fraction cap
gets the CLAMPED value stashed, not the raw `window-total-width'."
      (edmacs-sidebar-test--with-frame ((fw (frame-width frame))
                                        (oversized (max 40 (- fw 10))))
        (let ((edmacs-sidebar-max-width-fraction 1.0)
              (edmacs-sidebar-width oversized))
          (edmacs-sidebar-show frame)
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (let* ((edmacs-sidebar-max-width-fraction 0.2)
                 (edmacs-sidebar--min-width 5)
                 (window (edmacs-sidebar--window frame))
                 (measured (window-total-width window))
                 (expected (edmacs-sidebar--clamp-width measured frame)))
            ;; The scenario is only meaningful if the live window is
            ;; actually wider than the shrunk cap.
            (should (> measured expected))
            (edmacs-sidebar--remember-width frame t)
            (should (= expected (frame-parameter frame 'edmacs-sidebar-remembered-width)))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-show-clamps-poisoned-remembered-width ()
      "A frame parameter already poisoned to (at or above) the frame's full
width still yields a clamped window from `edmacs-sidebar-show' -- the
reported bug of the sidebar coming back at ~50% of the frame."
      (edmacs-sidebar-test--with-frame ((fw (frame-width frame)))
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width (+ fw 50))
        (edmacs-sidebar-show frame)
        (let ((window (edmacs-sidebar--window frame)))
          (should (<= (window-width window)
                      (floor (* fw edmacs-sidebar-max-width-fraction)))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-desktop-restore-clamps-poisoned-width ()
      "`--on-desktop-read' (this file's documented stand-in for a real
`desktop-read' round trip under `-Q --batch') brings a frame carrying a
poisoned remembered-width back clamped, not full-frame-wide."
      (edmacs-sidebar-test--with-frame ((fw (frame-width frame)))
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width (* fw 2))
        (edmacs-sidebar--on-desktop-read)
        (let ((window (edmacs-sidebar--window frame)))
          (should (<= (window-width window)
                      (floor (* fw edmacs-sidebar-max-width-fraction)))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

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
      (edmacs-sidebar-test--with-frame
          ((before-buffers (mapcar #'window-buffer (window-list frame 'never)))
           (before-count (length before-buffers)))
        (let ((window-sides-slots (list 0 (nth 1 window-sides-slots)
                                        (nth 2 window-sides-slots)
                                        (nth 3 window-sides-slots))))
          (should (null (edmacs-sidebar-show frame)))
          (should-not (seq-find (lambda (w) (window-parameter w 'window-side))
                                (window-list frame 'never)))
          (should-not (seq-find #'window-dedicated-p (window-list frame 'never)))
          (should (= before-count (length (window-list frame 'never))))
          (should (equal before-buffers (mapcar #'window-buffer (window-list frame 'never)))))))

    (ert-deftest edmacs-sidebar-test-show-cleans-up-non-left-window-from-placement ()
      "Belt-and-suspenders branch: even if `display-buffer-in-side-window'
itself returned a live window that is NOT a left side window (stubbed
here via `cl-letf' to fabricate an ordinary split, independent of
whatever real side-window semantics the exhausted-slot test above
relies on), `edmacs-sidebar-show' must delete that window and return
nil rather than dedicating and keeping it."
      (edmacs-sidebar-test--with-frame
          ((before-buffers (mapcar #'window-buffer (window-list frame 'never)))
           (before-count (length before-buffers))
           (stub-window nil))
        (cl-letf (((symbol-function 'display-buffer-in-side-window)
                   (lambda (buffer _alist)
                     (setq stub-window (split-window (selected-window)))
                     (set-window-buffer stub-window buffer)
                     stub-window)))
          (should (null (edmacs-sidebar-show frame))))
        (should-not (window-live-p stub-window))
        (should (= before-count (length (window-list frame 'never))))
        (should (equal before-buffers (mapcar #'window-buffer (window-list frame 'never))))
        :cleanup
        (when (window-live-p stub-window) (delete-window stub-window))))

    (ert-deftest edmacs-sidebar-test-header-line-name-prefers-the-active-group ()
      "The header line names the ACTIVE PROJECT -- the current tab's own
tab-bar group, which is the repo name -- not a per-frame repo
parameter, and falls back to the frame's own sanitised name when the
frame carries no group at all. Pure: `--header-line-name' needs no
window, so this drives it directly instead of through
`edmacs-sidebar-show'."
      (edmacs-sidebar-test--with-plan-fixture
          '((:group "repo" :main-root "/repo/main/"
             :tabs (("main" "/repo/main/" main t))))
        (should (equal "repo" (edmacs-sidebar--header-line-name nil))))
      (edmacs-sidebar-test--with-plan-fixture
          '((:group nil :tabs (("main" nil nil t))))
        (cl-letf (((symbol-function 'frame-parameter)
                   (lambda (_frame _param) "edmacs-sidebar-test-boot-frame - Emacs")))
          (should (equal "edmacs-sidebar-test-boot-frame"
                         (edmacs-sidebar--header-line-name nil))))))

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
      (should (equal "◆" (edmacs-sidebar--glyph 'roadmap-worktree)))
      (should (equal "◇" (edmacs-sidebar--glyph 'task-worktree)))
      (should (equal "·" (edmacs-sidebar--glyph 'worktree)))
      (should (equal "?" (edmacs-sidebar--glyph 'some-unknown-kind))))

    (ert-deftest edmacs-sidebar-test-worktree-kind-glyph-key-maps-classification ()
      "AC3: a roadmap and a task worktree row map to distinct glyph keys,
distinct from each other and from an unclassified worktree's generic key."
      (should (eq 'roadmap-worktree (edmacs-sidebar--worktree-kind-glyph-key 'roadmap)))
      (should (eq 'task-worktree (edmacs-sidebar--worktree-kind-glyph-key 'task)))
      (should (eq 'worktree (edmacs-sidebar--worktree-kind-glyph-key nil)))
      (should (eq 'worktree (edmacs-sidebar--worktree-kind-glyph-key 'main))))

    (ert-deftest edmacs-sidebar-test-glyph-nerd-icons-precedence ()
      "A present `nerd-icons' wins over the plain fallback; a nerd-icons
call that ERRORS falls back to plain text rather than signalling; and
`edmacs-sidebar-force-text-glyphs' overrides a working nerd-icons
outright (the terminal-frame escape hatch)."
      (cl-letf (((symbol-function 'nerd-icons-octicon)
                 (lambda (name) (format "NERD-%s" name)))
                ((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons))))
        (should (equal "NERD-nf-oct-arrow_right" (edmacs-sidebar--glyph 'current-tab)))
        (should (equal "NERD-nf-oct-circle" (edmacs-sidebar--glyph 'open-tab)))
        (should (equal "NERD-nf-oct-git_branch" (edmacs-sidebar--glyph 'roadmap-worktree)))
        (should (equal "NERD-nf-oct-checklist" (edmacs-sidebar--glyph 'task-worktree)))
        (should (equal "NERD-nf-oct-file_directory" (edmacs-sidebar--glyph 'worktree)))
        (let ((edmacs-sidebar-force-text-glyphs t))
          (should (equal "●" (edmacs-sidebar--glyph 'current-tab)))))
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'fboundp) (lambda (f) (eq f 'nerd-icons-octicon)))
                ((symbol-function 'nerd-icons-octicon) (lambda (_name) (error "boom"))))
        (should (equal "●" (edmacs-sidebar--glyph 'current-tab)))))

    ;; ==========================================================================
    ;; Ellipsis truncation to the sidebar window's live width (phase 8, AC2/AC5)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-truncate-label-boundary ()
      "A label exactly as wide as WIDTH is left untouched; one character
over gets truncated with a trailing … so the result is still exactly
WIDTH columns wide; a width of 0 or less yields the empty string rather
than signalling. Pure: `edmacs-sidebar--truncate-label' takes its width
as an argument now, so this test names no frame and no window."
      (should (equal "0123456789" (edmacs-sidebar--truncate-label "0123456789" 10)))
      (should (equal "012345678…" (edmacs-sidebar--truncate-label "0123456789X" 10)))
      (should (= 10 (string-width (edmacs-sidebar--truncate-label "0123456789X" 10))))
      (should (equal "0123…" (edmacs-sidebar--truncate-label "0123456789" 5)))
      (should (equal "" (edmacs-sidebar--truncate-label "0123456789" 0)))
      (should (equal "" (edmacs-sidebar--truncate-label "0123456789" -3))))

    (ert-deftest edmacs-sidebar-test-render-width-live-window-then-clamped-fallback ()
      "`edmacs-sidebar--render-width' keys off the sidebar window's current
width once one is live -- a widened sidebar must not keep truncating to
the old `edmacs-sidebar-width' default. With no live window it falls
back to the frame's remembered width, then that default, each run
through `edmacs-sidebar--clamp-width' exactly like `edmacs-sidebar-show'
and `--remember-width' already do for the same frame parameter: without
that clamp a poisoned remembered width (the reported ~50%-of-frame bug)
renders a full, untruncated label on the very first pre-window redraw."
      (edmacs-sidebar-test--with-frame ()
        (should-not (edmacs-sidebar--window frame))
        (let ((edmacs-sidebar--min-width 1)
              (edmacs-sidebar-width 10))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
          (should (= 10 (edmacs-sidebar--render-width frame)))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width 5)
          (should (= 5 (edmacs-sidebar--render-width frame))))
        (let* ((edmacs-sidebar--min-width 5)
               (edmacs-sidebar-max-width-fraction 0.33)
               (clamped (edmacs-sidebar--clamp-width most-positive-fixnum frame)))
          (set-frame-parameter frame 'edmacs-sidebar-remembered-width most-positive-fixnum)
          (should (= clamped (edmacs-sidebar--render-width frame))))
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
        (edmacs-sidebar-show frame)
        (let ((window (edmacs-sidebar--window frame)))
          (should (window-live-p window))
          (should (= (window-width window) (edmacs-sidebar--render-width frame))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

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
        (set-window-parameter window 'mode-line-format 'none)
        (set-window-dedicated-p window t)
        window))

    (ert-deftest edmacs-sidebar-test-hide-on-sole-window-frame-does-not-signal ()
      "`edmacs-sidebar-hide' used to call `delete-window' unconditionally, so
the frame's sole window signalled. It now releases the window in place,
leaving the frame with a real main window rather than a wedged one."
      (edmacs-sidebar-test--with-frame ()
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
                                 no-other-window no-delete-other-windows
                                 mode-line-format))
              (should-not (window-parameter window parameter)))
            (should-not (edmacs-sidebar--window frame))
            (should-not (edmacs-windows-frame-wedged-p frame))
            (should (eq (edmacs-windows-designate-main frame) window))))))

    (ert-deftest edmacs-sidebar-test-hide-in-ordinary-window-keeps-the-window ()
      "`edmacs-sidebar--window' matches on buffer identity, so it also finds
the sidebar buffer in an ordinary window -- a window the sidebar does not
own and must not delete."
      (edmacs-sidebar-test--with-frame ()
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
            (should-not (edmacs-windows-frame-wedged-p frame))))))

    (ert-deftest edmacs-sidebar-test-hide-deletes-a-real-side-window ()
      "The normal case is unchanged: a side window with a parent is deleted."
      (edmacs-sidebar-test--with-frame ()
        (save-window-excursion
          (delete-other-windows)
          (let ((window (edmacs-sidebar-show frame)))
            (should (window-live-p window))
            (should (window-parent window))
            (should-not (edmacs-sidebar-hide frame))
            (should-not (window-live-p window))
            (should-not (edmacs-sidebar--window frame))
            (should-not (edmacs-windows-frame-wedged-p frame))))))

    (ert-deftest edmacs-sidebar-test-show-into-mainless-frame-yields-side-window-and-main ()
      "Without the repair, `display-buffer-in-side-window' just reuses the
existing slot-0 left window and the frame stays wedged. The unchanged
frame count is what proves repair rebuilt this frame rather than
escaping to a new one."
      (edmacs-sidebar-test--with-frame ()
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
                (should-not (window-parameter main 'window-side))))))))

    (ert-deftest edmacs-sidebar-test-show-returns-nil-when-the-left-slot-is-forbidden ()
      "With no left slot available `display-buffer-in-side-window' returns
nil, and `edmacs-sidebar-show' must return nil rather than fall through
to splitting the widest window -- which is what would put the sidebar on
the right of a wide frame. The frame keeps its main window either way."
      (edmacs-sidebar-test--with-frame ()
        (save-window-excursion
          (delete-other-windows)
          (edmacs-window-set-main (selected-window))
          (let ((window-sides-slots '(0 nil nil nil)))
            (should-not (edmacs-sidebar-show frame)))
          (should-not (edmacs-sidebar--side-window frame))
          (should (window-live-p (edmacs-main-window)))
          (should-not (edmacs-windows-frame-wedged-p frame)))))

    (ert-deftest edmacs-sidebar-test-hide-twice-is-idempotent ()
      "The second call finds no window at all -- `edmacs-sidebar--window'
matches on buffer identity and the buffer is gone from the frame -- so
it returns nil without signalling or re-wedging."
      (edmacs-sidebar-test--with-frame ()
        (save-window-excursion
          (edmacs-sidebar-test--make-sole-sidebar-window frame)
          (should (edmacs-sidebar-hide frame))
          (should-not (edmacs-sidebar-hide frame))
          (should-not (edmacs-windows-frame-wedged-p frame))
          (should (window-live-p (edmacs-windows-designate-main frame))))))

    (ert-deftest edmacs-sidebar-test-release-window-deletes-a-parented-side-window ()
      "The one shape `delete-window' is correct for."
      (edmacs-sidebar-test--with-frame ()
        (save-window-excursion
          (delete-other-windows)
          (let ((window (edmacs-sidebar-show frame)))
            (should (window-parent window))
            (should-not (edmacs-sidebar--release-window window frame))
            (should-not (window-live-p window))))))

    (ert-deftest edmacs-sidebar-test-release-window-releases-an-ordinary-window-in-place ()
      "A window the sidebar does not own is never deleted, only handed back."
      (edmacs-sidebar-test--with-frame ()
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
              (should-not (window-parameter other parameter)))))))

    (ert-deftest edmacs-sidebar-test-release-window-falls-back-to-scratch ()
      "When `other-buffer' can only offer the sidebar buffer back, the
released window must not simply re-show it."
      (edmacs-sidebar-test--with-frame ()
        (save-window-excursion
          (let* ((window (edmacs-sidebar-test--make-sole-sidebar-window frame))
                 (sidebar (window-buffer window)))
            (cl-letf (((symbol-function 'other-buffer)
                       (lambda (&rest _) sidebar)))
              (should (eq (edmacs-sidebar--release-window window frame) window)))
            (should (equal (buffer-name (window-buffer window)) "*scratch*"))))))

    (ert-deftest edmacs-sidebar-test-show-is-registered-on-the-repaired-hook ()
      "Repair hands the frame back a main window but no sidebar; this hook
membership is what puts one back."
      (should (memq #'edmacs-sidebar-show edmacs-windows-frame-repaired-functions)))

    (ert-deftest edmacs-sidebar-test-side-window-accessor-ignores-ordinary-windows ()
      (edmacs-sidebar-test--with-frame ()
        (save-window-excursion
          (delete-other-windows)
          (let ((other (split-window (selected-window) nil 'below)))
            (set-window-buffer other (edmacs-sidebar--ensure-buffer frame))
            (should (eq (edmacs-sidebar--window frame) other))
            (should-not (edmacs-sidebar--side-window frame))))))

    (ert-deftest edmacs-sidebar-test-buffer-remaps-every-surface-face ()
      "The sidebar reads as one surface, so `default', `fringe' AND
`header-line' are all remapped to `edmacs-sidebar-background-face'.
Remapping `default' alone leaves the side fringes and the top strip
painted in the frame's colour, framing the sidebar in the wrong shade --
they are separate faces, not `default' inheritors. The window has no
mode line (`mode-line-format' is `none'), so its bottom edge is ordinary
buffer area already covered by the `default' entry."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (dolist (face '(default fringe header-line))
          (should (equal (list face 'edmacs-sidebar-background-face)
                          (assq face face-remapping-alist))))))

    (ert-deftest edmacs-sidebar-test-face-remap-is-buffer-local-and-idempotent ()
      "The remap is buffer-local (never touching another buffer's faces) and
re-running the mode does not stack duplicate entries."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (edmacs-sidebar-mode)
        (should (= 1 (cl-count 'default face-remapping-alist :key #'car-safe)))
        (should (= 1 (cl-count 'fringe face-remapping-alist :key #'car-safe)))
        (should (local-variable-p 'face-remapping-alist)))
      ;; A fresh buffer is untouched.
      (with-temp-buffer
        (should-not (assq 'default face-remapping-alist))))

    (ert-deftest edmacs-sidebar-test-window-parameter-mode-line-format-is-none ()
      "The shown sidebar window's mode-line-format parameter is set to `none'
to prevent it from inheriting the default mode-line format and displaying
its raw buffer name."
      (edmacs-sidebar-test--with-frame ()
        (save-window-excursion
          (let ((window (edmacs-sidebar-show frame)))
            (should (window-live-p window))
            (should (eq (window-parameter window 'mode-line-format) 'none))))))

    (ert-deftest edmacs-sidebar-test-mode-line-format-clears-on-release ()
      "When the sidebar window is released (as in a sole-window frame),
the mode-line-format parameter set at sidebar creation is cleared,
preventing it from leaking onto the buffer that replaces the sidebar."
      (edmacs-sidebar-test--with-frame ()
        (save-window-excursion
          ;; Create a sole sidebar window manually with mode-line-format set
          (let ((window (edmacs-sidebar-test--make-sole-sidebar-window frame)))
            (should (edmacs-windows-frame-wedged-p frame))
            (should (eq (window-parameter window 'mode-line-format) 'none))
            ;; Hide/release the window, which should clear mode-line-format
            (let ((released (edmacs-sidebar-hide frame)))
              (should (window-live-p released))
              ;; Verify mode-line-format was cleared
              (should-not (window-parameter released 'mode-line-format)))))))

    ;; ==========================================================================
    ;; AC1 -- worktree-section-functions body-inserts inside the row's own
    ;; section, so a contributed section is a real child, not a sibling
    ;; ==========================================================================

    ;; ==========================================================================
    ;; AC2 -- visit/close/rename/toggle all resolve through one shared
    ;; enclosing-worktree parent walk, so they work from any nested row
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-close-worktree-from-nested-row ()
      "`d' with point on an agent leaf nested two levels under its
worktree row (tab-row > agents-group > agent) closes the ENCLOSING
worktree row's tab -- proof `edmacs-sidebar--enclosing-worktree' walks
past more than one level, unlike the old bespoke single-level walks."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let (agent-section)
          (let ((inhibit-read-only t))
            (magit-insert-section (edmacs-sidebar-root)
              (magit-insert-section (edmacs-sidebar-tab (cons "g1" "/repo/wt/"))
                (magit-insert-heading "tab row")
                (magit-insert-section (edmacs-sidebar-agents-group nil)
                  (magit-insert-heading "  agents group")
                  (setq agent-section
                        (magit-insert-section (edmacs-sidebar-agent "fake-agent")
                          (magit-insert-heading "    an agent row")))))))
          (goto-char (oref agent-section start))
          (should (eq (magit-current-section) agent-section))
          (let (closed)
            (cl-letf (((symbol-function 'tab-bar-close-tab) (lambda (n) (push n closed)))
                      ((symbol-function 'edmacs-workspaces-find-tab)
                       (lambda (_root) 'fake-tab))
                      ((symbol-function 'tab-bar--tab-index)
                       (lambda (_tab &optional _tabs _frame) 2)))
              (edmacs-sidebar-close-worktree))
            (should (equal closed '(3)))))))

    (ert-deftest edmacs-sidebar-test-rename-at-point-from-nested-buffer-row ()
      "`r' with point on a buffer-file leaf nested under a `buffers'
heading under its worktree row renames the ENCLOSING worktree row's
tab -- the generic (non-agent) branch of the shared walk."
      (with-temp-buffer
        (edmacs-sidebar-mode)
        (let (buf-section)
          (let ((inhibit-read-only t))
            (magit-insert-section (edmacs-sidebar-root)
              (magit-insert-section (edmacs-sidebar-tab (cons "g1" "/repo/wt/"))
                (magit-insert-heading "tab row")
                (magit-insert-section (edmacs-sidebar-buffers-root (cons "/repo/wt/" 2))
                  (magit-insert-heading "  buffers")
                  (setq buf-section
                        (magit-insert-section (edmacs-sidebar-buffers-file (current-buffer))
                          (magit-insert-heading "    a buffer row")))))))
          (goto-char (oref buf-section start))
          (should (eq (magit-current-section) buf-section))
          (let (renamed
                (tab-bar-tabs-function
                 (lambda () (list '((name . "first")) '((name . "old-name"))))))
            (cl-letf (((symbol-function 'tab-bar-rename-tab)
                       (lambda (name n) (push (cons name n) renamed)))
                      ((symbol-function 'read-from-minibuffer)
                       (lambda (&rest _) "edmacs-sidebar-test-nested-rename"))
                      ((symbol-function 'edmacs-workspaces-find-tab)
                       (lambda (_root) 'fake-tab))
                      ((symbol-function 'tab-bar--tab-index)
                       (lambda (_tab &optional _tabs _frame) 1)))
              (edmacs-sidebar-rename-at-point))
            (should (equal renamed '(("edmacs-sidebar-test-nested-rename" . 2))))))))

    ;; ==========================================================================
    ;; AC1 -- point survives a redraw that changes a row's label, via
    ;; magit-section-ident stability rather than any bespoke identity scheme
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-point-survives-redraw-through-label-change ()
      "Point starts on a project row that is NOT the frame's active group
\(hollow glyph, plain label\); that group's own main tab then becomes
the frame's current tab -- a differently shaped label \(filled glyph,
`edmacs-sidebar-current-tab-face'\) -- and a direct `--redraw' call
still resolves point back to that same project section afterward,
proven via `magit-section-ident' equality, rather than falling to
`point-min' or drifting onto the other project's row. The section value
is an `equal'-stable `(GROUP . ROOT)' cons -- keying identity on the
rendered label instead (as before this phase) would have missed this,
since the label itself is what changed."
      (edmacs-sidebar-test--with-project
          '(("repoL" "/repoL/main/" "/repoL/main/.git" ("/repoL/main/" "main"))
            ("repoM" "/repoM/main/" "/repoM/main/.git" ("/repoM/main/" "main")))
        (edmacs-sidebar-test--with-frame ()
          ;; This fixture leaves "repoM" current -- point-min is
          ;; "repoL"'s row, the inactive one.
          (edmacs-sidebar-show (selected-frame))
          (let (section-before)
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (goto-char (point-min))
              ;; `--capture-positions' reads each window's own
              ;; `window-point', which redisplay (never run under `-Q
              ;; --batch') would otherwise sync from the buffer's actual
              ;; point on its own.
              (set-window-point (edmacs-sidebar--window (selected-frame)) (point))
              (should (equal (oref (magit-current-section) value) (cons "repoL" "/repoL/main/")))
              (should (eq (get-text-property (point) 'face) nil))
              (setq section-before (magit-current-section)))
            ;; "repoL" is now the frame's active group -- a differently
            ;; shaped label, filled glyph, current-tab-face. Outside the
            ;; `with-current-buffer' above: the real `tab-bar-select-tab'
            ;; this calls changes the frame's own selected window/buffer
            ;; as a side effect, which would otherwise hijack "current
            ;; buffer" away from the sidebar buffer for the rest of that
            ;; form.
            (edmacs-workspaces-select-tab "/repoL/main/")
            (edmacs-sidebar--redraw (selected-frame))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (should (equal (oref (magit-current-section) value) (cons "repoL" "/repoL/main/")))
              (should (eq (get-text-property (point) 'face) 'edmacs-sidebar-current-tab-face))
              (should (equal (magit-section-ident (magit-current-section))
                             (magit-section-ident section-before))))))))

    ;; ==========================================================================
    ;; AC2 -- fold state survives a redraw, via magit-section's own
    ;; visibility cache keyed on the now-stable magit-section-ident
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-fold-state-survives-redraw-through-label-change ()
      "A project row's own child section, folded before a redraw that
changes the row's label (inactive to active group, same transition
AC1's test drives), is still folded afterward -- with no bespoke
fold-preservation code of this phase's own: `magit-section-cache-
visibility' defaults to t, so `magit-section-hide' already cached this
child's hidden state under its `magit-section-ident', and the freshly
recreated child gets that same ident (its own value is constant, and
its parent -- the project row -- now has the `equal'-stable `(GROUP
. ROOT)' ident AC1 relies on), so `magit-section-cached-visibility'
restores it as hidden without this phase adding anything beyond the
Step-1 data-shape fix."
      (edmacs-sidebar-test--with-project
          '(("repoL" "/repoL/main/" "/repoL/main/.git" ("/repoL/main/" "main"))
            ("repoM" "/repoM/main/" "/repoM/main/.git" ("/repoM/main/" "main")))
        (let ((edmacs-sidebar-worktree-section-functions
               (list (lambda (root _has-tab _frame _tab-number)
                       (when (equal root "/repoL/main/")
                         (magit-insert-section (edmacs-sidebar-test-child nil)
                           (magit-insert-heading "  test child")
                           (insert "  test child body\n")))))))
          (edmacs-sidebar-test--with-frame ()
            (edmacs-sidebar-show (selected-frame))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (let (child)
                (edmacs-sidebar--map-sections
                 magit-root-section
                 (lambda (s) (when (eq (oref s type) 'edmacs-sidebar-test-child)
                               (setq child s))))
                (should child)
                (should (eq nil (oref child hidden)))
                (magit-section-hide child)))
            ;; "repoL" is now the frame's active group -- a differently
            ;; shaped label, filled glyph, current-tab-face. Outside the
            ;; `with-current-buffer' above: the real `tab-bar-select-tab'
            ;; this calls changes the frame's own selected window/buffer
            ;; as a side effect, which would otherwise hijack "current
            ;; buffer" away from the sidebar buffer for the rest of that
            ;; form.
            (edmacs-workspaces-select-tab "/repoL/main/")
            (edmacs-sidebar--redraw (selected-frame))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (let (child)
                (edmacs-sidebar--map-sections
                 magit-root-section
                 (lambda (s) (when (eq (oref s type) 'edmacs-sidebar-test-child)
                               (setq child s))))
                (should child)
                (should (eq t (oref child hidden)))))))))

    ;; ==========================================================================
    ;; Window-start half of `edmacs-sidebar--capture-positions'/
    ;; `--restore-positions', untouched by the AC1/AC2 tests above (they only
    ;; ever assert on point and fold state)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-window-start-survives-redraw-through-shifted-content ()
      "Window-start, scrolled onto one worktree child row while point sits
on a later one, is restored to that same row (by section identity, not
the stale raw integer) after a redraw that inserts an extra line into an
EARLIER row -- shifting every later row's buffer position down.
`edmacs-sidebar--restore-positions' first tries `magit-section-equal' on
the old raw integer, which -- after the shift -- now lands on the wrong
row entirely, so a correct restore has to fall through to its
`magit-section-goto-successor--same' recovery branch instead of
silently keeping a wrong-looking-but-live window-start."
      (edmacs-sidebar-test--with-project
          (list (append (list "repoP" "/repoP/main/" "/repoP/main/.git" (list "/repoP/main/" "main"))
                        (cl-loop for i from 0 below 8
                                 collect (list (format "/repoP__worktrees/roadmap-%d/" i)
                                               (format "roadmap-%d" i)))))
        (edmacs-sidebar-test--with-frame ()
          (let ((window nil))
            (edmacs-sidebar-show (selected-frame))
            (setq window (edmacs-sidebar--window (selected-frame)))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              ;; Line 0: the project row. Lines 1-8: children 0-7.
              (goto-char (point-min))
              (forward-line 3)
              (should (equal (oref (magit-current-section) value)
                             (cons "repoP" "/repoP__worktrees/roadmap-2/")))
              (set-window-start window (point) t)
              (goto-char (point-min))
              (forward-line 7)
              (should (equal (oref (magit-current-section) value)
                             (cons "repoP" "/repoP__worktrees/roadmap-6/")))
              (set-window-point window (point)))
            ;; An extra line inside roadmap-0's own row shifts every row
            ;; below it down, so the old raw `window-start' integer no
            ;; longer names roadmap-2's row.
            (let ((edmacs-sidebar-worktree-section-functions
                   (list (lambda (root &rest _)
                           (when (string-suffix-p "roadmap-0/" root)
                             (insert "    shifted\n"))))))
              (edmacs-sidebar--redraw (selected-frame)))
            (with-selected-window window
              (should (equal (oref (magit-section-at (window-start)) value)
                             (cons "repoP" "/repoP__worktrees/roadmap-2/"))))))))

    ;; ==========================================================================
    ;; Sanitiser and collision prevention for repo-less frames
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-sanitise-frame-title-table ()
      "`--sanitise-frame-title' strips a trailing \" - Emacs\", strips
leading/trailing `*...*' earmuffs, collapses internal whitespace, and
answers \"\" for anything that leaves nothing usable -- nil included.
Table-driven: twelve near-identical one-assertion tests said the same
thing twelve times over."
      (dolist (row '(("*Minibuf-1* - Emacs" . "")
                     ("*foo* - Emacs" . "")
                     ("  *a* *b*  - Emacs" . "")
                     ("Foo - Emacs" . "Foo")
                     ("My Frame - Emacs" . "My Frame")
                     ("Foo" . "Foo")
                     ("" . "")
                     ("   " . "")
                     (nil . "")
                     ("My   Frame   Name" . "My Frame Name")
                     ("*buffer* My Frame" . "My Frame")
                     ("My Frame *buffer*" . "My Frame")))
        (should (equal (cdr row)
                       (ert-info ((format "%S" (car row)))
                         (edmacs-sidebar--sanitise-frame-title (car row)))))))

    ;; The two buffer-NAME-collision tests that used to live here
    ;; (`-buffer-name-collision-prevention',
    ;; `-permanent-name-collision-does-not-poison-quit-restore') no longer
    ;; have a premise: since edmacs-tab-groups phase 3's singleton-buffer
    ;; collapse there is no per-frame title-derived name to collide on at
    ;; all -- see `edmacs-sidebar-test-singleton-buffer-shared-across-frames'
    ;; and `edmacs-sidebar-test-ensure-buffer-is-always-the-singleton-name'
    ;; above for this phase's replacement coverage. A second frame's own
    ;; `quit-restore' health (the other half of the old permanent-collision
    ;; test) is unrelated to buffer naming and stays covered by
    ;; `edmacs-sidebar-test-show-into-mainless-frame-yields-side-window-and-main'
    ;; and the window-parameter tests nearby.

    ;; ==========================================================================
    ;; edmacs-sidebar-polish phase 13 -- bottom-anchor the usage section
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-anchor-plan-table ()
      "`edmacs-sidebar--anchor-plan' is pure arithmetic over (ABOVE
ANCHORED BODY-HEIGHT POINT-ABOVE-P): short content pads, an exact fit
no-ops, overflow scrolls, and POINT-ABOVE-P suppresses only the scroll,
never the pad. No window call, so nothing here restates a stub."
      (dolist (row '((2 2 10 nil (:pad 6))
                     (4 5 10 nil (:pad 1))
                     (2 1 3 nil nil)
                     (20 2 5 nil (:scroll-to 5))
                     (20 2 5 t nil)
                     (2 2 10 t (:pad 6))
                     (0 9 5 nil (:scroll-to 5))
                     (0 1 0 nil (:scroll-to 0))))
        (let ((args (butlast row))
              (expected (car (last row))))
          (should (equal expected
                         (ert-info ((format "%S" row))
                           (apply #'edmacs-sidebar--anchor-plan args)))))))

    (defmacro edmacs-sidebar-test--with-anchor-window (var &rest body)
      "Run BODY with VAR bound to a real, live split window showing a fresh
temp buffer, that buffer current; both cleaned up after. Real, so
`window-body-size' reports a height to measure against, not a stub."
      (declare (indent 1))
      `(let* ((buf (generate-new-buffer " *anchor-window-test*"))
              (,var (split-window (selected-window))))
         (unwind-protect
             (progn
               (set-window-buffer ,var buf)
               (with-current-buffer buf ,@body))
           (when (window-live-p ,var) (delete-window ,var))
           (when (buffer-live-p buf) (kill-buffer buf)))))

    (ert-deftest edmacs-sidebar-test-anchor-region-pads-short-content ()
      "The applier pads a short anchored region with blank lines so the
buffer ends flush with the real window's bottom edge, leaving the
anchored text itself untouched -- nothing at all on a second call,
which is exactly the repeated-reapply shape
`edmacs-sidebar--reapply-bottom-anchor' produces, and nothing at all
against a window that is not `window-live-p'."
      (edmacs-sidebar-test--with-anchor-window window
        (let ((height (window-body-size window)))
          (skip-unless (> height 4))
          (insert "row one\nrow two\n")
          (let ((region-start (point)))
            (insert "anchored one\nanchored two\n")
            ;; A dead window is a no-op -- the shape `--redraw' hits on the
            ;; very first paint, before any window exists at all.
            (let ((before (buffer-string)))
              (edmacs-sidebar--anchor-region-to-bottom nil region-start)
              (should (equal before (buffer-string))))
            (edmacs-sidebar--anchor-region-to-bottom window region-start)
            (should (string-suffix-p "anchored one\nanchored two\n" (buffer-string)))
            (should (string-prefix-p "row one\nrow two\n" (buffer-string)))
            (should (= height (count-screen-lines (point-min) (point-max) nil window)))
            (let ((padded (buffer-string))
                  (start (window-start window)))
              (edmacs-sidebar--anchor-region-to-bottom window region-start)
              (should (equal padded (buffer-string)))
              (should (= start (window-start window))))))))

    (ert-deftest edmacs-sidebar-test-anchor-region-scrolls-past-overflow ()
      "The applier forces `window-start' past overflowing content above the
anchored region, so the last screen lines of the buffer -- which
necessarily include the whole anchored region -- are what the real
window would show. Batch mode never runs redisplay, so this is asserted
via `window-start' and `count-screen-lines' rather than
`pos-visible-in-window-p'. But point wins in the window the user is
actually in: forcing the scroll there would drag the cursor out of the
project rows and into the Claude usage block, with every redraw dragging
it back, so `C-w h' landed in the usage section and `k' could not climb
out. A backgrounded window still gets the anchor -- only the selected
one yields."
      (edmacs-sidebar-test--with-anchor-window window
        (let ((height (window-body-size window)))
          (dotimes (i (+ height 10)) (insert (format "row %d\n" i)))
          (let ((region-start (point))
                (top (save-excursion (goto-char (point-min)) (forward-line 2) (point))))
            (insert "anchored one\nanchored two\n")
            (edmacs-sidebar--anchor-region-to-bottom window region-start)
            (should (<= (window-start window) region-start))
            (should (= height (count-screen-lines
                               (window-start window) (point-max) nil window)))
            ;; Same buffer, same overflow -- but now WINDOW is the selected
            ;; one and point is above the forced start, so nothing moves.
            (set-window-point window top)
            (with-selected-window window
              (edmacs-sidebar--anchor-region-to-bottom window region-start))
            (should (= top (window-point window)))))))

    (ert-deftest edmacs-sidebar-test-anchor-region-pulls-point-forward-on-unselected-frame ()
      "The overflow branch's force-scroll-and-pull-forward step is gated on
WINDOW being the process-wide `selected-window', never on WINDOW's own
frame being the selected frame. A window on a real, backgrounded frame
is not that window, so it must still get the forced `window-start' and
the point pulled forward to meet it. Asserts concrete state changes --
not merely `>=' between two values that can trivially agree at their
untouched defaults -- so a guard that wrongly widens to skip every
backgrounded frame's own selected window fails here rather than passing
vacuously."
      (let* ((f1 (selected-frame))
             (f2 (edmacs-test-support-make-second-frame-or-skip))
             buf window)
        (unwind-protect
            (progn
              ;; `make-frame' selects the frame it creates -- select F1 back
              ;; so F2's window below is genuinely NOT the selected window,
              ;; even though it is F2's own frame-selected window.
              (select-frame f1)
              (setq buf (generate-new-buffer " *anchor-unselected-frame-test*"))
              (setq window (frame-first-window f2))
              (set-window-buffer window buf)
              (with-current-buffer buf
                (dotimes (i (+ (window-body-size window) 10))
                  (insert (format "row %d\n" i)))
                (let ((region-start (point))
                      (start-before-call (window-start window)))
                  (insert "anchored one\nanchored two\n")
                  ;; Well above where the forced overflow `window-start' will land.
                  (set-window-point window (point-min))
                  (progn
                    (should (eq f1 (selected-frame)))
                    (should (eq window (frame-selected-window f2)))
                    (should-not (eq window (selected-window)))
                    (edmacs-sidebar--anchor-region-to-bottom window region-start))
                  ;; The force-scroll branch actually ran: `window-start'
                  ;; moved off its pre-call value, and point was pulled all
                  ;; the way forward to meet it rather than left behind.
                  (should-not (= (window-start window) start-before-call))
                  (should (= (window-point window) (window-start window))))))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (frame-live-p f2) (delete-frame f2)))))

    (ert-deftest edmacs-sidebar-test-bottom-anchor-section-visible-through-show ()
      "A registrant on `edmacs-sidebar-bottom-anchor-section-functions' is
padded down to the sidebar window's real bottom edge through
`edmacs-sidebar-show', not left floating just below the tab list with
dead space beneath it -- and stays visible, via a forced `window-start',
once it inserts more than the window can hold, instead of scrolling off
the bottom unseen. The window's height is measured, not stubbed, and
the overflow case is driven by inserting past that measured height."
      (edmacs-sidebar-test--with-frame ()
        (let ((edmacs-sidebar-bottom-anchor-section-functions
               (list (lambda (_frame) (insert "ZZBOTTOMMARKERZZ\n")))))
          (edmacs-sidebar-show frame)
          (let* ((window (edmacs-sidebar--window frame))
                 (height (window-body-size window)))
            (with-current-buffer (edmacs-sidebar--buffer frame)
              (should (string-suffix-p "ZZBOTTOMMARKERZZ\n" (buffer-string)))
              (should (= height (count-screen-lines
                                 (point-min) (point-max) nil window))))
            (let ((edmacs-sidebar-bottom-anchor-section-functions
                   (list (lambda (_frame)
                           (dotimes (i (+ height 10)) (insert (format "fill %d\n" i)))
                           (insert "ZZBOTTOMMARKERZZ\n")))))
              (edmacs-sidebar--redraw frame)
              (with-current-buffer (edmacs-sidebar--buffer frame)
                (should (string-suffix-p "ZZBOTTOMMARKERZZ\n" (buffer-string)))
                (should (= height (count-screen-lines
                                   (window-start window) (point-max) nil window)))))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-collapsed-bottom-anchor-section-visible-through-show ()
      "The collapsed strip's own bottom-anchor hook
\(`edmacs-sidebar-collapsed-bottom-anchor-section-functions'\) gets the
same padding and the same forced `window-start' as the expanded one --
AC4's \"yes, anchor there too\" decision. The collapsed branch nils the
header line rather than rendering one, and hands its section hooks the
window's real usable width."
      (edmacs-sidebar-test--with-frame ()
        (let* ((widths nil)
               (edmacs-sidebar-collapsed-section-functions
                (list (lambda (_frame width) (push width widths))))
               (edmacs-sidebar-collapsed-bottom-anchor-section-functions
                (list (lambda (_frame _width) (insert "ZZC\n")))))
          (set-frame-parameter frame 'edmacs-sidebar-collapsed t)
          (edmacs-sidebar-show frame)
          (let* ((window (edmacs-sidebar--window frame))
                 (height (window-body-size window)))
            ;; The strip hook is handed the window's real usable width,
            ;; not the `--collapsed-width' constant it was asked for.
            (should (equal (list (edmacs-sidebar--strip-width frame)) widths))
            (with-current-buffer (edmacs-sidebar--buffer frame)
              (should-not header-line-format)
              (should (string-suffix-p "ZZC\n" (buffer-string)))
              (should (= height (count-screen-lines
                                 (point-min) (point-max) nil window))))
            (let ((edmacs-sidebar-collapsed-bottom-anchor-section-functions
                   (list (lambda (_frame _width)
                           (dotimes (i (+ height 10)) (insert (format "f%d\n" i)))
                           (insert "ZZC\n")))))
              (edmacs-sidebar--redraw frame)
              (with-current-buffer (edmacs-sidebar--buffer frame)
                (should (string-suffix-p "ZZC\n" (buffer-string)))
                (should (= height (count-screen-lines
                                   (window-start window) (point-max) nil window)))))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-on-window-size-change-anchor-registered ()
      "`--on-window-size-change-anchor' is registered on
`window-size-change-functions' alongside the pre-existing
`--on-window-size-change', and is a no-op for any frame with no live
sidebar window, including one whose sidebar was never shown -- calling
neither `--reapply-bottom-anchor' nor `--redraw'."
      (should (memq #'edmacs-sidebar--on-window-size-change-anchor
                     window-size-change-functions))
      (edmacs-sidebar-test--with-frame ((reapplied nil) (redrawn nil))
        (edmacs-sidebar-test--cleanup-sidebar frame)
        (cl-letf (((symbol-function 'edmacs-sidebar--reapply-bottom-anchor)
                   (lambda (_frame) (setq reapplied t)))
                  ((symbol-function 'edmacs-sidebar--redraw)
                   (lambda (_frame) (setq redrawn t))))
          (edmacs-sidebar--on-window-size-change-anchor frame))
        (should-not reapplied)
        (should-not redrawn)
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-on-window-size-change-anchor-gates-on-own-geometry ()
      "Regression for the bug the phase context names: this hook fires for
ANY window's resize or buffer change anywhere on the frame -- e.g. every
window pushed onto windows.el's master-and-stack column used to redraw
the whole sidebar tree even though the sidebar window's own height never
moved. Comparing against `window-old-pixel-height'/
`window-old-body-pixel-height' narrows this to a real change in the
sidebar window's own geometry, and reapplies the bottom anchor
(`--reapply-bottom-anchor'), never a full `--redraw', when it does."
      (edmacs-sidebar-test--with-frame ((reapplied nil) (redrawn nil))
        (edmacs-sidebar-show frame)
        (let ((window (edmacs-sidebar--window frame)))
          ;; Unchanged geometry: neither function runs.
          (cl-letf (((symbol-function 'edmacs-sidebar--reapply-bottom-anchor)
                     (lambda (_frame) (setq reapplied t)))
                    ((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (_frame) (setq redrawn t)))
                    ((symbol-function 'window-old-pixel-height)
                     (lambda (&optional w) (window-pixel-height (or w window))))
                    ((symbol-function 'window-old-body-pixel-height)
                     (lambda (&optional w) (window-body-size (or w window) nil t))))
            (edmacs-sidebar--on-window-size-change-anchor frame))
          (should-not reapplied)
          (should-not redrawn)
          ;; A changed total height: reapplies the anchor, never redraws.
          (cl-letf (((symbol-function 'edmacs-sidebar--reapply-bottom-anchor)
                     (lambda (_frame) (setq reapplied t)))
                    ((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (_frame) (setq redrawn t)))
                    ((symbol-function 'window-old-pixel-height)
                     (lambda (&optional _w) 1)))
            (edmacs-sidebar--on-window-size-change-anchor frame))
          (should reapplied)
          (should-not redrawn))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-bottom-anchor-adds-no-new-timer ()
      "Showing a sidebar with a bottom-anchor registrant, and driving a
resize through `--on-window-size-change-anchor', arms no new timer --
mirrors `edmacs-sidebar-test-collapse-expand-adds-no-new-timer's own
timer-list snapshot pattern."
      (edmacs-sidebar-test--with-frame ((edmacs-sidebar-bottom-anchor-section-functions
                                         (list (lambda (_frame) (insert "ZZTIMERZZ\n")))))
        (let ((before (length (append timer-list timer-idle-list))))
          (edmacs-sidebar-show frame)
          (edmacs-sidebar--redraw frame)
          (edmacs-sidebar--on-window-size-change-anchor frame)
          (should (= before (length (append timer-list timer-idle-list)))))
        :cleanup
        (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)))

    (ert-deftest edmacs-sidebar-test-no-reference-to-claude-usage ()
      "sidebar.el never mentions claude-usage.el by name -- the
bottom-anchor hooks are a generic seam, exactly like the five
pre-existing ones; only claude-usage.el is allowed to know about the
hook variable names."
      (let ((source (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "modules/sidebar.el" default-directory))
                       (buffer-string))))
        (should-not (string-match-p "claude-usage" source))))

    ;; ==========================================================================
    ;; AC3 -- the hand-rolled point-identity family no longer exists
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-point-identity-family-retired ()
      "Retired internals are gone entirely, not merely unused:
`--point-identity'/`--goto-identity' (replaced by
`--capture-positions'/`--restore-positions'), `--find-worktree-section'
(zero callers once the cons-dispatch wrapper went), and the five row
inserters the planner/renderer split replaced."
      (should-not (fboundp 'edmacs-sidebar--point-identity))
      (should-not (fboundp 'edmacs-sidebar--goto-identity))
      (should-not (fboundp 'edmacs-sidebar--find-worktree-section))
      ;; The five row inserters the planner/renderer split replaced.
      (dolist (fn '(edmacs-sidebar--redraw-tabs edmacs-sidebar--redraw-projects
                    edmacs-sidebar--insert-tab-row edmacs-sidebar--insert-project-row
                    edmacs-sidebar--insert-worktree-child-row))
        (should-not (fboundp fn))))

    (ert-deftest edmacs-sidebar-test-show-refuses-an-unusable-frame ()
      "`edmacs-sidebar-show' must create no window on a frame this config may
not drive -- the daemon's initial tty placeholder above all. Under the
old per-frame `*sidebar: <repo>*' naming this cost nothing, since each
frame drew into its own buffer. With one shared `*sidebar*' buffer a
redraw for the placeholder -- which belongs to no project group --
overwrites the real frame's tree with an empty one, which is how a
three-project sidebar rendered as one stale row under an `F1' header."
      (let ((frame (selected-frame)))
        (cl-letf (((symbol-function 'edmacs-workspaces-frame-usable-p) (lambda (_f) nil)))
          (should-not (edmacs-sidebar-show frame))
          (should-not (seq-find
                       (lambda (w)
                         (string-prefix-p "*sidebar" (buffer-name (window-buffer w))))
                       (window-list frame 'never))))))

    (ert-deftest edmacs-sidebar-test-redraw-frames-excludes-unusable-frames ()
      "`edmacs-sidebar-redraw-frames' is the guard the redraw-all loops use.
`edmacs-sidebar-agents--redraw-all' and
`edmacs-sidebar-buffers--redraw-all' write the ONE shared `*sidebar*'
buffer once per frame they visit, so the last frame in the list is the
one left on screen. Looping `frame-list' directly therefore let the
daemon's tty placeholder -- last in `frame-list', in no project group --
be that last writer, which is how a live sidebar reverted to a stale row
under an `F1' header a moment after drawing correctly."
      (cl-letf (((symbol-function 'edmacs-workspaces-frame-usable-p)
                 (lambda (_f) nil)))
        (should (null (edmacs-sidebar-redraw-frames))))
      (cl-letf (((symbol-function 'edmacs-workspaces-frame-usable-p)
                 (lambda (_f) t)))
        (should (equal (edmacs-sidebar-redraw-frames) (frame-list)))))

    ;; ==========================================================================
    ;; edmacs-sidebar-invalidate -- coalesced redraw
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-test-invalidate-coalesces-a-burst-into-one-redraw ()
      "N invalidations of the same frame within one command loop -- standing
in for N stack pushes, each of which used to call `--redraw' directly --
schedule exactly one pending idle timer and redraw exactly once when it
runs, not once per invalidation."
      (let ((frame (selected-frame)) (redraw-count 0))
        (edmacs-sidebar-test--with-clean-redraw-queue
          (cl-letf (((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (_frame) (setq redraw-count (1+ redraw-count)))))
            (dotimes (_ 20) (edmacs-sidebar-invalidate frame))
            (should (= 0 redraw-count))
            (should (timerp edmacs-sidebar--redraw-timer))
            (should (equal (list frame) edmacs-sidebar--dirty-frames))
            ;; Manually invoking the flush function stands in for its own
            ;; idle timer's eventual real firing: idle timers never fire
            ;; under `sit-for' in `--batch' (there is no real idle
            ;; detection there -- confirmed against a
            ;; `run-with-idle-timer' that a `sit-for' loop never reaches),
            ;; unlike the plain `run-at-time' debounces elsewhere in this
            ;; file, which `sleep-for'+`sit-for' do flush for real.
            (edmacs-sidebar--flush-dirty-frames)
            (should (= 1 redraw-count))
            (should-not edmacs-sidebar--dirty-frames)
            (should-not edmacs-sidebar--redraw-timer)))))

    (ert-deftest edmacs-sidebar-test-invalidate-noop-for-unusable-frame ()
      "`edmacs-sidebar-invalidate' marks nothing dirty and schedules no timer
at all for a frame `edmacs-sidebar-redraw-frames' excludes -- checked at
invalidation time, not merely skipped later at flush time, so an
unusable frame (the daemon's tty placeholder above all) never causes
timer churn either."
      (edmacs-sidebar-test--with-clean-redraw-queue
        (cl-letf (((symbol-function 'edmacs-workspaces-frame-usable-p) (lambda (_f) nil)))
          (edmacs-sidebar-invalidate (selected-frame)))
        (should-not edmacs-sidebar--dirty-frames)
        (should-not edmacs-sidebar--redraw-timer)))

    (ert-deftest edmacs-sidebar-test-flush-dirty-frames-skips-dead-frames ()
      "A frame deleted between invalidation and the flush is skipped, not
redrawn -- every other still-live dirty frame is still redrawn."
      (let* ((frame (selected-frame))
             (real-frame-live-p (symbol-function 'frame-live-p))
             (redrawn nil))
        (edmacs-sidebar-test--with-clean-redraw-queue
          (setq edmacs-sidebar--dirty-frames
                (list 'edmacs-sidebar-test--dead-frame frame))
          (cl-letf (((symbol-function 'frame-live-p)
                     (lambda (f) (if (eq f 'edmacs-sidebar-test--dead-frame) nil
                                   (funcall real-frame-live-p f))))
                    ((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (f) (push f redrawn))))
            (edmacs-sidebar--flush-dirty-frames))
          (should (equal redrawn (list frame))))))

    (ert-deftest edmacs-sidebar-test-tab-group-change-invalidates-without-buffer-list-event ()
      "A tab-bar group change alone -- via
`tab-bar-tab-post-change-group-functions', which core's
`tab-bar-change-tab-group' runs (an interactive `M-x tab-group'; this
config itself no longer writes a tab's `group') -- invalidates the
selected frame's sidebar with no buffer-list activity involved at all,
matching the phase context's own \"no trigger exists for
`tab-bar-change-tab-group'\" bug."
      (let ((frame (selected-frame)) (redraw-count 0))
        (edmacs-sidebar-test--with-clean-redraw-queue
          (cl-letf (((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (_frame) (setq redraw-count (1+ redraw-count)))))
            (run-hook-with-args 'tab-bar-tab-post-change-group-functions
                                (tab-bar--current-tab-find nil frame))
            (should (equal (list frame) edmacs-sidebar--dirty-frames))
            (edmacs-sidebar--flush-dirty-frames)
            (should (= 1 redraw-count))))))

    (ert-deftest edmacs-sidebar-test-tab-root-set-invalidates ()
      "`edmacs-workspaces-set-tab-root' has no sidebar.el of its own to call
directly (workspaces.el loads first) -- it runs
`edmacs-workspaces-tab-root-set-functions' instead, and sidebar.el's own
`--on-tab-root-set' member is what actually invalidates."
      (should (memq #'edmacs-sidebar--on-tab-root-set
                     edmacs-workspaces-tab-root-set-functions))
      (let ((frame (selected-frame)))
        (edmacs-sidebar-test--with-clean-redraw-queue
          (edmacs-sidebar--on-tab-root-set "/some/root/" frame)
          (should (equal (list frame) edmacs-sidebar--dirty-frames)))))

    (ert-deftest edmacs-sidebar-test-tab-select-pre-close-rename-honour-redraw-frames ()
      "Regression for the bug the phase context names: `edmacs-sidebar-
redraw-frames' is honoured by the two `--redraw-all's but was not by
tab-select, pre-close or rename before they routed through
`edmacs-sidebar-invalidate', which checks it uniformly."
      (edmacs-sidebar-test--with-clean-redraw-queue
        (cl-letf (((symbol-function 'edmacs-workspaces-frame-usable-p) (lambda (_f) nil)))
          (edmacs-sidebar--on-tab-select nil nil)
          (should-not edmacs-sidebar--dirty-frames)
          (edmacs-sidebar--after-tab-rename)
          (should-not edmacs-sidebar--dirty-frames))))

    (ert-deftest edmacs-sidebar-test-double-load-leaves-one-advice ()
      "Every `advice-add' in sidebar.el names a symbol, so re-evaluating the
file replaces rather than stacks. An anonymous lambda cannot be
`advice-remove'd and grows a new layer on every `eval-buffer' -- which is
exactly what `tab-bar-rename-tab' carried before this."
      (let ((path (expand-file-name "modules/sidebar.el" default-directory)))
        (should (file-readable-p path))
        (let ((window-sides-slots window-sides-slots)
              (tab-bar-show tab-bar-show))
          (load path nil t)))
      (let ((n 0))
        (advice-mapc (lambda (f _props)
                       (when (eq f #'edmacs-sidebar--after-tab-rename)
                         (setq n (1+ n))))
                     'tab-bar-rename-tab)
        (should (= n 1)))
      (should (= 1 (seq-count (lambda (f) (eq f #'edmacs-sidebar-show))
                               edmacs-windows-frame-repaired-functions))))

    (ert-deftest edmacs-sidebar-test-visibility-hook-runs-on-show-and-hide ()
      "The seam sidebar-agents.el reacts on, in place of its old advice on
`edmacs-sidebar-hide'. Runs on every return path of both functions,
including a show that produced no window."
      (edmacs-sidebar-test--with-frame ((seen nil)
                                        (edmacs-sidebar-visibility-functions
                                         (list (lambda (f state) (push (cons f state) seen)))))
        (save-window-excursion
          (delete-other-windows)
          (edmacs-sidebar-show frame)
          (should (equal (car seen) (cons frame 'shown)))
          (edmacs-sidebar-hide frame)
          (should (equal (car seen) (cons frame 'hidden)))
          ;; A hide with nothing to hide still reports.
          (edmacs-sidebar-hide frame)
          (should (equal (car seen) (cons frame 'hidden)))
          (should (= 3 (length seen))))))

    )) ; end of build-root-found branch

;;; sidebar-test.el ends here
