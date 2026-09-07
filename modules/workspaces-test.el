;;; workspaces-test.el --- Tests for workspaces.el -*- lexical-binding: t -*-

;;; Commentary:
;; Covers workspaces.el's whole public surface. Runs entirely under plain
;; `-Q --batch': no pty is needed, because a batch Emacs process already
;; owns one live frame (its initial "F1") on which real `tab-bar-new-tab',
;; `tab-bar-select-tab' and `tab-bar-change-tab-group' calls work fine --
;; confirmed experimentally, unlike the handful of other suites in this
;; repo that genuinely need a second frame and therefore a controlling
;; terminal (see CLAUDE.md's "A second real frame needs a pty" section;
;; this suite only ever uses the one frame batch already has).
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/workspaces.el -l modules/workspaces-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'tab-bar)

;; None of the stubs below target a subr (all are plain Lisp `defun's in
;; git-common-dir.el, vc-git.el, or tab-bar.el), so no native-comp subr
;; trampoline is ever built here -- this guard is defensive, matching
;; every sibling `-test.el' file in this repo (see CLAUDE.md).
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defmacro edmacs-workspaces-test--with-scratch-tabs (&rest body)
  "Run BODY against a freshly reset tab list on the selected frame.
Snapshots and restores the frame's `tabs' parameter around BODY, so one
test's real tab-bar mutations (`tab-bar-new-tab', `tab-bar-select-tab',
`tab-bar-change-tab-group', ...) never leak into the next. Resetting the
parameter to nil and calling `tab-bar-tabs' once forces a single fresh
default tab, per `tab-bar-tabs' own \"create default tabs\" branch."
  (declare (indent 0))
  `(let ((saved (frame-parameter nil 'tabs)))
     (unwind-protect
         (progn
           (set-frame-parameter nil 'tabs nil)
           (tab-bar-tabs)
           ,@body)
       (set-frame-parameter nil 'tabs saved))))

;; ============================================================================
;; edmacs-workspaces-group-name
;; ============================================================================

(ert-deftest edmacs-workspaces-test-group-name-derives-via-git-common-dir ()
  "Derivation goes through `edmacs-git-common-dir'/-repo-name, not ROOT's own path."
  (cl-letf (((symbol-function 'edmacs-git-common-dir)
             (lambda (root) (should (equal root "/repo/wt/")) "/repo/.git"))
            ((symbol-function 'edmacs-git-common-dir-repo-name)
             (lambda (common) (should (equal common "/repo/.git")) "repo")))
    (should (equal (edmacs-workspaces-group-name "/repo/wt/") "repo"))))

(ert-deftest edmacs-workspaces-test-group-name-nil-when-unresolvable ()
  "A ROOT git-common-dir cannot resolve at all yields nil, not an error."
  (cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_) nil)))
    (should-not (edmacs-workspaces-group-name "/not/a/repo/"))))

;; ============================================================================
;; edmacs-workspaces-classify-root
;; ============================================================================

(defmacro edmacs-workspaces-test--with-main (main &rest body)
  "Stub git-common-dir resolution so the repo's main worktree is MAIN."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_) "/repo/.git"))
             ((symbol-function 'edmacs-git-common-dir-main-worktree)
              (lambda (_) ,main)))
     ,@body))

(ert-deftest edmacs-workspaces-test-classify-root-roadmap ()
  (edmacs-workspaces-test--with-main "/Users/edward/Projects/edmacs/"
    (should (eq (edmacs-workspaces-classify-root
                 "/Users/edward/Projects/edmacs__worktrees/roadmap-edmacs-claude-tools")
                'roadmap))))

(ert-deftest edmacs-workspaces-test-classify-root-task ()
  (edmacs-workspaces-test--with-main "/Users/edward/Projects/rdm/"
    (should (eq (edmacs-workspaces-classify-root
                 "/Users/edward/Projects/rdm__worktrees/task-stop-hook-review-misfires")
                'task))))

(ert-deftest edmacs-workspaces-test-classify-root-main ()
  (edmacs-workspaces-test--with-main "/Users/edward/Projects/edmacs/"
    (should (eq (edmacs-workspaces-classify-root "/Users/edward/Projects/edmacs")
                'main))))

(ert-deftest edmacs-workspaces-test-classify-root-main-ignores-trailing-slash ()
  "A truename comparison must not be defeated by a bare trailing-slash mismatch."
  (edmacs-workspaces-test--with-main "/Users/edward/Projects/edmacs"
    (should (eq (edmacs-workspaces-classify-root "/Users/edward/Projects/edmacs/")
                'main))))

(ert-deftest edmacs-workspaces-test-classify-root-unclassified-fallback ()
  "A linked worktree outside any `__worktrees' directory classifies as nil."
  (edmacs-workspaces-test--with-main "/Users/edward/Projects/edmacs/"
    (should-not (edmacs-workspaces-classify-root "/Users/edward/elsewhere/my-checkout"))))

(ert-deftest edmacs-workspaces-test-classify-root-prefix-without-worktrees-parent ()
  "A `roadmap-'-prefixed leaf NOT under a `__worktrees' parent must not misclassify."
  (edmacs-workspaces-test--with-main "/Users/edward/Projects/edmacs/"
    (should-not (edmacs-workspaces-classify-root "/Users/edward/notes/roadmap-notes"))))

(ert-deftest edmacs-workspaces-test-classify-root-nil-when-git-common-dir-nil ()
  "An unresolvable ROOT (not a repo, pruned worktree) falls through to nil
rather than erroring."
  (cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_) nil)))
    (should-not (edmacs-workspaces-classify-root "/not/a/repo/at/all"))))

;; ============================================================================
;; Tab root: write / read / survives a tab switch
;; ============================================================================

(ert-deftest edmacs-workspaces-test-tab-root-write-read-survives-switch ()
  ;; The post-open stamper is deliberately out of the way here: this test
  ;; covers the pure write/read pair, and a stamped tab-b would say nothing
  ;; about it. The stamper's own behaviour is
  ;; `edmacs-workspaces-test-plain-new-tab-is-stamped-once' below.
  (let ((tab-bar-tab-post-open-functions nil))
   (edmacs-workspaces-test--with-scratch-tabs
    (let ((tab-a (tab-bar--current-tab-find nil (selected-frame))))
      (edmacs-workspaces-set-tab-root "/root/a/")
      (should (equal (edmacs-workspaces-tab-root tab-a) "/root/a/"))
      (tab-bar-new-tab)
      (let ((tab-b (tab-bar--current-tab-find nil (selected-frame))))
        (should-not (eq tab-a tab-b))
        (should-not (edmacs-workspaces-tab-root tab-b))
        (should (equal (edmacs-workspaces-tab-root
                        (nth 0 (tab-bar-tabs)))
                       "/root/a/")))))))

(ert-deftest edmacs-workspaces-test-set-tab-root-replaces-not-pushes ()
  "Re-stamping must REPLACE the parameter, never leave a shadowed stale cons."
  (edmacs-workspaces-test--with-scratch-tabs
    (edmacs-workspaces-set-tab-root "/root/first/")
    (edmacs-workspaces-set-tab-root "/root/second/")
    (let ((tab (tab-bar--current-tab-find nil (selected-frame))))
      (should (equal (edmacs-workspaces-tab-root tab) "/root/second/"))
      (should (= 1 (length (seq-filter
                            (lambda (kv) (eq (car kv) edmacs-workspaces-root-parameter))
                            (cdr tab))))))))

;; ============================================================================
;; Worktree enumeration: no cache
;; ============================================================================

(ert-deftest edmacs-workspaces-test-worktrees-shells-out-every-call ()
  "Two calls with the same ROOT must invoke the underlying git query twice."
  (let ((calls 0))
    (cl-letf (((symbol-function 'vc-git-root) (lambda (_) "/repo/main/.git"))
              ((symbol-function 'vc-git-known-other-working-trees)
               (lambda () (setq calls (1+ calls)) '("/repo/other/"))))
      (edmacs-workspaces-worktrees "/repo/main/")
      (edmacs-workspaces-worktrees "/repo/main/")
      (should (= calls 2)))))

(ert-deftest edmacs-workspaces-test-worktrees-includes-root-itself ()
  "ROOT's own worktree is consed back on, since the underlying git query excludes it."
  (cl-letf (((symbol-function 'vc-git-root) (lambda (_) "/repo/main/.git"))
            ((symbol-function 'vc-git-known-other-working-trees)
             (lambda () nil)))
    (let ((result (edmacs-workspaces-worktrees "/repo/main/")))
      (should (member (file-truename (file-name-as-directory "/repo/main/")) result)))))

(ert-deftest edmacs-workspaces-test-worktrees-nonexistent-root-no-signal ()
  "A nonexistent ROOT must fall through to just ROOT itself, never signal.
Exercises the real `vc-git-root'/`vc-git-known-other-working-trees'
interaction (no stub) -- `vc-git-known-other-working-trees' signals
`wrong-type-argument' on a ROOT that doesn't resolve to a git repo
unless this function guards against it first."
  (should (equal (edmacs-workspaces-worktrees "/no/such/dir/")
                 (list (file-truename (file-name-as-directory "/no/such/dir/"))))))

(ert-deftest edmacs-workspaces-test-worktrees-non-repo-root-no-signal ()
  "A real but non-git directory must also fall through rather than signal."
  (should (equal (edmacs-workspaces-worktrees "/tmp/")
                 (list (file-truename (file-name-as-directory "/tmp/"))))))

(ert-deftest edmacs-workspaces-test-source-has-no-cache-watch-or-timer ()
  "The AC's own \"asserted, not just intended\": grep the module's own text."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   ;; The documented invocation always runs from the repo
                   ;; root, so this module's known relative path resolves
                   ;; regardless of how `-l' set (or didn't set)
                   ;; `load-file-name' for this file.
                   (expand-file-name "modules/workspaces.el" default-directory))
                  (buffer-string))))
    ;; API calls, not the bare word: the Commentary above legitimately
    ;; discusses "file-notify" and "cache" in prose explaining their
    ;; deliberate absence, so the assertion targets the actual calls
    ;; that would install one, not that substring. `run-at-time' is
    ;; absent from the list on purpose -- the stray sweep's zero-delay
    ;; hop off the redisplay path schedules nothing recurring, and is
    ;; not the polling/debounce machinery this assertion exists to keep
    ;; out.
    (dolist (forbidden '("file-notify-add-watch" "file-notify-rm-watch"
                         "run-with-timer" "run-with-idle-timer"))
      (should-not (string-match-p (regexp-quote forbidden) source)))
    (should-not (string-match-p "(defvar [^\n]*cache" source))))

;; ============================================================================
;; Group assignment: through `tab-bar-change-tab-group', adjacency
;; ============================================================================

(ert-deftest edmacs-workspaces-test-assign-group-calls-tab-bar-change-tab-group ()
  (edmacs-workspaces-test--with-scratch-tabs
    (let ((calls '()))
      (cl-letf* ((real (symbol-function 'tab-bar-change-tab-group))
                 ((symbol-function 'tab-bar-change-tab-group)
                  (lambda (name &optional n)
                    (push (list name n) calls)
                    (funcall real name n))))
        (edmacs-workspaces-assign-group "proj-a")
        (should (equal calls '(("proj-a" nil))))))))

(ert-deftest edmacs-workspaces-test-assign-group-adjacency ()
  "Grouping the 1st and 3rd of three tabs ends with them adjacent."
  (edmacs-workspaces-test--with-scratch-tabs
    (tab-bar-rename-tab "t1")
    (tab-bar-new-tab)
    (tab-bar-rename-tab "t2")
    (tab-bar-new-tab)
    (tab-bar-rename-tab "t3")
    ;; t1 is now tab 1, t2 tab 2, t3 (current) tab 3.
    (edmacs-workspaces-assign-group "proj-a" 1)
    (edmacs-workspaces-assign-group "proj-a" (1+ (tab-bar--current-tab-index)))
    (let* ((names (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
           (t1-pos (seq-position names "t1"))
           (t3-pos (seq-position names "t3")))
      (should (= 1 (abs (- t1-pos t3-pos)))))))

;; ============================================================================
;; find-tab / select-tab across two (group, root) pairs
;; ============================================================================

(ert-deftest edmacs-workspaces-test-find-and-select-tab ()
  (edmacs-workspaces-test--with-scratch-tabs
    (edmacs-workspaces-set-tab-root "/root/a/")
    (edmacs-workspaces-assign-group "group-a")
    (tab-bar-new-tab)
    (edmacs-workspaces-set-tab-root "/root/b/")
    (edmacs-workspaces-assign-group "group-b")
    (should-not (edmacs-workspaces-find-tab "group-a" "/root/b/"))
    (should (edmacs-workspaces-find-tab "group-a" "/root/a/"))
    (should (edmacs-workspaces-find-tab "group-b" "/root/b/"))
    (edmacs-workspaces-select-tab "group-a" "/root/a/")
    (should (equal (edmacs-workspaces-tab-root
                    (tab-bar--current-tab-find nil (selected-frame)))
                   "/root/a/"))))

(ert-deftest edmacs-workspaces-test-select-tab-nil-when-no-match ()
  (edmacs-workspaces-test--with-scratch-tabs
    (should-not (edmacs-workspaces-select-tab "no-such-group" "/no/such/root/"))))

;; ============================================================================
;; Group / tabs-in-group enumeration excludes ungrouped tabs
;; ============================================================================

(ert-deftest edmacs-workspaces-test-groups-excludes-ungrouped-tabs ()
  (edmacs-workspaces-test--with-scratch-tabs
    ;; The single default tab from the fixture carries no group at all.
    (should-not (edmacs-workspaces-groups))
    (tab-bar-new-tab)
    (edmacs-workspaces-assign-group "group-a")
    (should (equal (edmacs-workspaces-groups) '("group-a")))))

(ert-deftest edmacs-workspaces-test-tabs-in-group-excludes-ungrouped ()
  (edmacs-workspaces-test--with-scratch-tabs
    (tab-bar-new-tab)
    (edmacs-workspaces-assign-group "group-a")
    (should (= 1 (length (edmacs-workspaces-tabs-in-group "group-a"))))
    (should-not (edmacs-workspaces-tabs-in-group nil))))

;; ============================================================================
;; Phase 2 fixture: a real temp repo tree, with git-common-dir stubbed onto it
;; ============================================================================
;; The entry points below really do run `dired' and `tab-bar-new-tab', so the
;; roots they are handed have to exist on disk; the git resolution on top of
;; them does not, and is stubbed. Nothing here targets a subr, so no
;; native-comp trampoline is built.

(defvar edmacs-workspaces-test--tmp nil
  "Fixture root, bound by `edmacs-workspaces-test--with-repos'.")

(defun edmacs-workspaces-test--dir (relative)
  "Create RELATIVE under the fixture root; return its normalized truename."
  (let ((dir (expand-file-name relative edmacs-workspaces-test--tmp)))
    (make-directory dir t)
    (file-name-as-directory (file-truename dir))))

(defun edmacs-workspaces-test--repo-of (root)
  "Return the fixture repo name ROOT belongs to, or nil.
`<tmp>/repoA/...' and `<tmp>/repoA__worktrees/<slug>/' both answer
\"repoA\", mirroring rdm's real on-disk layout."
  (let* ((rel (file-relative-name (file-truename root) edmacs-workspaces-test--tmp))
         (leaf (car (split-string rel "/" t))))
    (when (and leaf (not (string-prefix-p ".." leaf)))
      (if (string-suffix-p "__worktrees" leaf)
          (substring leaf 0 (- (length leaf) (length "__worktrees")))
        leaf))))

(defmacro edmacs-workspaces-test--with-repos (&rest body)
  "Run BODY with a temp repo tree and git-common-dir resolution stubbed onto it.
`window-buffer-change-functions' is unhooked for the duration: the real
entry is a `run-at-time 0' scheduler, and a pending zero-delay timer
DOES get a chance to run inside `dired''s own subprocess wait, so a
sweep would otherwise fire between a test's setup and its assertions and
move the very windows under test. Every sweep assertion below therefore
calls `edmacs-workspaces--relocate-stray-visits' directly -- the timer's
own payload -- and the hook's membership is asserted separately by
`edmacs-workspaces-test-sweep-is-scheduled-off-the-redisplay-path'."
  (declare (indent 0))
  `(let ((window-buffer-change-functions nil)
         (edmacs-workspaces-test--tmp
          (file-name-as-directory (file-truename (make-temp-file "edmacs-ws-" t)))))
     (unwind-protect
         (cl-letf (((symbol-function 'edmacs-git-common-dir)
                    (lambda (root)
                      (when-let* ((repo (edmacs-workspaces-test--repo-of root)))
                        (expand-file-name (concat repo "/.git")
                                          edmacs-workspaces-test--tmp))))
                   ((symbol-function 'edmacs-git-common-dir-main-worktree)
                    (lambda (common) (file-name-directory (directory-file-name common))))
                   ((symbol-function 'edmacs-git-common-dir-repo-name)
                    (lambda (common)
                      (file-name-nondirectory
                       (directory-file-name
                        (file-name-directory (directory-file-name common)))))))
           ,@body)
       (delete-directory edmacs-workspaces-test--tmp t))))

(defun edmacs-workspaces-test--current-tab ()
  (tab-bar--current-tab-find nil (selected-frame)))

(defun edmacs-workspaces-test--group-of (tab)
  (funcall tab-bar-tab-group-function tab))

(defun edmacs-workspaces-test--repo-file (relative)
  "Return RELATIVE resolved against the repository root this suite runs from."
  (expand-file-name relative default-directory))

;; ============================================================================
;; AC1 -- `SPC p p': create the group and its main tab, then only select
;; ============================================================================

(ert-deftest edmacs-workspaces-test-open-project-creates-then-selects-group ()
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((main (edmacs-workspaces-test--dir "repoA")))
        (edmacs-workspaces-open-project main)
        (should (member "repoA" (edmacs-workspaces-groups)))
        (should (= 1 (length (edmacs-workspaces-tabs-in-group "repoA"))))
        (should (equal (edmacs-workspaces-tab-root
                        (car (edmacs-workspaces-tabs-in-group "repoA")))
                       main))
        ;; From another project's tab, a second call on repoA selects its
        ;; existing tab and creates nothing.
        (edmacs-workspaces-open-project (edmacs-workspaces-test--dir "repoB"))
        (let ((count (length (tab-bar-tabs))))
          (edmacs-workspaces-open-project main)
          (should (= count (length (tab-bar-tabs))))
          (should (= 1 (length (edmacs-workspaces-tabs-in-group "repoA"))))
          (should (equal (edmacs-workspaces-tab-root (edmacs-workspaces-test--current-tab))
                         main)))))))

(ert-deftest edmacs-workspaces-test-open-project-refuses-a-non-repo ()
  "A directory outside any git repo must `user-error', not make a groupless tab."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_) nil)))
        (let ((count (length (tab-bar-tabs))))
          (should-error (edmacs-workspaces-open-project
                         (edmacs-workspaces-test--dir "loose"))
                        :type 'user-error)
          (should (= count (length (tab-bar-tabs)))))))))

;; ============================================================================
;; AC2 -- `SPC T p' / `C-x t p': group-scoped find-or-create
;; ============================================================================

(ert-deftest edmacs-workspaces-test-open-worktree-groups-and-dedupes ()
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((main (edmacs-workspaces-test--dir "repoA"))
            (wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x")))
        (edmacs-workspaces-open-project main)
        (edmacs-workspaces-open-worktree wt)
        (let ((tab (edmacs-workspaces-test--current-tab)))
          ;; The group is repoA's, not whatever the originating tab carried.
          (should (equal (edmacs-workspaces-test--group-of tab) "repoA"))
          (should (equal (edmacs-workspaces-tab-root tab) wt)))
        (should (= 2 (length (edmacs-workspaces-tabs-in-group "repoA"))))
        ;; Re-invoking from another tab selects the existing one.
        (let ((count (length (tab-bar-tabs))))
          (edmacs-workspaces-select-tab "repoA" main)
          (edmacs-workspaces-open-worktree wt)
          (should (= count (length (tab-bar-tabs))))
          (should (equal (edmacs-workspaces-tab-root (edmacs-workspaces-test--current-tab))
                         wt)))))))

(ert-deftest edmacs-workspaces-test-open-worktree-scoped-by-group ()
  "Same root, different project group: not a duplicate."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x")))
        (edmacs-workspaces-open-worktree wt)
        (should (edmacs-workspaces-find-tab "repoA" wt))
        (should-not (edmacs-workspaces-find-tab "repoB" wt))))))

;; ============================================================================
;; AC3 -- three projects, three groups, one frame
;; ============================================================================

(ert-deftest edmacs-workspaces-test-three-projects-one-frame ()
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((frames (length (frame-list))))
        (dolist (name '("repoA" "repoB" "repoC"))
          (edmacs-workspaces-open-project (edmacs-workspaces-test--dir name))
          (should (= frames (length (frame-list)))))
        (should (equal (sort (copy-sequence (edmacs-workspaces-groups)) #'string<)
                       '("repoA" "repoB" "repoC")))
        (dolist (name '("repoA" "repoB" "repoC"))
          (should (= 1 (length (edmacs-workspaces-tabs-in-group name)))))
        ;; Contiguity, courtesy of `tab-bar-move-tab-to-group': collapsing the
        ;; group sequence into runs must leave no name appearing twice.
        (let* ((groups (mapcar #'edmacs-workspaces-test--group-of (tab-bar-tabs)))
               (runs (let (acc)
                       (dolist (g groups (nreverse acc))
                         (unless (equal g (car acc)) (push g acc))))))
          (should (= (length runs) (length (delete-dups (copy-sequence runs))))))))))

;; ============================================================================
;; AC4/AC5 -- the call sites and hook slots frames.el no longer owns
;; ============================================================================

(ert-deftest edmacs-workspaces-test-entry-points-are-bound ()
  (should (commandp 'edmacs-workspaces-open-project))
  (should (commandp 'edmacs-workspaces-open-worktree)))

(ert-deftest edmacs-workspaces-test-no-frames-open-call-sites ()
  "AC4's call-site grep, as an assertion.
`frames.el' itself is excluded (its own definitions and Commentary are
allowed to survive); every other non-test source must be clean."
  (dolist (file '("init.el" "modules/core.el" "modules/sessions.el"
                  "modules/sidebar.el" "modules/sidebar-agents.el"
                  "modules/workspaces.el"))
    (with-temp-buffer
      (insert-file-contents (edmacs-workspaces-test--repo-file file))
      (goto-char (point-min))
      (should-not (search-forward "edmacs-frames-open" nil t))))
  ;; ...and the two chords/entry points name the new commands.
  (with-temp-buffer
    (insert-file-contents (edmacs-workspaces-test--repo-file "modules/core.el"))
    (goto-char (point-min))
    (should (search-forward
             "(setq project-switch-commands #'edmacs-workspaces-open-project)" nil t)))
  (with-temp-buffer
    (insert-file-contents (edmacs-workspaces-test--repo-file "modules/sessions.el"))
    (let ((source (buffer-string)))
      (should (string-match-p
               (regexp-quote "(\"t p\" . edmacs-workspaces-open-worktree)") source))
      (should (string-match-p
               (regexp-quote "\"p\" '(edmacs-workspaces-open-worktree") source)))))

(ert-deftest edmacs-workspaces-test-frames-hooks-are-detached ()
  "AC5, at the source level: frames.el installs none of the three hooks.
The running-config half of this AC is a `STARTUP_CHECK_EVAL' assertion
through `scripts/startup-check.sh' -- under `-Q --batch' frames.el's
`add-hook' forms would never run at all, so an ERT-only version would
pass vacuously."
  (let ((source (with-temp-buffer
                  (insert-file-contents (edmacs-workspaces-test--repo-file "modules/frames.el"))
                  (buffer-string))))
    (dolist (form '("(add-hook 'tab-bar-tab-post-open-functions"
                    "(add-hook 'tab-bar-tab-post-select-functions"
                    "(add-hook 'window-buffer-change-functions"))
      (should-not (string-match-p (regexp-quote form) source)))
    ;; The teardown/fullscreen hooks are deliberately untouched.
    (dolist (form '("(add-hook 'delete-frame-functions"
                    "(add-hook 'after-make-frame-functions"
                    "(add-hook 'emacs-startup-hook"))
      (should (string-match-p (regexp-quote form) source)))))

;; ============================================================================
;; AC6 -- a tab opened through the new entry point stays open
;; ============================================================================

(ert-deftest edmacs-workspaces-test-new-worktree-tab-survives-post-open ()
  "Regression for the reconciler self-collision the phase body probed.
The stamper really is on `tab-bar-tab-post-open-functions' while this
runs -- asserted, not assumed -- so the tab genuinely survives the
post-open hook rather than surviving its absence."
  (should (memq #'edmacs-workspaces--on-tab-post-open tab-bar-tab-post-open-functions))
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((main (edmacs-workspaces-test--dir "repoA"))
            (wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x")))
        (edmacs-workspaces-open-project main)
        (let ((before (length (tab-bar-tabs))))
          (edmacs-workspaces-open-worktree wt)
          (should (= (1+ before) (length (tab-bar-tabs))))
          (should (edmacs-workspaces-find-tab "repoA" wt))
          (should (equal (edmacs-workspaces-tab-root (edmacs-workspaces-test--current-tab))
                         wt)))))))

;; ============================================================================
;; AC7 -- an ordinary file visit creates no frame
;; ============================================================================

(ert-deftest edmacs-workspaces-test-file-visit-creates-no-frame ()
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let* ((root (edmacs-workspaces-test--dir "repoA"))
             (file (expand-file-name "note.txt" root))
             (frames (length (frame-list)))
             (buf nil))
        (write-region "" nil file nil 'silent)
        (edmacs-workspaces-open-project root)
        (unwind-protect
            (progn
              (setq buf (find-file-noselect file))
              (set-window-buffer (selected-window) buf)
              ;; `make-frame' is a plain Lisp `defun' in frame.el, so this
              ;; `cl-letf' builds no native-comp subr trampoline.
              (cl-letf (((symbol-function 'make-frame)
                         (lambda (&rest _) (ert-fail "make-frame called"))))
                (edmacs-workspaces--relocate-stray-visits (selected-frame)))
              (should (= frames (length (frame-list)))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;; ============================================================================
;; AC8 -- a stray DIRED buffer is relocated (the `buffer-file-name' defect)
;; ============================================================================

(ert-deftest edmacs-workspaces-test-stray-dired-buffer-is-relocated ()
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let* ((a (edmacs-workspaces-test--dir "repoA"))
             (b (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x"))
             (stray nil))
        (edmacs-workspaces-open-project a)
        (edmacs-workspaces-open-worktree b)
        (edmacs-workspaces-select-tab "repoA" a)
        (unwind-protect
            (progn
              (setq stray (dired-noselect b))
              ;; The must-fail-against-the-old-gate half: keyed on
              ;; `buffer-file-name' alone this buffer is invisible, so the
              ;; scenario is not even reachable by the old sweep.
              (should-not (buffer-file-name stray))
              (should-not (let ((f (buffer-file-name stray)))
                            (and f (file-name-directory f))))
              ;; The `default-directory' fallback does see it.
              (should (equal (edmacs-workspaces--buffer-dir stray) b))
              (set-window-buffer (selected-window) stray)
              (let ((target (edmacs-workspaces--stray-tab-number stray)))
                (should target)
                (should (equal (edmacs-workspaces-tab-root
                                (nth (1- target) (tab-bar-tabs)))
                               b))
                (edmacs-workspaces--relocate-stray-visits (selected-frame))
                ;; It now lives in B's tab...
                (should (equal (edmacs-workspaces-tab-root
                                (edmacs-workspaces-test--current-tab))
                               b))
                (should (memq stray (mapcar #'window-buffer (window-list nil 'never))))
                ;; ...and no longer in A's.
                (edmacs-workspaces-select-tab "repoA" a)
                (should-not (memq stray
                                  (mapcar #'window-buffer (window-list nil 'never))))))
          (when (buffer-live-p stray) (kill-buffer stray)))))))

(ert-deftest edmacs-workspaces-test-two-strays-in-one-sweep-both-relocate ()
  "Every stray found in one sweep moves, not just the last one discovered.
The sweep collects its moves with `push' and walks them twice -- once to
un-display, once to relocate -- so the reversed list has to be captured,
not produced inline in the first `dolist'. Reversed inline, the variable
is left holding only the final cons and every earlier buffer is
un-displayed and then relocated nowhere. A single-window scenario cannot
see it: `nreverse' on a one-element list is a structural no-op.

The stray buffers are dired buffers of SUBDIRECTORIES of their target
worktrees. A dired buffer of the root itself is the very buffer
`edmacs-workspaces--open-tab' already displayed there, so the arrival
assertion would hold without the sweep having moved anything."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let* ((a (edmacs-workspaces-test--dir "repoA"))
             (b (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x"))
             (c (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-y"))
             (b-sub (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x/sub"))
             (c-sub (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-y/sub"))
             (stray-b nil)
             (stray-c nil))
        (edmacs-workspaces-open-project a)
        (edmacs-workspaces-open-worktree b)
        (edmacs-workspaces-open-worktree c)
        (edmacs-workspaces-select-tab "repoA" a)
        (unwind-protect
            (progn
              (setq stray-b (dired-noselect b-sub))
              (setq stray-c (dired-noselect c-sub))
              ;; Two ordinary windows in A's tab, each showing a buffer that
              ;; belongs in a different sibling tab.
              (delete-other-windows)
              (let ((w1 (selected-window))
                    (w2 (split-window-below)))
                (set-window-buffer w1 stray-b)
                (set-window-buffer w2 stray-c))
              (should (edmacs-workspaces--stray-tab-number stray-b))
              (should (edmacs-workspaces--stray-tab-number stray-c))
              (edmacs-workspaces--relocate-stray-visits (selected-frame))
              ;; Both arrived, not just whichever was discovered last.
              (edmacs-workspaces-select-tab "repoA" b)
              (should (memq stray-b (mapcar #'window-buffer (window-list nil 'never))))
              (edmacs-workspaces-select-tab "repoA" c)
              (should (memq stray-c (mapcar #'window-buffer (window-list nil 'never)))))
          (when (buffer-live-p stray-b) (kill-buffer stray-b))
          (when (buffer-live-p stray-c) (kill-buffer stray-c)))))))

(ert-deftest edmacs-workspaces-test-stray-tab-number-nil-for-own-tab ()
  "A buffer already in the right tab is not a stray."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let* ((b (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x"))
             (buf nil))
        (edmacs-workspaces-open-worktree b)
        (unwind-protect
            (progn
              (setq buf (dired-noselect b))
              (should-not (edmacs-workspaces--stray-tab-number buf)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest edmacs-workspaces-test-stray-tab-number-longest-root-wins ()
  "A nested worktree beats the repo directory containing it.
Real directories, because `file-in-directory-p' short-circuits on a
`file-directory-p' check of its DIR argument."
  (let ((tab-bar-tab-post-open-functions nil))
    (edmacs-workspaces-test--with-repos
      (edmacs-workspaces-test--with-scratch-tabs
        (let ((outer (edmacs-workspaces-test--dir "outer"))
              (inner (edmacs-workspaces-test--dir "outer/inner"))
              (sub (edmacs-workspaces-test--dir "outer/inner/sub"))
              (other (edmacs-workspaces-test--dir "elsewhere")))
          ;; Grouped, because only a project tab is ever a relocation target.
          (edmacs-workspaces-set-tab-root outer)
          (edmacs-workspaces-assign-group "repoA")
          (tab-bar-new-tab)
          (edmacs-workspaces-set-tab-root inner)
          (edmacs-workspaces-assign-group "repoA")
          (tab-bar-new-tab)
          (edmacs-workspaces-set-tab-root other)
          (edmacs-workspaces-assign-group "repoB")
          (cl-letf (((symbol-function 'edmacs-workspaces--buffer-dir)
                     (lambda (_) sub)))
            (should (equal (edmacs-workspaces-tab-root
                            (nth (1- (edmacs-workspaces--stray-tab-number
                                      (current-buffer)))
                                 (tab-bar-tabs)))
                           inner))))))))

;; ============================================================================
;; AC9 -- a plain `tab-bar-new-tab' (`SPC T n') tab still carries a root
;; ============================================================================

(ert-deftest edmacs-workspaces-test-plain-new-tab-is-stamped-once ()
  "`SPC T n' stays bound to plain `tab-bar-new-tab'; the stamper covers it.
At post-open time the new tab's window still shows the ORIGINATING
tab's buffer, which is exactly the wanted answer here."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x")))
        (edmacs-workspaces-open-worktree wt)
        (tab-bar-new-tab)
        (let ((tab (edmacs-workspaces-test--current-tab)))
          (should (equal (edmacs-workspaces-tab-root tab) wt))
          ;; Inherited via `tab-bar-new-tab-group' = t.
          (should (equal (edmacs-workspaces-test--group-of tab) "repoA"))
          ;; `setf', not `push': exactly one root entry.
          (should (= 1 (length (seq-filter
                                (lambda (kv)
                                  (and (consp kv)
                                       (eq (car kv) edmacs-workspaces-root-parameter)))
                                (cdr tab))))))))))

(ert-deftest edmacs-workspaces-test-post-open-hook-is-stamp-only ()
  "No reconciliation, no tab closing, no group assignment in the hook."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (edmacs-workspaces-test--repo-file "modules/workspaces.el"))
                  (buffer-string))))
    (let* ((start (string-match
                   (regexp-quote "(defun edmacs-workspaces--on-tab-post-open") source))
           (end (string-match (regexp-quote "(add-hook 'tab-bar-tab-post-open-functions")
                              source start))
           (body (substring source start end)))
      (dolist (forbidden '("tab-bar-close-tab" "tab-bar-select-tab"
                           "seq-find" "edmacs-workspaces-assign-group"))
        (should-not (string-match-p (regexp-quote forbidden) body))))))

(ert-deftest edmacs-workspaces-test-stray-sweep-ignores-ungrouped-tab ()
  "The frame's original ungrouped tab is never a relocation target.
Its root is whatever directory the frame started in -- typically an
ancestor of everything -- so a visit to a file outside every open
worktree must be left alone, not dragged onto it."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let* ((home (edmacs-workspaces-test--dir "home"))
             (loose (edmacs-workspaces-test--dir "home/loose"))
             (wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x"))
             (buf nil))
        ;; The fixture's ungrouped tab stands in for the boot tab.
        (edmacs-workspaces-set-tab-root home)
        (should-not (edmacs-workspaces-test--group-of
                     (edmacs-workspaces-test--current-tab)))
        (edmacs-workspaces-open-worktree wt)
        (unwind-protect
            (progn
              (setq buf (dired-noselect loose))
              (should-not (edmacs-workspaces--stray-tab-number buf)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest edmacs-workspaces-test-sweep-is-scheduled-off-the-redisplay-path ()
  "The sweep is installed, and only ever deferred -- never run in the hook.
`window-buffer-change-functions' fires mid-redisplay."
  (should (memq #'edmacs-workspaces--on-window-buffer-change
                window-buffer-change-functions))
  (let ((scheduled '()))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (secs repeat fn &rest args)
                 (push (list secs repeat fn args) scheduled)
                 nil))
              ((symbol-function 'edmacs-workspaces--relocate-stray-visits)
               (lambda (_frame) (ert-fail "swept synchronously from the hook"))))
      (edmacs-workspaces--on-window-buffer-change (selected-frame))
      (should (= 1 (length scheduled)))
      (pcase-let ((`(,secs ,repeat ,fn ,_args) (car scheduled)))
        (should (equal secs 0))
        (should-not repeat)
        (should (eq fn #'edmacs-workspaces--relocate-stray-visits))))))

(ert-deftest edmacs-workspaces-test-sweep-re-entrancy-guard ()
  "A sweep already in progress neither re-enters nor re-schedules."
  (let ((edmacs-workspaces--relocating t))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (ert-fail "re-scheduled from inside a sweep"))))
      (edmacs-workspaces--on-window-buffer-change (selected-frame)))
    (cl-letf (((symbol-function 'window-list)
               (lambda (&rest _) (ert-fail "re-entered a running sweep"))))
      (edmacs-workspaces--relocate-stray-visits (selected-frame)))))

;; ============================================================================
;; Phase 4 -- desktop migration: frames model -> groups and tabs in one frame
;; ============================================================================
;; The fixture below mirrors the SHAPE of the user's real
;; `~/.config/emacs/.cache/desktop/.emacs.desktop' as it stood before this
;; roadmap: two frame states, each carrying an `edmacs-repo' frame
;; parameter and a `tabs' list whose single `current-tab' carries
;; `frames.el''s legacy `edmacs-root', with a window state holding the
;; sidebar side window and bufferlo's own `bufferlo-buffer-list' entry.
;; It is a sanitized literal, never the real file: the real one's
;; `environment' parameter dumps the whole shell environment, tokens
;; included. Paths are synthetic (`/w/...') so `file-truename' resolves
;; no symlink and needs nothing on disk.

(defmacro edmacs-workspaces-test--with-stub-git (&rest body)
  "Run BODY with git resolution stubbed onto the `/w/<repo>/' fixture layout.
Group derivation must go through the same `edmacs-workspaces-group-name'
the runtime open paths use, or the migrated group strings would not be
`equal' to the ones a later reopen computes."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'edmacs-git-common-dir)
              (lambda (root)
                ;; `/w/<repo>/' and `/w/<repo>__worktrees/<slug>/' both
                ;; resolve to `<repo>', mirroring rdm's on-disk layout.
                (let* ((clean (directory-file-name (file-truename root)))
                       (parent (file-name-nondirectory
                                (directory-file-name
                                 (or (file-name-directory clean) "/"))))
                       (repo (if (string-suffix-p "__worktrees" parent)
                                 (string-remove-suffix "__worktrees" parent)
                               (file-name-nondirectory clean))))
                  (concat "/w/" repo "/.git"))))
             ((symbol-function 'edmacs-git-common-dir-repo-name)
              (lambda (common)
                (file-name-nondirectory
                 (directory-file-name
                  (file-name-directory (directory-file-name common)))))))
     ,@body))

(defun edmacs-workspaces-test--window-state (buffer-names)
  "Return a window-state literal shaped like a real saved frame's.
Carries the sidebar side window and the `bufferlo-buffer-list' entry
bufferlo's own `window-state-get' advice appends -- the entry that is a
restored tab's only surviving record of its buffer scope, since desktop
strips `wc-bl'/`wc-bbl' on save."
  `((min-height . 4) (min-width . 42)
    hc (pixel-width . 1508) (pixel-height . 923)
    (leaf (pixel-width . 288)
          (parameters (window-side . left) (window-slot . 0)
                      (no-delete-other-windows . t) (no-other-window . t))
          (buffer ,(format "*sidebar: %s*" (car buffer-names))
                  (selected) (point . 1) (start . 1)))
    (leaf (last . t) (pixel-width . 1220)
          (parameters (edmacs-stack-popup . t) (edmacs-main . t))
          (buffer ,(car buffer-names) (selected . t) (point . 1) (start . 1)))
    (bufferlo-buffer-list ,buffer-names)))

(defun edmacs-workspaces-test--frames-model-fixture ()
  "Return a two-state frameset in the pre-roadmap frames model's shape.
Deep-copied on every call: the literals below are compile-time
constants, and the tests that reshape the fixture would otherwise mutate
them in place for every later test in the process."
  (copy-tree
   (frameset--make
   :version 1 :timestamp '(27294 4191 109240 0)
   :app '(desktop . "208") :name "test"
   :states
   (list
    (cons `((minibuffer . t)
            (tab-bar-lines . 0)
            (fullscreen . fullboth)
            (frameset--id . "5BD5-09A4-276B-7EFE")
            (tabs (current-tab (edmacs-root . "/w/cloudcitydotgay/")
                               (name . "cloudcitydotgay")
                               (explicit-name . t)))
            (last-focus-update)
            (edmacs-repo . "/w/cloudcitydotgay/.git")
            (edmacs-repo-missing)
            (height . 42) (width . 165))
          (edmacs-workspaces-test--window-state '("cloudcitydotgay" " *Minibuf-1*")))
    (cons `((minibuffer . t)
            (tab-bar-lines . 0)
            (fullscreen . fullboth)
            (tabs (current-tab (edmacs-root . "/w/edmacs/")
                               (name . "edmacs")
                               (explicit-name . t)))
            (last-focus-update . t)
            (frameset--id . "9B25-4094-FC7D-6ED3")
            (edmacs-repo . "/w/edmacs/.git")
            (edmacs-repo-missing)
            (height . 72) (width . 424))
          (edmacs-workspaces-test--window-state '("edmacs" " *Minibuf-1*")))))
   t))

(defun edmacs-workspaces-test--migrated-tabs (fs)
  "Return the tabs of the single frame state of migrated frameset FS."
  (alist-get 'tabs (car (car (frameset-states fs)))))

(defun edmacs-workspaces-test--tab-values (tabs key)
  (delq nil (mapcar (lambda (tab) (alist-get key (cdr tab))) tabs)))

(ert-deftest edmacs-workspaces-test-migrate-frames-model-fixture ()
  "AC2: the real desktop's shape converts with no project or worktree lost.
Every tab keeps its worktree, gains its project group, and swaps
`frames.el''s `edmacs-root' for this module's own parameter -- a
migration that reshaped the frames but left the old name in place would
restore tabs the new model cannot read at all."
  (edmacs-workspaces-test--with-stub-git
    (let* ((out (edmacs-workspaces-migrate-frameset
                 (edmacs-workspaces-test--frames-model-fixture)))
           (tabs (edmacs-workspaces-test--migrated-tabs out)))
      (should (= 1 (length (frameset-states out))))
      (should (= 2 (length tabs)))
      (should (equal (edmacs-workspaces-test--tab-values tabs 'group)
                     '("edmacs" "cloudcitydotgay")))
      (should (equal (sort (edmacs-workspaces-test--tab-values
                            tabs edmacs-workspaces-root-parameter)
                           #'string<)
                     '("/w/cloudcitydotgay/" "/w/edmacs/")))
      ;; The legacy name is gone everywhere, tab and frame alike.
      (should-not (edmacs-workspaces-test--tab-values tabs 'edmacs-root))
      (should-not (alist-get 'edmacs-repo (car (car (frameset-states out)))))
      (should-not (alist-get 'edmacs-repo-missing (car (car (frameset-states out)))))
      ;; `tab-bar--current-tab-find' is a bare `(assq 'current-tab tabs)': two
      ;; would silently pin selection to the wrong tab, none would leave the
      ;; frame with no selected tab at all.
      (should (= 1 (seq-count (lambda (tab) (eq (car tab) 'current-tab)) tabs)))
      ;; The focused frame stays primary, so its tab is the one still selected.
      (should (equal (alist-get 'name (cdr (assq 'current-tab tabs))) "edmacs")))))

(ert-deftest edmacs-workspaces-test-migrate-folds-window-state-into-ws ()
  "AC5: a folded `current-tab' MUST gain the frame's window state as `ws'.
A `current-tab' carries none by construction, and a restored tab has no
live `wc' either -- so without this `tab-bar-select-tab' would have
nothing to `window-state-put'. The same `ws' is what carries bufferlo's
per-tab buffer list across a restart, desktop having stripped
`wc-bl'/`wc-bbl' on save."
  (edmacs-workspaces-test--with-stub-git
    (let* ((out (edmacs-workspaces-migrate-frameset
                 (edmacs-workspaces-test--frames-model-fixture)))
           (folded (seq-filter (lambda (tab) (eq (car tab) 'tab))
                               (edmacs-workspaces-test--migrated-tabs out))))
      (should (= 1 (length folded)))
      (dolist (tab folded)
        (let ((ws (alist-get 'ws (cdr tab))))
          (should ws)
          (should (alist-get 'time (cdr tab)))
          ;; bufferlo's documented fallback: `(assq 'bufferlo-buffer-list
          ;; (assq 'ws tab))'.
          (should (equal (car (cdr (assq 'bufferlo-buffer-list (assq 'ws (cdr tab)))))
                         '("cloudcitydotgay" " *Minibuf-1*"))))))))

(ert-deftest edmacs-workspaces-test-migrate-two-frames-yields-one-state ()
  "AC4: one state out means `frameset-restore' reuses the boot GUI frame
and creates no second one -- no frame has to be deleted to get there."
  (edmacs-workspaces-test--with-stub-git
    (should (= 1 (length (frameset-states
                          (edmacs-workspaces-migrate-frameset
                           (edmacs-workspaces-test--frames-model-fixture))))))))

(ert-deftest edmacs-workspaces-test-migrate-keeps-groups-contiguous ()
  "`tab-bar-move-tab-to-group' is not running while a frameset is being
assembled, so each group's tabs have to be made contiguous here."
  (edmacs-workspaces-test--with-stub-git
    (let* ((fixture (edmacs-workspaces-test--frames-model-fixture))
           (states (frameset-states fixture)))
      ;; A second `edmacs' worktree tab on the NON-primary frame -- a real
      ;; possibility under the frames model, whose stray-visit relocator
      ;; only made it rare. A naive primary-then-fold concatenation would
      ;; leave the two `edmacs' tabs split around the `cloudcitydotgay' one.
      (setf (alist-get 'tabs (car (car states)))
            (append (alist-get 'tabs (car (car states)))
                    '((tab (edmacs-root . "/w/edmacs__worktrees/roadmap-x/")
                           (name . "roadmap-x") (time . 1.0) (ws nil)))))
      (let* ((out (edmacs-workspaces-migrate-frameset fixture))
             (groups (mapcar (lambda (tab) (alist-get 'group (cdr tab)))
                             (edmacs-workspaces-test--migrated-tabs out))))
        (should (equal groups '("edmacs" "edmacs" "cloudcitydotgay")))))))

(ert-deftest edmacs-workspaces-test-migrate-drops-only-duplicate-pairs ()
  "A folded tab is dropped only when the identical (group, root) pair is
already present: AC2 says nothing may be lost, so a second worktree of
the same project must survive."
  (edmacs-workspaces-test--with-stub-git
    (let* ((fixture (edmacs-workspaces-test--frames-model-fixture))
           (states (frameset-states fixture)))
      (setf (alist-get 'tabs (car (car states)))
            '((current-tab (edmacs-root . "/w/edmacs/") (name . "edmacs"))
              (tab (edmacs-root . "/w/edmacs-other/") (name . "other")
                   (time . 1.0) (ws nil))))
      (let* ((out (edmacs-workspaces-migrate-frameset fixture))
             (tabs (edmacs-workspaces-test--migrated-tabs out)))
        ;; The duplicate `/w/edmacs/' tab folded away; the distinct one did not.
        (should (equal (sort (edmacs-workspaces-test--tab-values
                              tabs edmacs-workspaces-root-parameter)
                             #'string<)
                       '("/w/edmacs-other/" "/w/edmacs/")))))))

(ert-deftest edmacs-workspaces-test-migrate-keeps-a-tab-whose-worktree-is-gone ()
  "A root pointing at a removed worktree keeps its tab: the group falls
back to the pure repo name of the frame's own legacy `edmacs-repo',
which needs no disk access at all."
  (cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_root) nil))
            ((symbol-function 'edmacs-git-common-dir-repo-name)
             (lambda (common)
               (file-name-nondirectory
                (directory-file-name
                 (file-name-directory (directory-file-name common)))))))
    (let* ((out (edmacs-workspaces-migrate-frameset
                 (edmacs-workspaces-test--frames-model-fixture)))
           (tabs (edmacs-workspaces-test--migrated-tabs out)))
      (should (= 2 (length tabs)))
      (should (equal (sort (edmacs-workspaces-test--tab-values tabs 'group) #'string<)
                     '("cloudcitydotgay" "edmacs"))))))

(ert-deftest edmacs-workspaces-test-migrate-state-without-tabs-keeps-its-layout ()
  "A frame state carrying no `tabs' parameter contributes a synthesized
ungrouped tab rather than losing that frame's whole layout."
  (cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_root) nil))
            ((symbol-function 'edmacs-git-common-dir-repo-name) (lambda (_) nil)))
    (let* ((ws (edmacs-workspaces-test--window-state '("scratch")))
           (fs (frameset--make
                :version 1 :timestamp '(27294 4191 109240 0)
                :app '(desktop . "208") :name "test"
                :states (list (cons '((last-focus-update . t)
                                      (tabs (current-tab (name . "main"))))
                                    nil)
                              (cons '((name . "spare")) ws))))
           (tabs (edmacs-workspaces-test--migrated-tabs
                  (edmacs-workspaces-migrate-frameset fs))))
      (should (= 2 (length tabs)))
      (should (equal (alist-get 'ws (cdr (car (last tabs)))) ws)))))

;; ----------------------------------------------------------------------------
;; AC3 -- the migration is a fixed point, not a one-shot
;; ----------------------------------------------------------------------------

(ert-deftest edmacs-workspaces-test-migrate-frameset-is-idempotent ()
  "Running it twice is a no-op the second time. It ensures rather than
detects: `edmacs-frames-stamp-frame-tabs' still re-adds `edmacs-root' to
live tabs until phase 5, so a \"has edmacs-root\" detector would re-fire
on every boot."
  (edmacs-workspaces-test--with-stub-git
    (let ((fixture (edmacs-workspaces-test--frames-model-fixture)))
      (should (equal (edmacs-workspaces-migrate-frameset fixture)
                     (edmacs-workspaces-migrate-frameset
                      (edmacs-workspaces-migrate-frameset fixture)))))))

(ert-deftest edmacs-workspaces-test-migrate-new-shape-frameset-is-a-noop ()
  "A frameset already in the new shape comes back `equal' to its input --
including no re-stamped `time', which is what makes leaving this in the
daemon's boot path permanently safe."
  (edmacs-workspaces-test--with-stub-git
    (let ((fs (frameset--make
               :version 1 :timestamp '(27294 4191 109240 0)
               :app '(desktop . "208") :name "test"
               :states (list (cons `((last-focus-update . t)
                                     (tabs (current-tab (name . "edmacs")
                                                        (group . "edmacs")
                                                        (edmacs-workspace-root
                                                         . "/w/edmacs/"))
                                           (tab (name . "cloudcitydotgay")
                                                (group . "cloudcitydotgay")
                                                (time . 1.0)
                                                (edmacs-workspace-root
                                                 . "/w/cloudcitydotgay/")
                                                (ws nil)))
                                     (height . 72))
                                   nil)))))
      (should (equal (edmacs-workspaces-migrate-frameset fs) fs)))))

(ert-deftest edmacs-workspaces-test-migrate-does-not-mutate-its-input ()
  "Without this the idempotency assertions above could pass by aliasing."
  (edmacs-workspaces-test--with-stub-git
    (let* ((fixture (edmacs-workspaces-test--frames-model-fixture))
           (before (copy-tree fixture t)))
      (edmacs-workspaces-migrate-frameset fixture)
      (should (equal fixture before)))))

(ert-deftest edmacs-workspaces-test-migrate-empty-frameset-unchanged ()
  "A zero-state frameset -- what a frameless daemon's own save writes --
is returned untouched, so `edmacs-sessions--frameset-has-frames-p' keeps
seeing exactly what it sees today."
  (let ((empty (frameset--make :version 1 :timestamp '(0 0 0 0)
                               :app '(desktop . "208") :name "test" :states nil)))
    (should (eq (edmacs-workspaces-migrate-frameset empty) empty)))
  (should-not (edmacs-workspaces-migrate-frameset nil))
  (should (eq (edmacs-workspaces-migrate-frameset 'not-a-frameset) 'not-a-frameset)))

(ert-deftest edmacs-workspaces-test-migrate-normalizes-roots-like-open-tab ()
  "The migrated root must be normalized exactly as
`edmacs-workspaces--open-tab' stamps one, or `edmacs-workspaces-find-tab'
\(an `equal' match) would never find a migrated tab and every reopen
would duplicate it."
  (edmacs-workspaces-test--with-stub-git
    (let* ((fs (frameset--make
                :version 1 :timestamp '(27294 4191 109240 0)
                :app '(desktop . "208") :name "test"
                :states (list (cons '((last-focus-update . t)
                                      (tabs (current-tab
                                             (edmacs-root . "/w/edmacs")
                                             (name . "edmacs"))))
                                    nil))))
           (tabs (edmacs-workspaces-test--migrated-tabs
                  (edmacs-workspaces-migrate-frameset fs))))
      (should (equal (alist-get edmacs-workspaces-root-parameter (cdr (car tabs)))
                     (file-name-as-directory (file-truename "/w/edmacs")))))))

(provide 'workspaces-test)
;;; workspaces-test.el ends here
