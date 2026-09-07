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

(provide 'workspaces-test)
;;; workspaces-test.el ends here
