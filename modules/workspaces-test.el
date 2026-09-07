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
                       "/root/a/"))))))

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
    (cl-letf (((symbol-function 'vc-git-known-other-working-trees)
               (lambda () (setq calls (1+ calls)) '("/repo/other/"))))
      (edmacs-workspaces-worktrees "/repo/main/")
      (edmacs-workspaces-worktrees "/repo/main/")
      (should (= calls 2)))))

(ert-deftest edmacs-workspaces-test-worktrees-includes-root-itself ()
  "ROOT's own worktree is consed back on, since the underlying git query excludes it."
  (cl-letf (((symbol-function 'vc-git-known-other-working-trees)
             (lambda () nil)))
    (let ((result (edmacs-workspaces-worktrees "/repo/main/")))
      (should (member (file-truename (file-name-as-directory "/repo/main/")) result)))))

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
    ;; that would install one, not that substring.
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

(provide 'workspaces-test)
;;; workspaces-test.el ends here
