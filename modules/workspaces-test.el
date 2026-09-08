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
;;   emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/git-common-dir.el -l modules/windows.el \
;;         -l modules/workspaces.el -l modules/workspaces-test.el \
;;         -f edmacs-test-support-run-and-exit
;;
;; modules/windows.el is not optional: workspaces.el owns the config's
;; only `tab-bar-tab-post-open-functions' entry and calls
;; `edmacs-windows-designate-main' and `edmacs-windows-ws-main-buffer-names'
;; from it, so a real `tab-bar-new-tab' without windows.el loaded dies
;; with a void-function.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'tab-bar)
;; For `desktop-buffer-args-list', the pending-restore list
;; `edmacs-workspaces--buffer-name-directory' falls back to. Required
;; rather than `defvar'd so the variable is special exactly as it is in
;; production, where sessions.el has loaded desktop.el long before any
;; restore-time stamping runs.
(require 'desktop)

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
default tab, per `tab-bar-tabs' own \"create default tabs\" branch.

`edmacs-workspaces--group-memo' is cleared on both edges too: a group
name is derived from a root through whatever git stub was in force, and
a memoized answer surviving into the next test would serve one fixture's
repo layout to another's roots."
  (declare (indent 0))
  `(let ((saved (frame-parameter nil 'tabs)))
     (unwind-protect
         (progn
           (edmacs-workspaces-clear-group-memo)
           (set-frame-parameter nil 'tabs nil)
           (tab-bar-tabs)
           ,@body)
       (edmacs-workspaces-clear-group-memo)
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
  "A LIVE directory git-common-dir cannot resolve at all yields nil, not
an error and not a path-derived name: the daemon's boot tab is stamped
with a real home directory and must stay ungrouped.
`edmacs-workspaces-test-group-name-path-fallback' covers the other side
-- a root that is NOT on disk, where the path is all there is."
  (cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_) nil)))
    (should (file-directory-p (temporary-file-directory)))
    (should-not (edmacs-workspaces-group-name
                 (file-name-as-directory (temporary-file-directory))))))

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
    ;; Spelled in pieces: `-no-worktree-cache-watch-or-timer-anywhere'
    ;; below scans every file under `modules/', this one included.
    (dolist (forbidden (list (concat "file-notify" "-add-watch")
                             (concat "file-notify" "-rm-watch")
                             "run-with-timer" "run-with-idle-timer"))
      (should-not (string-match-p (regexp-quote forbidden) source)))
    ;; `edmacs-workspaces--group-memo' is deliberately not spelled
    ;; "cache": it is a pure root->name memo with no watch, no timer and
    ;; no invalidation of its own, not the retired worktree-discovery
    ;; cache this assertion exists to keep out. The grep therefore stays
    ;; on the literal word, and the memo must never adopt it.
    (should-not (string-match-p "(defvar [^\n]*cache" source))))

;; ============================================================================
;; Group assignment: through `tab-bar-change-tab-group', adjacency
;; ============================================================================

(ert-deftest edmacs-workspaces-test-nothing-writes-a-tab-group-parameter ()
  "The `group' tab parameter is not part of a tab's stored identity any
more, so no non-test module may write one -- neither directly nor via
core's `tab-bar-change-tab-group'. A stored group that disagreed with
its root is exactly what made a tab invisible to `find-tab' and
misfiled in the sidebar."
  (dolist (file (edmacs-workspaces-test--module-files))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((text (buffer-string)))
        (dolist (forbidden '("(tab-bar-change-tab-group" "(setf (alist-get 'group"))
          (should (equal (list file forbidden nil)
                         (list file forbidden
                               (string-match-p (regexp-quote forbidden) text)))))))))

(ert-deftest edmacs-workspaces-test-move-tab-to-group-adjacency ()
  "Rooting the 1st and 3rd of three tabs in one repo ends with them adjacent.
Core's own `tab-bar-move-tab-to-group' cannot do this any more: it reads
`(alist-get \\='group tab)', which nothing writes now, so it would see
three ungrouped tabs and move none of them."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((wt-a (edmacs-workspaces-test--dir "repoA__worktrees/a"))
            (wt-b (edmacs-workspaces-test--dir "repoA__worktrees/b"))
            (other (edmacs-workspaces-test--dir "repoB")))
        (edmacs-workspaces-set-tab-root wt-a)
        (tab-bar-rename-tab "t1")
        (tab-bar-new-tab)
        (edmacs-workspaces-set-tab-root other)
        (tab-bar-rename-tab "t2")
        (tab-bar-new-tab)
        (edmacs-workspaces-set-tab-root wt-b)
        (tab-bar-rename-tab "t3")
        ;; t1 (repoA) is tab 1, t2 (repoB) tab 2, t3 (repoA, current) tab 3.
        (should (equal (mapcar #'edmacs-workspaces-test--group-of (tab-bar-tabs))
                       '("repoA" "repoB" "repoA")))
        (edmacs-workspaces-move-tab-to-group)
        (let* ((names (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
               (t1-pos (seq-position names "t1"))
               (t3-pos (seq-position names "t3")))
          (should (= 1 (abs (- t1-pos t3-pos)))))))))

(defun edmacs-workspaces-test--tab-names ()
  "Return the selected frame's tab names, left to right."
  (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))

(defun edmacs-workspaces-test--build-tabs (&rest specs)
  "Build the frame's tab bar from SPECS, a list of (NAME . ROOT) conses.
The first spec renames and stamps the scratch tab the enclosing
`edmacs-workspaces-test--with-scratch-tabs' already made; each later one
adds a tab to its right. A nil ROOT leaves the tab unstamped, i.e.
ungrouped."
  (let ((first t))
    (dolist (spec specs)
      (unless first (tab-bar-new-tab))
      (setq first nil)
      (when (cdr spec) (edmacs-workspaces-set-tab-root (cdr spec)))
      (tab-bar-rename-tab (car spec)))))

(ert-deftest edmacs-workspaces-test-move-tab-to-group-leaves-an-ungrouped-tab-alone ()
  "A tab with no root at all derives no group, so there is nothing to move
it toward and it stays put -- rather than taking the new-group branch and
being shoved to the end. Batch's own tab and the daemon's boot tab are
exactly this tab."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (edmacs-workspaces-test--build-tabs
       (cons "t1" (edmacs-workspaces-test--dir "repoA__worktrees/a"))
       (cons "t2" (edmacs-workspaces-test--dir "repoA__worktrees/b"))
       (cons "t3" nil))
      (let ((before (edmacs-workspaces-test--tab-names)))
        ;; t3 carries no root, so it has no group to be moved toward.
        (should-not (edmacs-workspaces-test--group-of
                     (edmacs-workspaces-test--current-tab)))
        (edmacs-workspaces-move-tab-to-group)
        (should (equal before (edmacs-workspaces-test--tab-names)))))))

(ert-deftest edmacs-workspaces-test-move-tab-to-group-leaves-a-settled-tab-alone ()
  "A tab already inside its own group's contiguous block is not moved.
The distinct branch from the ungrouped case above: this tab HAS a group,
that group HAS another tab, and it is the bounds test -- not a missing
group -- that declines the move. t2 sits at index 1 with repoA's block
running 0..1, so the whole tab list must come back untouched."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (edmacs-workspaces-test--build-tabs
       (cons "t1" (edmacs-workspaces-test--dir "repoA__worktrees/a"))
       (cons "t2" (edmacs-workspaces-test--dir "repoA__worktrees/b"))
       (cons "t3" (edmacs-workspaces-test--dir "repoB")))
      (should (equal (mapcar #'edmacs-workspaces-test--group-of (tab-bar-tabs))
                     '("repoA" "repoA" "repoB")))
      (let ((before (edmacs-workspaces-test--tab-names)))
        (edmacs-workspaces-move-tab-to-group (nth 1 (tab-bar-tabs)))
        (should (equal before (edmacs-workspaces-test--tab-names)))))))

(ert-deftest edmacs-workspaces-test-move-tab-to-group-appends-a-brand-new-group ()
  "A tab whose group has no other tab on the frame goes to the END of the
bar, via the -1 sentinel `tab-bar-move-tab-to' reads as \"last\".
This is the commonest production path of all -- every `SPC p p' or
`SPC T p' opening the first tab of a project -- and it is the one branch
whose absence would leave a new project's tab dropped into the middle of
someone else's group."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (edmacs-workspaces-test--build-tabs
       (cons "t1" (edmacs-workspaces-test--dir "repoA__worktrees/a"))
       (cons "new" (edmacs-workspaces-test--dir "repoB"))
       (cons "t2" (edmacs-workspaces-test--dir "repoA__worktrees/b")))
      ;; "new" is the only repoB tab, and it currently splits repoA in two.
      (should (equal (mapcar #'edmacs-workspaces-test--group-of (tab-bar-tabs))
                     '("repoA" "repoB" "repoA")))
      (edmacs-workspaces-move-tab-to-group (nth 1 (tab-bar-tabs)))
      (should (equal (edmacs-workspaces-test--tab-names) '("t1" "t2" "new"))))))

(ert-deftest edmacs-workspaces-test-move-tab-to-group-pulls-a-tab-forward ()
  "A tab sitting BEFORE its group's block is pulled up against that block.
The mirror of `-move-tab-to-group-adjacency''s move-backward case: t1 is
repoA at index 0 while repoA's other tab sits at index 2, so t1 moves
forward to the group's near edge instead of the group moving to it."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (edmacs-workspaces-test--build-tabs
       (cons "t1" (edmacs-workspaces-test--dir "repoA__worktrees/a"))
       (cons "t2" (edmacs-workspaces-test--dir "repoB"))
       (cons "t3" (edmacs-workspaces-test--dir "repoA__worktrees/b"))
       (cons "t4" (edmacs-workspaces-test--dir "repoB__worktrees/x")))
      (should (equal (mapcar #'edmacs-workspaces-test--group-of (tab-bar-tabs))
                     '("repoA" "repoB" "repoA" "repoB")))
      (edmacs-workspaces-move-tab-to-group (nth 0 (tab-bar-tabs)))
      (should (equal (edmacs-workspaces-test--tab-names) '("t2" "t1" "t3" "t4"))))))

;; ============================================================================
;; find-tab / select-tab across two (group, root) pairs
;; ============================================================================

(ert-deftest edmacs-workspaces-test-find-and-select-tab ()
  (edmacs-workspaces-test--with-scratch-tabs
    (edmacs-workspaces-set-tab-root "/root/a/")
    (tab-bar-new-tab)
    (edmacs-workspaces-set-tab-root "/root/b/")
    (should-not (edmacs-workspaces-find-tab "/root/c/"))
    (should (edmacs-workspaces-find-tab "/root/a/"))
    (should (edmacs-workspaces-find-tab "/root/b/"))
    (edmacs-workspaces-select-tab "/root/a/")
    (should (equal (edmacs-workspaces-tab-root
                    (tab-bar--current-tab-find nil (selected-frame)))
                   "/root/a/"))))

(ert-deftest edmacs-workspaces-test-find-tab-is-keyed-on-root-alone ()
  "AC4's lookup half: a stale stored `group' cannot hide a tab from
`find-tab'. Under the old (GROUP, ROOT) key it did, and `open-worktree'
then created a second tab for a worktree already open."
  (edmacs-workspaces-test--with-scratch-tabs
    (edmacs-workspaces-set-tab-root "/root/a/")
    (let ((tab (edmacs-workspaces-test--current-tab)))
      ;; What `M-x tab-group' still writes, and what `SPC T n' used to
      ;; leave behind: a `group' entry saying something else entirely.
      (setf (alist-get 'group (cdr tab)) "bogus")
      (should (eq tab (edmacs-workspaces-find-tab "/root/a/")))
      (should (eq tab (edmacs-workspaces-select-tab "/root/a/"))))))

(ert-deftest edmacs-workspaces-test-no-two-argument-find-tab-call-sites ()
  "The rekey has to be complete: a leftover `(find-tab GROUP ROOT)' call
would silently pass GROUP as the root and match nothing at all."
  (dolist (file (edmacs-workspaces-test--module-files))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              "(edmacs-workspaces-\\(?:find\\|select\\)-tab\\_>" nil t)
        (let ((args (save-excursion
                      (goto-char (match-beginning 0))
                      (let ((end (progn (forward-sexp) (point))))
                        (buffer-substring (match-end 0) (1- end))))))
          ;; ROOT plus an optional FRAME: never three.
          (should (equal (list file args)
                         (list file
                               (if (< (length (split-string args nil t)) 3)
                                   args
                                 "TOO MANY ARGUMENTS")))))))))

(ert-deftest edmacs-workspaces-test-select-tab-nil-when-no-match ()
  (edmacs-workspaces-test--with-scratch-tabs
    (should-not (edmacs-workspaces-select-tab "/no/such/root/"))))

;; ============================================================================
;; Group / tabs-in-group enumeration excludes ungrouped tabs
;; ============================================================================

(ert-deftest edmacs-workspaces-test-groups-excludes-ungrouped-tabs ()
  "A tab with no ROOT has no group: the group IS a function of the root."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      ;; The single default tab from the fixture carries no root at all.
      (should-not (edmacs-workspaces-groups))
      (tab-bar-new-tab)
      (edmacs-workspaces-set-tab-root (edmacs-workspaces-test--dir "repoA"))
      (should (equal (edmacs-workspaces-groups) '("repoA"))))))

(ert-deftest edmacs-workspaces-test-tabs-in-group-excludes-ungrouped ()
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (tab-bar-new-tab)
      (edmacs-workspaces-set-tab-root (edmacs-workspaces-test--dir "repoA"))
      (should (= 1 (length (edmacs-workspaces-tabs-in-group "repoA"))))
      (should-not (edmacs-workspaces-tabs-in-group nil)))))

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
     (edmacs-workspaces-clear-group-memo)
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
                        (file-name-directory (directory-file-name common))))))
                   ;; The fixture's worktrees are plain directories, so the
                   ;; real `vc-git-root' finds no `.git' to walk up to.
                   ;; Resolve to the fixture's own worktree level --
                   ;; `<tmp>/repoA/' or `<tmp>/repoA__worktrees/<slug>/' --
                   ;; so a SUBDIRECTORY normalizes the way it does on disk.
                   ;; A Lisp function, not a subr: still no trampoline.
                   ((symbol-function 'vc-git-root)
                    (lambda (dir)
                      (let* ((rel (file-relative-name
                                   (file-truename dir)
                                   edmacs-workspaces-test--tmp))
                             (parts (and (not (equal rel "."))
                                         (split-string rel "/" t))))
                        (when (and parts (not (string-prefix-p ".." (car parts))))
                          (let ((depth (if (string-suffix-p "__worktrees" (car parts))
                                           2 1)))
                            (when (>= (length parts) depth)
                              (file-name-as-directory
                               (expand-file-name
                                (mapconcat #'identity (seq-take parts depth) "/")
                                edmacs-workspaces-test--tmp)))))))))
           ,@body)
       (edmacs-workspaces-clear-group-memo)
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
          (edmacs-workspaces-select-tab main)
          (edmacs-workspaces-open-worktree wt)
          (should (= count (length (tab-bar-tabs))))
          (should (equal (edmacs-workspaces-tab-root (edmacs-workspaces-test--current-tab))
                         wt)))))))

(ert-deftest edmacs-workspaces-test-open-worktree-groups-by-root ()
  "A worktree tab is found by its root and files under its own repo's group.
There is no such thing as \"the same root in another project's group\"
any more: `edmacs-workspaces-tab-group' derives the group FROM the root,
so a root determines exactly one group."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x"))
            (other (edmacs-workspaces-test--dir "repoB")))
        (edmacs-workspaces-open-worktree wt)
        (should (edmacs-workspaces-find-tab wt))
        (should-not (edmacs-workspaces-find-tab other))
        (should (member (edmacs-workspaces-find-tab wt)
                        (edmacs-workspaces-tabs-in-group "repoA")))
        (should-not (member (edmacs-workspaces-find-tab wt)
                            (edmacs-workspaces-tabs-in-group "repoB")))))))

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
;; The call sites and hook slots this module owns
;; ============================================================================

(ert-deftest edmacs-workspaces-test-entry-points-are-bound ()
  (should (commandp 'edmacs-workspaces-open-project))
  (should (commandp 'edmacs-workspaces-open-worktree)))

(ert-deftest edmacs-workspaces-test-no-frames-open-call-sites ()
  "The call-site grep, as an assertion: nothing opens a repo FRAME."
  (dolist (file '("init.el" "modules/core.el" "modules/sessions.el"
                  "modules/sidebar.el" "modules/sidebar-agents.el"
                  "modules/workspaces.el"))
    (with-temp-buffer
      (insert-file-contents (edmacs-workspaces-test--repo-file file))
      (goto-char (point-min))
      (should-not (search-forward
                   (concat edmacs-workspaces-test--retired-prefix "open") nil t))))
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

(ert-deftest edmacs-workspaces-test-owns-the-tab-and-window-hooks ()
  "This module owns the two hooks the frames model used to share with it:
the post-open stamper and the stray-visit sweep. The frames model's own
post-select root repair has no counterpart here -- a tab reaching the
session unstamped is repaired once, by `stamp-frame-tabs' at restore
time, not on every tab switch."
  (should (memq #'edmacs-workspaces--on-tab-post-open tab-bar-tab-post-open-functions))
  (should (memq #'edmacs-workspaces--on-window-buffer-change
                window-buffer-change-functions))
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (edmacs-workspaces-test--repo-file "modules/workspaces.el"))
                  (buffer-string))))
    (should-not (string-match-p
                 (regexp-quote "(add-hook 'tab-bar-tab-post-select-functions")
                 source))
    (should-not (string-match-p
                 (regexp-quote "(add-hook 'delete-frame-functions") source))))

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
          (should (edmacs-workspaces-find-tab wt))
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
        (edmacs-workspaces-select-tab a)
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
                (edmacs-workspaces-select-tab a)
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
        (edmacs-workspaces-select-tab a)
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
              (edmacs-workspaces-select-tab b)
              (should (memq stray-b (mapcar #'window-buffer (window-list nil 'never))))
              (edmacs-workspaces-select-tab c)
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
          ;; The fixture's `--repo-of' names every top-level directory as
          ;; its own repo, so all three roots derive a real group.
          ;; Grouped, because only a project tab is ever a relocation
          ;; target -- and a stamped root is now all it takes.
          (edmacs-workspaces-set-tab-root outer)
          (tab-bar-new-tab)
          (edmacs-workspaces-set-tab-root inner)
          (tab-bar-new-tab)
          (edmacs-workspaces-set-tab-root other)
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
  "No reconciliation, no tab closing, no group write in the hook.
Designating a main window and running the seam are the only additions;
neither reconciles nor closes anything."
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
                           "seq-find" "tab-bar-change-tab-group"))
        (should-not (string-match-p (regexp-quote forbidden) body))))))

(ert-deftest edmacs-workspaces-test-stray-sweep-ignores-ungrouped-tab ()
  "The frame's original ungrouped tab is never a relocation target.
Its root is whatever directory the frame started in -- typically an
ancestor of everything -- so a visit to a file outside every open
worktree must be left alone, not dragged onto it."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let* ((boot (file-name-as-directory (file-truename (temporary-file-directory))))
             (loose (edmacs-workspaces-test--dir "loose"))
             (wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x"))
             (buf nil))
        ;; The daemon's boot tab: a LIVE directory outside every repo, so
        ;; it derives no group at all -- and it contains the whole
        ;; fixture, which is exactly the "ancestor of everything" shape
        ;; that made excluding it necessary.
        (edmacs-workspaces-set-tab-root boot)
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
`window-buffer-change-functions' fires mid-redisplay. The scheduled
function is this module's own thin wrapper
(`edmacs-workspaces--run-stray-sweep', which clears the pending-timer
bookkeeping before sweeping -- see `edmacs-workspaces--stray-sweep-timers')
rather than `--relocate-stray-visits' directly; invoking it confirms the
sweep still reaches that function."
  (should (memq #'edmacs-workspaces--on-window-buffer-change
                window-buffer-change-functions))
  (remhash (selected-frame) edmacs-workspaces--stray-sweep-timers)
  (unwind-protect
      (let ((scheduled '()) (relocated '()))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (secs repeat fn &rest args)
                     (push (list secs repeat fn args) scheduled)
                     'edmacs-workspaces-test--fake-timer))
                  ((symbol-function 'edmacs-workspaces--relocate-stray-visits)
                   (lambda (frame) (push frame relocated))))
          (edmacs-workspaces--on-window-buffer-change (selected-frame))
          (should (= 1 (length scheduled)))
          (pcase-let ((`(,secs ,repeat ,fn ,args) (car scheduled)))
            (should (equal secs 0))
            (should-not repeat)
            (should (eq fn #'edmacs-workspaces--run-stray-sweep))
            (apply fn args)
            (should (equal relocated (list (selected-frame)))))))
    (remhash (selected-frame) edmacs-workspaces--stray-sweep-timers)))

(ert-deftest edmacs-workspaces-test-sweep-coalesces-a-burst-into-one-timer ()
  "N firings of `window-buffer-change-functions' for the SAME frame within
one command loop queue exactly one pending sweep timer, not N -- the
rolling-debounce fix for the bug the phase context names: an unconditional
fresh zero-delay timer per firing, with no pending-timer guard at all."
  (remhash (selected-frame) edmacs-workspaces--stray-sweep-timers)
  (unwind-protect
      (let ((scheduled 0) (canceled 0))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _) (setq scheduled (1+ scheduled))
                     (make-symbol (format "fake-timer-%d" scheduled))))
                  ((symbol-function 'cancel-timer)
                   (lambda (_timer) (setq canceled (1+ canceled)))))
          (dotimes (_ 5)
            (edmacs-workspaces--on-window-buffer-change (selected-frame)))
          (should (= 5 scheduled))
          (should (= 4 canceled))
          (should (gethash (selected-frame) edmacs-workspaces--stray-sweep-timers))))
    (remhash (selected-frame) edmacs-workspaces--stray-sweep-timers)))

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
;; the frames model's legacy `edmacs-root', with a window state holding the
;; sidebar side window and bufferlo's own `bufferlo-buffer-list' entry.
;; It is a sanitized literal, never the real file: the real one's
;; `environment' parameter dumps the whole shell environment, tokens
;; included. Paths are synthetic (`/w/...') so `file-truename' resolves
;; no symlink and needs nothing on disk.

(defmacro edmacs-workspaces-test--clearing-group-memo (&rest body)
  "Run BODY with `edmacs-workspaces--group-memo' cleared on both edges.
Every fixture that stubs git resolution needs this: a group name is
derived from a root through whichever stub was in force when it was
first asked for, and `edmacs-workspaces-tab-group' memoizes the answer
per root. Without the clear, one fixture's repo layout is served to the
next fixture's identically named roots."
  (declare (indent 0))
  `(unwind-protect
       (progn (edmacs-workspaces-clear-group-memo) ,@body)
     (edmacs-workspaces-clear-group-memo)))

(defmacro edmacs-workspaces-test--with-stub-git (&rest body)
  "Run BODY with git resolution stubbed onto the `/w/<repo>/' fixture layout.
Group derivation must go through the same `edmacs-workspaces-group-name'
the runtime open paths use, or the migrated group strings would not be
`equal' to the ones a later reopen computes."
  (declare (indent 0))
  `(edmacs-workspaces-test--clearing-group-memo
    (cl-letf (((symbol-function 'edmacs-git-common-dir)
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
      ,@body)))

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

(defun edmacs-workspaces-test--tab-ws (buffer-name &optional prev-names)
  "Return a tab `ws' value showing BUFFER-NAME in its `edmacs-main' leaf.
`window-state-get's RAW return shape: a `(CONSTRAINTS-ALIST . TREE)'
cons, which is what tab-bar.el stores and therefore what
`edmacs-workspaces--root-from-ws' is handed. Distinct from
`edmacs-workspaces-test--window-state', which is the flat sanitized
literal the desktop-migration tests assert bufferlo entries against and
carries no such cons.

The sidebar side window comes first, so the leaf the derivation picks is
genuinely the marked one rather than merely the first."
  (cons '((min-height . 4) (min-width . 10))
        `(hc (leaf (parameters (window-side . left) (window-slot . 0))
                   (buffer "*sidebar*" (selected) (point . 1) (start . 1)))
             (leaf (parameters (edmacs-main . t))
                   (buffer ,buffer-name (selected . t) (point . 1) (start . 1))
                   (prev-buffers ,@(mapcar (lambda (n) (list n 1 1))
                                           prev-names))))))

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

(defun edmacs-workspaces-test--tab-groups (tabs)
  "Return TABS' DERIVED group names, nils dropped.
The migrated tabs carry no stored `group' at all any more, so every
assertion about grouping has to go through `tab-bar-tab-group-function'
-- which is exactly what the sidebar and the tab bar themselves read."
  (delq nil (mapcar (lambda (tab) (funcall tab-bar-tab-group-function (cdr tab)))
                    tabs)))

(ert-deftest edmacs-workspaces-test-migrate-frames-model-fixture ()
  "AC2: the real desktop's shape converts with no project or worktree lost.
Every tab keeps its worktree, derives its project group from it, and
swaps the legacy `edmacs-root' for this module's own parameter -- a
migration that reshaped the frames but left the old name in place would
restore tabs the new model cannot read at all."
  (edmacs-workspaces-test--with-stub-git
    (let* ((out (edmacs-workspaces-migrate-frameset
                 (edmacs-workspaces-test--frames-model-fixture)))
           (tabs (edmacs-workspaces-test--migrated-tabs out)))
      (should (= 1 (length (frameset-states out))))
      (should (= 2 (length tabs)))
      (should (equal (edmacs-workspaces-test--tab-groups tabs)
                     '("edmacs" "cloudcitydotgay")))
      ;; The stored entry is gone entirely: the group is derived now.
      (should-not (edmacs-workspaces-test--tab-values tabs 'group))
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

(ert-deftest edmacs-workspaces-test-migrate-leaves-a-rootless-tab-ungrouped ()
  "A tab with no root of any kind belongs to no project. The daemon's boot
tab is exactly that tab; filing it under a frame-wide group rendered it
as a phantom worktree row inside that project's tree in the sidebar.
There is no frame-wide group left to graft: the group is derived from
the tab's own root, and a rootless tab has none."
  (let* ((rootless (list 'tab (cons 'name "*sidebar*") (list 'explicit-name)))
         (out (edmacs-workspaces--migrate-tab rootless)))
    (should (null (alist-get 'group (cdr out))))
    (should (null (funcall tab-bar-tab-group-function (cdr out))))
    (should (null (alist-get edmacs-workspaces-root-parameter (cdr out))))
    ;; Still a fixed point on its own output.
    (should (equal out (edmacs-workspaces--migrate-tab out)))))

(ert-deftest edmacs-workspaces-test-migrate-still-groups-a-legacy-rooted-tab ()
  "The rootless carve-out above must not disarm the migration itself: a tab
carrying only the LEGACY root parameter is what the frames model saved,
and it still has to come back with this module's root -- and therefore
with a derived group."
  (edmacs-workspaces-test--clearing-group-memo
  (let* ((legacy (list 'tab (cons 'name "old")
                       (cons edmacs-workspaces--legacy-root-parameter
                             (expand-file-name default-directory))))
         (out (edmacs-workspaces--migrate-tab legacy)))
    (should (funcall tab-bar-tab-group-function (cdr out)))
    (should (alist-get edmacs-workspaces-root-parameter (cdr out)))
    (should (null (alist-get edmacs-workspaces--legacy-root-parameter
                             (cdr out)))))))

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
             (groups (edmacs-workspaces-test--tab-groups
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
  "A root pointing at a removed worktree keeps its tab AND its project.
Git resolution answers nothing for a directory that is not there, and
the frame-level `edmacs-repo' fallback is gone with the frames model, so
what keeps such a tab inside its project is
`edmacs-workspaces--group-name-from-path': the `/w/<repo>/' fixture
roots do not exist on disk, so their own leaf basename names the repo.
Without it the tab would drop out of the sidebar's tree entirely instead
of rendering with `edmacs-sidebar-missing-worktree-face'."
  (edmacs-workspaces-test--clearing-group-memo
    (cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_root) nil)))
      (let* ((out (edmacs-workspaces-migrate-frameset
                   (edmacs-workspaces-test--frames-model-fixture)))
             (tabs (edmacs-workspaces-test--migrated-tabs out)))
        (should (= 2 (length tabs)))
        (should (equal (sort (edmacs-workspaces-test--tab-groups tabs) #'string<)
                       '("cloudcitydotgay" "edmacs")))))))

(ert-deftest edmacs-workspaces-test-migrate-state-without-tabs-keeps-its-layout ()
  "A frame state carrying no `tabs' parameter contributes a synthesized
ungrouped tab rather than losing that frame's whole layout."
  (edmacs-workspaces-test--clearing-group-memo
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
      (should (equal (alist-get 'ws (cdr (car (last tabs)))) ws))))))

;; ----------------------------------------------------------------------------
;; AC3 -- the migration is a fixed point, not a one-shot
;; ----------------------------------------------------------------------------

(ert-deftest edmacs-workspaces-test-migrate-frameset-is-idempotent ()
  "Running it twice is a no-op the second time. It ensures rather than
detects: a \"has edmacs-root\" detector would have to be right about
every legacy desktop file ever written, where ensuring is right by
construction."
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
                                                        (edmacs-workspace-root
                                                         . "/w/edmacs/"))
                                           (tab (name . "cloudcitydotgay")
                                                (time . 1.0)
                                                (edmacs-workspace-root
                                                 . "/w/cloudcitydotgay/")
                                                (ws nil)))
                                     (height . 72))
                                   nil)))))
      (should (equal (edmacs-workspaces-migrate-frameset fs) fs)))))

(ert-deftest edmacs-workspaces-test-migrate-drops-a-stored-group ()
  "AC1: a stored `group' does not survive the migration at all, even when
it AGREES with the root. It is not part of a tab's identity any more, and
leaving one behind would let a later `M-x tab-group' reintroduce exactly
the disagreement this phase removes."
  (edmacs-workspaces-test--with-stub-git
    (let* ((fs (frameset--make
                :version 1 :timestamp '(27294 4191 109240 0)
                :app '(desktop . "208") :name "test"
                :states (list (cons `((last-focus-update . t)
                                      (tabs (current-tab
                                             (name . "edmacs")
                                             (group . "edmacs")
                                             (edmacs-workspace-root . "/w/edmacs/"))))
                                    nil))))
           (tabs (edmacs-workspaces-test--migrated-tabs
                  (edmacs-workspaces-migrate-frameset fs))))
      (should-not (edmacs-workspaces-test--tab-values tabs 'group))
      (should (equal (edmacs-workspaces-test--tab-groups tabs) '("edmacs"))))))

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

;; ============================================================================
;; Phase 6 -- the root is a tab's whole identity
;; ============================================================================

(ert-deftest edmacs-workspaces-test-tab-group-is-derived-from-the-root ()
  "`tab-bar-tab-group-function' is this module's, and it reads the ROOT.
A stored `group' saying otherwise is inert -- that disagreement is the
state this phase removes."
  (should (eq tab-bar-tab-group-function #'edmacs-workspaces-tab-group))
  (edmacs-workspaces-test--with-stub-git
    (let ((tab '(tab (edmacs-workspace-root . "/w/edmacs__worktrees/roadmap-x/")
                     (group . "cloudcitydotgay"))))
      (should (equal (edmacs-workspaces-tab-group tab) "edmacs"))
      (should (equal (funcall tab-bar-tab-group-function tab) "edmacs")))
    ;; A tab with no root has no group at all.
    (should-not (edmacs-workspaces-tab-group '(tab (name . "boot"))))))

(ert-deftest edmacs-workspaces-test-group-memo-is-per-root-and-clearable ()
  "The memo answers once per ROOT and is a pure function of it -- so the
derivation must not run twice for the same root, and clearing it must
make the next call derive again."
  (edmacs-workspaces-test--clearing-group-memo
    (let ((calls 0))
      (cl-letf (((symbol-function 'edmacs-git-common-dir)
                 (lambda (_root) (setq calls (1+ calls)) "/w/edmacs/.git"))
                ((symbol-function 'edmacs-git-common-dir-repo-name)
                 (lambda (_common) "edmacs")))
        (should (equal (edmacs-workspaces--group-name-memoized "/w/edmacs/") "edmacs"))
        (should (equal (edmacs-workspaces--group-name-memoized "/w/edmacs/") "edmacs"))
        (should (= calls 1))
        ;; A negative answer is memoized too, or every ungrouped tab pays
        ;; the derivation on every redisplay.
        (edmacs-workspaces-clear-group-memo)
        (setq calls 0))
      (let ((live (file-name-as-directory (temporary-file-directory))))
        (cl-letf (((symbol-function 'edmacs-git-common-dir)
                   (lambda (_root) (setq calls (1+ calls)) nil)))
          (should-not (edmacs-workspaces--group-name-memoized live))
          (should-not (edmacs-workspaces--group-name-memoized live))
          (should (= calls 1)))))))

(ert-deftest edmacs-workspaces-test-group-name-path-fallback ()
  "With git resolution answering nothing: an rdm worktree path still names
its repo, a MISSING directory falls back to its own leaf, and a LIVE
non-repo directory stays ungrouped -- the daemon's boot tab is stamped
with a real home directory and must not render as a phantom project."
  (edmacs-workspaces-test--clearing-group-memo
    (cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_root) nil)))
      (should (equal (edmacs-workspaces-group-name "/w/edmacs__worktrees/roadmap-x/")
                     "edmacs"))
      (should (equal (edmacs-workspaces-group-name "/w/gone-repo/") "gone-repo"))
      ;; A directory that really is there.
      (should-not (edmacs-workspaces-group-name (file-name-as-directory
                                                 (temporary-file-directory))))
      ;; Never stat a remote path from the group function's hot path.
      (should-not (edmacs-workspaces-group-name "/ssh:host:/srv/app/")))))

(ert-deftest edmacs-workspaces-test-migrate-rewrites-a-group-that-disagrees-with-its-root ()
  "AC1 end to end: a tab whose stored group names another project comes
out of the migration carrying no stored group, deriving the RIGHT one,
findable by root, and bucketed under the root's project -- the exact
bucketing `edmacs-sidebar--plan-projects' does."
  (edmacs-workspaces-test--with-stub-git
    (let* ((root "/w/edmacs__worktrees/roadmap-x/")
           (fs (frameset--make
                :version 1 :timestamp '(27294 4191 109240 0)
                :app '(desktop . "208") :name "test"
                :states (list (cons `((last-focus-update . t)
                                      (tabs (current-tab
                                             (name . "roadmap-x")
                                             ;; The `SPC T n' bug: the tab
                                             ;; inherited the originating
                                             ;; tab's group, then was
                                             ;; stamped with its own root.
                                             (group . "cloudcitydotgay")
                                             (edmacs-workspace-root . ,root))))
                                    nil))))
           (tabs (edmacs-workspaces-test--migrated-tabs
                  (edmacs-workspaces-migrate-frameset fs)))
           (tab (car tabs)))
      (should (= 1 (length tabs)))
      (should-not (alist-get 'group (cdr tab)))
      (should (equal (funcall tab-bar-tab-group-function (cdr tab)) "edmacs"))
      (edmacs-workspaces-test--with-scratch-tabs
        (set-frame-parameter nil 'tabs tabs)
        (should (eq (car tabs) (edmacs-workspaces-find-tab root)))
        (should (member (car tabs) (edmacs-workspaces-tabs-in-group "edmacs")))
        (should-not (member (car tabs)
                            (edmacs-workspaces-tabs-in-group "cloudcitydotgay")))))))

(ert-deftest edmacs-workspaces-test-open-worktree-finds-a-misgrouped-tab ()
  "AC4: `open-worktree' on a tab poisoned with a bogus stored group
switches to it rather than creating a second tab for the same worktree."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((main (edmacs-workspaces-test--dir "repoA"))
            (wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x")))
        (edmacs-workspaces-open-project main)
        (edmacs-workspaces-open-worktree wt)
        (setf (alist-get 'group (cdr (edmacs-workspaces-test--current-tab))) "bogus")
        (edmacs-workspaces-select-tab main)
        (let ((count (length (tab-bar-tabs))))
          (edmacs-workspaces-open-worktree wt)
          (should (= count (length (tab-bar-tabs))))
          (should (equal (edmacs-workspaces-tab-root
                          (edmacs-workspaces-test--current-tab))
                         wt)))))))

(ert-deftest edmacs-workspaces-test-stamp-frame-tabs-never-selects-a-tab ()
  "AC2: a three-tab frame is stamped with `tab-bar-select-tab' fatal.
The current tab derives from its live window; the two background tabs
derive from their own serialized `ws' main leaf, which is the whole
point -- selecting each in turn ran two tab-select repair advices, a
sidebar redraw and a stray-sweep timer per tab, on the boot path."
  (edmacs-workspaces-test--with-repos
    (let* ((frame (selected-frame))
           (saved (frame-parameter frame 'tabs))
           (a (edmacs-workspaces-test--dir "repoA"))
           (b (edmacs-workspaces-test--dir "repoB"))
           (c (edmacs-workspaces-test--dir "repoC"))
           (buf-b nil) (buf-c nil))
      (unwind-protect
          (progn
            (setq buf-b (get-buffer-create "ws-bg-b")
                  buf-c (get-buffer-create "ws-bg-c"))
            (with-current-buffer buf-b (setq-local default-directory b))
            (with-current-buffer buf-c (setq-local default-directory c))
            (let ((tabs (list (list 'current-tab (cons 'name "a"))
                              (list 'tab (cons 'name "b")
                                    (cons 'ws (edmacs-workspaces-test--tab-ws "ws-bg-b")))
                              (list 'tab (cons 'name "c")
                                    (cons 'ws (edmacs-workspaces-test--tab-ws "ws-bg-c"))))))
              (set-frame-parameter frame 'tabs tabs)
              (cl-letf (((symbol-function 'edmacs-workspaces--derive-frame-root)
                         (lambda (_frame) a))
                        ((symbol-function 'tab-bar-select-tab)
                         (lambda (&rest _) (error "tab-bar-select-tab called"))))
                (edmacs-workspaces-stamp-frame-tabs frame))
              (should (equal (mapcar #'edmacs-workspaces-tab-root tabs)
                             (list a b c)))))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))
        (when (buffer-live-p buf-c) (kill-buffer buf-c))
        (set-frame-parameter frame 'tabs saved)))))

(ert-deftest edmacs-workspaces-test-root-from-ws-uses-the-desktop-args-list ()
  "`desktop-restore-eager' is 10, so at `desktop-after-read-hook' time most
restored buffers do not exist yet: a `get-buffer'-only derivation would
answer nil for nearly every background tab. The pending-restore list is
the fallback that makes the migration actually stamp them."
  (edmacs-workspaces-test--with-repos
    (let* ((root (edmacs-workspaces-test--dir "repoA"))
           (file (expand-file-name "notes.org" root))
           (ws (edmacs-workspaces-test--tab-ws "notes.org")))
      (should-not (get-buffer "notes.org"))
      ;; No live buffer and no pending entry: nothing to derive from.
      (let ((desktop-buffer-args-list nil))
        (should-not (edmacs-workspaces--root-from-ws ws)))
      (let ((desktop-buffer-args-list (list (list file "notes.org" 'org-mode))))
        (should (equal (edmacs-workspaces--root-from-ws ws) root))))))

(ert-deftest edmacs-workspaces-test-root-from-ws-fails-soft ()
  "Nothing derivable is nil, never a signal: this runs under
`desktop-after-read-hook', where an error reaching a frameless daemon's
top level exits it 255."
  (let ((desktop-buffer-args-list nil))
    (should-not (edmacs-workspaces--root-from-ws nil))
    (should-not (edmacs-workspaces--root-from-ws '(nil)))
    (should-not (edmacs-workspaces--root-from-ws (cons nil '(leaf))))
    ;; Names that resolve to no live buffer and no pending restore.
    (should-not (edmacs-workspaces--root-from-ws
                 (edmacs-workspaces-test--tab-ws "no-such-buffer")))
    ;; The whole (CONSTRAINTS . TREE) cons passed where the TREE belongs
    ;; reads as "nothing derivable" rather than as an error.
    (should-not (edmacs-workspaces--root-from-ws
                 (cons nil (edmacs-workspaces-test--tab-ws "no-such-buffer"))))))

(ert-deftest edmacs-workspaces-test-source-has-no-select-loop ()
  "AC2 asserted on the source, not just the behaviour: `stamp-frame-tabs'
must not name `tab-bar-select-tab' at all."
  (let* ((source (with-temp-buffer
                   (insert-file-contents
                    (edmacs-workspaces-test--repo-file "modules/workspaces.el"))
                   (buffer-string)))
         (start (string-match
                 (regexp-quote "(defun edmacs-workspaces-stamp-frame-tabs") source))
         (end (string-match "^(defun \\|^;; =====" source (1+ start)))
         (body (substring source start end)))
    (should-not (string-match-p (regexp-quote "tab-bar-select-tab") body))
    (should-not (string-match-p (regexp-quote "dotimes") body))))

(ert-deftest edmacs-workspaces-test-owns-the-only-tab-post-open-entry ()
  "AC3: exactly one `add-hook' on core's post-open variable anywhere under
modules/, and it is this module's. The three independent entries this
replaced ran in an `add-hook'-prepending order nobody chose, and the
sidebar's redraw won it -- so the first tree drawn for a new tab saw an
unstamped tab."
  (let ((sites '()))
    (dolist (file (edmacs-workspaces-test--module-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (search-forward "(add-hook 'tab-bar-tab-post-open-functions" nil t)
          (push (file-name-nondirectory file) sites))))
    (should (equal sites '("workspaces.el"))))
  ;; Only this config's own entries are counted: a real session also
  ;; carries bufferlo's `bufferlo--tab-include-exclude-buffers', which is
  ;; a third-party package's and not ours to collapse.
  (should (equal (seq-filter (lambda (f) (string-prefix-p "edmacs-" (symbol-name f)))
                             tab-bar-tab-post-open-functions)
                 (list #'edmacs-workspaces--on-tab-post-open))))

(ert-deftest edmacs-workspaces-test-new-tab-is-stamped-before-the-seam-runs ()
  "AC3's ordering guarantee: by the time a
`edmacs-workspaces-tab-post-open-functions' member runs, the new tab
carries its root and the frame carries a designated main window."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let* ((wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x"))
             (seen '())
             (probe (lambda (tab frame)
                      (push (list (edmacs-workspaces-tab-root tab)
                                  (funcall tab-bar-tab-group-function tab)
                                  (and (edmacs-windows-main-window-of frame) t)
                                  (eq frame (selected-frame)))
                            seen))))
        (let ((edmacs-workspaces-tab-post-open-functions (list probe)))
          (edmacs-workspaces-open-worktree wt))
        (should (= 1 (length seen)))
        (should (equal (car seen) (list wt "repoA" t t)))))))

;; ============================================================================
;; Frame eligibility -- edmacs-workspaces-frame-usable-p
;; ============================================================================
;; Fake frames are plain symbols; `frame-list', `frame-live-p' and
;; `frame-parameter' are stubbed to treat them as an alist of parameters,
;; so no real frame is ever created here. `daemonp' reports non-nil
;; throughout, so the daemon-placeholder exclusion is the shape actually
;; exercised rather than short-circuited.

(defvar edmacs-workspaces-test--frames nil
  "Alist of (FRAME-SYMBOL . PARAMS) the faked frame primitives read.")

(defun edmacs-workspaces-test--fake-param (frame key default)
  "Return FRAME's fake KEY parameter, or DEFAULT when it carries none.
A nil FRAME means the selected frame, matching every real frame
primitive's own optional-FRAME convention."
  (let ((params (cdr (assq (or frame (selected-frame))
                           edmacs-workspaces-test--frames))))
    (if (assq key params) (alist-get key params) default)))

(defmacro edmacs-workspaces-test--with-fake-frames (alist &rest body)
  "Run BODY with frame-scanning primitives faked from ALIST.
ALIST is a list of (FRAME-SYMBOL . PARAMS-ALIST); beyond the parameters
under test, `graphic' drives `display-graphic-p' (default t) and
`initial' drives `frame-initial-p' (default nil)."
  (declare (indent 1))
  `(let ((edmacs-workspaces-test--frames ,alist))
     (cl-letf (((symbol-function 'frame-list)
                (lambda () (mapcar #'car edmacs-workspaces-test--frames)))
               ((symbol-function 'frame-live-p)
                (lambda (f) (assq f edmacs-workspaces-test--frames)))
               ((symbol-function 'display-graphic-p)
                (lambda (&optional f) (edmacs-workspaces-test--fake-param f 'graphic t)))
               ((symbol-function 'frame-initial-p)
                (lambda (f) (edmacs-workspaces-test--fake-param f 'initial nil)))
               ((symbol-function 'daemonp) (lambda (&rest _) t))
               ((symbol-function 'frame-parameter)
                (lambda (f param) (edmacs-workspaces-test--fake-param f param nil))))
       ,@body)))

(ert-deftest edmacs-workspaces-test-frame-usable-p-excludes-daemon-placeholder ()
  "The daemon's initial tty frame is never usable while a GUI frame exists.
That is the frame `desktop--check-dont-save' already refuses to save and
that nothing can ever be displayed on."
  (edmacs-workspaces-test--with-fake-frames
      '((gui . ((graphic . t)))
        (f1 . ((graphic . nil) (initial . t))))
    (should (edmacs-workspaces-frame-usable-p 'gui))
    (should-not (edmacs-workspaces-frame-usable-p 'f1))))

(ert-deftest edmacs-workspaces-test-frame-usable-p-excludes-child-frame ()
  "A corfu-style popup is a frame by construction, never one to drive."
  (edmacs-workspaces-test--with-fake-frames
      '((gui . ((graphic . t)))
        (popup . ((graphic . t) (parent-frame . gui))))
    (should-not (edmacs-workspaces-frame-usable-p 'popup))))

(ert-deftest edmacs-workspaces-test-frame-usable-p-allows-tty-in-a-tty-only-session ()
  "With no graphical frame anywhere, a tty frame is still usable.
The disjunct that keeps this repo's own tty-only batch harnesses (and a
genuinely terminal-only Emacs) working."
  (edmacs-workspaces-test--with-fake-frames
      '((tty-a . ((graphic . nil)))
        (tty-b . ((graphic . nil) (initial . t))))
    (should (edmacs-workspaces-frame-usable-p 'tty-a))
    ;; The daemon's own placeholder stays excluded even here.
    (should-not (edmacs-workspaces-frame-usable-p 'tty-b))))

;; ============================================================================
;; edmacs-workspaces-stamp-frame-tabs -- the restore-side stamper
;; ============================================================================

(ert-deftest edmacs-workspaces-test-tab-root-stored-property-wins ()
  (let ((tab '(tab (edmacs-workspace-root . "/stored/root/"))))
    (should (equal (edmacs-workspaces-tab-root tab) "/stored/root/"))))

(ert-deftest edmacs-workspaces-test-tab-root-is-a-pure-read ()
  "An unstamped tab reads nil -- no derivation, and nothing cached onto it."
  (let ((tab (list 'current-tab (cons 'ws '((min-height . 4) (leaf))))))
    (cl-letf (((symbol-function 'window-buffer)
               (lambda (&rest _) (error "a tab root is never derived here"))))
      (should-not (edmacs-workspaces-tab-root tab)))
    (should-not (assq 'edmacs-workspace-root (cdr tab)))))

(ert-deftest edmacs-workspaces-test-stamp-frame-tabs-stamps-only-an-unstamped-tab ()
  "The restore-walk entry point: stamp a tab with no root, leave a
stamped one strictly alone (never re-derive over a stored answer)."
  (let* ((frame (selected-frame))
         (saved (frame-parameter frame 'tabs))
         (bare (list 'current-tab))
         (stamped (list 'current-tab (cons 'edmacs-workspace-root "/kept/"))))
    (unwind-protect
        (cl-letf (((symbol-function 'edmacs-workspaces--derive-frame-root)
                   (lambda (_frame) "/derived/")))
          (set-frame-parameter frame 'tabs (list bare))
          (edmacs-workspaces-stamp-frame-tabs frame)
          (should (equal (alist-get 'edmacs-workspace-root (cdr bare)) "/derived/"))
          (set-frame-parameter frame 'tabs (list stamped))
          (edmacs-workspaces-stamp-frame-tabs frame)
          (should (equal (alist-get 'edmacs-workspace-root (cdr stamped)) "/kept/")))
      (set-frame-parameter frame 'tabs saved))))

(ert-deftest edmacs-workspaces-test-stamp-frame-tabs-replaces-rather-than-shadows ()
  "Stamping must REPLACE the root parameter, not `push' a second cons in
front of it: `tab-bar--tab' copies every unrecognized tab parameter
forward on each switch, so a shadowed stale entry would be duplicated
into the desktop file forever."
  (let* ((frame (selected-frame))
         (saved (frame-parameter frame 'tabs))
         (tab (list 'current-tab (cons 'edmacs-workspace-root "/old/"))))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'tabs (list tab))
          (edmacs-workspaces-set-tab-root "/new/" frame)
          (should (equal (alist-get 'edmacs-workspace-root (cdr tab)) "/new/"))
          (should (= 1 (seq-count (lambda (e) (eq (car-safe e) 'edmacs-workspace-root))
                                  (cdr tab)))))
      (set-frame-parameter frame 'tabs saved))))

(ert-deftest edmacs-workspaces-test-current-tab-root-and-group ()
  "The two public current-tab accessors read the tab, never a buffer --
and the group comes from the ROOT, not from a stored `group' entry
saying something else."
  (edmacs-workspaces-test--with-stub-git
    (let* ((frame (selected-frame))
           (saved (frame-parameter frame 'tabs))
           (tab (list 'current-tab
                      (cons 'edmacs-workspace-root "/w/edmacs__worktrees/roadmap-x/")
                      (cons 'group "cloudcitydotgay"))))
      (unwind-protect
          (progn
            (set-frame-parameter frame 'tabs (list tab))
            (should (equal (edmacs-workspaces-current-tab-root frame)
                           "/w/edmacs__worktrees/roadmap-x/"))
            (should (equal (edmacs-workspaces-current-group frame) "edmacs"))
            (set-frame-parameter frame 'tabs (list (list 'current-tab)))
            (should-not (edmacs-workspaces-current-tab-root frame))
            (should-not (edmacs-workspaces-current-group frame)))
        (set-frame-parameter frame 'tabs saved)))))

(ert-deftest edmacs-workspaces-test-root-survives-frameset-tab-filter ()
  "The desktop half of \"every tab is stamped\": `frameset-filter-tabs'
strips only the `wc' family on save, so a background tab's own
`edmacs-workspace-root' (and its `ws') round-trip through the desktop
file and need no re-derivation on restore."
  (let* ((tabs (list (list 'tab
                           (cons 'name "wt")
                           (cons 'group "repo")
                           (cons 'edmacs-workspace-root "/wt/")
                           (cons 'ws '((min-height . 4) (leaf)))
                           (cons 'wc 'unprintable)
                           (cons 'wc-point 1)
                           (cons 'wc-bl nil))))
         (saved (car (frameset-filter-tabs tabs nil nil t))))
    (should (equal (alist-get 'edmacs-workspace-root saved) "/wt/"))
    (should (equal (alist-get 'group saved) "repo"))
    (should (alist-get 'ws saved))
    (should-not (assq 'wc saved))
    (should-not (assq 'wc-point saved))
    (should-not (assq 'wc-bl saved))))

;; ============================================================================
;; edmacs-workspaces--read-worktree -- the interactive prompt's own plumbing
;; ============================================================================
;; `edmacs-workspaces-open-worktree's other tests call it as a plain
;; function with an explicit DIR, bypassing its `(interactive (list
;; (edmacs-workspaces--read-worktree current-prefix-arg)))' spec -- and
;; `--read-worktree' itself -- entirely.

(ert-deftest edmacs-workspaces-test-read-worktree-prefix-skips-candidate-lookup ()
  "A PREFIX arg falls straight to `read-directory-name', never enumerating
worktrees (which would shell out) at all."
  (cl-letf (((symbol-function 'edmacs-workspaces-worktrees)
             (lambda (&rest _) (error "should not enumerate under a prefix arg")))
            ((symbol-function 'read-directory-name)
             (lambda (&rest _) "/picked/")))
    (should (equal (edmacs-workspaces--read-worktree t) "/picked/"))))

(ert-deftest edmacs-workspaces-test-read-worktree-offers-the-tabs-own-worktrees ()
  "No PREFIX: the candidate list is the CURRENT TAB's own root's worktrees,
not a set derived from whatever buffer happens to be current."
  (let* ((frame (selected-frame))
         (saved (frame-parameter frame 'tabs))
         (tab (list 'current-tab (cons 'edmacs-workspace-root "/repo/wt/"))))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'tabs (list tab))
          (cl-letf (((symbol-function 'edmacs-workspaces-worktrees)
                     (lambda (base)
                       (should (equal base "/repo/wt/"))
                       '("/wt-a/" "/wt-b/")))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (should (equal collection '("/wt-a/" "/wt-b/")))
                       "/wt-a/")))
            (should (equal (edmacs-workspaces--read-worktree nil) "/wt-a/"))))
      (set-frame-parameter frame 'tabs saved))))

;; ============================================================================
;; Fullscreen policy
;; ============================================================================
;; Still no real frame: `display-graphic-p' joins the faked primitives, and
;; the deferring `run-at-time' is stubbed so the timer body can be run
;; synchronously and inspected.

(defmacro edmacs-workspaces-test--with-fullscreen-frames (alist &rest body)
  "Run BODY over fake frames ALIST with `display-graphic-p' faked too.
A fake frame counts as graphical when its params carry a non-nil
`graphic' entry, so the policy's own `display-graphic-p' gate is
exercised without ever opening a GUI frame."
  (declare (indent 1))
  `(edmacs-workspaces-test--with-fake-frames ,alist
     (cl-letf (((symbol-function 'display-graphic-p)
                (lambda (&optional f) (frame-parameter f 'graphic))))
       ,@body)))

(ert-deftest edmacs-workspaces-test-fullscreen-target-for-plain-graphical-frame ()
  (let ((edmacs-workspaces-fullscreen 'fullboth))
    (edmacs-workspaces-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (should (eq (edmacs-workspaces--fullscreen-target 'fa) 'fullboth)))))

(ert-deftest edmacs-workspaces-test-fullscreen-target-nil-when-policy-disabled ()
  (let ((edmacs-workspaces-fullscreen nil))
    (edmacs-workspaces-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (should-not (edmacs-workspaces--fullscreen-target 'fa)))))

(ert-deftest edmacs-workspaces-test-fullscreen-target-nil-for-tty-frame ()
  "The daemon's own tty placeholder and every `emacsclient -t' frame:
`fullscreen' means nothing there and is mangled by frameset's tty
shelving on the way into a desktop file. This gate is the reason the
policy is a hook rather than an entry in `default-frame-alist', which
those frames read too."
  (let ((edmacs-workspaces-fullscreen 'fullboth))
    (edmacs-workspaces-test--with-fullscreen-frames '((f1 . ((graphic . nil))))
      (should-not (edmacs-workspaces--fullscreen-target 'f1)))))

(ert-deftest edmacs-workspaces-test-fullscreen-policy-is-not-in-default-frame-alist ()
  "Pins the decision the test above documents: no `fullscreen' entry may
be added to `default-frame-alist', or the tty frames would inherit it."
  (should-not (assq 'fullscreen default-frame-alist)))

(ert-deftest edmacs-workspaces-test-fullscreen-target-nil-for-child-frame ()
  "A corfu/posframe-style completion popup is a graphical frame by
construction and must keep the size its owner gave it."
  (let ((edmacs-workspaces-fullscreen 'fullboth))
    (edmacs-workspaces-test--with-fullscreen-frames
        '((fa . ((graphic . t)))
          (popup . ((graphic . t) (parent-frame . fa))))
      (should-not (edmacs-workspaces--fullscreen-target 'popup)))))

(ert-deftest edmacs-workspaces-test-fullscreen-target-nil-when-already-there ()
  (let ((edmacs-workspaces-fullscreen 'fullboth))
    (edmacs-workspaces-test--with-fullscreen-frames
        '((fa . ((graphic . t) (fullscreen . fullboth))))
      (should-not (edmacs-workspaces--fullscreen-target 'fa)))
    ;; A frame at some OTHER fullscreen value still needs correcting.
    (edmacs-workspaces-test--with-fullscreen-frames
        '((fa . ((graphic . t) (fullscreen . maximized))))
      (should (eq (edmacs-workspaces--fullscreen-target 'fa) 'fullboth)))))

(ert-deftest edmacs-workspaces-test-fullscreen-target-nil-for-dead-frame ()
  (let ((edmacs-workspaces-fullscreen 'fullboth))
    (edmacs-workspaces-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (should-not (edmacs-workspaces--fullscreen-target 'gone)))))

(ert-deftest edmacs-workspaces-test-apply-fullscreen-defers-then-sets ()
  "Nothing is set inside the creation hook itself -- a frame is not fully
mapped there, and the NS port drops a fullscreen toggle sent to an
unmapped window -- only from the zero-delay timer."
  (let ((edmacs-workspaces-fullscreen 'fullboth)
        (deferred nil) (set-calls nil))
    (edmacs-workspaces-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest _) (setq deferred fn) nil))
                ((symbol-function 'set-frame-parameter)
                 (lambda (f param value) (push (list f param value) set-calls))))
        (edmacs-workspaces-apply-fullscreen 'fa)
        (should deferred)
        (should-not set-calls)
        (funcall deferred)
        (should (equal set-calls '((fa fullscreen fullboth))))))))

(ert-deftest edmacs-workspaces-test-apply-fullscreen-schedules-nothing-when-ineligible ()
  (let ((edmacs-workspaces-fullscreen 'fullboth)
        (scheduled 0))
    (edmacs-workspaces-test--with-fullscreen-frames
        '((f1 . ((graphic . nil)))
          (fa . ((graphic . t) (fullscreen . fullboth))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq scheduled (1+ scheduled)) nil)))
        (edmacs-workspaces-apply-fullscreen 'f1)
        (edmacs-workspaces-apply-fullscreen 'fa)
        (should (= scheduled 0))))))

(ert-deftest edmacs-workspaces-test-apply-fullscreen-rechecks-target-in-timer ()
  "The frame can be deleted -- or reach the target by another route --
between the creation hook and the timer, so the timer body re-checks
instead of setting a parameter on a frame that no longer qualifies."
  (let ((calls 0) (deferred nil) (set-calls nil))
    (cl-letf (((symbol-function 'edmacs-workspaces--fullscreen-target)
               (lambda (_frame) (setq calls (1+ calls)) (and (= calls 1) 'fullboth)))
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest _) (setq deferred fn) nil))
              ((symbol-function 'set-frame-parameter)
               (lambda (&rest args) (push args set-calls))))
      (edmacs-workspaces-apply-fullscreen 'fa)
      (funcall deferred)
      (should (= calls 2))
      (should-not set-calls))))

(ert-deftest edmacs-workspaces-test-apply-fullscreen-warns-instead-of-signalling ()
  "An error out of the timer body would reach a frameless daemon's top
level, which exits Emacs 255 (see core.el); it is warned about instead."
  (let ((edmacs-workspaces-fullscreen 'fullboth)
        (deferred nil) (warnings nil))
    (edmacs-workspaces-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest _) (setq deferred fn) nil))
                ((symbol-function 'set-frame-parameter)
                 (lambda (&rest _) (error "NS refused the toggle")))
                ((symbol-function 'display-warning)
                 (lambda (&rest args) (push args warnings))))
        (edmacs-workspaces-apply-fullscreen 'fa)
        (funcall deferred)
        (should (= 1 (length warnings)))
        (should (eq (car (car warnings)) 'edmacs-workspaces))))))

(ert-deftest edmacs-workspaces-test-fullscreen-startup-covers-every-live-frame ()
  "`after-make-frame-functions' never fires for a non-daemon Emacs's own
initial frame -- the only frame a plain `emacs' start has -- so the
policy is applied from `emacs-startup-hook' as well."
  (let ((applied nil))
    (edmacs-workspaces-test--with-fullscreen-frames
        '((fa . ((graphic . t))) (fb . ((graphic . t))))
      (cl-letf (((symbol-function 'edmacs-workspaces-apply-fullscreen)
                 (lambda (frame) (push frame applied))))
        (edmacs-workspaces--apply-fullscreen-at-startup)
        (should (equal (nreverse applied) '(fa fb)))))))

(ert-deftest edmacs-workspaces-test-fullscreen-is-wired-to-both-hooks ()
  "Membership only, never position: the policy moved from a module that
loaded LAST to one that loads early, which flips its `add-hook' order
relative to sessions.el's own entries. Both defer via `run-at-time 0',
so order is not the contract -- presence is."
  (should (memq #'edmacs-workspaces-apply-fullscreen after-make-frame-functions))
  (should (memq #'edmacs-workspaces--apply-fullscreen-at-startup emacs-startup-hook)))

(ert-deftest edmacs-workspaces-test-fullscreen-symbols-renamed ()
  "The policy carries `edmacs-workspaces-' names now: the old ones would
trip this phase's own no-references-to-the-retired-module gate."
  (dolist (suffix '("fullscreen" "apply-fullscreen" "-fullscreen-target"))
    (let ((sym (intern (concat edmacs-workspaces-test--retired-prefix suffix))))
      (should-not (boundp sym))
      (should-not (fboundp sym)))))

;; ============================================================================
;; The frames model is gone
;; ============================================================================

(defconst edmacs-workspaces-test--retired-prefix (concat "edmacs-" "frames-")
  "The retired module's symbol prefix, spelled in two pieces.
This file enforces \"that prefix appears nowhere under `modules/'\", tests
included -- so it must not contain the literal itself.")

(defconst edmacs-workspaces-test--retired-worktrees-fn
  (concat "edmacs-" "worktrees-for-repo")
  "The retired per-repo worktree enumerator's name, spelled in two pieces.")

(defun edmacs-workspaces-test--module-files (&optional include-tests)
  "Return every `.el' under `modules/', test files excluded unless asked.
`test-support.el' counts as a test file for this purpose despite its
name not ending in \"-test.el\": it is shared test fixture code (see
modules/test-support.el's own Commentary), not a production config
module, so its own `(make-frame ...)' call site (the second-frame-or-skip
fixture) must not count toward the production-code sentinels below."
  (seq-filter
   (lambda (f) (or include-tests
                   (not (or (string-match-p "-test\\.el\\'" f)
                            (string-match-p "/test-support\\.el\\'" f)))))
   (directory-files (edmacs-workspaces-test--repo-file "modules") t "\\.el\\'")))

(ert-deftest edmacs-workspaces-test-retired-modules-are-gone ()
  "The frames model's module and its two suites are retired, not merely
unused."
  (dolist (f '("modules/frames.el" "modules/frames-test.el"
               "modules/frames-live-test.el"))
    (should-not (file-exists-p (edmacs-workspaces-test--repo-file f))))
  (should-not (featurep 'frames))
  (with-temp-buffer
    (insert-file-contents (edmacs-workspaces-test--repo-file "init.el"))
    (goto-char (point-min))
    (should-not (search-forward "(load-module \"frames\")" nil t))))

(ert-deftest edmacs-workspaces-test-no-frames-module-references ()
  "This phase's gate, and it has to be a grep: every cross-module caller
of the retired module was `declare-function'd, which is what suppresses
the byte-compiler's \"not known to be defined\" warning. Test files are
included -- the AC's own grep excludes nothing."
  (let ((pattern (concat edmacs-workspaces-test--retired-prefix "\\|"
                         edmacs-workspaces-test--retired-worktrees-fn)))
    (dolist (file (edmacs-workspaces-test--module-files t))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((text (buffer-string)))
          (should (equal (list file nil)
                         (list file (string-match-p pattern text)))))))))

(ert-deftest edmacs-workspaces-test-no-live-repo-frame-parameter ()
  "`edmacs-repo' survives in exactly one place: the legacy-desktop
migration in this module, which reads it as DATA off a saved frameset.
Every other non-test module must be clean of it."
  (dolist (file (edmacs-workspaces-test--module-files))
    (unless (equal (file-name-nondirectory file) "workspaces.el")
      (with-temp-buffer
        (insert-file-contents file)
        (let ((text (buffer-string)))
          (should (equal (list file nil)
                         (list file (string-match-p "edmacs-repo" text))))))))
  ;; In workspaces.el every occurrence outside a `;;' comment line sits at
  ;; or after the migration's own `--legacy-root-parameter' defconst. The
  ;; Commentary discusses the legacy parameter well above that point, and
  ;; is meant to.
  (with-temp-buffer
    (insert-file-contents (edmacs-workspaces-test--repo-file "modules/workspaces.el"))
    (goto-char (point-min))
    (let ((migration (save-excursion
                       (search-forward "edmacs-workspaces--legacy-root-parameter" nil t))))
      (should migration)
      (while (search-forward "edmacs-repo" nil t)
        (unless (save-excursion
                  (goto-char (line-beginning-position))
                  (looking-at-p "[ \t]*;"))
          (should (> (point) migration)))))))

(ert-deftest edmacs-workspaces-test-single-make-frame-call-site ()
  "No code path creates a frame because a project was opened: the config
has exactly one `make-frame' call site left, sessions.el's GUI-frame
maker, and the frames model's own two frame factories are gone."
  (dolist (suffix '("-make-frame" "-spare-frame"))
    (should-not (fboundp (intern (concat edmacs-workspaces-test--retired-prefix
                                         suffix)))))
  (let ((total 0))
    (dolist (file (append (edmacs-workspaces-test--module-files)
                          (list (edmacs-workspaces-test--repo-file "init.el")
                                (edmacs-workspaces-test--repo-file "early-init.el"))))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (search-forward "(make-frame" nil t)
          (setq total (1+ total)))))
    (should (= total 1))))

(ert-deftest edmacs-workspaces-test-no-worktree-cache-watch-or-timer-anywhere ()
  "The discovery layer is gone from the whole config, not just this
module. The two surviving timers are named explicitly, so this fails on
a NEW one rather than on either of them: sidebar.el's mouse-resize width
debounce and sidebar-agents.el's elapsed-time repeater -- neither a
worktree cache or watch."
  (dolist (file (edmacs-workspaces-test--module-files t))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((text (buffer-string)))
        ;; The API calls that would install one, not the bare word: a
        ;; module's Commentary legitimately discusses their deliberate
        ;; absence, exactly as `-source-has-no-cache-watch-or-timer' notes.
        ;; Spelled in pieces because this file is itself in the scan.
        (dolist (forbidden (list (concat "file-notify" "-add-watch")
                                 (concat "file-notify" "-rm-watch")
                                 (concat "worktrees" "-cache")
                                 (concat "worktree" "-watches")
                                 (concat "worktree" "-refresh-timers")))
          (should (equal (list file forbidden nil)
                         (list file forbidden
                               (string-match-p (regexp-quote forbidden) text))))))))
  (should-not (fboundp (intern edmacs-workspaces-test--retired-worktrees-fn))))

(ert-deftest edmacs-workspaces-test-open-paths-create-no-frame ()
  "Neither entry point makes a frame: a project is a tab group inside the
frame that is already there."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let ((root (edmacs-workspaces-test--dir "repoA"))
            (wt (edmacs-workspaces-test--dir "repoA__worktrees/wt-a"))
            (frames (length (frame-list))))
        ;; `make-frame' is a plain Lisp `defun' in frame.el, so this
        ;; `cl-letf' builds no native-comp subr trampoline.
        (cl-letf (((symbol-function 'make-frame)
                   (lambda (&rest _) (ert-fail "make-frame called"))))
          (edmacs-workspaces-open-project root)
          (edmacs-workspaces-open-worktree wt))
        (should (= frames (length (frame-list))))))))


;; ============================================================================
;; Code-review regressions (2026-09-07)
;; ============================================================================

(ert-deftest edmacs-workspaces-test-open-project-opens-main-beside-a-worktree-tab ()
  "`SPC p p' reaches the MAIN worktree even when a linked worktree tab is open.
The regression: a group that had tabs but no main tab selected
`(car tabs)' instead of creating one, so opening any rdm worktree left
the project's own checkout unreachable from `SPC p p' -- and sent the
sidebar's project row, which names the main worktree, to a sibling."
  (edmacs-workspaces-test--with-repos
    (edmacs-workspaces-test--with-scratch-tabs
      (let* ((main (edmacs-workspaces-test--dir "repoA"))
             (wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x"))
             (group (edmacs-workspaces-group-name main)))
        (edmacs-workspaces-open-worktree wt)
        (should (equal (edmacs-workspaces-tab-root
                        (edmacs-workspaces-test--current-tab))
                       wt))
        (edmacs-workspaces-open-project main)
        ;; The main worktree, not the sibling that happened to be open.
        (should (equal (edmacs-workspaces-tab-root
                        (edmacs-workspaces-test--current-tab))
                       main))
        ;; Opened alongside it, not in place of it.
        (should (= 2 (length (edmacs-workspaces-tabs-in-group group))))))))

(ert-deftest edmacs-workspaces-test-worktree-root-of-normalizes-a-subdirectory ()
  "A buffer's subdirectory resolves to its worktree root, never to itself.
Stamping `.../repoA/modules/' on a tab would make that tab the
longest-prefix match for every buffer under `modules/', so the stray
sweep would drag them all onto it."
  (edmacs-workspaces-test--with-repos
    (let ((main (edmacs-workspaces-test--dir "repoA"))
          (sub (edmacs-workspaces-test--dir "repoA/modules"))
          (wt (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x"))
          (wtsub (edmacs-workspaces-test--dir "repoA__worktrees/roadmap-x/modules")))
      (should (equal (edmacs-workspaces--worktree-root-of sub) main))
      (should (equal (edmacs-workspaces--worktree-root-of main) main))
      ;; A LINKED worktree resolves to itself, not to the main checkout --
      ;; this is what keeps sibling worktrees distinct.
      (should (equal (edmacs-workspaces--worktree-root-of wtsub) wt)))))

(ert-deftest edmacs-workspaces-test-remote-paths-never-reach-the-filesystem ()
  "Neither the sweep nor root derivation may touch a remote path.
Both run from `window-buffer-change-functions' for every buffer in every
window, and a TRAMP round trip there stalls the sweep on the network."
  (should (file-remote-p "/ssh:host:/srv/app/"))
  (should-not (edmacs-workspaces--worktree-root-of "/ssh:host:/srv/app/"))
  (with-temp-buffer
    (setq-local default-directory "/ssh:host:/srv/app/")
    (should-not (edmacs-workspaces--buffer-dir (current-buffer)))))

(provide 'workspaces-test)

;;; workspaces-test.el ends here
