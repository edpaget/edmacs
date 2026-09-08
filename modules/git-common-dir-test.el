;;; git-common-dir-test.el --- Tests for git-common-dir.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage: the memoization contract of
;; `edmacs-git-common-dir' and the string derivations built on top of a
;; resolved git-common-dir (`edmacs-git-common-dir-main-worktree',
;; `edmacs-git-common-dir-repo-name') that `modules/sidebar.el',
;; `modules/claude-term-registry.el' and `modules/workspaces.el' share.
;; No real git process is exercised here; `edmacs-git-common-dir-1' is
;; stubbed or the cache is pre-populated directly.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/git-common-dir-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; See CLAUDE.md's Testing section: `cl-letf' on a C subr forces a
;; synchronous native-comp trampoline build (~28s) the first time it is
;; hit. Defensive here even where no target below is a subr.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

;; ============================================================================
;; edmacs-git-common-dir memoization
;; ============================================================================

(ert-deftest edmacs-git-common-dir-test-caches-hit ()
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
    (puthash "/repo/wt/" "/repo/.git" edmacs-git-common-dir-cache)
    (cl-letf (((symbol-function 'edmacs-git-common-dir-1)
               (lambda (&rest _) (error "should not shell out"))))
      (should (equal (edmacs-git-common-dir "/repo/wt/") "/repo/.git")))))

(ert-deftest edmacs-git-common-dir-test-caches-miss-as-none ()
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
    (puthash "/not-a-repo/" 'none edmacs-git-common-dir-cache)
    (cl-letf (((symbol-function 'edmacs-git-common-dir-1)
               (lambda (&rest _) (error "should not shell out"))))
      (should-not (edmacs-git-common-dir "/not-a-repo/")))))

(ert-deftest edmacs-git-common-dir-test-populates-cache-on-first-call ()
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'edmacs-git-common-dir-1)
               (lambda (root) (concat root ".git"))))
      (should (equal (edmacs-git-common-dir "/repo/wt/") "/repo/wt/.git"))
      (should (equal (gethash "/repo/wt/" edmacs-git-common-dir-cache)
                     "/repo/wt/.git")))))

;; ============================================================================
;; edmacs-git-common-dir-main-worktree / -repo-name
;; ============================================================================

(ert-deftest edmacs-git-common-dir-test-main-worktree-one-level-up ()
  (should (equal (edmacs-git-common-dir-main-worktree "/Users/ed/Projects/edmacs/.git")
                 "/Users/ed/Projects/edmacs/")))

(ert-deftest edmacs-git-common-dir-test-repo-name-from-common-dir ()
  (should (equal (edmacs-git-common-dir-repo-name "/Users/ed/Projects/edmacs/.git")
                 "edmacs")))

(provide 'git-common-dir-test)
;;; git-common-dir-test.el ends here
