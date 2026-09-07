;;; core-live-test.el --- Tests for core.el needing real git repos -*- lexical-binding: t -*-

;;; Commentary:
;; Covers `edmacs--register-projects-under-projects-dir' (core.el), the
;; picker-seeding registrar that must list a repo's MAIN worktree and never
;; its `<repo>__worktrees/<slug>' siblings. Real `git init'/`git worktree
;; add' work in a temp sandbox: the property under test is what git's own
;; on-disk shapes make `project.el' remember, which a stub cannot prove.
;;
;; No frame is created here, so no pty is needed -- but `core.el' does not
;; load standalone without `compat'/`cond-let'/`transient' on `load-path',
;; which come from this checkout's `straight/build' (or, from a worktree,
;; the sibling main checkout's). The single test in this file's
;; unavailable branch reports that as a skip.
;;
;; `user-emacs-directory' AND `project-list-file' are both rebound into
;; the sandbox before core.el loads, and every test asserts
;; `project-list-file' really resolved under the sandbox before touching
;; `project-known-project-roots' -- otherwise a throwaway subprocess repo
;; could be written into the real user's own project list.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/core-live-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'project)

;; `cl-letf' on a C subr forces a synchronous native-comp trampoline build
;; (~28s, and an outright failure where the eln-cache is unwritable). The
;; one `cl-letf' below targets `directory-files', a subr -- so this guard
;; is load-bearing here, not merely defensive. See CLAUDE.md.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defun edmacs-core-live-test--locate-straight-build-root ()
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

(defvar edmacs-core-live-test--build-root
  (edmacs-core-live-test--locate-straight-build-root))

(defun edmacs-core-live-test--add-dep (dep)
  "Add DEP's directory under the located `straight/build' to `load-path'."
  (when edmacs-core-live-test--build-root
    (let ((dir (expand-file-name dep edmacs-core-live-test--build-root)))
      (when (file-directory-p dir) (add-to-list 'load-path dir)))))

(defvar edmacs-core-live-test--available
  (and edmacs-core-live-test--build-root
       (dolist (dep '("compat" "cond-let" "transient") t)
         (edmacs-core-live-test--add-dep dep)))
  "Non-nil once the deps `core.el' needs to load standalone are on `load-path'.")

(defun edmacs-core-live-test--make-git-repo (dir)
  "Init a real, one-commit git repo at DIR."
  (make-directory dir t)
  (let ((default-directory (file-name-as-directory dir)))
    (call-process "git" nil nil nil "init" "-q")
    (call-process "git" nil nil nil "config" "user.email" "t@t.com")
    (call-process "git" nil nil nil "config" "user.name" "t")
    (write-region "x" nil (expand-file-name "f.txt" dir))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-q" "-m" "init")))

(defun edmacs-core-live-test--add-worktree (repo wt-dir)
  "Add a real git worktree of REPO at WT-DIR."
  (let ((default-directory (file-name-as-directory repo)))
    (call-process "git" nil nil nil "worktree" "add" wt-dir)))

(defmacro edmacs-core-live-test--with-registrar-sandbox (vars &rest body)
  "Load core.el's registrar into a sandboxed `user-emacs-directory', run BODY.
VARS binds (SANDBOX PROJECTS-DIR) for BODY's use. `project.el' may already
be loaded in this process, which freezes `project-list-file's VALUE at its
first-load `user-emacs-directory' -- rebinding that variable alone would
not move it, so `project-list-file' (and `project--list', so a previously
read real list is never reused) are bound directly too."
  (declare (indent 1))
  (cl-destructuring-bind (sandbox-var projects-var) vars
    `(if (not edmacs-core-live-test--available)
         (ert-skip "compat/cond-let/transient's straight build was not found; \
bootstrap straight once (open this checkout in a real Emacs session) to enable \
this test")
       (let* ((,sandbox-var (file-name-as-directory (make-temp-file "edmacs-core-test-" t)))
              (,projects-var (expand-file-name "Projects/" ,sandbox-var))
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

(defun edmacs-core-live-test--call-registrar-against (projects-dir)
  "Run the real registrar with `directory-files' redirected to PROJECTS-DIR
for its one `~/Projects' lookup, leaving every other call untouched."
  (let ((orig (symbol-function 'directory-files)))
    (cl-letf (((symbol-function 'directory-files)
               (lambda (dir &rest args)
                 (if (equal (expand-file-name dir) (expand-file-name "~/Projects"))
                     (apply orig projects-dir args)
                   (apply orig dir args)))))
      (edmacs--register-projects-under-projects-dir))))

;; ============================================================================
;; The picker-seeding registrar lists repos, never their worktrees
;; ============================================================================

(ert-deftest edmacs-core-live-test-registrar-lists-repos-not-worktrees ()
  (edmacs-core-live-test--with-registrar-sandbox (sandbox projects)
    (let* ((repo1 (expand-file-name "repo1" projects))
           (repo2 (expand-file-name "repo2" projects))
           (wt-dir (expand-file-name "repo1__worktrees" projects))
           (wt-a (expand-file-name "wt-a" wt-dir)))
      (edmacs-core-live-test--make-git-repo repo1)
      (edmacs-core-live-test--make-git-repo repo2)
      (make-directory wt-dir t)
      (edmacs-core-live-test--add-worktree repo1 wt-a)
      (edmacs-core-live-test--call-registrar-against projects)
      (let ((roots (project-known-project-roots)))
        (should (member (file-name-as-directory repo1) roots))
        (should (member (file-name-as-directory repo2) roots))
        (should-not (seq-find (lambda (r) (string-match-p "__worktrees/" r)) roots))
        ;; Visiting a file inside the worktree via plain find-file must not
        ;; cause it to be remembered either.
        (let ((buf (find-file-noselect (expand-file-name "f.txt" wt-a))))
          (unwind-protect
              (with-current-buffer buf
                (call-interactively #'project-remember-project))
            (kill-buffer buf)))
        (should-not (seq-find (lambda (r) (string-match-p "__worktrees/" r))
                              (project-known-project-roots)))))))

(ert-deftest edmacs-core-live-test-registrar-idempotent-after-worktree-visit ()
  "Re-running the registrar after a worktree visit still excludes it."
  (edmacs-core-live-test--with-registrar-sandbox (sandbox projects)
    (let* ((repo1 (expand-file-name "repo1" projects))
           (wt-dir (expand-file-name "repo1__worktrees" projects))
           (wt-a (expand-file-name "wt-a" wt-dir)))
      (edmacs-core-live-test--make-git-repo repo1)
      (make-directory wt-dir t)
      (edmacs-core-live-test--add-worktree repo1 wt-a)
      (edmacs-core-live-test--call-registrar-against projects)
      (find-file-noselect (expand-file-name "f.txt" wt-a))
      (edmacs-core-live-test--call-registrar-against projects)
      (should-not (seq-find (lambda (r) (string-match-p "__worktrees/" r))
                            (project-known-project-roots))))))

(provide 'core-live-test)
;;; core-live-test.el ends here
