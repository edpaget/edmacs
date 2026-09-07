;;; completion-test.el --- Tests for completion.el -*- lexical-binding: t -*-

;;; Commentary:
;; Run from the repo root (main checkout -- `straight/build' must be
;; populated):
;;
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/completion-test.el -f ert-run-tests-batch-and-exit
;;
;; `modules/completion.el' loads cleanly to EOF under bare `-Q' with no
;; packages on `load-path': every package reference in it goes through
;; the `use-package' macro, whose own error handling prints a
;; non-fatal "Error (use-package): ..." per missing package to stderr
;; and lets the file finish loading -- the same pattern documented for
;; `claude-term.el'/`windows.el' in .claude/CLAUDE.md. Expect (and do
;; not treat as failure) benign stderr noise for vertico, orderless,
;; consult, embark, corfu, cape and kind-icon: only `marginalia''s
;; straight build directory is added to `load-path' below, since it is
;; the only package these tests need actually loaded (confirmed live:
;; `marginalia' loads standalone with no further straight dependencies,
;; and `use-package''s default eager `:demand'-like behavior for a
;; block with no deferring keyword -- no `:bind'/`:commands'/`:mode' --
;; means `(require 'marginalia)' really runs during the `completion.el'
;; load here, not merely on first use).
;;
;; By contrast `modules/core.el' cannot be `load'ed standalone under
;; `-Q' without a stub: its three top-level `(straight-use-package ...)'
;; calls (for compat/cond-let/transient) are real function calls, not
;; wrapped in `use-package', and hard-abort the load with
;; `void-function straight-use-package' before reaching the `savehist'
;; block this file cares about (confirmed live this session). So
;; `completion-test--load-core' below stubs `straight-use-package' as a
;; no-op first, then adds `straight/build/compat' and
;; `straight/build/cond-let' to `load-path' so the real, unstubbed
;; `(require 'compat)' / `(require 'cond-let)' calls that follow
;; succeed for real.
;;
;; No test in this file `cl-letf's a C subr -- every setting under test
;; (`enable-recursive-minibuffers', `marginalia-align', `history-length',
;; etc.) is read via plain variable access after a real `load', never
;; advised or rebound -- so the `native-comp-enable-subr-trampolines'
;; guard documented in .claude/CLAUDE.md is deliberately omitted here.
;; A future contributor adding a `cl-letf' test to this file should add
;; that guard then.

;;; Code:

(require 'ert)
(require 'subr-x)

(defun completion-test--locate-straight-build-root ()
  "Return this checkout's `straight/build' directory, or nil.
Tries this checkout's own `straight/build' first, then falls back to
the sibling main `edmacs' checkout's `straight/build' -- the same
worktree-vs-sibling-main-checkout fallback used elsewhere in this repo
\(see `edmacs-sidebar-test--locate-straight-build-root' in
modules/sidebar-test.el\): a roadmap worktree lives under
`<parent>/edmacs__worktrees/<name>', sibling to the main
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

(defvar completion-test--build-root
  (completion-test--locate-straight-build-root)
  "This checkout's (or its sibling main checkout's) `straight/build' root.")

(if (null completion-test--build-root)

    (ert-deftest completion-test-straight-build-unavailable ()
      (ert-skip "straight/build was not found in this checkout or its \
sibling main checkout; bootstrap straight once (open this worktree in a \
real Emacs session) to enable this suite"))

  (progn

    (add-to-list 'load-path
                 (expand-file-name "marginalia" completion-test--build-root))

    (load (expand-file-name "modules/completion.el" default-directory) nil t)

    (defun completion-test--load-core ()
      "Load modules/core.el standalone, stubbing `straight-use-package'.
`core.el' begins with three raw `(straight-use-package ...)' calls
that are real function calls, not wrapped in `use-package' -- they
hard-abort a standalone load with `void-function' otherwise. The
`require's that follow them are real and unstubbed."
      (unless (fboundp 'straight-use-package)
        (defalias 'straight-use-package (lambda (&rest _) nil)))
      (add-to-list 'load-path
                   (expand-file-name "compat" completion-test--build-root))
      (add-to-list 'load-path
                   (expand-file-name "cond-let" completion-test--build-root))
      (load (expand-file-name "modules/core.el" default-directory) nil t))

    (completion-test--load-core)

    ;; ==========================================================================
    ;; AC1 -- marginalia--annotator resolves the built-in registry
    ;; ==========================================================================

    (ert-deftest completion-test-marginalia-annotator-command ()
      (should (eq (marginalia--annotator 'command) 'marginalia-annotate-command)))

    (ert-deftest completion-test-marginalia-annotator-file ()
      (should (eq (marginalia--annotator 'file) 'marginalia-annotate-file)))

    (ert-deftest completion-test-marginalia-align ()
      (should (eq marginalia-align 'right)))

    ;; ==========================================================================
    ;; AC3 -- recursive minibuffers and the depth indicator
    ;; ==========================================================================

    (ert-deftest completion-test-enable-recursive-minibuffers ()
      (should (eq enable-recursive-minibuffers t)))

    (ert-deftest completion-test-minibuffer-depth-indicate-mode ()
      (should minibuffer-depth-indicate-mode))

    ;; ==========================================================================
    ;; Step 5 -- M-x hides commands that cannot run in the current mode
    ;; ==========================================================================

    (ert-deftest completion-test-read-extended-command-predicate ()
      (should (eq read-extended-command-predicate
                  #'command-completion-default-include-p)))

    ;; ==========================================================================
    ;; Step 6 -- cursor stays out of the minibuffer prompt text
    ;; ==========================================================================

    (ert-deftest completion-test-cursor-intangible-prompt-property ()
      (should (memq 'cursor-intangible minibuffer-prompt-properties))
      (should (memq #'cursor-intangible-mode minibuffer-setup-hook)))

    ;; ==========================================================================
    ;; AC4 -- history-length and savehist-additional-variables
    ;; ==========================================================================

    (ert-deftest completion-test-history-length ()
      (should (= history-length 300)))

    (ert-deftest completion-test-savehist-additional-variables ()
      (should (memq 'search-ring savehist-additional-variables))
      (should (memq 'regexp-search-ring savehist-additional-variables)))

    ;; ==========================================================================
    ;; AC6 -- M-g f is untouched, still bound to consult-flymake
    ;; ==========================================================================

    (ert-deftest completion-test-m-g-f-still-flymake ()
      (should (eq (lookup-key global-map (kbd "M-g f")) 'consult-flymake)))))

(provide 'completion-test)
;;; completion-test.el ends here
