;;; completion-test.el --- Tests for completion.el -*- lexical-binding: t -*-

;;; Commentary:
;; Run with this file on the command line, from any `default-directory'
;; (the repo root, a sibling worktree, wherever) -- every path below
;; resolves against this file's own directory via `load-file-name', not
;; against `default-directory', so cwd does not matter:
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
;; not treat as failure) benign stderr noise for embark, corfu, cape
;; and kind-icon, whose build dirs this file does not add.
;;
;; vertico, vertico's `:straight nil' extensions (vertico-directory,
;; vertico-multiform, vertico-repeat, vertico-quick), orderless and
;; consult all get their build dirs added AND get an explicit `require'
;; before `completion.el' loads (below), for two independent reasons
;; confirmed live against this checkout's real `straight/build/':
;;
;; 1. A `use-package' block carrying `:bind' (vertico's own block does)
;;    defers its `require' until the bound key is actually pressed --
;;    never, in a batch run -- so `:config' silently never executes and
;;    `vertico-map'/`vertico-cycle' stay unbound unless `vertico' is
;;    `require'd first. The same applies to every `:after vertico'
;;    extension block.
;; 2. `:straight nil' is an unrecognized `use-package' keyword with no
;;    `straight.el' loaded (`-Q' loads none): each such block's macro
;;    expansion aborts before its `:init'/`:config'/`:hook' ever runs.
;;    A no-op `:straight' keyword handler, registered before the load,
;;    fixes this for `vertico-directory'/`vertico-multiform'/
;;    `vertico-repeat'/`vertico-quick' the same way it would for
;;    `corfu-popupinfo' (left unstubbed here since nothing in this file
;;    asserts on it).
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
;; `core.el' must load BEFORE `completion.el': `core.el''s `savehist'
;; block does a real (eager, no deferring keyword) `require' of
;; `savehist' and `setq's `savehist-additional-variables' -- a plain
;; `setq', not `add-to-list'. `completion.el''s `vertico-repeat' block
;; `add-to-list's `vertico-repeat-history' onto that same variable, so
;; loading `core.el' second would silently clobber it back out
;; (confirmed live: loading in the old completion-then-core order left
;; `vertico-repeat-history' entirely absent from
;; `savehist-additional-variables').
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

(defvar completion-test--repo-root
  (expand-file-name
   ".."
   (file-name-directory
    (or load-file-name buffer-file-name
        (expand-file-name "modules/completion-test.el" default-directory))))
  "Repo root this file lives under (the parent of its own `modules/' dir).
Resolved from `load-file-name' (or `buffer-file-name' when evaluated
interactively) rather than `default-directory', so every path derived
from it below is correct regardless of the caller's cwd -- loading this
file by absolute path from an unrelated `default-directory' (e.g. the
main checkout while this file exists only on a roadmap worktree) must
still load *this* checkout's `modules/completion.el' and
`modules/core.el', not whatever happens to sit at that relative path
from cwd.")

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
   (let ((here (expand-file-name "straight/build" completion-test--repo-root)))
     (and (file-directory-p here) here))
   (let* ((root (directory-file-name completion-test--repo-root))
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

    (dolist (pkg '("marginalia" "vertico" "orderless" "consult"))
      (add-to-list 'load-path (expand-file-name pkg completion-test--build-root)))

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
      (load (expand-file-name "modules/core.el" completion-test--repo-root) nil t))

    ;; Must run before `completion.el' loads -- see the load-order note
    ;; in the Commentary above.
    (completion-test--load-core)

    (require 'use-package)
    ;; No-op `:straight' keyword: lets the `:straight nil' blocks below
    ;; macro-parse under bare `-Q' (no `straight.el' loaded) so their
    ;; `:init'/`:config'/`:hook' forms actually run and are assertable.
    (add-to-list 'use-package-keywords :straight t)
    (defun use-package-normalize/:straight (_name _keyword args) args)
    (defun use-package-handler/:straight (name _keyword _arg rest state)
      (use-package-process-keywords name rest state))

    ;; Forces real loading ahead of `completion.el': every one of these
    ;; is `:after vertico' or otherwise deferred via `:bind', so without
    ;; a `require' here its `:config'/`:hook' would silently never run.
    (require 'vertico)
    (require 'vertico-directory)
    (require 'vertico-multiform)
    (require 'vertico-repeat)
    (require 'vertico-quick)
    (require 'orderless)
    (require 'consult)

    (load (expand-file-name "modules/completion.el" completion-test--repo-root) nil t)

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
      (should (eq (lookup-key global-map (kbd "M-g f")) 'consult-flymake)))

    ;; ==========================================================================
    ;; Phase 2 -- vertico core display variables
    ;; ==========================================================================

    (ert-deftest completion-test-vertico-count ()
      (should (= vertico-count 13)))

    (ert-deftest completion-test-vertico-resize ()
      (should (eq vertico-resize t)))

    (ert-deftest completion-test-vertico-scroll-margin ()
      (should (= vertico-scroll-margin 2)))

    ;; ==========================================================================
    ;; AC1 -- vertico-multiform-mode active, command/category alists
    ;; ==========================================================================

    (ert-deftest completion-test-vertico-multiform-mode-active ()
      (should (bound-and-true-p vertico-multiform-mode)))

    (ert-deftest completion-test-vertico-multiform-commands ()
      (should (equal (assq 'consult-imenu vertico-multiform-commands)
                      '(consult-imenu buffer)))
      (should (equal (assq 'consult-ripgrep vertico-multiform-commands)
                      '(consult-ripgrep buffer)))
      (should (equal (assq 'consult-line vertico-multiform-commands)
                      '(consult-line buffer))))

    (ert-deftest completion-test-vertico-multiform-categories-file-grid ()
      (should (equal (assq 'file vertico-multiform-categories) '(file grid))))

    ;; ==========================================================================
    ;; AC2 -- vertico-repeat-save on minibuffer-setup-hook,
    ;; vertico-repeat-history in savehist-additional-variables
    ;; ==========================================================================

    (ert-deftest completion-test-vertico-repeat-save-hook ()
      (should (memq 'vertico-repeat-save minibuffer-setup-hook)))

    (ert-deftest completion-test-vertico-repeat-history-in-savehist ()
      (should (memq 'vertico-repeat-history savehist-additional-variables))
      ;; Regression guard: core.el's plain `setq' additions must survive
      ;; alongside completion.el's `add-to-list' addition.
      (should (memq 'search-ring savehist-additional-variables))
      (should (memq 'regexp-search-ring savehist-additional-variables)))

    ;; ==========================================================================
    ;; AC3 -- vertico-quick bindings do not collide
    ;; ==========================================================================

    (ert-deftest completion-test-vertico-quick-bindings ()
      (should (eq (lookup-key vertico-map (kbd "C-q")) 'vertico-quick-exit))
      (should (eq (lookup-key vertico-map (kbd "M-q")) 'vertico-quick-insert)))

    (ert-deftest completion-test-vertico-quick-no-collision-regression ()
      (should (eq (lookup-key vertico-map (kbd "C-h")) 'vertico-directory-up))
      (should (eq (lookup-key vertico-map (kbd "C-j")) 'vertico-next))
      (should (eq (lookup-key vertico-map (kbd "C-d")) 'vertico-scroll-down))
      (should (eq (lookup-key vertico-map (kbd "C-w")) 'backward-kill-word)))

    ;; ==========================================================================
    ;; AC4 -- orderless applies to the file category, out of order
    ;; ==========================================================================

    (ert-deftest completion-test-completion-category-overrides-file-orderless ()
      (should (equal (assq 'file completion-category-overrides)
                      '(file (styles orderless partial-completion)))))

    (ert-deftest completion-test-orderless-matching-styles-initialism ()
      (should (memq 'orderless-initialism orderless-matching-styles)))

    (defun completion-test--strip-completion-base-size (all)
      "Drop the trailing base-size cdr `completion-all-completions' returns.
Its return value is an improper list whose final cdr is an integer, not
nil -- walk conses only, so callers get a plain list of candidates."
      (let (acc (rest all))
        (while (consp rest)
          (push (car rest) acc)
          (setq rest (cdr rest)))
        (nreverse acc)))

    (ert-deftest completion-test-orderless-file-category-out-of-order ()
      (let* ((cands '("foo-bar.txt" "bar-foo.txt" "baz.txt"))
             (table (lambda (str pred action)
                      (if (eq action 'metadata)
                          '(metadata (category . file))
                        (complete-with-action action cands str pred))))
             (names (mapcar #'substring-no-properties
                             (completion-test--strip-completion-base-size
                              (completion-all-completions "bar foo" table nil 7)))))
        (should (equal (sort (copy-sequence names) #'string<)
                        '("bar-foo.txt" "foo-bar.txt")))))

    ;; ==========================================================================
    ;; AC5 -- consult-narrow-key
    ;; ==========================================================================

    (ert-deftest completion-test-consult-narrow-key ()
      (should (equal consult-narrow-key "<")))))

(provide 'completion-test)
;;; completion-test.el ends here
