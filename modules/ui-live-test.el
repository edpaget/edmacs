;;; ui-live-test.el --- Tests for the term-mode nano-modeline fix -*- lexical-binding: t -*-

;;; Commentary:
;; Regression coverage for the `nano-modeline-term-shell-mode' bug fixed
;; in ui.el: it called the macro `term-in-char-mode' as an ordinary
;; function (nano-modeline.el is byte-compiled by straight without `term'
;; loaded), which signals `invalid-function' at every render and
;; `format-mode-line' silently swallows into "" -- so a term-mode buffer's
;; whole mode line appeared empty rather than erroring visibly.
;;
;; `format-mode-line' cannot be used for any assertion here: it returns
;; "" under `--batch' unconditionally, independent of whether the
;; underlying `:eval' construct is broken or fine (see the identical note
;; in claude-usage-test.el and claude-term-agents-test.el). Every
;; assertion below evaluates `(cadr mode-line-format)' directly instead.
;;
;; Loading order matters: this file loads the real `nano-modeline'
;; package from the straight build tree FIRST, then loads modules/ui.el
;; itself (rather than passing ui.el on the command line) -- ui.el's own
;; `(use-package nano-modeline :config ...)' silently no-ops its `:config'
;; body when `nano-modeline' cannot be `require'd, so preloading it here
;; is the only way to get the `term-mode-hook' wiring and the
;; `advice-add' this fix installs to actually run during a batch test.
;;
;; Two tiers:
;;
;;   Tier 1 -- runs under plain `-Q --batch', against a process-less
;;   `term-mode' buffer.
;;
;;   Tier 2 -- needs a real graphical frame (a live `make-term'
;;   subprocess is spawned against it), so it skips under `--batch' and
;;   runs via `scripts/gui-ert.sh'.
;;
;; Tier 1 invocation (this is the CI-equivalent one):
;;
;;   emacs -Q --batch -l ert -l modules/ui-live-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Tier 2 invocation:
;;
;;   scripts/gui-ert.sh modules/ui-live-test.el
;;
;; Both skip cleanly (a single reported skip) if this checkout and its
;; sibling main checkout have no bootstrapped straight tree.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'term)

(defun edmacs-ui-live-test--locate-straight-build-root ()
  "Return this checkout's `straight/build' directory, or nil.
Tries this checkout's own `straight/build' first, then falls back to the
sibling main `edmacs' checkout's `straight/build' -- see
`edmacs-sidebar-test--locate-straight-build-root' for the identical
worktree-vs-sibling-main-checkout rationale."
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

(defvar edmacs-ui-live-test--build-root
  (edmacs-ui-live-test--locate-straight-build-root)
  "This checkout's (or its sibling main checkout's) `straight/build' root.")

(if (null edmacs-ui-live-test--build-root)

    (ert-deftest edmacs-ui-live-test-straight-unavailable ()
      (ert-skip "no bootstrapped straight/build found in this checkout or its \
sibling main checkout; bootstrap straight once (open this worktree in a real \
Emacs session) to enable this suite"))

  (progn

    ;; nano-modeline needs only `cl-lib' beyond Emacs core, so a single
    ;; `load-path' entry under the straight build root is enough.
    (let ((dir (expand-file-name "nano-modeline" edmacs-ui-live-test--build-root)))
      (add-to-list 'load-path dir))
    (require 'nano-modeline)

    ;; Loading ui.el here, after nano-modeline is already provided, is
    ;; what makes its `use-package nano-modeline :config' block actually
    ;; run -- see the Commentary header above.
    (load (expand-file-name "modules/ui.el" default-directory) nil t)

    ;; ==========================================================================
    ;; AC1/AC2 -- process-less term-mode buffer, real rendered output
    ;; ==========================================================================

    (ert-deftest edmacs-ui-live-test-term-mode-line-renders-nonempty ()
      "A `term-mode' buffer's mode line renders a real, non-empty string.
This fails against the pre-fix code with `(invalid-function
term-in-char-mode)': nano-modeline.el's own `nano-modeline-term-shell-mode'
calls that macro as a function, which `format-mode-line' would otherwise
mask as an empty string -- see the Commentary header."
      (let ((nano-modeline-position #'nano-modeline-footer))
        ;; Wide enough that the shell-mode segment is never elided by
        ;; nano-modeline's own truncation of the left side.
        (set-frame-width (selected-frame) 200)
        (with-temp-buffer
          (term-mode)
          (nano-modeline-term-mode)
          (let ((rendered (eval (cadr mode-line-format) t)))
            (should (stringp rendered))
            (should (> (length rendered) 0))
            (should (string-match-p "(line mode)" rendered))))))

    ;; ==========================================================================
    ;; AC3 -- live ansi-term-style subprocess, real GUI frame
    ;; ==========================================================================

    (ert-deftest edmacs-ui-live-test-term-mode-line-renders-nonempty-with-live-process ()
      "Same property as the Tier-1 test, against a real subprocess.
A live process may bind `term-raw-map' or attach filters a process-less
buffer never does, so this repeats the assertion under a real GUI frame
with a real shell attached, per the phase's explicit requirement."
      (unless (display-graphic-p)
        (ert-skip "needs a graphical frame; run via scripts/gui-ert.sh"))
      (let* ((nano-modeline-position #'nano-modeline-footer)
             (buf (make-term "edmacs-ui-live-test" (or (getenv "SHELL") "/bin/sh")))
             (proc (get-buffer-process buf)))
        (set-frame-width (selected-frame) 200)
        (unwind-protect
            (with-current-buffer buf
              (nano-modeline-term-mode)
              (let ((rendered (eval (cadr mode-line-format) t)))
                (should (stringp rendered))
                (should (> (length rendered) 0))
                (should (string-match-p "(line mode)\\|(char mode)" rendered))
                ;; `nano-modeline-default-directory' truncates to 32 columns
                ;; with a leading "…", so check the trailing path component
                ;; rather than the full `default-directory' string.
                (should (string-match-p
                         (regexp-quote
                          (file-name-nondirectory
                           (directory-file-name default-directory)))
                         rendered))))
          (when (process-live-p proc) (delete-process proc))
          (kill-buffer buf))))))

;;; ui-live-test.el ends here
