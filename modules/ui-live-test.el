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
;;   emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/ui-live-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Tier 2 invocation:
;;
;;   scripts/gui-ert.sh modules/ui-live-test.el t -l modules/test-support.el
;;
;; Both skip cleanly (a single reported skip) if this checkout and its
;; sibling main checkout have no bootstrapped straight tree.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'term)

;; See CLAUDE.md's Testing section: `cl-letf' on a C subr forces a
;; synchronous native-comp trampoline build (~28s) the first time it is
;; hit. Defensive here even where no target below is a subr.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defvar edmacs-ui-live-test--build-root
  (edmacs-test-support-straight-build-root)
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
      (edmacs-test-support-gui-frame-or-skip)
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
          (kill-buffer buf))))

    ;; ==========================================================================
    ;; Icon-face regression -- edmacs-modeline--restore-icon-face
    ;; ==========================================================================
    ;; A `nerd-icons' glyph is a single character whose 'face carries the
    ;; patched PUA font's family and a height correction, both required for
    ;; the codepoint to render as the intended glyph rather than a box.
    ;; `nano-modeline-buffer-status'/`edmacs-modeline-fixed-status' both
    ;; finish by `propertize'-ing their whole padded span with ONE uniform
    ;; badge face, which -- unguarded -- replaces (not merges with) that
    ;; per-character face. These tests run the REAL nano-modeline
    ;; constructors (loaded above), not a stub that only captures the
    ;; STATUS argument, so they exercise the exact `propertize' call where
    ;; the clobber would happen. `nerd-icons' itself is not loaded (it is
    ;; not part of this file's `straight/build' preload); `provide'-ing
    ;; the feature and defining its two entry points directly, rather than
    ;; `cl-letf'-stubbing them, is enough to satisfy
    ;; `edmacs-modeline--mode-icon'/`--agent-icon's own `featurep'/`fboundp'
    ;; guards without building a native-comp subr trampoline for `featurep'
    ;; itself (see the guard note in modules/ui-test.el).

    (provide 'nerd-icons)

    (defun nerd-icons-icon-for-mode (_mode)
      "Stand-in for the real function: same shape, a fixed family/height."
      (propertize "" 'face '(:family "Symbols Nerd Font Mono" :height 1.2)
                  'display '(raise 0.0)))

    (defun nerd-icons-mdicon (_name)
      "Stand-in for the real function: same shape, a fixed family/height."
      (propertize "" 'face '(:family "Material Design Icons" :height 1.1)
                  'display '(raise 0.0)))

    (defun edmacs-ui-live-test--face-list-at (string pos)
      "The face(s) at POS in STRING, always as a list.
`get-text-property' returns either one face spec or a list of them
depending on whether more than one was merged in via
`add-face-text-property' -- normalize both shapes so callers can
`member' against either case uniformly."
      (let ((face (get-text-property pos 'face string)))
        (if (and (consp face) (not (keywordp (car face)))) face (list face))))

    (ert-deftest edmacs-ui-live-test-buffer-status-icon-keeps-its-own-face ()
      "The mode icon's own family/height face survives
`edmacs-modeline-buffer-status's real (unstubbed) badge construction,
alongside -- not instead of -- the badge's own read-write face."
      (let ((edmacs-modeline-force-text-glyphs nil))
        (with-temp-buffer
          (setq buffer-read-only nil)
          (set-buffer-modified-p nil)
          (let* ((result (edmacs-modeline-buffer-status))
                 (faces (edmacs-ui-live-test--face-list-at result 1)))
            (should (stringp result))
            (should (member '(:family "Symbols Nerd Font Mono" :height 1.2) faces))
            (should (member (nano-modeline-face 'status-RW) faces))))))

    (ert-deftest edmacs-ui-live-test-ghostel-status-icon-keeps-its-own-face ()
      "The claude-term robot glyph's own family/height face survives
`edmacs-modeline-ghostel-status's real (unstubbed) badge construction."
      (let ((edmacs-modeline-force-text-glyphs nil))
        (with-temp-buffer
          (setq-local claude-term-mode t)
          (let* ((result (edmacs-modeline-ghostel-status))
                 (faces (edmacs-ui-live-test--face-list-at result 1)))
            (should (stringp result))
            (should (member '(:family "Material Design Icons" :height 1.1) faces))
            (should (member (nano-modeline-face 'status-RO) faces))))))))

;;; ui-live-test.el ends here
