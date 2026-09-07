;;; ambient-reads-test.el --- Tests for ambient-reads.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage of the lint's detection logic -- no modules/
;; dependency, no real Emacs config.  Run with:
;;
;;   emacs -Q --batch -l ert -l scripts/ambient-reads.el \
;;         -l scripts/ambient-reads-test.el -f ert-run-tests-batch-and-exit
;;
;; Each test writes a small synthetic snippet to a temp file and runs
;; `edmacs-ambient-reads-file' over it directly, rather than asserting
;; against `edmacs-ambient-reads-batch''s stdout/exit-code shape.

;;; Code:

(require 'ert)

(defun edmacs-ambient-reads-test--findings (source)
  "Return `edmacs-ambient-reads-file' findings for SOURCE (a string)."
  (let ((file (make-temp-file "ambient-reads-test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert source)
            (write-region (point-min) (point-max) file nil 'silent))
          (edmacs-ambient-reads-file file))
      (delete-file file))))

(defun edmacs-ambient-reads-test--severities (source)
  "Return the list of severities (`error'/`warn') found in SOURCE."
  (mapcar (lambda (f) (nth 2 f)) (edmacs-ambient-reads-test--findings source)))

;; ============================================================================
;; The existing literal-symbol idiom (`(selected-frame)'/`(selected-window)')
;; ============================================================================

(ert-deftest edmacs-ambient-reads-test-literal-with-parameter-is-error ()
  "A function that HAS a frame parameter and reads `(selected-frame)'
anyway (not inside an `(or ...)' guard) is the bug shape: ERROR."
  (should (equal '(error)
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (frame)\n  (ignore frame)\n  (selected-frame))\n"))))

(ert-deftest edmacs-ambient-reads-test-literal-without-parameter-is-warn ()
  "A function with no frame parameter at all is ambient by construction: WARN."
  (should (equal '(warn)
                 (edmacs-ambient-reads-test--severities
                  "(defun foo ()\n  (selected-frame))\n"))))

(ert-deftest edmacs-ambient-reads-test-or-defaulting-idiom-is-exempt ()
  "`(or frame (selected-frame))' is the sanctioned defaulting shape."
  (should (equal '()
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (&optional frame)\n  (let ((frame (or frame (selected-frame))))\n    frame))\n"))))

(ert-deftest edmacs-ambient-reads-test-interactive-spec-is-exempt ()
  "`(interactive (list (selected-frame)))' supplies the ambient value as
the command's own argument -- the other sanctioned shape, not the
`(or ...)' guard above."
  (should (equal '()
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (frame)\n  (interactive (list (selected-frame)))\n  frame)\n"))))

(ert-deftest edmacs-ambient-reads-test-interactive-spec-nested-is-exempt ()
  "A numeric-prefix branch (`edmacs-window-promote''s own shape) can nest
the read several levels inside the `interactive' spec; still exempt."
  (should (equal '()
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (window)
  (interactive
   (list (if current-prefix-arg
             (other-window-somehow)
           (selected-window))))
  window)
"))))

(ert-deftest edmacs-ambient-reads-test-interactive-spec-without-matching-parameter-is-warn ()
  "The `interactive' exemption is conditioned on the SAME has-parameter
gate as the `(or FRAME (selected-frame))' guard above -- not applied
unconditionally to any ambient read textually inside an `(interactive
...)' form. A function with no frame parameter at all reading
`(selected-frame)' there is still ambient by construction: WARN, not
silently exempt."
  (should (equal '(warn)
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (name)\n  (interactive (list (format \"tab in %s\" (selected-frame))))\n  name)\n"))))

(ert-deftest edmacs-ambient-reads-test-suppression-comment-is-exempt ()
  "The `;; ambient-reads: ok' suppression comment on the preceding line
silences a finding that would otherwise be an ERROR."
  (should (equal '()
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (frame)\n  (ignore frame)\n  ;; ambient-reads: ok\n  (selected-frame))\n"))))

;; ============================================================================
;; The nil-argument idiom (`(frame-parameter nil ...)' and friends)
;; ============================================================================

(ert-deftest edmacs-ambient-reads-test-nil-arg-with-parameter-is-error ()
  "A literal nil FRAME argument to `frame-parameter' in a function that
HAS a frame parameter is the same bug shape as a literal `(selected-frame)'
read: ERROR."
  (should (equal '(error)
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (frame)\n  (ignore frame)\n  (frame-parameter nil 'edmacs-workspace-root))\n"))))

(ert-deftest edmacs-ambient-reads-test-nil-arg-without-parameter-is-warn ()
  "No frame parameter at all: ambient by construction, WARN."
  (should (equal '(warn)
                 (edmacs-ambient-reads-test--severities
                  "(defun foo ()\n  (frame-parameter nil 'edmacs-workspace-root))\n"))))

(ert-deftest edmacs-ambient-reads-test-nil-arg-explicit-value-is-clean ()
  "Passing the parameter itself, rather than nil, is the fix -- no finding."
  (should (equal '()
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (frame)\n  (frame-parameter frame 'edmacs-workspace-root))\n"))))

(ert-deftest edmacs-ambient-reads-test-nil-arg-window-list-flagged ()
  "`window-list' is in the flagged-function table too, not just
`frame-parameter'."
  (should (equal '(error)
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (frame)\n  (ignore frame)\n  (window-list nil 'no-minibuf))\n"))))

(ert-deftest edmacs-ambient-reads-test-nil-arg-suppressed ()
  "The same suppression comment convention applies to the nil-argument scan."
  (should (equal '()
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (frame)\n  (ignore frame)\n  ;; ambient-reads: ok\n  (frame-parameter nil 'edmacs-workspace-root))\n"))))

(ert-deftest edmacs-ambient-reads-test-get-buffer-window-current-buffer-any-frame-exempt ()
  "`(get-buffer-window nil t)' means \"the current buffer's window, on any
frame\" -- a deliberate idiom, not the ambient-frame bug shape, so it is
exempt regardless of whether the enclosing function has a buffer parameter."
  (should (equal '()
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (buf)\n  (ignore buf)\n  (get-buffer-window nil t))\n"))))

(ert-deftest edmacs-ambient-reads-test-get-buffer-window-other-second-arg-flagged ()
  "A nil BUFFER-OR-NAME with any other ALL-FRAMES value is not the
sanctioned \"any frame\" idiom and is flagged like any other kind."
  (should (equal '(error)
                 (edmacs-ambient-reads-test--severities
                  "(defun foo (buf)\n  (ignore buf)\n  (get-buffer-window nil nil))\n"))))

(provide 'ambient-reads-test)
;;; ambient-reads-test.el ends here
