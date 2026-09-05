;;; gui-ert.el --- run an ERT suite inside a real graphical frame -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded into the throwaway daemon started by `scripts/gui-ert.sh'.
;; `emacs --batch' has no window system on any platform, so a suite that
;; asserts anything about fringes, scroll bars or `window-body-width'
;; cannot run there.  A daemon can make a real graphical frame, so the
;; suite runs inside one, driven over `emacsclient -e'.
;;
;; ERT's own batch reporter writes through `message', which in a daemon
;; goes to *Messages* rather than to the client, so this file drives
;; `ert-run-test' itself and returns a printable report string.

;;; Code:

(require 'ert)

(defvar edmacs-gui-ert-frame nil
  "The graphical frame every test runs inside.")

(defun edmacs-gui-ert-make-frame ()
  "Create the graphical frame the suite runs in, and return it.
Positioned far off-screen and mapped without focus so a run cannot steal
the keyboard from whatever the user is doing.  Sized generously: a side
window plus a main window needs real width, and a frame too narrow makes
`display-buffer-in-side-window' return nil, which the suite reports as a
skip rather than a failure."
  (setq edmacs-gui-ert-frame
        (make-frame `((window-system . ,(cond ((eq system-type 'darwin) 'ns)
                                              (t (or (car (last (frame-parameter nil 'display)))
                                                     'x))))
                      (width . 160) (height . 48)
                      (left . 6000) (top . 6000)
                      (no-focus-on-map . t)
                      (no-accept-focus . t)))))

(defun edmacs-gui-ert-describe-frame ()
  "Return a one-line description of the frame the suite will run in."
  (let ((f edmacs-gui-ert-frame))
    (format "frame %sx%s  window-system=%S  char=%sx%s  fringes=%S/%S  scroll-bar=%S  graphic=%S"
            (frame-width f) (frame-height f) (window-system f)
            (frame-char-width f) (frame-char-height f)
            (frame-parameter f 'left-fringe) (frame-parameter f 'right-fringe)
            (frame-parameter f 'scroll-bar-width)
            (with-selected-frame f (display-graphic-p)))))

(defun edmacs-gui-ert--render (condition)
  "Render CONDITION as one safe, single-line string.
An ERT explanation can contain raw control characters -- a fringe-width
`should' that fails reports `(different-atoms (8 \"#x8\" \"?\\b\") (0 \"#x0\"
\"?^@\"))', and the NUL in there truncates the report on its way out
through `emacsclient'."
  (let ((text (format "%S" condition)))
    (replace-regexp-in-string "[[:cntrl:]]" "." (truncate-string-to-width text 400))))

(defun edmacs-gui-ert-run (selector)
  "Run every ERT test matching SELECTOR inside `edmacs-gui-ert-frame'.
A test whose `:expected-result' is `:failed' still fails here -- ERT
tracks the expectation on the test, not the result -- so classification
goes through `ert-test-result-expected-p' the same way ERT's own batch
reporter does, and only a genuinely UNEXPECTED result counts toward
FAILURE-COUNT.  Return a cons of (REPORT-STRING . FAILURE-COUNT)."
  (let ((tests (ert-select-tests selector t))
        (lines nil) (passed 0) (xfailed 0) (failed 0) (skipped 0))
    (with-selected-frame edmacs-gui-ert-frame
      (dolist (test tests)
        (let* ((result (ert-run-test test))
               (name (ert-test-name test))
               (condition (and (ert-test-result-with-condition-p result)
                                (edmacs-gui-ert--render
                                 (ert-test-result-with-condition-condition result)))))
          (cond
           ((ert-test-passed-p result)
            (setq passed (1+ passed))
            (push (format "   passed  %s" name) lines))
           ((ert-test-skipped-p result)
            (setq skipped (1+ skipped))
            (push (format "  skipped  %s -- %s" name condition) lines))
           ((ert-test-result-expected-p test result)
            (setq xfailed (1+ xfailed))
            (push (format "  xfailed  %s -- %s" name condition) lines))
           (t
            (setq failed (1+ failed))
            (push (format "   FAILED  %s\n            %s" name
                          (or condition "(no condition recorded)"))
                  lines))))))
    (cons (concat (edmacs-gui-ert-describe-frame) "\n"
                  (mapconcat #'identity (nreverse lines) "\n")
                  (format "\n\nRan %d tests, %d passed, %d expected-failed, %d failed, %d skipped\n"
                          (length tests) passed xfailed failed skipped))
          failed)))

(provide 'gui-ert)
;;; gui-ert.el ends here
