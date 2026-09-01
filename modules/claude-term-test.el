;;; claude-term-test.el --- Tests for claude-term.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage only -- no live ghostel process is spawned here.
;; See claude-term-live-test.el for coverage of the kill/restart/exit
;; lifecycle, which drives claude-term--exec against real (non-ghostel)
;; subprocesses instead.
;; Run with:
;;   emacs -Q --batch -l ert -l modules/claude-term.el -l modules/claude-term-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; (Loading claude-term.el under `-Q' prints a benign "Unrecognized
;; keyword: :straight" notice from the `use-package ghostel' form, since
;; straight.el is not bootstrapped in this bare batch harness; that error
;; is caught internally by use-package and does not abort the load or
;; affect any test below.)

;;; Code:

(require 'ert)

(ert-deftest claude-term-test-leaf ()
  (should (equal (claude-term--leaf "/foo/bar-baz/") "bar-baz"))
  (should (equal (claude-term--leaf "/foo/bar-baz") "bar-baz")))

(ert-deftest claude-term-test-buffer-name-no-instance ()
  (should (equal (claude-term-buffer-name "/foo/bar-baz/") "*claude-term:bar-baz*")))

(ert-deftest claude-term-test-buffer-name-with-instance ()
  (should (equal (claude-term-buffer-name "/foo/bar-baz/" "2") "*claude-term:bar-baz:2*")))

(ert-deftest claude-term-test-parse-buffer-name-round-trip ()
  (should (equal (claude-term--parse-buffer-name "*claude-term:bar-baz:2*") '("bar-baz" . "2")))
  (should (equal (claude-term--parse-buffer-name "*claude-term:bar-baz*") '("bar-baz" . nil)))
  (should-not (claude-term--parse-buffer-name "*vterm*")))

(ert-deftest claude-term-test-spawn-args-appends-call-args ()
  (let ((claude-term-extra-args '("--foo")))
    (should (equal (claude-term--spawn-args '("--bar")) '("--foo" "--bar")))))

(ert-deftest claude-term-test-spawn-args-nil-extra-args ()
  (let ((claude-term-extra-args nil))
    (should (equal (claude-term--spawn-args nil) nil))
    (should (equal (claude-term--spawn-args '("--bar")) '("--bar")))))

(ert-deftest claude-term-test-spawn-args-nil-call-args ()
  (let ((claude-term-extra-args '("--foo")))
    (should (equal (claude-term--spawn-args nil) '("--foo")))))

;; ============================================================================
;; Side-window display
;; ============================================================================
;; These tests create REAL windows through `display-buffer-in-side-window',
;; which works fine under plain `emacs -Q --batch' with no display and no
;; ghostel or straight bootstrap needed (verified empirically before
;; writing these).  Each test resets the monotonic slot counter to 0 via
;; `claude-term-test--with-fresh-slots' and kills every buffer it creates
;; in an `unwind-protect', which also tears down the side window showing
;; it -- confirmed live: killing a side window's buffer deletes the
;; window automatically, no explicit `delete-window' needed.

(defmacro claude-term-test--with-fresh-slots (&rest body)
  "Run BODY with a fresh `claude-term--next-slot' counter."
  (declare (indent 0))
  `(let ((claude-term--next-slot 0))
     ,@body))

(ert-deftest claude-term-test-allocate-slot-stable-and-monotonic ()
  (claude-term-test--with-fresh-slots
    (let ((buf1 (generate-new-buffer "claude-term-test-slot-1"))
          (buf2 (generate-new-buffer "claude-term-test-slot-2")))
      (unwind-protect
          (progn
            (should (equal (claude-term--allocate-slot buf1) 0))
            ;; Re-allocating for the same buffer returns the same slot
            ;; rather than drawing a fresh one.
            (should (equal (claude-term--allocate-slot buf1) 0))
            (should (equal (claude-term--allocate-slot buf2) 1))
            (should (equal (claude-term--allocate-slot buf2) 1)))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

(ert-deftest claude-term-test-display-buffer-three-agents-stacked ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (bufs (list (generate-new-buffer "claude-term-test-agent-1")
                      (generate-new-buffer "claude-term-test-agent-2")
                      (generate-new-buffer "claude-term-test-agent-3"))))
      (unwind-protect
          (let ((windows (mapcar #'claude-term--display-buffer bufs)))
            (should (= (length windows) 3))
            (dolist (win windows)
              (should (window-live-p win))
              (should (eq (window-parameter win 'window-side) 'right))
              ;; Fractional width, not claude-code-ide.el's 100-column
              ;; default: well under 60% of the (80-column batch) frame.
              (should (< (window-width win) (* 0.6 (frame-width)))))
            ;; Three distinct slots, in creation order.
            (should (equal (mapcar (lambda (w) (window-parameter w 'window-slot)) windows)
                            '(0 1 2))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest claude-term-test-display-buffer-redisplay-reuses-slot ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-redisplay")))
      (unwind-protect
          (let* ((win1 (claude-term--display-buffer buf))
                 (slot1 (window-parameter win1 'window-slot))
                 (win2 (claude-term--display-buffer buf))
                 (slot2 (window-parameter win2 'window-slot)))
            (should (equal slot1 slot2)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-delete-other-windows-preserves-side-window ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-c-x-1")))
      (unwind-protect
          (let* ((win (claude-term--display-buffer buf))
                 (count-before (length (window-list))))
            (delete-other-windows)
            (should (= (length (window-list)) count-before))
            (should (window-live-p win))
            (should (eq (window-buffer win) buf)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-toggle-side-windows-preserves-size ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-toggle")))
      (unwind-protect
          (let* ((win (claude-term--display-buffer buf))
                 (width (window-width win)))
            (window-toggle-side-windows)
            (window-toggle-side-windows)
            (let ((win2 (get-buffer-window buf)))
              (should win2)
              (should (<= (abs (- (window-width win2) width)) 1))))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-display-buffer-dedicated-for-rotate ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-dedicated")))
      (unwind-protect
          (let ((win (claude-term--display-buffer buf)))
            ;; `rotate--window-list' filters with `(cl-remove-if
            ;; #'window-dedicated-p ...)'; side windows return the
            ;; symbol `side', which is non-nil.
            (should (window-dedicated-p win)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-no-other-window-excludes-side-window ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-no-other")))
      (unwind-protect
          (let* ((ordinary (selected-window))
                 (win (claude-term--display-buffer buf)))
            (should (window-no-other-p win))
            (select-window ordinary)
            (other-window 1)
            (should-not (eq (selected-window) win))
            (should-not (eq (window-in-direction 'right) win)))
        (kill-buffer buf)))))

;;; claude-term-test.el ends here
