;;; wedge-trace-test.el --- Tests for wedge-trace.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-batch coverage of the wedge tracer: label rendering, the
;; append/rotate write path, the ENTER/EXIT bracketing the
;; `timer-event-handler' advice produces (including on a timer that
;; signals), and the verdict parser that reads the tail back.
;;
;; Nothing here wedges anything -- the failure this module exists to
;; attribute is a macOS NS event-loop hang that cannot be reproduced in
;; batch.  What is testable is that the record the hang would leave
;; behind is written, is written before the hanging call rather than
;; after it, and is read back correctly.
;;
;;   scripts/run-ert-suite.sh 30 \
;;     emacs -Q --batch -l ert \
;;           -l modules/test-support.el \
;;           -l modules/wedge-trace.el \
;;           -l modules/wedge-trace-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'wedge-trace)

;; No subr is `cl-letf''d here, so this guard is not load-bearing today;
;; it travels with the file per this repo's CLAUDE.md so a later
;; addition cannot reintroduce the ~28s native-comp trampoline cost.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

;; ============================================================================
;; Helpers
;; ============================================================================

(defmacro wedge-trace-test--with-trace-file (&rest body)
  "Run BODY with the trace and heartbeat files pointed at temp files."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "wedge-trace-test-" t))
          (edmacs-wedge-trace-file (expand-file-name "trace.log" dir))
          (edmacs-wedge-trace-heartbeat-file (expand-file-name "hb" dir))
          (edmacs-wedge-trace--writes 0))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun wedge-trace-test--contents ()
  "Return the trace file's contents, or \"\" when it does not exist."
  (if (file-exists-p edmacs-wedge-trace-file)
      (with-temp-buffer
        (insert-file-contents edmacs-wedge-trace-file)
        (buffer-string))
    ""))

(defun wedge-trace-test--timer-for (fn)
  "Return a timer object whose function is FN, never scheduled."
  (let ((tm (timer-create)))
    (timer-set-function tm fn)
    tm))

;; ============================================================================
;; Label rendering
;; ============================================================================

(ert-deftest wedge-trace-label-renders-a-symbol-as-its-name ()
  (should (equal (edmacs-wedge-trace--label 'my-timer-fn) "my-timer-fn")))

(ert-deftest wedge-trace-label-truncates-a-long-lambda ()
  (let* ((fn (lambda () (ignore (make-string 400 ?x))))
         (label (edmacs-wedge-trace--label fn)))
    (should (<= (length label) 80))
    ;; Single line: a multi-line record would break the tail parser.
    (should-not (string-match-p "\n" label))))

(ert-deftest wedge-trace-label-is-single-line-for-every-shape ()
  (dolist (fn (list 'sym (lambda (_x) _x) #'ignore))
    (should-not (string-match-p "\n" (edmacs-wedge-trace--label fn)))))

;; ============================================================================
;; The write path
;; ============================================================================

(ert-deftest wedge-trace-write-appends-rather-than-replacing ()
  (wedge-trace-test--with-trace-file
    (edmacs-wedge-trace--write "first")
    (edmacs-wedge-trace--write "second")
    (should (equal (wedge-trace-test--contents) "first\nsecond\n"))))

(ert-deftest wedge-trace-write-swallows-an-unwritable-path ()
  (wedge-trace-test--with-trace-file
    (let ((edmacs-wedge-trace-file "/nonexistent-dir-xyz/trace.log"))
      ;; Must not signal: this runs inside every timer firing.
      (should (null (edmacs-wedge-trace--write "boom"))))))

(ert-deftest wedge-trace-rotate-only-stats-every-200th-record ()
  (wedge-trace-test--with-trace-file
    (edmacs-wedge-trace--write (make-string 100 ?x))
    (let ((edmacs-wedge-trace-max-bytes 1))
      (dotimes (_ 199) (edmacs-wedge-trace--rotate-maybe))
      (should-not (file-exists-p (concat edmacs-wedge-trace-file ".1")))
      (edmacs-wedge-trace--rotate-maybe)
      (should (file-exists-p (concat edmacs-wedge-trace-file ".1"))))))

(ert-deftest wedge-trace-rotate-leaves-a-small-file-alone ()
  (wedge-trace-test--with-trace-file
    (edmacs-wedge-trace--write "small")
    (let ((edmacs-wedge-trace-max-bytes (* 1024 1024)))
      (dotimes (_ 200) (edmacs-wedge-trace--rotate-maybe))
      (should-not (file-exists-p (concat edmacs-wedge-trace-file ".1"))))))

;; ============================================================================
;; The timer advice
;; ============================================================================

(ert-deftest wedge-trace-around-timer-brackets-the-call ()
  (wedge-trace-test--with-trace-file
    (let ((ran nil))
      (edmacs-wedge-trace--around-timer
       (lambda (_tm) (setq ran t))
       (wedge-trace-test--timer-for 'some-timer-fn))
      (should ran)
      (let ((lines (split-string (string-trim (wedge-trace-test--contents)) "\n")))
        (should (= 2 (length lines)))
        (should (string-match-p " ENTER some-timer-fn\\'" (nth 0 lines)))
        (should (string-match-p " EXIT  some-timer-fn " (nth 1 lines)))))))

(ert-deftest wedge-trace-around-timer-writes-enter-before-calling ()
  ;; The whole point: the ENTER record must already be on disk when the
  ;; call hangs, not written afterwards.
  (wedge-trace-test--with-trace-file
    (edmacs-wedge-trace--around-timer
     (lambda (_tm)
       (should (string-match-p " ENTER hangs-here\n\\'"
                               (wedge-trace-test--contents))))
     (wedge-trace-test--timer-for 'hangs-here))))

(ert-deftest wedge-trace-around-timer-still-writes-exit-on-error ()
  (wedge-trace-test--with-trace-file
    (should-error
     (edmacs-wedge-trace--around-timer
      (lambda (_tm) (error "timer blew up"))
      (wedge-trace-test--timer-for 'exploding-fn)))
    (should (string-match-p " EXIT  exploding-fn " (wedge-trace-test--contents)))))

(ert-deftest wedge-trace-around-timer-propagates-the-return-value ()
  (wedge-trace-test--with-trace-file
    (should (equal 'result
                   (edmacs-wedge-trace--around-timer
                    (lambda (_tm) 'result)
                    (wedge-trace-test--timer-for 'returning-fn))))))

;; ============================================================================
;; Heartbeat
;; ============================================================================

(ert-deftest wedge-trace-heartbeat-overwrites-rather-than-appending ()
  (wedge-trace-test--with-trace-file
    (edmacs-wedge-trace--heartbeat)
    (edmacs-wedge-trace--heartbeat)
    (with-temp-buffer
      (insert-file-contents edmacs-wedge-trace-heartbeat-file)
      (should (= 1 (count-lines (point-min) (point-max))))
      (should (string-match-p (format "pid=%d" (emacs-pid)) (buffer-string))))))

;; ============================================================================
;; Reading the trace back
;; ============================================================================

(ert-deftest wedge-trace-tail-returns-the-last-n-records ()
  (wedge-trace-test--with-trace-file
    (dotimes (i 10) (edmacs-wedge-trace--write (format "line-%d" i)))
    (let ((lines (seq-remove #'string-empty-p
                             (split-string (edmacs-wedge-trace-tail 3) "\n"))))
      (should (equal lines '("line-7" "line-8" "line-9"))))))

(ert-deftest wedge-trace-tail-on-a-missing-file-is-empty ()
  (wedge-trace-test--with-trace-file
    (should (equal "" (string-trim (edmacs-wedge-trace-tail 5))))))

(ert-deftest wedge-trace-verdict-blames-an-unmatched-enter ()
  (wedge-trace-test--with-trace-file
    (edmacs-wedge-trace--write "2026-09-11 21:00:00.000 EXIT  earlier-fn 1.0ms")
    (edmacs-wedge-trace--write "2026-09-11 21:00:05.000 ENTER culprit-fn")
    (let ((msg (edmacs-wedge-trace-verdict)))
      (should (string-match-p "culprit-fn" msg))
      (should (string-match-p "never returned" msg)))))

(ert-deftest wedge-trace-verdict-points-at-redisplay-after-a-trailing-exit ()
  (wedge-trace-test--with-trace-file
    (edmacs-wedge-trace--write "2026-09-11 21:00:05.000 ENTER ticker-fn")
    (edmacs-wedge-trace--write "2026-09-11 21:00:05.010 EXIT  ticker-fn 10.0ms")
    (let ((msg (edmacs-wedge-trace-verdict)))
      (should (string-match-p "ticker-fn" msg))
      (should (string-match-p "redisplay" msg)))))

(ert-deftest wedge-trace-verdict-on-an-empty-trace-says-so ()
  (wedge-trace-test--with-trace-file
    (should (string-match-p "is empty" (edmacs-wedge-trace-verdict)))))

;; ============================================================================
;; The mode
;; ============================================================================

(ert-deftest wedge-trace-mode-installs-and-removes-its-hooks ()
  (edmacs-test-support-with-hermetic-state
   (wedge-trace-test--with-trace-file
     (unwind-protect
         (progn
           (edmacs-wedge-trace-mode 1)
           (should (advice-member-p #'edmacs-wedge-trace--around-timer
                                    'timer-event-handler))
           (should (timerp edmacs-wedge-trace--heartbeat-timer))
           (should (string-match-p " START pid=" (wedge-trace-test--contents))))
       (edmacs-wedge-trace-mode -1))
     (should-not (advice-member-p #'edmacs-wedge-trace--around-timer
                                  'timer-event-handler))
     (should (null edmacs-wedge-trace--heartbeat-timer))
     (should (string-match-p " STOP pid=" (wedge-trace-test--contents))))))

(ert-deftest wedge-trace-mode-leaves-redisplay-untraced-by-default ()
  (edmacs-test-support-with-hermetic-state
   (wedge-trace-test--with-trace-file
     (unwind-protect
         (progn
           (edmacs-wedge-trace-mode 1)
           (should-not (advice-member-p #'edmacs-wedge-trace--pre-redisplay
                                        'pre-redisplay-function)))
       (edmacs-wedge-trace-mode -1)))))

(provide 'wedge-trace-test)
;;; wedge-trace-test.el ends here
