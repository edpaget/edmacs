;;; claude-lib-ert-live-test.el --- Real-subprocess round trip for claude-lib-ert.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tier 1 only -- no GUI tier needed, since nothing here is a pixel
;; assertion.  Writes a tiny synthetic two-test ERT suite to a temp
;; file (one fast test, one that sleeps 0.3s to reliably exceed a small
;; per-test budget without making this suite itself slow), then runs
;; `claude-lib-ert-durations' for real: a genuine `call-process' out to
;; `scripts/run-ert-suite.sh', which spawns a real `emacs -Q --batch'
;; subprocess against the fixture.  This is the one place the whole
;; plumbing -- subprocess spawn, ERT output parse, `(ert-deftest ...)'
;; location scan, `claude-lib-render' row construction, and
;; rasterisation -- is proven end to end rather than stage by stage.
;;
;; Invocation:
;;
;;   scripts/run-ert-suite.sh 60 \
;;     emacs -Q --batch -l ert \
;;           -l modules/claude-lib-view.el \
;;           -l modules/claude-lib-ert.el \
;;           -l modules/claude-lib-ert-live-test.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; Each test here spawns its own `emacs -Q --batch' child, so this
;; suite is slower than the pure-batch one (a few seconds, not
;; milliseconds) -- expected, not a regression.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'claude-lib-view)
(require 'claude-lib-ert)

;; See this repo's CLAUDE.md on the ~28s native-comp subr-trampoline
;; `cl-letf' forces on a C subr.  Nothing here `cl-letf's a subr, but
;; the guard travels with the file so a later addition cannot
;; reintroduce the cost silently.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

;; ============================================================================
;; Helpers
;; ============================================================================

(defmacro claude-lib-ert-live-test--with-views (&rest body)
  "Run BODY, killing every `*claude-view: ...*' buffer afterwards."
  (declare (indent 0) (debug t))
  `(unwind-protect (progn ,@body)
     (dolist (buf (buffer-list))
       (when (string-prefix-p "*claude-view: " (buffer-name buf))
         (kill-buffer buf)))))

(defun claude-lib-ert-live-test--require-rsvg ()
  "Skip unless the real `rsvg-convert' is on `exec-path'."
  (unless (executable-find claude-lib-render-rsvg-program)
    (ert-skip (format "needs %s on exec-path; a convenience for CHECKING \
output, never a requirement for producing it" claude-lib-render-rsvg-program))))

(defun claude-lib-ert-live-test--magic (path n)
  "Return PATH's first N bytes as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 n)
    (buffer-string)))

(defmacro claude-lib-ert-live-test--with-fixture-suite (var &rest body)
  "Bind VAR to a temp `.el' file with two `ert-deftest' forms, run BODY, delete it.
One test (`synthetic-fast') passes instantly; the other
(`synthetic-slow') sleeps 0.3s -- reliably over a 0.1s per-test budget
without making this live suite itself slow."
  (declare (indent 1) (debug t))
  `(let ((,var (make-temp-file "claude-lib-ert-live-fixture-" nil ".el")))
     (unwind-protect
         (progn
           (with-temp-file ,var
             (insert "(require 'ert)\n"
                     "(ert-deftest synthetic-fast () (should t))\n"
                     "(ert-deftest synthetic-slow () (sleep-for 0.3) (should t))\n"
                     "(provide 'claude-lib-ert-live-fixture)\n"))
           ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

;; ============================================================================
;; Tier 1 -- the real subprocess round trip
;; ============================================================================

(ert-deftest claude-lib-ert-live-test-real-run-produces-real-durations ()
  "Real per-row durations, from a real subprocess, distinguish fast from slow.
No static analysis of the fixture file could produce this -- it comes
only from actually running it."
  (claude-lib-ert-live-test--with-views
    (claude-lib-ert-live-test--with-fixture-suite fixture
      (let* ((command (list "emacs" "-Q" "--batch" "-l" "ert" "-l" fixture
                            "-f" "ert-run-tests-batch-and-exit"))
             (result (claude-lib-ert-durations default-directory command
                                               30 0.1
                                               :name "live-ert-durations"
                                               :display nil)))
        (should (= 2 (plist-get result :rows)))
        (should (eq (plist-get result :mode) 'unified))
        (should (equal (mapcar #'car (plist-get result :over-budget))
                       '("synthetic-slow")))
        (should (> (cdr (car (plist-get result :over-budget))) 0.1))
        (should (string-match-p "synthetic-slow" (plist-get result :summary)))
        (should (numberp (plist-get result :suite-elapsed)))
        (should (numberp (plist-get result :suite-budget)))
        (with-current-buffer (plist-get result :buffer)
          (should (eq major-mode 'claude-lib-view-mode))
          (goto-char (point-min))
          ;; Both rows present, and distinguishable by their own text.
          (should (search-forward "synthetic-fast" nil t))
          (should (search-forward "synthetic-slow" nil t)))))))

(ert-deftest claude-lib-ert-live-test-location-resolves-to-the-real-fixture ()
  "A row built from a REAL parsed run carries a :location that visits the fixture."
  (claude-lib-ert-live-test--with-views
    (claude-lib-ert-live-test--with-fixture-suite fixture
      (let* ((command (list "emacs" "-Q" "--batch" "-l" "ert" "-l" fixture
                            "-f" "ert-run-tests-batch-and-exit"))
             (result (claude-lib-ert-durations default-directory command
                                               30 0.1
                                               :name "live-ert-location"
                                               :display nil)))
        (with-current-buffer (plist-get result :buffer)
          (goto-char (point-min))
          (should (eq (tabulated-list-get-id) 'synthetic-fast))
          (let ((visited (claude-lib-view-visit)))
            (unwind-protect
                (with-current-buffer visited
                  (should (equal (expand-file-name (buffer-file-name))
                                (expand-file-name fixture)))
                  (should (looking-at-p "(ert-deftest synthetic-fast")))
              (kill-buffer visited))))))))

(ert-deftest claude-lib-ert-live-test-rasterizes-the-slow-row ()
  "`claude-lib-render-rasterize' on the over-budget row produces a real file.
PNG when `rsvg-convert' is found, `.svg' otherwise -- mirroring
`claude-lib-view-live-test.el's own degrade-gracefully coverage of that
exact fallback, so this phase does not assume PNG conversion always
succeeds."
  (claude-lib-ert-live-test--with-views
    (claude-lib-ert-live-test--with-fixture-suite fixture
      (let* ((command (list "emacs" "-Q" "--batch" "-l" "ert" "-l" fixture
                            "-f" "ert-run-tests-batch-and-exit"))
             (result (claude-lib-ert-durations default-directory command
                                               30 0.1
                                               :name "live-ert-rasterize"
                                               :display nil))
             (slow-id (car (car (plist-get result :over-budget))))
             (path (claude-lib-render-rasterize (plist-get result :buffer)
                                                (intern slow-id))))
        (unwind-protect
            (progn
              (should (file-exists-p path))
              (should (> (file-attribute-size (file-attributes path)) 0))
              (if (executable-find claude-lib-render-rsvg-program)
                  (progn
                    (should (string-suffix-p ".png" path))
                    (should (equal (claude-lib-ert-live-test--magic path 4)
                                  (unibyte-string 137 ?P ?N ?G))))
                (should (string-suffix-p ".svg" path))))
          (when (file-exists-p path) (delete-file path)))))))

(provide 'claude-lib-ert-live-test)
;;; claude-lib-ert-live-test.el ends here
