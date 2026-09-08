;;; claude-lib-ert-test.el --- Tests for claude-lib-ert.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-batch coverage of the parsing, location-scan and budget
;; arithmetic behind `claude-lib-ert-durations' -- nothing here spawns
;; a subprocess.  The real subprocess round trip (a genuine
;; `scripts/run-ert-suite.sh' run against a synthetic fixture suite) is
;; `modules/claude-lib-ert-live-test.el'.
;;
;; Narrow invocation, the module alone:
;;
;;   scripts/run-ert-suite.sh 30 \
;;     emacs -Q --batch -l ert \
;;           -l modules/claude-lib-view.el \
;;           -l modules/claude-lib-ert.el \
;;           -l modules/claude-lib-ert-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'claude-lib-view)
(require 'claude-lib-ert)

;; No subr is `cl-letf''d here (nothing stubs `buffer-live-p',
;; `call-process', etc.) -- this guard is not load-bearing today, but
;; travels with the file per this repo's CLAUDE.md so a later addition
;; cannot reintroduce the ~28s native-comp trampoline cost silently.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

;; ============================================================================
;; Helpers
;; ============================================================================

(defmacro claude-lib-ert-test--with-fixture-file (var contents &rest body)
  "Bind VAR to a temp file's name written with CONTENTS, run BODY, then delete it."
  (declare (indent 2) (debug t))
  `(let ((,var (make-temp-file "claude-lib-ert-test-" nil ".el")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert ,contents))
           ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

;; ============================================================================
;; AC1 / AC4 -- parsing ERT's own batch-output shape
;; ============================================================================

(ert-deftest claude-lib-ert-test-parses-all-passed-run ()
  "A clean run's per-test lines parse into name/status/seconds triples."
  (let* ((text (concat
                "Running 2 tests (2026-09-07 00:00:00+0000, selector `t')\n"
                "   passed  1/2  test-fast (0.001234 sec)\n"
                "   passed  2/2  test-slow (1.500000 sec)\n"
                "\n"
                "Ran 2 tests, 2 results as expected, 0 unexpected (2026-09-07 00:00:02+0000, 1.501234 sec)\n"
                "\n"
                "Suite took 1.60s (budget 30s)\n"))
         (parsed (claude-lib--ert-parse-output text))
         (rows (plist-get parsed :rows)))
    (should (= 2 (length rows)))
    (should (equal (plist-get (nth 0 rows) :name) "test-fast"))
    (should (equal (plist-get (nth 0 rows) :status) "passed"))
    (should (= (plist-get (nth 0 rows) :seconds) 0.001234))
    (should (null (plist-get (nth 0 rows) :at-hint)))
    (should (equal (plist-get (nth 1 rows) :name) "test-slow"))
    (should (= (plist-get (nth 1 rows) :seconds) 1.5))
    (should (= (plist-get parsed :suite-elapsed) 1.6))
    (should (= (plist-get parsed :suite-budget) 30))))

(ert-deftest claude-lib-ert-test-parses-failed-line-with-at-hint ()
  "An unexpected result's own \"at FILE:LINE\" suffix becomes :at-hint."
  (let* ((text (concat
                "   FAILED  1/1  test-broken (0.250000 sec) at modules/foo-test.el:17\n"
                "Suite took 0.30s (budget 10s)\n"))
         (rows (plist-get (claude-lib--ert-parse-output text) :rows)))
    (should (= 1 (length rows)))
    (should (equal (plist-get (car rows) :status) "FAILED"))
    (should (equal (plist-get (car rows) :at-hint) (cons "modules/foo-test.el" 17)))))

(ert-deftest claude-lib-ert-test-tolerant-of-real-column-padding ()
  "The regexp does not hard-code a column width -- a bigger suite widens it."
  (let* ((text "     passed  9/123  test-name (0.010000 sec)\n")
         (rows (plist-get (claude-lib--ert-parse-output text) :rows)))
    (should (= 1 (length rows)))
    (should (equal (plist-get (car rows) :name) "test-name"))))

(ert-deftest claude-lib-ert-test-summary-line-alone-is-not-mistaken-for-a-test ()
  "ERT's own \"Ran N tests...\" line never parses as a per-test row."
  (let* ((text (concat
                "   passed  1/1  test-one (0.000100 sec)\n"
                "\n"
                "Ran 1 tests, 1 results as expected, 0 unexpected (2026-09-07, 0.000100 sec)\n"
                "Suite took 0.10s (budget 5s)\n"))
         (rows (plist-get (claude-lib--ert-parse-output text) :rows)))
    (should (= 1 (length rows)))))

(ert-deftest claude-lib-ert-test-zero-rows-is-a-loud-user-error ()
  "A broken COMMAND (wrong -f target, a load error) fails loudly, not silently."
  (let ((err (should-error (claude-lib--ert-parse-output "no test lines here at all\n")
                           :type 'user-error)))
    (should (string-match-p "no test lines here" (error-message-string err)))))

(ert-deftest claude-lib-ert-test-zero-rows-excerpt-is-bounded ()
  "The zero-rows error excerpts at most ~500 chars, never the whole output."
  (let* ((huge (make-string 5000 ?x))
         (err (should-error (claude-lib--ert-parse-output huge) :type 'user-error)))
    (should (< (length (error-message-string err)) 600))))

;; ============================================================================
;; AC4 -- location scan
;; ============================================================================

(ert-deftest claude-lib-ert-test-locate-scans-for-ert-deftest ()
  "A plain text scan finds each `(ert-deftest NAME' at its real line."
  (claude-lib-ert-test--with-fixture-file file
      (concat ";;; fixture -*- lexical-binding: t -*-\n"
              "(require 'ert)\n"
              "\n"
              "(ert-deftest fixture-one ()\n"
              "  (should t))\n"
              "\n"
              "(ert-deftest fixture-two ()\n"
              "  (should t))\n")
    (should (equal (claude-lib--ert-locate "fixture-one" nil (list file))
                   (cons file 4)))
    (should (equal (claude-lib--ert-locate "fixture-two" nil (list file))
                   (cons file 7)))))

(ert-deftest claude-lib-ert-test-locate-prefers-at-hint ()
  "A supplied AT-HINT is used directly, never re-derived by scanning."
  (should (equal (claude-lib--ert-locate "anything" (cons "elsewhere.el" 99) nil)
                (cons "elsewhere.el" 99))))

(ert-deftest claude-lib-ert-test-locate-returns-nil-when-not-found ()
  "A test absent from every candidate file resolves to nil, not an error."
  (claude-lib-ert-test--with-fixture-file file
      "(ert-deftest fixture-one () (should t))\n"
    (should (null (claude-lib--ert-locate "no-such-test" nil (list file))))))

(ert-deftest claude-lib-ert-test-locate-picks-first-file-in-order ()
  "Two files defining the same test name resolve to the FIRST file, deterministically."
  (claude-lib-ert-test--with-fixture-file file-a "(ert-deftest dup () (should t))\n"
    (claude-lib-ert-test--with-fixture-file file-b "(ert-deftest dup () (should t))\n"
      (should (equal (claude-lib--ert-locate "dup" nil (list file-a file-b))
                     (cons file-a 1)))
      (should (equal (claude-lib--ert-locate "dup" nil (list file-b file-a))
                     (cons file-b 1))))))

(ert-deftest claude-lib-ert-test-files-from-command-reads-dash-l-flags ()
  "The default :files set is every `-l FILE' argument, resolved against ROOT.
Includes a bare `-l ert' -- it resolves to a non-existent path under
ROOT, which `claude-lib--ert-locate' simply skips via its own
`file-exists-p' guard, exactly as it would skip any other candidate a
test is not defined in."
  (let ((command '("emacs" "-Q" "--batch" "-l" "ert" "-l" "modules/a.el"
                   "-l" "modules/b-test.el" "-f" "ert-run-tests-batch-and-exit")))
    (should (equal (claude-lib--ert-files-from-command command "/root/")
                  (list (expand-file-name "ert" "/root/")
                        (expand-file-name "modules/a.el" "/root/")
                        (expand-file-name "modules/b-test.el" "/root/"))))))

;; ============================================================================
;; AC1 / AC5 -- rows, budget arithmetic, summary augmentation
;; ============================================================================

(ert-deftest claude-lib-ert-test-build-rows-is-render-shaped ()
  "The constructed rows satisfy `claude-lib--view-validate-spec's own contract."
  (let* ((parsed-rows (list (list :name "a" :status "passed" :seconds 0.1 :at-hint nil)
                            (list :name "b" :status "FAILED" :seconds 2.0
                                  :at-hint (cons "x.el" 3))))
         (rows (claude-lib--ert-build-rows parsed-rows nil)))
    (should (= 2 (length rows)))
    (should (eq (plist-get (nth 0 rows) :id) 'a))
    (should (equal (plist-get (nth 0 rows) :cells) '("a" "passed")))
    (should (= (plist-get (nth 0 rows) :bar) 0.1))
    (should (null (plist-get (nth 0 rows) :location)))
    (should (equal (plist-get (nth 1 rows) :location) '("x.el" . 3)))
    ;; Doesn't signal against the substrate's own validator: 2 cells per
    ;; row, matching a 2-column spec.
    (claude-lib--view-validate-spec "s" [("Test" 10 t) ("Status" 10 t)] rows nil)
    (should t)))

(ert-deftest claude-lib-ert-test-bar-max-never-below-per-test-budget ()
  "`:bar-max' is never smaller than PER-TEST-BUDGET, even when every test is instant."
  (let ((rows (list (list :seconds 0.0) (list :seconds 0.0))))
    (should (= 5 (claude-lib--ert-bar-max 5 rows))))
  (let ((rows (list (list :seconds 1.0) (list :seconds 9.0))))
    (should (= 9.0 (claude-lib--ert-bar-max 5 rows)))))

(ert-deftest claude-lib-ert-test-over-budget-sorted-slowest-first ()
  "Rows over budget come back slowest-first; rows at or under budget are excluded."
  (let ((rows (list (list :name "at-budget" :seconds 5.0)
                    (list :name "slowest" :seconds 20.0)
                    (list :name "mid" :seconds 8.0)
                    (list :name "fast" :seconds 0.1))))
    (should (equal (claude-lib--ert-over-budget rows 5.0)
                  (list (cons "slowest" 20.0) (cons "mid" 8.0))))))

(ert-deftest claude-lib-ert-test-summary-clause-names-offenders ()
  "The clause names the offending test and its duration against the budget."
  (let ((clause (claude-lib--ert-summary-clause '(("slow-one" . 28.1)) 5 60)))
    (should (string-match-p "slow-one" clause))
    (should (string-match-p "28.10" clause))
    (should (string-match-p "60 tests" clause))
    (should (string-match-p "5s" clause))))

(ert-deftest claude-lib-ert-test-summary-clause-nil-when-nothing-over ()
  "No violation clause at all when nothing is over budget."
  (should (null (claude-lib--ert-summary-clause nil 5 10))))

(ert-deftest claude-lib-ert-test-summary-clause-caps-the-name-list ()
  "A large over-budget list is capped in the clause, not dumped in full."
  (let* ((over (mapcar (lambda (i) (cons (format "t%d" i) (float (+ 10 i))))
                       (number-sequence 1 30)))
         (clause (claude-lib--ert-summary-clause over 5 30)))
    (should (string-match-p "and 10 more" clause))))

(ert-deftest claude-lib-ert-test-aggregate-clause-states-figures ()
  "The aggregate clause states pass/fail and both elapsed/budget figures."
  (should (string-match-p "passed in 1.5s (aggregate budget 30s)"
                          (claude-lib--ert-aggregate-clause 0 1.5 30)))
  (should (string-match-p "reported a failure"
                          (claude-lib--ert-aggregate-clause 1 1.5 30)))
  (should (null (claude-lib--ert-aggregate-clause 0 nil nil))))

;; ============================================================================
;; Argument validation
;; ============================================================================

(ert-deftest claude-lib-ert-test-rejects-non-positive-budgets ()
  "Zero, negative, or a per-test budget above the suite budget all `user-error'."
  (should-error (claude-lib-ert-durations "/tmp" '("emacs") 0 1) :type 'user-error)
  (should-error (claude-lib-ert-durations "/tmp" '("emacs") 30 0) :type 'user-error)
  (should-error (claude-lib-ert-durations "/tmp" '("emacs") 30 -1) :type 'user-error)
  (should-error (claude-lib-ert-durations "/tmp" '("emacs") 5 10) :type 'user-error))

(ert-deftest claude-lib-ert-test-rejects-init-directory ()
  "COMMAND carrying --init-directory is refused before anything runs."
  (should-error
   (claude-lib-ert-durations "/tmp" '("emacs" "--init-directory=/tmp" "--batch") 30 5)
   :type 'user-error))

(ert-deftest claude-lib-ert-test-rejects-empty-root-or-command ()
  "An empty ROOT or COMMAND is rejected before any subprocess is spawned."
  (should-error (claude-lib-ert-durations "" '("emacs") 30 5) :type 'user-error)
  (should-error (claude-lib-ert-durations "/tmp" nil 30 5) :type 'user-error))

(provide 'claude-lib-ert-test)
;;; claude-lib-ert-test.el ends here
