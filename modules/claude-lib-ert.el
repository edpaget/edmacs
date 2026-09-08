;;; claude-lib-ert.el --- ERT per-test wall-clock durations as a claude-lib view -*- lexical-binding: t -*-

;;; Commentary:
;; ONE consumer on the phase-4 rendering substrate
;; (`claude-lib-render'/`claude-lib-render-rasterize', see
;; `modules/claude-lib-view.el'): a per-test wall-clock duration view
;; for one ERT suite, rendered through the UNIFIED shape (rows plus an
;; inline SVG bar) with a per-test budget line drawn across every bar.
;;
;; WHY THIS VIEW: `modules/windows-test.el' once regressed from 4.2s to
;; 282s from an un-guarded native-comp trampoline compile and reported
;; "108/108 passed" the whole time -- ERT has no notion of a suite
;; taking too long.  `scripts/run-ert-suite.sh' answers with a single
;; aggregate scalar budget: pass or fail, never which test and by how
;; much.  This view answers that in one glance.
;;
;; THE DATA PRODUCERS ALREADY EXIST, reused unmodified:
;;   - `scripts/run-ert-suite.sh' wraps a batch ERT invocation with an
;;     aggregate wall-clock budget and appends a trailing
;;     "Suite took Xs (budget Ys)" line.
;;   - ERT's own batch runner (`ert-run-tests-batch', core `ert.el')
;;     already prints one line per test in the shape
;;     "<status>  N/M  <test> (D.DDDDDD sec)", with " at FILE:LINE"
;;     appended only when the result is unexpected (confirmed by
;;     reading `ert-run-tests-batch's `message' calls directly).
;;
;; RET-TO-SOURCE needs no daemon-side `load' or `find-function': each
;; test's `(ert-deftest NAME' is found by a plain text scan of the
;; suite's own source files, opened via `insert-file-contents' into a
;; temporary buffer -- never a live buffer, so nothing is loaded into
;; the running daemon's obarray.  A failed test's own "at FILE:LINE"
;; suffix (ERT already computed it via
;; `find-function-search-for-symbol') is used first, as a free
;; cross-check, when the parser finds one.
;;
;; NAMESPACE: `claude-lib-ert-durations' is the one entry point (the
;; `claude-lib-' library's "\\`claude-lib-[^-]" entry-point convention,
;; see `modules/claude-lib.el'); `claude-lib--ert-NAME' are internal
;; helpers.  Hand-written with its own tests, exactly like
;; `modules/claude-lib-view.el' -- neither file goes through
;; `claude-lib-promote'.
;;
;; THE ROUND TRIP IS THE DOCUMENTED WORKFLOW for a regression claim,
;; not just available machinery: call this function, call
;; `claude-lib-render-rasterize' on the returned buffer with the
;; slowest (or any `:over-budget') row's id, and read the resulting
;; path with an ordinary file-reading tool before asserting a
;; regression in prose.  The per-test figures also stay in the
;; returned `:summary' text and the structured `:over-budget' list, so
;; the claim stays checkable even when nobody rasterises anything.
;;
;; PHASE-4 EXTENSION MADE FROM HERE: this view needed a way to draw a
;; shared threshold across every row's bar, which the substrate did
;; not have.  Rather than working around it, `modules/claude-lib-view.el'
;; gained a `:bar-threshold' keyword on `claude-lib-render' -- see that
;; file's own commit for the rationale; this file's job is only to use
;; it.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'claude-lib-view)

;; ============================================================================
;; Constants
;; ============================================================================

(defconst claude-lib--ert-run-script "scripts/run-ert-suite.sh"
  "Path, relative to ROOT, of the suite-budget wrapper shelled out to.")

(defconst claude-lib--ert-line-regexp
  (concat "^[[:space:]]*\\([[:alpha:]]+\\)[[:space:]]+"
          "[0-9]+/[0-9]+[[:space:]]+"
          "\\(\\S-+\\)[[:space:]]+"
          "(\\([0-9.]+\\)[[:space:]]+sec)"
          "\\(?:[[:space:]]+at[[:space:]]+\\(\\S-+\\)\\)?")
  "Matches one ERT batch per-test line.
Group 1 the status word (\"passed\"/\"PASSED\"/\"failed\"/\"FAILED\"/...,
per `ert-string-for-test-result'), group 2 the test symbol's printed
name, group 3 the duration in seconds, group 4 (optional) the
\"FILE:LINE\" that follows \" at \" on an unexpected result -- ERT's own
`ert-test-location', a free cross-check when present.  Deliberately
loose on whitespace: the real line right-justifies the status and
position fields to a width that depends on the suite's own test count,
so this never hard-codes a column position.")

(defconst claude-lib--ert-suite-line-regexp
  "Suite took \\([0-9.]+\\)s (budget \\([0-9.]+\\)s)"
  "Matches `scripts/run-ert-suite.sh's own trailing summary line.")

(defconst claude-lib--ert-over-budget-name-cap 20
  "Most offending test names named in the summary clause.
Mirrors `claude-lib-render-rasterize's own 20-id cap: a badly regressed
suite must not put hundreds of names back through the output-byte
ceiling.")

;; ============================================================================
;; Running and parsing
;; ============================================================================

(defun claude-lib--ert-run (root suite-budget command)
  "Run COMMAND under `scripts/run-ert-suite.sh's SUITE-BUDGET, inside ROOT.
Returns (TEXT . EXIT-STATUS), TEXT the combined stdout+stderr.  ROOT is
bound as `default-directory' explicitly from the parameter -- never
read ambiently -- so relative paths inside COMMAND (e.g. \"-l
modules/windows-test.el\") resolve against the caller's project, not
whatever buffer happened to be current in the daemon."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (script (expand-file-name claude-lib--ert-run-script root)))
    (with-temp-buffer
      (let ((default-directory root))
        (let ((status (apply #'call-process script nil t nil
                             (cons (number-to-string suite-budget) command))))
          (cons (buffer-string) status))))))

(defun claude-lib--ert-parse-output (text)
  "Parse TEXT (ERT batch output plus run-ert-suite.sh's trailing line).
Returns a plist (:rows ROWS :suite-elapsed FLOAT-or-nil :suite-budget
FLOAT-or-nil).  ROWS is a list of plists (:name STRING :status STRING
:seconds FLOAT :at-hint (FILE . LINE)-or-nil), one per parsed per-test
line, in the order ERT printed them.

Signals a `user-error', bounded to the first 500 characters of TEXT,
when zero per-test lines are found -- a broken COMMAND (a wrong `-f'
target, a load error before any test runs, `ert-quiet' bound non-nil)
must fail loudly rather than render an empty or bogus view."
  (let (rows suite-elapsed suite-budget)
    (dolist (line (split-string text "\n"))
      (when (string-match claude-lib--ert-line-regexp line)
        (let* ((status (match-string 1 line))
               (name (match-string 2 line))
               (seconds (string-to-number (match-string 3 line)))
               (at (match-string 4 line))
               (at-hint (and at
                            (string-match "\\`\\(.*\\):\\([0-9]+\\)\\'" at)
                            (cons (match-string 1 at)
                                  (string-to-number (match-string 2 at))))))
          (push (list :name name :status status :seconds seconds :at-hint at-hint)
                rows)))
      (when (string-match claude-lib--ert-suite-line-regexp line)
        (setq suite-elapsed (string-to-number (match-string 1 line))
              suite-budget (string-to-number (match-string 2 line)))))
    (setq rows (nreverse rows))
    (unless rows
      (user-error "claude-lib-ert-durations: no per-test lines found in ERT output; got: %s"
                  (substring text 0 (min 500 (length text)))))
    (list :rows rows :suite-elapsed suite-elapsed :suite-budget suite-budget)))

;; ============================================================================
;; Source locations
;; ============================================================================

(defun claude-lib--ert-locate (name at-hint files)
  "Return (FILE . LINE) for the `ert-deftest' named NAME, or nil.
AT-HINT, from a failed test's own \"at FILE:LINE\" suffix, is used
directly when present -- ERT already resolved it via
`find-function-search-for-symbol'.  Otherwise each of FILES is
text-scanned in order, via `insert-file-contents' into a temporary
buffer (never a live buffer -- no `load' and no `find-function' touch
the running daemon's obarray), for the literal substring
\"(ert-deftest NAME\".  The first file that contains it wins, so two
same-named tests defined in different files resolve deterministically
to FILES's own order rather than erroring or duplicating.  Returns nil
when no candidate file contains it -- a dynamically-defined test, or a
file omitted from FILES -- rather than signalling: the row still
renders, and RET on it signals `claude-lib-view-visit's own \"carries
no :location\" error instead of the whole render failing."
  (or at-hint
      (let ((needle (format "(ert-deftest %s" name))
            found)
        (dolist (file files)
          (unless found
            (when (file-exists-p file)
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (when (search-forward needle nil t)
                  (setq found (cons file (line-number-at-pos))))))))
        found)))

(defun claude-lib--ert-files-from-command (command root)
  "Return the `-l FILE' arguments in COMMAND, resolved against ROOT.
The default :files for `claude-lib-ert-durations' when none is given
explicitly: every source file the wrapped ERT invocation itself loads
is a candidate to scan for `(ert-deftest ...)' forms."
  (let (files (rest command))
    (while rest
      (if (and (equal (car rest) "-l") (cadr rest))
          (progn (push (expand-file-name (cadr rest) root) files)
                 (setq rest (cddr rest)))
        (setq rest (cdr rest))))
    (nreverse files)))

;; ============================================================================
;; Rows, budget arithmetic, summary
;; ============================================================================

(defun claude-lib--ert-build-rows (parsed-rows files)
  "Return `claude-lib-render'-shaped :rows for PARSED-ROWS.
Each PARSED-ROWS element (see `claude-lib--ert-parse-output') becomes
one row: :id the interned test name (ERT guarantees test names are
unique within a single run, so this can never collide), :cells the
name and status strings, :bar the duration, and :location whatever
`claude-lib--ert-locate' resolves from FILES."
  (mapcar
   (lambda (r)
     (let* ((name (plist-get r :name))
            (loc (claude-lib--ert-locate name (plist-get r :at-hint) files)))
       (list :id (intern name)
             :cells (list name (plist-get r :status))
             :bar (plist-get r :seconds)
             :location loc)))
   parsed-rows))

(defun claude-lib--ert-bar-max (per-test-budget rows)
  "Return the bar-chart ceiling for ROWS' :seconds against PER-TEST-BUDGET.
Never less than PER-TEST-BUDGET, so the threshold line drawn at that
value always sits inside the bar's own width, even when every observed
duration is smaller (every test finishes near-instantly)."
  (apply #'max per-test-budget (mapcar (lambda (r) (plist-get r :seconds)) rows)))

(defun claude-lib--ert-over-budget (rows per-test-budget)
  "Return ROWS whose :seconds exceeds PER-TEST-BUDGET, slowest first.
Each element is (NAME . SECONDS)."
  (let ((over (seq-filter (lambda (r) (> (plist-get r :seconds) per-test-budget)) rows)))
    (sort (mapcar (lambda (r) (cons (plist-get r :name) (plist-get r :seconds))) over)
          (lambda (a b) (> (cdr a) (cdr b))))))

(defun claude-lib--ert-summary-clause (over-budget per-test-budget total)
  "Return a clause naming OVER-BUDGET tests against PER-TEST-BUDGET, or nil.
TOTAL is the suite's test count.  Capped to
`claude-lib--ert-over-budget-name-cap' names so a badly regressed suite
cannot blow past the output-byte ceiling."
  (when over-budget
    (format "; %d of %d tests exceeded the %gs per-test budget: %s%s"
            (length over-budget) total (float per-test-budget)
            (mapconcat (lambda (pair) (format "%s (%.2fs)" (car pair) (cdr pair)))
                       (seq-take over-budget claude-lib--ert-over-budget-name-cap)
                       ", ")
            (if (> (length over-budget) claude-lib--ert-over-budget-name-cap)
                (format ", and %d more"
                        (- (length over-budget) claude-lib--ert-over-budget-name-cap))
              ""))))

(defun claude-lib--ert-aggregate-clause (exit-status suite-elapsed suite-budget)
  "Return a clause naming the aggregate run's outcome and figures, or nil.
`scripts/run-ert-suite.sh' can exit non-zero for two independent
reasons -- the wrapped ERT run itself reported an unexpected result, or
the aggregate wall clock exceeded SUITE-BUDGET -- and neither is fatal
to rendering the per-test view.  Both figures are stated in text rather
than left implicit, so the render call never masks a suite-level
failure that happened not to involve any single over-budget test."
  (when (and suite-elapsed suite-budget)
    (format "; suite %s in %ss (aggregate budget %ss)"
            (if (zerop exit-status) "passed" "reported a failure")
            suite-elapsed suite-budget)))

;; ============================================================================
;; The entry point
;; ============================================================================

;; Added 2026-09-07: proves the phase-4 substrate carries a real
;; consumer -- ERT's own batch output, parsed and rendered through the
;; unified rows+bar shape with a budget line, so a suite regression the
;; ERT summary line reports as green is visible on screen instead.
;; Destination: edmacs.
(defun claude-lib-ert-durations (root command suite-budget per-test-budget &rest keys)
  "Run an ERT suite, render its per-test wall-clock durations, and return
a summary plist naming the buffer and any test over PER-TEST-BUDGET.

ROOT is the project root COMMAND runs in -- required, never ambient
`default-directory'.  COMMAND is the argv list for the ERT batch
invocation forwarded to `scripts/run-ert-suite.sh', e.g.:

  (\"emacs\" \"-Q\" \"--batch\" \"-l\" \"ert\"
   \"-l\" \"modules/git-common-dir.el\" \"-l\" \"modules/windows.el\"
   \"-l\" \"modules/windows-test.el\" \"-f\" \"ert-run-tests-batch-and-exit\")

COMMAND must never contain `--init-directory': that flag makes its
argument a full `user-emacs-directory' and bootstraps a second package
tree when ROOT is a worktree (see this repo's CLAUDE.md, Worktrees).

SUITE-BUDGET is the aggregate seconds forwarded to
`scripts/run-ert-suite.sh'.  PER-TEST-BUDGET is the smaller, per-row
threshold this view draws as a dashed line across every bar -- the
whole point is that a single slow test can hide inside a suite still
under its own aggregate budget.  Both must be positive numbers, and
PER-TEST-BUDGET may not exceed SUITE-BUDGET.

KEYS:
  :name     buffer base name, forwarded to `claude-lib-render'.
  :display  forwarded to `claude-lib-render' (default t).
  :files    explicit list of source files to scan for
            `(ert-deftest ...)' locations; defaults to every file
            argument following an `-l' flag in COMMAND, resolved
            against ROOT.

Returns everything `claude-lib-render' returns (:buffer :mode :rows
:summary :displayed :displaced :truncated) plus :over-budget (a list
of (NAME . SECONDS), slowest first), :per-test-budget, :suite-elapsed
and :suite-budget -- a distinct, richer contract from
`claude-lib-render's own frozen return shape, since this is a
different entry point rather than a second implementation of it.

THE ROUND TRIP is the documented workflow for a regression claim: call
this function, then call `claude-lib-render-rasterize' on the returned
:buffer with the slowest (or any :over-budget) row's id, and read the
resulting path with an ordinary file-reading tool before asserting a
regression in prose -- a claim about the figures, never about a
picture nobody looked at."
  (unless (and (stringp root) (not (string-empty-p root)))
    (user-error "claude-lib-ert-durations: ROOT must be a non-empty string"))
  (unless (and (listp command) command (seq-every-p #'stringp command))
    (user-error "claude-lib-ert-durations: COMMAND must be a non-empty list of strings"))
  (when (seq-some (lambda (arg) (string-prefix-p "--init-directory" arg)) command)
    (user-error "claude-lib-ert-durations: COMMAND must not pass --init-directory (see CLAUDE.md, Worktrees)"))
  (unless (and (numberp suite-budget) (> suite-budget 0))
    (user-error "claude-lib-ert-durations: SUITE-BUDGET must be a positive number: %S" suite-budget))
  (unless (and (numberp per-test-budget) (> per-test-budget 0))
    (user-error "claude-lib-ert-durations: PER-TEST-BUDGET must be a positive number: %S" per-test-budget))
  (when (> per-test-budget suite-budget)
    (user-error "claude-lib-ert-durations: PER-TEST-BUDGET (%s) exceeds SUITE-BUDGET (%s)"
                per-test-budget suite-budget))
  (let* ((root (expand-file-name root))
         (files (or (plist-get keys :files)
                    (claude-lib--ert-files-from-command command root)))
         (name (plist-get keys :name))
         (display (if (plist-member keys :display) (plist-get keys :display) t))
         (run (claude-lib--ert-run root suite-budget command))
         (exit-status (cdr run))
         (parsed (claude-lib--ert-parse-output (car run)))
         (parsed-rows (plist-get parsed :rows))
         (bar-max (claude-lib--ert-bar-max per-test-budget parsed-rows))
         (over-budget (claude-lib--ert-over-budget parsed-rows per-test-budget))
         (rows (claude-lib--ert-build-rows parsed-rows files))
         (base-summary (format "ERT durations (%d tests)" (length parsed-rows)))
         (render-result (claude-lib-render base-summary
                                           :columns [("Test" 48 t) ("Status" 10 t)]
                                           :rows rows
                                           :bar-max bar-max
                                           :bar-threshold per-test-budget
                                           :name name
                                           :display display))
         (aggregate-clause (claude-lib--ert-aggregate-clause
                            exit-status (plist-get parsed :suite-elapsed)
                            (plist-get parsed :suite-budget)))
         (budget-clause (claude-lib--ert-summary-clause
                         over-budget per-test-budget (length parsed-rows))))
    (list :buffer (plist-get render-result :buffer)
          :mode (plist-get render-result :mode)
          :rows (plist-get render-result :rows)
          :summary (concat (plist-get render-result :summary)
                           (or aggregate-clause "")
                           (or budget-clause ""))
          :displayed (plist-get render-result :displayed)
          :displaced (plist-get render-result :displaced)
          :truncated (plist-get render-result :truncated)
          :over-budget over-budget
          :per-test-budget per-test-budget
          :suite-elapsed (plist-get parsed :suite-elapsed)
          :suite-budget (plist-get parsed :suite-budget))))

(provide 'claude-lib-ert)
;;; claude-lib-ert.el ends here
