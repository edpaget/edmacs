;;; claude-lib-test.el --- Tests for claude-lib.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage of `edmacs-claude-lib-eval-file' and its
;; helpers: multi-form reading (AC6), string-vs-prin1 value formatting
;; and unescaped multi-line output (AC2/AC5), interleaved print/message
;; capture with `*Messages*' left intact (AC2), explicit ROOT binding
;; and restoration (AC4), error propagation with partial-output
;; preservation and advice cleanup (AC3), and hard truncation (AC7). No
;; real `emacsclient' subprocess is exercised here -- that is
;; `claude-lib-live-test.el's job, driving the identical function
;; through the real transport.
;;
;; Also covers the phase-3 promotion library built on top of that same
;; file: discovery-form regression tests against the seeded
;; `claude-lib-demo' (its own AC2), `claude-lib-promote's validation
;; gates and provenance formatting (AC3/AC4), the no-gptel/no-registry
;; scope guard (AC5), and the `claude-lib-relevant-functions'
;; safe-local-variable predicate plus behavioral proof that the
;; documented interactive-driving primitives (`completing-read-function',
;; `unread-command-events', `select-window' pinning) actually work the
;; way the convention describes (AC6). Every `claude-lib-promote' test
;; operates on a temp-directory copy of the real file (see
;; `claude-lib-test--with-temp-library') and never mutates the
;; checked-in modules/claude-lib.el. The cross-process persistence
;; check for phase-3's AC1 (a promoted function surviving into a
;; second, separate Emacs process) lives in claude-lib-live-test.el
;; instead, since it needs a real forked `emacs' subprocess.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/claude-lib.el \
;;         -l modules/claude-lib-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elisp-mode)

;; Forward declarations so this file byte-compiles clean standalone
;; (its own header invocation loads claude-lib.el first, which already
;; defines both; this is only for a bare `batch-byte-compile' pass).
(defvar edmacs-claude-lib-max-output-bytes)
(defvar claude-lib-file)
(defvar claude-lib-relevant-functions)
(declare-function edmacs-claude-lib-eval-file "claude-lib" (form-file output-file root))
(declare-function claude-lib-demo "claude-lib" (root &optional depth))
(declare-function claude-lib-promote "claude-lib" (source destination problem &optional allow-redefine))

(defvar claude-lib-test--repo-root
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's own directory (modules/), used to locate claude-lib.el
regardless of the caller's `default-directory'.")

(defmacro claude-lib-test--with-temp-library (var &rest body)
  "Bind VAR and the dynamic `claude-lib-file' to a temp copy of the
real library, run BODY, then kill any buffer left visiting it and
delete the temp file. Never touches the checked-in modules/claude-lib.el."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "claude-lib-test-promote-lib" nil ".el"))
          (claude-lib-file ,var))
     (unwind-protect
         (progn
           (copy-file (expand-file-name "claude-lib.el" claude-lib-test--repo-root)
                      ,var t)
           ,@body)
       (let ((buf (get-file-buffer ,var)))
         (when buf (kill-buffer buf)))
       (ignore-errors (delete-file ,var)))))

(defmacro claude-lib-test--with-form-file (content var &rest body)
  "Bind VAR to a fresh temp file containing CONTENT, run BODY, then delete it."
  (declare (indent 2))
  `(let ((,var (make-temp-file "claude-lib-test-form")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert ,content))
           ,@body)
       (delete-file ,var))))

(defmacro claude-lib-test--with-output-file (var &rest body)
  "Bind VAR to a not-yet-existing OUTPUT-FILE path, run BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (make-temp-name (expand-file-name "claude-lib-test-output" temporary-file-directory))))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(defun claude-lib-test--read-output (output-file)
  "Read OUTPUT-FILE back as plain text, the way the Bash caller would."
  (with-temp-buffer
    (insert-file-contents output-file)
    (buffer-string)))

;; ============================================================================
;; AC6 -- read ALL forms, not just the first
;; ============================================================================

(ert-deftest claude-lib-test-evaluates-every-form-last-wins ()
  (claude-lib-test--with-form-file "(+ 1 2)\n(+ 3 4)\n" form-file
    (claude-lib-test--with-output-file output-file
      (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory)
      (let ((out (claude-lib-test--read-output output-file)))
        (should (string-match-p "=== value ===\n7\\'" out))))))

(ert-deftest claude-lib-test-empty-form-file-errors ()
  (claude-lib-test--with-form-file "" form-file
    (claude-lib-test--with-output-file output-file
      (should-error (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory)))))

(ert-deftest claude-lib-test-comment-only-form-file-errors ()
  (claude-lib-test--with-form-file ";; just a comment\n" form-file
    (claude-lib-test--with-output-file output-file
      (should-error (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory)))))

(ert-deftest claude-lib-test-truncated-trailing-form-errors-not-clean-eof ()
  "An unbalanced trailing form must not be silently treated as \"no more forms\"."
  (claude-lib-test--with-form-file "(+ 1 2)\n(+ 3 4" form-file
    (claude-lib-test--with-output-file output-file
      (should-error (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory)))))

;; ============================================================================
;; AC2 -- both the return value and what the form printed
;; ============================================================================

(ert-deftest claude-lib-test-printed-and-value-and-messages-preserved ()
  (claude-lib-test--with-form-file "(message \"x\")\n(print \"y\")\n99\n" form-file
    (claude-lib-test--with-output-file output-file
      (let ((messages-before (with-current-buffer (messages-buffer) (buffer-string))))
        (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory)
        (let ((out (claude-lib-test--read-output output-file)))
          ;; x (from message) then y (from print), in that order, before the value section.
          (should (string-match-p "x\\(.\\|\n\\)*y\\(.\\|\n\\)*=== value ===\n99\\'" out)))
        ;; the advice calls through to the original `message' -- *Messages* still gets it.
        (with-current-buffer (messages-buffer)
          (should (> (length (buffer-string)) (length messages-before)))
          (should (string-match-p "x" (buffer-substring (length messages-before) (point-max)))))))))

(ert-deftest claude-lib-test-message-nil-sentinel-not-inserted ()
  "`(message nil)' cancels the echo area; it must not print the string \"nil\"."
  (claude-lib-test--with-form-file "(message \"x\")\n(message nil)\n(print \"y\")\n1\n" form-file
    (claude-lib-test--with-output-file output-file
      (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory)
      (let ((out (claude-lib-test--read-output output-file)))
        (should-not (string-match-p "nil" (car (split-string out "=== value ==="))))))))

;; ============================================================================
;; AC3 -- usable text and propagation on error; partial output preserved
;; ============================================================================

(ert-deftest claude-lib-test-error-propagates-with-message ()
  (claude-lib-test--with-form-file "(error \"boom %s\" 42)\n" form-file
    (claude-lib-test--with-output-file output-file
      (let ((err (should-error (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory))))
        (should (string-match-p "boom 42" (error-message-string err)))))))

(ert-deftest claude-lib-test-error-writes-partial-output-first ()
  (claude-lib-test--with-form-file "(print \"before\")\n(error \"boom\")\n" form-file
    (claude-lib-test--with-output-file output-file
      (should-error (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory))
      (should (file-exists-p output-file))
      (should (string-match-p "before" (claude-lib-test--read-output output-file))))))

(ert-deftest claude-lib-test-error-still-removes-message-advice ()
  "A prior call's dead capture buffer must not still be advised onto `message'.
If cleanup leaked, this second, unrelated `message' call would try to
insert into that killed buffer and error -- it must not."
  (claude-lib-test--with-form-file "(error \"boom\")\n" form-file
    (claude-lib-test--with-output-file output-file
      (should-error (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory))
      (should (progn (message "sentinel-after-error") t))
      (with-current-buffer (messages-buffer)
        (should (string-match-p "sentinel-after-error" (buffer-string)))))))

;; ============================================================================
;; AC4 -- explicit ROOT, never ambient default-directory
;; ============================================================================

(ert-deftest claude-lib-test-root-is-bound-and-restored ()
  (claude-lib-test--with-form-file "default-directory\n" form-file
    (claude-lib-test--with-output-file output-file
      (let* ((root (file-name-as-directory (make-temp-file "claude-lib-test-root" t)))
             (caller-default-directory default-directory))
        (unwind-protect
            (progn
              (edmacs-claude-lib-eval-file form-file output-file root)
              (let ((out (claude-lib-test--read-output output-file)))
                (should (string-match-p (regexp-quote (format "=== value ===\n%s" root)) out)))
              (should (equal default-directory caller-default-directory)))
          (delete-directory root t))))))

(ert-deftest claude-lib-test-root-must-be-a-directory ()
  (claude-lib-test--with-form-file "1\n" form-file
    (claude-lib-test--with-output-file output-file
      (should-error (edmacs-claude-lib-eval-file form-file output-file "/no/such/directory-at-all")))))

;; ============================================================================
;; AC5 -- multi-line output unescaped
;; ============================================================================

(ert-deftest claude-lib-test-string-value-unescaped-newlines ()
  (claude-lib-test--with-form-file "(concat \"a\" \"\\n\" \"b\" \"\\n\")\n" form-file
    (claude-lib-test--with-output-file output-file
      (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory)
      (let* ((out (claude-lib-test--read-output output-file))
             (value-section (cadr (split-string out "=== value ===\n"))))
        (should-not (string-match-p "\\\\n" value-section))
        (should (equal value-section "a\nb\n"))))))

(ert-deftest claude-lib-test-nil-value-written-literally ()
  (claude-lib-test--with-form-file "nil\n" form-file
    (claude-lib-test--with-output-file output-file
      (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory)
      (let ((out (claude-lib-test--read-output output-file)))
        (should (string-match-p "=== value ===\nnil\\'" out))))))

;; ============================================================================
;; AC7 -- hard truncation, never a silently truncated write
;; ============================================================================

(ert-deftest claude-lib-test-oversized-output-errors-and-writes-nothing ()
  (claude-lib-test--with-form-file "(make-string 1000 ?x)\n" form-file
    (claude-lib-test--with-output-file output-file
      (let ((edmacs-claude-lib-max-output-bytes 10))
        (let ((err (should-error (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory))))
          (should (string-match-p "10" (error-message-string err)))))
      (should-not (file-exists-p output-file)))))

(ert-deftest claude-lib-test-form-error-survives-oversized-write-output ()
  "When the form errors AND the cleanup write-output also errors (oversized
body), the form's own error -- not the write-output error -- must be
what propagates: it is the one that actually explains the failure."
  (claude-lib-test--with-form-file "(print \"before\")\n(error \"boom\")\n" form-file
    (claude-lib-test--with-output-file output-file
      (let ((edmacs-claude-lib-max-output-bytes 1))
        (let ((err (should-error (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory))))
          (should (string-match-p "boom" (error-message-string err)))
          (should-not (string-match-p "exceeds" (error-message-string err))))))))

(ert-deftest claude-lib-test-oversized-output-does-not-clobber-stale-file ()
  (claude-lib-test--with-form-file "(make-string 1000 ?x)\n" form-file
    (claude-lib-test--with-output-file output-file
      (with-temp-file output-file (insert "stale-but-current-looking-content"))
      (let ((edmacs-claude-lib-max-output-bytes 10))
        (should-error (edmacs-claude-lib-eval-file form-file output-file temporary-file-directory)))
      (should (equal (claude-lib-test--read-output output-file) "stale-but-current-looking-content")))))

;; ============================================================================
;; Phase 3 AC2 -- discovery needs no second tool
;; ============================================================================

(ert-deftest claude-lib-test-demo-discoverable-by-apropos ()
  (should (memq 'claude-lib-demo (apropos-internal "^claude-lib-" #'fboundp))))

(ert-deftest claude-lib-test-demo-docstring-exact ()
  (should (equal (documentation 'claude-lib-demo)
                 "Summarise the project at ROOT to DEPTH levels.\nReturns an alist of (FILE . LINES).")))

(ert-deftest claude-lib-test-demo-arglist-exact ()
  (should (equal (help-function-arglist 'claude-lib-demo) '(root &optional depth))))

(ert-deftest claude-lib-test-demo-eldoc-args-string ()
  (should (equal (elisp-get-fnsym-args-string 'claude-lib-demo) "(ROOT &optional DEPTH)")))

;; ============================================================================
;; Phase 3 AC3/AC4 -- claude-lib-promote: provenance and validation
;; ============================================================================

(ert-deftest claude-lib-test-promote-writes-provenance-comment-above-form ()
  (claude-lib-test--with-temp-library lib
    (unwind-protect
        (progn
          (claude-lib-promote
           "(defun claude-lib-test-promoted-fn (x)\n  \"Return X unchanged.\"\n  x)"
           "rdm/editors/emacs" "needed a trivial passthrough for a live test")
          (let ((text (with-temp-buffer
                        (insert-file-contents lib)
                        (buffer-string))))
            (should (string-match-p "needed a trivial passthrough for a live test" text))
            (should (string-match-p "Destination: rdm/editors/emacs\\." text))
            (should (string-match-p
                     (rx "\n;; Promoted " (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) ": "
                         "needed a trivial passthrough for a live test "
                         "Destination: rdm/editors/emacs.\n"
                         "(defun claude-lib-test-promoted-fn")
                     text))))
      (when (fboundp 'claude-lib-test-promoted-fn) (fmakunbound 'claude-lib-test-promoted-fn)))))

(ert-deftest claude-lib-test-promote-rejects-missing-docstring ()
  (claude-lib-test--with-temp-library lib
    (should-error
     (claude-lib-promote "(defun claude-lib-test-no-doc (x) x)" "edmacs" "problem")
     :type 'user-error)))

(ert-deftest claude-lib-test-promote-rejects-docstring-without-period ()
  (claude-lib-test--with-temp-library lib
    (should-error
     (claude-lib-promote "(defun claude-lib-test-bad-doc (x)\n  \"Return X\"\n  x)" "edmacs" "problem")
     :type 'user-error)))

(ert-deftest claude-lib-test-promote-rejects-wrong-prefix ()
  (claude-lib-test--with-temp-library lib
    (should-error
     (claude-lib-promote "(defun not-claude-lib-prefixed (x)\n  \"Return X unchanged.\"\n  x)" "edmacs" "problem")
     :type 'user-error)))

(ert-deftest claude-lib-test-promote-rejects-multiple-forms ()
  (claude-lib-test--with-temp-library lib
    (should-error
     (claude-lib-promote
      (concat "(defun claude-lib-test-multi-a (x)\n  \"Return X unchanged.\"\n  x)\n"
              "(defun claude-lib-test-multi-b (x)\n  \"Return X unchanged.\"\n  x)")
      "edmacs" "problem")
     :type 'user-error)))

(ert-deftest claude-lib-test-promote-rejects-blank-destination ()
  (claude-lib-test--with-temp-library lib
    (should-error
     (claude-lib-promote "(defun claude-lib-test-blank-dest (x)\n  \"Return X unchanged.\"\n  x)"
                          "   " "problem")
     :type 'user-error)))

(ert-deftest claude-lib-test-promote-rejects-blank-problem ()
  (claude-lib-test--with-temp-library lib
    (should-error
     (claude-lib-promote "(defun claude-lib-test-blank-problem (x)\n  \"Return X unchanged.\"\n  x)"
                          "edmacs" "")
     :type 'user-error)))

(ert-deftest claude-lib-test-promote-rejects-newline-in-problem ()
  "A multi-line PROBLEM must be rejected outright, not spliced into the
file as an uncommented second line -- see the newline-injection finding
against an earlier pass of this function."
  (claude-lib-test--with-temp-library lib
    (should-error
     (claude-lib-promote
      "(defun claude-lib-test-nl-problem (x)\n  \"Return X unchanged.\"\n  x)"
      "edmacs" "Fixed a bug.\nAlso handles the empty case.")
     :type 'user-error)
    ;; The rejection must happen before anything is written: the file
    ;; must still parse cleanly and must not mention the rejected call.
    (with-temp-buffer
      (insert-file-contents lib)
      (should-not (string-match-p "Also handles the empty case" (buffer-string)))
      (goto-char (point-min))
      (should (read (current-buffer))))))

(ert-deftest claude-lib-test-promote-rejects-newline-in-destination ()
  "A multi-line DESTINATION must be rejected outright for the same reason
a multi-line PROBLEM is."
  (claude-lib-test--with-temp-library lib
    (should-error
     (claude-lib-promote
      "(defun claude-lib-test-nl-dest (x)\n  \"Return X unchanged.\"\n  x)"
      "edmacs\n(defun claude-lib-evil (x) x)" "problem")
     :type 'user-error)
    (with-temp-buffer
      (insert-file-contents lib)
      (should-not (fboundp 'claude-lib-evil))
      (should-not (string-match-p "claude-lib-evil" (buffer-string))))))

(ert-deftest claude-lib-test-promote-nil-destination-defaults-to-edmacs ()
  (claude-lib-test--with-temp-library lib
    (unwind-protect
        (progn
          (claude-lib-promote "(defun claude-lib-test-default-dest (x)\n  \"Return X unchanged.\"\n  x)"
                               nil "problem")
          (should (string-match-p "Destination: edmacs\\."
                                  (with-temp-buffer
                                    (insert-file-contents lib)
                                    (buffer-string)))))
      (when (fboundp 'claude-lib-test-default-dest) (fmakunbound 'claude-lib-test-default-dest)))))

(ert-deftest claude-lib-test-promote-duplicate-rejected-then-allowed ()
  (claude-lib-test--with-temp-library lib
    (unwind-protect
        (progn
          (claude-lib-promote "(defun claude-lib-test-dup (x)\n  \"Return X unchanged.\"\n  x)"
                               "edmacs" "first promotion")
          (should-error
           (claude-lib-promote "(defun claude-lib-test-dup (x)\n  \"Return X, again.\"\n  x)"
                                "edmacs" "second promotion")
           :type 'user-error)
          (should (claude-lib-promote "(defun claude-lib-test-dup (x)\n  \"Return X, again.\"\n  x)"
                                       "edmacs" "second promotion, allowed" t)))
      (when (fboundp 'claude-lib-test-dup) (fmakunbound 'claude-lib-test-dup)))))

(declare-function claude-lib-test-valid nil (x))

(ert-deftest claude-lib-test-promote-valid-defines-live-and-persists ()
  (claude-lib-test--with-temp-library lib
    (unwind-protect
        (progn
          (should (eq (claude-lib-promote "(defun claude-lib-test-valid (x)\n  \"Return X unchanged.\"\n  x)"
                                           "edmacs" "valid promotion")
                      'claude-lib-test-valid))
          (should (fboundp 'claude-lib-test-valid))
          (should (equal (claude-lib-test-valid 5) 5))
          (should (string-match-p "defun claude-lib-test-valid"
                                  (with-temp-buffer
                                    (insert-file-contents lib)
                                    (buffer-string)))))
      (when (fboundp 'claude-lib-test-valid) (fmakunbound 'claude-lib-test-valid)))))

(declare-function claude-lib-test-cl-valid nil (x &optional y))

(ert-deftest claude-lib-test-promote-accepts-cl-defun ()
  "claude-lib-promote must accept a cl-defun SOURCE, not just defun --
its own docstring and validation both advertise all three shapes."
  (claude-lib-test--with-temp-library lib
    (unwind-protect
        (progn
          (should (eq (claude-lib-promote
                       "(cl-defun claude-lib-test-cl-valid (x &optional y)\n  \"Return X plus Y, defaulting Y to zero.\"\n  (+ x (or y 0)))"
                       "edmacs" "cl-defun promotion coverage")
                      'claude-lib-test-cl-valid))
          (should (fboundp 'claude-lib-test-cl-valid))
          (should (equal (claude-lib-test-cl-valid 5 2) 7))
          (should (string-match-p "cl-defun claude-lib-test-cl-valid"
                                  (with-temp-buffer
                                    (insert-file-contents lib)
                                    (buffer-string)))))
      (when (fboundp 'claude-lib-test-cl-valid) (fmakunbound 'claude-lib-test-cl-valid)))))

(ert-deftest claude-lib-test-promote-accepts-defmacro ()
  "claude-lib-promote must accept a defmacro SOURCE, not just defun."
  (claude-lib-test--with-temp-library lib
    (unwind-protect
        (progn
          (should (eq (claude-lib-promote
                       "(defmacro claude-lib-test-macro-valid (x)\n  \"Expand to a form that doubles X.\"\n  `(* 2 ,x))"
                       "edmacs" "defmacro promotion coverage")
                      'claude-lib-test-macro-valid))
          (should (fboundp 'claude-lib-test-macro-valid))
          (should (equal (macroexpand '(claude-lib-test-macro-valid 5)) '(* 2 5)))
          (should (string-match-p "defmacro claude-lib-test-macro-valid"
                                  (with-temp-buffer
                                    (insert-file-contents lib)
                                    (buffer-string)))))
      (when (fboundp 'claude-lib-test-macro-valid) (fmakunbound 'claude-lib-test-macro-valid)))))

(ert-deftest claude-lib-test-promote-errors-on-modified-buffer ()
  (claude-lib-test--with-temp-library lib
    (let ((buf (find-file-noselect lib)))
      (unwind-protect
          (progn
            (with-current-buffer buf (insert ";; unsaved local edit\n"))
            (should-error
             (claude-lib-promote "(defun claude-lib-test-guarded (x)\n  \"Return X unchanged.\"\n  x)"
                                  "edmacs" "problem")
             :type 'user-error))
        (kill-buffer buf)))))

;; ============================================================================
;; Phase 3 AC5 -- no gptel dependency, no tool-registry
;; ============================================================================

(ert-deftest claude-lib-test-no-gptel-or-registry-references ()
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "claude-lib.el" claude-lib-test--repo-root))
                (buffer-string))))
    (should-not (string-match-p "gptel\\|llm-tool-collection" text))))

;; ============================================================================
;; Phase 3 AC6 -- interactive-driving convention documented; per-project override
;; ============================================================================

(ert-deftest claude-lib-test-relevant-functions-safe-local-variable ()
  (let ((pred (get 'claude-lib-relevant-functions 'safe-local-variable)))
    (should (funcall pred '(claude-lib-demo foo)))
    (should-not (funcall pred "not-a-list"))
    (should-not (funcall pred '(claude-lib-demo "not-a-symbol")))))

(ert-deftest claude-lib-test-header-documents-interactive-driving-convention ()
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "claude-lib.el" claude-lib-test--repo-root))
                (buffer-string))))
    (should (string-match-p "completing-read-function" text))
    (should (string-match-p "unread-command-events" text))))

;; No promoted `claude-lib-' function drives an interactive command yet
;; (that is phase 6's reusable-helper territory), so there is nothing
;; real inside claude-lib.el itself for a behavioral test to exercise.
;; These three tests instead prove the documented primitives actually
;; behave the way the convention says, against small fixtures local to
;; this file -- a real regression guard on the mechanism a future
;; promoted function must use, not just a grep for the words.

(ert-deftest claude-lib-test-interactive-driving-completing-read-function-primitive ()
  "A bound `completing-read-function' must answer `completing-read'
without ever reaching the real minibuffer -- the mechanism the
convention prescribes for \"the caller is the point\"."
  (let ((completing-read-function
         (lambda (_prompt _collection &rest _ignored) "chosen")))
    (should (equal (completing-read "Pick: " '("chosen" "other")) "chosen"))))

(ert-deftest claude-lib-test-interactive-driving-unread-command-events-primitive ()
  "Pre-fed `unread-command-events' must satisfy a direct event read --
the mechanism the convention prescribes for \"the picker is the
point\" (a loop that reads its own keys rather than being driven
through `completing-read-function')."
  (let ((unread-command-events (listify-key-sequence "x")))
    (should (equal (read-char) ?x))))

(ert-deftest claude-lib-test-interactive-driving-select-window-pins-target ()
  "A key fed through the real command loop must land in the SELECTED
window's buffer, not whatever buffer a plain `set-buffer' left
current -- this is why the convention requires pinning the window
with `select-window' before feeding keys, rather than trusting
`current-buffer'."
  (let ((decoy (generate-new-buffer " *claude-lib-test-decoy*"))
        (target (generate-new-buffer " *claude-lib-test-target*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) target)
          (select-window (selected-window))
          ;; `current-buffer' now disagrees with the selected window's
          ;; buffer on purpose, mimicking a caller that never switched it.
          (set-buffer decoy)
          (execute-kbd-macro [?x])
          (should (equal (with-current-buffer target (buffer-string)) "x"))
          (should (equal (with-current-buffer decoy (buffer-string)) "")))
      (kill-buffer decoy)
      (kill-buffer target))))

(provide 'claude-lib-test)
;;; claude-lib-test.el ends here
