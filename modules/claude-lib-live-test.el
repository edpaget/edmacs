;;; claude-lib-live-test.el --- Real-subprocess tests for claude-lib.el -*- lexical-binding: t -*-

;;; Commentary:
;; Drives `edmacs-claude-lib-eval-file' the exact way a Bash-tool call
;; would: a THROWAWAY `emacs -Q --daemon=NAME', driven purely through
;; real `emacsclient -s NAME -e' subprocesses -- no in-process function
;; call ever touches the function under test directly. Each test starts
;; and kills its own daemon (unique server name per test, via
;; `make-temp-name'), torn down in `unwind-protect' so a failing
;; assertion still kills the daemon.
;;
;; Follows scripts/gui-ert.sh's own throwaway-daemon pattern (own server
;; name, never the user's real "server", never `--init-directory') and
;; the live-daemon-diagnosis project memory's requirement that
;; `emacsclient' needs `TERM=dumb' in this agent environment or it fails
;; with "Unknown terminal type" -- set on every `call-process' below via
;; a `process-environment' let-binding, never relying on the ambient
;; shell's own `TERM'.
;;
;; Run:
;;   scripts/run-ert-suite.sh 60 emacs -Q --batch -l ert \
;;         -l modules/claude-lib-live-test.el -f ert-run-tests-batch-and-exit
;;
;; Needs a real `emacs' and `emacsclient' on PATH and the ability to fork
;; a subprocess; skips are not expected in a normal dev environment, so
;; none are built in here (contrast the pty-only suites in CLAUDE.md's
;; Testing section, which this is not one of).
;;
;; Also covers phase 3's AC1 for the promotion library built on top of
;; this same file: a function `claude-lib-promote'd in one plain batch
;; Emacs process must be `fboundp' in a second, wholly separate batch
;; process that freshly loads the (temp-copied) library afterward --
;; proving persistence across a process boundary, not merely within one
;; image. That check needs a real forked `emacs' but no daemon or
;; `emacsclient' at all, so it drives `call-process' directly rather
;; than going through the daemon helpers above.
;;
;; Phase 3's AC5 boot-safety regressions live here for the same reason:
;; "the file still loads at the next daemon start" is only provable by
;; actually starting another Emacs on it. One drives the reproduced
;; docstring-anchor corruption end to end and requires the result to
;; load; the other proves `claude-lib-verify-load-in-subprocess' refuses
;; a library that parses but no longer boots, and rolls the file back.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar claude-lib-live-test--repo-root
  (file-name-as-directory (expand-file-name default-directory))
  "Repository root, captured at load time.
Matches every other suite's own \"run from the repository root\"
convention rather than deriving it from `load-file-name', so this file
runs the same way whether loaded by path or by name.")

(defun claude-lib-live-test--emacsclient (server-name form)
  "Run \"emacsclient -s SERVER-NAME -e FORM\" synchronously.
Returns (EXIT-CODE STDOUT STDERR). Sets TERM=dumb for the child process
-- required for `emacsclient' to run at all in this agent environment,
per the live-daemon-diagnosis project memory -- rather than trusting
whatever TERM the ambient shell happens to have."
  (let* ((process-environment (cons "TERM=dumb" process-environment))
         (stderr-file (make-temp-file "claude-lib-live-test-stderr"))
         exit-code stdout stderr)
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq exit-code
                  (call-process "emacsclient" nil (list t stderr-file) nil
                                 "-s" server-name "-e" form))
            (setq stdout (buffer-string)))
          (with-temp-buffer
            (insert-file-contents stderr-file)
            (setq stderr (buffer-string))))
      (ignore-errors (delete-file stderr-file)))
    (list exit-code stdout stderr)))

(defmacro claude-lib-live-test--with-daemon (server-var &rest body)
  "Bind SERVER-VAR to a fresh throwaway daemon's server name for BODY.
Starts \"emacs -Q --daemon=SERVER-VAR\", loads claude-lib.el into it,
runs BODY, then kills it in `unwind-protect' regardless of outcome."
  (declare (indent 1))
  `(let ((,server-var (make-temp-name "edmacs-claude-lib-live-test-")))
     (unwind-protect
         (progn
           (let ((process-environment (cons "TERM=dumb" process-environment)))
             (should (zerop (call-process "emacs" nil nil nil "-Q"
                                           (concat "--daemon=" ,server-var)))))
           (cl-destructuring-bind (exit-code stdout stderr)
               (claude-lib-live-test--emacsclient
                ,server-var
                (format "(load %S nil t)"
                        (expand-file-name "modules/claude-lib.el" claude-lib-live-test--repo-root)))
             (unless (zerop exit-code)
               (error "claude-lib-live-test: could not load claude-lib.el into %s: %s%s"
                      ,server-var stdout stderr)))
           ,@body)
       (ignore-errors (claude-lib-live-test--emacsclient ,server-var "(kill-emacs)")))))

(defun claude-lib-live-test--eval-file (server-name form-file output-file root)
  "Drive `edmacs-claude-lib-eval-file' in SERVER-NAME via real emacsclient."
  (claude-lib-live-test--emacsclient
   server-name
   (format "(edmacs-claude-lib-eval-file %S %S %S)" form-file output-file root)))

(defun claude-lib-live-test--read-file (file)
  "Read FILE back as plain text, the way the Bash caller would."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; ============================================================================
;; AC1 -- end-to-end success against a named daemon
;; ============================================================================

(ert-deftest claude-lib-live-test-eval-file-end-to-end ()
  (claude-lib-live-test--with-daemon server
    (let ((form-file (make-temp-file "claude-lib-live-test-form"))
          (output-file (make-temp-name (expand-file-name "claude-lib-live-test-output" temporary-file-directory))))
      (unwind-protect
          (progn
            (with-temp-file form-file (insert "(+ 40 2)\n"))
            (cl-destructuring-bind (exit-code stdout stderr)
                (claude-lib-live-test--eval-file server form-file output-file temporary-file-directory)
              (ignore stdout)
              (should (zerop exit-code))
              (should (equal stderr ""))
              (should (file-exists-p output-file))
              (should (string-match-p "=== value ===\n42\\'"
                                      (claude-lib-live-test--read-file output-file)))))
        (ignore-errors (delete-file form-file))
        (when (file-exists-p output-file) (delete-file output-file))))))

;; ============================================================================
;; AC3 -- non-zero exit and readable stderr text on error
;; ============================================================================

(ert-deftest claude-lib-live-test-error-nonzero-exit-and-stderr ()
  (claude-lib-live-test--with-daemon server
    (let ((form-file (make-temp-file "claude-lib-live-test-form"))
          (output-file (make-temp-name (expand-file-name "claude-lib-live-test-output" temporary-file-directory))))
      (unwind-protect
          (progn
            (with-temp-file form-file (insert "(error \"boom %s\" 42)\n"))
            (cl-destructuring-bind (exit-code stdout stderr)
                (claude-lib-live-test--eval-file server form-file output-file temporary-file-directory)
              (ignore stdout)
              (should-not (zerop exit-code))
              (should (string-match-p "boom 42" stderr))))
        (ignore-errors (delete-file form-file))
        (when (file-exists-p output-file) (delete-file output-file))))))

;; ============================================================================
;; AC4 -- explicit ROOT per call, never leaking across calls
;; ============================================================================

(ert-deftest claude-lib-live-test-root-is-per-call-not-per-daemon ()
  (claude-lib-live-test--with-daemon server
    (let ((form-file (make-temp-file "claude-lib-live-test-form"))
          (output-file (make-temp-name (expand-file-name "claude-lib-live-test-output" temporary-file-directory)))
          (root-a (file-name-as-directory (make-temp-file "claude-lib-live-test-root-a" t)))
          (root-b (file-name-as-directory (make-temp-file "claude-lib-live-test-root-b" t))))
      (unwind-protect
          (progn
            (with-temp-file form-file (insert "default-directory\n"))
            (cl-destructuring-bind (exit-code-a _stdout-a _stderr-a)
                (claude-lib-live-test--eval-file server form-file output-file root-a)
              (should (zerop exit-code-a))
              (should (string-match-p (regexp-quote (concat "=== value ===\n" root-a))
                                      (claude-lib-live-test--read-file output-file))))
            (cl-destructuring-bind (exit-code-b _stdout-b _stderr-b)
                (claude-lib-live-test--eval-file server form-file output-file root-b)
              (should (zerop exit-code-b))
              (let ((out (claude-lib-live-test--read-file output-file)))
                (should (string-match-p (regexp-quote (concat "=== value ===\n" root-b)) out))
                (should-not (string-match-p (regexp-quote root-a) out)))))
        (ignore-errors (delete-file form-file))
        (when (file-exists-p output-file) (delete-file output-file))
        (ignore-errors (delete-directory root-a t))
        (ignore-errors (delete-directory root-b t))))))

;; ============================================================================
;; AC5 -- multi-line output unescaped through the full transport
;; ============================================================================

(ert-deftest claude-lib-live-test-multiline-output-unescaped ()
  (claude-lib-live-test--with-daemon server
    (let ((form-file (make-temp-file "claude-lib-live-test-form"))
          (output-file (make-temp-name (expand-file-name "claude-lib-live-test-output" temporary-file-directory))))
      (unwind-protect
          (progn
            (with-temp-file form-file (insert "(concat \"a\" \"\\n\" \"b\" \"\\n\")\n"))
            (cl-destructuring-bind (exit-code _stdout _stderr)
                (claude-lib-live-test--eval-file server form-file output-file temporary-file-directory)
              (should (zerop exit-code))
              (let* ((out (claude-lib-live-test--read-file output-file))
                     (value-section (cadr (split-string out "=== value ===\n"))))
                (should-not (string-match-p "\\\\n" value-section))
                (should (equal value-section "a\nb\n")))))
        (ignore-errors (delete-file form-file))
        (when (file-exists-p output-file) (delete-file output-file))))))

;; ============================================================================
;; AC7 -- hard truncation end to end
;; ============================================================================

(ert-deftest claude-lib-live-test-oversized-output-errors ()
  (claude-lib-live-test--with-daemon server
    (let ((form-file (make-temp-file "claude-lib-live-test-form"))
          (output-file (make-temp-name (expand-file-name "claude-lib-live-test-output" temporary-file-directory))))
      (unwind-protect
          (progn
            (with-temp-file form-file (insert "(make-string 1000 ?x)\n"))
            (cl-destructuring-bind (exit-code _stdout _stderr)
                (claude-lib-live-test--emacsclient server "(setq edmacs-claude-lib-max-output-bytes 10)")
              (should (zerop exit-code)))
            (cl-destructuring-bind (exit-code _stdout stderr)
                (claude-lib-live-test--eval-file server form-file output-file temporary-file-directory)
              (should-not (zerop exit-code))
              (should (string-match-p "10" stderr))
              (should-not (file-exists-p output-file))))
        (ignore-errors (delete-file form-file))
        (when (file-exists-p output-file) (delete-file output-file))))))

;; ============================================================================
;; Phase 3 AC1 -- promotion survives to a fresh, separate Emacs process
;; ============================================================================

(ert-deftest claude-lib-live-test-promote-persists-across-process-boundary ()
  "A function `claude-lib-promote'd in one batch Emacs process must be
`fboundp' in a second, wholly separate batch Emacs process that
freshly loads the same (temp-copied) library file afterward."
  (let* ((tmp-lib (make-temp-file "claude-lib-live-test-lib" nil ".el"))
         (source-lib (expand-file-name "modules/claude-lib.el" claude-lib-live-test--repo-root))
         (process-environment (cons "TERM=dumb" process-environment)))
    (unwind-protect
        (progn
          (copy-file source-lib tmp-lib t)
          (let ((promote-form
                 `(progn
                    (load ,tmp-lib nil t)
                    (claude-lib-promote
                     "(defun claude-lib-live-test-promoted (x)\n  \"Return X unchanged.\"\n  x)"
                     "edmacs" "live-test fixture")
                    (kill-emacs 0))))
            (with-temp-buffer
              (let ((exit (call-process "emacs" nil t nil "-Q" "--batch"
                                         "--eval" (prin1-to-string promote-form))))
                (unless (zerop exit)
                  (error "claude-lib-live-test: promote process failed (exit %d): %s"
                         exit (buffer-string))))))
          (let ((check-form
                 `(progn
                    (load ,tmp-lib nil t)
                    (kill-emacs (if (fboundp 'claude-lib-live-test-promoted) 0 1)))))
            (should (zerop (call-process "emacs" nil nil nil "-Q" "--batch"
                                          "--eval" (prin1-to-string check-form))))))
      (ignore-errors (delete-file tmp-lib)))))


;; ============================================================================
;; Phase 3 AC5 -- the promoted library still boots in a fresh Emacs
;; ============================================================================

(defun claude-lib-live-test--batch-eval (form)
  "Run FORM in a fresh \"emacs -Q --batch\", returning (EXIT-CODE OUTPUT)."
  (with-temp-buffer
    (let ((exit (call-process "emacs" nil t nil "-Q" "--batch"
                              "--eval" (prin1-to-string form))))
      (list exit (buffer-string)))))

(ert-deftest claude-lib-live-test-promoted-library-still-loads-in-fresh-emacs ()
  "The direct regression for \"leaves the daemon unbootable across
restarts\": promote a function whose docstring carries the library's own
`(provide ...)' tail at column 0, promote a second function behind it
with a `\"' in PROBLEM, then require the resulting file to load cleanly
in a wholly separate Emacs. Before the structural anchor landed this
step failed with `End of file during parsing'."
  (let* ((tmp-lib (make-temp-file "claude-lib-live-test-boot" nil ".el"))
         (source-lib (expand-file-name "modules/claude-lib.el" claude-lib-live-test--repo-root))
         (process-environment (cons "TERM=dumb" process-environment)))
    (unwind-protect
        (progn
          (copy-file source-lib tmp-lib t)
          (cl-destructuring-bind (exit output)
              (claude-lib-live-test--batch-eval
               `(progn
                  (load ,tmp-lib nil t)
                  (claude-lib-promote
                   ,(concat "(defun claude-lib-live-test-anchor-trap (x)\n"
                            "  \"Return X unchanged.\n"
                            "Illustrative library tail, as plain docstring text:\n"
                            "(provide 'claude-lib)\n"
                            ";;; claude-lib.el ends here\"\n"
                            "  x)")
                   "edmacs" "seed a docstring carrying the library tail")
                  (claude-lib-promote
                   "(defun claude-lib-live-test-behind-trap (y)\n  \"Return Y unchanged.\"\n  y)"
                   "edmacs" "a problem mentioning a \" character")
                  (kill-emacs 0)))
            (unless (zerop exit)
              (error "claude-lib-live-test: trap promotions failed (exit %d): %s" exit output)))
          (cl-destructuring-bind (exit output)
              (claude-lib-live-test--batch-eval
               `(progn (load ,tmp-lib nil t)
                       (kill-emacs (if (fboundp 'claude-lib-live-test-behind-trap) 0 1))))
            (should (equal (list exit output) (list 0 "")))))
      (ignore-errors (delete-file tmp-lib)))))

(ert-deftest claude-lib-live-test-subprocess-gate-rejects-corrupt-library ()
  "With the boot-safety gate at its production default, a library that
still parses but no longer LOADS must be refused, and the file left
byte-identical to how the promotion found it."
  (let* ((tmp-lib (make-temp-file "claude-lib-live-test-corrupt" nil ".el"))
         (source-lib (expand-file-name "modules/claude-lib.el" claude-lib-live-test--repo-root))
         (process-environment (cons "TERM=dumb" process-environment)))
    (unwind-protect
        (progn
          (copy-file source-lib tmp-lib t)
          ;; Parses, ends with the provide form, and dies on `load'.
          (with-temp-buffer
            (insert-file-contents tmp-lib)
            (goto-char (point-min))
            (should (re-search-forward "^(provide 'claude-lib)$" nil t))
            (goto-char (match-beginning 0))
            (insert "(error \"deliberate boot failure\")\n\n")
            (write-region (point-min) (point-max) tmp-lib nil 'quiet))
          (let ((before (claude-lib-live-test--read-file tmp-lib)))
            (cl-destructuring-bind (exit output)
                (claude-lib-live-test--batch-eval
                 `(progn
                    (load ,(expand-file-name "modules/claude-lib.el"
                                             claude-lib-live-test--repo-root)
                          nil t)
                    (setq claude-lib-file ,tmp-lib)
                    (princ (condition-case err
                               (progn (claude-lib-promote
                                       "(defun claude-lib-live-test-corrupt (x)\n  \"Return X unchanged.\"\n  x)"
                                       "edmacs" "the library no longer boots")
                                      "PROMOTE UNEXPECTEDLY SUCCEEDED")
                             (user-error (error-message-string err))))
                    (kill-emacs 0)))
              (should (zerop exit))
              ;; Specifically the boot-safety gate, not some earlier check
              ;; that happens to reject the same input.
              (should (string-match-p "no longer loads in a fresh Emacs" output)))
            (should (equal before (claude-lib-live-test--read-file tmp-lib)))))
      (ignore-errors (delete-file tmp-lib)))))

(provide 'claude-lib-live-test)
;;; claude-lib-live-test.el ends here
