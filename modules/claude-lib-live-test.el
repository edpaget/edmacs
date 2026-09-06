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

(provide 'claude-lib-live-test)
;;; claude-lib-live-test.el ends here
