;;; claude-lib.el --- Bash-driven elisp eval channel for the resident daemon -*- lexical-binding: t -*-

;;; Commentary:
;; This module is the whole "Claude reaches Emacs" channel: no MCP
;; server, no allowlist, no new error protocol. A Bash-tool call drives
;; it directly through `emacsclient', file-in/file-out:
;;
;;   1. Write the elisp form(s) to evaluate, verbatim, to a FORM-FILE.
;;   2. Run:
;;        emacsclient -s <daemon-name> -e \
;;          '(edmacs-claude-lib-eval-file "<form-file>" "<output-file>" "<root>")'
;;      Three plain string literals -- no nested shell-quoting hazard, even
;;      for a form containing a quote, backslash, or newline (that hazard
;;      lives inside FORM-FILE's own contents, written by a normal file
;;      write, not assembled on a shell command line).
;;   3. Read OUTPUT-FILE from Bash. A non-zero `emacsclient' exit plus
;;      `*ERROR*: ...' text on stderr means a form signaled -- there is
;;      no separate success/failure protocol beyond emacsclient's own.
;;
;; Every form in FORM-FILE is read and evaluated in order, not just the
;; first (see `edmacs-claude-lib--read-all-forms'); the LAST form's
;; return value is what OUTPUT-FILE's value section reports. Output from
;; `print'/`princ' (via `standard-output') and from `message' (via a
;; temporary `advice-add', deliberately NOT `cl-letf' -- see this repo's
;; CLAUDE.md on the ~28s native-comp subr-trampoline `cl-letf' forces on
;; a C subr; this path runs on every single call, not just in tests) are
;; both captured, in call order, into OUTPUT-FILE's printed section.
;;
;; ROOT is a required argument, never ambient `default-directory' or
;; `(project-current)': the daemon's last-touched buffer says nothing
;; reliable about which project a given call means.
;;
;; No static elisp allowlist/form-walker/sanitizer is built here,
;; deliberately -- see the roadmap phase body this module implements for
;; why (rhblind/emacs-mcp-server's own documented let-binding and
;; funcall-intern bypasses). This channel's permission footing is
;; Bash's `permissions.defaultMode', exactly as for any other Bash call.
;;
;; Kept transport-agnostic on purpose: `edmacs-claude-lib-eval-file'
;; knows nothing about `emacsclient' beyond what its own three
;; parameters express, so a later swap to a real MCP server (e.g.
;; laurynas-biveinis/mcp-server-lib.el, whose stdio bridge shells out to
;; `emacsclient' anyway) would call this same function rather than
;; rewrite it.

;;; Code:

(require 'subr-x)

(defvar edmacs-claude-lib-max-output-bytes (* 1024 1024)
  "Hard ceiling, in bytes, on a single OUTPUT-FILE body.
Matches macher's own `macher-tool-output-max-length' convention cited in
the roadmap phase this module implements: an oversized result ERRORS --
propagating through emacsclient as a normal non-zero-exit failure --
rather than silently writing a truncated file that looks complete.")

(defun edmacs-claude-lib--skip-form-whitespace ()
  "Advance point past whitespace and `;'-comments, in the current buffer."
  (skip-chars-forward " \t\n\r\f")
  (while (eq (char-after) ?\;)
    (skip-chars-forward "^\n")
    (skip-chars-forward " \t\n\r\f")))

(defun edmacs-claude-lib--read-all-forms (form-file)
  "Read every top-level form in FORM-FILE, in order, as a list.
Deliberately does not rely on `read' signaling `end-of-file' to mean
\"nothing left\": that same signal is what a genuinely truncated
trailing form (an unbalanced paren at EOF) raises too, so catching it
unconditionally would misreport a malformed file as a clean, empty
stop. Instead, whitespace/comments are skipped by hand and `eobp'
alone decides whether more input remains; a `read' past that point
that itself hits EOF is a real error and is left to propagate."
  (with-temp-buffer
    (insert-file-contents form-file)
    (goto-char (point-min))
    (edmacs-claude-lib--skip-form-whitespace)
    (let (forms)
      (while (not (eobp))
        (push (read (current-buffer)) forms)
        (edmacs-claude-lib--skip-form-whitespace))
      (unless forms
        (error "edmacs-claude-lib: no forms in %s" form-file))
      (nreverse forms))))

(defun edmacs-claude-lib--format-value (value)
  "Render VALUE for OUTPUT-FILE's value section.
A string is written verbatim -- never through `prin1-to-string' --
so embedded newlines and quotes stay literal instead of becoming an
escaped `\\n' or `\\\"'. Anything else (including nil, which must come
back as the literal text `nil', not an empty section) goes through
`prin1-to-string'; an object `prin1' cannot render (a buffer, process,
or marker) falls back to a placeholder instead of erroring the whole
call."
  (if (stringp value)
      value
    (condition-case err
        (prin1-to-string value)
      (error (format "#<claude-lib: unprintable %S: %s>"
                      (type-of value) (error-message-string err))))))

(defun edmacs-claude-lib--message-advice (capture-buffer)
  "Return an `:around' advice for `message' that mirrors it into CAPTURE-BUFFER.
`message' never consults `standard-output', so capturing it needs this
separate mechanism. `(message nil)' is a sentinel that cancels a
pending echo-area message rather than a request to print the string
\"nil\"; a nil first argument is left alone -- nothing is inserted --
and the original `message' is always called through afterward, so
echo-area/`*Messages*' behavior is unaffected by this advice."
  (lambda (orig-fun &rest args)
    (when (car args)
      (with-current-buffer capture-buffer
        (goto-char (point-max))
        (insert (apply #'format-message args) "\n")))
    (apply orig-fun args)))

(defun edmacs-claude-lib--write-output (output-file capture-buffer value)
  "Compose and write OUTPUT-FILE from CAPTURE-BUFFER's printed text and VALUE.
Errors -- writing nothing -- if the composed body exceeds
`edmacs-claude-lib-max-output-bytes', so a prior run's stale OUTPUT-FILE
is never left in place looking like a truncated-but-current result."
  (let* ((printed (with-current-buffer capture-buffer (buffer-string)))
         (value-text (edmacs-claude-lib--format-value value))
         (body (concat "=== printed ===\n" printed "\n=== value ===\n" value-text)))
    (when (> (string-bytes body) edmacs-claude-lib-max-output-bytes)
      (error "edmacs-claude-lib: output is %d bytes, exceeds edmacs-claude-lib-max-output-bytes (%d)"
             (string-bytes body) edmacs-claude-lib-max-output-bytes))
    (with-temp-file output-file
      (insert body))))

;;;###autoload
(defun edmacs-claude-lib-eval-file (form-file output-file root)
  "Evaluate every form in FORM-FILE under default-directory ROOT.
Reads FORM-FILE with `edmacs-claude-lib--read-all-forms' (every form,
not just the first) and evaluates each in order with lexical binding.
Printed output (`print'/`princ' via `standard-output', `message' via a
temporary advice -- see `edmacs-claude-lib--message-advice') from every
form, plus the LAST form's return value, are written to OUTPUT-FILE by
`edmacs-claude-lib--write-output'.

ROOT is required, never defaulted to the daemon's ambient
`default-directory': it must already exist as a directory or this
signals before anything is read or evaluated. `default-directory' is
bound to it for the whole read+eval span via `let', so plain dynamic
unwind restores the daemon's own prior value on both normal return and
a non-local exit.

FORM-FILE and OUTPUT-FILE are resolved to absolute paths against this
function's own original `default-directory' -- before ROOT's binding
takes effect -- so a relative path for either one is never silently
reinterpreted against ROOT instead.

If a form signals, whatever OUTPUT-FILE content the forms before it
produced is still written before the error is left to propagate:
emacsclient's own existing `*ERROR*: ...' text on stderr plus a
non-zero exit is the whole error channel here, deliberately not
duplicated with a second one. That write is best-effort when a form
error is already in flight: a failure in
`edmacs-claude-lib--write-output' itself (e.g. an oversized body)
is swallowed rather than signaled, so it cannot replace the form's
own error the way a plain `unwind-protect' cleanup error would --
a later error signaled from a cleanup form supersedes whatever
condition was already propagating, which would otherwise report the
wrong problem (\"output too large\" instead of the form's real bug).
Only when no form error occurred does a write-output failure surface
as this call's error."
  (unless (file-directory-p root)
    (error "edmacs-claude-lib: ROOT is not a directory: %s" root))
  (let* ((form-file (expand-file-name form-file))
         (output-file (expand-file-name output-file))
         (forms (edmacs-claude-lib--read-all-forms form-file))
         (capture-buffer (generate-new-buffer " *claude-lib-capture*"))
         (advice (edmacs-claude-lib--message-advice capture-buffer))
         (value nil)
         (eval-error nil)
         (default-directory (file-name-as-directory (expand-file-name root))))
    (unwind-protect
        (progn
          (advice-add 'message :around advice)
          (let ((standard-output capture-buffer))
            (condition-case err
                (dolist (form forms)
                  (setq value (eval form t)))
              (error (setq eval-error err)))
            (condition-case write-err
                (edmacs-claude-lib--write-output output-file capture-buffer value)
              (error (unless eval-error
                       (signal (car write-err) (cdr write-err)))))))
      (advice-remove 'message advice)
      (kill-buffer capture-buffer))
    (when eval-error
      (signal (car eval-error) (cdr eval-error)))))

(provide 'claude-lib)
;;; claude-lib.el ends here
