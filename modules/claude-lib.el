;;; claude-lib.el --- Bash-driven eval channel and Claude's promoted-code library -*- lexical-binding: t -*-

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
;;
;; ------------------------------------------------------------------
;; THE PROMOTION LIBRARY (namespace `claude-lib-', distinct from the
;; eval channel's own `edmacs-claude-lib-' namespace above -- the two
;; halves of this file never share a prefix).
;;
;; Anything Claude writes and tests in a session evaporates when the
;; session ends unless it is promoted here via `claude-lib-promote',
;; which appends the tested form (with a dated provenance comment) to
;; THIS file, evals it into the running daemon, and saves -- so the
;; next session's fresh load (this file is loaded from init.el on
;; every daemon start) finds it too, not just the promoting session's
;; own live image.
;;
;; DISCOVERY IS SOLVED, and needs no second tool -- Emacs already does
;; it, verified against Emacs 31.1 in batch mode:
;;
;;   (apropos-internal "^claude-lib-" #'fboundp)     ; list the library
;;   (documentation 'claude-lib-demo)                ; full docstring
;;   (help-function-arglist 'claude-lib-demo)        ; signature
;;   (elisp-get-fnsym-args-string 'claude-lib-demo)   ; eldoc's own helper
;;
;; VERIFIED TRAP: `(with-output-to-string (describe-function 'foo))'
;; returns the EMPTY STRING. `describe-function' renders into a *Help*
;; buffer, not `standard-output', so it captures nothing -- use
;; `documentation' plus `help-function-arglist' instead. Do not build a
;; tool around `describe-function'.
;;
;; A promoted function's docstring is the interface: `claude-lib-promote'
;; hard-requires one whose first line is a complete sentence (ends in
;; `.') naming what the function returns, because that line is what a
;; library listing shows.
;;
;; GRADUATION does not always mean "stays in edmacs" -- a prototype may
;; belong to another repository entirely (e.g. an rdm Emacs package
;; graduates to rdm's own `editors/emacs/', under the `rdm-' prefix).
;; `claude-lib-promote' therefore requires a DESTINATION naming where
;; the function is meant to land, recorded in its provenance comment,
;; so a prototype bound elsewhere is never mistaken for a permanent
;; part of this config.
;;
;; DRIVING INTERACTIVE CODE is a house convention, not an ad-hoc trick:
;; a promoted function that exercises an interactive command must never
;; call the interactive surface directly. Use a bound
;; `completing-read-function' where the caller is the point, pre-fed
;; `unread-command-events' where the picker is, and `select-window' to
;; pin the target window first -- simulated keys follow the selected
;; window, not the current buffer. (This is phase 6's territory; until
;; its reusable input-feeding helpers land, drive these raw primitives
;; directly rather than inventing a helper symbol that does not exist
;; yet.)
;;
;; `claude-lib-relevant-functions' is a per-project override, set via
;; `.dir-locals.el', naming which `claude-lib-' symbols matter most for
;; the project at hand. Combine it with plain apropos rather than
;; adding a new listing function -- that would itself be the "second
;; tool" this design avoids:
;;
;;   (or (and (local-variable-p 'claude-lib-relevant-functions)
;;            claude-lib-relevant-functions)
;;       (apropos-internal "^claude-lib-" #'fboundp))
;;
;; No third-party tool-collection dependency and no tool-registry live
;; here: this file defines no registration plist and requires no
;; chat-tool framework of any kind.
;; ------------------------------------------------------------------

;;; Code:

(require 'subr-x)
(require 'seq)

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

(defvar claude-lib-file
  (or load-file-name buffer-file-name)
  "Absolute path to this file, resolved once at load time.
`claude-lib-promote' reads and writes this path, so it always targets
whichever copy of the library is actually loaded into this Emacs
image -- never a hard-coded location that could silently diverge from
it (e.g. when this file is loaded from a worktree's own checkout under
test, or from a temp copy in a test).")

(defvar-local claude-lib-relevant-functions nil
  "Project-specific subset of `claude-lib-' symbols worth using here.
Meant to be set via `.dir-locals.el' for a project whose work leans on
a handful of promoted functions -- the per-tool usage-guidance idea
from acmorrow/claude-code-ide-extras, adapted: the docstring already
plays that role here, so this variable only narrows which docstrings
are worth reading first. See this file's Commentary for how to combine
it with plain `apropos-internal' rather than adding a listing function.")

(put 'claude-lib-relevant-functions 'safe-local-variable
     (lambda (value) (and (listp value) (seq-every-p #'symbolp value))))

;; Promoted 2026-09-06: bootstrap/self-test fixture proving discovery
;; still works against a real promoted function (its arglist, docstring
;; and apropos membership match the roadmap phase's own verification
;; transcript exactly). Destination: edmacs -- this is scaffolding for
;; the library itself, not a candidate for promotion elsewhere.
(defun claude-lib-demo (root &optional depth)
  "Summarise the project at ROOT to DEPTH levels.
Returns an alist of (FILE . LINES)."
  (claude-lib--demo-walk root (or depth 1)))

(defun claude-lib--demo-walk (dir depth)
  "Collect (FILE . LINES) for DIR's regular files, recursing DEPTH levels.
Internal helper for `claude-lib-demo'; deliberately not itself a
`claude-lib-' promoted entry point."
  (let (result)
    (dolist (entry (directory-files dir t "\\`[^.]" t))
      (cond
       ((and (> depth 1) (file-directory-p entry))
        (setq result (nconc result (claude-lib--demo-walk entry (1- depth)))))
       ((file-regular-p entry)
        (push (cons entry
                    (with-temp-buffer
                      (insert-file-contents entry)
                      (count-lines (point-min) (point-max))))
              result))))
    (nreverse result)))

(defun claude-lib--read-source-forms (source)
  "Read every top-level Lisp form in the string SOURCE.
Reuses `edmacs-claude-lib--skip-form-whitespace' so blank lines and
`;'-comments around/between forms in SOURCE are tolerated exactly as
they are in a FORM-FILE. Wraps a malformed SOURCE (unbalanced parens,
nothing but comments) in a `user-error' rather than letting a raw
reader error escape: SOURCE is a string a Claude session hand-built for
this call, not a file already known to parse."
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (condition-case err
        (progn
          (edmacs-claude-lib--skip-form-whitespace)
          (let (forms)
            (while (not (eobp))
              (push (read (current-buffer)) forms)
              (edmacs-claude-lib--skip-form-whitespace))
            (unless forms
              (error "no forms in SOURCE"))
            (nreverse forms)))
      (error (user-error "claude-lib-promote: SOURCE does not parse as elisp: %s"
                          (error-message-string err))))))

(defun claude-lib--provenance-comment (destination problem)
  "Return a provenance comment recording PROBLEM and its DESTINATION.
Both must already be non-empty strings; validating and defaulting them
is `claude-lib-promote's job, not this formatter's."
  (format ";; Promoted %s: %s Destination: %s.\n"
          (format-time-string "%Y-%m-%d")
          problem destination))

(defun claude-lib--name-defined-in-file-p (name)
  "Return non-nil if NAME is already defined as a top-level form in BUFFER.
Meant to be called with `claude-lib-file's buffer current. Checks the
buffer's TEXT directly, not `fboundp': a previous promotion may already
be on disk without yet being loaded into this image (a fresh daemon
that has not reloaded claude-lib.el since), and a text search still
catches that case."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (format "^(\\(?:defun\\|cl-defun\\|defmacro\\)[ \t\n]+%s[ \t\n(]"
             (regexp-quote (symbol-name name)))
     nil t)))

(defun claude-lib--ensure-fresh-buffer (file)
  "Return a buffer visiting FILE with contents matching disk, or error.
If FILE is already visited by an unmodified buffer, reverts it first
so a promote call sees FILE's latest on-disk content even if something
else wrote to it since the buffer was opened. If that buffer instead
has unsaved local edits, signals a `user-error' rather than either
silently clobbering them or invoking `revert-buffer's interactive
confirmation, which would hang a non-interactive caller."
  (let ((buf (get-file-buffer file)))
    (if (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (when (buffer-modified-p)
            (user-error
             "claude-lib-promote: %s has unsaved changes in buffer %s; save or discard them first"
             file (buffer-name buf)))
          (revert-buffer t t t)
          buf)
      (find-file-noselect file))))

(defun claude-lib-promote (source destination problem &optional allow-redefine)
  "Promote SOURCE into this library as a permanent, provenance-tracked entry.

SOURCE is a string containing exactly one top-level `defun',
`cl-defun' or `defmacro' form, passed verbatim (so its own formatting
is preserved on disk) -- the tested, session-evaluated code to keep.
Its name must carry the `claude-lib-' prefix, and its docstring is
required and must have a first line ending in `.' (a complete sentence
naming what it returns): this is the one hard gate promotion enforces,
because an undocumented promoted function is invisible to discovery.
The period is a necessary, not sufficient, proxy for that -- it is not
a check that the sentence actually says anything useful.

DESTINATION names where the promotion is meant to graduate to --
\"edmacs\" (the default, used when DESTINATION is nil) or another
repository, e.g. \"rdm/editors/emacs\". An explicitly blank DESTINATION
is always an error, never silently coerced to the default: a
prototype's intended home must be stated outright or left unstated
entirely, never half-stated. PROBLEM is a required, non-empty
description of what SOURCE solves; both are recorded in a provenance
comment above the promoted form.

Unless NAME is already defined in this file, in which case
ALLOW-REDEFINE must be non-nil to re-promote it -- promotion never
silently overwrites an existing entry's history; a re-promotion is
appended after the old one, not in place of it.

On success: evaluates the parsed form into this Emacs, appends the
provenance comment and SOURCE to the file (ahead of the trailing
`(provide \\='claude-lib)' form), saves it, and returns the defined
symbol."
  (let ((destination
         (cond
          ((null destination) "edmacs")
          ((string-empty-p (string-trim destination))
           (user-error "claude-lib-promote: DESTINATION must not be blank"))
          (t destination))))
    (when (or (null problem) (string-empty-p (string-trim problem)))
      (user-error "claude-lib-promote: PROBLEM must be a non-empty string"))
    (let ((forms (claude-lib--read-source-forms source)))
      (unless (= (length forms) 1)
        (user-error "claude-lib-promote: SOURCE must contain exactly one top-level form, got %d"
                    (length forms)))
      (let* ((form (car forms))
             (head (car-safe form)))
        (unless (memq head '(defun cl-defun defmacro))
          (user-error "claude-lib-promote: SOURCE must be a single defun/cl-defun/defmacro form, got %S"
                      head))
        (let ((name (nth 1 form)))
          (unless (and (symbolp name)
                       (string-match-p "\\`claude-lib-" (symbol-name name)))
            (user-error "claude-lib-promote: %S must carry the claude-lib- prefix" name))
          (let ((docstring (nth 3 form)))
            (unless (and (stringp docstring) (not (string-empty-p docstring)))
              (user-error "claude-lib-promote: %s has no docstring" name))
            (let ((first-line (car (split-string docstring "\n"))))
              (unless (string-suffix-p "." first-line)
                (user-error
                 "claude-lib-promote: %s's docstring first line must end in a complete sentence (a period): %S"
                 name first-line))))
          (let ((buf (claude-lib--ensure-fresh-buffer claude-lib-file)))
            (with-current-buffer buf
              (when (and (not allow-redefine) (claude-lib--name-defined-in-file-p name))
                (user-error
                 "claude-lib-promote: %s is already defined in %s; pass a non-nil allow-redefine to re-promote"
                 name claude-lib-file))
              (goto-char (point-min))
              (unless (re-search-forward "^(provide 'claude-lib)" nil t)
                (user-error "claude-lib-promote: could not find the trailing (provide 'claude-lib) form in %s"
                            claude-lib-file))
              (goto-char (match-beginning 0))
              ;; Collapse whatever blank run already precedes (provide ...)
              ;; so successive promotions never accumulate extra blank
              ;; lines here -- exactly one separates each entry.
              (let ((provide-start (point)))
                (skip-chars-backward "\n\t ")
                (delete-region (point) provide-start))
              (insert "\n\n" (claude-lib--provenance-comment destination problem)
                      (string-trim-right source) "\n\n")
              (eval form t)
              (save-buffer))
            name))))))

(provide 'claude-lib)
;;; claude-lib.el ends here
