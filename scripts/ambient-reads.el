;;; ambient-reads.el --- flag ambient state reads in edmacs modules -*- lexical-binding: t -*-

;;; Commentary:
;; No off-the-shelf Emacs linter flags "this function reads the selected
;; frame instead of taking one".  That read is the root cause of a whole
;; bug family in this config -- a function called with an explicit FRAME
;; that then consults `(selected-frame)' halfway down works perfectly
;; until the day it is called for a frame that is not selected.
;;
;; Usage:
;;
;;   emacs -Q --batch -l scripts/ambient-reads.el \
;;         -f edmacs-ambient-reads-batch modules/*.el
;;
;;   emacs -Q --batch -l scripts/ambient-reads.el \
;;         --eval '(setq edmacs-ambient-reads-only-errors t)' \
;;         -f edmacs-ambient-reads-batch modules/*.el
;;
;; Exits non-zero when any ERROR-severity finding is reported, so it can
;; gate a landing run.  WARN findings are informational: a function with
;; no parameter of the relevant kind is ambient by design, and threading
;; an argument through it is a refactor, not a bug fix.
;;
;; Two severities, and the distinction is the whole point:
;;
;;   ERROR  the enclosing function HAS a parameter of the right kind and
;;          reads the ambient value anyway, somewhere other than the
;;          `(or FRAME (selected-frame))' defaulting idiom.  That is the
;;          bug shape: the caller's argument is accepted and then ignored.
;;
;;   WARN   the enclosing function has no such parameter at all.  Ambient
;;          by construction; a candidate for taking one.
;;
;; Known limitation: a read inside a nested `lambda' is attributed to the
;; enclosing top-level definition, so a closure that deliberately reads a
;; caller\'s dynamic binding is reported against the outer function\'s
;; parameter list.  Annotate those with the suppression comment below.
;;
;; Deliberately a plain source walk rather than a byte-compiler warning:
;; the compiler sees macro-expanded code, where `if-let'/`when-let' and
;; this config's own macros have already rewritten the argument lists a
;; parameter-vs-ambient comparison depends on.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst edmacs-ambient-reads-kinds
  '((frame  "(selected-frame)"  "(selected-frame)"
            ("frame" "frames" "f" "the-frame" "target-frame" "new-frame" "orig-frame"))
    (window "(selected-window)" "(selected-window)"
            ("window" "windows" "win" "w" "target-window" "orig-window"))
    (buffer "(current-buffer)"  "(current-buffer)"
            ("buffer" "buf" "b" "target-buffer" "orig-buffer"))
    (dir    "default-directory" "\\_<default-directory\\_>"
            ("dir" "directory" "default-directory" "root" "path" "worktree" "repo")))
  "Ambient reads to flag: (KIND LABEL REGEXP PARAMETER-NAMES).
PARAMETER-NAMES are the argument names that count as \"this function was
handed one of these already\"; a parameter whose name merely ENDS in one
of them counts too, so `sidebar-frame' matches KIND `frame'.")

(defvar edmacs-ambient-reads-only-errors nil
  "When non-nil, `edmacs-ambient-reads-batch' prints ERROR findings only.")

(defconst edmacs-ambient-reads-definers
  '(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst define-inline)
  "Top-level forms whose second element is a name and third an arglist.")

(defun edmacs-ambient-reads--parameter-names (arglist)
  "Return ARGLIST's parameter names as strings, lambda-list keywords dropped."
  (let (names)
    (dolist (a (if (listp arglist) arglist nil))
      (let ((sym (if (consp a) (car a) a)))
        (when (and (symbolp sym) sym
                   (not (string-prefix-p "&" (symbol-name sym))))
          (push (symbol-name sym) names))))
    (nreverse names)))

(defun edmacs-ambient-reads--has-parameter-p (params candidates)
  "Non-nil when PARAMS holds a name equal to, or ending in `-NAME', for
some NAME in CANDIDATES."
  (cl-some (lambda (p)
             (cl-some (lambda (c)
                        (or (string= p c) (string-suffix-p (concat "-" c) p)))
                      candidates))
           params))

(defconst edmacs-ambient-reads--assignment-heads
  '(setq setq-local setq-default setf push cl-pushnew cl-incf cl-decf add-to-list)
  "Forms whose second element is assigned to rather than read from.")

(defun edmacs-ambient-reads--binding-or-write-p ()
  "Non-nil when point is at a BINDING or an assignment, not a read.
`(let ((default-directory root)) ...)' scopes the variable -- the correct
way to use it -- and `(setq default-directory root)' writes it; neither
is the ambient read this tool is looking for."
  (save-excursion
    (condition-case nil
        (let ((symbol-start (point)))
          (backward-up-list 1 t t)
          (let ((head-start (point))
                (form (save-excursion (read (current-buffer)))))
            (and (consp form)
                 (or
                  ;; `(VAR VALUE)' or `(VAR)' in a `let' varlist: the symbol
                  ;; is the head of its own list.
                  (= symbol-start (1+ head-start))
                  (memq (car form) edmacs-ambient-reads--assignment-heads)))))
      (error nil))))

(defun edmacs-ambient-reads--defaulting-idiom-p (candidates)
  "Non-nil when point sits inside `(or PARAM ...)' for a matching PARAM.
That is the sanctioned shape -- `(let ((frame (or frame (selected-frame))))'
honours an explicit argument and only falls back when none was given.
Walks up to three levels out, so a fallback wrapped in an intervening
`if' or `and' still counts."
  (save-excursion
    (condition-case nil
        (catch 'found
          (dotimes (_ 3)
            (backward-up-list 1 t t)
            (let ((form (save-excursion (read (current-buffer)))))
              (when (and (consp form)
                         (eq (car form) 'or)
                         (symbolp (cadr form))
                         (edmacs-ambient-reads--has-parameter-p
                          (list (symbol-name (cadr form))) candidates))
                (throw 'found t))))
          nil)
      (error nil))))

(defun edmacs-ambient-reads--interactive-spec-p ()
  "Non-nil when point sits inside a function's `(interactive (list ...))' form.
That is the other sanctioned shape -- `(interactive (list (selected-frame)))'
supplies the ambient value as the single, auditable argument
`call-interactively' passes in, rather than defaulting it deep in the
body; `edmacs-window-promote's numeric-prefix branch nests the read
inside an intervening `if'/`let*'/`progn', so this walks up to six
levels rather than the three levels `--defaulting-idiom-p' needs.
Like that sibling predicate, this only answers \"is point inside an
`interactive' form\" -- the caller gates the exemption on `has' (the
enclosing function actually taking a parameter of the matching kind)
exactly as it does for `--defaulting-idiom-p', so a command with NO
such parameter reading `(selected-frame)' in its `interactive' spec
still reports WARN rather than being silently exempted."
  (save-excursion
    (condition-case nil
        (catch 'found
          (dotimes (_ 6)
            (backward-up-list 1 t t)
            (when (eq (car-safe (save-excursion (read (current-buffer))))
                      'interactive)
              (throw 'found t)))
          nil)
      (error nil))))

(defconst edmacs-ambient-reads-nil-arg-functions
  '(("frame-parameter"       0 frame)
    ("frame-parameters"      0 frame)
    ("frame-selected-window" 0 frame)
    ("window-list"           0 frame)
    ("window-parameter"      0 window)
    ("next-window"           0 window)
    ("previous-window"       0 window)
    ("get-buffer-window"     0 buffer))
  "Functions whose Nth (0-based) argument defaults to the selected
frame/window, or the current buffer, when nil -- a literal `nil' there
is the same ambient-read idiom as a literal
`(selected-frame)'/`(selected-window)'/`(current-buffer)' call. Each
entry is (FUNCTION-NAME ARG-INDEX KIND); KIND indexes
`edmacs-ambient-reads-kinds' for its LABEL and PARAMETER-NAMES.")

(defun edmacs-ambient-reads--nil-arg-exempt-p (fn-name form)
  "Non-nil when FORM (a call to FN-NAME) is a sanctioned nil-argument idiom.
`(get-buffer-window nil t)' means \"the window showing the CURRENT
buffer, on any frame\" -- a deliberate, non-ambient-frame read, not the
\"this frame/window defaults away an argument the caller supplied\" bug
shape the rest of this table exists to catch."
  (and (string= fn-name "get-buffer-window")
       (eq (nth 2 form) t)))

(defun edmacs-ambient-reads--nil-arg-findings (file name params start end)
  "Return nil-argument-idiom findings for the definition spanning [START,END).
Mirrors the literal-symbol scan in `edmacs-ambient-reads-file', but for
the functions in `edmacs-ambient-reads-nil-arg-functions': a literal
`nil' in the flagged argument position, rather than a literal
`(selected-frame)'-shaped call."
  (let (findings)
    (pcase-dolist (`(,fn-name ,arg-index ,kind) edmacs-ambient-reads-nil-arg-functions)
      (let* ((kind-entry (assq kind edmacs-ambient-reads-kinds))
             (label (format "nil argument to `%s' (defaults to %s)"
                            fn-name (nth 1 kind-entry)))
             (candidates (nth 3 kind-entry)))
        (save-excursion
          (goto-char start)
          (while (re-search-forward
                  (concat "(\\s-*" (regexp-quote fn-name) "\\_>") end t)
            (let ((call-start (match-beginning 0)))
              (goto-char call-start)
              (if (not (edmacs-ambient-reads--in-code-p))
                  (goto-char (1+ call-start))
                (let ((form (condition-case nil
                                (save-excursion (read (current-buffer)))
                              (error nil))))
                  (goto-char (1+ call-start))
                  (when (and (consp form)
                             (> (length form) (1+ arg-index))
                             (eq (nth (1+ arg-index) form) nil)
                             (not (edmacs-ambient-reads--nil-arg-exempt-p
                                   fn-name form))
                             (not (save-excursion
                                    (goto-char call-start)
                                    (edmacs-ambient-reads--suppressed-p))))
                    (let ((has (edmacs-ambient-reads--has-parameter-p
                                params candidates)))
                      (push (list file (line-number-at-pos call-start)
                                  (if has 'error 'warn) name label)
                            findings))))))))))
    findings))

(defconst edmacs-ambient-reads-suppression "ambient-reads: ok"
  "Comment text that suppresses a finding on its own or the preceding line.
For a read that is ambient on purpose -- a stub closure that must see the
caller\'s dynamic binding, say -- annotate it rather than reshaping the
code around the linter.")

(defun edmacs-ambient-reads--suppressed-p ()
  "Non-nil when this line, or the one above it, carries the suppression."
  (save-excursion
    (let ((this (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))
          (prev (progn (forward-line -1)
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))))
      (or (string-match-p edmacs-ambient-reads-suppression this)
          (string-match-p edmacs-ambient-reads-suppression prev)))))

(defun edmacs-ambient-reads--in-code-p ()
  "Non-nil when point is in code -- not a comment and not a string."
  (let ((state (syntax-ppss)))
    (not (or (nth 3 state) (nth 4 state)))))

(defun edmacs-ambient-reads-file (file)
  "Return a list of findings for FILE.
Each finding is (FILE LINE SEVERITY FUNCTION LABEL)."
  (let (findings)
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (let ((forms nil))
        ;; Collect (NAME PARAMS START END) for every top-level definer.
        (condition-case nil
            (while t
              (let* ((start (progn (skip-chars-forward " \t\n\f") (point)))
                     (form (read (current-buffer)))
                     (end (point)))
                (when (and (consp form)
                           (memq (car form) edmacs-ambient-reads-definers)
                           (symbolp (cadr form)))
                  (push (list (symbol-name (cadr form))
                              (edmacs-ambient-reads--parameter-names (nth 2 form))
                              start end)
                        forms))))
          (error nil))
        (dolist (entry (nreverse forms))
          (cl-destructuring-bind (name params start end) entry
            (pcase-dolist (`(,_kind ,label ,regexp ,candidates)
                           edmacs-ambient-reads-kinds)
              (save-excursion
                (goto-char start)
                (while (re-search-forward regexp end t)
                  ;; Capture both ends up front: `syntax-ppss' and
                  ;; `backward-up-list' below both clobber the match data.
                  (let ((hit (match-beginning 0))
                        (after (match-end 0)))
                    (goto-char hit)
                    (when (and (edmacs-ambient-reads--in-code-p)
                               (not (edmacs-ambient-reads--binding-or-write-p))
                               (not (edmacs-ambient-reads--suppressed-p)))
                      (let ((has (edmacs-ambient-reads--has-parameter-p
                                  params candidates)))
                        (unless (and has
                                     (or (edmacs-ambient-reads--defaulting-idiom-p
                                          candidates)
                                         (edmacs-ambient-reads--interactive-spec-p)))
                          (push (list file (line-number-at-pos hit)
                                      (if has 'error 'warn) name label)
                                findings))))
                    (goto-char after)))))
            (setq findings
                  (append findings
                          (edmacs-ambient-reads--nil-arg-findings
                           file name params start end))))))
      nil)
    (nreverse findings)))

(defun edmacs-ambient-reads-batch ()
  "Report ambient reads for every file named on the command line.
Exits non-zero when any ERROR finding was reported."
  (let ((files command-line-args-left)
        (all nil))
    (setq command-line-args-left nil)
    (dolist (file files)
      (setq all (append all (edmacs-ambient-reads-file file))))
    (let ((errors (cl-count 'error all :key (lambda (f) (nth 2 f))))
          (warns (cl-count 'warn all :key (lambda (f) (nth 2 f)))))
      (dolist (f all)
        (cl-destructuring-bind (file line severity fn label) f
          (when (or (eq severity 'error) (not edmacs-ambient-reads-only-errors))
            (princ (format "%s:%d: %s: %s reads %s\n"
                           file line (upcase (symbol-name severity)) fn label)))))
      (princ (format "\n%d ambient read(s): %d error, %d warn, across %d file(s)\n"
                     (length all) errors warns (length files)))
      (kill-emacs (if (> errors 0) 1 0)))))

(provide 'ambient-reads)
;;; ambient-reads.el ends here
