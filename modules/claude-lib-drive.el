;;; claude-lib-drive.el --- Drive interactive Emacs code without wedging the daemon -*- lexical-binding: t -*-

;;; Commentary:
;; The loop for prototyping Emacs INTERACTION -- commands, keymaps,
;; modes, transients, pickers -- rather than rendering a view for a
;; human to read.  `modules/claude-lib-view.el' serves the read half;
;; this file serves the half where the artifact under test is a
;; keystroke away from a `completing-read'.
;;
;; ------------------------------------------------------------------
;; THE HAZARD.  An interactive form is not a bug someone might write.
;; It is the ordinary shape of every surface an Emacs package is made
;; of, and evaluating one naively through the eval channel does not
;; merely hang its own request -- it takes the daemon down for every
;; later client.  Reproduced against a throwaway daemon:
;;
;;   emacsclient -e '(completing-read "pick: " (list "alpha" "beta"))'
;;     => rc=124, timed out
;;   emacsclient -e '(list :alive t)'        ; a second, independent client
;;     => rc=124, ALSO timed out
;;   emacsclient -e '(kill-emacs)'           ; recovery through the channel
;;     => never returned; the daemon needed a real kill -9
;;
;; So the rule is absolute: NEVER call an interactive surface directly
;; from the eval channel.  Call it through `claude-lib-drive' or
;; `claude-lib-drive-command', which cannot block (see ANTI-WEDGE
;; below), and do it against a scratch daemon -- `scripts/claude-scratch.sh'
;; -- not the daemon the operator works in all day.
;;
;; ------------------------------------------------------------------
;; TWO MITIGATIONS, and which to prefer.  Both verified to return
;; promptly and leave the daemon alive:
;;
;;   ;; pre-fed input -- drives the REAL `completing-read'.  PREFER THIS.
;;   (let ((unread-command-events (listify-key-sequence (kbd "b e t a RET"))))
;;     (completing-read "pick: " (list "alpha" "beta")))      => "beta"
;;
;;   ;; stub -- fast, but bypasses the code path actually under test
;;   (let ((completing-read-function (lambda (p c &rest _) "beta")))
;;     (completing-read "pick: " (list "alpha" "beta")))      => "beta"
;;
;; The stub proves only that a caller handles a return value.  The
;; pre-fed form exercises the completion machinery itself -- which,
;; for a package built on consult, embark and friends, IS the code
;; under test.  `claude-lib-drive's `:keys' is the pre-fed form and
;; `:choice' is the stub; reach for `:choice' only when the caller
;; rather than the picker is the point.
;;
;; A `:keys' string is `kbd'-parsed and let-bound, never `setq'ed, so
;; input the driven code did not consume is discarded here instead of
;; leaking into the daemon's command loop and prefixing the operator's
;; next keystroke.  How much was left over comes back as
;; `:unconsumed-keys', so a wrong `:keys' string is visible rather than
;; silent.
;;
;; ------------------------------------------------------------------
;; THE SELECTED-WINDOW TRAP, verified and easy to get wrong.  Simulated
;; keys follow the SELECTED WINDOW, not the current buffer.
;; `execute-kbd-macro' inside `with-temp-buffer' typed into the selected
;; window's buffer: the returned string was `*scratch*''s contents with
;; the typed characters appended, and `current-buffer' was left pointing
;; there afterwards.  In a live daemon that means keys land in whatever
;; the operator was editing.  So the target window is `select-window'ed
;; BEFORE any input is fed, and what that changed is put back.
;;
;; The configuration restored is the TARGET WINDOW'S OWN FRAME'S:
;; `save-window-excursion' only ever covers the selected frame, and this
;; config is multi-frame by design, so a command acting on a window
;; elsewhere would otherwise leave that frame's real layout standing.
;; Same fix, same reason, as the promoted
;; `claude-lib-window-buffer-after-command' in `modules/claude-lib.el'.
;;
;; ------------------------------------------------------------------
;; ANTI-WEDGE, two INDEPENDENT layers, because the whole point is that
;; one of them failing must not take the daemon with it:
;;
;;   primary   a `minibuffer-setup-hook' entry.  When a prompt opens
;;             with `unread-command-events' empty -- nothing left to
;;             answer it with -- it records the prompt and schedules
;;             `abort-recursive-edit'.  An unanswerable prompt aborts
;;             instead of blocking, and comes back as `:error'
;;             `unanswered-prompt' with the prompt text in `:prompts'.
;;   backstop  `with-timeout' on `claude-lib-drive-default-timeout',
;;             covering a form that blocks somewhere other than the
;;             minibuffer.  Comes back as `:error' `timeout'.
;;
;; `abort-recursive-edit' signals `quit', which would otherwise
;; propagate out and abort the caller's whole eval form; it is caught
;; and reported as data, since an uncaught quit through the eval channel
;; is a confusing non-answer that defeats the point of observing at all.
;;
;; ------------------------------------------------------------------
;; A BATCH EMACS CANNOT TEST THE MINIBUFFER LAYER.  Under `--batch',
;; `noninteractive' is t and `read-from-minibuffer' reads from STDIN:
;; the pre-fed form above yields `(end-of-file "Error reading from
;; stdin")' and no `minibuffer-setup-hook' ever runs.  Everything about
;; prompts is therefore tested in `claude-lib-drive-live-test.el'
;; against a real daemon, not in the in-process suite.  The window pin,
;; the restore, the `:choice' stub and `execute-kbd-macro' itself all
;; work in batch and are tested there.
;;
;; ------------------------------------------------------------------
;; RELOADING, and an honest account of its limits -- see
;; `claude-lib-reload'.  `load-file' re-evaluates `defun's, which covers
;; most iteration.  It does NOT reset `defvar', `defcustom' or `defface'
;; (they only initialise when unbound), it double-adds anything the file
;; `add-hook's, it stacks `advice-add', and a keymap already installed
;; keeps the bindings it was given.  A long prototyping session
;; therefore accumulates state that is invisible and eventually
;; misleading -- a "fix" that works only because a stale hook is still
;; installed.  The remedy is `scripts/claude-scratch.sh restart', which
;; is cheap precisely because the daemon is throwaway.  There is
;; deliberately no teardown framework here.
;;
;; ------------------------------------------------------------------
;; THE `-Q' GATE IS A STEP IN THE LOOP, not a pre-landing afterthought
;; -- see `claude-lib-check-q'.  The daemon a prototype is driven in has
;; evil, evil-collection, consult, embark, magit and transient all
;; loaded, so a hard `require' leaking into code whose declared
;; dependency posture is SOFT is INVISIBLE from there and only ever
;; fails later, in the consumer repo's own `emacs -Q --batch' harness.
;; A prototype is not done when it works in the daemon; it is done when
;; it also byte-compiles and loads under `-Q'.
;;
;; ------------------------------------------------------------------
;; NOT A TEST FRAMEWORK.  Four entry points -- `claude-lib-drive',
;; `claude-lib-drive-command', `claude-lib-reload', `claude-lib-check-q'
;; -- over one internal driver is the ceiling.  ERT already exists and a
;; consumer package brings its own harness; a `define-interaction-test'
;; macro, a teardown registry or an assertion vocabulary here would mean
;; this has gone wrong.
;;
;; NAMESPACE.  Entry points are the four above; every internal helper is
;; `claude-lib--drive-NAME'/`claude-lib--check-q-NAME' (double dash)
;; rather than `claude-lib-drive--NAME', because the library's
;; entry-point-only regexp is "\\`claude-lib-[^-]" and the latter shape
;; would fill a listing of what is callable with this file's internals.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)

;; Defined by `modules/claude-lib.el', which loads first from init.el but
;; which a bare `-Q' tier need not have loaded at all; declared so a
;; plain `batch-byte-compile' of this file is clean.  Same
;; forward-declaration posture as `modules/claude-lib-view.el'.
(defvar edmacs-claude-lib-max-output-bytes)
(declare-function edmacs-claude-lib--read-forms-in-current-buffer "claude-lib" (context))

(defcustom claude-lib-drive-default-timeout 5
  "Seconds `claude-lib-drive' lets a driven form run before giving up.
The backstop layer, not the primary one: an unanswerable minibuffer
prompt is aborted by `claude-lib--drive-abort' well before this
expires.  This covers a form that blocks somewhere else entirely."
  :type 'number
  :group 'claude-lib)

(defcustom claude-lib-check-q-timeout 60
  "Seconds `claude-lib-check-q' waits for each of its two batch children.
A hung child is a FAILED check, not a pass -- matching
`claude-lib-verify-load-timeout's posture in `modules/claude-lib.el'."
  :type 'number
  :group 'claude-lib)

;; ============================================================================
;; Output capture
;; ============================================================================

(defun claude-lib--drive-message-advice (capture-buffer)
  "Return an `:around' advice for `message' mirroring it into CAPTURE-BUFFER.
Deliberately NOT `cl-letf': `message' is a C subr, and `cl-letf' on one
forces a synchronous native-comp trampoline build (~28s on a cold
eln-cache, see this repo's CLAUDE.md).  This path runs on every single
drive call.

Mirrors `edmacs-claude-lib--message-advice' rather than calling it, so
this module still drives interactive code in a bare `-Q' image that
never loaded `modules/claude-lib.el'.  `(message nil)' cancels a pending
echo-area message rather than printing the string \"nil\", so a nil
first argument inserts nothing; the original is always called through."
  (lambda (orig-fun &rest args)
    (when (car args)
      (with-current-buffer capture-buffer
        (goto-char (point-max))
        (insert (apply #'format-message args) "\n")))
    (apply orig-fun args)))

;; ============================================================================
;; The driver
;; ============================================================================

(defun claude-lib--drive-abort ()
  "Abort a minibuffer read that has nothing left to answer it.
Scheduled from a zero-delay timer by `claude-lib--drive-1's
`minibuffer-setup-hook' entry, which is the only way to interrupt a read
that has already begun.  Checks `minibuffer-depth' first: by the time
the timer fires the prompt may already have exited, and
`abort-recursive-edit' with no recursive edit in progress errors."
  (when (> (minibuffer-depth) 0)
    (abort-recursive-edit)))

(defun claude-lib--drive-1 (thunk keys)
  "Run THUNK pinned to KEYS' target window and report what it did.
The single owner of the pin/feed/observe/restore sequence shared by
`claude-lib-drive' and `claude-lib-drive-command'; KEYS is their plist,
and their docstrings describe it.  Returns their result plist."
  (let ((window (or (plist-get keys :window) (selected-window))) ; ambient-reads: ok
        (buffer (plist-get keys :buffer))
        (key-string (plist-get keys :keys))
        (choice (plist-get keys :choice))
        (timeout (or (plist-get keys :timeout) claude-lib-drive-default-timeout)))
    (unless (window-live-p window)
      (user-error "claude-lib-drive: WINDOW is not live"))
    (let* ((wconfig (current-window-configuration (window-frame window)))
           (capture (generate-new-buffer " *claude-lib-drive-capture*"))
           (advice (claude-lib--drive-message-advice capture))
           (prompts nil) (unanswered nil)
           (value nil) (err nil) (unconsumed 0)
           (window-buffer nil) (current nil) (messages ""))
      (unwind-protect
          (progn
            (advice-add 'message :around advice)
            (save-selected-window
              (let ((minibuffer-setup-hook
                     (cons (lambda ()
                             (push (or (minibuffer-prompt) "") prompts)
                             ;; Empty here means the caller supplied nothing
                             ;; that could answer this prompt -- abort rather
                             ;; than block the daemon for every later client.
                             (when (null unread-command-events)
                               (setq unanswered t)
                               (run-with-timer 0 nil #'claude-lib--drive-abort)))
                           minibuffer-setup-hook))
                    ;; Bound even with no `:keys', so a stray pending event
                    ;; cannot answer a prompt on the caller's behalf, and so
                    ;; nothing the driven code pushes survives this call.
                    (unread-command-events
                     (and key-string (listify-key-sequence (kbd key-string))))
                    (completing-read-function
                     (cond ((functionp choice) choice)
                           ((stringp choice) (lambda (&rest _) choice))
                           (t completing-read-function)))
                    (standard-output capture))
                (select-window window)
                (when buffer (set-window-buffer window (get-buffer-create buffer)))
                (condition-case signalled
                    (setq value (with-timeout (timeout (setq err 'timeout) nil)
                                  (funcall thunk)))
                  (quit (setq err (if unanswered 'unanswered-prompt 'quit)))
                  (error (setq err signalled)))
                (setq unconsumed (length unread-command-events))
                ;; A command may have deleted the pinned window; report that
                ;; rather than dereferencing it.  The restore below still runs.
                (setq window-buffer (and (window-live-p window)
                                         (buffer-name (window-buffer window))))
                (setq current (buffer-name)))))
        (advice-remove 'message advice)
        (setq messages (with-current-buffer capture (buffer-string)))
        (kill-buffer capture)
        (set-window-configuration wconfig t))
      (list :value value
            :messages messages
            :window-buffer window-buffer
            :current-buffer current
            :prompts (nreverse prompts)
            :unconsumed-keys unconsumed
            :error err))))

(defun claude-lib-drive (thunk &rest keys)
  "Run THUNK with simulated input and return a plist of what it did.
The return value is
 (:value V :messages TEXT :window-buffer NAME :current-buffer NAME
  :prompts (STRING...) :unconsumed-keys N
  :error nil|timeout|quit|unanswered-prompt|(SIGNAL . DATA)).
:window-buffer is nil when THUNK deleted the pinned window.

THUNK is a function of no arguments.  It CANNOT block the daemon: a
minibuffer prompt opened with nothing left to answer it is aborted and
reported as :error `unanswered-prompt' with the prompt in :prompts, and
anything else that hangs is cut off by :timeout as :error `timeout'.
That is the whole reason to route an interactive form through here
rather than evaluating it directly -- a bare `completing-read' through
the eval channel wedges the daemon for every later client, not just its
own request.

KEYS is a plist:
  :keys     a `kbd' key-sequence string pre-fed to `unread-command-events'.
            PREFER THIS: it drives the real `completing-read' and the
            real completion machinery, which is usually the code under
            test.  Let-bound, so whatever was not consumed is discarded
            here rather than leaking into the command loop; the leftover
            count comes back as :unconsumed-keys.
  :choice   a string every `completing-read' answers with, or a function
            to bind as `completing-read-function'.  The STUB: it bypasses
            the picker, so use it only when the caller is the point.
  :window   the window to select before feeding input, defaulting to the
            selected one.  Simulated keys follow the SELECTED WINDOW, not
            the current buffer, so this is what decides where they land.
  :buffer   a buffer (or name) to show in that window first.
  :timeout  seconds for the backstop, defaulting to
            `claude-lib-drive-default-timeout'.

The window configuration of :window's OWN FRAME is restored afterwards
-- not the caller's, which `save-window-excursion' would have restored
instead, leaving a command's real layout changes standing on a frame the
caller was never on."
  (claude-lib--drive-1 thunk keys))

(defun claude-lib-drive-command (command &rest keys)
  "Run COMMAND through `call-interactively' and return a plist of what it did.
The return value and KEYS are exactly `claude-lib-drive's; this differs
only in running COMMAND the way a keystroke would, so its interactive
spec and whatever `display-buffer' placement rules apply to it are
exercised for real rather than approximated by a direct call."
  (claude-lib--drive-1 (lambda () (call-interactively command)) keys))

;; ============================================================================
;; Reload, and what it does not do
;; ============================================================================

(defconst claude-lib--drive-stale-definers '(defvar defcustom defface)
  "Definers whose value survives a reload, because they only init when unbound.")

(defconst claude-lib--drive-stacking-forms
  '((add-hook . :add-hook)
    (advice-add . :advice)
    (define-advice . :advice)
    (define-key . :keymap)
    (keymap-set . :keymap)
    (add-to-list . :add-to-list)
    (evil-set-initial-state . :evil-initial-state))
  "Forms that STACK rather than replace when a file is loaded twice.")

(defun claude-lib--drive-file-forms (file)
  "Return every top-level form in FILE, in order.
Delegates to `modules/claude-lib.el's reader rather than adding a third
one to this repo; signals a `user-error' naming that file when it has
not been loaded."
  (unless (fboundp 'edmacs-claude-lib--read-forms-in-current-buffer)
    (user-error "claude-lib-reload: load modules/claude-lib.el first (its reader is not defined)"))
  (with-temp-buffer
    (insert-file-contents file)
    (edmacs-claude-lib--read-forms-in-current-buffer file)))

(defun claude-lib--drive-walk (form definers counts)
  "Tally FORM's definer names into DEFINERS and stacking forms into COUNTS.
Both are cons cells used as mutable boxes.  Walks the whole tree, not
just the top level, so a form inside `with-eval-after-load' or `progn'
is counted too; it cannot tell code from quoted data, which is why
`claude-lib-reload' reports a heuristic rather than a proof."
  (when (consp form)
    (let ((head (car form)))
      (when (symbolp head)
        (when (and (memq head claude-lib--drive-stale-definers)
                   (symbolp (nth 1 form)) (nth 1 form))
          (push (nth 1 form) (car definers)))
        (let ((key (cdr (assq head claude-lib--drive-stacking-forms))))
          (when key
            (setf (alist-get key (car counts)) (1+ (or (alist-get key (car counts)) 0)))))))
    (dolist (sub form)
      (claude-lib--drive-walk sub definers counts))))

(defun claude-lib-reload (file)
  "Reload FILE and return a plist naming what the reload did NOT undo.
The return value is
 (:file F :loaded t :not-reset (SYMBOL...) :add-hook N :advice N
  :keymap N :add-to-list N :evil-initial-state N :restart-recommended BOOL).

`load-file' re-evaluates `defun's, which covers most iteration.  It does
NOT do the rest, and this is the honest accounting of that:
:not-reset names every `defvar'/`defcustom'/`defface' in FILE, all of
which keep their stale values because they only initialise when unbound;
the counts are forms that STACK on a second load -- `add-hook'
double-adds, `advice-add'/`define-advice' pile up, `define-key' and
`keymap-set' leave an already-installed keymap holding the bindings it
was given, `add-to-list' and `evil-set-initial-state' likewise.

:restart-recommended is non-nil whenever any stacking form is present.
Its remedy is `scripts/claude-scratch.sh restart' -- restarting the
throwaway scratch daemon, which is cheap precisely because it is
throwaway.  There is deliberately no teardown framework here: a long
prototyping session that keeps reloading instead accumulates invisible
state, and eventually a \"fix\" that works only because a stale hook is
still installed."
  (let ((path (expand-file-name file)))
    (unless (file-readable-p path)
      (user-error "claude-lib-reload: cannot read %s" path))
    (load-file path)
    (let ((definers (list nil))
          (counts (list nil)))
      (dolist (form (claude-lib--drive-file-forms path))
        (claude-lib--drive-walk form definers counts))
      (let* ((tally (car counts))
             (n (lambda (key) (or (alist-get key tally) 0)))
             (stacking (+ (funcall n :add-hook) (funcall n :advice)
                          (funcall n :keymap) (funcall n :add-to-list)
                          (funcall n :evil-initial-state))))
        (list :file path
              :loaded t
              :not-reset (nreverse (car definers))
              :add-hook (funcall n :add-hook)
              :advice (funcall n :advice)
              :keymap (funcall n :keymap)
              :add-to-list (funcall n :add-to-list)
              :evil-initial-state (funcall n :evil-initial-state)
              :restart-recommended (> stacking 0))))))

;; ============================================================================
;; The `-Q' gate
;; ============================================================================

(defun claude-lib--check-q-environment ()
  "Return `process-environment' with EMACSLOADPATH removed.
Without this the child inherits the daemon's fully-loaded path and the
whole gate passes on a file that would fail for a real `-Q' consumer --
the exact class of miss the gate exists to prevent."
  (seq-remove (lambda (entry) (string-prefix-p "EMACSLOADPATH=" entry))
              process-environment))

(defun claude-lib--check-q-compile-form (file dest)
  "Return the `--eval' form byte-compiling FILE to DEST, as a string.
`bytecomp' is required first because `--eval' reads with lexical
binding: without the require, `byte-compile-dest-file-function' has no
dynamic declaration yet and `let' on it errors \"Defining as dynamic an
already lexical var\" -- which would fail every check, including the
files that are fine."
  (format "(progn (require (quote bytecomp))
                  (setq byte-compile-dest-file-function (lambda (_) %S))
                  (kill-emacs (if (byte-compile-file %S) 0 1)))"
          dest file))

(defun claude-lib--check-q-run (args timeout)
  "Run \"emacs ARGS\" in a fresh batch child and return (EXIT . OUTPUT).
EXIT is nil when the child outlived TIMEOUT seconds and was killed --
a hung child is a failed check, never a silent pass.  Follows
`claude-lib--verify-file-loads's shape in `modules/claude-lib.el'."
  (let ((emacs (executable-find "emacs")))
    (unless emacs
      (user-error "claude-lib-check-q: no `emacs' on exec-path"))
    (with-temp-buffer
      (let* ((process-environment (claude-lib--check-q-environment))
             (proc (make-process
                    :name "claude-lib-check-q"
                    :buffer (current-buffer)
                    :noquery t
                    ;; A failing child is this function's normal path, so the
                    ;; default sentinel's "exited abnormally" would be noise.
                    :sentinel #'ignore
                    :connection-type 'pipe
                    :command (cons emacs args)))
             (deadline (+ (float-time) timeout)))
        (while (and (process-live-p proc) (< (float-time) deadline))
          (accept-process-output proc 0.05))
        (if (process-live-p proc)
            (progn (delete-process proc)
                   (cons nil (buffer-string)))
          (while (accept-process-output proc 0.05))
          (cons (process-exit-status proc) (buffer-string)))))))

(defun claude-lib--check-q-missing-features (output)
  "Return the features OUTPUT reports as unloadable, as a list of symbols."
  (let ((start 0) (found nil))
    (while (string-match "Cannot open load file:[^,\n]*, \\([^ \n\"')]+\\)" output start)
      (push (intern (match-string 1 output)) found)
      (setq start (match-end 0)))
    (nreverse (delete-dups found))))

(defun claude-lib--check-q-warnings (output)
  "Return OUTPUT's byte-compiler warning and error lines."
  (seq-filter (lambda (line) (string-match-p "\\(Warning\\|Error\\):" line))
              (split-string output "\n" t)))

(defun claude-lib--check-q-truncate (text)
  "Truncate TEXT to the shared output ceiling, noting it when it was cut.
Reuses `edmacs-claude-lib-max-output-bytes' from `modules/claude-lib.el'
rather than defining a second ceiling; falls back only so this module
still works in a `-Q' tier that never loaded that file."
  (let ((limit (or (bound-and-true-p edmacs-claude-lib-max-output-bytes) (* 1024 1024))))
    (if (<= (string-bytes text) limit)
        text
      (concat (substring text 0 (min (length text) limit))
              "\n[claude-lib-check-q: output truncated]"))))

(defun claude-lib-check-q (file &rest keys)
  "Check FILE byte-compiles and loads under a bare `emacs -Q' and report.
The return value is
 (:ok BOOL :compiled BOOL :loaded BOOL :warnings (STRING...)
  :output TEXT :exit N :missing-features (SYMBOL...)).

This is a step IN the prototyping loop, not a pre-landing afterthought.
The daemon a prototype is driven in has evil, evil-collection, consult,
embark, magit and transient all loaded, so a hard `require' leaking into
code whose declared dependency posture is SOFT is invisible from there
and only ever fails later, in the consumer repo's own `emacs -Q --batch'
harness.  :missing-features names the offending dependency instead of
handing back a wall of text.

Two children run, and neither substitutes for the other: byte-compiling
alone can pass a file whose dependency is only referenced lazily, and
loading alone misses warnings.  The `.elc' is written into a temp
directory and deleted -- compiled output is never left beside the
source.  EMACSLOADPATH is scrubbed from both children, or a daemon that
has it set would hand them its own fully-loaded path and the gate would
pass on a file that fails for a real `-Q' consumer.

KEYS is a plist:
  :load-path  extra directories for `-L', DEFAULT NIL -- a bare `-Q' is
              the point.  FILE's own directory is always added.
  :strict     treat byte-compiler warnings as failure.
  :timeout    seconds per child, defaulting to `claude-lib-check-q-timeout'.
              A child that outlives it is a FAILED check, not a pass."
  (let* ((path (expand-file-name file))
         (extra (plist-get keys :load-path))
         (strict (plist-get keys :strict))
         (timeout (or (plist-get keys :timeout) claude-lib-check-q-timeout))
         (dirs (cons (file-name-directory path) extra))
         (load-args (apply #'append (mapcar (lambda (d) (list "-L" d)) dirs)))
         (tmpdir (make-temp-file "claude-lib-check-q" t)))
    (unless (file-readable-p path)
      (user-error "claude-lib-check-q: cannot read %s" path))
    (unwind-protect
        (let* ((compile
                (claude-lib--check-q-run
                 (append (list "-Q" "--batch") load-args
                         (list "--eval" (claude-lib--check-q-compile-form
                                         path (expand-file-name "check-q.elc" tmpdir))))
                 timeout))
               (load-run
                (claude-lib--check-q-run
                 (append (list "-Q" "--batch") load-args
                         (list "--eval" (format "(load %S nil t)" path)))
                 timeout))
               (output (concat (cdr compile) (cdr load-run)))
               (compiled (eq (car compile) 0))
               (loaded (eq (car load-run) 0))
               (warnings (claude-lib--check-q-warnings (cdr compile))))
          (list :ok (and compiled loaded (or (not strict) (null warnings)) t)
                :compiled compiled
                :loaded loaded
                :warnings warnings
                :output (claude-lib--check-q-truncate output)
                :exit (or (car load-run) (car compile))
                :missing-features (claude-lib--check-q-missing-features output)))
      (ignore-errors (delete-directory tmpdir t)))))

(provide 'claude-lib-drive)
;;; claude-lib-drive.el ends here
