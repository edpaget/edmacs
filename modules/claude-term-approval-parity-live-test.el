;;; claude-term-approval-parity-live-test.el --- Terminal-vs-Emacs approval parity -*- lexical-binding: t -*-

;;; Commentary:
;; Executes the behavioural half of phase 6's fourth "Done when" clause:
;;
;;   "The same tool-triggering prompt run in a terminal Claude and in an
;;    Emacs session under `defaultMode: auto' prompts for approval on the
;;    same set of tools."
;;
;; The retirement of `modules/claude-repl/' removed the only code path
;; that could ever have made those two sets differ: claude-repl spawned
;; `claude -p' with a `make-temp-file' settings JSON registering a
;; PreToolUse hook, so an Emacs-hosted session ran under a permission
;; configuration a terminal session did not have.  Nothing replaces it --
;; `claude-term--exec' hands `ghostel-exec' an opaque argv and lets the
;; interactive CLI raise its own prompts.  That claim was previously
;; supported only by grep and by a structural argument in a commit
;; message; a code review correctly recorded the clause as PARTIAL
;; because no comparison had actually been RUN.  This file runs one.
;;
;; WHY NOT DRIVE TWO REAL SESSIONS AND COUNT PROMPTS: a real A/B would
;; need two interactive PTY-hosted CLI sessions, a scripted prompt, model
;; calls, and a human-visible permission dialog on each side -- none of
;; which a batch harness can observe, and none of which is reproducible
;; from one run to the next.  So this file compares the INPUT that
;; decides the prompt set instead, which is both observable and exact.
;;
;; A Claude CLI session's prompt set under `defaultMode: auto' is a pure
;; function of three things:
;;
;;   1. the effective auto-mode policy resolved from the settings tiers,
;;   2. the permission-affecting command-line arguments, and
;;   3. the permission-affecting environment variables.
;;
;; Each test below pins one of them:
;;
;; - `...-effective-auto-mode-policy-is-identical' is the behavioural
;;   comparison.  It runs the REAL `claude' binary's `auto-mode config'
;;   subcommand -- documented as "print the effective auto mode config as
;;   JSON: your settings where set, defaults otherwise", i.e. the very
;;   allow/soft_deny/hard_deny rule set that decides which tools raise a
;;   prompt -- once through a login shell (the terminal side) and once
;;   through the argv, cwd and environment that the REAL
;;   `claude-term--exec' -> real `ghostel-exec' -> real
;;   `ghostel--spawn-pty' chain hands to its exec primitive (the Emacs
;;   side), and asserts the two are byte-identical.  Only the native PTY
;;   leaf is stubbed; every Lisp step that could inject a setting,
;;   including `ghostel-pre-spawn-hook', runs for real.
;;
;; - `...-spawn-argv-carries-no-permission-flag' pins (2): the argv
;;   `claude-term' resolves carries none of the CLI flags that could
;;   move the prompt set (`--settings', `--permission-mode',
;;   `--allowedTools', `--dangerously-skip-permissions', `-p', ...).
;;   This is the direct regression guard on the deleted claude-repl
;;   behaviour: `claude-repl-process.el' passed `--settings <tmpfile>'
;;   and `--output-format stream-json' on every spawn.
;;
;; - `...-spawn-env-adds-no-permission-variable' pins (3): the ghostel
;;   spawn machinery -- `ghostel-environment', the INSIDE_EMACS marker,
;;   the terminal-env block and `ghostel-pre-spawn-hook' (where
;;   claude-term-registry.el injects `EDMACS_AGENT_INSTANCE') -- adds,
;;   drops and rewrites nothing matching `CLAUDE*'/`ANTHROPIC*'.  Its
;;   control is this Emacs process's own ambient environment, which the
;;   spawn env is derived from, so the delta is precisely ghostel's
;;   contribution; a login shell is deliberately NOT the control there,
;;   because it re-resolves the environment from rc files and so reports
;;   unrelated inherited variables as differences.
;;
;; Recorded result of the run made when this file was added (2026-09-02,
;; on the author's machine, `defaultMode: auto' in
;; ~/Projects/dotfiles/claude/settings.json): all three pass -- "Ran 3
;; tests, 3 results as expected, 0 unexpected" -- and the
;; `auto-mode config' JSON hashed to
;; 9173c9df4032a3ccbdb466ea47eb1ce8c67a3677db6ca81092c9cb959ce7428c
;; identically from a login shell, from a bare `call-process', and under
;; the ghostel spawn environment.
;;
;; ONE NOTE FOR ANYONE GREPPING: this file is the only place in the
;; repository that still contains the literals `--settings', `-p' and
;; `--output-format' -- the exact flags the retired claude-repl passed on
;; every spawn.  They are here as the DENYLIST the argv test asserts
;; against, not as anything this configuration passes.  An audit that
;; greps for them should expect exactly these hits and no others; a hit
;; anywhere else is the regression.
;;
;; Every test `ert-skip's rather than fails when its prerequisites are
;; absent (no `claude' on `exec-path', no fetched ghostel source in this
;; checkout), so a bare clone or CI box does not report a red run for a
;; machine-local dependency.
;;
;; Run:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/claude-term.el \
;;         -l modules/claude-term-registry.el \
;;         -l modules/claude-term-approval-parity-live-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; The real `inheritenv' package is not bootstrapped under `-Q --batch'
;; (straight.el is absent), but `claude-term--exec' unconditionally wraps
;; its `ghostel-exec' call in `(inheritenv ...)' -- same shim, and same
;; reason, as claude-term-live-test.el's.
(unless (fboundp 'inheritenv)
  (defmacro inheritenv (&rest body)
    `(progn ,@body)))

(defvar claude-term-approval-parity-live-test--repo-root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "This checkout's root, resolved at LOAD time.
`load-file-name' is nil once a test body actually runs, so the value has
to be captured here rather than recomputed inside a `ert-deftest'.")

(defvar claude-term-approval-parity-live-test--ghostel-source
  (expand-file-name
   "../straight/repos/ghostel/lisp/ghostel.el"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the real ghostel.el, when this checkout has fetched it.")

(defconst claude-term-approval-parity-live-test--ghostel-native-fns
  '(ghostel--new ghostel--set-size ghostel--set-palette
    ghostel--set-default-colors ghostel--set-bold-config
    ghostel--redraw ghostel--write-vt ghostel--write-pty
    ghostel--mode-enabled ghostel--alt-screen-p
    ghostel--module-version ghostel--encode-key)
  "Native-module leaf functions stubbed while capturing a spawn.
Only leaves: each is implemented in ghostel-module.dylib, which a batch
harness cannot load.  Every Lisp step between `ghostel-exec' and the
exec primitive runs for real -- which is the point of the capture.")

(defconst claude-term-approval-parity-live-test--permission-flags
  '("--settings" "--permission-mode" "--permission-prompt-tool"
    "--allowedTools" "--allowed-tools" "--disallowedTools"
    "--disallowed-tools" "--dangerously-skip-permissions"
    "--allow-dangerously-skip-permissions" "--add-dir" "--tools"
    "-p" "--print" "--output-format" "--sandbox" "--bare")
  "Claude CLI flags that can move which tools raise an approval prompt.
`-p'/`--print' and `--output-format' are listed because the retired
claude-repl passed exactly those, alongside the `--settings' temp file
that carried its PreToolUse hook.")

(defconst claude-term-approval-parity-live-test--permission-env-rx
  "\\`\\(CLAUDE\\|ANTHROPIC\\)"
  "Regexp matching environment variable names the CLI reads for auth/permissions.")

;; ---------------------------------------------------------------------------
;; Capturing the real spawn context
;; ---------------------------------------------------------------------------

(cl-defstruct (claude-term-approval-parity-live-test--spawn
               (:constructor claude-term-approval-parity-live-test--spawn-make))
  program args cwd env)

(defun claude-term-approval-parity-live-test--prereqs-p ()
  "Non-nil when both the real ghostel source and a `claude' binary are present."
  (and (file-exists-p claude-term-approval-parity-live-test--ghostel-source)
       (executable-find "claude")))

(defun claude-term-approval-parity-live-test--capture-spawn (root args)
  "Return the spawn context the real chain would exec for ROOT and ARGS.

Drives the REAL `claude-term--exec', the REAL `ghostel-exec' (including
`ghostel--init-buffer' and `ghostel-mode') and the REAL
`ghostel--spawn-pty' -- so `ghostel-environment', the terminal-env
prelude, the logical-PWD entry and `ghostel-pre-spawn-hook' (which is
where claude-term-registry.el injects `EDMACS_AGENT_INSTANCE') all run
unmodified.  Only `ghostel--spawn-process', the pure-Lisp dispatcher
that would hand the argv to the native or Emacs exec backend, is
replaced -- with a recorder that copies what it was given."
  (add-to-list 'load-path
               (file-name-directory
                claude-term-approval-parity-live-test--ghostel-source))
  (require 'ghostel)
  (let* ((captured nil)
         (stubbed nil)
         (instance "parity")
         (name (claude-term-buffer-name root instance))
         (buf (get-buffer-create name)))
    (unwind-protect
        (progn
          (dolist (fn claude-term-approval-parity-live-test--ghostel-native-fns)
            (unless (fboundp fn)
              (push fn stubbed)
              (fset fn (lambda (&rest _) nil))))
          (cl-letf (((symbol-function 'claude-term--ensure-ghostel)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ghostel--load-module)
                     (lambda (&rest _) t))
                    ((symbol-function 'ghostel--spawn-process)
                     (lambda (program program-args &rest _)
                       ;; `process-environment' is dynamically bound by
                       ;; the real `ghostel--spawn-pty' at this point,
                       ;; and `default-directory' is the spawn buffer's.
                       (setq captured
                             (claude-term-approval-parity-live-test--spawn-make
                              :program program
                              :args (copy-sequence program-args)
                              :cwd default-directory
                              :env (copy-sequence process-environment)))
                       (let ((proc (start-process
                                    "claude-term-parity" (current-buffer)
                                    "sleep" "3600")))
                         (set-process-query-on-exit-flag proc nil)
                         (setq-local ghostel--process proc)
                         proc))))
            (claude-term--exec buf root instance args))
          captured)
      (dolist (fn stubbed) (fmakunbound fn))
      (when-let* ((proc (get-buffer-process buf))) (delete-process proc))
      (kill-buffer buf))))

(defun claude-term-approval-parity-live-test--json-tail (string)
  "Return STRING from its first line beginning with `{' onward, trimmed.
A login shell echoes whatever its rc files print before the command's
own output; the CLI's JSON starts at the first such line."
  (if (string-match "^{" string)
      (string-trim (substring string (match-beginning 0)))
    (string-trim string)))

(defun claude-term-approval-parity-live-test--auto-mode-config-via-login-shell (root)
  "Run `claude auto-mode config' in a login shell at ROOT; return its JSON.
This is the terminal side of the comparison: the same shell, rc files and
environment resolution a Ghostty window would give the CLI."
  (let ((default-directory root)
        (shell (or (getenv "SHELL") "/bin/sh")))
    (with-temp-buffer
      (let ((status (call-process shell nil t nil "-lc" "claude auto-mode config")))
        (unless (eq status 0)
          (error "Login-shell `claude auto-mode config' exited %S" status)))
      (claude-term-approval-parity-live-test--json-tail (buffer-string)))))

(defun claude-term-approval-parity-live-test--auto-mode-config-via-spawn (spawn)
  "Run `claude auto-mode config' in SPAWN's argv, cwd and env; return its JSON.
SPAWN is a capture from
`claude-term-approval-parity-live-test--capture-spawn'.  The subcommand
is APPENDED to the captured argv, so whatever `claude-term' would have
passed the CLI is passed here too."
  (let ((default-directory (claude-term-approval-parity-live-test--spawn-cwd spawn))
        (process-environment (claude-term-approval-parity-live-test--spawn-env spawn)))
    (with-temp-buffer
      (let ((status (apply #'call-process
                           (claude-term-approval-parity-live-test--spawn-program spawn)
                           nil t nil
                           (append (claude-term-approval-parity-live-test--spawn-args spawn)
                                   '("auto-mode" "config")))))
        (unless (eq status 0)
          (error "Ghostel-env `claude auto-mode config' exited %S" status)))
      (claude-term-approval-parity-live-test--json-tail (buffer-string)))))

(defun claude-term-approval-parity-live-test--permission-env (env)
  "Return the sorted `CLAUDE*'/`ANTHROPIC*' entries of ENV, a `NAME=VALUE' list."
  (sort (seq-filter
         (lambda (entry)
           (string-match-p claude-term-approval-parity-live-test--permission-env-rx
                           entry))
         env)
        #'string<))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(ert-deftest claude-term-approval-parity-live-test-spawn-argv-carries-no-permission-flag ()
  "`claude-term' resolves an argv with no permission-affecting flag.

The retired claude-repl spawned `claude -p --output-format stream-json
--settings <tmpfile>', and that temp file was the ONLY PreToolUse hook
this configuration ever registered.  Nothing may reintroduce a flag of
that class through `claude-term-extra-args' defaults or through
`claude-term--spawn-args' itself, or an Emacs-hosted session stops
raising the same prompts as a terminal one."
  ;; The shipped default must be empty: a non-nil default would ride on
  ;; every spawn without any call site naming it.
  (should (null (default-value 'claude-term-extra-args)))
  (should (null (claude-term--spawn-args nil)))
  ;; A caller's own EXTRA-ARGS pass through verbatim -- assert the
  ;; resolved argv, not the source, so the check survives a refactor.
  (let ((argv (claude-term--spawn-args '("--resume"))))
    (should (equal argv '("--resume")))
    (dolist (flag claude-term-approval-parity-live-test--permission-flags)
      (should-not (member flag argv))
      ;; Also catch the `--flag=value' spelling, which `member' misses.
      (should-not (seq-find (lambda (arg)
                              (string-prefix-p (concat flag "=") arg))
                            argv)))))

(ert-deftest claude-term-approval-parity-live-test-effective-auto-mode-policy-is-identical ()
  "The effective auto-mode policy is byte-identical in a terminal and in Emacs.

This is the behavioural comparison behind phase 6's approval-parity
clause.  `claude auto-mode config' prints the resolved allow /
soft_deny / hard_deny rule set -- the thing that decides which tools
raise a prompt under `defaultMode: auto'.  Running it through a login
shell and through the real `claude-term' spawn context and getting the
same bytes means the two sessions cannot prompt on different tool sets,
because they are consulting the same policy."
  (unless (claude-term-approval-parity-live-test--prereqs-p)
    (ert-skip "needs a `claude' on exec-path and a fetched straight/repos/ghostel"))
  (let* ((root claude-term-approval-parity-live-test--repo-root)
         (spawn (claude-term-approval-parity-live-test--capture-spawn
                 root (claude-term--spawn-args nil))))
    (should spawn)
    ;; Guard the guard: if the capture ever stops going through the real
    ;; ghostel spawn path, the comparison below would pass vacuously
    ;; against two identical plain environments.
    (should (equal (claude-term-approval-parity-live-test--spawn-program spawn) "claude"))
    (should (member "INSIDE_EMACS=ghostel"
                    (claude-term-approval-parity-live-test--spawn-env spawn)))
    (let ((terminal (claude-term-approval-parity-live-test--auto-mode-config-via-login-shell root))
          (emacs-side (claude-term-approval-parity-live-test--auto-mode-config-via-spawn spawn)))
      (should (string-prefix-p "{" terminal))
      (should (string-prefix-p "{" emacs-side))
      (should (equal terminal emacs-side)))))

(ert-deftest claude-term-approval-parity-live-test-spawn-env-adds-no-permission-variable ()
  "The ghostel spawn machinery contributes no CLAUDE*/ANTHROPIC* variable.

`ghostel--spawn-pty' prepends `ghostel-environment', an INSIDE_EMACS
marker and a terminal-env block, and `ghostel-pre-spawn-hook' lets
claude-term-registry.el inject `EDMACS_AGENT_INSTANCE'.  None of that
may add, drop or rewrite a variable the CLI reads for auth or
permissions -- if any did, the Emacs child could resolve a different
policy than a terminal child even with identical settings files.

The control is this Emacs process's OWN ambient environment, not a
login shell's: the spawn env is derived from it, so the delta between
the two is exactly what the spawn machinery contributed and nothing
else.  (A login shell is the wrong control here -- it re-resolves the
whole environment from rc files, so unrelated inherited variables show
up as spurious differences.)"
  (unless (claude-term-approval-parity-live-test--prereqs-p)
    (ert-skip "needs a `claude' on exec-path and a fetched straight/repos/ghostel"))
  (let* ((root claude-term-approval-parity-live-test--repo-root)
         (ambient (copy-sequence process-environment))
         (spawn (claude-term-approval-parity-live-test--capture-spawn
                 root (claude-term--spawn-args nil)))
         (spawn-env (claude-term-approval-parity-live-test--spawn-env spawn)))
    ;; Guard the guard: the spawn env must really be ghostel's, or the
    ;; assertions below compare the ambient environment with itself.
    (should (member "INSIDE_EMACS=ghostel" spawn-env))
    (should (seq-find (lambda (e) (string-prefix-p "EDMACS_AGENT_INSTANCE=" e))
                      spawn-env))
    ;; Everything ghostel contributed, by name.
    (let* ((names (lambda (env)
                    (delete-dups
                     (mapcar (lambda (e) (car (split-string e "=" nil))) env))))
           (added (seq-difference (funcall names spawn-env)
                                  (funcall names ambient)))
           (dropped (seq-difference (funcall names ambient)
                                    (funcall names spawn-env))))
      (should-not (seq-filter
                   (lambda (n)
                     (string-match-p
                      claude-term-approval-parity-live-test--permission-env-rx n))
                   added))
      (should-not (seq-filter
                   (lambda (n)
                     (string-match-p
                      claude-term-approval-parity-live-test--permission-env-rx n))
                   dropped)))
    ;; And no surviving permission variable had its value rewritten.
    ;; `getenv'-style first-wins lookup is what the child sees, so compare
    ;; resolved values rather than raw list membership.
    (dolist (entry (claude-term-approval-parity-live-test--permission-env ambient))
      (let ((name (car (split-string entry "=" nil))))
        (should (equal (getenv-internal name ambient)
                       (getenv-internal name spawn-env)))))))

(provide 'claude-term-approval-parity-live-test)
;;; claude-term-approval-parity-live-test.el ends here
