;;; claude-lib-drive-live-test.el --- Real-subprocess tests for claude-lib-drive.el -*- lexical-binding: t -*-

;;; Commentary:
;; The tier `emacs -Q --batch' cannot reach.  Under `--batch',
;; `noninteractive' is t and `read-from-minibuffer' reads from STDIN, so
;; the whole minibuffer story -- the wedge, the pre-fed real
;; `completing-read', the unanswerable-prompt abort -- is unfalsifiable
;; in process.  Everything here therefore drives a REAL throwaway
;; `emacs -Q --daemon', or a real `scripts/claude-scratch.sh'.
;;
;;   scripts/run-ert-suite.sh 300 \
;;     emacs -Q --batch -l ert \
;;           -l modules/claude-lib.el \
;;           -l modules/claude-lib-drive.el \
;;           -l modules/claude-lib-drive-live-test.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; Two tests can skip, both legitimately: the cross-frame restore needs
;; `python3' to allocate a pty for a second tty frame (CLAUDE.md's
;; Testing section), and the `-Q'-gate-versus-loaded-host test needs a
;; populated `straight/build', which a worktree does not have -- it
;; reaches for the sibling main checkout's, exactly as
;; `modules/claude-lib-view-test.el' does.
;;
;; TEARDOWN IS NOT `emacsclient -e (kill-emacs)' HERE.  The negative
;; control deliberately leaves a daemon wedged, and a wedged daemon never
;; answers that either -- verified: it needs a real signal.  Every daemon
;; is started, its pid recorded through the channel while it is still
;; healthy, and killed with `signal-process' in `unwind-protect'; a
;; leaked wedged daemon would survive the suite and eat a socket.
;;
;; `emacsclient' also needs TERM=dumb in this agent environment or it
;; fails with "Unknown terminal type" (live-daemon-diagnosis project
;; memory), set explicitly on every child rather than trusted from the
;; ambient shell.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'claude-lib-drive)

;; See this repo's CLAUDE.md; the canonical comment is in
;; `modules/sessions-test.el'. Not load-bearing today -- nothing here
;; `cl-letf's a subr -- but it travels with the file so a later addition
;; cannot reintroduce the ~28s trampoline cost silently.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defvar claude-lib-drive-live-test--repo-root
  (file-name-as-directory (expand-file-name default-directory))
  "Repository root, captured at load time, as every sibling suite does.")

;; ============================================================================
;; Deadline-bounded emacsclient
;; ============================================================================

(defun claude-lib-drive-live-test--client (server form seconds)
  "Run \"emacsclient -s SERVER -e FORM\", giving up after SECONDS.
Returns (EXIT . OUTPUT); EXIT is nil when the call was still running at
the deadline and was killed.  `call-process' cannot be interrupted on a
deadline and the whole point of the wedge test is that the call never
returns, so this is `make-process' plus `delete-process' -- the shape
`claude-lib--verify-file-loads' already uses."
  (with-temp-buffer
    (let* ((process-environment (cons "TERM=dumb" process-environment))
           (proc (make-process :name "cldlt-emacsclient"
                               :buffer (current-buffer)
                               :noquery t
                               :sentinel #'ignore
                               :connection-type 'pipe
                               :command (list "emacsclient" "-s" server "-e" form)))
           (deadline (+ (float-time) seconds)))
      (while (and (process-live-p proc) (< (float-time) deadline))
        (accept-process-output proc 0.05))
      (if (process-live-p proc)
          (progn (delete-process proc) (cons nil (buffer-string)))
        (while (accept-process-output proc 0.05))
        (cons (process-exit-status proc) (buffer-string))))))

(defun claude-lib-drive-live-test--start-daemon ()
  "Start a throwaway daemon with claude-lib-drive loaded; return (NAME . PID).
PID is read back through the channel while the daemon is still healthy,
because a daemon this suite has since wedged cannot be asked for it --
or killed through `emacsclient' at all."
  (let ((server (make-temp-name "edmacs-cldlt-"))
        (process-environment (cons "TERM=dumb" process-environment)))
    (should (zerop (call-process "emacs" nil nil nil "-Q" (concat "--daemon=" server))))
    (dolist (module '("claude-lib" "claude-lib-drive"))
      (let ((result (claude-lib-drive-live-test--client
                     server
                     (format "(load %S nil t)"
                             (expand-file-name (format "modules/%s.el" module)
                                               claude-lib-drive-live-test--repo-root))
                     30)))
        (unless (eq (car result) 0)
          (error "claude-lib-drive-live-test: could not load %s into %s: %s"
                 module server (cdr result)))))
    (let ((pid (claude-lib-drive-live-test--client server "(emacs-pid)" 15)))
      (cons server (string-to-number (string-trim (cdr pid)))))))

(defun claude-lib-drive-live-test--kill-daemon (daemon)
  "Kill DAEMON, a (NAME . PID) cons, by signal -- never through the channel."
  (ignore-errors (signal-process (cdr daemon) 'KILL))
  (let ((socket (expand-file-name (car daemon)
                                  (expand-file-name (format "emacs%d" (user-uid))
                                                    temporary-file-directory))))
    (when (file-exists-p socket) (ignore-errors (delete-file socket)))))

(defmacro claude-lib-drive-live-test--with-daemon (var &rest body)
  "Bind VAR to a fresh throwaway daemon's server name for BODY, then kill it."
  (declare (indent 1))
  (let ((daemon (make-symbol "daemon")))
    `(let ((,daemon (claude-lib-drive-live-test--start-daemon)))
       (unwind-protect
           (let ((,var (car ,daemon))) ,@body)
         (claude-lib-drive-live-test--kill-daemon ,daemon)))))

;; ============================================================================
;; AC1 -- the wedge, and the mitigation that avoids it
;; ============================================================================

(ert-deftest claude-lib-drive-live-test-blocking-read-wedges-the-daemon ()
  "THE NEGATIVE CONTROL, and the reason this whole module exists.
A bare `completing-read' through the eval channel does not merely hang
its own request: a SECOND, INDEPENDENT client gets nothing either.  The
wedge is daemon-wide, so an interactive form evaluated naively takes down
the Emacs the operator works in all day, for every later call."
  (claude-lib-drive-live-test--with-daemon server
    (let ((blocked (claude-lib-drive-live-test--client
                    server "(completing-read \"pick: \" (list \"alpha\" \"beta\"))" 6)))
      (should-not (car blocked)))
    (let ((second (claude-lib-drive-live-test--client server "(list :alive t)" 6)))
      (should-not (car second)))))

(ert-deftest claude-lib-drive-live-test-prefed-input-leaves-the-daemon-alive ()
  "Pre-fed `:keys' drives the REAL `completing-read' and returns promptly.
The same form as the negative control above, through `claude-lib-drive';
the daemon then still answers an independent client, which is the whole
difference."
  (claude-lib-drive-live-test--with-daemon server
    (let ((driven (claude-lib-drive-live-test--client
                   server
                   (concat "(plist-get (claude-lib-drive"
                           " (lambda () (completing-read \"pick: \" (list \"alpha\" \"beta\")))"
                           " :keys \"b e t a RET\") :value)")
                   20)))
      (should (eq (car driven) 0))
      (should (string-match-p "\"beta\"" (cdr driven))))
    (let ((second (claude-lib-drive-live-test--client server "(list :alive t)" 15)))
      (should (eq (car second) 0))
      (should (string-match-p ":alive t" (cdr second))))))

(ert-deftest claude-lib-drive-live-test-unanswerable-prompt-aborts-not-blocks ()
  "The PRIMARY anti-wedge layer, tested where a minibuffer really exists.
A prompt opened with nothing left to answer it aborts and is reported as
data, naming the prompt -- rather than blocking, and rather than letting
the `quit' propagate out as a confusing non-answer."
  (claude-lib-drive-live-test--with-daemon server
    (let ((driven (claude-lib-drive-live-test--client
                   server
                   (concat "(let ((r (claude-lib-drive"
                           " (lambda () (completing-read \"unanswerable: \" (list \"a\"))))))"
                           " (format \"%S %S\" (plist-get r :error) (plist-get r :prompts)))")
                   20)))
      (should (eq (car driven) 0))
      (should (string-match-p "unanswered-prompt" (cdr driven)))
      (should (string-match-p "unanswerable: " (cdr driven))))
    (let ((second (claude-lib-drive-live-test--client server "(list :alive t)" 15)))
      (should (eq (car second) 0)))))

(ert-deftest claude-lib-drive-live-test-choice-stub-also-leaves-the-daemon-alive ()
  "The stub path is bounded too -- it never reaches a minibuffer at all."
  (claude-lib-drive-live-test--with-daemon server
    (let ((driven (claude-lib-drive-live-test--client
                   server
                   (concat "(plist-get (claude-lib-drive"
                           " (lambda () (completing-read \"pick: \" (list \"alpha\" \"beta\")))"
                           " :choice \"alpha\") :value)")
                   20)))
      (should (eq (car driven) 0))
      (should (string-match-p "\"alpha\"" (cdr driven))))))

;; ============================================================================
;; AC1b -- the restore covers the TARGET window's frame, not the caller's
;; ============================================================================

(defun claude-lib-drive-live-test--pty-batch (form)
  "Run FORM in a fresh \"emacs -Q --batch\" attached to a real pty.
Returns (EXIT-CODE OUTPUT).  A second real frame needs a controlling
terminal, which this suite's own process has none of.  `pty.spawn' does
not propagate the child's exit status, so callers assert on tokens the
child prints, not on EXIT-CODE."
  (with-temp-buffer
    (let ((exit (call-process "python3" nil t nil "-c"
                              "import pty,sys; pty.spawn(sys.argv[1:])"
                              "emacs" "-Q" "--batch"
                              "--eval" (prin1-to-string form))))
      (list exit (buffer-string)))))

(ert-deftest claude-lib-drive-live-test-restores-configuration-of-the-target-windows-frame ()
  "A command acting on ANOTHER frame's window has THAT frame put back.
`save-window-excursion' covers only the selected frame, so it would
restore an untouched frame and leave the real layout change standing on
the frame under test -- in a config that is multi-frame by design."
  (unless (executable-find "python3")
    (ert-skip "needs python3 to attach a pty for a second real frame"))
  (let* ((process-environment (cons "TERM=dumb" process-environment))
         (root claude-lib-drive-live-test--repo-root))
    (cl-destructuring-bind (_exit output)
        (claude-lib-drive-live-test--pty-batch
         `(progn
            (load ,(expand-file-name "modules/claude-lib.el" root) nil t)
            (load ,(expand-file-name "modules/claude-lib-drive.el" root) nil t)
            (defun claude-lib-drive-live-test--switch ()
              (interactive)
              (switch-to-buffer (get-buffer-create "*cldlt-target*")))
            (condition-case err
                (let* ((home-frame (selected-frame))
                       (other (make-frame '((window-system . nil)
                                            (tty . "/dev/tty")
                                            (tty-type . "xterm"))))
                       (window (frame-selected-window other))
                       (resident (get-buffer-create "*cldlt-resident*")))
                  ;; `make-frame' selects the new tty frame, and the bug only
                  ;; shows with WINDOW on a frame the caller is not on.
                  (select-frame home-frame)
                  (set-window-buffer window resident)
                  (let ((result (claude-lib-drive-command
                                 #'claude-lib-drive-live-test--switch :window window)))
                    (princ (format "SEEN=%s RESTORED=%s HOME=%s\n"
                                   (plist-get result :window-buffer)
                                   (buffer-name (window-buffer window))
                                   (eq (selected-frame) home-frame)))))
              (error (princ (format "PROBE-ERROR=%S\n" err))))
            (kill-emacs 0)))
      (should (string-match-p "SEEN=\\*cldlt-target\\*" output))
      (should (string-match-p "RESTORED=\\*cldlt-resident\\*" output))
      (should (string-match-p "HOME=t" output)))))

;; ============================================================================
;; AC4 -- the `-Q' gate fails on what a loaded host image would accept
;; ============================================================================

(defun claude-lib-drive-live-test--straight-build-root ()
  "Return a populated `straight/build', this checkout's or the main one's.
Same worktree-vs-main fallback as `modules/claude-lib-view-test.el': a
worktree lives at `<parent>/edmacs__worktrees/<name>', sibling to the
main `<parent>/edmacs' checkout, and only the main one has packages."
  (or (let ((here (expand-file-name "straight/build" claude-lib-drive-live-test--repo-root)))
        (and (file-directory-p here) here))
      (let* ((root (directory-file-name
                    (expand-file-name claude-lib-drive-live-test--repo-root)))
             (worktrees-dir (directory-file-name (file-name-directory root))))
        (when (string-suffix-p "__worktrees" worktrees-dir)
          (let* ((projects-dir (file-name-directory worktrees-dir))
                 (repo-name (string-remove-suffix
                             "__worktrees" (file-name-nondirectory worktrees-dir)))
                 (main-build (expand-file-name
                              (concat repo-name "/straight/build") projects-dir)))
            (and (file-directory-p main-build) main-build))))))

(ert-deftest claude-lib-drive-live-test-check-q-fails-what-the-host-image-accepts ()
  "The gate must fail a hard `require' the HOST can satisfy.
This is the exact miss the gate exists to prevent: the daemon has evil
and friends loaded, so soft-dependency code with a hard `require' leaking
in works there and only ever fails later, in the consumer repo's own
`emacs -Q --batch' harness."
  (let* ((build (claude-lib-drive-live-test--straight-build-root))
         (evil-dir (and build (expand-file-name "evil" build))))
    (unless (and evil-dir (file-directory-p evil-dir))
      (ert-skip "needs a populated straight/build with evil in it"))
    (let ((file (make-temp-file "cldlt-hard" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert ";;; -*- lexical-binding: t -*-\n(require 'evil)\n(defun cldlt-hard () 1)\n"))
            ;; The host really can load it -- so a "works in the daemon"
            ;; check would pass here, which is the point.
            (let ((load-path (cons evil-dir load-path)))
              (should (file-exists-p (expand-file-name "evil.el" evil-dir))))
            (let ((report (claude-lib-check-q file)))
              (should-not (plist-get report :ok))
              (should-not (plist-get report :loaded))
              (should (memq 'evil (plist-get report :missing-features))))
            ;; Handed the directory explicitly, the same file passes -- so the
            ;; failure above is the missing `-L', not a broken fixture.
            (should (plist-get (claude-lib-check-q file :load-path (list evil-dir)) :ok)))
        (ignore-errors (delete-file file))))))

;; ============================================================================
;; AC2 -- scripts/claude-scratch.sh
;; ============================================================================

(defun claude-lib-drive-live-test--scratch (name args &optional root packages)
  "Run scripts/claude-scratch.sh with ARGS under server NAME.
ROOT defaults to this checkout; PACKAGES sets EDMACS_SCRATCH_PACKAGES.
Returns (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((process-environment
           (append (list "TERM=dumb"
                         (concat "EDMACS_SCRATCH_NAME=" name))
                   (when packages (list (concat "EDMACS_SCRATCH_PACKAGES=" packages)))
                   process-environment)))
      (cons (apply #'call-process
                   (expand-file-name "scripts/claude-scratch.sh"
                                     (or root claude-lib-drive-live-test--repo-root))
                   nil t nil args)
            (buffer-string)))))

(ert-deftest claude-lib-drive-live-test-scratch-refuses-the-live-server-name ()
  "The one rule the script exists to enforce, on every subcommand.
`restart' matters most: a mistyped EDMACS_SCRATCH_NAME there is how the
operator's daemon actually dies."
  (dolist (subcommand '("start" "restart" "stop" "status" "eval"))
    (cl-destructuring-bind (exit . output)
        (claude-lib-drive-live-test--scratch "server" (list subcommand "(quote x)"))
      (should (eq exit 2))
      (should (string-match-p "server" output))))
  ;; ... and it did not touch the operator's daemon on the way out.
  (should-not (string-match-p
               "kill-emacs"
               (cdr (claude-lib-drive-live-test--scratch "server" '("stop"))))))

(ert-deftest claude-lib-drive-live-test-scratch-refuses-a-name-with-a-slash ()
  (cl-destructuring-bind (exit . output)
      (claude-lib-drive-live-test--scratch "../server" '("start"))
    (should (eq exit 2))
    (should (string-match-p "/" output))))

(ert-deftest claude-lib-drive-live-test-scratch-start-status-restart-stop ()
  "The lifecycle, on a pure `-Q' daemon so no package tree is required.
`restart' must yield FRESH state -- that is what makes it the answer to
every limit of `claude-lib-reload' rather than a cosmetic reset."
  (let ((name (make-temp-name "edmacs-cldlt-scratch-")))
    (unwind-protect
        (progn
          (cl-destructuring-bind (exit . output)
              (claude-lib-drive-live-test--scratch name '("start") nil "none")
            (should (eq exit 0))
            (should (string-match-p (regexp-quote name) output))
            (should-not (string-match-p "daemonp=\\\\\"server\\\\\"" output)))
          (should (eq 0 (car (claude-lib-drive-live-test--scratch name '("status") nil "none"))))
          (cl-destructuring-bind (exit . output)
              (claude-lib-drive-live-test--scratch
               name '("eval" "(format \"%S %S\" (daemonp) (featurep 'claude-lib-drive))")
               nil "none")
            (should (eq exit 0))
            (should (string-match-p (regexp-quote name) output))
            (should (string-match-p " t" output)))
          (claude-lib-drive-live-test--scratch
           name '("eval" "(setq cldlt-scratch-marker 42)") nil "none")
          (should (string-match-p
                   "42" (cdr (claude-lib-drive-live-test--scratch
                              name '("eval" "cldlt-scratch-marker") nil "none"))))
          (should (eq 0 (car (claude-lib-drive-live-test--scratch name '("restart") nil "none"))))
          (should (string-match-p
                   "nil" (cdr (claude-lib-drive-live-test--scratch
                               name '("eval" "(boundp 'cldlt-scratch-marker)") nil "none"))))
          (should (eq 0 (car (claude-lib-drive-live-test--scratch name '("stop") nil "none"))))
          (should-not (eq 0 (car (claude-lib-drive-live-test--scratch
                                  name '("status") nil "none")))))
      (claude-lib-drive-live-test--scratch name '("stop") nil "none"))))

(ert-deftest claude-lib-drive-live-test-scratch-never-bootstraps-straight ()
  "A root with no built package tree is a refusal, never a bootstrap.
`--init-directory' at a worktree is how a second straight tree gets
created and the main checkout's `straight/build' poisoned; this script
must not have an equivalent path."
  (let ((root (make-temp-file "cldlt-scratch-root" t))
        (name (make-temp-name "edmacs-cldlt-nopkg-")))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "scripts" root))
          (make-directory (expand-file-name "modules" root))
          (copy-file (expand-file-name "scripts/claude-scratch.sh"
                                       claude-lib-drive-live-test--repo-root)
                     (expand-file-name "scripts/claude-scratch.sh" root))
          (set-file-modes (expand-file-name "scripts/claude-scratch.sh" root) #o755)
          (cl-destructuring-bind (exit . output)
              (claude-lib-drive-live-test--scratch name '("start") root)
            (should (eq exit 2))
            (should (string-match-p "straight/build" output)))
          (should-not (file-exists-p (expand-file-name "straight" root))))
      (claude-lib-drive-live-test--scratch name '("stop") nil "none")
      (ignore-errors (delete-directory root t)))))

(provide 'claude-lib-drive-live-test)
;;; claude-lib-drive-live-test.el ends here
