;;; claude-term-registry-live-test.el --- Real-subprocess tests for claude-term-registry.el -*- lexical-binding: t -*-

;;; Commentary:
;; Follows claude-term-live-test.el's own fake-ghostel-exec/sentinel stub
;; pattern (see that file's Commentary for the full "why a stub, not the
;; real ghostel package" rationale) to drive `claude-term--exec' against
;; real subprocesses while exercising:
;;  - the real registry put/touch/remove call sites wired into
;;    claude-term.el's spawn/kill/exit lifecycle (AC3),
;;  - `claude-term-rename''s registry migration against a sibling live
;;    session under the same root (AC5), and
;;  - the real `ghostel-pre-spawn-hook' -> `claude-term-registry--set-instance-env'
;;    wiring, by running the REAL hook (not a re-implementation of it)
;;    with `process-environment' dynamically bound, then execing `env'
;;    so each session's own resolved environment is directly observable
;;    in its output (AC6).
;;
;; This file duplicates a small amount of stub plumbing from
;; claude-term-live-test.el (the sentinel re-implementation, the
;; wait-until poll helper) rather than requiring that file, so it has no
;; load-order dependency on it -- each of these two live-test files
;; loads and runs standalone per its own header recipe.
;;
;; Run:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/claude-term.el \
;;         -l modules/claude-term-registry.el \
;;         -l modules/claude-term-registry-live-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; The real `inheritenv' package is not bootstrapped under `-Q --batch'
;; (straight.el is absent), but `claude-term--exec' unconditionally
;; wraps its `ghostel-exec' call in `(inheritenv ...)' -- see
;; claude-term-live-test.el's own identical shim and its commentary.
(unless (fboundp 'inheritenv)
  (defmacro inheritenv (&rest body)
    `(progn ,@body)))

(defvar claude-term-registry-live-test--spawn-log nil
  "List of (BUFFER PROGRAM . ARGS) recorded by the `ghostel-exec' stubs below.")

(defun claude-term-registry-live-test--fake-ghostel-sentinel (process event)
  "Re-implementation of ghostel.el's `ghostel--sentinel' tail for PROCESS/EVENT.
Identical in shape to claude-term-live-test--fake-ghostel-sentinel;
duplicated here rather than shared so this file has no load-order
dependency on claude-term-live-test.el."
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (run-hook-with-args 'ghostel-exit-functions buf event))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (unless ghostel-kill-buffer-on-exit
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "\n[Process exited]\n"))))))))

(defun claude-term-registry-live-test--fake-exec (buffer program args)
  "Stub for `ghostel-exec': attach a real dummy \"sleep\" process to BUFFER.
Same shape as claude-term-live-test--fake-exec."
  (push (list buffer program args) claude-term-registry-live-test--spawn-log)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)) (erase-buffer))
    (let ((proc (start-process "claude-term-registry-live-test" buffer "sleep" "3600")))
      (set-process-sentinel proc #'claude-term-registry-live-test--fake-ghostel-sentinel)
      (set-process-query-on-exit-flag proc nil)
      (setq-local ghostel--process proc)
      proc)))

(defun claude-term-registry-live-test--fake-exec-env (buffer program args)
  "Stub for `ghostel-exec' that runs the REAL `ghostel-pre-spawn-hook' with
`process-environment' dynamically bound -- mirroring ghostel.el's own
spawn path (lisp/ghostel.el:4020-4052, already read and cited in
claude-term.el's own commentary) -- then execs `env' instead of PROGRAM,
so this session's own resolved environment is directly observable in
its output. Ignores PROGRAM/ARGS beyond logging them.

Runs `env' through a shell that sleeps afterward (mirroring
claude-term-live-test--fake-exec-transcript's own \"print then sleep\"
shape) rather than letting the bare `env' process exit immediately --
otherwise `claude-term--on-exit's real plain-exit cleanup path (this
stub attaches the same real sentinel every other stub here does) would
kill the buffer out from under this test before its output could be
inspected."
  (push (list buffer program args) claude-term-registry-live-test--spawn-log)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)) (erase-buffer))
    (let* ((process-environment (copy-sequence process-environment))
           (proc (progn
                   (run-hooks 'ghostel-pre-spawn-hook)
                   (start-process "claude-term-registry-live-test-env" buffer
                                   "sh" "-c" "env; sleep 3600"))))
      (set-process-sentinel proc #'claude-term-registry-live-test--fake-ghostel-sentinel)
      (set-process-query-on-exit-flag proc nil)
      (setq-local ghostel--process proc)
      proc)))

(defun claude-term-registry-live-test--wait-until (pred &optional timeout)
  "Pump the process event loop until PRED is non-nil or TIMEOUT elapses.
TIMEOUT defaults to 2 seconds. Returns PRED's final value."
  (let ((deadline (+ (float-time) (or timeout 2))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defmacro claude-term-registry-live-test--with-buffer (var name &rest body)
  "Bind VAR to a fresh buffer named NAME for BODY, killing it (and any
live process still attached) afterward regardless of how BODY exits."
  (declare (indent 2))
  `(let ((,var (generate-new-buffer ,name)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p ,var)
         (ignore-errors
           (let ((proc (buffer-local-value 'ghostel--process ,var)))
             (when (process-live-p proc) (kill-process proc))))
         (kill-buffer ,var)))))

;; `claude-term.el' declares `ghostel--process' via a bare `(defvar
;; ghostel--process)' for byte-compiler purposes only; production
;; installs its buffer-local nil default by `require'ing the real
;; `ghostel' package inside `claude-term--ensure-ghostel', which is
;; stubbed to a no-op below. Install the same nil default here --
;; mirrors claude-term-live-test.el's identical setup, needed before
;; any `claude-term--exec' call in a fresh buffer.
(unless (default-boundp 'ghostel--process)
  (setq-default ghostel--process nil))

;; ============================================================================
;; AC3 -- three worktrees hold independent registry sessions; killing one
;; leaves the other two, and their registry entries, untouched
;; ============================================================================

(ert-deftest claude-term-registry-live-test-kill-one-leaves-others-untouched ()
  (let ((claude-term-registry-live-test--spawn-log nil)
        (claude-term-registry--table (make-hash-table :test #'equal))
        (root1 (file-name-as-directory (make-temp-file "claude-term-registry-live-1-" t)))
        (root2 (file-name-as-directory (make-temp-file "claude-term-registry-live-2-" t)))
        (root3 (file-name-as-directory (make-temp-file "claude-term-registry-live-3-" t))))
    (cl-letf (((symbol-function 'claude-term--ensure-ghostel) #'ignore)
              ((symbol-function 'ghostel-exec) #'claude-term-registry-live-test--fake-exec))
      (unwind-protect
          (claude-term-registry-live-test--with-buffer
              buf1 "*claude-term-registry-live-test:root1*"
            (claude-term-registry-live-test--with-buffer
                buf2 "*claude-term-registry-live-test:root2*"
              (claude-term-registry-live-test--with-buffer
                  buf3 "*claude-term-registry-live-test:root3*"
                (claude-term--exec buf1 root1 nil nil)
                (claude-term--exec buf2 root2 nil nil)
                (claude-term--exec buf3 root3 nil nil)
                (should (= (length (claude-term-registry-sessions)) 3))
                (claude-term-kill buf1)
                (should (claude-term-registry-live-test--wait-until
                         (lambda () (not (buffer-live-p buf1)))))
                ;; buf1's registry entry is gone; the other two remain,
                ;; live, with their own processes still running.
                (should-not (claude-term-registry-get root1 nil))
                (should (claude-term-registry-get root2 nil))
                (should (claude-term-registry-get root3 nil))
                (should (buffer-live-p buf2))
                (should (buffer-live-p buf3))
                (should (process-live-p (buffer-local-value 'ghostel--process buf2)))
                (should (process-live-p (buffer-local-value 'ghostel--process buf3)))
                (should (= (length (claude-term-registry-sessions)) 2)))))
        (ignore-errors (delete-directory root1 t))
        (ignore-errors (delete-directory root2 t))
        (ignore-errors (delete-directory root3 t))))))

;; ============================================================================
;; F1 -- claude-term-kill-all, the actual command SPC a X invokes, kills
;; every live session including one caught mid-restart
;; ============================================================================

(ert-deftest claude-term-registry-live-test-kill-all-clears-in-flight-restart-and-kills-all ()
  "`claude-term-kill-all' kills every live session against real
subprocesses, including one that is mid-restart when the global kill
fires -- exercising its `claude-term--restarting'-clearing guard, which
no test previously called this command to exercise. Without that
guard, `claude-term--on-exit's restart branch would silently resurrect
the mid-restart session moments after being asked to die instead of
tearing it down like its sibling."
  (let ((claude-term-registry-live-test--spawn-log nil)
        (claude-term-registry--table (make-hash-table :test #'equal))
        (root1 (file-name-as-directory (make-temp-file "claude-term-registry-live-killall-1-" t)))
        (root2 (file-name-as-directory (make-temp-file "claude-term-registry-live-killall-2-" t))))
    (cl-letf (((symbol-function 'claude-term--ensure-ghostel) #'ignore)
              ((symbol-function 'ghostel-exec) #'claude-term-registry-live-test--fake-exec))
      (unwind-protect
          (claude-term-registry-live-test--with-buffer
              buf1 "*claude-term-registry-live-test:killall1*"
            (claude-term-registry-live-test--with-buffer
                buf2 "*claude-term-registry-live-test:killall2*"
              (claude-term--exec buf1 root1 nil nil)
              (claude-term--exec buf2 root2 nil nil)
              (should (= (length (claude-term-registry-sessions)) 2))
              ;; Simulate buf2 being mid-restart at the exact moment the
              ;; global kill fires.
              (with-current-buffer buf2 (setq-local claude-term--restarting t))
              (let ((spawn-count-before (length claude-term-registry-live-test--spawn-log)))
                (claude-term-kill-all)
                (should (claude-term-registry-live-test--wait-until
                         (lambda () (and (not (buffer-live-p buf1))
                                         (not (buffer-live-p buf2))))
                         3))
                ;; Neither session was resurrected: no new `ghostel-exec'
                ;; call happened after the kill (buf2's restart branch
                ;; never fired), and both registry entries are gone.
                (should (= (length claude-term-registry-live-test--spawn-log) spawn-count-before))
                (should-not (claude-term-registry-get root1 nil))
                (should-not (claude-term-registry-get root2 nil))
                (should (= (length (claude-term-registry-sessions)) 0)))))
        (ignore-errors (delete-directory root1 t))
        (ignore-errors (delete-directory root2 t))))))

;; ============================================================================
;; AC5 -- rename migrates one instance's key without disturbing a sibling
;; instance's live session under the same root
;; ============================================================================

(ert-deftest claude-term-registry-live-test-rename-does-not-disturb-sibling-instance ()
  (let ((claude-term-registry-live-test--spawn-log nil)
        (claude-term-registry--table (make-hash-table :test #'equal))
        (root (file-name-as-directory
               (make-temp-file "claude-term-registry-live-rename-" t))))
    (cl-letf (((symbol-function 'claude-term--ensure-ghostel) #'ignore)
              ((symbol-function 'ghostel-exec) #'claude-term-registry-live-test--fake-exec))
      (unwind-protect
          (claude-term-registry-live-test--with-buffer
              buf-a "*claude-term-registry-live-test:rename-a*"
            (claude-term-registry-live-test--with-buffer
                buf-b "*claude-term-registry-live-test:rename-b*"
              (claude-term--exec buf-a root "a" nil)
              (claude-term--exec buf-b root "b" nil)
              (let ((proc-b (buffer-local-value 'ghostel--process buf-b))
                    (session-b-before (claude-term-registry-get root "b")))
                (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "c")))
                  (claude-term-rename buf-a))
                ;; Instance "a"'s key is gone; "c" now points at the same
                ;; (renamed) buffer, with its buffer-local instance and
                ;; on-disk name updated to match.
                (should-not (claude-term-registry-get root "a"))
                (let ((session-c (claude-term-registry-get root "c")))
                  (should session-c)
                  (should (eq (claude-term-session-buffer session-c) buf-a)))
                (should (equal (buffer-local-value 'claude-term--instance buf-a) "c"))
                (should (equal (buffer-name buf-a) (claude-term-buffer-name root "c")))
                ;; Sibling "b" is completely untouched: same session
                ;; struct (identity, not just equal fields), same live
                ;; process, same buffer.
                (should (eq (claude-term-registry-get root "b") session-b-before))
                (should (eq (buffer-local-value 'ghostel--process buf-b) proc-b))
                (should (process-live-p proc-b))
                (should (buffer-live-p buf-b)))))
        (ignore-errors (delete-directory root t))))))

;; ============================================================================
;; AC6 -- EDMACS_AGENT_INSTANCE in each session's real child env matches
;; that session's own instance, via the REAL ghostel-pre-spawn-hook
;; ============================================================================

(ert-deftest claude-term-registry-live-test-env-instance-matches-per-session ()
  (let ((claude-term-registry-live-test--spawn-log nil)
        (claude-term-registry--table (make-hash-table :test #'equal))
        (root (file-name-as-directory (make-temp-file "claude-term-registry-live-env-" t))))
    (cl-letf (((symbol-function 'claude-term--ensure-ghostel) #'ignore)
              ((symbol-function 'ghostel-exec) #'claude-term-registry-live-test--fake-exec-env))
      (unwind-protect
          (claude-term-registry-live-test--with-buffer
              buf-a "*claude-term:env-root:a*"
            (claude-term-registry-live-test--with-buffer
                buf-b "*claude-term:env-root:b*"
              (claude-term-registry-live-test--with-buffer
                  buf-default "*claude-term:env-root*"
                (claude-term--exec buf-a root "a" nil)
                (claude-term--exec buf-b root "b" nil)
                (claude-term--exec buf-default root nil nil)
                (should (claude-term-registry-live-test--wait-until
                         (lambda ()
                           (and (with-current-buffer buf-a
                                  (string-match-p "EDMACS_AGENT_INSTANCE=" (buffer-string)))
                                (with-current-buffer buf-b
                                  (string-match-p "EDMACS_AGENT_INSTANCE=" (buffer-string)))
                                (with-current-buffer buf-default
                                  (string-match-p "EDMACS_AGENT_INSTANCE=" (buffer-string)))))
                         3))
                (with-current-buffer buf-a
                  (should (string-match-p "^EDMACS_AGENT_INSTANCE=a$" (buffer-string))))
                (with-current-buffer buf-b
                  (should (string-match-p "^EDMACS_AGENT_INSTANCE=b$" (buffer-string))))
                (with-current-buffer buf-default
                  (should (string-match-p "^EDMACS_AGENT_INSTANCE=default$" (buffer-string)))))))
        (ignore-errors (delete-directory root t))))))

(provide 'claude-term-registry-live-test)
;;; claude-term-registry-live-test.el ends here
