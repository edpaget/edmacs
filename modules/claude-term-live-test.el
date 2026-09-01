;;; claude-term-live-test.el --- Simulated live-process regression tests -*- lexical-binding: t -*-

;;; Commentary:
;; claude-term-test.el intentionally covers only pure helpers, since no
;; `ghostel' package is bootstrapped in this repository's straight
;; lockfile yet (see modules/claude-term.el's commentary on
;; `use-package ghostel').  That leaves the kill/restart/exit lifecycle
;; -- the phase's actual "Done when" clause -- verified only by manual
;; M-x use.  This file closes that gap as far as a batch harness can:
;; it drives `claude-term--exec', `claude-term-kill', and
;; `claude-term-restart' against REAL Emacs subprocesses (plain `sleep'
;; stand-ins for `claude') through Emacs's genuine async process
;; machinery, with `ghostel-exec' replaced by a stub whose sentinel is a
;; faithful re-implementation of ghostel.el's own `ghostel--sentinel'
;; tail (lisp/ghostel.el:3821-3841 in dakra/ghostel, fetched from
;; GitHub main 2026-09-01 -- no lockfile pin exists yet to read
;; instead).  That tail is exactly what a prior code review found this
;; module's restart path was racing: `ghostel-exit-functions' runs
;; first, and THEN, unconditionally, if `ghostel-kill-buffer-on-exit'
;; is nil, a "[Process exited]" banner is stamped into the buffer --
;; regardless of whether the hook already attached a new live process.
;; `claude-term-live-test-restart-no-stray-banner' below reproduces
;; that race with a real process and asserts the banner never survives
;; into the restarted buffer.
;;
;; What this does NOT cover: the real `ghostel' package's terminal
;; rendering, `ghostel-mode', or the actual `claude' CLI -- those need
;; an interactive Emacs and are out of scope for a batch harness.
;;
;; Run:
;;   emacs -Q --batch -l ert -l modules/claude-term.el \
;;         -l modules/claude-term-live-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; The real `inheritenv' package is not bootstrapped under `-Q --batch'
;; (straight.el is absent), but `claude-term--exec' unconditionally
;; wraps its `ghostel-exec' call in `(inheritenv ...)'.  Production
;; startup always bootstraps straight first (see claude-term.el's own
;; commentary on this exact gap for `inheritenv' itself), so this shim
;; is test-only: a plain `progn' is exactly what `inheritenv' reduces
;; to for a local, non-Tramp process, which is all this harness spawns.
(unless (fboundp 'inheritenv)
  (defmacro inheritenv (&rest body)
    `(progn ,@body)))

(defvar claude-term-live-test--spawn-log nil
  "List of (BUFFER PROGRAM . ARGS) recorded by the `ghostel-exec' stub.")

(defun claude-term-live-test--fake-ghostel-sentinel (process event)
  "Re-implementation of ghostel.el's `ghostel--sentinel' tail for PROCESS/EVENT.
Runs `ghostel-exit-functions' first, then -- unconditionally, exactly as
real ghostel does -- stamps a \"[Process exited]\" banner into the
buffer when `ghostel-kill-buffer-on-exit' is nil, whether or not the
hook already replaced the buffer's live process."
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

(defun claude-term-live-test--fake-exec (buffer program args)
  "Stub for `ghostel-exec': attach a real dummy process to BUFFER.
Ignores PROGRAM beyond logging it; always spawns \"sleep 3600\" as a
stand-in long-lived process.  Erases BUFFER first, mirroring
`ghostel-exec' -> `ghostel--init-buffer's `erase-buffer' -- this is
what discards the stray exit banner on a real restart."
  (push (list buffer program args) claude-term-live-test--spawn-log)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)) (erase-buffer))
    (let ((proc (start-process "claude-term-live-test" buffer "sleep" "3600")))
      (set-process-sentinel proc #'claude-term-live-test--fake-ghostel-sentinel)
      ;; Batch Emacs has no minibuffer to answer `kill-buffer's "has a
      ;; running process, kill it?" query, which `end-of-file's on
      ;; stdin; these are throwaway test processes, never query.
      (set-process-query-on-exit-flag proc nil)
      (setq-local ghostel--process proc)
      proc)))

(defun claude-term-live-test--wait-until (pred &optional timeout)
  "Pump the process event loop until PRED is non-nil or TIMEOUT elapses.
TIMEOUT defaults to 2 seconds.  Returns PRED's final value."
  (let ((deadline (+ (float-time) (or timeout 2))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defmacro claude-term-live-test--with-stubs (&rest body)
  "Run BODY with `ghostel-exec' and `claude-term--ensure-ghostel' stubbed.
`claude-term--ensure-ghostel' is stubbed to a no-op since the real
`ghostel' package is not installed in this batch harness (see
Commentary); `ghostel-exec' is replaced with the real-subprocess stub
above."
  `(let ((claude-term-live-test--spawn-log nil))
     (cl-letf (((symbol-function 'claude-term--ensure-ghostel) #'ignore)
               ((symbol-function 'ghostel-exec) #'claude-term-live-test--fake-exec))
       ,@body)))

(defmacro claude-term-live-test--with-buffer (var name &rest body)
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

(ert-deftest claude-term-live-test-kill-cleans-up-buffer ()
  "`claude-term-kill' leaves no stale buffer once the sentinel fires."
  (claude-term-live-test--with-stubs
   (claude-term-live-test--with-buffer buf "*claude-term-live-test:kill*"
     (claude-term--exec buf (temporary-file-directory) nil nil)
     (should (process-live-p (buffer-local-value 'ghostel--process buf)))
     (claude-term-kill buf)
     (should (claude-term-live-test--wait-until
              (lambda () (not (buffer-live-p buf)))))
     (should-not (get-buffer "*claude-term-live-test:kill*")))))

(ert-deftest claude-term-live-test-plain-exit-cleans-up ()
  "A process exiting on its own (not via `claude-term-kill') is cleaned up
identically -- `claude-term--on-exit' is the single point of truth
regardless of how the process died."
  (claude-term-live-test--with-stubs
   (claude-term-live-test--with-buffer buf "*claude-term-live-test:plain-exit*"
     (claude-term--exec buf (temporary-file-directory) nil nil)
     (let ((proc (buffer-local-value 'ghostel--process buf)))
       (should (process-live-p proc))
       (kill-process proc))
     (should (claude-term-live-test--wait-until
              (lambda () (not (buffer-live-p buf))))))))

(ert-deftest claude-term-live-test-restart-no-stray-banner ()
  "Restarting a live session leaves the buffer's NAME and identity intact,
attaches a genuinely new live process, and -- the prior code review's
finding -- never lets ghostel's unconditional post-hook \"[Process
exited]\" banner survive into the restarted buffer."
  (claude-term-live-test--with-stubs
   (claude-term-live-test--with-buffer buf "*claude-term-live-test:restart*"
     (claude-term--exec buf (temporary-file-directory) nil '("--x"))
     (let ((old-pid (process-id (buffer-local-value 'ghostel--process buf))))
       (claude-term-restart buf)
       (should (claude-term-live-test--wait-until
                (lambda ()
                  (and (buffer-live-p buf)
                       (process-live-p (buffer-local-value 'ghostel--process buf))
                       (not (eql (process-id (buffer-local-value 'ghostel--process buf))
                                 old-pid))))))
       ;; Give any further pending sentinel/timer activity a moment to
       ;; settle before asserting on final buffer state.
       (accept-process-output nil 0.3)
       (should (buffer-live-p buf))
       (should (equal (buffer-name buf) "*claude-term-live-test:restart*"))
       (should-not (buffer-local-value 'claude-term--restarting buf))
       (with-current-buffer buf
         (should-not (string-match-p "Process exited" (buffer-string))))
       ;; The restart re-exec must reuse the args frozen at original
       ;; spawn, never re-resolve them -- two log entries, same args.
       (should (equal (length claude-term-live-test--spawn-log) 2))
       (should (equal (nth 2 (nth 0 claude-term-live-test--spawn-log)) '("--x")))
       (should (equal (nth 2 (nth 1 claude-term-live-test--spawn-log)) '("--x")))))))

(ert-deftest claude-term-live-test-restart-noop-while-in-flight ()
  "Firing `claude-term-restart' twice in immediate succession is a no-op
the second time -- no second `kill-process' call, no duplicate re-exec."
  (claude-term-live-test--with-stubs
   (claude-term-live-test--with-buffer buf "*claude-term-live-test:double-restart*"
     (claude-term--exec buf (temporary-file-directory) nil nil)
     (claude-term-restart buf)
     (should (buffer-local-value 'claude-term--restarting buf))
     ;; Second call while the flag is still set: must not error and must
     ;; not attempt another `kill-process' on an already-dying process.
     (claude-term-restart buf)
     (should (claude-term-live-test--wait-until
              (lambda ()
                (and (buffer-live-p buf)
                     (process-live-p (buffer-local-value 'ghostel--process buf))))))
     (accept-process-output nil 0.3)
     ;; Exactly two spawns total: the original plus the one restart.
     (should (equal (length claude-term-live-test--spawn-log) 2)))))

(provide 'claude-term-live-test)
;;; claude-term-live-test.el ends here
