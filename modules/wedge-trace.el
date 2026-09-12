;;; wedge-trace.el --- Record what ran just before a daemon wedge -*- lexical-binding: t -*-

;;; Commentary:
;; Instrumentation for the macOS daemon wedge where the frame stops
;; responding and `emacsclient' gets ECONNREFUSED on a socket that
;; plainly exists.  A `sample(1)' of a wedged daemon shows the main
;; thread parked here:
;;
;;   wait_reading_process_output -> detect_input_pending_run_timers
;;     -> redisplay_preserve_echo_area -> ns_flush_display
;;       -> ns_read_socket_1 -> -[EmacsApp run] -> nextEventMatchingMask
;;
;; That is not a Lisp loop -- there is no `Ffuncall' frame and CPU sits
;; near idle -- so the Lisp debugger, `debug-on-event' and a backtrace
;; all have nothing to say.  The Homebrew build is stripped, so
;; etc/emacs_lldb.py's `xbacktrace' cannot walk the specpdl either.
;; What is left is to record, from inside Emacs, what ran immediately
;; before the event loop stopped returning.
;;
;; keyboard.c's `detect_input_pending_run_timers' runs due timers and
;; *then* redisplays if any of them ran, so the hang lands in one of
;; two places, and the tail of the trace file distinguishes them:
;;
;;   ENTER with no matching EXIT -- the timer function itself hung.
;;   EXIT as the final record    -- the redisplay that followed that
;;                                  timer hung, which is the shape the
;;                                  observed stack actually has.
;;
;; Either way the last named function is the suspect.  Records are
;; written with `write-region', one `write(2)' each, so a wedged daemon
;; still has every completed record visible to `cat' from outside --
;; nothing is buffered in Emacs waiting for a flush that will never come.
;;
;; Redisplay tracing (`edmacs-wedge-trace-redisplay') is off by default:
;; `pre-redisplay-function' runs several times a second while typing,
;; and logging it costs a write each time.  Turn it on only for a
;; session where the timer trace proved too coarse.
;;
;; `scripts/wedge-watchdog.sh' is the outside half -- it notices the
;; wedge within seconds and captures a `sample(1)' while the daemon is
;; still in the wedged state, instead of minutes later.

;;; Code:

(require 'timer)

(defgroup edmacs-wedge-trace nil
  "Record timer activity so a daemon wedge can be attributed afterwards."
  :group 'edmacs)

(defcustom edmacs-wedge-trace-file
  (expand-file-name ".cache/wedge-trace.log" user-emacs-directory)
  "File the timer trace is appended to."
  :type 'file
  :group 'edmacs-wedge-trace)

(defcustom edmacs-wedge-trace-heartbeat-file
  (expand-file-name ".cache/wedge-heartbeat" user-emacs-directory)
  "File rewritten with a timestamp on every heartbeat.
Single-line and overwritten rather than appended, so checking liveness
from outside is one `cat' with no parsing."
  :type 'file
  :group 'edmacs-wedge-trace)

(defcustom edmacs-wedge-trace-heartbeat-interval 5
  "Seconds between heartbeat writes."
  :type 'number
  :group 'edmacs-wedge-trace)

(defcustom edmacs-wedge-trace-max-bytes (* 4 1024 1024)
  "Rotate the trace file once it exceeds this size."
  :type 'integer
  :group 'edmacs-wedge-trace)

(defcustom edmacs-wedge-trace-redisplay nil
  "When non-nil, also log every `pre-redisplay-function' call.
Hot enough to be noticeable while typing; see this file's Commentary."
  :type 'boolean
  :group 'edmacs-wedge-trace)

(defvar edmacs-wedge-trace--heartbeat-timer nil
  "The repeating heartbeat timer, or nil when the mode is off.")

(defvar edmacs-wedge-trace--writes 0
  "Count of records written, used to amortize the rotation size check.")

(defun edmacs-wedge-trace--stamp ()
  "Return the current time, to milliseconds."
  (format-time-string "%F %T.%3N"))

(defun edmacs-wedge-trace--label (fn)
  "Return a short, single-line label for timer function FN."
  (cond
   ((symbolp fn) (symbol-name fn))
   ((byte-code-function-p fn) "#[byte-code]")
   (t (let ((s (format "%S" fn)))
        (if (> (length s) 80) (concat (substring s 0 77) "...") s)))))

(defun edmacs-wedge-trace--write (line)
  "Append LINE to `edmacs-wedge-trace-file', swallowing any error.
A failed write must never break the timer whose firing triggered it.

`create-lockfiles' is off because `write-region' otherwise locks its
target, and a lock conflict calls `ask-user-about-lock', which reads the
minibuffer: from inside a timer that enters a recursive edit the daemon
never leaves, and every later timer recurses into another one.
`inhibit-interaction' is the backstop -- any other prompt this path could
reach signals instead of blocking.  Both are load-bearing; this tracer
wedged the daemon exactly this way on 2026-09-11."
  (condition-case nil
      (let ((coding-system-for-write 'utf-8-unix)
            (write-region-inhibit-fsync t)
            (create-lockfiles nil)
            (inhibit-interaction t))
        (write-region (concat line "\n") nil edmacs-wedge-trace-file
                      'append 'no-message))
    (error nil)))

(defun edmacs-wedge-trace--rotate-maybe ()
  "Rotate the trace file when it has outgrown `edmacs-wedge-trace-max-bytes'.
The size is only stat'd every 200th record; the check is amortized
because it sits on the path of every timer firing."
  (setq edmacs-wedge-trace--writes (1+ edmacs-wedge-trace--writes))
  (when (zerop (mod edmacs-wedge-trace--writes 200))
    (condition-case nil
        (let ((inhibit-interaction t)
              (size (file-attribute-size
                     (file-attributes edmacs-wedge-trace-file))))
          (when (and size (> size edmacs-wedge-trace-max-bytes))
            (rename-file edmacs-wedge-trace-file
                         (concat edmacs-wedge-trace-file ".1") t)))
      (error nil))))

(defun edmacs-wedge-trace--around-timer (orig timer)
  "Log ENTER/EXIT around ORIG's run of TIMER."
  (let ((label (edmacs-wedge-trace--label (timer--function timer)))
        (start (float-time)))
    (edmacs-wedge-trace--rotate-maybe)
    (edmacs-wedge-trace--write
     (format "%s ENTER %s" (edmacs-wedge-trace--stamp) label))
    (unwind-protect
        (funcall orig timer)
      (edmacs-wedge-trace--write
       (format "%s EXIT  %s %.1fms" (edmacs-wedge-trace--stamp) label
               (* 1000 (- (float-time) start)))))))

(defun edmacs-wedge-trace--pre-redisplay (_windows)
  "Log one redisplay entry.  Installed only when `edmacs-wedge-trace-redisplay'."
  (edmacs-wedge-trace--write (format "%s REDISPLAY" (edmacs-wedge-trace--stamp))))

(defun edmacs-wedge-trace--heartbeat ()
  "Overwrite `edmacs-wedge-trace-heartbeat-file' with the current time."
  (condition-case nil
      (let ((coding-system-for-write 'utf-8-unix)
            (write-region-inhibit-fsync t)
            (create-lockfiles nil)
            (inhibit-interaction t))
        (write-region (format "%s pid=%d\n" (edmacs-wedge-trace--stamp) (emacs-pid))
                      nil edmacs-wedge-trace-heartbeat-file nil 'no-message))
    (error nil)))

(defun edmacs-wedge-trace--ensure-directory ()
  "Create the directory holding the trace and heartbeat files."
  (condition-case nil
      (make-directory (file-name-directory edmacs-wedge-trace-file) t)
    (error nil)))

;;;###autoload
(define-minor-mode edmacs-wedge-trace-mode
  "Record every timer firing to `edmacs-wedge-trace-file'.
Left running so that the next daemon wedge can be attributed to the
last function that ran before the event loop stopped returning."
  :global t
  :init-value nil
  :group 'edmacs-wedge-trace
  (if edmacs-wedge-trace-mode
      (progn
        (edmacs-wedge-trace--ensure-directory)
        (advice-add 'timer-event-handler :around #'edmacs-wedge-trace--around-timer)
        (when edmacs-wedge-trace-redisplay
          (add-function :before pre-redisplay-function
                        #'edmacs-wedge-trace--pre-redisplay))
        (unless edmacs-wedge-trace--heartbeat-timer
          (setq edmacs-wedge-trace--heartbeat-timer
                (run-with-timer 0 edmacs-wedge-trace-heartbeat-interval
                                #'edmacs-wedge-trace--heartbeat)))
        (edmacs-wedge-trace--write
         (format "%s START pid=%d" (edmacs-wedge-trace--stamp) (emacs-pid))))
    (advice-remove 'timer-event-handler #'edmacs-wedge-trace--around-timer)
    (remove-function pre-redisplay-function #'edmacs-wedge-trace--pre-redisplay)
    (when edmacs-wedge-trace--heartbeat-timer
      (cancel-timer edmacs-wedge-trace--heartbeat-timer)
      (setq edmacs-wedge-trace--heartbeat-timer nil))
    (edmacs-wedge-trace--write
     (format "%s STOP pid=%d" (edmacs-wedge-trace--stamp) (emacs-pid)))))

(defun edmacs-wedge-trace-tail (&optional n)
  "Return the last N (default 40) records of the trace file as a string."
  (let ((n (or n 40)))
    (with-temp-buffer
      (condition-case nil
          (insert-file-contents edmacs-wedge-trace-file)
        (error nil))
      (goto-char (point-max))
      (forward-line (- n))
      (buffer-substring-no-properties (point) (point-max)))))

(defun edmacs-wedge-trace-verdict ()
  "Report which function the trace file blames for the last wedge.
Reads the tail of the trace and names the final record: an unmatched
ENTER means that timer function never returned; a trailing EXIT means
the redisplay which followed it is where the event loop stopped."
  (interactive)
  (let* ((tail (edmacs-wedge-trace-tail 200))
         (lines (seq-remove #'string-empty-p (split-string tail "\n")))
         (last (car (last lines))))
    (message
     "%s"
     (cond
      ((null last) (format "wedge-trace: %s is empty" edmacs-wedge-trace-file))
      ((string-match "\\`\\(.*?\\) ENTER \\(.*\\)\\'" last)
       (format "wedge-trace: %s entered at %s and never returned"
               (match-string 2 last) (match-string 1 last)))
      ((string-match "\\`\\(.*?\\) EXIT  \\([^ ]+\\)" last)
       (format "wedge-trace: last completed timer was %s at %s; \
a hang after this points at the redisplay it triggered"
               (match-string 2 last) (match-string 1 last)))
      (t (format "wedge-trace: last record: %s" last))))))

(provide 'wedge-trace)
;;; wedge-trace.el ends here
