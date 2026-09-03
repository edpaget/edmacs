;;; agents-live-test.el --- Live file-notify test for agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; AC3's actual claim -- "sending a prompt flips its row to `working'
;; within a second... no timer polls the directory" -- is a claim about
;; the real OS-level `file-notify' wiring, not about
;; `edmacs-agents--workmux-watch-callback' in isolation (that direct-call
;; coverage lives in modules/agents-test.el, unconditionally). This file
;; arms a REAL watch via `edmacs-agents--ensure-workmux-watch' against a
;; real temp directory and writes real files to it, exactly as workmux
;; would, then waits for the row to change with no explicit ingest call
;; from the test.
;;
;; Self-preflights first, following modules/frames-live-test.el's own
;; documented convention: on this suite's development machine, `--batch'
;; mode's kqueue backend accepts a watch but never actually delivers a
;; callback for a real filesystem change, while the identical watch fires
;; immediately under a real `emacs --daemon'. The preflight distinguishes
;; that known `--batch'/kqueue gap from an actual regression in this
;; file's own watch plumbing, and skips (rather than fails) when it can't
;; be exercised here.
;;
;; Manually verified green against a real, throwaway `emacs --daemon'
;; (`emacsclient --eval' driving `ert-run-tests-batch' directly):
;; `edmacs-agents-live-test-real-watch-flips-and-reaps' passes end to
;; end there -- a fresh JSON file appearing, a status edit landing on
;; the same path, and the file being deleted are each reflected in
;; `edmacs-agents--table' purely off the real `file-notify' watch, with
;; no explicit ingest call from the test. Under plain `-Q --batch' on
;; this same machine the preflight above correctly reports the backend
;; cannot deliver here and the suite skips instead of failing.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/agents.el -l modules/agents-live-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'filenotify)

(defun edmacs-agents-live-test--wait-until (predicate timeout)
  "Pump the event loop until PREDICATE is non-nil or TIMEOUT seconds pass.
`sit-for' (not `sleep-for') is what actually lets a pending `file-notify'
callback run -- it processes input/timer/subprocess events, `sleep-for'
merely blocks."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (< (float-time) deadline) (not (funcall predicate)))
      (sit-for 0.05))
    (funcall predicate)))

(defun edmacs-agents-live-test--file-notify-delivers-p ()
  "Return non-nil iff a real `file-notify' watch actually delivers here.
Identical rationale and mechanism to
`edmacs-frames-live-test--file-notify-delivers-p': preflighting this
trivial case, rather than skipping only on `file-notify-add-watch'
itself signaling, is what distinguishes \"this backend cannot deliver
under `--batch' here\" from a genuine regression in this file's own
watch plumbing."
  (let* ((dir (make-temp-file "edmacs-agents-fnprobe-" t))
         (probe (expand-file-name "probe" dir))
         (fired nil)
         (desc (ignore-errors
                 (file-notify-add-watch dir '(change) (lambda (_ev) (setq fired t))))))
    (unwind-protect
        (progn
          (when desc
            (write-region "x" nil probe nil 'silent)
            (edmacs-agents-live-test--wait-until (lambda () fired) 2.0))
          fired)
      (when desc (ignore-errors (file-notify-rm-watch desc)))
      (delete-directory dir t))))

(defun edmacs-agents-live-test--write (path workdir status ts)
  "Write a real workmux-shaped JSON file at PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert (json-serialize
               `((pane_key . ((backend . "tmux") (instance . "/tmp/x") (pane_id . "%1")))
                 (workdir . ,workdir)
                 (status . ,status)
                 (status_ts . ,ts)
                 (updated_ts . ,ts)
                 (pane_title . "Claude Code")
                 (window_name . "w1")
                 (session_name . "s1")))))))

(if (not (edmacs-agents-live-test--file-notify-delivers-p))

    (ert-deftest edmacs-agents-live-test-file-notify-unavailable ()
      (ert-skip "the real file-notify backend in this batch environment never \
delivers a callback for a real filesystem change (confirmed independently of \
this feature's own code -- see this file's Commentary); run against a real \
`emacs --daemon' to exercise this suite"))

  (ert-deftest edmacs-agents-live-test-real-watch-flips-and-reaps ()
    "A real `file-notify' watch on a real directory -- no explicit ingest
call from this test -- carries a fresh file to `working', an edit to
`done'+unread, and a deletion to row-gone, purely off the OS-level
watch `edmacs-agents--ensure-workmux-watch' arms."
    (let* ((edmacs-agents--table (make-hash-table :test #'equal))
           (edmacs-agents--workmux-path->key (make-hash-table :test #'equal))
           (edmacs-agents-changed-hook nil)
           (edmacs-agents--workmux-watch nil)
           (dir (make-temp-file "edmacs-agents-live-test-" t))
           (edmacs-agents-workmux-dir dir)
           (workdir (make-temp-file "edmacs-agents-live-test-workdir-" t))
           (root (file-truename workdir))
           (key (edmacs-agents--key root "%1"))
           (path (expand-file-name "pane.json" dir)))
      (unwind-protect
          (progn
            (edmacs-agents--ensure-workmux-watch)
            (should edmacs-agents--workmux-watch)
            ;; Created: a fresh file with no prior ingest call.
            (edmacs-agents-live-test--write path workdir "working" 100)
            (should (edmacs-agents-live-test--wait-until
                     (lambda () (gethash key edmacs-agents--table)) 5.0))
            (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'working))
            ;; Changed: an edit to the same path lands as `done'+unread.
            (edmacs-agents-live-test--write path workdir "done" 200)
            (should (edmacs-agents-live-test--wait-until
                     (lambda () (eq (edmacs-agent-status (gethash key edmacs-agents--table))
                                     'done))
                     5.0))
            (should (edmacs-agent-unread (gethash key edmacs-agents--table)))
            ;; Deleted: the row disappears with no sweep involved.
            (delete-file path)
            (should (edmacs-agents-live-test--wait-until
                     (lambda () (not (gethash key edmacs-agents--table))) 5.0)))
        (when edmacs-agents--workmux-watch
          (ignore-errors (file-notify-rm-watch edmacs-agents--workmux-watch)))
        (ignore-errors (delete-directory dir t))
        (ignore-errors (delete-directory workdir t))))))

(provide 'agents-live-test)
;;; agents-live-test.el ends here
