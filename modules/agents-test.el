;;; agents-test.el --- Tests for agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage -- no real subprocess, timer, watch, or
;; nano-modeline construct is involved. Every test let-binds
;; `edmacs-agents--table' to a fresh hash table, the same isolation
;; convention `claude-term-registry-test.el' uses for its own table, so
;; no test reads or mutates any real state.
;;
;; This file no longer carries any mode-line construct-level coverage:
;; agents.el sheds that concern entirely (the edmacs-modeline roadmap
;; phase moved it to claude-term-agents.el, scoped to one buffer's own
;; status instead of a cross-project roll-up) -- see
;; claude-term-agents-test.el for that coverage now.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/agents.el -l modules/agents-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

(defmacro edmacs-agents-test--with-clean-state (&rest body)
  "Run BODY with a fresh agent table and changed hook."
  (declare (indent 0))
  `(let ((edmacs-agents--table (make-hash-table :test #'equal))
         (edmacs-agents-changed-hook nil))
     ,@body))

;; ============================================================================
;; edmacs-agents--compute-unread (the one shared transition rule)
;; ============================================================================

(ert-deftest edmacs-agents-test-compute-unread ()
  "`edmacs-agents--compute-unread' is the single, pure definition of the
unread rule every writer delegates to: `done' sets it, `working'/`waiting'
clears it, anything else (including a fresh row and a done -> done
refresh) preserves whatever it already was."
  ;; Fresh row (no prior state): OLD-STATUS and OLD-UNREAD are nil.
  (should (eq (edmacs-agents--compute-unread nil 'done nil) t))
  (should (eq (edmacs-agents--compute-unread nil 'working nil) nil))
  (should (eq (edmacs-agents--compute-unread nil 'waiting nil) nil))
  ;; Transition into `done' from something else: sets unread regardless
  ;; of what it was before.
  (should (eq (edmacs-agents--compute-unread 'working 'done nil) t))
  (should (eq (edmacs-agents--compute-unread 'waiting 'done t) t))
  ;; Transition into `working'/`waiting': always clears, regardless of
  ;; the prior status or unread value.
  (should (eq (edmacs-agents--compute-unread 'done 'working t) nil))
  (should (eq (edmacs-agents--compute-unread 'idle 'waiting t) nil))
  ;; done -> done refresh (timestamp-only): preserves, does not re-set.
  (should (eq (edmacs-agents--compute-unread 'done 'done nil) nil))
  (should (eq (edmacs-agents--compute-unread 'done 'done t) t))
  ;; Any other transition (e.g. into `idle'): preserves old-unread.
  (should (eq (edmacs-agents--compute-unread 'working 'idle t) t))
  (should (eq (edmacs-agents--compute-unread 'working 'idle nil) nil)))

;; ============================================================================
;; edmacs-agents-set-status
;; ============================================================================

(ert-deftest edmacs-agents-test-set-status-fires-hook-once ()
  "Each call to `edmacs-agents-set-status' fires the changed hook exactly once."
  (edmacs-agents-test--with-clean-state
    (let* ((root (make-temp-file "edmacs-agents-test-root-" t))
           (fire-count 0))
      (add-hook 'edmacs-agents-changed-hook (lambda (_keys) (cl-incf fire-count)))
      (edmacs-agents-set-status root 'waiting)
      (should (= fire-count 1))
      (should (= (hash-table-count edmacs-agents--table) 1))
      (edmacs-agents-set-status root 'working)
      (should (= fire-count 2))
      (should (= (hash-table-count edmacs-agents--table) 1))
      (let (row)
        (maphash (lambda (_k r) (setq row r)) edmacs-agents--table)
        (should (eq (edmacs-agent-status row) 'working))))))

(ert-deftest edmacs-agents-test-set-status-ambiguous-instance-errors ()
  "`edmacs-agents-set-status' with no INSTANCE and two existing rows
under the same root signals a clear `user-error' instead of guessing."
  (edmacs-agents-test--with-clean-state
    (let* ((root (make-temp-file "edmacs-agents-test-root-" t)))
      (edmacs-agents-set-status root 'waiting "a")
      (edmacs-agents-set-status root 'waiting "b")
      (should-error (edmacs-agents-set-status root 'working) :type 'user-error))))

;; ============================================================================
;; Nothing reaps on age any more
;; ============================================================================

(ert-deftest edmacs-agents-test-no-age-based-reap ()
  "A row created through the public `edmacs-agents-set-status' writer
survives no matter how old its heartbeat is -- there is no sweep any
more, so a `waiting' or unread-`done' row is never silently discarded
for sitting quiet."
  (edmacs-agents-test--with-clean-state
    (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t))))
      (edmacs-agents-set-status root 'waiting)
      (let (key row)
        (maphash (lambda (k r) (setq key k row r)) edmacs-agents--table)
        ;; Force the heartbeat far into the past, as if the row had sat
        ;; quietly in `waiting' for hours with no further explicit call.
        (setf (edmacs-agent-updated-ts row) (- (float-time) 999999))
        (should (gethash key edmacs-agents--table))
        (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'waiting))))))

;; ============================================================================
;; Unread transitions
;; ============================================================================

(ert-deftest edmacs-agents-test-unread-transition ()
  "A transition into `done' sets `unread'; `edmacs-agents-mark-read'
clears it and moves the row to `idle'."
  (edmacs-agents-test--with-clean-state
    (let* ((root (make-temp-file "edmacs-agents-test-root-" t)))
      (edmacs-agents-set-status root 'working)
      (let (key)
        (maphash (lambda (k _r) (setq key k)) edmacs-agents--table)
        (edmacs-agents-set-status root 'done)
        (should (edmacs-agent-unread (gethash key edmacs-agents--table)))
        (edmacs-agents-mark-read key)
        (should-not (edmacs-agent-unread (gethash key edmacs-agents--table)))
        (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'idle))))))

(ert-deftest edmacs-agents-test-unread-refresh-does-not-retrigger ()
  "A done -> done timestamp-only refresh (status never left `done' in
between) does not spuriously re-set `unread' once cleared -- contrast
with `edmacs-agents-test-unread-transition', where the row passes back
through `idle' (via `edmacs-agents-mark-read') before going `done'
again, which IS a fresh transition and DOES re-set `unread'."
  (edmacs-agents-test--with-clean-state
    (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (key (edmacs-agents--key root "%1"))
           (row (make-edmacs-agent :key key :root root :instance "%1"
                                    :status 'done :status-ts 100 :updated-ts 100
                                    :title "t" :source 'claude-term :locator nil
                                    :unread (edmacs-agents--compute-unread nil 'done nil))))
      (edmacs-agents--upsert row)
      (should (edmacs-agent-unread (gethash key edmacs-agents--table)))
      ;; Cleared by some means other than mark-read, while status stays
      ;; `done' throughout -- e.g. a future "peek" affordance.
      (setf (edmacs-agent-unread (gethash key edmacs-agents--table)) nil)
      ;; Refresh: status still `done', so this is not a fresh transition.
      (let ((refreshed (gethash key edmacs-agents--table)))
        (setf (edmacs-agent-status-ts refreshed) 150
              (edmacs-agent-updated-ts refreshed) 150
              (edmacs-agent-unread refreshed)
              (edmacs-agents--compute-unread 'done 'done (edmacs-agent-unread refreshed)))
        (edmacs-agents--upsert refreshed))
      (should-not (edmacs-agent-unread (gethash key edmacs-agents--table))))))

(ert-deftest edmacs-agents-test-mark-read-bumps-status-ts ()
  "`edmacs-agents-mark-read' clears unread, sets status to `idle', and
bumps STATUS-TS to (approximately) now."
  (edmacs-agents-test--with-clean-state
    (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (key (edmacs-agents--key root "%1"))
           (row (make-edmacs-agent :key key :root root :instance "%1"
                                    :status 'done :status-ts 100 :updated-ts 100
                                    :title "t" :source 'claude-term :locator nil
                                    :unread t)))
      (edmacs-agents--upsert row)
      (edmacs-agents-mark-read key)
      (let ((updated (gethash key edmacs-agents--table)))
        (should (eq (edmacs-agent-status updated) 'idle))
        (should-not (edmacs-agent-unread updated))
        (should (< (abs (- (edmacs-agent-status-ts updated) (float-time))) 5))))))

;; ============================================================================
;; Tabulated-list view
;; ============================================================================

(ert-deftest edmacs-agents-test-list-entries ()
  "`edmacs-agents--list-entries' returns one `tabulated-list-entries' row
per table row, keyed by the row's own key, with the unread `done' flag
rendered as a trailing `*' and a read `idle' row rendered without one."
  (edmacs-agents-test--with-clean-state
    (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (done-key (edmacs-agents--key root "%1"))
           (idle-key (edmacs-agents--key root "%2")))
      (puthash done-key (make-edmacs-agent
                          :key done-key :root root :instance "%1"
                          :status 'done :status-ts 1 :updated-ts 1
                          :title "Fixing the thing" :source 'claude-term
                          :locator nil :unread t)
               edmacs-agents--table)
      (puthash idle-key (make-edmacs-agent
                          :key idle-key :root root :instance "%2"
                          :status 'idle :status-ts 1 :updated-ts 1
                          :title "Already read" :source 'claude-term
                          :locator nil :unread nil)
               edmacs-agents--table)
      (let ((entries (edmacs-agents--list-entries)))
        (should (= (length entries) 2))
        (should (assoc done-key entries))
        (should (assoc idle-key entries))
        (let ((done-cols (cadr (assoc done-key entries)))
              (idle-cols (cadr (assoc idle-key entries))))
          (should (equal (aref done-cols 0) "%1"))
          (should (equal (aref done-cols 1) root))
          (should (equal (aref done-cols 2) "%1"))
          (should (equal (aref done-cols 3) "done*"))
          (should (equal (aref done-cols 4) "Fixing the thing"))
          (should (equal (aref idle-cols 3) "idle"))
          (should (equal (aref idle-cols 4) "Already read")))))))

(ert-deftest edmacs-agents-test-list-entries-empty ()
  "An empty table produces an empty entries list, not an error."
  (edmacs-agents-test--with-clean-state
    (should (equal (edmacs-agents--list-entries) nil))))

(provide 'agents-test)
;;; agents-test.el ends here
