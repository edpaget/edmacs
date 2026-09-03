;;; agents-test.el --- Tests for agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage only -- no real workmux process, tmux pane, or
;; file-notify event is involved. `edmacs-agents-init' is never called
;; here (and agents.el itself never calls it at load time -- see that
;; file's Commentary): every test let-binds `edmacs-agents--table' (and
;; `edmacs-agents--workmux-path->key' where relevant) to a fresh hash
;; table, the same isolation convention `claude-term-registry-test.el'
;; uses for its own table, so no test reads or mutates this machine's
;; real `~/.local/state/workmux/agents/' state.
;;
;; The genuinely environment-dependent acceptance checks (this
;; machine's real state directory, a live tmux prompt round-trip,
;; actually killing a pane) are manual/live steps recorded in this
;; phase's commit body instead, per the phase body's own hedge.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/agents.el -l modules/agents-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

(defmacro edmacs-agents-test--with-clean-state (&rest body)
  "Run BODY with a fresh agent table, path map, and changed hook."
  (declare (indent 0))
  `(let ((edmacs-agents--table (make-hash-table :test #'equal))
         (edmacs-agents--workmux-path->key (make-hash-table :test #'equal))
         (edmacs-agents-changed-hook nil))
     ,@body))

(defun edmacs-agents-test--write-json (dir status-ts updated-ts &optional status workdir pane-id)
  "Write one workmux-shaped JSON file under DIR and return its path."
  (let* ((workdir (or workdir (make-temp-file "edmacs-agents-test-workdir-" t)))
         (pane-id (or pane-id (format "%%%d" (random 100000))))
         (path (expand-file-name (format "test-%s.json" (random 100000)) dir))
         (json (json-serialize
                `((pane_key . ((backend . "tmux") (instance . "/tmp/x") (pane_id . ,pane-id)))
                  (workdir . ,workdir)
                  (status . ,(or status "working"))
                  (status_ts . ,status-ts)
                  (updated_ts . ,updated-ts)
                  (pane_title . "⠂ Claude Code")
                  (window_name . "w1")
                  (session_name . "s1")))))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file path (insert json)))
    path))

;; ============================================================================
;; Parsing
;; ============================================================================

(ert-deftest edmacs-agents-test-parse-row ()
  "A well-formed workmux JSON file parses into an `edmacs-agent' with
every field carried over, and the spinner glyph is stripped from the title."
  (let* ((dir (make-temp-file "edmacs-agents-test-scan-" t))
         (workdir (make-temp-file "edmacs-agents-test-workdir-" t))
         (path (edmacs-agents-test--write-json dir 1000 1000 "working" workdir "%42")))
    (let* ((json (edmacs-agents--parse-workmux-file path))
           (row (edmacs-agents--row-from-workmux-json json)))
      (should row)
      (should (equal (edmacs-agent-root row) (file-truename workdir)))
      (should (equal (edmacs-agent-instance row) "%42"))
      (should (eq (edmacs-agent-status row) 'working))
      (should (equal (edmacs-agent-status-ts row) 1000))
      (should (equal (edmacs-agent-updated-ts row) 1000))
      (should (equal (edmacs-agent-title row) "Claude Code"))
      (should (eq (edmacs-agent-source row) 'workmux))
      (should (equal (plist-get (edmacs-agent-locator row) :pane-id) "%42")))))

(ert-deftest edmacs-agents-test-parse-malformed-ignored ()
  "Malformed JSON is skipped without signaling, both at the parse layer
and at the row-building layer for a well-formed-but-unrecognized status."
  (let* ((dir (make-temp-file "edmacs-agents-test-scan-" t))
         (bad-path (expand-file-name "bad.json" dir)))
    (with-temp-file bad-path (insert "{ this is not json"))
    (should-not (edmacs-agents--parse-workmux-file bad-path))
    (should-not (edmacs-agents--intern-workmux-status "exploding"))
    (should-not (edmacs-agents--row-from-workmux-json '((workdir . "/tmp/x")
                                                         (status . "exploding")
                                                         (pane_key . ((pane_id . "%1"))))))))

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
;; status_ts ordering
;; ============================================================================

(ert-deftest edmacs-agents-test-timestamp-ordering ()
  "A stale update (older `status_ts') is ignored; a newer one is applied."
  (edmacs-agents-test--with-clean-state
    (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (key (edmacs-agents--key root "%1")))
      (should (edmacs-agents--apply-workmux-row
               (make-edmacs-agent :key key :root root :instance "%1"
                                   :status 'working :status-ts 100 :updated-ts 100
                                   :title "t" :source 'workmux :locator nil)))
      (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'working))
      ;; Older status_ts: ignored.
      (should-not (edmacs-agents--apply-workmux-row
                   (make-edmacs-agent :key key :root root :instance "%1"
                                       :status 'done :status-ts 50 :updated-ts 200
                                       :title "t" :source 'workmux :locator nil)))
      (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'working))
      ;; Newer status_ts: applied.
      (should (edmacs-agents--apply-workmux-row
               (make-edmacs-agent :key key :root root :instance "%1"
                                   :status 'done :status-ts 200 :updated-ts 200
                                   :title "t" :source 'workmux :locator nil)))
      (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'done))
      (should (edmacs-agent-unread (gethash key edmacs-agents--table))))))

;; ============================================================================
;; Load-time scan-then-sweep ordering
;; ============================================================================

(ert-deftest edmacs-agents-test-scan-then-sweep-order ()
  "Scanning a directory with one fresh and one stale heartbeat, then
sweeping, leaves only the fresh row -- the corrected AC1 load ordering."
  (edmacs-agents-test--with-clean-state
    (let* ((dir (make-temp-file "edmacs-agents-test-scan-" t))
           (edmacs-agents-workmux-dir dir)
           (edmacs-agents-stale-seconds 5)
           (now (float-time)))
      (edmacs-agents-test--write-json dir now now "working" nil "%fresh")
      (edmacs-agents-test--write-json dir (- now 3600) (- now 3600) "done" nil "%stale")
      (edmacs-agents--scan-workmux-dir)
      (should (= (hash-table-count edmacs-agents--table) 2))
      (edmacs-agents--sweep)
      (should (= (hash-table-count edmacs-agents--table) 1))
      (let (survivor)
        (maphash (lambda (_k row) (setq survivor row)) edmacs-agents--table)
        (should (equal (edmacs-agent-instance survivor) "%fresh"))))))

;; ============================================================================
;; file-notify watch callback (AC3's actual mechanism)
;; ============================================================================

(ert-deftest edmacs-agents-test-watch-callback-created-changed-deleted ()
  "`edmacs-agents--workmux-watch-callback' -- the function `file-notify'
actually invokes, per `edmacs-agents--ensure-workmux-watch' -- ingests on
`created'/`changed' and forgets on `deleted', driven by real files on
disk (no live file-notify backend involved: the EVENT tuples are
synthesized directly, since a real backend is not guaranteed to deliver
under `--batch' -- see modules/frames-live-test.el's own documented
finding on this machine)."
  (edmacs-agents-test--with-clean-state
    (let* ((dir (make-temp-file "edmacs-agents-test-watch-" t))
           (path (edmacs-agents-test--write-json dir 100 100 "working"))
           (key (edmacs-agent-key (edmacs-agents--row-from-workmux-json
                                    (edmacs-agents--parse-workmux-file path)))))
      ;; `created': ingests the row.
      (edmacs-agents--workmux-watch-callback (list 'desc 'created path))
      (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'working))
      (should (equal (gethash path edmacs-agents--workmux-path->key) key))
      ;; `changed': re-reads the same file and applies the update.
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file path
          (insert (json-serialize
                   `((pane_key . ((pane_id . ,(edmacs-agent-instance
                                                (gethash key edmacs-agents--table)))))
                     (workdir . ,(edmacs-agent-root (gethash key edmacs-agents--table)))
                     (status . "done")
                     (status_ts . 200)
                     (updated_ts . 200)
                     (pane_title . "Claude Code"))))))
      (edmacs-agents--workmux-watch-callback (list 'desc 'changed path))
      (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'done))
      (should (edmacs-agent-unread (gethash key edmacs-agents--table)))
      ;; `deleted': forgets the row via the path -> key map, no rescan needed.
      (edmacs-agents--workmux-watch-callback (list 'desc 'deleted path))
      (should-not (gethash key edmacs-agents--table))
      (should-not (gethash path edmacs-agents--workmux-path->key)))))

(ert-deftest edmacs-agents-test-watch-callback-ignores-non-json ()
  "A `created'/`changed' event on a file that is not a `.json' path is
ignored, and a callback error (e.g. from a malformed event tuple) is
caught rather than propagated -- the watch must not die on one bad event."
  (edmacs-agents-test--with-clean-state
    (let* ((dir (make-temp-file "edmacs-agents-test-watch-" t))
           (txt-path (expand-file-name "not-json.txt" dir)))
      (with-temp-file txt-path (insert "hello"))
      (edmacs-agents--workmux-watch-callback (list 'desc 'created txt-path))
      (should (zerop (hash-table-count edmacs-agents--table)))
      ;; A malformed event (missing FILE) throws inside the body (a nil
      ;; FILE reaching `string-suffix-p'); the callback's own
      ;; `condition-case' must swallow it rather than letting it escape.
      (should-not (condition-case nil
                      (progn (edmacs-agents--workmux-watch-callback (list 'desc 'created))
                             nil)
                    (error t)))
      (should (zerop (hash-table-count edmacs-agents--table))))))

;; ============================================================================
;; Heartbeat reaping
;; ============================================================================

(ert-deftest edmacs-agents-test-heartbeat-reap ()
  "`edmacs-agents--sweep' removes a row whose heartbeat has aged past
`edmacs-agents-stale-seconds', firing the changed hook exactly once
with the removed key."
  (edmacs-agents-test--with-clean-state
    (let* ((edmacs-agents-stale-seconds 5)
           (root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (key (edmacs-agents--key root "%1"))
           (fire-count 0)
           (fired-keys nil))
      (puthash key (make-edmacs-agent :key key :root root :instance "%1"
                                       :status 'waiting
                                       :status-ts (- (float-time) 3600)
                                       :updated-ts (- (float-time) 3600)
                                       :title "t" :source 'workmux :locator nil)
               edmacs-agents--table)
      (add-hook 'edmacs-agents-changed-hook
                (lambda (keys) (cl-incf fire-count) (setq fired-keys keys)))
      (edmacs-agents--sweep)
      (should-not (gethash key edmacs-agents--table))
      (should (= fire-count 1))
      (should (equal fired-keys (list key))))))

(ert-deftest edmacs-agents-test-heartbeat-reap-noop-when-fresh ()
  "A row with a fresh heartbeat survives the sweep untouched."
  (edmacs-agents-test--with-clean-state
    (let* ((edmacs-agents-stale-seconds 300)
           (root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (key (edmacs-agents--key root "%1")))
      (puthash key (make-edmacs-agent :key key :root root :instance "%1"
                                       :status 'working
                                       :status-ts (float-time) :updated-ts (float-time)
                                       :title "t" :source 'workmux :locator nil)
               edmacs-agents--table)
      (edmacs-agents--sweep)
      (should (gethash key edmacs-agents--table)))))

;; ============================================================================
;; Sweep timer actually firing (AC4's real mechanism, not just the
;; pure edmacs-agents--sweep function above)
;; ============================================================================

(defun edmacs-agents-test--wait-until (predicate timeout)
  "Pump the event loop until PREDICATE is non-nil or TIMEOUT seconds pass.
`sit-for' (not `sleep-for') is what lets a pending real timer actually
run inside `--batch'; returns PREDICATE's own final value."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (< (float-time) deadline) (not (funcall predicate)))
      (sit-for 0.05))
    (funcall predicate)))

(ert-deftest edmacs-agents-test-sweep-timer-fires ()
  "`edmacs-agents--ensure-sweep-timer' arms a REAL repeating timer that
reaps a stale row on its own, with no explicit `edmacs-agents--sweep'
call from the test -- the literal AC4 mechanism, not the pure function
in isolation. Real `run-with-timer' does fire under `--batch' with
`sit-for' pumping the loop (unlike `file-notify' on this backend, per
modules/frames-live-test.el's documented finding), so this needs no
preflight/skip."
  (edmacs-agents-test--with-clean-state
    (let* ((edmacs-agents-stale-seconds 1)
           (edmacs-agents-sweep-seconds 0.2)
           (edmacs-agents--sweep-timer nil)
           (root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (key (edmacs-agents--key root "%1")))
      (puthash key (make-edmacs-agent :key key :root root :instance "%1"
                                       :status 'waiting
                                       :status-ts (- (float-time) 3600)
                                       :updated-ts (- (float-time) 3600)
                                       :title "t" :source 'workmux :locator nil)
               edmacs-agents--table)
      (unwind-protect
          (progn
            (edmacs-agents--ensure-sweep-timer)
            (should (edmacs-agents-test--wait-until
                     (lambda () (not (gethash key edmacs-agents--table)))
                     3.0))
            (should-not (gethash key edmacs-agents--table)))
        (when (timerp edmacs-agents--sweep-timer)
          (cancel-timer edmacs-agents--sweep-timer))))))

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
           (key (edmacs-agents--key root "%1")))
      (should (edmacs-agents--apply-workmux-row
               (make-edmacs-agent :key key :root root :instance "%1"
                                   :status 'done :status-ts 100 :updated-ts 100
                                   :title "t" :source 'workmux :locator nil)))
      (should (edmacs-agent-unread (gethash key edmacs-agents--table)))
      ;; Cleared by some means other than mark-read, while status stays
      ;; `done' throughout -- e.g. a future "peek" affordance.
      (setf (edmacs-agent-unread (gethash key edmacs-agents--table)) nil)
      ;; Newer status_ts, status still `done': a refresh, not a transition.
      (should (edmacs-agents--apply-workmux-row
               (make-edmacs-agent :key key :root root :instance "%1"
                                   :status 'done :status-ts 150 :updated-ts 150
                                   :title "t" :source 'workmux :locator nil)))
      (should-not (edmacs-agent-unread (gethash key edmacs-agents--table))))))

;; ============================================================================
;; Mode-line roll-up
;; ============================================================================

(ert-deftest edmacs-agents-test-mode-line-string ()
  "The roll-up string counts working/unread-done/waiting rows, and is
empty once the table has none of those."
  (edmacs-agents-test--with-clean-state
    (let ((mk (lambda (instance status unread)
                (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
                       (key (edmacs-agents--key root instance)))
                  (puthash key (make-edmacs-agent :key key :root root :instance instance
                                                   :status status :status-ts 1 :updated-ts 1
                                                   :title "t" :source 'workmux :locator nil
                                                   :unread unread)
                           edmacs-agents--table)))))
      (funcall mk "%1" 'working nil)
      (funcall mk "%2" 'working nil)
      (funcall mk "%3" 'done t)
      (funcall mk "%4" 'done nil) ; done but already read: not counted
      (funcall mk "%5" 'waiting nil)
      (should (equal (edmacs-agents--mode-line-string-compute) "[2⟳ 1✓ 1💬]")))
    (clrhash edmacs-agents--table)
    (should (equal (edmacs-agents--mode-line-string-compute) ""))))

(provide 'agents-test)
;;; agents-test.el ends here
