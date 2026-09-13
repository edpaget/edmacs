;;; claude-agent-agents-test.el --- Tests for claude-agent-agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage of the ACP <-> agents-table adapter: instance
;; resolution against the shared key space, the event -> status map, the
;; same-status write suppression a streamed response depends on, the
;; SOURCE/LOCATOR survival premise the whole phase rests on, rename, and
;; reaping.
;;
;; agent-shell is NOT loaded.  Its `agent-shell-subscribe-to' is stubbed
;; where a subscription is needed, and every synthetic event below is the
;; plain alist that function documents its `:on-event' receiving -- so
;; this suite needs no ACP process, no Node agent and no network.  The
;; one real subprocess it does start is `sleep', for the sentinel test,
;; which cannot be faked: the whole point is that a process dying by
;; SIGNAL rather than by protocol still reaps the row.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/agents.el -l modules/claude-agent.el \
;;         -l modules/claude-agent-agents.el \
;;         -l modules/claude-agent-agents-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'subr-x)

;; See .claude/CLAUDE.md's Testing section: `cl-letf' on a C subr forces a
;; synchronous native-comp trampoline build (~28s of wall clock) the first
;; time it is hit, cached per machine so the cost reappears on a cold
;; eln-cache.  `signal-process' below is CALLED, never redirected, which
;; is the cheap path -- this guard covers the non-subr redirections here
;; and any later one that is not so careful.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defconst claude-agent-agents-test--module
  (expand-file-name "claude-agent-agents.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the module under test, sibling to this file.
Resolved at LOAD time: `load-file-name' is nil by the time ERT actually
runs a test body.")

;; ============================================================================
;; Fixtures
;; ============================================================================

(defmacro claude-agent-agents-test--with-clean-state (&rest body)
  "Run BODY against a fresh agents table and fresh adapter bookkeeping.
Mirrors `edmacs-agents-test--with-clean-state's isolation convention: no
test reads or mutates any real session state, and the sidebar redraw the
adapter fires on a reap is stubbed out (sidebar.el is not loaded here)."
  (declare (indent 0))
  `(let ((edmacs-agents--table (make-hash-table :test #'equal))
         (edmacs-agents-changed-hook nil)
         (claude-agent-agents--sessions (make-hash-table :test #'eq))
         (claude-agent-agents--watched (make-hash-table :test #'eq)))
     ,@body))

(defun claude-agent-agents-test--root ()
  "Return a fresh truename'd, slash-terminated temporary root."
  (file-name-as-directory
   (file-truename (make-temp-file "claude-agent-agents-test-root-" t))))

(defun claude-agent-agents-test--put-claude-term-row (root instance)
  "Register a synthetic `claude-term' row under ROOT/INSTANCE and return it.
Built directly rather than through claude-term-agents.el, which this
suite deliberately does not load -- the point of a mixed table here is
the SHAPE of a second source in the same key space, not that module's
own wiring."
  (let ((key (edmacs-agents--key root instance)))
    (edmacs-agents--upsert
     (make-edmacs-agent :key key :root root :instance instance
                        :status 'idle :status-ts (float-time)
                        :updated-ts (float-time) :title instance
                        :source 'claude-term :locator nil :unread nil))))

(defmacro claude-agent-agents-test--with-session (root buffer key &rest body)
  "Create an ACP row for a fresh temp BUFFER under ROOT, bind KEY, run BODY.
`agent-shell-subscribe-to' is stubbed to record the handler rather than
touching agent-shell's state, and the sentinel resolver is stubbed to a
no-op -- there is no acp client in a temp buffer, and the sentinel has
its own dedicated tests below."
  (declare (indent 3))
  `(let* ((,root (claude-agent-agents-test--root))
          (,buffer (generate-new-buffer " *claude-agent-agents-test*"))
          (claude-agent-agents-test--handler nil))
     (unwind-protect
         (cl-letf (((symbol-function 'claude-agent-agents--subscribe)
                    (lambda (buf)
                      (setq claude-agent-agents-test--handler
                            (lambda (event)
                              (claude-agent-agents--on-event buf event)))))
                   ((symbol-function 'claude-agent-agents--arm-sentinel)
                    (lambda (_buffer) nil)))
           (let ((,key (claude-agent-agents--on-create ,root ,buffer)))
             ,@body))
       (when (buffer-live-p ,buffer) (kill-buffer ,buffer)))))

(defvar claude-agent-agents-test--handler nil
  "The event handler the stubbed subscribe recorded, for feeding events.")

(defun claude-agent-agents-test--feed (event-symbol &optional data)
  "Feed EVENT-SYMBOL (with optional DATA) through the recorded handler."
  (funcall claude-agent-agents-test--handler
           (list (cons :event event-symbol) (cons :data data))))

;; ============================================================================
;; Instance resolution: one key space, two sources
;; ============================================================================

(ert-deftest claude-agent-agents-test-resolve-instance-on-empty-table ()
  "The first ACP session under a root is plain \"acp\"."
  (claude-agent-agents-test--with-clean-state
    (should (equal "acp" (claude-agent-agents--resolve-instance
                          (claude-agent-agents-test--root))))))

(ert-deftest claude-agent-agents-test-resolve-instance-skips-taken ()
  "A second, then a third ACP session mint \"acp-2\" and \"acp-3\"."
  (claude-agent-agents-test--with-clean-state
    (let ((root (claude-agent-agents-test--root)))
      (claude-agent-agents-test--put-claude-term-row root "acp")
      (should (equal "acp-2" (claude-agent-agents--resolve-instance root)))
      (claude-agent-agents-test--put-claude-term-row root "acp-2")
      (should (equal "acp-3" (claude-agent-agents--resolve-instance root))))))

(ert-deftest claude-agent-agents-test-resolve-instance-probes-the-whole-table ()
  "A CLAUDE-TERM row labelled \"acp\" pushes the ACP session to \"acp-2\".
agents.el has one key space shared by every source, so probing only ACP
rows would silently overwrite that claude-term row's entry."
  (claude-agent-agents-test--with-clean-state
    (let ((root (claude-agent-agents-test--root)))
      (let ((row (claude-agent-agents-test--put-claude-term-row root "acp")))
        (should (eq 'claude-term (edmacs-agent-source row))))
      (should (equal "acp-2" (claude-agent-agents--resolve-instance root)))
      ;; And the claude-term row is still there, untouched.
      (should (eq 'claude-term
                  (edmacs-agent-source
                   (gethash (edmacs-agents--key root "acp")
                            edmacs-agents--table)))))))

(ert-deftest claude-agent-agents-test-resolve-instance-is-per-root ()
  "\"acp\" taken under one root does not push a DIFFERENT root's session."
  (claude-agent-agents-test--with-clean-state
    (let ((a (claude-agent-agents-test--root))
          (b (claude-agent-agents-test--root)))
      (claude-agent-agents-test--put-claude-term-row a "acp")
      (should (equal "acp" (claude-agent-agents--resolve-instance b))))))

;; ============================================================================
;; Row creation
;; ============================================================================

(ert-deftest claude-agent-agents-test-on-create-builds-row ()
  "The adapter creates its own row, fully populated -- source and locator
included, which is exactly what `edmacs-agents-set-status's creation
branch would NOT have done."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (let ((row (gethash key edmacs-agents--table)))
        (should row)
        (should (equal key (edmacs-agents--key root "acp")))
        (should (eq 'claude-agent (edmacs-agent-source row)))
        (should (eq buffer (edmacs-agent-locator row)))
        (should (eq 'idle (edmacs-agent-status row)))
        (should (equal "acp" (edmacs-agent-title row)))
        (should (equal "acp" (edmacs-agent-instance row)))
        (should (equal root (edmacs-agent-root row)))
        (should-not (edmacs-agent-unread row))
        ;; And the buffer -> key indirection the rename/sentinel rely on.
        (should (equal key (gethash buffer claude-agent-agents--sessions)))))))

(ert-deftest claude-agent-agents-test-on-create-coexists-with-claude-term ()
  "An ACP row and a claude-term row live under one root at once."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents-test--put-claude-term-row root "%1")
      (should (= 2 (hash-table-count edmacs-agents--table)))
      (should (eq 'claude-agent (edmacs-agent-source
                                 (gethash key edmacs-agents--table))))
      (should (eq 'claude-term
                  (edmacs-agent-source
                   (gethash (edmacs-agents--key root "%1")
                            edmacs-agents--table)))))))

;; ============================================================================
;; Event -> status
;; ============================================================================

(ert-deftest claude-agent-agents-test-event-status-map ()
  "The whole mapping, including the events that deliberately write nothing."
  (dolist (pair '((input-submitted     . working)
                  (agent-message-chunk . working)
                  (tool-call-update    . working)
                  (permission-request  . waiting)
                  (permission-response . working)
                  (turn-complete       . done)
                  (error               . done)
                  (clean-up            . remove)))
    (should (eq (cdr pair)
                (claude-agent-agents--event-status
                 (list (cons :event (car pair)))))))
  ;; Everything else maps to nil -- no write at all. `idle' and
  ;; `session-title-changed' are decisions, not oversights; see the
  ;; module's Commentary.
  (dolist (event '(init-started init-client init-subscriptions init-handshake
                   init-authenticate init-session init-model init-session-mode
                   init-config-options session-list session-prompt
                   session-selected session-selection-cancelled init-finished
                   prompt-ready file-write config-option-update
                   session-title-changed session-restored idle))
    (should-not (claude-agent-agents--event-status
                 (list (cons :event event))))))

(ert-deftest claude-agent-agents-test-turn-drives-row ()
  "A whole turn walks the row idle -> working -> done through the feed.
Also pins the two properties that make that cheap: the streamed chunks
collapse into ONE write, and nothing shells out."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (let ((writes 0)
            (processes-before (length (process-list))))
        (advice-add 'edmacs-agents-set-status :before
                    (lambda (&rest _) (cl-incf writes))
                    '((name . claude-agent-agents-test-count)))
        (unwind-protect
            (progn
              (should (eq 'idle (edmacs-agent-status
                                 (gethash key edmacs-agents--table))))
              (claude-agent-agents-test--feed 'input-submitted)
              (should (eq 'working (edmacs-agent-status
                                    (gethash key edmacs-agents--table))))
              (claude-agent-agents-test--feed 'agent-message-chunk)
              (claude-agent-agents-test--feed 'agent-message-chunk)
              (claude-agent-agents-test--feed 'agent-message-chunk)
              (claude-agent-agents-test--feed 'tool-call-update)
              (should (eq 'working (edmacs-agent-status
                                    (gethash key edmacs-agents--table))))
              (claude-agent-agents-test--feed 'turn-complete)
              (should (eq 'done (edmacs-agent-status
                                 (gethash key edmacs-agents--table))))
              (should (edmacs-agent-unread (gethash key edmacs-agents--table)))
              ;; working once, done once -- the four redundant `working'
              ;; events in between were suppressed.
              (should (= 2 writes))
              ;; No shell hook, no subprocess: the whole path is in-process.
              (should (= processes-before (length (process-list)))))
          (advice-remove 'edmacs-agents-set-status
                         'claude-agent-agents-test-count))))))

(ert-deftest claude-agent-agents-test-unmapped-event-writes-nothing ()
  "An unmapped event leaves the row's status AND its timestamp alone."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (let* ((row (gethash key edmacs-agents--table))
             (ts (edmacs-agent-status-ts row)))
        (dolist (event '(prompt-ready idle session-title-changed file-write))
          (claude-agent-agents-test--feed event))
        (should (eq 'idle (edmacs-agent-status
                           (gethash key edmacs-agents--table))))
        (should (equal ts (edmacs-agent-status-ts
                           (gethash key edmacs-agents--table))))))))

(ert-deftest claude-agent-agents-test-apply-never-creates-a-row ()
  "A status for a key with no row is dropped, not turned into a new row.
`edmacs-agents-set-status's creation branch mints a `:source nil' row,
so a late event arriving after a reap would otherwise resurrect the
session as an unjumpable, unkillable one."
  (claude-agent-agents-test--with-clean-state
    (let ((key (edmacs-agents--key (claude-agent-agents-test--root) "acp")))
      (claude-agent-agents--apply key 'working)
      (should (zerop (hash-table-count edmacs-agents--table))))))

;; ============================================================================
;; AC6: a pending permission request shows `waiting'
;; ============================================================================

(ert-deftest claude-agent-agents-test-permission-request-sets-waiting ()
  "A pending permission request parks the row in `waiting' with UNREAD
clear, and answering it returns the row to `working'."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents-test--feed 'input-submitted)
      (claude-agent-agents-test--feed
       'permission-request '((:request-id . "r1") (:tool-call-id . "t1")))
      (let ((row (gethash key edmacs-agents--table)))
        (should (eq 'waiting (edmacs-agent-status row)))
        (should-not (edmacs-agent-unread row)))
      (claude-agent-agents-test--feed
       'permission-response '((:request-id . "r1") (:option-id . "allow")))
      (should (eq 'working (edmacs-agent-status
                            (gethash key edmacs-agents--table)))))))

(ert-deftest claude-agent-agents-test-permission-response-cancelled-clears-waiting ()
  "A CANCELLED permission response clears `waiting' too -- the row must
not sit in attention order until the turn happens to end."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents-test--feed 'permission-request)
      (claude-agent-agents-test--feed
       'permission-response '((:request-id . "r1") (:cancelled . t)))
      (should (eq 'working (edmacs-agent-status
                            (gethash key edmacs-agents--table)))))))

;; ============================================================================
;; The premise: SOURCE and LOCATOR survive a status write
;; ============================================================================

(ert-deftest claude-agent-agents-test-source-survives-set-status ()
  "`edmacs-agents-set-status' never touches SOURCE or LOCATOR.
The premise this whole phase rests on: the adapter sets them once at
creation, and every later status write goes through the existing-row
branch, which only `setf's status/status-ts/updated-ts/unread.  A change
to that branch must fail HERE rather than silently demote every ACP row
to an unjumpable one."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (dolist (status '(working waiting done working))
        (edmacs-agents-set-status root status "acp")
        (let ((row (gethash key edmacs-agents--table)))
          (should (eq status (edmacs-agent-status row)))
          (should (eq 'claude-agent (edmacs-agent-source row)))
          (should (eq buffer (edmacs-agent-locator row))))))))

(ert-deftest claude-agent-agents-test-set-status-resolves-with-a-mixed-table ()
  "With two sources under one root, a write still lands on the ACP row.
`edmacs-agents-set-status' signals when a root holds several rows and
INSTANCE is nil, which is why the adapter always passes it explicitly."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents-test--put-claude-term-row root "%1")
      (claude-agent-agents-test--feed 'input-submitted)
      (should (eq 'working (edmacs-agent-status
                            (gethash key edmacs-agents--table))))
      ;; The claude-term row beside it was not touched.
      (should (eq 'idle (edmacs-agent-status
                         (gethash (edmacs-agents--key root "%1")
                                  edmacs-agents--table))))
      ;; ... and the nil-INSTANCE ambiguity really is live here.
      (should-error (edmacs-agents-set-status root 'done nil)
                    :type 'user-error))))

;; ============================================================================
;; Reaping
;; ============================================================================

(ert-deftest claude-agent-agents-test-clean-up-reaps ()
  "agent-shell's `clean-up' event removes the row and its bookkeeping."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents-test--feed 'clean-up)
      (should-not (gethash key edmacs-agents--table))
      (should-not (gethash buffer claude-agent-agents--sessions)))))

(ert-deftest claude-agent-agents-test-reap-is-idempotent ()
  "A second reap is a silent no-op -- an ordinary buffer kill fires BOTH
`clean-up' and, moments later, the process sentinel."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents--reap key)
      (should-not (gethash key edmacs-agents--table))
      (claude-agent-agents--reap key)
      (claude-agent-agents--reap key)
      (should-not (gethash key edmacs-agents--table)))))

(ert-deftest claude-agent-agents-test-events-after-a-reap-do-nothing ()
  "A late event for a reaped session neither errors nor resurrects a row."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents-test--feed 'clean-up)
      (claude-agent-agents-test--feed 'agent-message-chunk)
      (claude-agent-agents-test--feed 'turn-complete)
      (should (zerop (hash-table-count edmacs-agents--table))))))

(ert-deftest claude-agent-agents-test-sentinel-reaps-on-signal ()
  "A process that dies by SIGNAL, emitting no protocol end event, still
has its row reaped.
The claude-repl failure this phase exists not to repeat: its sentinel
never removed the hash entry, so a crashed agent left a permanently
stuck row.  `clean-up' cannot cover this -- it runs from
`kill-buffer-hook', and the buffer here stays alive throughout.  A real
`sleep' subprocess, killed with a real signal: the only honest shape."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (let ((proc (start-process "claude-agent-agents-test" nil "sleep" "60")))
        (unwind-protect
            (progn
              (should (process-live-p proc))
              (claude-agent-agents--watch-process proc buffer)
              (should (gethash key edmacs-agents--table))
              (signal-process proc 'SIGKILL)
              (let ((deadline (+ (float-time) 5)))
                (while (and (process-live-p proc) (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              ;; Let the sentinel actually run after the status flips.
              (let ((deadline (+ (float-time) 5)))
                (while (and (gethash key edmacs-agents--table)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should (memq (process-status proc) '(exit signal)))
              (should-not (gethash key edmacs-agents--table))
              (should (buffer-live-p buffer)))
          (when (process-live-p proc) (delete-process proc)))))))

(ert-deftest claude-agent-agents-test-watch-process-arms-once ()
  "The resolver runs on every event, so double-arming must be a no-op.
Also: a dead or non-process argument arms nothing at all."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents--watch-process nil buffer)
      (should-not (gethash buffer claude-agent-agents--watched))
      (let ((proc (start-process "claude-agent-agents-test" nil "sleep" "60")))
        (unwind-protect
            (progn
              (claude-agent-agents--watch-process proc buffer)
              (should (eq proc (gethash buffer claude-agent-agents--watched)))
              ;; A second (and third) arm leaves the recorded process alone.
              (claude-agent-agents--watch-process proc buffer)
              (claude-agent-agents--watch-process proc buffer)
              (should (eq proc (gethash buffer claude-agent-agents--watched))))
          (when (process-live-p proc) (delete-process proc)))))))

(ert-deftest claude-agent-agents-test-arm-sentinel-tolerates-a-plain-buffer ()
  "The resolver must never take the event handler down.
`agent-shell--state' signals outside an `agent-shell-mode' buffer, and a
session buffer can be dead by the time a deferred handler runs."
  (claude-agent-agents-test--with-clean-state
    (let ((buffer (generate-new-buffer " *claude-agent-agents-test-plain*")))
      (unwind-protect
          (should-not (claude-agent-agents--arm-sentinel buffer))
        (kill-buffer buffer))
      ;; ... and now against the dead buffer.
      (should-not (claude-agent-agents--arm-sentinel buffer)))))

;; ============================================================================
;; Rename
;; ============================================================================

(ert-deftest claude-agent-agents-test-rename-rekeys-in-place ()
  "A rename re-keys the EXISTING row, preserving STATUS and UNREAD, and
re-points the buffer -> key indirection the event feed resolves through."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents-test--feed 'input-submitted)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "reviewer")))
        (claude-agent-agents-rename (gethash key edmacs-agents--table)))
      (let* ((new-key (edmacs-agents--key root "reviewer"))
             (row (gethash new-key edmacs-agents--table)))
        (should-not (gethash key edmacs-agents--table))
        (should row)
        (should (equal "reviewer" (edmacs-agent-title row)))
        (should (equal "reviewer" (edmacs-agent-instance row)))
        (should (eq 'working (edmacs-agent-status row)))
        (should (eq 'claude-agent (edmacs-agent-source row)))
        (should (eq buffer (edmacs-agent-locator row)))
        (should (equal new-key (gethash buffer claude-agent-agents--sessions)))
        ;; The feed follows the rename rather than writing to the old key.
        (claude-agent-agents-test--feed 'turn-complete)
        (should (eq 'done (edmacs-agent-status
                           (gethash new-key edmacs-agents--table))))
        (should (= 1 (hash-table-count edmacs-agents--table)))))))

(ert-deftest claude-agent-agents-test-rename-rejects-empty-and-collisions ()
  "An empty label and one already taken under the same root both signal,
leaving the row exactly as it was."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (claude-agent-agents-test--put-claude-term-row root "taken")
      (let ((row (gethash key edmacs-agents--table)))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
          (should-error (claude-agent-agents-rename row) :type 'user-error))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "taken")))
          (should-error (claude-agent-agents-rename row) :type 'user-error))
        (should (gethash key edmacs-agents--table))
        (should (equal "acp" (edmacs-agent-title
                              (gethash key edmacs-agents--table))))))))

(ert-deftest claude-agent-agents-test-rename-to-the-same-label-is-a-no-op ()
  "Accepting the prefilled label neither errors on itself nor re-keys."
  (claude-agent-agents-test--with-clean-state
    (claude-agent-agents-test--with-session root buffer key
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "acp")))
        (claude-agent-agents-rename (gethash key edmacs-agents--table)))
      (should (gethash key edmacs-agents--table))
      (should (= 1 (hash-table-count edmacs-agents--table))))))

;; ============================================================================
;; AC5: the header names what the shell hooks do that this cannot
;; ============================================================================

(ert-deftest claude-agent-agents-test-header-names-hook-gaps ()
  "The module's own Commentary names each unreproducible hook behaviour.
A grep-the-source test is the honest shape: the claim is documentation,
and the failure mode being guarded is someone deleting the paragraph
while the gap is still real."
  (let ((source (with-temp-buffer
                  (insert-file-contents claude-agent-agents-test--module)
                  (buffer-string))))
    (should (string-match-p "WHAT THE SHELL HOOKS DO THAT THIS CANNOT" source))
    ;; (a) the hooks are never executed at all
    (should (string-match-p "NO HOOK EXECUTION" source))
    ;; (b) sessions started outside Emacs
    (should (string-match-p "NO OUT-OF-EMACS INGRESS" source))
    ;; (c) the idle Notification, and why `idle' stays unmapped
    (should (string-match-p "NO FAITHFUL IDLE NOTIFICATION" source))
    (should (string-match-p "agent-shell-idle-timeout" source))
    ;; (d) SessionEnd vs the sentinel
    (should (string-match-p "NO SessionEnd" source))
    ;; the absent preferred API, with the pin that would invalidate it
    (should (string-match-p "agent-shell-attention" source))
    (should (string-match-p "7377ba8" source))
    ;; why the sentinel is not redundant with `clean-up'
    (should (string-match-p "SIGKILLed" source))))

(provide 'claude-agent-agents-test)
;;; claude-agent-agents-test.el ends here
