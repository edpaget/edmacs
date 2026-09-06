;;; agents.el --- Agent state table for tracked coding agents -*- lexical-binding: t -*-

;;; Commentary:
;; Phase 5 of the edmacs-sidebar roadmap: one hash table answering
;; "which agent is doing what", so phase 6's sidebar section (and, per
;; buffer, claude-term-agents.el's own mode-line segment) have a single
;; place to read from. This file owns no mode-line concern of its own --
;; it is pure data plus the tabulated-list view below.
;;
;; `edmacs-agents-set-status' (CWD STATUS &optional INSTANCE) is the
;; documented, stable public writer -- edmacs-claude-terminal phase 5's
;; emacsclient hook and phase 9's claude-term adapter call it directly.
;; It terminates in `edmacs-agents--upsert', the one function that
;; actually mutates the table and fires `edmacs-agents-changed-hook' --
;; that hook fires exactly once per external call, never once per
;; struct field touched. The unread/done semantics themselves ("done is
;; UNREAD until visited") have exactly one definition,
;; `edmacs-agents--compute-unread', called from every place a row's
;; status changes, so the rule cannot drift between call sites.
;;
;; Rows are created and removed entirely by the claude-term registry
;; hooks, via `edmacs-agents-set-status' -- there is no scan of any
;; external state directory and nothing polls or watches the
;; filesystem. A row appears when a real claude-term process starts and
;; is removed (STATUS `remove') when that process's SessionEnd hook
;; fires, so the table's contents track real process lifecycle
;; directly; there is no separate age-based reap of rows that sit idle.
;;
;; Run pure-function tests:
;;   emacs -Q --batch -l ert -l modules/agents.el -l modules/agents-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)

;; ============================================================================
;; Struct and table
;; ============================================================================

(cl-defstruct edmacs-agent
  "One tracked coding agent.
KEY is `(edmacs-agents--key ROOT INSTANCE)', the hash-table key -- kept
as a struct field too so a row pulled out of the table (e.g. during a
tabulated-list refresh) carries its own removal/lookup key without
recomputing it. ROOT is `file-truename'-resolved so a
symlinked path to the same worktree does not register as a second row
-- the same convention `claude-term-registry--key' uses, and the same
key SHAPE (truename-root . instance) edmacs-claude-terminal phase 4's
session registry uses, so a future correlation between the two tables
can key off it directly. STATUS is one of `working', `waiting', `done'
or `idle' (`idle' only ever appears after `edmacs-agents-mark-read').
STATUS-TS is the row's last status-change time; UPDATED-TS is its most
recent write of any kind. TITLE is a human-readable label. SOURCE is
`claude-term' or `claude-repl'. LOCATOR is adapter-specific: a
claude-term row's (phase 9) is its buffer. UNREAD is set on a
transition into `done' and cleared by `edmacs-agents-mark-read'."
  key root instance status status-ts updated-ts title source locator unread)

(defvar edmacs-agents--table (make-hash-table :test #'equal)
  "Hash table of every tracked `edmacs-agent', keyed by `edmacs-agents--key'.")

(defvar edmacs-agents-changed-hook nil
  "Hook run after any mutation to `edmacs-agents--table'.
Called with one argument, the list of affected keys -- a single-row
update passes a one-element list, so a listener recomputing something
table-wide (e.g. the sidebar's roll-up, or a repaint of a per-buffer
segment) still only recomputes once per mutating operation.")

(defun edmacs-agents--key (root instance)
  "Return the table key for project ROOT and agent INSTANCE.
`file-truename' normalizes ROOT so two different-looking paths to the
same worktree collapse to one row; wrapped in `condition-case' because
ROOT may name a worktree already deleted out from under us (a killed
`rdm worktree'), and `file-truename' on such a path is not guaranteed
to behave -- falling back to the raw ROOT string keeps the row instead
of erroring the whole scan or update over one dead path."
  (cons (condition-case nil (file-truename root) (error root)) instance))

(defun edmacs-agents--rows-for-root (root)
  "Return every row in the table whose root is ROOT (already truename'd)."
  (let (acc)
    (maphash (lambda (key row) (when (equal (car key) root) (push row acc)))
              edmacs-agents--table)
    acc))

(defun edmacs-agents--upsert (row)
  "Store ROW under its own key and fire the changed hook once for it.
The single place in this file that both mutates `edmacs-agents--table'
for a live row and fires `edmacs-agents-changed-hook' -- every writer
(`edmacs-agents-set-status', `edmacs-agents-mark-read') funnels
through this, so the hook never fires more than once per external
call."
  (puthash (edmacs-agent-key row) row edmacs-agents--table)
  (run-hook-with-args 'edmacs-agents-changed-hook (list (edmacs-agent-key row)))
  row)

(defun edmacs-agents--remove (key)
  "Remove KEY from the table and fire the changed hook once for it."
  (remhash key edmacs-agents--table)
  (run-hook-with-args 'edmacs-agents-changed-hook (list key)))

(defun edmacs-agents--compute-unread (old-status new-status old-unread)
  "Return the UNREAD value for a transition from OLD-STATUS to NEW-STATUS.
The single, shared definition of this table's central semantic rule
\(see this file's Commentary and the phase body it implements\): a
transition into `done' sets UNREAD; a transition into `working' or
`waiting' clears it \(this doubles as the \"visited\" signal\); any
other transition -- including a `done'-to-`done' timestamp-only
refresh -- preserves OLD-UNREAD unchanged. OLD-STATUS is nil for a
fresh row with no prior state, which behaves like any other non-`done'
predecessor: fresh-into-`done' sets UNREAD, fresh-into-anything-else
does not. Every writer -- currently only `edmacs-agents-set-status' --
calls this instead of re-encoding the rule; `edmacs-agents--upsert'
itself performs no such computation and trusts its caller to have done
so."
  (cond
   ((memq new-status '(working waiting)) nil)
   ((and (eq new-status 'done) (not (eq old-status 'done))) t)
   (t old-unread)))

;; ============================================================================
;; Single writer API
;; ============================================================================

(defun edmacs-agents-set-status (cwd status &optional instance)
  "Set the agent under CWD's root to STATUS, creating a row if needed.
CWD is `file-truename'-resolved to the root. INSTANCE nil means: the
only row already under that root, if exactly one exists; a fresh row
with a default instance if none exist; or a `user-error' if more than
one already exists, rather than guessing which one the caller means.

STATUS `remove' is special-cased: rather than upserting a row with
that bogus status value, the row resolved by the same CWD/INSTANCE
rule above is deleted via `edmacs-agents--remove' -- a no-op if no
such row exists -- and no row is created or updated. This is the
entry point the ported claude-terminal SessionEnd hook (and, for its
own in-Emacs rows, edmacs-sidebar phase 9's claude-term adapter,
redundantly) calls to reap a session.

UNREAD is recomputed by `edmacs-agents--compute-unread', the single
shared definition of that rule."
  (let* ((root (condition-case nil (file-truename cwd) (error cwd)))
         (matches (edmacs-agents--rows-for-root root))
         (resolved-instance
          (or instance
              (cond
               ((null matches) "default")
               ;; The sole row's own KEY, not its INSTANCE field, is the
               ;; authority for what it is actually stored under: a
               ;; claude-term row's INSTANCE can be raw/nil while its KEY
               ;; is normalized to the display label (see
               ;; claude-term-agents.el's `claude-term-agents--on-create'),
               ;; so deriving from INSTANCE here could look up a key this
               ;; row was never stored under and spawn a duplicate.
               ((null (cdr matches)) (cdr (edmacs-agent-key (car matches))))
               (t (user-error
                   "edmacs-agents: multiple agents under %s; specify INSTANCE" root)))))
         (key (edmacs-agents--key root resolved-instance)))
    (if (eq status 'remove)
        (edmacs-agents--remove key)
      (let* ((existing (gethash key edmacs-agents--table))
             (old-status (and existing (edmacs-agent-status existing)))
             (now (float-time))
             (unread (edmacs-agents--compute-unread
                      old-status status (and existing (edmacs-agent-unread existing))))
             (row (if existing
                      (progn
                        (setf (edmacs-agent-status existing) status
                              (edmacs-agent-status-ts existing) now
                              (edmacs-agent-updated-ts existing) now
                              (edmacs-agent-unread existing) unread)
                        existing)
                    (make-edmacs-agent :key key :root root :instance resolved-instance
                                        :status status :status-ts now :updated-ts now
                                        :title resolved-instance :source nil
                                        :locator nil :unread unread))))
        (edmacs-agents--upsert row)))))

(defun edmacs-agents-mark-read (key)
  "Clear KEY's unread flag and move it to `idle'.
The generic \"visited\" entry point for any source; phase 6 calls this
when the user jumps to a row from the sidebar. A no-op if KEY names no
row (already gone, e.g. the process it tracked has exited).

Also bumps STATUS-TS to now, so a later write carrying an
equal-or-older STATUS-TS is not mistaken for newer than this read."
  (when-let* ((row (gethash key edmacs-agents--table)))
    (setf (edmacs-agent-unread row) nil
          (edmacs-agent-status row) 'idle
          (edmacs-agent-status-ts row) (float-time))
    (edmacs-agents--upsert row)))

;; ============================================================================
;; Tabulated-list view
;; ============================================================================

(defun edmacs-agents--list-entries ()
  "Return `tabulated-list-entries' for every tracked agent."
  (let (entries)
    (maphash
     (lambda (key row)
       (push (list key
                   (vector (format "%s" (cdr key))
                           (or (edmacs-agent-root row) "")
                           (format "%s" (or (edmacs-agent-instance row) ""))
                           (format "%s%s" (edmacs-agent-status row)
                                   (if (edmacs-agent-unread row) "*" ""))
                           (or (edmacs-agent-title row) "")))
             entries))
     edmacs-agents--table)
    (nreverse entries)))

(defun edmacs-agents--revert-list (&rest _)
  "`revert-buffer-function' for `edmacs-agents-list-mode'."
  (setq tabulated-list-entries (edmacs-agents--list-entries))
  (tabulated-list-print t))

(define-derived-mode edmacs-agents-list-mode tabulated-list-mode "Edmacs-Agents"
  "Major mode listing every tracked agent. Read-only; `g' refreshes.
A temporary view for this phase -- phase 6 replaces it with the
sidebar's own ALL AGENTS section."
  (setq tabulated-list-format
        [("Instance" 10 t) ("Root" 34 t) ("Pane" 8 t) ("Status" 12 t) ("Title" 24 t)])
  (setq tabulated-list-padding 2)
  (setq revert-buffer-function #'edmacs-agents--revert-list)
  (tabulated-list-init-header))

;;;###autoload
(defun edmacs-agents-list ()
  "Display every tracked agent in a read-only tabulated listing."
  (interactive)
  (let ((buffer (get-buffer-create "*edmacs-agents*")))
    (with-current-buffer buffer
      (edmacs-agents-list-mode)
      (edmacs-agents--revert-list))
    (pop-to-buffer buffer)))

(provide 'agents)
;;; agents.el ends here
