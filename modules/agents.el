;;; agents.el --- Agent state table fed by workmux -*- lexical-binding: t -*-

;;; Commentary:
;; Phase 5 of the edmacs-sidebar roadmap: one hash table answering
;; "which agent is doing what", so phase 6's sidebar section and this
;; phase's own mode-line roll-up have a single place to read from.
;;
;; `edmacs-agents-set-status' (CWD STATUS &optional INSTANCE) is the
;; documented, stable public writer -- edmacs-claude-terminal phase 5's
;; emacsclient hook and phase 9's claude-term adapter call it directly.
;; Internally, both it and the workmux adapter below terminate in
;; `edmacs-agents--upsert', the one function that actually mutates the
;; table and fires `edmacs-agents-changed-hook' -- that hook fires
;; exactly once per external call, never once per struct field touched.
;; The unread/done semantics themselves ("done is UNREAD until visited")
;; have exactly one definition, `edmacs-agents--compute-unread', called
;; from every place a row's status changes -- `edmacs-agents-set-status',
;; the workmux adapter's `edmacs-agents--apply-workmux-row', and a fresh
;; row's own `edmacs-agents--row-from-workmux-json' -- so the rule cannot
;; drift between call sites.
;;
;; The workmux adapter reads `edmacs-agents-workmux-dir'
;; (`~/.local/state/workmux/agents/*.json', one file per tmux pane) at
;; load, then tracks changes via `file-notify' -- never polling. Two
;; timestamps matter and must not be confused: `status_ts' is when the
;; pane's status last CHANGED (the ordering guard on file-notify updates
;; compares against this), while `updated_ts' is a heartbeat that
;; advances every few seconds while the pane lives, whether or not its
;; status changed. workmux never deletes a pane's JSON file when the
;; pane dies (`edmacs-agents-stale-seconds' seen on this machine: 13 of
;; 17 files were dead-pane orphans from earlier boots), so
;; `edmacs-agents--sweep', keyed off `updated_ts', is the PRIMARY reap
;; path, not a backstop -- a file-delete event is handled too, but
;; workmux practically never produces one.
;;
;; The sweep only ever reaps rows whose SOURCE is `workmux': that is
;; the one adapter that independently rewrites `updated_ts' on a
;; heartbeat cadence whether or not status changed. A row created
;; through the public `edmacs-agents-set-status' writer has no such
;; heartbeat -- its `updated-ts' is stamped once, by that call, and
;; never again until another explicit call -- so sweeping it on
;; heartbeat age would silently discard a still-live `waiting' or
;; unread `done' row well before `edmacs-agents-stale-seconds' means
;; anything for it. A future adapter only gets sweep protection by
;; actually re-heartbeating through `edmacs-agents-set-status' on its
;; own cadence, not merely by existing in the table.
;;
;; Load-time order is scan-then-sweep-then-watch/render, strictly:
;; `edmacs-agents-init' populates the table from every JSON file on
;; disk, THEN runs the sweep once to drop already-stale rows, and only
;; THEN arms the file-notify watch, the sweep timer, and the
;; mode-line construct. Anything reading the table before `edmacs-agents-init'
;; returns (an early redisplay, a stray file-notify event) would
;; otherwise see the raw unswept scan instead of "exactly the alive
;; rows" -- see the roadmap's own plan-review finding on this ordering.
;;
;; `workmux status --json' is scoped to the current project (it misses
;; live agents in other repos) and is therefore at most an optional,
;; secondary corroborating check the sweep could shell out for -- this
;; phase does not implement it; `updated_ts' age alone is the liveness
;; signal in production per the roadmap body. Verified against a real,
;; freshly-created workmux pane on this machine (a throwaway `workmux
;; add' worktree running the real `claude' binary): the shared JSON
;; state file it produced was, moments after the agent replied, kept
;; by `edmacs-agents--scan-workmux-dir' + `edmacs-agents--sweep' as the
;; sole survivor against this machine's other 25 real (all
;; hours-to-months stale) workmux files -- exactly AC1's "alive rows
;; only" claim, on real, non-synthetic data. `workmux status --json'
;; itself reported no agents throughout that pane's real `working' and
;; `done' states, for that project and every other -- a limitation of
;; this machine's `workmux' status query, not of this table -- so the
;; "matches workmux status" half of AC1 could not be exercised as a
;; literal byte-for-byte comparison here even with a genuinely live
;; pane in hand; `updated_ts' heartbeat age remains the actual
;; liveness signal this module relies on.
;;
;; Run pure-function tests:
;;   emacs -Q --batch -l ert -l modules/agents.el -l modules/agents-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'filenotify)
(require 'tabulated-list)

;; ============================================================================
;; Struct and table
;; ============================================================================

(cl-defstruct edmacs-agent
  "One tracked coding agent.
KEY is `(edmacs-agents--key ROOT INSTANCE)', the hash-table key -- kept
as a struct field too so a row pulled out of the table (e.g. during a
sweep or a tabulated-list refresh) carries its own removal/lookup key
without recomputing it. ROOT is `file-truename'-resolved so a
symlinked path to the same worktree does not register as a second row
-- the same convention `claude-term-registry--key' uses, and the same
key SHAPE (truename-root . instance) edmacs-claude-terminal phase 4's
session registry uses, so a future correlation between the two tables
can key off it directly. STATUS is one of `working', `waiting', `done'
or `idle' (`idle' only ever appears after `edmacs-agents-mark-read').
STATUS-TS is the pane's last status-change time (workmux's
`status_ts'); UPDATED-TS is its heartbeat (workmux's `updated_ts'),
the sweep's staleness clock -- see this file's Commentary for why the
two are not interchangeable. TITLE is a human-readable label
(workmux's `pane_title', spinner glyphs stripped). SOURCE is
`workmux', `claude-term' or `claude-repl'. LOCATOR is adapter-specific:
a workmux row's `locator' is a plist of `:pane-id', `:session' and
`:window'; a claude-term row's (phase 9) is its buffer. UNREAD is set
on a transition into `done' and cleared by `edmacs-agents-mark-read'."
  key root instance status status-ts updated-ts title source locator unread)

(defvar edmacs-agents--table (make-hash-table :test #'equal)
  "Hash table of every tracked `edmacs-agent', keyed by `edmacs-agents--key'.")

(defvar edmacs-agents-changed-hook nil
  "Hook run after any mutation to `edmacs-agents--table'.
Called with one argument, the list of affected keys -- a single-row
update passes a one-element list; `edmacs-agents--sweep' passes every
key it removed in one call, so a listener recomputing something
table-wide (the mode-line roll-up) still only recomputes once per
mutating operation, not once per removed row.")

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
(`edmacs-agents-set-status', `edmacs-agents-mark-read', the workmux
adapter) funnels through this, so the hook never fires more than once
per external call."
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
`waiting' clears it \(this doubles as the workmux \"visited in tmux\"
signal\); any other transition -- including a `done'-to-`done'
timestamp-only refresh -- preserves OLD-UNREAD unchanged. OLD-STATUS is
nil for a fresh row with no prior state, which behaves like any other
non-`done' predecessor: fresh-into-`done' sets UNREAD, fresh-into-anything-else
does not. Every writer -- `edmacs-agents-set-status', the workmux
adapter's `edmacs-agents--apply-workmux-row', and a brand new row's own
`edmacs-agents--row-from-workmux-json' -- calls this instead of
re-encoding the rule."
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
row (already gone, e.g. reaped by a race with the sweep).

Also bumps STATUS-TS to now. Without this, a workmux row marked read
locally stays `done' on disk with its OLD `status_ts' (workmux only
advances `status_ts' on a genuine status change, not on its frequent
`updated_ts' heartbeat) -- the very next heartbeat-only file-notify
event for that file would then carry an equal-or-older `status_ts',
which `edmacs-agents--apply-workmux-row's ordering guard treats as
\"fresh enough to apply\" (only a STRICTLY older `status_ts' is
rejected), clobbering the local `idle' back to `done' and, because
`edmacs-agents--compute-unread' sees a non-`done' -> `done' transition,
re-setting UNREAD right back. Stamping STATUS-TS to now makes that
next on-disk re-apply strictly older, so the ordering guard rejects it
instead of reverting the just-cleared read."
  (when-let* ((row (gethash key edmacs-agents--table)))
    (setf (edmacs-agent-unread row) nil
          (edmacs-agent-status row) 'idle
          (edmacs-agent-status-ts row) (float-time))
    (edmacs-agents--upsert row)))

;; ============================================================================
;; workmux adapter
;; ============================================================================

(defvar edmacs-agents-workmux-dir "~/.local/state/workmux/agents/"
  "Directory of workmux's per-pane JSON state files.")

(defvar edmacs-agents-stale-seconds 300
  "A row is swept once its heartbeat (`updated_ts') is older than this.
A plain `defvar', not `defcustom', so an acceptance run can `let'-bind
it to a small value.")

(defvar edmacs-agents-sweep-seconds 60
  "How often the recurring sweep timer runs, in seconds.
A plain `defvar' for the same reason as `edmacs-agents-stale-seconds'.")

(defvar edmacs-agents--workmux-path->key (make-hash-table :test #'equal)
  "Workmux JSON file path -> the table key it was last ingested as.
Lets a file-delete event (or the sweep) find and drop the matching row
without scanning the whole table for a path match.")

(defconst edmacs-agents--spinner-regexp
  "\\`[\x2800-\x28ff\x2596-\x259f]+[ \t]*"
  "Leading spinner glyphs (Braille-pattern and block-element frames) plus
the separating space, stripped from a workmux `pane_title'. A character
class rather than one hardcoded glyph, since the spinner cycles through
many frames of the same block and any one of them can be the value
snapshotted into the JSON file at read time.")

(defun edmacs-agents--strip-spinner (title)
  "Return TITLE with any leading spinner glyph run removed."
  (if (stringp title)
      (replace-regexp-in-string edmacs-agents--spinner-regexp "" title)
    title))

(defun edmacs-agents--intern-workmux-status (str)
  "Return STR (a workmux status string) as one of the symbols
`working'/`waiting'/`done', or nil if STR is not one of those three."
  (and (stringp str)
       (let ((sym (intern (downcase str))))
         (and (memq sym '(working waiting done)) sym))))

(defun edmacs-agents--parse-workmux-file (path)
  "Return PATH's contents as a parsed alist, or nil on any failure.
Covers both a malformed file and one that has vanished or is mid-write
(workmux writes these non-atomically) -- either way this is a skip,
never a signal, both from the initial directory scan and from a
file-notify callback."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (json-parse-buffer :object-type 'alist :array-type 'list))
    (error nil)))

(defun edmacs-agents--row-from-workmux-json (json)
  "Build an `edmacs-agent' from a parsed workmux JSON alist, or nil.
Returns nil when a required field is missing or `status' is not one of
the three values workmux emits -- treated the same as malformed JSON
by every caller."
  (when-let* ((workdir (alist-get 'workdir json))
              (status (edmacs-agents--intern-workmux-status (alist-get 'status json)))
              (pane-key (alist-get 'pane_key json))
              (pane-id (alist-get 'pane_id pane-key)))
    (let* ((root (condition-case nil (file-truename workdir) (error workdir)))
           (key (edmacs-agents--key root pane-id)))
      (make-edmacs-agent
       :key key :root root :instance pane-id
       :status status
       :status-ts (alist-get 'status_ts json)
       :updated-ts (alist-get 'updated_ts json)
       :title (edmacs-agents--strip-spinner (alist-get 'pane_title json))
       :source 'workmux
       :locator (list :pane-id pane-id
                       :session (alist-get 'session_name json)
                       :window (alist-get 'window_name json))
       ;; A fresh row has no prior state -- OLD-STATUS/OLD-UNREAD are nil;
       ;; `edmacs-agents--apply-workmux-row' recomputes this against the
       ;; same shared rule once an existing row is in play.
       :unread (edmacs-agents--compute-unread nil status nil)))))

(defun edmacs-agents--apply-workmux-row (new-row)
  "Apply NEW-ROW to the table, honoring `status_ts' ordering.
Ignored (nil, no table change, no hook fire) when a row already exists
under NEW-ROW's key with a strictly newer `status_ts' -- a stale
update, e.g. from a burst of file-notify events landing out of order.
An exactly-equal `status_ts' is treated as fresh enough to apply, since
workmux's own write is otherwise indistinguishable from a no-op
refresh. Otherwise recomputes `unread' via `edmacs-agents--compute-unread',
the single shared definition of that rule, then delegates to
`edmacs-agents--upsert'. Returns the applied key, or nil if ignored as
stale."
  (let* ((key (edmacs-agent-key new-row))
         (existing (gethash key edmacs-agents--table))
         (new-status (edmacs-agent-status new-row))
         (new-ts (edmacs-agent-status-ts new-row)))
    (if (and existing
              (numberp new-ts)
              (numberp (edmacs-agent-status-ts existing))
              (< new-ts (edmacs-agent-status-ts existing)))
        nil
      (setf (edmacs-agent-unread new-row)
            (edmacs-agents--compute-unread
             (and existing (edmacs-agent-status existing))
             new-status
             (and existing (edmacs-agent-unread existing))))
      (edmacs-agents--upsert new-row)
      key)))

(defun edmacs-agents--ingest-workmux-file (path)
  "Parse PATH and apply the resulting row, recording PATH -> key for later removal.
A no-op if PATH fails to parse or does not describe a well-formed row."
  (when-let* ((json (edmacs-agents--parse-workmux-file path))
              (row (edmacs-agents--row-from-workmux-json json)))
    (puthash path (edmacs-agent-key row) edmacs-agents--workmux-path->key)
    (edmacs-agents--apply-workmux-row row)))

(defun edmacs-agents--forget-workmux-file (path)
  "Drop the row PATH was last ingested as, if any."
  (when-let* ((key (gethash path edmacs-agents--workmux-path->key)))
    (remhash path edmacs-agents--workmux-path->key)
    (edmacs-agents--remove key)))

(defun edmacs-agents--scan-workmux-dir ()
  "Ingest every `*.json' file in `edmacs-agents-workmux-dir'.
A no-op, not an error, when the directory does not exist yet (a
machine that has never run workmux)."
  (let ((dir (expand-file-name edmacs-agents-workmux-dir)))
    (when (file-directory-p dir)
      (dolist (path (directory-files dir t "\\.json\\'"))
        (edmacs-agents--ingest-workmux-file path)))))

;; ============================================================================
;; file-notify watch
;; ============================================================================

(defvar edmacs-agents--workmux-watch nil
  "file-notify descriptor watching the workmux state dir (or its parent,
until that directory exists -- see `edmacs-agents--ensure-workmux-watch').")

(defun edmacs-agents--workmux-watch-callback (event)
  "Handle a file-notify EVENT on the workmux state directory itself."
  (condition-case err
      (pcase-let ((`(,_desc ,action ,file) event))
        (cond
         ((and (memq action '(created changed))
               (string-suffix-p ".json" file))
          (edmacs-agents--ingest-workmux-file file))
         ((memq action '(deleted renamed))
          (edmacs-agents--forget-workmux-file file))))
    (error (message "edmacs-agents: workmux watch callback error: %s" err))))

(defun edmacs-agents--upgrade-workmux-watch ()
  "Replace the parent-dir watch (if any) with a direct watch on the state dir."
  (when edmacs-agents--workmux-watch
    (ignore-errors (file-notify-rm-watch edmacs-agents--workmux-watch)))
  (setq edmacs-agents--workmux-watch
        (file-notify-add-watch
         (directory-file-name (expand-file-name edmacs-agents-workmux-dir))
         '(change) #'edmacs-agents--workmux-watch-callback)))

(defun edmacs-agents--parent-watch-callback (event)
  "Handle EVENT on the workmux state dir's parent, before it exists.
Upgrades to a direct watch and runs one scan-then-sweep pass the
moment the `agents' directory itself appears."
  (condition-case err
      (pcase-let ((`(,_desc ,action ,file) event))
        (when (and (memq action '(created changed))
                   (equal (file-name-nondirectory (directory-file-name file))
                          (file-name-nondirectory
                           (directory-file-name edmacs-agents-workmux-dir))))
          (edmacs-agents--upgrade-workmux-watch)
          (edmacs-agents--scan-workmux-dir)
          (edmacs-agents--sweep)))
    (error (message "edmacs-agents: workmux parent-watch callback error: %s" err))))

(defun edmacs-agents--ensure-workmux-watch ()
  "Arm the workmux file-notify watch, direct if the dir exists already.
No-op if a watch is already armed. Never signals: a backend that
refuses to watch (or a directory whose own parent is also missing)
just leaves this repo without live updates until the next Emacs
restart, rather than breaking module load."
  (unless edmacs-agents--workmux-watch
    (condition-case err
        (let ((dir (directory-file-name (expand-file-name edmacs-agents-workmux-dir))))
          (if (file-directory-p dir)
              (edmacs-agents--upgrade-workmux-watch)
            (setq edmacs-agents--workmux-watch
                  (file-notify-add-watch (file-name-directory dir) '(change)
                                          #'edmacs-agents--parent-watch-callback))))
      (file-notify-error
       (message "edmacs-agents: could not watch workmux state dir: %s" err)))))

;; ============================================================================
;; Stale sweep
;; ============================================================================

(defun edmacs-agents--sweep ()
  "Remove every `workmux'-sourced row whose heartbeat (`updated-ts') is
older than `edmacs-agents-stale-seconds'. The PRIMARY reap path for
that source -- see this file's Commentary on why workmux's own
dead-pane files cannot be trusted to disappear.

Scoped to SOURCE `workmux' only: that adapter alone rewrites
`updated-ts' every few seconds regardless of whether status changed,
which is what makes heartbeat age a liveness signal in the first
place. A row created through the public `edmacs-agents-set-status'
API (or, later, any adapter that does not re-heartbeat on its own) has
its `updated-ts' stamped exactly once per explicit call and would
otherwise be silently reaped mid-`waiting' or mid-unread-`done' just
for sitting quiet past the threshold -- contradicting \"waiting is the
only state that blocks on the user\" and \"done is UNREAD until
visited\". Sweeping only rows with a genuine heartbeat contract avoids
that; a future adapter that wants sweep protection has to actually
heartbeat, not merely exist.

Assumes the table is already populated; never triggers a scan itself.
Fires `edmacs-agents-changed-hook' once with every removed key, not
once per row."
  (let ((now (float-time)) stale-keys)
    (maphash
     (lambda (key row)
       (when (and (eq (edmacs-agent-source row) 'workmux)
                  (numberp (edmacs-agent-updated-ts row))
                  (> (- now (edmacs-agent-updated-ts row)) edmacs-agents-stale-seconds))
         (push key stale-keys)))
     edmacs-agents--table)
    (when stale-keys
      (dolist (key stale-keys)
        (remhash key edmacs-agents--table))
      (let (dead-paths)
        (maphash (lambda (path key)
                   (when (member key stale-keys) (push path dead-paths)))
                 edmacs-agents--workmux-path->key)
        (dolist (path dead-paths)
          (remhash path edmacs-agents--workmux-path->key)))
      (run-hook-with-args 'edmacs-agents-changed-hook stale-keys))))

(defun edmacs-agents--sweep-safe ()
  "Call `edmacs-agents--sweep', catching any error so the repeating timer
never dies silently on one bad row."
  (condition-case err
      (edmacs-agents--sweep)
    (error (message "edmacs-agents: sweep failed: %s" err))))

(defvar edmacs-agents--sweep-timer nil
  "The repeating sweep timer, or nil before `edmacs-agents--ensure-sweep-timer'.")

(defun edmacs-agents--ensure-sweep-timer ()
  "Arm the recurring sweep timer if not already armed.
`run-with-timer' rather than `run-with-idle-timer': the sweep must
still fire under continuous interactive use (typing, a busy agent
pane), not only once Emacs falls idle."
  (unless edmacs-agents--sweep-timer
    (setq edmacs-agents--sweep-timer
          (run-with-timer edmacs-agents-sweep-seconds edmacs-agents-sweep-seconds
                           #'edmacs-agents--sweep-safe))))

;; ============================================================================
;; Mode-line roll-up
;; ============================================================================

(defvar edmacs-agents--mode-line-string ""
  "Cached mode-line roll-up string, recomputed only from
`edmacs-agents-changed-hook' -- never a `:eval' form re-run on every
redisplay.")

(defun edmacs-agents--mode-line-string-compute ()
  "Return the `[Nwork Nunread-done Nwaiting]' roll-up string, or \"\" if empty."
  (let ((working 0) (unread-done 0) (waiting 0))
    (maphash
     (lambda (_key row)
       (pcase (edmacs-agent-status row)
         ('working (cl-incf working))
         ('waiting (cl-incf waiting))
         ('done (when (edmacs-agent-unread row) (cl-incf unread-done)))))
     edmacs-agents--table)
    (if (zerop (+ working unread-done waiting))
        ""
      (format "[%s]"
              (string-join
               (delq nil
                     (list (and (> working 0) (format "%d⟳" working))
                           (and (> unread-done 0) (format "%d✓" unread-done))
                           (and (> waiting 0) (format "%d💬" waiting))))
               " ")))))

(defun edmacs-agents--refresh-mode-line (&rest _keys)
  "Recompute and cache the mode-line roll-up string.
Wired onto `edmacs-agents-changed-hook'; ignores the hook's KEYS
argument since every recompute walks the whole table regardless."
  (setq edmacs-agents--mode-line-string (edmacs-agents--mode-line-string-compute)))

(defun edmacs-agents--ensure-mode-line ()
  "Splice the cached roll-up string into `global-mode-string' once.
Checks membership in the actual list (not a separate flag) so
reloading this file during interactive development never inserts a
second copy."
  (edmacs-agents--refresh-mode-line)
  (add-hook 'edmacs-agents-changed-hook #'edmacs-agents--refresh-mode-line)
  (unless (memq 'edmacs-agents--mode-line-string global-mode-string)
    (setq global-mode-string
          (append (or global-mode-string '(""))
                  (list 'edmacs-agents--mode-line-string)))))

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

;; ============================================================================
;; Load-time setup: scan, THEN sweep, THEN watch/timer/mode-line
;; ============================================================================

(defun edmacs-agents-init ()
  "Idempotent one-time setup, in the strict order this file's Commentary
documents: populate the table from every workmux JSON file, remove
whatever is already stale, and only then arm the file-notify watch,
the sweep timer and the mode-line construct -- so the first thing any
of those three touch is post-sweep data, never the raw scan.

Deliberately NOT called at this file's own top level: init.el calls it
once, right after `(load-module \"agents\")', so that merely loading
this file (every ERT run, and `M-x eval-buffer' during development)
never reaches into the real workmux state directory or arms a real
timer/watch as a side effect of loading."
  (edmacs-agents--scan-workmux-dir)
  (edmacs-agents--sweep)
  (edmacs-agents--ensure-workmux-watch)
  (edmacs-agents--ensure-sweep-timer)
  (edmacs-agents--ensure-mode-line))

(provide 'agents)
;;; agents.el ends here
