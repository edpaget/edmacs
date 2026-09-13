;;; claude-agent-agents.el --- ACP adapter for the shared agent table -*- lexical-binding: t -*-

;;; Commentary:
;; Phase 3 of the edmacs-claude-acp roadmap: mirrors an ACP session
;; (modules/claude-agent.el, agent-shell + acp.el) into the shared
;; `edmacs-agents--table' (agents.el) as a `claude-agent'-sourced row, so
;; an ACP session shows up in the sidebar's per-worktree and ALL AGENTS
;; lists, participates in `SPC a TAB' attention cycling, and answers the
;; source-agnostic jump/rename/kill verbs -- alongside claude-term rows,
;; in one list.  The structural mirror of claude-term-agents.el, for the
;; table's second SOURCE.
;;
;; ROW CREATION: the adapter creates its own rows.  Exactly
;; claude-term-agents.el's precedent -- `claude-term-agents--on-create'
;; builds a full `make-edmacs-agent' and calls `edmacs-agents--upsert'
;; directly -- and deliberately NOT through `edmacs-agents-set-status',
;; whose creation branch hardcodes `:source nil :locator nil' and would
;; therefore mint a second-class row with no jump target.  Fixing that
;; branch is task `agents-set-status-drops-source', which is open and
;; unlanded; its fix rewrites the same three `pcase' dispatch sites in
;; sidebar-agents.el this phase touches, so nothing here assumes it.
;;
;; The seam is `claude-agent-session-create-functions' (claude-agent.el),
;; fired with (ROOT BUFFER) at the end of `claude-agent--start-at'.
;;
;; STATUS DERIVATION.  agent-shell's `agent-shell-subscribe-to' event
;; feed, in-process, with no shell command anywhere on the path: no
;; `emacs-status.sh', no `claude/settings.json' hook, one Elisp call into
;; `edmacs-agents-set-status' per real transition.  The chain:
;;
;;   `agent-message-chunk', `tool-call-update' and `turn-complete' are
;;   agent-shell's own decoding of the agent's `session/update'
;;   notifications, so "derived from session/update" is exact for
;;   `working' and `done'.  `permission-request' is NOT a
;;   `session/update' -- it is agent-shell surfacing an incoming
;;   `session/request_permission' JSON-RPC *request*, the other half of
;;   the ACP channel.  That is where `waiting' comes from, and it is
;;   genuinely new for this source: the retired claude-repl had no
;;   waiting signal at all, and a claude-term row only reaches `waiting'
;;   through the CLI's Notification shell hook.
;;
;; `claude-agent-agents--event-status' is the whole mapping, pure and
;; table-driven.  Everything not named there -- the entire `init-*'
;; family, `session-list', `prompt-ready', `file-write',
;; `config-option-update', `session-title-changed', `session-restored',
;; `idle' -- maps to nil and performs NO write.  Two of those omissions
;; are decisions, not oversights:
;;
;;   `idle' is unmapped because agent-shell fires it purely on
;;   `agent-shell-idle-timeout' elapsing.  Mapping it would flip a
;;   settled, unread `done' row back out of `done', destroying the unread
;;   and attention semantics `edmacs-agents--compute-unread' exists to
;;   protect.
;;
;;   `session-title-changed' is unmapped because the agent picks that
;;   title, and consuming it would silently overwrite a user's own
;;   `claude-agent-agents-rename' on the next turn.
;;
;; `claude-agent-agents--apply' suppresses a write whose status already
;; matches the row's.  This is load-bearing, not a micro-optimization:
;; `agent-message-chunk' fires once per streamed chunk, so a single long
;; response is hundreds of events.  Without suppression each one would
;; fire `edmacs-agents-changed-hook' and repaint every frame's sidebar,
;; and STATUS-TS would jitter so badly the elapsed column never settles.
;;
;; ## WHAT THE SHELL HOOKS DO THAT THIS CANNOT
;;
;; claude-term's rows are fed by `emacs-status.sh' commands wired into
;; `claude/settings.json' (Notification, PostToolUse, Stop,
;; UserPromptSubmit, SessionEnd) -- a separate dotfiles repository, not
;; this tree, and untouched by this phase.  Roadmap phase 1 rated the ACP
;; equivalent `missing': `claude-agent-acp' never executes those hook
;; commands at all.  So this module does not extend that plumbing, it
;; replaces it -- and four things do not carry over:
;;
;;   (a) NO HOOK EXECUTION.  The settings.json hooks remain claude-term's
;;       ingress exclusively.  Nothing here runs a shell command, and an
;;       ACP session contributes nothing to that channel.
;;
;;   (b) NO OUT-OF-EMACS INGRESS.  `emacs-status.sh' tracks sessions
;;       started anywhere -- a bare terminal, a tmux pane -- by posting
;;       into the daemon from outside.  An ACP row only ever exists for a
;;       session THIS Emacs started through `claude-agent-start'.  There
;;       is no out-of-band way to register one, by design.
;;
;;   (c) NO FAITHFUL IDLE NOTIFICATION.  The CLI's Notification hook
;;       fires when Claude is waiting on the user with nothing pending.
;;       agent-shell's nearest analogue is `idle', which is a timeout
;;       rather than a statement about the agent, and is deliberately
;;       unmapped (above).  A `waiting' ACP row therefore means a pending
;;       permission request specifically, not "idle at the prompt".
;;
;;   (d) NO SessionEnd.  That hook is what reaps a claude-term row when
;;       the process ends.  Here the job belongs to the process sentinel
;;       plus agent-shell's `clean-up' event -- see REAPING below.
;;
;; PINNED REVISION.  The phase body preferred `agent-shell-attention.el'
;; and its `agent-shell-attention-notify-function' as the status feed.
;; That file does not exist at the revision this config pins --
;; agent-shell 7377ba8, where the only occurrence of "attention" in the
;; whole repository is in README.org -- so this module parses the event
;; feed directly instead.  Moving that pin is what would invalidate this
;; paragraph; even then `agent-shell-subscribe-to' surfaces strictly more
;; states than a notify hook would and should stay the feed.  The sibling
;; pins phase 1 evaluated parity against are acp.el 0f2cac4 and
;; shell-maker f448a74.
;;
;; REAPING, AND WHY THE SENTINEL IS NOT REDUNDANT WITH `clean-up'.
;; agent-shell emits `clean-up' from `kill-buffer-hook', so it covers
;; exactly one ending: somebody killed the buffer.  An agent process that
;; is SIGKILLed, crashes, or whose node interpreter dies leaves the
;; buffer perfectly alive, emits no `clean-up' -- and, without the
;; sentinel below, a permanently stuck row.  That is precisely the
;; claude-repl failure this phase exists to not repeat: its sentinel
;; never removed its hash entry, never stopped its server and never
;; finalized the interaction, so a process dying without a protocol end
;; event left a dead row and an input-less buffer behind forever.  Both
;; paths therefore run, and `claude-agent-agents--reap' is idempotent
;; because an ordinary buffer kill fires both.
;;
;; The acp client -- and so its process -- is nil in `agent-shell--state'
;; until the async init pipeline creates it, so the sentinel cannot be
;; armed at create time.  `claude-agent-agents--arm-sentinel' is called
;; from the shared event handler on EVERY event instead; it is a cheap
;; idempotent no-op once armed.
;;
;; IDENTITY.  One key space, two sources: `edmacs-agents--key' is
;; (TRUENAME-ROOT . INSTANCE) for both.  `--resolve-instance' mints
;; "acp", "acp-2", ... probing the WHOLE agents table, so a claude-term
;; session whose instance label happens to be "acp" cannot be silently
;; overwritten.  Every write passes INSTANCE explicitly, because
;; `edmacs-agents-set-status' signals `user-error' when a root holds more
;; than one row and INSTANCE is nil -- guaranteed the moment a
;; claude-term and an ACP session share a worktree, which is the normal
;; case.
;;
;; `SPC a' COVERAGE.  `j' (jump), `L' (list), `r' (rename) and `x' (kill)
;; are source-agnostic and reach ACP rows; `n' (new claude-term session),
;; `w' (toggle pane), `A' (show all) and `X' (kill all) remain
;; claude-term-only, because each operates on ghostel panes and the
;; claude-term registry -- neither of which an ACP session has.  `c'
;; starts an ACP session.  See claude-term-registry.el, SPC a's sole
;; owner, for the binding form itself.
;;
;; Run pure-function tests:
;;   emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/agents.el -l modules/claude-agent.el \
;;         -l modules/claude-agent-agents.el \
;;         -l modules/claude-agent-agents-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)

;; agents.el's row API and table -- init.el loads it before this module
;; (see this file's load line there), and every call below happens inside
;; a function body, resolved at call time.
(declare-function edmacs-agents--key "agents")
(declare-function edmacs-agents--upsert "agents")
(declare-function edmacs-agents--remove "agents")
(declare-function edmacs-agents-set-status "agents")
(declare-function make-edmacs-agent "agents")
(declare-function edmacs-agent-key "agents")
(declare-function edmacs-agent-root "agents")
(declare-function edmacs-agent-instance "agents")
(declare-function edmacs-agent-status "agents")
(declare-function edmacs-agent-title "agents")
(declare-function edmacs-agent-locator "agents")
(defvar edmacs-agents--table)

(declare-function edmacs-sidebar-agents--redraw-all "sidebar-agents")

;; agent-shell is a soft dependency: it is loaded lazily by
;; `claude-agent--start-at's own `(require 'agent-shell nil t)', never at
;; this file's load time, so every use below is `fboundp'-guarded.
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell--state "agent-shell")

(defvar claude-agent-session-create-functions)

;; ============================================================================
;; Session bookkeeping
;; ============================================================================

(defvar claude-agent-agents--sessions (make-hash-table :test #'eq)
  "Agent-shell BUFFER -> the `edmacs-agents--table' key of its row.
The indirection that makes a rename safe.  The event subscription and
the process sentinel are both installed once, at session start, and
close over the BUFFER -- which never changes -- rather than over the row
key, which `claude-agent-agents-rename' re-keys.  A closure that had
captured the key directly would keep writing to the vacated key for the
rest of the session.")

(defvar claude-agent-agents--watched (make-hash-table :test #'eq)
  "Agent-shell BUFFER -> the acp process whose sentinel this module extended.
Guards `claude-agent-agents--watch-process' against double-arming: the
resolver runs on every event, since the acp client does not exist yet
when the session's row is created.")

(defun claude-agent-agents--forget (buffer)
  "Drop BUFFER's session bookkeeping, leaving the agents table alone."
  (remhash buffer claude-agent-agents--sessions)
  (remhash buffer claude-agent-agents--watched))

(defun claude-agent-agents--buffers-for-key (key)
  "Return every session buffer currently mapped to row KEY."
  (let (acc)
    (maphash (lambda (buffer k) (when (equal k key) (push buffer acc)))
             claude-agent-agents--sessions)
    acc))

;; ============================================================================
;; Instance resolution
;; ============================================================================

(defun claude-agent-agents--resolve-instance (root)
  "Return the first free ACP instance label under ROOT: \"acp\", \"acp-2\", ...
ROOT is already truename-normalized.  The probe is against the WHOLE
`edmacs-agents--table', not only `claude-agent' rows: agents.el has one
key space shared by every source, so a claude-term session that happens
to carry the instance label \"acp\" must push this one to \"acp-2\"
rather than be silently overwritten by it."
  (let ((n 1) instance)
    (while (progn
             (setq instance (if (= n 1) "acp" (format "acp-%d" n)))
             (gethash (edmacs-agents--key root instance) edmacs-agents--table))
      (setq n (1+ n)))
    instance))

;; ============================================================================
;; Row creation
;; ============================================================================

(defun claude-agent-agents--on-create (root buffer)
  "Register a `claude-agent'-sourced row for the session ROOT/BUFFER.
Runs on `claude-agent-session-create-functions'.  Builds the row
directly rather than going through `edmacs-agents-set-status' -- see
this file's Commentary on ROW CREATION for why.  STATUS starts `idle':
a freshly started session has had no prompt sent to it yet.  Returns the
row's key."
  (let* ((truename-root (condition-case nil (file-truename root) (error root)))
         (instance (claude-agent-agents--resolve-instance truename-root))
         (key (edmacs-agents--key truename-root instance))
         (now (float-time)))
    (edmacs-agents--upsert
     (make-edmacs-agent
      :key key
      :root truename-root
      :instance instance
      :status 'idle
      :status-ts now
      :updated-ts now
      :title instance
      :source 'claude-agent
      :locator buffer
      :unread nil))
    (puthash buffer key claude-agent-agents--sessions)
    (claude-agent-agents--subscribe buffer)
    (claude-agent-agents--arm-sentinel buffer)
    key))

(defun claude-agent-agents--subscribe (buffer)
  "Subscribe this module's single handler to every event in BUFFER.
`:event' is deliberately omitted so all events arrive through one
handler -- `claude-agent-agents--event-status' is the filter, and the
sentinel resolver needs to run on events it does not itself map.
Guarded three ways because agent-shell is a soft dependency and
`agent-shell--state' errors outside an `agent-shell-mode' buffer."
  (when (and (fboundp 'agent-shell-subscribe-to)
             (buffer-live-p buffer)
             (with-current-buffer buffer (derived-mode-p 'agent-shell-mode)))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :on-event (lambda (event) (claude-agent-agents--on-event buffer event)))))

;; ============================================================================
;; Event -> status
;; ============================================================================

(defconst claude-agent-agents--event-status-alist
  '((input-submitted     . working)
    (agent-message-chunk . working)
    (tool-call-update    . working)
    (permission-request  . waiting)
    (permission-response . working)
    (turn-complete       . done)
    (error               . done)
    (clean-up            . remove))
  "agent-shell event symbol -> the row status it means.
Every event agent-shell can emit that is NOT listed here maps to nil and
writes nothing at all; see this file's Commentary for why `idle' and
`session-title-changed' in particular are omissions by decision.

`error' maps to `done' so a failed request surfaces as a row wanting
attention rather than one stuck `working' -- acp.el synthesizes one for
every pending request when the agent process dies mid-flight
\(`acp--fail-pending-requests'), which is exactly when a user needs to
be told.  The process sentinel then reaps the row a moment later; the
two do not fight, because a reap removes unconditionally.")

(defun claude-agent-agents--event-status (event)
  "Return the row status agent-shell's EVENT means, or nil for no write.
Pure: EVENT is the alist `agent-shell-subscribe-to' hands its
`:on-event' function, and only its `:event' key is consulted."
  (alist-get (map-elt event :event) claude-agent-agents--event-status-alist))

(defun claude-agent-agents--apply (key status)
  "Write STATUS onto row KEY, or reap it when STATUS is `remove'.
Three deliberate behaviours:

A write whose STATUS already equals the row's current one is SKIPPED
entirely -- see this file's Commentary on why suppression is
load-bearing rather than an optimization.

A STATUS for a key with NO row is skipped too, rather than letting
`edmacs-agents-set-status' create one.  Its creation branch mints a
`:source nil :locator nil' row, so a stray late event arriving after a
reap would resurrect the session as an unjumpable, unrenameable,
unkillable row.

INSTANCE is always passed explicitly, never left to
`edmacs-agents-set-status' to infer: that function signals `user-error'
when a root holds more than one row and INSTANCE is nil, which is the
normal case the moment a claude-term and an ACP session share a
worktree."
  (if (eq status 'remove)
      (claude-agent-agents--reap key)
    (when-let* ((row (gethash key edmacs-agents--table)))
      (unless (eq (edmacs-agent-status row) status)
        (edmacs-agents-set-status (edmacs-agent-root row) status
                                  (edmacs-agent-instance row))))))

(defun claude-agent-agents--on-event (buffer event)
  "Handle one agent-shell EVENT for session BUFFER.
Resolves BUFFER's CURRENT row key on every call rather than closing over
the key captured at subscribe time, so a rename mid-session keeps
landing on the right row.  A no-op once the session has been reaped."
  (when-let* ((key (gethash buffer claude-agent-agents--sessions)))
    (claude-agent-agents--arm-sentinel buffer)
    (when-let* ((status (claude-agent-agents--event-status event)))
      (claude-agent-agents--apply key status))))

;; ============================================================================
;; Reaping: the process sentinel, and `clean-up'
;; ============================================================================

(defun claude-agent-agents--reap (key)
  "Remove row KEY and forget its session bookkeeping. Idempotent.
Idempotency is required, not defensive: an ordinary buffer kill fires
BOTH agent-shell's `clean-up' event and, moments later, the acp
process's sentinel, and either may arrive first.  Redraws every frame's
sidebar so the row disappears at once rather than at the next heartbeat
tick."
  (dolist (buffer (claude-agent-agents--buffers-for-key key))
    (claude-agent-agents--forget buffer))
  (when (gethash key edmacs-agents--table)
    (edmacs-agents--remove key)
    (when (fboundp 'edmacs-sidebar-agents--redraw-all)
      (edmacs-sidebar-agents--redraw-all))))

(defun claude-agent-agents--watch-process (process buffer)
  "Reap BUFFER's row when PROCESS exits or is signalled.
The testable core: takes the process outright so a test can arm it
against a real subprocess and kill that by signal.  A no-op unless
PROCESS is live, and at most one sentinel extension is installed per
BUFFER (`claude-agent-agents--watched'), since the resolver that calls
this runs on every event.

Extends the sentinel with `add-function' rather than replacing it:
acp.el installs its own, which kills the stderr buffer and fails every
pending request, and must keep running.

Resolves BUFFER's row key at FIRE time, not here, so a session renamed
after the sentinel was armed still reaps the row it actually has."
  (when (and (processp process)
             (process-live-p process)
             (not (gethash buffer claude-agent-agents--watched)))
    (puthash buffer process claude-agent-agents--watched)
    (add-function :after (process-sentinel process)
                  (lambda (proc _event)
                    (when (memq (process-status proc) '(exit signal))
                      (when-let* ((key (gethash buffer
                                                claude-agent-agents--sessions)))
                        (claude-agent-agents--reap key))))
                  '((name . claude-agent-agents)))))

(defun claude-agent-agents--arm-sentinel (buffer)
  "Dig BUFFER's acp process out of agent-shell's state and watch it.
The thin resolver over `claude-agent-agents--watch-process'.  Called
from the shared event handler on EVERY event because the acp client --
and therefore its process -- is nil in `agent-shell--state' until the
async init pipeline creates it, so there is no single later moment that
is reliably \"after\" it and still early enough.

Wrapped in `condition-case' and guarded on a live `agent-shell-mode'
buffer: `agent-shell--state' signals outside such a buffer, the session
buffer can be dead by the time a deferred handler runs, and a failure to
arm a sentinel must never take the event handler -- and with it the
whole status feed -- down."
  (when (and (buffer-live-p buffer)
             (not (gethash buffer claude-agent-agents--watched)))
    (condition-case nil
        (with-current-buffer buffer
          (when (and (derived-mode-p 'agent-shell-mode)
                     (fboundp 'agent-shell--state))
            (claude-agent-agents--watch-process
             (map-elt (map-elt (agent-shell--state) :client) :process)
             buffer)))
      (error nil))))

;; ============================================================================
;; Rename
;; ============================================================================

(defun claude-agent-agents-rename (agent)
  "Rename AGENT's row label, reached from the sidebar's `r'.
This renames the ROW only.  ACP has no rename verb of its own and
agent-shell owns the buffer's name, so there is nothing to push the new
label through to -- and `session-title-changed' is deliberately not
consumed (see this file's Commentary), precisely so the agent's own
title cannot overwrite a label the user chose.

Re-keys the EXISTING row in place -- remove, `setf', upsert -- exactly
as `claude-term-agents--on-rename' does, rather than rebuilding it: a
rename mid-turn must not reset STATUS or UNREAD.  The session's event
subscription and process sentinel both resolve the row key through
`claude-agent-agents--sessions' at fire time, so re-pointing that one
entry is all the re-keying either needs."
  (let* ((root (edmacs-agent-root agent))
         (old-key (edmacs-agent-key agent))
         (new (string-trim (read-string "New agent label: "
                                        (edmacs-agent-title agent)))))
    (when (string-empty-p new)
      (user-error "Claude-agent: an agent label must not be empty"))
    (let ((new-key (edmacs-agents--key root new)))
      (unless (equal new-key old-key)
        (when (gethash new-key edmacs-agents--table)
          (user-error "Claude-agent: an agent named %s is already registered here"
                      new))
        (edmacs-agents--remove old-key)
        (setf (edmacs-agent-instance agent) new
              (edmacs-agent-key agent) new-key
              (edmacs-agent-title agent) new)
        (edmacs-agents--upsert agent)
        (dolist (buffer (claude-agent-agents--buffers-for-key old-key))
          (puthash buffer new-key claude-agent-agents--sessions))))))

(add-hook 'claude-agent-session-create-functions #'claude-agent-agents--on-create)

(provide 'claude-agent-agents)
;;; claude-agent-agents.el ends here
