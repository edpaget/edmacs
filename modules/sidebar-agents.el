;;; sidebar-agents.el --- Agents in the sidebar, ALL AGENTS, attention jump -*- lexical-binding: t -*-

;;; Commentary:
;; Phase 6 of the edmacs-sidebar roadmap: renders phase 5's agent table
;; (`modules/agents.el', `edmacs-agents--table') into phase 1's sidebar
;; (`modules/sidebar.el'), under each worktree section -- matched by
;; truename against phase 3's worktree list (`modules/frames.el') so a
;; tmux agent running in a worktree with no open tab still shows up,
;; dimmed -- and again in a global ALL AGENTS section at the bottom of
;; every frame's sidebar. Generalizes sidebar.el's RET into a
;; type-dispatching visit command (raise the repo's frame, open/select
;; the worktree's tab, then either drive tmux or select an in-Emacs side
;; window), adds a repeatable `SPC a TAB' attention-cycling command, and
;; fires a defcustom-pluggable desktop notification on every transition
;; into `waiting' and on an unfocused transition into `done', coalesced
;; within a short window.
;;
;; sidebar.el exposes three extension points this file hooks into rather
;; than sidebar.el needing to know anything about agents:
;;   - `edmacs-sidebar-worktree-label-suffix-function' -- a swappable
;;     seam (same pattern as claude-term-registry.el's own
;;     `claude-term-registry-state-accessor') that appends this file's
;;     per-worktree agent count onto that worktree's own row label.
;;   - `edmacs-sidebar-worktree-section-functions' -- a hook run with
;;     (ROOT HAS-TAB) right after each worktree row is inserted, letting
;;     this file append its own `agents' child section underneath.
;;   - `edmacs-sidebar-extra-section-functions' -- a hook run with FRAME
;;     at the end of every redraw, letting this file append the
;;     frame-independent ALL AGENTS section.
;;
;; `edmacs-agents--table' is a single hash table shared by every frame
;; (agents.el is not frame-scoped), so "every agent across frames" for
;; the ALL AGENTS section and the attention list is simply the whole
;; table -- no cross-frame aggregation is needed.
;;
;; Transition detection (`edmacs-sidebar-agents--last-state') is driven
;; directly off `edmacs-agents-changed-hook', independent of whether any
;; sidebar window is visible or has ever been shown -- a hidden or
;; never-opened sidebar must still notify on a background agent
;; finishing.
;;
;; Loaded from init.el right after `sidebar' (see init.el's own comment
;; on this module's load line); `frames' and `agents' load later in
;; init.el's own sequence, but every cross-module call here is inside a
;; function body, resolved at call time, never at this file's own load
;; time, so the load-time ordering among the three is not load-bearing.
;;
;; Run pure-function tests:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el -l modules/agents.el \
;;         -l modules/sidebar-agents-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

;; Loaded by sidebar.el (init.el's `load-module' order puts this file
;; right after it) -- required again here so this file's own standalone
;; test harness (mirroring sidebar-test.el's build-root detection) can
;; load it directly without depending on sidebar.el having done so first.
(require 'magit-section)

;; ============================================================================
;; Forward declarations
;; ============================================================================
;; Every one of these resolves at real init.el runtime (see this file's
;; own Commentary on load order); declared for byte-compile hygiene only,
;; matching sidebar.el's own `(declare-function edmacs-worktrees-for-repo
;; "frames")' pattern for a module that loads later.

(declare-function edmacs-workspaces-open-worktree "workspaces")
(declare-function edmacs-worktrees-for-repo "frames")
(declare-function edmacs-sidebar--redraw "sidebar")
(declare-function edmacs-sidebar--window "sidebar")
(declare-function edmacs-sidebar-hide "sidebar")
(declare-function claude-term--pop-to-window "claude-term")
(declare-function claude-term-registry-get "claude-term-registry")
(declare-function claude-term-session-buffer "claude-term-registry")
(declare-function claude-term-rename "claude-term-registry")
(declare-function claude-term-kill "claude-term")
(defvar edmacs-sidebar-worktree-label-suffix-function)
(defvar edmacs-sidebar-worktree-section-functions)
(defvar edmacs-sidebar-extra-section-functions)
(defvar edmacs-sidebar-header-line-function)
(defvar edmacs-sidebar-force-text-glyphs)

;; agents.el's struct accessors/table -- loaded by init.el before this
;; module ever actually runs any of the functions below that touch them.
(declare-function make-edmacs-agent "agents")
(declare-function edmacs-agent-key "agents")
(declare-function edmacs-agent-p "agents")
(declare-function edmacs-agent-root "agents")
(declare-function edmacs-agent-instance "agents")
(declare-function edmacs-agent-status "agents")
(declare-function edmacs-agent-status-ts "agents")
(declare-function edmacs-agent-title "agents")
(declare-function edmacs-agent-source "agents")
(declare-function edmacs-agent-locator "agents")
(declare-function edmacs-agent-unread "agents")
(declare-function edmacs-agents-mark-read "agents")
(defvar edmacs-agents--table)
(defvar edmacs-agents-changed-hook)

;; nerd-icons is loaded eagerly by ui.el (`use-package nerd-icons', no
;; `:defer') in a real init.el session; declared here so the
;; byte-compiler doesn't warn under a standalone `-Q --batch' load where
;; it is genuinely absent -- every call site below is guarded by
;; `featurep'/`fboundp' first.
(declare-function nerd-icons-octicon "nerd-icons")
(declare-function nerd-icons-faicon "nerd-icons")

(defgroup edmacs-sidebar-agents nil
  "Agents rendered in the sidebar, attention jump, and desktop notification."
  :group 'edmacs-sidebar)

;; ============================================================================
;; Faces
;; ============================================================================

(defface edmacs-sidebar-agent-working-face
  '((t))
  "Face for a `working' agent row -- deliberately ambient/neutral, no
strong color, so a busy worktree's list doesn't read as an alert."
  :group 'edmacs-sidebar-agents)

(defface edmacs-sidebar-agent-waiting-face
  '((t :inherit warning))
  "Face for a `waiting' agent row -- it wants the user's attention."
  :group 'edmacs-sidebar-agents)

(defface edmacs-sidebar-agent-done-face
  '((t :inherit success))
  "Face for a `done' agent row."
  :group 'edmacs-sidebar-agents)

(defface edmacs-sidebar-agent-idle-face
  '((t :inherit shadow))
  "Face for an `idle' agent row (only reached after `edmacs-agents-mark-read')."
  :group 'edmacs-sidebar-agents)

(defun edmacs-sidebar-agents--status-face (status)
  "Return STATUS's face, or nil for an unrecognized status."
  (pcase status
    ('working 'edmacs-sidebar-agent-working-face)
    ('waiting 'edmacs-sidebar-agent-waiting-face)
    ('done 'edmacs-sidebar-agent-done-face)
    ('idle 'edmacs-sidebar-agent-idle-face)
    (_ nil)))

;; ============================================================================
;; Glyphs
;; ============================================================================

(defconst edmacs-sidebar-agents--fallback-glyphs
  '((working . "*") (waiting . "?") (done . "v") (idle . "o"))
  "Plain-ASCII fallback per status, matching claude-term-registry.el's own
`claude-term-registry--state-icons' table so the two agent surfaces
read the same way whenever nerd-icons is unavailable.")

(defun edmacs-sidebar-agents--nerd-icon (status)
  "Return STATUS's nerd-icons glyph string, or nil if unavailable.
Never signals: an icon lookup failing (a data-table miss, a future
nerd-icons release renaming a glyph) falls back to the plain-ASCII
table exactly as if nerd-icons were not loaded at all."
  (and (not edmacs-sidebar-force-text-glyphs)
       (featurep 'nerd-icons)
       (condition-case nil
           (pcase status
             ('working (and (fboundp 'nerd-icons-faicon)
                             (nerd-icons-faicon "nf-fa-spinner")))
             ('waiting (and (fboundp 'nerd-icons-octicon)
                             (nerd-icons-octicon "nf-oct-comment")))
             ('done (and (fboundp 'nerd-icons-octicon)
                         (nerd-icons-octicon "nf-oct-check_circle")))
             ('idle (and (fboundp 'nerd-icons-octicon)
                         (nerd-icons-octicon "nf-oct-dot_fill")))
             (_ nil))
         (error nil))))

(defun edmacs-sidebar-agents--glyph (status)
  "Return the display glyph for STATUS: a nerd-icon if available, else
the plain-ASCII fallback from `edmacs-sidebar-agents--fallback-glyphs'."
  (or (edmacs-sidebar-agents--nerd-icon status)
      (alist-get status edmacs-sidebar-agents--fallback-glyphs "?")))

;; ============================================================================
;; Elapsed-time string
;; ============================================================================

(defun edmacs-sidebar-agents--elapsed-string (status-ts)
  "Return a short humanized elapsed-time string since STATUS-TS.
Same shape as claude-term-registry.el's own
`claude-term-registry--elapsed-string'. Returns \"\" for a non-numeric
STATUS-TS rather than erroring -- a freshly constructed row in a test,
or a malformed upstream value, renders with no elapsed suffix instead
of taking down the whole redraw."
  (if (not (numberp status-ts))
      ""
    (let ((seconds (max 0 (round (- (float-time) status-ts)))))
      (cond
       ((< seconds 60) (format "%ds" seconds))
       ((< seconds 3600) (format "%dm" (/ seconds 60)))
       ((< seconds 86400) (format "%dh" (/ seconds 3600)))
       (t (format "%dd" (/ seconds 86400)))))))

;; ============================================================================
;; Worktree matching
;; ============================================================================

(defun edmacs-sidebar-agents--agent-root-truename (agent)
  "Return AGENT's root, truename-normalized.
Both `edmacs-agent-root' (agents.el) and `edmacs-worktrees-for-repo'
(frames.el) already return truenames from their own producers, so this
re-normalize is defensive, not the primary comparison -- it only
matters if either producer's own contract ever drifts. This is the
single implementation of \"does this agent belong to this worktree
root\"; `--for-root' below is its only caller."
  (condition-case nil (file-truename (edmacs-agent-root agent))
    (error (edmacs-agent-root agent))))

;; ============================================================================
;; Reading the (frame-independent) global agent table
;; ============================================================================

(defun edmacs-sidebar-agents--all ()
  "Return every tracked agent, as a fresh list.
`edmacs-agents--table' is one hash table shared by every frame -- this
is already \"every agent across frames\", no per-frame aggregation."
  (let (acc)
    (maphash (lambda (_key row) (push row acc)) edmacs-agents--table)
    acc))

(defun edmacs-sidebar-agents--for-root (root)
  "Return every tracked agent whose truename-normalized root equals ROOT.
ROOT is expected to already be a truename (every caller passes one from
`edmacs-worktrees-for-repo'); see `--agent-root-truename'."
  (seq-filter (lambda (agent)
                (equal (edmacs-sidebar-agents--agent-root-truename agent) root))
              (edmacs-sidebar-agents--all)))

;; ============================================================================
;; Attention order: waiting, then unread done, then working, then idle
;; ============================================================================

(defun edmacs-sidebar-agents--state-rank (agent)
  "Return AGENT's sort rank: 0 waiting, 1 unread done, 2 working, 3 else."
  (cond
   ((eq (edmacs-agent-status agent) 'waiting) 0)
   ((and (eq (edmacs-agent-status agent) 'done) (edmacs-agent-unread agent)) 1)
   ((eq (edmacs-agent-status agent) 'working) 2)
   (t 3)))

(defun edmacs-sidebar-agents--compare (a b)
  "Return non-nil when A sorts before B in attention order.
Ties within the same rank break on `status-ts' ascending (the
longest-waiting/oldest-unread row first), then on TITLE, so ordering is
deterministic for two rows in the same state at the same timestamp."
  (let ((ra (edmacs-sidebar-agents--state-rank a))
        (rb (edmacs-sidebar-agents--state-rank b)))
    (cond
     ((/= ra rb) (< ra rb))
     ((/= (or (edmacs-agent-status-ts a) 0) (or (edmacs-agent-status-ts b) 0))
      (< (or (edmacs-agent-status-ts a) 0) (or (edmacs-agent-status-ts b) 0)))
     (t (string< (or (edmacs-agent-title a) "") (or (edmacs-agent-title b) ""))))))

(defun edmacs-sidebar-agents--attention-p (agent)
  "Return non-nil when AGENT is attention-worthy: waiting, or unread done."
  (or (eq (edmacs-agent-status agent) 'waiting)
      (and (eq (edmacs-agent-status agent) 'done) (edmacs-agent-unread agent))))

(defun edmacs-sidebar-agents--attention-list ()
  "Return every attention-worthy agent, sorted waiting then unread done."
  (sort (seq-filter #'edmacs-sidebar-agents--attention-p (edmacs-sidebar-agents--all))
        #'edmacs-sidebar-agents--compare))

;; ============================================================================
;; Rendering: per-worktree `agents' subsection
;; ============================================================================

;; An `edmacs-sidebar-agent' row's section value is the live `edmacs-agent'
;; cl-defstruct, and `edmacs-agents-set-status' always builds a fresh struct
;; under the same key rather than mutating one in place -- so the struct
;; itself is not `equal'-stable across a heartbeat-only redraw, and the
;; default `magit-section-ident-value' (which returns a non-EIEIO value
;; verbatim) would make the row's ident change on every tick. Register a
;; distinct, package-prefixed subclass for this one type and specialize
;; ident-value on it, exactly as real Magit extends section identity for
;; its own non-trivial values; this must NOT be done on the shared base
;; `magit-section' class, or it would leak into claude-usage.el's own
;; `magit-section-mode' buffer and any real Magit buffer.
(defclass edmacs-sidebar-agent-section (magit-section) ())
(add-to-list 'magit--section-type-alist (cons 'edmacs-sidebar-agent 'edmacs-sidebar-agent-section))
(cl-defmethod magit-section-ident-value ((section edmacs-sidebar-agent-section))
  "Key on `edmacs-agent-key', falling back to the raw value when it is
not actually an `edmacs-agent' struct -- a degenerate/direct-call
construction (never `--insert-row' itself) can give this section type a
nil or otherwise non-agent value, and `magit-section-cached-visibility'
computes every section's ident unconditionally on every insertion, so
this runs even then."
  (let ((value (oref section value)))
    (if (edmacs-agent-p value) (edmacs-agent-key value) value)))

(defun edmacs-sidebar-agents--insert-row (agent)
  "Insert one row for AGENT: glyph, title, elapsed time, per-status face.
Bold is layered ON TOP of the status face (rather than replacing it)
when AGENT is an unread `done' row -- cleared the moment
`edmacs-agents-mark-read' flips it to `idle', which the visit commands
below call before redrawing."
  (let* ((unread (and (eq (edmacs-agent-status agent) 'done) (edmacs-agent-unread agent)))
         (glyph (edmacs-sidebar-agents--glyph (edmacs-agent-status agent)))
         (elapsed (edmacs-sidebar-agents--elapsed-string (edmacs-agent-status-ts agent)))
         (label (string-trim-right
                 (format "  %s %s %s" glyph (or (edmacs-agent-title agent) "") elapsed)))
         (status-face (edmacs-sidebar-agents--status-face (edmacs-agent-status agent)))
         (face (cond ((and unread status-face) (list 'bold status-face))
                     (unread 'bold)
                     (t status-face))))
    (magit-insert-section (edmacs-sidebar-agent agent)
      (magit-insert-heading
        (if face (propertize label 'face face) label)))))

(defun edmacs-sidebar-agents--insert-group (root agents)
  "Insert AGENTS (already known to belong to ROOT) as child rows of one
`edmacs-sidebar-agents-group' section, in attention order. The
section's own value is ROOT, not AGENTS: `edmacs-agents-set-status'
always rebuilds this list fresh (new cons cells, new structs) even
when membership and order are unchanged, so keying identity on it would
make the group's `magit-section-ident' -- and therefore every child
agent row's full ident chain -- change on every heartbeat tick. A bare
ROOT string is `equal'-stable the same way `edmacs-sidebar--insert-tab-
row's worktree-row value is, so this needs no
`magit-section-ident-value' specializer of its own."
  (magit-insert-section (edmacs-sidebar-agents-group root)
    (dolist (agent (sort (copy-sequence agents) #'edmacs-sidebar-agents--compare))
      (edmacs-sidebar-agents--insert-row agent))))

(defun edmacs-sidebar-agents--on-worktree-section (root _has-tab &optional _frame _tab-number)
  "Append ROOT's `agents' child section, if it has any tracked agents.
Registered on `edmacs-sidebar-worktree-section-functions'; a worktree
with no agents renders no extra section at all, so an ordinary
agent-less worktree row is unchanged from before this phase. HAS-TAB is
unused here: an agent still renders under a tab-less (dimmed) worktree
row exactly the same as under an open one -- only the worktree row
itself carries the dimmed face, per AC1. FRAME/TAB-NUMBER (added by
sidebar-buffers.el, phase 7) are unused here too -- accepted only so
this function's arity still matches the now-4-arg
`run-hook-with-args' call in sidebar.el."
  (let ((agents (edmacs-sidebar-agents--for-root root)))
    (when agents
      (edmacs-sidebar-agents--insert-group root agents))))

(add-hook 'edmacs-sidebar-worktree-section-functions #'edmacs-sidebar-agents--on-worktree-section)

(defun edmacs-sidebar-agents--label-suffix (root)
  "Return \" (N)\" for ROOT's agent count, or nil when it has none.
Assigned to sidebar.el's `edmacs-sidebar-worktree-label-suffix-function'
swappable seam below."
  (let ((n (length (edmacs-sidebar-agents--for-root root))))
    (and (> n 0) (format " (%d)" n))))

(setq edmacs-sidebar-worktree-label-suffix-function #'edmacs-sidebar-agents--label-suffix)

;; ============================================================================
;; Header line: repo-wide agent-status roll-up
;; ============================================================================

(defun edmacs-sidebar-agents--agents-for-frame (frame)
  "Return the agents belonging to FRAME's own repo, or all of them.
An agent is keyed by its worktree root, and a frame owns exactly the
worktrees of its stamped `edmacs-repo' common-dir -- so the roll-up
counts this project's work, not every project's. Falls back to the whole
table when the frame carries no repo stamp or the worktree cache has not
been populated (a miss returns nil, which is not the same as a repo with
no worktrees), since a global count is a better answer than an empty one."
  (let* ((common (and frame (frame-parameter frame 'edmacs-repo)))
         (worktrees (and common (edmacs-worktrees-for-repo common)))
         (roots (and worktrees (mapcar #'cdr worktrees))))
    (if (null roots)
        (edmacs-sidebar-agents--all)
      (seq-filter (lambda (a) (member (edmacs-agent-root a) roots))
                  (edmacs-sidebar-agents--all)))))

(defun edmacs-sidebar-agents--header-line (frame)
  "Return a \"  [N⟳ N💬 N✓]\" roll-up suffix, or nil when no agent is
tracked at all. Assigned to sidebar.el's
`edmacs-sidebar-header-line-function' swappable seam, mirroring
`--label-suffix's own assignment above; the
agent table is scoped to FRAME's own repo by
`edmacs-sidebar-agents--agents-for-frame'.

Glyphs, not words, and zero counts omitted: the prose form
(\"  [1 working, 0 waiting, 0 done]\") is 31 columns and this suffix
renders inside a sidebar whose default body width is 30, so it was
always truncated away. Same glyph vocabulary as claude-term-agents.el's
own per-buffer mode-line segment."
  (let ((agents (edmacs-sidebar-agents--agents-for-frame frame)))
    (when agents
      (let ((working (cl-count-if (lambda (a) (eq (edmacs-agent-status a) 'working)) agents))
            (waiting (cl-count-if (lambda (a) (eq (edmacs-agent-status a) 'waiting)) agents))
            (done (cl-count-if (lambda (a) (eq (edmacs-agent-status a) 'done)) agents)))
        (format "  [%s]"
                (string-join
                 (or (delq nil
                           (list (and (> working 0) (format "%d⟳" working))
                                 (and (> waiting 0) (format "%d💬" waiting))
                                 (and (> done 0) (format "%d✓" done))))
                     (list "0"))
                 " "))))))

(setq edmacs-sidebar-header-line-function #'edmacs-sidebar-agents--header-line)

;; ============================================================================
;; Rendering: global ALL AGENTS section
;; ============================================================================

(defcustom edmacs-sidebar-agents-show-all nil
  "Non-nil shows the global ALL AGENTS section at the bottom of every
frame's sidebar. Toggled interactively by
`edmacs-sidebar-agents-toggle-all', bound to `a' in
`edmacs-sidebar-mode-map'."
  :type 'boolean
  :group 'edmacs-sidebar-agents)

(defun edmacs-sidebar-agents--insert-all-section (_frame)
  "Append the ALL AGENTS section when `edmacs-sidebar-agents-show-all'.
Registered on `edmacs-sidebar-extra-section-functions'; FRAME is unused
-- the section's content is the whole (frame-independent) agent table,
identical on every frame."
  (when edmacs-sidebar-agents-show-all
    (let ((sorted (sort (edmacs-sidebar-agents--all) #'edmacs-sidebar-agents--compare)))
      (magit-insert-section (edmacs-sidebar-agents-all)
        (magit-insert-heading "ALL AGENTS")
        (dolist (agent sorted)
          (edmacs-sidebar-agents--insert-row agent))))))

(add-hook 'edmacs-sidebar-extra-section-functions #'edmacs-sidebar-agents--insert-all-section)

(defun edmacs-sidebar-agents--redraw-all ()
  "Redraw every live frame's sidebar buffer."
  (dolist (frame (frame-list))
    (when (frame-live-p frame)
      (edmacs-sidebar--redraw frame))))

;;;###autoload
(defun edmacs-sidebar-agents-toggle-all ()
  "Toggle the global ALL AGENTS section, felt on every frame at once."
  (interactive)
  (setq edmacs-sidebar-agents-show-all (not edmacs-sidebar-agents-show-all))
  (edmacs-sidebar-agents--redraw-all))

;; ============================================================================
;; Visiting an agent row
;; ============================================================================

(defun edmacs-sidebar-agents--visit-common (agent)
  "Open or select AGENT's worktree tab, mark it read, then redraw every
frame's sidebar so the row's bold face clears immediately.
A worktree is a tab inside its project's tab-bar group (workspaces.el);
no frame is raised or created. Shared by the full RET visit and the
attention-jump command below; the difference between the two is only the
source-specific extra step in `edmacs-sidebar-agents--visit-source-extra'."
  (when agent
    (edmacs-workspaces-open-worktree (edmacs-agent-root agent))
    (edmacs-agents-mark-read (edmacs-agent-key agent))
    (edmacs-sidebar-agents--redraw-all)))

(defun edmacs-sidebar-agents--visit-source-extra (agent)
  "Run AGENT's source-specific jump side effect.
A `claude-term' row displays and selects its own pane via
`claude-term--pop-to-window' -- an ordinary window, not a side window,
since claude-term stopped allocating right-hand side slots. Any other
source (including a `nil', unattached row) is a deliberate no-op: this
pcase has no catch-all, so a row with no jump target simply does
nothing here, after `edmacs-sidebar-agents--visit-common' has already
selected the worktree tab and marked it read. Called unguarded on purpose: the
`fboundp' guard this replaced turned a renamed-away function into a
silent no-op rather than an error."
  (pcase (edmacs-agent-source agent)
    ('claude-term
     (claude-term--pop-to-window (edmacs-agent-locator agent)))))

;;;###autoload
(defun edmacs-sidebar-agents-visit ()
  "Visit the agent row at point: the full RET jump.
Pulls the `edmacs-agent' struct straight off the section's own VALUE
-- see `edmacs-sidebar-agents--insert-row'. Signals `user-error' when
there is no section at point, or its VALUE slot is unbound or nil,
rather than doing nothing."
  (interactive)
  (let* ((section (magit-current-section))
         (agent (and section (slot-boundp section 'value) (oref section value))))
    (if agent
        (progn
          (edmacs-sidebar-agents--visit-common agent)
          (edmacs-sidebar-agents--visit-source-extra agent))
      (user-error "Nothing to do on this row"))))

;; ============================================================================
;; r -- rename an agent instance (sidebar.el's `edmacs-sidebar-rename-at-point')
;; ============================================================================

(defun edmacs-sidebar-agents--claude-term-session (agent)
  "Return AGENT's live `claude-term' session, or signal `user-error'.
Shared by `edmacs-sidebar-agents-rename' and `edmacs-sidebar-agents-kill':
mapping an `edmacs-agent' row onto a registered session is one piece of
domain logic, so a later change to how instances are matched lands in
one place rather than half of two commands."
  (or (claude-term-registry-get (edmacs-agent-root agent)
                                (edmacs-agent-instance agent))
      (user-error "Claude-term: no session registered for instance %s"
                  (edmacs-agent-instance agent))))

(defun edmacs-sidebar-agents--claude-term-buffer (agent)
  "Return AGENT's live `claude-term' buffer, or signal `user-error'.
This module's single call site for the foreign `claude-term-session-buffer'
struct accessor: `claude-term-registry.el' compiles that accessor's
callers into an inlined type-check-plus-`aref' whenever it is loaded
before this module, which a `cl-letf' stub on the bare accessor cannot
intercept. Routing both `-rename' and `-kill' through this plain defun
instead keeps this file's own test suite's stubs immune to that
load-order-dependent inlining.
Resolves the session via a `let' binding rather than nesting the call
directly, so a missing session's `user-error' (from
`edmacs-sidebar-agents--claude-term-session') signals before this
function's own body ever needs `claude-term-session-buffer's binding --
Emacs Lisp resolves a call's function binding before evaluating its
argument forms, so `(claude-term-session-buffer (...session...))' would
instead report a `void-function' error in an environment where
`claude-term-registry.el' (which defines the real accessor) is not
loaded, such as this module's own pure test suite."
  (let ((session (edmacs-sidebar-agents--claude-term-session agent)))
    (claude-term-session-buffer session)))

;;;###autoload
(defun edmacs-sidebar-agents-rename (agent)
  "Rename AGENT's title, called by sidebar.el's `edmacs-sidebar-rename-at-point'.
Only a `claude-term'-sourced row can be renamed here: resolves AGENT's
live session's buffer via `edmacs-sidebar-agents--claude-term-buffer'
and delegates entirely to `claude-term-rename' on it, rather than
calling `claude-term-registry-rename' directly -- `claude-term-rename' also
updates the buffer-local `claude-term--instance' and renames the
buffer itself, both of which `claude-term--on-exit' (claude-term.el)
reads to deregister the session on kill; skipping them here would
leave the registry keyed under the OLD instance for that lookup while
this file's own row model already reflects the new one. Any non-
`claude-term' source, including a `nil', unattached row, signals
`user-error' -- only a `claude-term' row's title has a channel this UI
can push a rename through.
This function's own test coverage of the `claude-term' branch (this
file's pure suite) is against a synthetic `edmacs-agent' struct and a
mocked `claude-term-registry-get'/`claude-term-rename', not a real
registry entry -- `claude-term-rename' itself, the function this
delegates to, already has real-session coverage via
`claude-term-registry-live-test.el's AC5 test. Sibling commands
`edmacs-sidebar-agents-visit' and `-kill' now also have a real,
adapter-produced row to run against: see
`edmacs-sidebar-agents-live-test-real-claude-term-row-visit-and-kill'
(sidebar-agents-live-test.el, edmacs-sidebar roadmap phase 9)."
  (if (eq (edmacs-agent-source agent) 'claude-term)
      (let ((buffer (edmacs-sidebar-agents--claude-term-buffer agent)))
        (claude-term-rename buffer)
        (edmacs-sidebar-agents--redraw-all))
    (user-error "Cannot rename a %s agent" (edmacs-agent-source agent))))

;; ============================================================================
;; d -- kill an agent session (sidebar.el's `edmacs-sidebar-kill-at-point')
;; ============================================================================

;;;###autoload
(defun edmacs-sidebar-agents-kill (agent)
  "Kill AGENT's underlying session, after confirming with `yes-or-no-p'.
A `claude-term' row resolves its live session's buffer via
`edmacs-sidebar-agents--claude-term-buffer' and calls `claude-term-kill'
on it -- teardown then runs through claude-term.el's own async
sentinel/`claude-term--on-exit' path exactly as it does for the direct
command, so the registry and buffer stay in sync. Any other source
signals `user-error'."
  (when (yes-or-no-p (format "Kill agent session %s? " (edmacs-agent-title agent)))
    (pcase (edmacs-agent-source agent)
      ('claude-term
       (let ((buffer (edmacs-sidebar-agents--claude-term-buffer agent)))
         (claude-term-kill buffer)))
      (source (user-error "Cannot kill a %s agent" source)))
    (edmacs-sidebar-agents--redraw-all)))

;; ============================================================================
;; SPC a TAB -- attention cycling
;; ============================================================================

(defvar edmacs-sidebar-agents--attention-cache nil
  "The attention list's keys, as of the last `edmacs-sidebar-agents-goto-attention'
call -- membership (or order) changing since resets the cursor to 0.")

(defvar edmacs-sidebar-agents--attention-cursor 0
  "Index into the current attention list `edmacs-sidebar-agents-goto-attention'
will visit next.")

(defun edmacs-sidebar-agents--attention-keys ()
  "Return the current attention list's agent keys, in attention order."
  (mapcar #'edmacs-agent-key (edmacs-sidebar-agents--attention-list)))

;;;###autoload
(defun edmacs-sidebar-agents-goto-attention ()
  "Jump to the next agent in attention order: waiting, then unread done.
Repeatable: each call advances to the next row. Resets to the first row
whenever the attention set's membership or order has changed since the
last call -- a newly-appeared waiting agent is never skipped by an
already-exhausted cursor. Messages \"no agent wants you\" (and resets)
once the cursor runs past the end."
  (interactive)
  (let ((keys (edmacs-sidebar-agents--attention-keys)))
    (unless (equal keys edmacs-sidebar-agents--attention-cache)
      (setq edmacs-sidebar-agents--attention-cache keys
            edmacs-sidebar-agents--attention-cursor 0))
    (if (< edmacs-sidebar-agents--attention-cursor (length keys))
        (let* ((key (nth edmacs-sidebar-agents--attention-cursor keys))
               (agent (gethash key edmacs-agents--table)))
          (setq edmacs-sidebar-agents--attention-cursor
                (1+ edmacs-sidebar-agents--attention-cursor))
          (if agent
              (edmacs-sidebar-agents--visit-common agent)
            (message "no agent wants you")))
      (setq edmacs-sidebar-agents--attention-cursor 0)
      (message "no agent wants you"))))

;; ============================================================================
;; State-transition detection -> desktop notification, coalesced
;; ============================================================================

(defvar edmacs-sidebar-agents--last-state (make-hash-table :test #'equal)
  "Agent key -> last-observed STATUS, for transition detection.
Populated from `edmacs-agents-changed-hook' -- independent of whether
any sidebar window is visible or has ever been shown, so a background
agent finishing while the sidebar is hidden still notifies.")

(defcustom edmacs-sidebar-agents-notify-function
  #'edmacs-sidebar-agents--osascript-notify
  "Function of (TITLE BODY) that raises one desktop notification.
A swappable seam -- the default shells out to macOS's `osascript', but
terminal-notifier or ghostel's own notifier can replace it without
touching this file's transition/coalescing logic."
  :type 'function
  :group 'edmacs-sidebar-agents)

(defun edmacs-sidebar-agents--osascript-notify (title body)
  "Default `edmacs-sidebar-agents-notify-function': macOS `osascript'.
Async via `start-process', never blocking."
  (start-process "edmacs-sidebar-agents-notify" nil "osascript" "-e"
                 (format "display notification %S with title %S" body title)))

(defvar edmacs-sidebar-agents-coalesce-seconds 2
  "Notifications queued within this many seconds of each other collapse
into one call to `edmacs-sidebar-agents-notify-function'. A plain
`defvar', not `defcustom', so a test can shrink it.")

(defvar edmacs-sidebar-agents--pending-notification nil
  "The most recent queued (TITLE . BODY) pair, or nil.
Only the latest survives a burst -- the coalescing window collapses
several transitions into exactly one notification, not one per
transition, and the most recent is the most relevant to show.")

(defvar edmacs-sidebar-agents--coalesce-timer nil
  "The pending coalescing timer, or nil.
Canceled and rescheduled on every new queued notification -- a rolling
debounce, never a stack of timers -- so a burst inside the window
produces exactly one flush, deferred to (start of burst + this many
untouched seconds).")

(defun edmacs-sidebar-agents--flush-notification ()
  "Fire the queued notification, if any, and clear coalescing state."
  (setq edmacs-sidebar-agents--coalesce-timer nil)
  (when edmacs-sidebar-agents--pending-notification
    (let ((pair edmacs-sidebar-agents--pending-notification))
      (setq edmacs-sidebar-agents--pending-notification nil)
      (funcall edmacs-sidebar-agents-notify-function (car pair) (cdr pair)))))

(defun edmacs-sidebar-agents--worktree-name-for-root (root)
  "Return ROOT's leaf directory name, for a notification body."
  (file-name-nondirectory (directory-file-name (or root ""))))

(defun edmacs-sidebar-agents--queue-notify (agent)
  "Queue a notification for AGENT, coalescing with any pending one."
  (setq edmacs-sidebar-agents--pending-notification
        (cons (or (edmacs-agent-title agent) "")
              (edmacs-sidebar-agents--worktree-name-for-root (edmacs-agent-root agent))))
  (when (timerp edmacs-sidebar-agents--coalesce-timer)
    (cancel-timer edmacs-sidebar-agents--coalesce-timer))
  (setq edmacs-sidebar-agents--coalesce-timer
        (run-at-time edmacs-sidebar-agents-coalesce-seconds nil
                      #'edmacs-sidebar-agents--flush-notification)))

(defun edmacs-sidebar-agents--all-frames-unfocused-p ()
  "Return non-nil iff every live frame reports no input focus.
A dead frame (a race with `delete-frame') counts as unfocused rather
than erroring the whole check; wrapped in `condition-case' per-frame so
a minibuffer-only or otherwise unusual frame can never abort the scan."
  (seq-every-p
   (lambda (frame)
     (or (not (frame-live-p frame))
         (condition-case nil (null (frame-focus-state frame)) (error t))))
   (frame-list)))

(defun edmacs-sidebar-agents--on-transition (agent old-status new-status)
  "Queue a notification for AGENT's transition from OLD-STATUS to NEW-STATUS.
Always for a transition into `waiting'; only for `done' when every live
frame is unfocused. Every other transition (into `working' or `idle',
or a `done'-to-`done' refresh) notifies nothing."
  (ignore old-status)
  (cond
   ((eq new-status 'waiting) (edmacs-sidebar-agents--queue-notify agent))
   ((and (eq new-status 'done) (edmacs-sidebar-agents--all-frames-unfocused-p))
    (edmacs-sidebar-agents--queue-notify agent))))

(defun edmacs-sidebar-agents--observe-changes (keys)
  "Diff KEYS (from `edmacs-agents-changed-hook') against last-known state.
A key no longer in the table (reaped by the sweep, or otherwise
removed) is dropped from `edmacs-sidebar-agents--last-state' with no
notification -- disappearing is not a state transition."
  (dolist (key keys)
    (let ((agent (gethash key edmacs-agents--table))
          (old (gethash key edmacs-sidebar-agents--last-state)))
      (if (null agent)
          (remhash key edmacs-sidebar-agents--last-state)
        (let ((new (edmacs-agent-status agent)))
          (unless (eq old new)
            (edmacs-sidebar-agents--on-transition agent old new))
          (puthash key new edmacs-sidebar-agents--last-state))))))

;; ============================================================================
;; 30s elapsed-string re-render timer
;; ============================================================================

(defvar edmacs-sidebar-agents-elapsed-interval 30
  "Seconds between periodic redraws while a sidebar is visible and at
least one agent is `working' -- keeps a working row's elapsed-time
string advancing without a redraw on every tick regardless of
visibility. A plain `defvar' so a test can shrink it.")

(defvar edmacs-sidebar-agents--elapsed-timer nil
  "The periodic elapsed-render timer, or nil when disarmed.
A genuinely repeating `run-with-timer', not a self-rescheduling one-shot:
`timerp' stays non-nil on a one-shot timer object even after it has
already fired and left `timer-list', so a self-reschedule guarded only
by `timerp' would silently never re-arm past its first firing.")

(defun edmacs-sidebar-agents--any-working-p ()
  "Return non-nil iff any tracked agent is `working'."
  (let (found)
    (maphash (lambda (_key row) (when (eq (edmacs-agent-status row) 'working)
                                   (setq found t)))
             edmacs-agents--table)
    found))

(defun edmacs-sidebar-agents--any-sidebar-visible-p ()
  "Return non-nil iff any live frame's sidebar window is shown."
  (seq-some (lambda (frame) (and (frame-live-p frame) (edmacs-sidebar--window frame)))
            (frame-list)))

(defun edmacs-sidebar-agents--should-tick-p ()
  "Return non-nil iff the elapsed-render timer should be armed right now."
  (and (edmacs-sidebar-agents--any-working-p)
       (edmacs-sidebar-agents--any-sidebar-visible-p)))

(defun edmacs-sidebar-agents--elapsed-tick ()
  "One firing of the repeating elapsed-render timer: redraw, then disarm
if conditions no longer call for ticking. Re-arming (should conditions
later call for it again) goes back through `--ensure-elapsed-timer',
same as every other trigger site."
  (edmacs-sidebar-agents--redraw-all)
  (unless (edmacs-sidebar-agents--should-tick-p)
    (edmacs-sidebar-agents--ensure-elapsed-timer)))

(defun edmacs-sidebar-agents--elapsed-tick-safe ()
  "Call `edmacs-sidebar-agents--elapsed-tick', catching any error so the
repeating timer never dies silently on one bad redraw."
  (condition-case err
      (edmacs-sidebar-agents--elapsed-tick)
    (error (message "edmacs-sidebar-agents: elapsed tick failed: %s" err))))

(defun edmacs-sidebar-agents--ensure-elapsed-timer ()
  "Arm or disarm the periodic elapsed-render timer to match current
visibility/working-agent conditions. Idempotent: calling this when the
timer already matches the desired state is a no-op."
  (if (edmacs-sidebar-agents--should-tick-p)
      (unless edmacs-sidebar-agents--elapsed-timer
        (setq edmacs-sidebar-agents--elapsed-timer
              (run-with-timer edmacs-sidebar-agents-elapsed-interval
                               edmacs-sidebar-agents-elapsed-interval
                               #'edmacs-sidebar-agents--elapsed-tick-safe)))
    (when edmacs-sidebar-agents--elapsed-timer
      (cancel-timer edmacs-sidebar-agents--elapsed-timer)
      (setq edmacs-sidebar-agents--elapsed-timer nil))))

;; Re-evaluate whenever the sidebar stops being shown -- sidebar.el's own
;; `--redraw' (which the changed-hook handler below already triggers)
;; only runs on a SHOW-relevant event, never on a plain hide, so without
;; this a hidden sidebar's timer would linger until its own next tick.
(advice-add 'edmacs-sidebar-hide :after
            (lambda (&rest _) (edmacs-sidebar-agents--ensure-elapsed-timer)))

;; ============================================================================
;; Wire transition-detection + redraw + timer re-evaluation onto every change
;; ============================================================================

(defun edmacs-sidebar-agents--on-agents-changed (keys)
  "The single handler wired onto `edmacs-agents-changed-hook'.
Runs unconditionally -- independent of sidebar visibility -- so
notifications and redraws both stay correct whether or not any sidebar
window is currently on screen."
  (edmacs-sidebar-agents--observe-changes keys)
  (edmacs-sidebar-agents--redraw-all)
  (edmacs-sidebar-agents--ensure-elapsed-timer))

(add-hook 'edmacs-agents-changed-hook #'edmacs-sidebar-agents--on-agents-changed)

;; ============================================================================
;; Collapsed sidebar strip: agent-status summary
;; ============================================================================

(defun edmacs-sidebar-agents--non-idle-p (agent)
  "Return non-nil when AGENT has non-idle status (working, waiting, or done)."
  (not (eq (edmacs-agent-status agent) 'idle)))

(defun edmacs-sidebar-agents--collapsed-section (frame width)
  "Insert agent statuses into the current buffer's collapsed sidebar strip for FRAME.
Inserts one line per agent with non-idle status, or nothing when no such
agents exist. Each line is glyph + status indicator, applying the status face.
Fits each line independently with `edmacs-sidebar--fit' to respect double-width
nerd-font glyphs within WIDTH columns.
FRAME is currently unused (all agents are frame-independent); accepted
for registration on `edmacs-sidebar-collapsed-section-functions' compatibility."
  (ignore frame)
  (let ((agents (seq-filter #'edmacs-sidebar-agents--non-idle-p
                             (edmacs-sidebar-agents--all))))
    (when agents
      (dolist (agent (sort (copy-sequence agents) #'edmacs-sidebar-agents--compare))
        (let* ((status (edmacs-agent-status agent))
               (glyph (edmacs-sidebar-agents--glyph status))
               (face (edmacs-sidebar-agents--status-face status))
               ;; Format: glyph + one-char status indicator
               (line (format "%s %s" glyph (substring (symbol-name status) 0 1)))
               ;; Fit each line independently using edmacs-sidebar--fit
               (fitted (edmacs-sidebar--fit line width))
               (propertized (if face (propertize fitted 'face face) fitted)))
          (insert propertized "\n"))))))

(add-hook 'edmacs-sidebar-collapsed-section-functions
          #'edmacs-sidebar-agents--collapsed-section
          nil)  ;; Prepend (nil = default prepend position)

(provide 'sidebar-agents)
;;; sidebar-agents.el ends here
