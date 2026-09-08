;;; sidebar.el --- Per-frame tab list in a left side window -*- lexical-binding: t -*-

;;; Commentary:
;; One singleton `magit-section-mode' buffer (`*sidebar*'), shown in a left
;; side window at slot 0 on whichever frame(s) ask for it, listing either a
;; flat list of `tab-bar-tabs' or -- since edmacs-tab-groups phase 3 -- a
;; grouped tree of every open project. `tab-bar-mode' stays on as the model
;; -- only its strip (`tab-bar-show') is hidden -- so every `SPC T' binding
;; sessions.el already provides keeps working unchanged; this module only
;; adds the visual list and an `SPC t s' toggle.
;;
;; A redraw is two halves: `edmacs-sidebar--plan' answers with a pure tree
;; of row plists (see the plan-row contract further down), and
;; `edmacs-sidebar--render' is the only thing that inserts text. Section
;; contributions from the section hooks below stay INSERTION hooks,
;; carried in the plan as opaque `hook' rows the renderer runs at the
;; same points with the same arguments.
;;
;; A frame carrying at least one tab-bar GROUP (`edmacs-workspaces-groups',
;; workspaces.el) instead plans `edmacs-sidebar--plan-projects': one
;; top-level row per project GROUP, its main worktree's own row, with every
;; other OPEN tab in that group nested underneath as a worktree child row --
;; never a tab-less one; an unopened worktree is reached through `C-x t p'
;; instead, not this tree (edmacs-tab-groups phase 3, superseding this
;; file's original per-repo/tabless-row model). `RET' (`edmacs-sidebar-
;; activate') selects an already-open tab via `edmacs-workspaces-find-tab'/
;; `-select-tab', or -- on a project row whose main tab isn't open -- opens
;; one via `edmacs-workspaces-open-project'; a worktree child row's value is
;; always backed by an open tab, so it can never reach that fallback. `d'
;; (`edmacs-sidebar-close-worktree') closes an open row's tab and no-ops
;; when none is open. A frame with no tab-bar group at all (the daemon's
;; boot/spare frame) keeps the original flat tab list.
;;
;; The sidebar claims the LEFT element of `window-sides-slots' (cap 1)
;; through `modules/windows.el's `edmacs-windows-claim-side', the single
;; writer of that variable.
;;
;; Phase 8's "no subprocess frames on the render path" AC is checked two
;; ways. `sidebar-test.el's `edmacs-sidebar-test-redraw-and-hooks-never-
;; shell-out' is the automated, CI-equivalent guard (advises every
;; subprocess primitive to signal instead of run, then drives every
;; redraw/command path many times over) and is the actual evidence relied
;; on. A literal `M-x profiler-start' pass was also attempted on this
;; machine (GNU Emacs 31.1, emacs-plus@31/Homebrew) driving that same
;; loop for a real 60-second wall-clock run, in `--batch', in `--batch'
;; under a `script'-attached pty, and with a plain CPU-bound loop
;; containing no sidebar code at all as a control: `profiler-cpu-log' and
;; `profiler-memory-log' both come back as an empty hash table (0
;; samples) in every case, including the control -- this build's sampling
;; profiler records nothing under any non-GUI invocation on this machine,
;; not something specific to sidebar.el. A real sample-bearing profiler
;; run therefore needs an actual interactive GUI frame, outside what an
;; unattended worktree/batch pass can drive; the advice-guard test is
;; this codebase's substitute, per the same "automate the underlying
;; property, not the literal manual step" approach used elsewhere in this
;; codebase.
;;
;; One more of phase 8's ACs is met only partially, deliberately, and by
;; design (not oversight):
;;
;; - "Faces ... text fallback renders sensibly in a terminal frame" has
;;   two halves. Which glyph table gets SELECTED (nerd-icons vs. plain
;;   Unicode, including the force-text override and the nerd-icons-
;;   errors-at-runtime fallback) is fully unit-tested. Whether the
;;   plain-Unicode glyphs (● ○ ⋯ for tabs; * ? v o for agent status)
;;   read as sensible once actually painted in a real tty is a
;;   rendered-appearance judgment no `-Q --batch' ERT run can make on
;;   its own -- but it has now had an actual terminal spot-check: a real
;;   `emacs -Q -nw' tty frame, attached to a `tmux' pane under this
;;   machine's normal `en_US.UTF-8' locale, rendered every fallback
;;   glyph from both `edmacs-sidebar--fallback-glyphs' and
;;   `edmacs-sidebar-agents--fallback-glyphs' captured via
;;   `tmux capture-pane' -- every glyph came through as its intended
;;   character (no tofu boxes, no terminal-coding-system mangling to
;;   `?'). That check is manual/interactive by nature (a `-Q --batch'
;;   run has no tty to paint into at all), so it is not wired into the
;;   ERT suite and must be re-run by hand if these glyph tables change;
;;   it is otherwise the same "spot-check once, document the result"
;;   treatment the no-shellout AC's literal-profiler attempt above got.

;;; Code:

(require 'tab-bar)
(require 'desktop)
(require 'cl-lib)
(require 'windows)

;; `modules/git.el's `use-package magit :commands (...)' only activates
;; magit's autoloads file, which does not autoload `magit-section-mode',
;; `magit-insert-section', or `magit-insert-heading' (confirmed against
;; straight/build/magit-section/magit-section-autoloads.el) -- so without
;; this, `define-derived-mode' below parses fine but throws void-function
;; the first time `edmacs-sidebar-mode' actually turns on. This require
;; resolves cleanly at real init.el load time because straight already
;; puts a built package's directory (and its transitive deps: compat,
;; cond-let, llama, transient, seq) on `load-path' at build/registration
;; time, independent of magit's own `:commands' deferral. Under `-Q
;; --batch' (this module's own test harness), sidebar-test.el fixes
;; `load-path' before loading this file.
(require 'magit-section)

;; `general' loads only in a real init.el session (see the `SPC t s'
;; binding near the end of this file, gated by `with-eval-after-load');
;; declared here so the byte-compiler doesn't warn about that forward
;; reference.
(declare-function general-define-key "general")

;; workspaces.el loads BEFORE sidebar.el (see init.el's `load-module'
;; order), so every one of these resolves at runtime. They are declared
;; rather than `require'd because this file must stay loadable standalone
;; under `-Q --batch' for sidebar-test.el, which never loads workspaces.el
;; at all. This is the one lookup surface: no module resolves a tab from a
;; root except through these.
(declare-function edmacs-workspaces-groups "workspaces")
(declare-function edmacs-workspaces-tabs-in-group "workspaces")
(declare-function edmacs-workspaces-tab-root "workspaces")
(declare-function edmacs-workspaces-find-tab "workspaces")
(declare-function edmacs-workspaces-select-tab "workspaces")
(declare-function edmacs-workspaces-classify-root "workspaces")
(declare-function edmacs-workspaces-open-project "workspaces")
(declare-function edmacs-workspaces-open-worktree "workspaces")
(declare-function edmacs-workspaces-current-group "workspaces")
(declare-function edmacs-workspaces-frame-usable-p "workspaces")
(declare-function edmacs-workspaces-tab-number "workspaces")
(declare-function edmacs-workspaces-main-root "workspaces")

;; git-common-dir.el loads BEFORE sidebar.el (init.el's `load-module'
;; order), so these resolve at real load time too; declared anyway for
;; this file's own standalone `-Q --batch' test harness, which does not
;; always load it first. `edmacs-git-common-dir'/`-main-worktree' are
;; what a project row whose main tab isn't open derives its own root
;; from, directly -- see `edmacs-sidebar--derive-main-root' -- rather
;; than adding a new workspaces.el function of its own for it.
(declare-function edmacs-git-common-dir "git-common-dir")
(declare-function edmacs-git-common-dir-main-worktree "git-common-dir")
(declare-function edmacs-git-common-dir-repo-name "git-common-dir")

;; sidebar-agents.el (phase 6) loads AFTER this file (init.el's
;; `load-module' order); these commands are only ever reached
;; through the keymap below, resolved at keypress time.
(declare-function edmacs-sidebar-agents-visit "sidebar-agents")
(declare-function edmacs-sidebar-agents-toggle-all "sidebar-agents")
(declare-function edmacs-sidebar-agents-rename "sidebar-agents")
(declare-function edmacs-sidebar-agents-kill "sidebar-agents")

;; Autoloaded by Emacs 29+ core (`describe-keymap.el'); declared here so
;; the byte-compiler has no forward-reference warning on a build whose
;; autoloads have not yet been regenerated.
(declare-function which-key-show-full-keymap "which-key")

;; evil loads only in a real init.el session; the keymap block below runs
;; under `with-eval-after-load'. Declared so this file byte-compiles clean
;; with evil absent from `load-path'.
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-set-initial-state "evil-core" (mode state))

;; agents.el loads AFTER this file too; used by
;; `edmacs-sidebar--find-agent-section' below to key an agent row on its
;; stable struct field rather than its rendered (and frequently-changing)
;; label text, and by sidebar-agents.el's `magit-section-ident-value'
;; specializer on `edmacs-sidebar-agent-section' for the same reason.
(declare-function edmacs-agent-key "agents")

;; sidebar-buffers.el (phase 7) loads AFTER this file; these three
;; commands are only ever reached through the keymap below or the RET/d
;; dispatch, resolved at keypress time.
(declare-function edmacs-sidebar-buffers-visit "sidebar-buffers")
(declare-function edmacs-sidebar-buffers-kill "sidebar-buffers")
(declare-function edmacs-sidebar-buffers-next "sidebar-buffers")
(declare-function edmacs-sidebar-buffers-prev "sidebar-buffers")
(declare-function edmacs-sidebar-buffers-toggle-flat "sidebar-buffers")

;; ============================================================================
;; Extension points for sidebar-agents.el (phase 6)
;; ============================================================================
;; Kept here, rather than sidebar.el reaching into sidebar-agents.el
;; directly, so this file stays agent-agnostic -- exactly the same
;; swappable-seam convention claude-term-registry.el uses for
;; `claude-term-registry-state-accessor'.

(defvar edmacs-sidebar-worktree-label-suffix-function #'ignore
  "Function of one argument, a worktree ROOT (truename), returning a
string to append to that worktree's own row label, or nil.
sidebar-agents.el reassigns this to append its per-worktree agent
count.")

(defvar edmacs-sidebar-worktree-section-functions nil
  "Hook run with (ROOT HAS-TAB FRAME TAB-NUMBER) from inside each project
or worktree row's own section body -- the plan carries it as a `hook'
row among that row's :children and `edmacs-sidebar--render' runs it
there, before the row's `magit-insert-section' closes. ROOT is that row's
own worktree truename (a project row's main worktree root, possibly
derived rather than backed by an open tab), HAS-TAB is non-nil when
ROOT has an open tab (a worktree-child row's is always non-nil; a
project row's is nil when its main tab isn't open), FRAME is the frame
being redrawn, and TAB-NUMBER is that tab's 1-based `tab-bar-tabs' index
(nil when HAS-TAB is nil). Lets sidebar-agents.el append its own
`agents' child section, and sidebar-buffers.el its own `buffers' child
section, as real magit children of the row's own section -- not
top-level siblings following it -- without this file needing to know
anything about agents or buffers.")

(defvar edmacs-sidebar-extra-section-functions nil
  "Hook run with FRAME at the end of `edmacs-sidebar--redraw', after
every other section. Lets sidebar-agents.el append its own
frame-independent ALL AGENTS section.")

(defvar edmacs-sidebar-header-line-function #'ignore
  "Function of one argument, a FRAME, returning a suffix string to
append to that frame's own repo-name header line, or nil.
sidebar-agents.el reassigns this to append a repo-wide agent-status
roll-up -- the same swappable-seam convention
`edmacs-sidebar-worktree-label-suffix-function' uses.")

(defvar edmacs-sidebar-collapsed-section-functions nil
  "Hook run with (FRAME WIDTH) in place of every other section when
FRAME's sidebar is collapsed (see `edmacs-sidebar--redraw') -- WIDTH is
`edmacs-sidebar--collapsed-width'. Lets a later phase render the
collapsed strip's own content without this file depending on it, the
same swappable-seam convention `edmacs-sidebar-extra-section-functions'
uses.")

(defvar edmacs-sidebar-bottom-anchor-section-functions nil
  "Hook run with FRAME at the end of `edmacs-sidebar--redraw', right
after `edmacs-sidebar-extra-section-functions'. Whatever this hook
inserts is then pinned to FRAME's sidebar window's bottom edge by
`edmacs-sidebar--anchor-region-to-bottom' -- padded with blank filler
when the rest of the buffer is shorter than the window, or kept in view
by forcing `window-start' past the overflow when it is taller. Lets a
registrant keep its own section visually anchored to the bottom of the
sidebar without this file knowing anything about that section's content.")

(defvar edmacs-sidebar-visibility-functions nil
  "Abnormal hook run with (FRAME STATE) whenever FRAME's sidebar is
shown or hidden -- STATE is the symbol `shown' or `hidden'. Runs at the
tail of `edmacs-sidebar-show'/`edmacs-sidebar-hide', after the window
work, on every return path including the ones that produced no window.
Lets another module react to the sidebar appearing or disappearing
without advising either function.

A member must not itself show or hide the sidebar: both functions run
this hook unconditionally, so a member that re-enters one of them
recurses. The same constraint `edmacs-windows-frame-repaired-functions'
carries.")

(defvar edmacs-sidebar-collapsed-bottom-anchor-section-functions nil
  "Hook run with (FRAME WIDTH) at the end of the collapsed branch of
`edmacs-sidebar--redraw', right after
`edmacs-sidebar-collapsed-section-functions'. Same bottom-anchoring
treatment as `edmacs-sidebar-bottom-anchor-section-functions', for the
collapsed strip.")

;; Seven ad-hoc extension seams now live on this file: the five above,
;; plus these two bottom-anchor hooks (phase 13). Considered collapsing
;; them into one section-contribution protocol keyed by a named
;; position/slot, but deferred -- doing that well would grow this phase
;; past its own seam-contract fix. Left as a decision for a later phase
;; or a filed rdm task, not acted on now.

;; ============================================================================
;; Faces
;; ============================================================================

(defgroup edmacs-sidebar nil
  "Per-frame tab/worktree list in a left side window."
  :group 'convenience)

(defface edmacs-sidebar-missing-worktree-face
  '((t :inherit warning))
  "Face for an open tab whose worktree directory no longer exists."
  :group 'edmacs-sidebar)

(defface edmacs-sidebar-current-tab-face
  '((t :inherit magit-section-heading))
  "Face for the current tab's own row label."
  :group 'edmacs-sidebar)

(defface edmacs-sidebar-worktree-child-face
  '((t :inherit shadow))
  "Face for a worktree child row's own label when it is not the frame's
literal current tab -- gives a project's nested worktree rows a hue
distinct from the project row's own plain/current-tab pairing (AC3's
\"worktree hue\" vs \"project hue\"). Superseded by
`edmacs-sidebar-current-tab-face' when the row's own tab IS the
frame's current one."
  :group 'edmacs-sidebar)

(defface edmacs-sidebar-header-face
  '((t :inherit magit-section-heading))
  "Face for the sidebar buffer's header line."
  :group 'edmacs-sidebar)

;; ============================================================================
;; Customization: width, glyphs
;; ============================================================================

(defcustom edmacs-sidebar-width 32
  "Default TOTAL width, in columns, of the sidebar side window --
`window-total-width', not `window-body-width': the same convention
`edmacs-sidebar-remembered-width' holds, since both are read by
`edmacs-sidebar-show' and enforced via `edmacs-sidebar--enforce-width',
which resizes the live window's `window-total-width' to match exactly.
Overridden per frame once the user manually resizes the window -- see
`edmacs-sidebar--on-window-size-change'. Always subject to
`edmacs-sidebar--clamp-width' before it reaches a live window."
  :type 'integer
  :group 'edmacs-sidebar)

(defcustom edmacs-sidebar-max-width-fraction 0.33
  "Maximum fraction of the frame's total width the sidebar may occupy.
Enforced by `edmacs-sidebar--clamp-width' both when a width is stashed
into `edmacs-sidebar-remembered-width' and when one is read back out in
`edmacs-sidebar-show' -- so a value already poisoned in a live frame
parameter or a restored desktop self-heals on the next show rather than
persisting forever."
  :type 'float
  :group 'edmacs-sidebar)

(defvar edmacs-sidebar--min-width 15
  "Floor, in columns, below which a glyph plus a truncated name no
longer fits usefully. Wins over `edmacs-sidebar-max-width-fraction' on a
pathologically narrow frame, where the fraction-derived cap would be
smaller than this floor. A plain `defvar', not `defcustom', so a test
can shrink it -- mirrors `edmacs-sidebar-resize-debounce-seconds's own
convention.")

(defun edmacs-sidebar--clamp-width (width frame)
  "Clamp WIDTH to `edmacs-sidebar-max-width-fraction' of FRAME's width,
floored at `edmacs-sidebar--min-width'."
  (max edmacs-sidebar--min-width
       (min width (floor (* (frame-width frame) edmacs-sidebar-max-width-fraction)))))

(defvar edmacs-sidebar--collapsed-width 4
  "Width, in columns, of the collapsed sidebar strip.
Deliberately narrower than `edmacs-sidebar--min-width' -- a collapsed
strip has no truncated label to fit, just a glyph or two -- so
`edmacs-sidebar-show' branches to this value directly rather than
routing it through `edmacs-sidebar--clamp-width', whose unconditional
floor would otherwise widen it right back out.")

(defcustom edmacs-sidebar-force-text-glyphs nil
  "Non-nil forces the plain text/Unicode marker glyphs everywhere in the
sidebar, even when `nerd-icons' is loaded. Useful for a terminal frame
where nerd-icons's private-use-area glyphs render as unreadable boxes."
  :type 'boolean
  :group 'edmacs-sidebar)

;; ============================================================================
;; Major mode
;; ============================================================================

(defface edmacs-sidebar-background-face
  '((((background dark)) :background "#00212b")
    (((background light)) :background "#eee8d5")
    (t :inherit default))
  "Background for the sidebar window, one step off the frame's own.
The sidebar is chrome, not content, and reads as a separate surface only
if it is shaded differently -- solarized gives every buffer the same
`default' background, so without this the sidebar is indistinguishable
from the buffer beside it. Applied buffer-locally via
`face-remapping-alist' rather than by setting `default', so it cannot
leak into any other buffer."
  :group 'edmacs-sidebar)

(define-derived-mode edmacs-sidebar-mode magit-section-mode "Sidebar"
  "Major mode listing the current frame's tabs in a side window."
  ;; Remap rather than set: `default' is frame-wide, and setting it here
  ;; would repaint every window on the frame. `fringe' and `header-line'
  ;; need their own entries -- remapping `default' does not reach either,
  ;; so the window's side edges and its top strip stayed the frame colour
  ;; and framed the sidebar in the wrong shade. The window has no mode
  ;; line (`mode-line-format' window-parameter is `none'), so its bottom
  ;; is ordinary buffer area and the `default' entry already covers it.
  (setq-local face-remapping-alist
              (append '((default edmacs-sidebar-background-face)
                        (fringe edmacs-sidebar-background-face)
                        (header-line edmacs-sidebar-background-face))
                      (seq-remove
                       (lambda (entry)
                         (memq (car-safe entry) '(default fringe header-line)))
                       face-remapping-alist))))

;; Plain `define-key' on the mode's own local map is not enough by
;; itself: evil installs its state keymaps via `emulation-mode-map-alists',
;; which key lookup consults BEFORE the buffer's local map, and
;; `evil-motion-state-map' already binds RET to `evil-ret' -- so with
;; only this, RET in a motion-state sidebar buffer resolves to
;; `evil-ret', never reaching `edmacs-sidebar-activate' (verified live:
;; `(key-binding (kbd "RET"))' in a motion-state sidebar buffer returned
;; `evil-ret' despite `(lookup-key edmacs-sidebar-mode-map (kbd "RET"))'
;; correctly showing the intended binding present but unreachable). `q'
;; happens to work today only because `evil-motion-state-map' has no
;; binding for it (`evil-record-macro' on `q' lives in
;; `evil-normal-state-map', not motion) -- a coincidence, not a
;; mechanism, so it gets the same real fix below rather than relying on
;; that continuing to hold.
;;
;; `edmacs-sidebar-mode-map' is this mode's own dedicated map (not a
;; shared package-global one), so `evil-define-key' on it -- landing in
;; evil's AUXILIARY-MAPS bucket, which wins over the main motion-state
;; map -- is exactly git.el's `git-timemachine-mode-map' pattern, safely
;; scoped to sidebar buffers only.
(defconst edmacs-sidebar--bindings
  '(("RET" . edmacs-sidebar-visit-at-point)
    ("q"   . edmacs-sidebar-hide)
    ("d"   . edmacs-sidebar-kill-at-point)
    ("a"   . edmacs-sidebar-agents-toggle-all)
    ("["   . edmacs-sidebar-buffers-prev)
    ("]"   . edmacs-sidebar-buffers-next)
    ("s"   . edmacs-sidebar-buffers-toggle-flat)
    ("J"   . edmacs-sidebar-move-to-next-worktree)
    ("K"   . edmacs-sidebar-move-to-prev-worktree)
    ("r"   . edmacs-sidebar-rename-at-point)
    ("g r" . edmacs-sidebar-redraw)
    ("?"   . edmacs-sidebar-help)
    ("z"   . edmacs-sidebar-toggle-collapse)
    ;; TAB and `C-i' are the same event in a non-GUI/tty keymap lookup, and
    ;; `evil-motion-state-map' binds `C-i' to `evil-jump-forward' regardless
    ;; of `evil-want-C-i-jump' (that variable only governs whether evil
    ;; claims plain `TAB' too under a GUI frame, where the two differ) -- so
    ;; TAB needs the same dual-binding override as RET/q/K/?. Bound to
    ;; `edmacs-sidebar-toggle-at-point', not plain `magit-section-toggle', so
    ;; a leaf row (agent/buffer) folds its enclosing group instead of
    ;; toggling its own bodyless heading as a no-op.
    ("TAB" . edmacs-sidebar-toggle-at-point))
  "The sidebar's key bindings, as (KEY-DESCRIPTION . COMMAND) pairs.
One list, consumed twice: by `define-key' on
`edmacs-sidebar-mode-map' and, once evil loads, by
`evil-define-key*' in evil's motion state.")

(dolist (binding edmacs-sidebar--bindings)
  (define-key edmacs-sidebar-mode-map (kbd (car binding)) (cdr binding)))

(with-eval-after-load 'evil
  (dolist (binding edmacs-sidebar--bindings)
    (evil-define-key* 'motion edmacs-sidebar-mode-map
                      (kbd (car binding)) (cdr binding)))
  (evil-set-initial-state 'edmacs-sidebar-mode 'motion))

;; ============================================================================
;; Per-frame buffer management
;; ============================================================================

(defconst edmacs-sidebar--buffer-name "*sidebar*"
  "The sidebar's single buffer name.
Since edmacs-tab-groups phase 3 there is one grouped tree showing every
project, so one frame's sidebar is the same content as any other's --
there is no more per-frame `*sidebar: <repo>*' name to collide on or
drift out of sync with a frame's title.")

(defun edmacs-sidebar--buffer (frame)
  "Return the singleton sidebar buffer, or nil if it does not exist yet.
FRAME is accepted (and ignored) so every existing call site -- written
against the old per-frame notion -- keeps working unchanged; every
frame answers with the same buffer object."
  (ignore frame)
  (get-buffer edmacs-sidebar--buffer-name))

(defun edmacs-sidebar--ensure-buffer (frame)
  "Return the live, freshly redrawn singleton sidebar buffer.
Creates it, lazily, the first time any frame needs it -- not eagerly at
load time. `edmacs-sidebar--redraw' always renders FRAME's own tabs/
groups into this one buffer, so showing the sidebar on a second frame
redraws the same buffer with that frame's content instead of FRAME's."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (unless (buffer-live-p buf)
      (setq buf (generate-new-buffer edmacs-sidebar--buffer-name))
      (with-current-buffer buf
        (edmacs-sidebar-mode)))
    (edmacs-sidebar--redraw frame)
    buf))

(defun edmacs-sidebar--cleanup-frame (frame)
  "Kill the singleton sidebar buffer when FRAME's delete leaves no other
live frame still showing it.
The buffer is shared across every frame now (see
`edmacs-sidebar--buffer-name's docstring), so deleting one of several
frames must not strand the rest with no sidebar -- \"still showing it\"
is checked via `get-buffer-window-list' across every live frame's
windows, not a per-frame parameter."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (and (buffer-live-p buf)
               (null (seq-remove (lambda (w) (eq (window-frame w) frame))
                                  (get-buffer-window-list buf nil t))))
      (kill-buffer buf))))

(add-hook 'delete-frame-functions #'edmacs-sidebar--cleanup-frame)

(defun edmacs-sidebar-redraw-frames ()
  "Return the frames whose content may be rendered into the sidebar buffer.
There is one `*sidebar*' buffer for every frame (see
`edmacs-sidebar--buffer-name'), so a caller that redraws \"every frame\"
writes that one buffer once per frame and the LAST frame wins. Excluding
what `edmacs-workspaces-frame-usable-p' rejects keeps the daemon's tty
placeholder -- whose identity and tab set no on-screen frame is showing --
from being that last writer."
  (seq-filter #'edmacs-workspaces-frame-usable-p (frame-list)))

;; ============================================================================
;; Coalesced invalidation: one dirty flag per frame, one shared idle-0 flush
;; ============================================================================
;; A burst of N triggers within one command loop (N stack pushes, a rename
;; racing a tab select, ...) collapses onto the single pending timer below,
;; so the frame is redrawn once when Emacs next goes idle. The buffer-list
;; and agents hooks keep their own separately-debounced paths, and the
;; tab-open/desktop-read/frame-creation triggers call `edmacs-sidebar-show'
;; because they must create a window synchronously.

(defvar edmacs-sidebar--dirty-frames nil
  "Frames marked dirty by `edmacs-sidebar-invalidate', awaiting one
coalesced redraw from `edmacs-sidebar--flush-dirty-frames'.")

(defvar edmacs-sidebar--redraw-timer nil
  "The single pending idle-0 timer that will run
`edmacs-sidebar--flush-dirty-frames', or nil when none is pending. One
shared timer, not one per frame: every frame invalidated within the same
command loop collapses onto this one pending flush.")

(defun edmacs-sidebar--flush-dirty-frames ()
  "Redraw every still-live frame in `edmacs-sidebar--dirty-frames' once,
then clear both.
Clears `edmacs-sidebar--redraw-timer' and swaps `--dirty-frames' out to a
local FIRST, before redrawing anything: a redraw that itself triggers
`edmacs-sidebar-invalidate' (e.g. via a hook fired by its own buffer
changes) must schedule a fresh timer, not silently add to a set this
function is about to clear anyway. A frame deleted between invalidation
and this flush is skipped, never redrawn."
  (setq edmacs-sidebar--redraw-timer nil)
  (let ((frames edmacs-sidebar--dirty-frames))
    (setq edmacs-sidebar--dirty-frames nil)
    (dolist (frame frames)
      (when (frame-live-p frame)
        (edmacs-sidebar--redraw frame)))))

(defun edmacs-sidebar-invalidate (&optional frame)
  "Mark FRAME's (default the selected frame) sidebar dirty for one
coalesced redraw, scheduling `edmacs-sidebar--flush-dirty-frames' via a
zero-second idle timer if none is already pending.
No-ops entirely -- marks nothing dirty, schedules no timer -- when FRAME
is not among `edmacs-sidebar-redraw-frames'; checked here, at
invalidation time, so a frame this config will never redraw (the
daemon's tty placeholder above all) never causes timer churn either."
  (let ((frame (or frame (selected-frame))))
    (when (memq frame (edmacs-sidebar-redraw-frames))
      (unless (memq frame edmacs-sidebar--dirty-frames)
        (push frame edmacs-sidebar--dirty-frames))
      (unless edmacs-sidebar--redraw-timer
        (setq edmacs-sidebar--redraw-timer
              (run-with-idle-timer 0 nil #'edmacs-sidebar--flush-dirty-frames))))))

;; ============================================================================
;; Rendering
;; ============================================================================
;; Tab names are read verbatim from each tab alist's own precomputed `name'
;; field -- never recomputed via `edmacs-sessions--tab-name'/`project-current'
;; -- so redraw does no subprocess or directory-stat work.

(defun edmacs-sidebar--map-sections (section fn)
  "Call FN on SECTION and, recursively, on every descendant."
  (when section
    (funcall fn section)
    (dolist (child (oref section children))
      (edmacs-sidebar--map-sections child fn))))

(defun edmacs-sidebar--find-agent-section (key)
  "Return the `edmacs-sidebar-agent' section (anywhere in the current
buffer's section tree) whose agent's KEY matches, or nil."
  (catch 'edmacs-sidebar--found-agent-section
    (edmacs-sidebar--map-sections
     magit-root-section
     (lambda (section)
       (when (and (eq (oref section type) 'edmacs-sidebar-agent)
                  (slot-boundp section 'value)
                  (equal (edmacs-agent-key (oref section value)) key))
         (throw 'edmacs-sidebar--found-agent-section section))))
    nil))

(defun edmacs-sidebar--find-buffer-section (name)
  "Return the `edmacs-sidebar-buffers-file'/`-special' section (anywhere
in the current buffer's section tree) whose buffer's name is NAME, or nil."
  (catch 'edmacs-sidebar--found-buffer-section
    (edmacs-sidebar--map-sections
     magit-root-section
     (lambda (section)
       (when (and (memq (oref section type) '(edmacs-sidebar-buffers-file edmacs-sidebar-buffers-special))
                  (slot-boundp section 'value)
                  (buffer-live-p (oref section value))
                  (equal (buffer-name (oref section value)) name))
         (throw 'edmacs-sidebar--found-buffer-section section))))
    nil))

(defun edmacs-sidebar--capture-positions (buf)
  "Capture BUF's per-window point/scroll state ahead of a redraw.
Modeled on magit-mode.el's `magit--refresh-buffer-get-positions': for
each live window showing BUF, record that window, the section at point,
its position relative to that section (`magit-section-get-relative-
position'), and -- separately -- the section at the window's
`window-start', that section's own relative position, and the raw
`window-start' integer.

Relies entirely on `magit-section-ident' stability (via
`magit-section-goto-successor' in `edmacs-sidebar--restore-positions')
rather than any bespoke per-row-type identity scheme: a project or
worktree-child row's own value is an `equal'-stable `(GROUP . ROOT)'
cons (the plan row's own :value, see `edmacs-sidebar--plan-projects'),
an agents-group's section value is the
bare, `equal'-stable root string (`edmacs-sidebar-agents--insert-group'),
a buffer row's value is already
a stable buffer object, and an agent row's value -- a raw `edmacs-agent'
struct that `edmacs-agents-set-status' rebuilds fresh on every change --
gets a `magit-section-ident-value' specializer of its own
(`edmacs-sidebar-agent-section' in sidebar-agents.el) keyed on
`edmacs-agent-key' instead.

Falls back to a single window-less entry, keyed on BUF's own (buffer-
local, window-independent) point, when BUF has no live window at all --
e.g. a redraw while the sidebar is hidden, or in an ERT fixture that
never displays the buffer. Returns nil only when even that fallback
finds no section at point (an empty buffer)."
  (with-current-buffer buf
    (or (mapcan
         (lambda (window)
           (with-selected-window window
             (with-current-buffer buf
               (when-let* ((section (magit-section-at)))
                 (list (list window
                             section
                             (magit-section-get-relative-position section)
                             (when-let* ((ws-section (magit-section-at (window-start))))
                               (list ws-section
                                     (car (magit-section-get-relative-position ws-section))
                                     (window-start)))))))))
         (get-buffer-window-list buf nil t))
        (when-let* ((section (magit-section-at)))
          (list (list nil section (magit-section-get-relative-position section) nil))))))

(defun edmacs-sidebar--restore-positions (positions)
  "Restore POSITIONS, as captured by `edmacs-sidebar--capture-positions',
against the just-redrawn buffer -- modeled on magit-mode.el's
`magit--refresh-buffer-set-positions', dropped down to this buffer's
simpler single-buffer-type case (no hunk/log-mode special cases apply
here). A window-less entry (see `edmacs-sidebar--capture-positions's
own fallback) moves the current buffer's own point directly, with no
window involved at all. For each still-live window, moves point to the
successor of the captured section (`magit-section-goto-successor',
which tries the same ident first and falls back to a related section),
then restores WINDOW-START from the second capture -- unless point's
own successor already scrolled past the captured start, or the
window-start section's successor can't be found either, in which case
the window is left wherever its own redisplay puts it, exactly as
before."
  (pcase-dolist (`(,window ,section ,relative ,ws) positions)
    (if (null window)
        (apply #'magit-section-goto-successor section relative)
      (when (window-live-p window)
        (with-selected-window window
          (apply #'magit-section-goto-successor section relative)
          (pcase ws
            (`(,ws-section ,ws-line ,window-start)
             (cond
              ((> window-start (point)))
              ((magit-section-equal ws-section (magit-section-at window-start))
               (set-window-start window window-start t))
              ((when-let* ((pos (save-excursion
                                   (and (magit-section-goto-successor--same ws-section ws-line 0)
                                        (point)))))
                 (set-window-start window pos t)))))))))))

;; ============================================================================
;; Glyphs: nerd-icons with a plain-text fallback
;; ============================================================================

(defconst edmacs-sidebar--fallback-glyphs
  '((current-tab . "●") (open-tab . "○")
    (roadmap-worktree . "◆") (task-worktree . "◇") (worktree . "·"))
  "Plain-Unicode fallback marker glyph per row KIND.
`current-tab'/`open-tab' mark a project row's active/inactive state
\(AC2\); `roadmap-worktree'/`task-worktree'/`worktree' mark a worktree
child row's own kind, from `edmacs-workspaces-classify-root' (AC3) --
see `edmacs-sidebar--worktree-kind-glyph-key'.")

(declare-function nerd-icons-octicon "nerd-icons")

(defun edmacs-sidebar--nerd-icon (kind)
  "Return KIND's nerd-icons glyph, or nil when unavailable.
Never signals: an icon lookup failing falls back to plain text exactly
as if nerd-icons were not loaded at all -- mirrors
`edmacs-sidebar-agents--nerd-icon's own convention."
  (and (not edmacs-sidebar-force-text-glyphs)
       (featurep 'nerd-icons)
       (condition-case nil
           (pcase kind
             ('current-tab (and (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-arrow_right")))
             ('open-tab (and (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-circle")))
             ('roadmap-worktree (and (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-git_branch")))
             ('task-worktree (and (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-checklist")))
             ('worktree (and (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-file_directory")))
             (_ nil))
         (error nil))))

(defun edmacs-sidebar--worktree-kind-glyph-key (classification)
  "Map CLASSIFICATION (`edmacs-workspaces-classify-root's `roadmap'/
`task'/nil) to this file's own glyph-table KIND key."
  (pcase classification
    ('roadmap 'roadmap-worktree)
    ('task 'task-worktree)
    (_ 'worktree)))

(defun edmacs-sidebar--glyph (kind)
  "Return the display glyph for KIND: a nerd-icon if available and not
forced off by `edmacs-sidebar-force-text-glyphs', else the plain-Unicode
fallback from `edmacs-sidebar--fallback-glyphs'."
  (or (edmacs-sidebar--nerd-icon kind)
      (alist-get kind edmacs-sidebar--fallback-glyphs "?")))

;; ============================================================================
;; Ellipsis truncation to the sidebar window's live width
;; ============================================================================

(defun edmacs-sidebar--render-width (frame)
  "Return the column width row labels must fit into on FRAME.
The sidebar window's live `window-width' when there is one; otherwise
the frame's remembered width, or `edmacs-sidebar-width', run through
`edmacs-sidebar--clamp-width' just like every other read of the same
frame parameter (`edmacs-sidebar-show', `edmacs-sidebar--remember-width'):
otherwise a poisoned or merely larger-than-clamp remembered width would
render an untruncated label on the very first redraw of a freshly
created buffer, only to be truncated correctly from the next redraw on
once a live, clamped window exists to measure."
  (let ((window (edmacs-sidebar--window frame)))
    (if (window-live-p window)
        (window-width window)
      (edmacs-sidebar--clamp-width
       (or (frame-parameter frame 'edmacs-sidebar-remembered-width)
           edmacs-sidebar-width)
       frame))))

(defun edmacs-sidebar--truncate-label (label width)
  "Truncate LABEL with a trailing … to fit WIDTH columns.
A pure function of its two arguments -- WIDTH comes from
`edmacs-sidebar--render-width', measured once per redraw by
`edmacs-sidebar--redraw' rather than per row from inside the render
path. Measures with `string-width' via `edmacs-sidebar--fit', so a
double-width nerd-icons glyph costs the two columns it actually
occupies; a WIDTH of 0 or less yields \"\"."
  (edmacs-sidebar--fit label width))

(defun edmacs-sidebar--fit (label width)
  "Truncate LABEL with a trailing … to fit WIDTH columns.
Measures with `string-width', not `length': a collapsed strip's few
columns make a nerd-icons glyph's double display width visible -- at
~`edmacs-sidebar--collapsed-width' columns, one such glyph can overflow
by a whole column that `length' would never notice. WIDTH is
defensively floored at 0, returning \"\" rather than signaling on a
pathologically narrow strip. `edmacs-sidebar--truncate-label' is the
row-label entry point onto this."
  (let ((width (max 0 width)))
    (cond
     ((<= width 0) "")
     ((<= (string-width label) width) label)
     (t (concat (truncate-string-to-width label (max 0 (1- width))) "…")))))

(defun edmacs-sidebar--tab-label (tab)
  "Return TAB's marker-prefixed display label, unpropertized."
  (concat (edmacs-sidebar--glyph (if (eq (car tab) 'current-tab) 'current-tab 'open-tab))
          " " (alist-get 'name tab)))

;; ============================================================================
;; Grouped tree: one project row per tab-bar GROUP, worktree child rows
;; nested underneath (edmacs-tab-groups phase 3)
;; ============================================================================

(defvar edmacs-sidebar-worktree-live-p-function #'file-directory-p
  "Predicate deciding whether a stamped worktree root still exists.
Called with the root directory, once per rendered row. Must stay a
local stat, never a shell-out: `edmacs-sidebar--redraw' runs off
redisplay. Rebindable so a suite whose fixture roots name no real
directory can render them as live -- production never rebinds it.")

(defun edmacs-sidebar--root-missing-p (root)
  "Non-nil when ROOT is stamped but its directory is gone from disk.
A nil ROOT is not missing -- there is no stamp to be stale. A remote
root is never probed: `file-directory-p' on one blocks on the network,
which a redisplay-path redraw cannot afford."
  (and root
       (not (file-remote-p root))
       (not (funcall edmacs-sidebar-worktree-live-p-function root))))

(defconst edmacs-sidebar--missing-marker " (missing)"
  "Suffix appended to a row whose stamped worktree root is gone.
Concatenated before `edmacs-sidebar--truncate-label' so it competes for
the sidebar's width like any other part of the label rather than
overflowing it -- on a narrow sidebar the marker truncates away and
`edmacs-sidebar-missing-worktree-face' carries the signal alone.")

(defun edmacs-sidebar--derive-main-root (tabs)
  "Return the main worktree root shared by TABS' repo, derived directly
via `edmacs-git-common-dir'/`edmacs-git-common-dir-main-worktree' rather
than through an open main tab -- used only when no member of TABS is
itself classified `main'. Every worktree of one repo shares the same
git-common-dir, so which member of TABS anchors the lookup does not
matter. Returns nil when TABS is empty or no member's root resolves at
all -- `edmacs-git-common-dir' never signals on that, per its own
contract."
  (when-let* ((anchor (seq-some #'edmacs-workspaces-tab-root tabs))
              ;; Resolvability check only: `edmacs-workspaces-main-root'
              ;; falls back to its argument, and a project row must render
              ;; nothing rather than a worktree root here.
              (_ (edmacs-git-common-dir anchor)))
    (edmacs-workspaces-main-root anchor)))

;; ============================================================================
;; Render plan: pure data describing what a redraw will insert
;; ============================================================================
;; `edmacs-sidebar--plan' answers with a tree of ROWS -- plists, touching
;; no window and no buffer -- and `edmacs-sidebar--render' is the only
;; thing that inserts text. A row carries:
;;
;;   :kind      `project', `worktree', `tab' or `hook'
;;   :group     the tab-bar group a project/worktree row belongs to
;;   :root      that row's own worktree truename
;;   :label     the fully composed, UNtruncated label (glyph + name +
;;              " [N]" + agent suffix + " (missing)"); the renderer alone
;;              truncates, to the width it is handed
;;   :face      a face symbol, or nil for an unpropertized label. The
;;              distinction is load-bearing: project and flat tab rows
;;              propertize only when a face applies, worktree rows always
;;   :children  child rows
;;   :anchor    t on the rows making up the bottom-anchored region
;;   :value     the magit section value -- a `(GROUP . ROOT)' cons for
;;              project/worktree rows and for any `tab' row whose tab
;;              carries a root; the 1-based tab number only for a `tab'
;;              row with no root at all. Must be produced verbatim, or
;;              `edmacs-sidebar--capture-positions'/`--restore-positions'
;;              lose point and fold identity across a redraw.
;;   :hook :args  on a `hook' row only: a section-contribution hook the
;;              renderer runs in place, with those arguments. The five
;;              hooks stay INSERTION hooks; the plan records only where,
;;              and with what, each fires.
;;
;; Both project and worktree rows share the `edmacs-sidebar-tab' section
;; type (so J/K, `edmacs-sidebar--enclosing-worktree', and every RET/d/r
;; dispatch keep working unchanged) but carry a `(GROUP . ROOT)' cons as
;; their section value instead of a bare tab number -- see
;; `edmacs-sidebar-activate''s own dispatch. A row whose stamped root has
;; been deleted from disk is marked with
;; `edmacs-sidebar-missing-worktree-face' and a trailing " (missing)".

(defconst edmacs-sidebar--width 'edmacs-sidebar--width
  "Sentinel standing in for the render width inside a `hook' row's :args.
The collapsed strip's hooks are called with (FRAME WIDTH), but WIDTH is
`edmacs-sidebar--strip-width' -- a live window measurement the planner
must not make. So the plan carries this symbol and
`edmacs-sidebar--render' substitutes its own WIDTH argument for it. A
private, dedicated symbol precisely so no real hook argument can
collide with it.")

(defvar edmacs-sidebar-main-root-function #'edmacs-sidebar--derive-main-root
  "Function of one argument, a list of TABS, returning their repo's main
worktree root. The seam `edmacs-sidebar--plan-projects' reaches the git
lookup through when no member of TABS is itself classified `main'.
Defaults to the real `edmacs-sidebar--derive-main-root' -- production
still resolves this the same way, on the same path -- and is rebindable
so a plan-level test can supply a root with no git repo behind it.")

(defun edmacs-sidebar--plan-tabs (frame tabs)
  "Return one `tab' row per member of TABS, FRAME's own tab list.
The flat, group-less shape, reached only when FRAME carries no tab-bar
group at all. A row for a tab that HAS a worktree root carries the same
`(GROUP . ROOT)' cons value every project/worktree row does, so
activation resolves it by identity and a tab reorder cannot send RET to
a different tab than the row names.

A tab with no root has no such identity, so it keeps the bare 1-based
tab number -- the one reorder-stale value left in the plan, and
deliberately so: those are batch's own tab and the daemon's boot tab,
this list's real population, and a `(nil . nil)' value would turn RET on
them into a `user-error' where it used to select the tab.

FRAME is passed explicitly -- without it the number resolves against the
selected frame and comes back nil for a tab belonging to another."
  (mapcar (lambda (tab)
            (let ((root (edmacs-workspaces-tab-root tab)))
              (list :kind 'tab
                    :value (if root
                               (cons (funcall tab-bar-tab-group-function tab) root)
                             (edmacs-workspaces-tab-number tab frame))
                    :label (edmacs-sidebar--tab-label tab)
                    :face (and (eq (car tab) 'current-tab)
                               'edmacs-sidebar-current-tab-face))))
          tabs))

(defun edmacs-sidebar--plan-worktree (group tab kind frame)
  "Return the `worktree' row for TAB, nested under GROUP's project row.
This row exists only for an already-open tab, never a tab-less worktree;
KIND is `edmacs-workspaces-classify-root's `roadmap'/`task'/nil
classification of TAB's root, driving both the glyph (via
`edmacs-sidebar--worktree-kind-glyph-key') and the displayed name --
TAB's own `name' with the matching `roadmap-'/`task-' prefix stripped,
so a renamed-away tab degrades to `string-remove-prefix's own no-op."
  (let* ((root (edmacs-workspaces-tab-root tab))
         (name (alist-get 'name tab))
         (stripped (pcase kind
                     ('roadmap (string-remove-prefix "roadmap-" name))
                     ('task (string-remove-prefix "task-" name))
                     (_ name)))
         (suffix (funcall edmacs-sidebar-worktree-label-suffix-function root))
         (current-p (eq (car tab) 'current-tab))
         (missing-p (edmacs-sidebar--root-missing-p root)))
    (list :kind 'worktree
          :group group
          :root root
          :value (cons group root)
          :label (concat "  " (edmacs-sidebar--glyph
                               (edmacs-sidebar--worktree-kind-glyph-key kind))
                         " " stripped (or suffix "")
                         (and missing-p edmacs-sidebar--missing-marker))
          ;; Always a face here, unlike a project row: the child hue is
          ;; the default rather than the exception.
          :face (cond (missing-p 'edmacs-sidebar-missing-worktree-face)
                      (current-p 'edmacs-sidebar-current-tab-face)
                      (t 'edmacs-sidebar-worktree-child-face))
          :children (list (list :kind 'hook
                                :hook 'edmacs-sidebar-worktree-section-functions
                                :args (list root t frame
                                            (edmacs-workspaces-tab-number tab frame)))))))

(defun edmacs-sidebar--plan-projects (frame)
  "Return one `project' row per `edmacs-workspaces-groups' on FRAME, each
carrying that group's non-main open tabs as nested `worktree' rows.
A project row's root is its main worktree's, derived through
`edmacs-sidebar-main-root-function' when no open tab of the group is
itself the main one; a root gone from disk takes
`edmacs-sidebar-missing-worktree-face' and a \" (missing)\" marker,
outranking the current-tab face -- a row pointing at a directory that no
longer exists is the more urgent thing to say about it.
`edmacs-sidebar-activate' is the matching activation dispatch."
  (let ((active-group (edmacs-workspaces-current-group frame)))
    (mapcar
     (lambda (group)
       (let* ((tabs (edmacs-workspaces-tabs-in-group group frame))
              ;; Classify each tab ONCE. `edmacs-workspaces-classify-root'
              ;; truenames twice per call, and this runs for every frame on
              ;; every tab select; the child loop below reads the kind back
              ;; out rather than asking again.
              (kinds (mapcar (lambda (tab)
                               (cons tab
                                     (when-let* ((root (edmacs-workspaces-tab-root tab)))
                                       (edmacs-workspaces-classify-root root))))
                             tabs))
              (main-tab (car (seq-find (lambda (cell) (eq (cdr cell) 'main)) kinds)))
              (main-root (or (and main-tab (edmacs-workspaces-tab-root main-tab))
                             (funcall edmacs-sidebar-main-root-function tabs)))
              (child-tabs (if main-tab (remq main-tab tabs) tabs))
              (current-p (equal group active-group))
              (missing-p (edmacs-sidebar--root-missing-p main-root))
              (agent-suffix (funcall edmacs-sidebar-worktree-label-suffix-function main-root)))
         (list :kind 'project
               :group group
               :root main-root
               :value (cons group main-root)
               :label (concat (edmacs-sidebar--glyph (if current-p 'current-tab 'open-tab))
                              " " group (format " [%d]" (length child-tabs))
                              (or agent-suffix "")
                              (and missing-p edmacs-sidebar--missing-marker))
               :face (cond (missing-p 'edmacs-sidebar-missing-worktree-face)
                           (current-p 'edmacs-sidebar-current-tab-face))
               :children
               (cons (list :kind 'hook
                           :hook 'edmacs-sidebar-worktree-section-functions
                           :args (list main-root (and main-tab t) frame
                                       (and main-tab
                                            (edmacs-workspaces-tab-number main-tab frame))))
                     (mapcar (lambda (tab)
                               (edmacs-sidebar--plan-worktree
                                group tab (alist-get tab kinds nil nil #'eq) frame))
                             child-tabs)))))
     (edmacs-workspaces-groups frame))))

(defun edmacs-sidebar--plan-collapsed (frame)
  "Return the collapsed strip's plan: the two collapsed hook rows.
Both are called with (FRAME WIDTH); WIDTH is the
`edmacs-sidebar--width' sentinel, which `edmacs-sidebar--render'
substitutes."
  (list (list :kind 'hook
              :hook 'edmacs-sidebar-collapsed-section-functions
              :args (list frame edmacs-sidebar--width))
        (list :kind 'hook
              :hook 'edmacs-sidebar-collapsed-bottom-anchor-section-functions
              :args (list frame edmacs-sidebar--width)
              :anchor t)))

(defun edmacs-sidebar--plan (frame)
  "Return the row tree `edmacs-sidebar--redraw' will render for FRAME.
Pure: reads frame and tab-bar state, calls no window function and edits
no buffer. When FRAME carries `edmacs-sidebar-collapsed', every other
section is skipped in favour of the collapsed strip's own two hooks;
otherwise a frame carrying any tab-bar group at all
\(`edmacs-workspaces-groups') gets the projects tree and everything else
the flat tab list, each followed by the extra-section and bottom-anchor
hook rows."
  (if (frame-parameter frame 'edmacs-sidebar-collapsed)
      (edmacs-sidebar--plan-collapsed frame)
    (append (if (edmacs-workspaces-groups frame)
                (edmacs-sidebar--plan-projects frame)
              (edmacs-sidebar--plan-tabs frame (tab-bar-tabs frame)))
            (list (list :kind 'hook
                        :hook 'edmacs-sidebar-extra-section-functions
                        :args (list frame))
                  (list :kind 'hook
                        :hook 'edmacs-sidebar-bottom-anchor-section-functions
                        :args (list frame)
                        :anchor t)))))

(defun edmacs-sidebar--render (rows width)
  "Insert ROWS into the current buffer, fitting every label to WIDTH.
Wraps the whole render in the `edmacs-sidebar-root' section. Returns
the `(START . END)' buffer positions bracketing whatever the :anchor
rows inserted, or nil when they inserted nothing -- matching the
\"insert nothing when disabled\" contract their registrants follow."
  (let (anchor-start anchor-end)
    (cl-labels
        ((walk (rows)
           (dolist (row rows)
             (when (and (plist-get row :anchor) (null anchor-start))
               (setq anchor-start (point)))
             (pcase (plist-get row :kind)
               ('hook
                (apply #'run-hook-with-args
                       (plist-get row :hook)
                       (mapcar (lambda (arg)
                                 (if (eq arg edmacs-sidebar--width) width arg))
                               (plist-get row :args))))
               (_
                (magit-insert-section (edmacs-sidebar-tab (plist-get row :value))
                  (magit-insert-heading
                    (let ((label (edmacs-sidebar--truncate-label
                                  (plist-get row :label) width))
                          (face (plist-get row :face)))
                      (if face (propertize label 'face face) label)))
                  (walk (plist-get row :children)))))
             (when (plist-get row :anchor)
               (setq anchor-end (point))))))
      (magit-insert-section (edmacs-sidebar-root)
        (walk rows)))
    (and anchor-start anchor-end (> anchor-end anchor-start)
         (cons anchor-start anchor-end))))

(defun edmacs-sidebar--sanitise-frame-title (title)
  "Sanitise a frame TITLE for use as a sidebar buffer name.
Strips leading/trailing `*...*' earmuffs (e.g., `*Minibuf-1*' → empty),
removes trailing ` - Emacs' pattern, collapses whitespace, and returns
the cleaned string. Returns empty string if nothing usable remains."
  (if (null title)
      ""
    (let* (;; Remove trailing ` - Emacs' suffix first
           (without-emacs-suffix
            (if (string-match "^\\(.*?\\)\\s-*-\\s-*Emacs\\s-*$" title)
                (match-string 1 title)
              title))
           ;; Strip and remove leading/trailing *...*earmuffs iteratively
           (step1 (string-trim without-emacs-suffix))
           ;; Remove leading *...*
           (step2
            (if (string-match "^\\*[^*]*\\*\\s-*\\(.*\\)$" step1)
                (string-trim (match-string 1 step1))
              step1))
           ;; Remove trailing *...*
           (step3
            (if (string-match "^\\(.*?\\)\\s-*\\*[^*]*\\*\\s-*$" step2)
                (string-trim (match-string 1 step2))
              step2))
           ;; Collapse internal whitespace
           (collapsed (replace-regexp-in-string "\\s-+" " " step3)))
      (string-trim collapsed))))

(defun edmacs-sidebar--header-line-name (frame)
  "Return FRAME's own identity string for the header line.
The ACTIVE PROJECT's name when FRAME's current tab is in a project
group -- the group name IS the repo name, `edmacs-workspaces-group-name'
deriving it through `edmacs-git-common-dir-repo-name' -- else FRAME's
sanitised `name' parameter (the ungrouped flat-tab-list case)."
  (or (edmacs-workspaces-current-group frame)
      (edmacs-sidebar--sanitise-frame-title (or (frame-parameter frame 'name) ""))))

(defun edmacs-sidebar--header-line (frame)
  "Return FRAME's sidebar header-line string: its own repo/frame
identity plus whatever suffix `edmacs-sidebar-header-line-function'
supplies (sidebar-agents.el's repo-wide roll-up, by default none)."
  (let ((suffix (funcall edmacs-sidebar-header-line-function frame)))
    (propertize (concat (edmacs-sidebar--header-line-name frame) (or suffix ""))
                'face 'edmacs-sidebar-header-face)))

(defun edmacs-sidebar--strip-width (frame)
  "Return the collapsed strip's real usable text width on FRAME.
`edmacs-sidebar--collapsed-width' is what `edmacs-sidebar-show' asks
`display-buffer-in-side-window' for, never what the window ends up
with: the placement returns a column less, and a GUI frame's fringes
cost more again. A producer handed the constant fits to a width the
window does not have, so its own truncation never fires and the window
clips instead -- dropping a percentage's low-order digits with no
ellipsis to show anything was lost. Falls back to the constant only
when no live window exists to measure."
  (let ((window (edmacs-sidebar--window frame)))
    (if (window-live-p window)
        (max 1 (window-body-width window))
      edmacs-sidebar--collapsed-width)))
(defvar-local edmacs-sidebar--anchor-start nil
  "A marker at the position where the last `edmacs-sidebar--redraw'
pass's bottom-anchor hook began inserting, or nil when that hook
inserted nothing. A real marker, not a plain integer, and specifically
one whose `insertion-type' is t: `edmacs-sidebar--anchor-region-to-bottom'
inserts blank filler at exactly this position, and a marker with this
insertion-type moves forward past text inserted at its own position,
so it keeps tracking the start of the hook's own content -- not the
filler -- even across repeated reapplications. Lets
`edmacs-sidebar--reapply-bottom-anchor' reapply the anchor later
against a window that did not exist yet at redraw time (or against a
window whose size changed since), without rerunning any
section-contributing hook a second time and without miscounting
already-inserted filler as if it were the hook's own content.")

(defun edmacs-sidebar--anchor-plan (above anchored body-height point-above-p)
  "Decide how to pin an anchored region to a window's bottom edge.
ABOVE and ANCHORED are screen-line counts -- the content preceding the
region, and the region itself -- and BODY-HEIGHT is the window's body
height. Returns `(:pad N)' to insert N blank lines above the region,
`(:scroll-to LINES)' to force `window-start' back LINES screen lines
from `point-max', or nil to do nothing. Exactly-equal heights are the
nil case, which is what makes a repeated call against an already-padded
buffer a no-op.

POINT-ABOVE-P is non-nil when point in the truly-selected window sits
above where that scroll would start: point wins there, so the overflow
case becomes a no-op rather than dragging the cursor into the anchored
region.

:scroll-to carries a screen-LINE COUNT, not a buffer position. Turning
one into the other needs `vertical-motion' in a live window, which a
pure function cannot do, and threading a precomputed position in would
force every caller to compute one in the pad and no-op branches too."
  (let ((total (+ above anchored)))
    (cond
     ((< total body-height) (list :pad (- body-height above anchored)))
     ((= total body-height) nil)
     (point-above-p nil)
     (t (list :scroll-to body-height)))))

(defun edmacs-sidebar--anchor-region-to-bottom (window region-start)
  "Apply `edmacs-sidebar--anchor-plan' for WINDOW to the buffer region
from REGION-START to `point-max': pad it with blank lines when the rest
of the buffer is shorter than WINDOW's body height, or force WINDOW's
`window-start' past the overflow so the region stays in view when the
buffer is taller.

No-ops when WINDOW is not `window-live-p' -- the very first
`edmacs-sidebar--redraw' for a frame (from `edmacs-sidebar--ensure-buffer')
always runs before `display-buffer-in-side-window' has created a
window; `edmacs-sidebar--reapply-bottom-anchor' is what actually
applies the anchor once that window exists. Operates on the current
buffer, which callers always arrange to be the one WINDOW displays.

Point wins over the anchor in the window the user is actually in -- the
process-wide selected window, not merely WINDOW's own frame's selected
window: a backgrounded frame's own selected window still gets the forced
scroll, same as any other non-selected window. Forcing the scroll in the
truly-selected window drags the cursor out of the project rows and into
the anchored block, and the next redraw drags it back."
  (when (window-live-p window)
    (let* ((body-height (window-body-height window))
           (above (count-screen-lines (point-min) region-start nil window))
           (anchored (count-screen-lines region-start (point-max) nil window))
           ;; Only the overflow branch needs this, and it costs a
           ;; `vertical-motion' walk -- so the redisplay-path pad and
           ;; no-op branches pay nothing for it.
           (start (and (> (+ above anchored) body-height)
                       (save-excursion
                         (goto-char (point-max))
                         (vertical-motion (- body-height) window)
                         (point))))
           (point-above-p (and start
                               (eq window (frame-selected-window (selected-frame)))
                               (< (window-point window) start))))
      (pcase (edmacs-sidebar--anchor-plan above anchored body-height point-above-p)
        (`(:pad ,n)
         (save-excursion
           (goto-char region-start)
           (insert (make-string n ?\n))))
        (`(:scroll-to ,_)
         (set-window-start window start t)
         ;; Point may sit above the forced start (e.g. a backgrounded
         ;; window whose row scrolled off) -- pull it forward so the next
         ;; redisplay cycle doesn't fight the scroll trying to keep it
         ;; visible, which would silently undo the anchor.
         (when (< (window-point window) start)
           (set-window-point window start)))))))

(defun edmacs-sidebar--anchor-marker-at (position)
  "Return a marker at POSITION in the current buffer, with `insertion-type'
t -- see `edmacs-sidebar--anchor-start's docstring for why.

Deliberately takes POSITION rather than capturing point directly:
callers create this marker AFTER the bottom-anchor hook has already run,
passing the plain integer position recorded just before it, so the
hook's own insertion (at that exact position) leaves the marker where
it belongs -- at the start of the hook's content -- rather than moving
it, which is what would happen were this marker created (with this
same insertion-type) before the hook ran."
  (copy-marker position t))

(defun edmacs-sidebar--redraw (frame)
  "Redraw FRAME's sidebar buffer: plan, measure, erase, render, anchor.
No-ops when FRAME has no live sidebar buffer -- callers such as the
tab-bar hooks below fire for every frame regardless of whether that
frame's sidebar has ever been shown.

`edmacs-sidebar--plan' decides what to insert (see its own docstring for
the collapsed/grouped/flat branch); the width every label is fitted to is
measured ONCE here -- `edmacs-sidebar--strip-width' when collapsed,
`edmacs-sidebar--render-width' otherwise -- and handed to
`edmacs-sidebar--render', which is the only thing that touches the
buffer. Point and fold state are preserved on the same row when possible,
via `edmacs-sidebar--capture-positions'/`--restore-positions' and
`magit-section-ident' stability -- see the plan-row contract's `:value'
entry. The header line is nil'd on the collapsed branch.

Whatever the plan's `:anchor' rows inserted is then pinned to the
window's bottom edge by `edmacs-sidebar--anchor-region-to-bottom',
skipped entirely when they inserted nothing -- matching the \"insert
nothing when disabled\" contract their registrants already follow."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (collapsed (frame-parameter frame 'edmacs-sidebar-collapsed))
               (positions (edmacs-sidebar--capture-positions buf))
               (rows (edmacs-sidebar--plan frame))
               (width (if collapsed
                          (edmacs-sidebar--strip-width frame)
                        (edmacs-sidebar--render-width frame)))
               region)
          (erase-buffer)
          (setq region (edmacs-sidebar--render rows width))
          (setq header-line-format (unless collapsed (edmacs-sidebar--header-line frame)))
          (edmacs-sidebar--restore-positions positions)
          (setq edmacs-sidebar--anchor-start
                (and region (edmacs-sidebar--anchor-marker-at (car region))))
          (when edmacs-sidebar--anchor-start
            (edmacs-sidebar--anchor-region-to-bottom
             (edmacs-sidebar--window frame) edmacs-sidebar--anchor-start)))))))

(defun edmacs-sidebar--reapply-bottom-anchor (frame)
  "Reapply the bottom anchor for FRAME's sidebar against the position
its last `edmacs-sidebar--redraw' recorded in
`edmacs-sidebar--anchor-start', without rerunning any
section-contributing hook a second time.

`edmacs-sidebar-show' uses this as a belt-and-suspenders step once a
freshly created window's real height is known: the buffer's own
`edmacs-sidebar--redraw', run earlier from
`edmacs-sidebar--ensure-buffer', necessarily ran before that window
existed, so its own anchor application no-op'd. Safe to call any time
since the last redraw -- neither `--redraw' nor this function edits
the buffer between the recorded position and `point-max' except via
`edmacs-sidebar--anchor-region-to-bottom' itself. A no-op when the last
redraw's bottom-anchor hook inserted nothing, or FRAME has no live
sidebar buffer."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when edmacs-sidebar--anchor-start
          (let ((inhibit-read-only t))
            (edmacs-sidebar--anchor-region-to-bottom
             (edmacs-sidebar--window frame) edmacs-sidebar--anchor-start)))))))

;; ============================================================================
;; Commands
;; ============================================================================

(defun edmacs-sidebar-activate ()
  "Act on the section at point: switch to its tab, or open/create one.
Resolves point to its enclosing `edmacs-sidebar-tab' row first (see
`edmacs-sidebar--enclosing-worktree'), so this also reaches the
worktree from a nested descendant -- e.g. sidebar-buffers.el's own
`buffers' heading -- not only from the tab row itself. An integer
section value (a flat-list tab with no root of its own) is already the
1-based tab-number `tab-bar-select-tab' expects -- it treats 0 as a \"reselect
current tab\" sentinel, so redraw stores `(1+ index)', never the raw
0-based index. A `(GROUP . ROOT)' cons (a project or worktree child row,
edmacs-tab-groups phase 3) is looked up on its ROOT alone via
`edmacs-workspaces-find-tab' -- a worktree root belongs to exactly one
repo, so the row's GROUP is a function of it and testing it too could
only ever hide the tab the row names: a match selects that tab via
`edmacs-workspaces-select-tab'; with no match, a project row whose ROOT
is itself the repo's main worktree \(`edmacs-workspaces-classify-root'
returns `main') opens the whole project via
`edmacs-workspaces-open-project' -- find-or-create,
correct in every state per that function's own contract, so repeat
activations never duplicate a tab. A worktree child row's value is
always drawn from an already-open tab (AC7), so it can never reach the
`user-error' fallback below in practice -- that branch is defensive
only. Every other case -- no enclosing worktree row at all, or an
unbound or nil value slot (a usage row, deliberately valueless) --
signals `user-error' instead of silently doing nothing."
  (interactive)
  (let* ((section (edmacs-sidebar--enclosing-worktree (magit-current-section)))
         (value (and section (slot-boundp section 'value) (oref section value))))
    (cond
     ((integerp value) (tab-bar-select-tab value))
     ((consp value)
      (let ((root (cdr value)))
        (cond
         ((edmacs-workspaces-find-tab root) (edmacs-workspaces-select-tab root))
         ((eq (edmacs-workspaces-classify-root root) 'main) (edmacs-workspaces-open-project root))
         (t (user-error "No open tab for this worktree")))))
     (t (user-error "Nothing to do on this row")))))

(defun edmacs-sidebar-visit-at-point ()
  "Act on the section at point, dispatched by its magit-section TYPE.
An `edmacs-sidebar-agent' row (sidebar-agents.el, phase 6) is visited
via `edmacs-sidebar-agents-visit'; a `edmacs-sidebar-buffers-file'/
`-special' row (sidebar-buffers.el, phase 7) via
`edmacs-sidebar-buffers-visit'; a tab/worktree row (or any other,
unrecognized section type) keeps the original `edmacs-sidebar-activate'
behavior unchanged. Every section type known to have no RET action at
all -- the root wrapper, the missing-repo warning row, sidebar-agents.el's
group/ALL-AGENTS headings, and sidebar-buffers.el's directory-node
headings -- and no section at all, signal `user-error' instead of
silently doing nothing."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((and section (eq (oref section type) 'edmacs-sidebar-agent))
      (edmacs-sidebar-agents-visit))
     ((and section (memq (oref section type) '(edmacs-sidebar-buffers-file edmacs-sidebar-buffers-special)))
      (edmacs-sidebar-buffers-visit))
     ((or (null section)
          (memq (oref section type)
                '(edmacs-sidebar-root edmacs-sidebar-warning
                  edmacs-sidebar-agents-group edmacs-sidebar-agents-all
                  edmacs-sidebar-buffers-dir)))
      (user-error "Nothing to do on this row"))
     (t (edmacs-sidebar-activate)))))

(defun edmacs-sidebar--top-level-section-at (section)
  "Return the top-level (direct child of `magit-root-section') ancestor
of SECTION, or nil when SECTION is nil or is `magit-root-section' itself."
  (while (and section (oref section parent) (not (eq (oref section parent) magit-root-section)))
    (setq section (oref section parent)))
  (and section (not (eq section magit-root-section)) section))

(defun edmacs-sidebar--ancestor-satisfying (section predicate)
  "Return SECTION or its nearest ancestor satisfying PREDICATE, or nil.
Walks SECTION then its `parent' chain, stopping at (and excluding)
`magit-root-section' -- so PREDICATE is never tested against the root
even when PREDICATE would technically match it (e.g. \"has children\",
which is always true of the root); a section that reaches the root
without a match returns nil rather than folding the whole tree."
  (while (and section (not (eq section magit-root-section)) (not (funcall predicate section)))
    (setq section (oref section parent)))
  (and section (not (eq section magit-root-section)) section))

(defun edmacs-sidebar--enclosing-worktree (section)
  "Return SECTION or its nearest `edmacs-sidebar-tab' ancestor, or nil.
The one shared parent-walk `edmacs-sidebar-activate',
`edmacs-sidebar-close-worktree', and `edmacs-sidebar-rename-at-point's
generic fallback all use to reach the enclosing worktree row from a
nested descendant (an agents-group/agent row, a buffers-root/file/
special row) now that `edmacs-sidebar-worktree-section-functions' nests
its contributed sections as real children of that row."
  (edmacs-sidebar--ancestor-satisfying
   section (lambda (s) (eq (oref s type) 'edmacs-sidebar-tab))))

(defun edmacs-sidebar--move-to-worktree (delta)
  "Move point DELTA positions along the top-level rows (direct children
of `magit-root-section' -- tab/worktree rows, the warning row, and the
agents-all section are all direct children today; the agents-group and
buffers-root sections a worktree row's own hook contributes are now
nested under it rather than being top-level themselves, so this
correctly steps over them instead of stopping on them as if they were
worktree rows). A no-op past either end: DELTA is +1 for
`edmacs-sidebar-move-to-next-worktree', -1 for
`edmacs-sidebar-move-to-prev-worktree'. With no current top-level row
under point (e.g. point at `point-min' before any row), DELTA > 0 moves
to the first row and DELTA < 0 is a no-op."
  (let* ((children (oref magit-root-section children))
         (current (edmacs-sidebar--top-level-section-at (magit-current-section)))
         (index (and current (cl-position current children)))
         (target (cond (index (+ index delta))
                       ((> delta 0) 0))))
    (when (and target (>= target 0) (< target (length children)))
      (goto-char (oref (nth target children) start)))))

;;;###autoload
(defun edmacs-sidebar-move-to-next-worktree ()
  "Move point to the next top-level row. A no-op past the last row."
  (interactive)
  (edmacs-sidebar--move-to-worktree 1))

;;;###autoload
(defun edmacs-sidebar-move-to-prev-worktree ()
  "Move point to the previous top-level row. A no-op before the first row."
  (interactive)
  (edmacs-sidebar--move-to-worktree -1))

(defun edmacs-sidebar--section-tab-number (section)
  "Return the open tab-number for tab-row SECTION's value, or nil.
An integer value (a tab carrying no root at all -- see
`edmacs-sidebar--plan-tabs') is itself always an open tab's number; a
`(GROUP . ROOT)' cons (a project or worktree child row, or a flat row
whose tab has a root) is looked up on its ROOT alone via
`edmacs-workspaces-find-tab', returning nil when no tab is open at ROOT
-- e.g. a project row whose main tab isn't
open, which is what makes `d' on it a no-op rather than opening one
\(only RET/`edmacs-sidebar-activate' is allowed to open\). Ambient on the
selected frame, like every other command in this section -- a
worktree/project row is always acted on relative to the frame its own
sidebar buffer is showing. Also used by `edmacs-sidebar-close-worktree'
and `edmacs-sidebar-rename-at-point'."
  (let ((value (and section (slot-boundp section 'value) (oref section value))))
    (cond ((integerp value) value)
          ((consp value)
           (when-let* ((tab (edmacs-workspaces-find-tab (cdr value))))
             ;; No FRAME argument on this call chain to prefer -- ambient-reads: ok
             (edmacs-workspaces-tab-number tab))))))

;;;###autoload
(defun edmacs-sidebar-rename-at-point ()
  "Rename the row at point: `edmacs-sidebar-agents-rename'
(sidebar-agents.el) on an agent row; otherwise `tab-bar-rename-tab' on
the section's enclosing worktree row (see
`edmacs-sidebar--enclosing-worktree'), when that row has an open tab.
Every other row -- a tab-less worktree row, one with no enclosing
worktree row at all, or no row at all -- signals `user-error' instead.

The agent branch is checked first: an agent row now sits inside its
worktree's own section (AC1), and would otherwise also match the
generic enclosing-worktree branch and rename the tab instead of the
agent.

Always renames the tab whose row is under point, never the frame's
currently-selected tab: `tab-bar-rename-tab' called interactively
defaults TAB-NUMBER to the selected tab, which is wrong once J/K have
moved point onto a background tab's row, so this passes the row's own
tab-number through explicitly instead of using `call-interactively'."
  (interactive)
  (let* ((section (magit-current-section))
         (tab-section (edmacs-sidebar--enclosing-worktree section)))
    (cond
     ((and section (eq (oref section type) 'edmacs-sidebar-agent) (slot-boundp section 'value))
      (edmacs-sidebar-agents-rename (oref section value)))
     (tab-section
      (if-let* ((tab-number (edmacs-sidebar--section-tab-number tab-section)))
          (let* ((tabs (funcall tab-bar-tabs-function))
                 (tab-name (alist-get 'name (nth (1- tab-number) tabs)))
                 (new-name (read-from-minibuffer
                            "New name for tab (leave blank for automatic naming): "
                            nil nil nil nil tab-name)))
            (tab-bar-rename-tab new-name tab-number))
        (user-error "No tab to rename")))
     (t (user-error "Nothing to rename here")))))

;;;###autoload
(defun edmacs-sidebar-toggle-at-point (frame)
  "Fold/unfold the section at point, matching the design table's `TAB'
row: a section with its own children (a tab row, an agents/buffers
group heading) folds itself via `magit-section-toggle'; a leaf row
(an agent, or a buffer file/special row) has no body of its own to
fold, so this folds its nearest ancestor that does have children
instead -- via `edmacs-sidebar--ancestor-satisfying' -- exactly as the
design table's colspan cell for `On an agent'/`On a buffer' specifies.
Falls back to toggling SECTION itself when neither it nor any ancestor
has children, matching plain `magit-section-toggle's own no-op/error
behavior for the root and unparented sections.

On a collapsed sidebar, TAB expands it instead -- there is nothing
meaningful to fold in the collapsed strip's own render.

FRAME is the frame whose sidebar point sits in; interactively, always
the selected frame -- there is no other frame a keypress could mean."
  (interactive (list (selected-frame)))
  (if (frame-parameter frame 'edmacs-sidebar-collapsed)
      (edmacs-sidebar-expand frame)
    (let ((section (magit-current-section)))
      (cond
       ((or (null section) (eq section magit-root-section))
        (magit-section-toggle section))
       (t (magit-section-toggle
           (or (edmacs-sidebar--ancestor-satisfying section (lambda (s) (oref s children)))
               section)))))))

;;;###autoload
(defun edmacs-sidebar-redraw (frame)
  "Force an immediate redraw of FRAME's sidebar from cached data.
Bypasses `edmacs-sidebar-invalidate's coalescing deliberately -- this is
the user asking for a redraw right now, not one more trigger to fold
into the next idle flush -- but still drops FRAME from any pending
`edmacs-sidebar--dirty-frames' set first, so an already-scheduled idle
flush does not immediately redraw it again right after.
Never shells out to enumerate worktrees -- this only rebuilds the
section tree from data already on the frame's own tabs, so it is
always safe to bind to a bare key. Interactively, FRAME is always the
selected frame."
  (interactive (list (selected-frame)))
  (setq edmacs-sidebar--dirty-frames (delq frame edmacs-sidebar--dirty-frames))
  (edmacs-sidebar--redraw frame)
  (message "sidebar redrawn"))

;;;###autoload
(defun edmacs-sidebar-help ()
  "Show a cheat sheet of every binding in `edmacs-sidebar-mode-map'.
Prefers `which-key-show-full-keymap' when available; falls back to the
Emacs 29+ core `describe-keymap', which is always present. Emacs 31
ships which-key's autoloads by default, so `fboundp' alone is true even
under `-Q --batch' with no which-key loaded -- exercising the
`describe-keymap' branch in the ERT suite needs that autoload stub
unbound first, per
`edmacs-sidebar-test-help-falls-back-to-describe-keymap-for-real'."
  (interactive)
  (if (fboundp 'which-key-show-full-keymap)
      (which-key-show-full-keymap 'edmacs-sidebar-mode-map)
    (describe-keymap 'edmacs-sidebar-mode-map)))

(defun edmacs-sidebar-close-worktree ()
  "Close the open tab represented by the section at point, or, if point
is on a row nested inside a worktree row (an agent, a buffer, either
group heading), the tab of its enclosing worktree row -- see
`edmacs-sidebar--enclosing-worktree'.
A no-op on a tab-less worktree row -- worktree removal itself stays
with workmux/rdm, never this key (phase body Steps item 5) -- or when
no enclosing worktree row can be found at all."
  (interactive)
  (when-let* ((section (edmacs-sidebar--enclosing-worktree (magit-current-section)))
              (tab-number (edmacs-sidebar--section-tab-number section)))
    (tab-bar-close-tab tab-number)))

(defun edmacs-sidebar-kill-at-point (frame)
  "Act on the section at point: kill a buffer row (sidebar-buffers.el,
phase 7), kill an agent session (sidebar-agents.el, phase 8, after
confirming), else close the worktree row's tab exactly as before.
Interactively, FRAME is always the selected frame."
  (interactive (list (selected-frame)))
  (let ((section (magit-current-section)))
    (cond
     ((and section (memq (oref section type) '(edmacs-sidebar-buffers-file edmacs-sidebar-buffers-special)))
      (edmacs-sidebar-buffers-kill frame))
     ((and section (eq (oref section type) 'edmacs-sidebar-agent) (slot-boundp section 'value))
      (edmacs-sidebar-agents-kill (oref section value)))
     (t (edmacs-sidebar-close-worktree)))))

(defun edmacs-sidebar--window (frame)
  "Return FRAME's visible sidebar window, or nil.
Matches on buffer identity alone, so this also finds the sidebar buffer
sitting in an ordinary window -- a window the sidebar does not own and
must not delete. Use `edmacs-sidebar--side-window' where side-ness, not
mere presence, is the question."
  (let ((buf (edmacs-sidebar--buffer frame)))
    (when (buffer-live-p buf)
      (seq-find (lambda (w) (eq (window-buffer w) buf))
                 (window-list frame 'never)))))

(defun edmacs-sidebar--side-window (frame)
  "Return FRAME's sidebar window only when it really is a left side window."
  (let ((window (edmacs-sidebar--window frame)))
    (when (and window (eq (window-parameter window 'window-side) 'left))
      window)))

;; ============================================================================
;; Manual resize survives a hide/show cycle (AC3), deliberate resizes only
;; ============================================================================
;; `preserve-size's `(t . nil)' parameter (below) blocks only AUTOMATIC
;; resizing -- `balance-windows', `fit-window-to-buffer' -- not an
;; explicit user mouse-drag or `C-x {'/`}', which keep working exactly
;; as before. What mouse-resize does NOT survive on its own is a
;; hide/show cycle: `edmacs-sidebar-show' would otherwise always fall
;; back to the `edmacs-sidebar-width' defcustom. So the frame's actual
;; window width is mirrored into a frame parameter here, debounced per
;; frame (one timer per frame, keyed in a hash table) so
;; a mouse drag's stream of intermediate sizes doesn't thrash.
;;
;; Not every `window-size-change-functions' firing that touches the
;; sidebar's width is a deliberate choice, though -- another window
;; splitting or closing, a tab switch, or a frame resize can all leave
;; the sidebar transiently wide as a side effect, and that must never
;; be persisted (U4: a 165-column frame's transient state poisoned the
;; remembered width to 52 against a 32-column default, then rode the
;; desktop file into the next session). The rule: only a resize driven
;; by `edmacs-sidebar--interactive-resize-commands' -- this config's own
;; entry points for a user directly resizing the sidebar window -- is
;; ever remembered. `--on-window-size-change' samples `this-command'
;; synchronously against that allowlist, since it is still meaningful
;; at hook-fire time; the debounced `--remember-width' callback fires
;; ~`edmacs-sidebar-resize-debounce-seconds' later, once `this-command'
;; has moved on to something else, so the decision is captured now and
;; threaded through rather than re-derived at stash time.

(defvar edmacs-sidebar-resize-debounce-seconds 0.2
  "Seconds a frame's sidebar-window-width changes coalesce into one
remembered-width update. A plain `defvar', not `defcustom', so a test
can shrink it -- mirrors `edmacs-sidebar-agents-coalesce-seconds's own
convention.")

(defvar edmacs-sidebar--resize-debounce-timers (make-hash-table :test #'eq)
  "FRAME -> pending debounce timer for `edmacs-sidebar--on-window-size-change'.")

(defvar edmacs-sidebar--interactive-resize-commands
  '(evil-window-increase-width evil-window-decrease-width mouse-drag-line)
  "Commands that count as the user deliberately resizing the sidebar
window: this config's own `C-w H'/`C-w L' evil bindings (see
keybindings.el) plus a mouse-driven window-divider drag. Checked
against `this-command' by `edmacs-sidebar--on-window-size-change' to
decide whether a width change is worth remembering -- see the comment
block above. Deliberately NOT an exhaustive list of every Emacs resize
primitive, just this config's own entry points; extend it here if a
future entry point is added, rather than scattering the rule
elsewhere.")

(defun edmacs-sidebar--remember-width (frame &optional interactive-resize)
  "Stash the `window-total-width' value that reproduces FRAME's current
sidebar window width the next time `edmacs-sidebar-show' creates a
fresh side window. `edmacs-sidebar-show' feeds the stashed value into
`edmacs-sidebar--enforce-width's plain-integer (expanded) branch, which
resizes directly against `window-total-width' -- so the value stashed
here must be a TOTAL width, not a body width, or the round trip loses
exactly the window's chrome cost (fringes plus a scroll bar) on every
expand. A stale `(1+ (window-width window))' -- a body-width reading
with a fresh-split-rounding offset added back on -- happened to equal
the total width only when chrome cost exactly one column, true in
batch but not on a real GUI frame; `window-total-width' is correct in
both.

Refuses to stash unless INTERACTIVE-RESIZE is non-nil -- see the
comment block above `edmacs-sidebar--interactive-resize-commands' --
and WINDOW is genuinely a side window with at least one sibling window
in the frame -- a bare `window-width' read at a moment the sidebar is
effectively the frame's only live window (e.g. `delete-other-windows',
or mid-frameset-restore before other windows exist) is not a real
sidebar width and must never be persisted. Also refuses outright while
FRAME's `edmacs-sidebar-collapsed' parameter is set: the live window
width in that state is `edmacs-sidebar--collapsed-width', not a value
the user chose, and stashing it would overwrite the real remembered
width out from under a debounce timer that can fire after a collapse.
Read fresh here rather than snapshotted earlier in the call chain, so a
collapse/expand toggle racing the debounce timer is decided by the
state at the moment this actually runs. The stashed value itself is
clamped via `edmacs-sidebar--clamp-width'."
  (remhash frame edmacs-sidebar--resize-debounce-timers)
  (when (and (frame-live-p frame) (not (frame-parameter frame 'edmacs-sidebar-collapsed)))
    (let ((window (edmacs-sidebar--side-window frame)))
      (when (and interactive-resize
                 (window-live-p window)
                 (> (length (window-list frame 'never)) 1))
        (set-frame-parameter
         frame 'edmacs-sidebar-remembered-width
         (edmacs-sidebar--clamp-width (window-total-width window) frame))))))

(defun edmacs-sidebar--on-window-size-change (frame)
  "Registered on `window-size-change-functions': debounce-stash FRAME's
sidebar window width, if it currently has one shown. A no-op for a
frame with no live sidebar window at all -- most redisplay-triggering
size changes are unrelated windows.

Whether the eventual stash is allowed to persist is decided here, not
in the debounced callback: `this-command' is sampled against
`edmacs-sidebar--interactive-resize-commands' now, while it still
names whatever triggered this size change, and the resulting boolean
is carried through to `edmacs-sidebar--remember-width' when the timer
fires."
  (when (and (frame-live-p frame) (edmacs-sidebar--window frame))
    (let ((interactive-resize (and (memq this-command edmacs-sidebar--interactive-resize-commands) t)))
      (when-let* ((timer (gethash frame edmacs-sidebar--resize-debounce-timers)))
        (cancel-timer timer))
      (puthash frame
               (run-at-time edmacs-sidebar-resize-debounce-seconds nil
                             #'edmacs-sidebar--remember-width frame interactive-resize)
               edmacs-sidebar--resize-debounce-timers))))

(add-hook 'window-size-change-functions #'edmacs-sidebar--on-window-size-change)

(defun edmacs-sidebar--on-window-size-change-anchor (frame)
  "Registered on `window-size-change-functions': react to a size change
of FRAME's OWN sidebar window, ignoring every other window's.
`window-size-change-functions' fires for ANY window's resize or buffer
change anywhere on FRAME; comparing the sidebar window's current pixel
size against `window-old-pixel-width'/`window-old-pixel-height' (and
their body variants) -- redisplay's own before/after record for this
hook -- narrows this to firings that changed the sidebar window itself.

A width change invalidates the frame (`edmacs-sidebar-invalidate'):
every label is fitted to the window's width at render time, so a wider
or narrower sidebar needs its rows re-fitted, which only a redraw does.
A height-only change reapplies the bottom anchor
(`edmacs-sidebar--reapply-bottom-anchor'): only the anchor's position
depends on height, and reapplying is far cheaper than a rebuild. A no-op
for a frame with no live sidebar window, or whose sidebar window's
geometry did not change."
  (when (frame-live-p frame)
    (when-let* ((window (edmacs-sidebar--window frame)))
      (cond
       ((or (/= (window-pixel-width window) (window-old-pixel-width window))
            (/= (window-body-width window t) (window-old-body-pixel-width window)))
        (edmacs-sidebar-invalidate frame))
       ((or (/= (window-pixel-height window) (window-old-pixel-height window))
            (/= (window-body-height window t) (window-old-body-pixel-height window)))
        (edmacs-sidebar--reapply-bottom-anchor frame))))))

(add-hook 'window-size-change-functions #'edmacs-sidebar--on-window-size-change-anchor)

(defun edmacs-sidebar--target-width (frame)
  "Return the `window-width' request that sizes FRAME's sidebar.
A collapsed frame asks in `body-columns': that request is sized in body
pixels, so it lands on the width the producers are formatted for whatever
this frame's chrome costs -- one column in batch, two to five on a GUI
frame depending on `frame-char-width\=', fringe pixels and scroll bar.
Otherwise the remembered width, else `edmacs-sidebar-width', clamped
here at READ time -- which is what makes a value already poisoned in a
live frame parameter or a restored desktop self-heal rather than only
ever being prevented on write. The clamp is frame-relative, so the answer
legitimately differs between a laptop screen and an external display."
  (if (frame-parameter frame 'edmacs-sidebar-collapsed)
      (cons 'body-columns edmacs-sidebar--collapsed-width)
    (edmacs-sidebar--clamp-width
     (or (frame-parameter frame 'edmacs-sidebar-remembered-width)
         edmacs-sidebar-width)
     frame)))

(defun edmacs-sidebar--enforce-width (window frame width)
  "Resize WINDOW on FRAME to WIDTH, which the placement only sometimes does.
`display-buffer-in-side-window' honours a `window-width' request on a
window it REUSES only when that window's `quit-restore' parameter still
carries the symbol `window' in slot 1 (window.el's `window--display-buffer').
A frameset-restored window has no `quit-restore' at all -- it is not in
`window-persistent-parameters' -- and a window that ever showed a
different buffer carries the displaced buffer's quadruple there instead.
Both shapes occur in ordinary use, and in both the width request is
dropped without a word, which is why the collapsed strip could open at
full width.

WIDTH is either a total-column integer or a `(body-columns . N)' cons,
matching the two `window-width' forms. The body-columns case corrects
against the window's own measured body width and repeats, because chrome
is not a whole number of columns on a graphical frame -- a 17-pixel
scroll bar at a 7-pixel character width costs 2.43 columns, so a single
computed delta lands one column out. Two passes suffice; a third is
insurance, and a converged pass costs nothing.

IGNORE must be `safe' rather than t: plain t drops only
`window-min-width' and still respects the pixel cost of fringes and a
scroll bar, which alone is enough to refuse a four-column strip on a
graphical frame. Neither `window-resize' call is wrapped in
`ignore-errors' -- a genuine refusal (e.g. a fixed-size window, or a
request `safe' cannot satisfy) must signal, not vanish."
  (ignore frame)
  (if (consp width)
      (dotimes (_ 3)
        (let ((delta (- (cdr width) (window-body-width window))))
          (unless (zerop delta)
            (window-resize window delta t 'safe))))
    (window-resize window (- width (window-total-width window)) t 'safe)))

(defun edmacs-sidebar-show (frame)
  "Show FRAME's sidebar window, creating and redrawing its buffer first.
Guarantees the result is either FRAME's left side window or nil --
never a window on any other edge. Calls
`display-buffer-in-side-window' DIRECTLY rather than handing a
one-function action list to `display-buffer': `display-buffer'
concatenates that action with `display-buffer-alist',
`display-buffer-base-action' and `display-buffer-fallback-action' into
one list and keeps trying entries after any one of them returns nil,
so when the left slot can't be created (`window-sides-slots'
forbidding it, or the frame too small) `display-buffer' would fall
through to its own base/fallback actions -- which split whatever
window is widest, landing the sidebar on the right on a wide frame.
Bypassing `display-buffer' itself is what stops that fallthrough; a
`display-buffer-overriding-action' wrapper alone would not; only
inspecting the returned window afterward would be too late; and this
call site is exactly where a nil `display-buffer-in-side-window'
result is otherwise turned into an empty-not-nil action list, so the
placement guarantee and the width clamp below share this same call by
necessity, not convenience.

As a second line of defense, if the call somehow still returns a
live, non-nil window that is not a left side window, that window is
deleted and treated as nil for the rest of this function -- nothing is
dedicated. This mirrors `claude-term--pop-to-window's own nil
guard for the ordinary `display-buffer-in-side-window' returns-nil
case (slot exhausted, frame too small). A frame with no non-side window
is repaired first, since otherwise the existing slot-0 left window is
simply reused and the frame stays without a main window.

Sized by `edmacs-sidebar--target-width', so a manual resize survives a
hide/show cycle and a poisoned remembered width self-heals on the very
next show.

Runs `edmacs-sidebar-visibility-functions' with (FRAME `shown') on every
return path, including the ones that produce no window."
  (interactive (list (selected-frame)))
  (prog1
      ;; The daemon's initial tty placeholder must never get a sidebar. Under
      ;; the old per-frame `*sidebar: <repo>*' naming each frame drew into its
      ;; own buffer, so this cost nothing; with one shared `*sidebar*' buffer a
      ;; redraw for that frame -- which has no project group -- overwrites the
      ;; real frame's tree with an empty one, leaving one stale row under an
      ;; `F1' header. `edmacs-workspaces-frame-usable-p' already answers "may
      ;; this config drive FRAME"; the sidebar simply never asked.
      (when (edmacs-workspaces-frame-usable-p frame)
        (let* ((buf (edmacs-sidebar--ensure-buffer frame))
               (width (edmacs-sidebar--target-width frame))
               (collapsed (frame-parameter frame 'edmacs-sidebar-collapsed))
               (window (with-selected-frame frame
                         (display-buffer-in-side-window
                          buf
                          `((side . left)
                            (slot . 0)
                            (window-width . ,width)
                            (preserve-size . (t . nil))
                            (window-parameters . ((no-delete-other-windows . t)
                                                  (no-other-window . t)
                                                  (mode-line-format . none))))))))
          (cond
           ((null window) nil)
           ((not (eq (window-parameter window 'window-side) 'left))
            ;; This call is what produced WINDOW, so deleting it is the right
            ;; cleanup -- except on the one shape `delete-window' refuses, a
            ;; window with no parent, which is released in place instead.
            (if (window-parent window)
                (delete-window window)
              (edmacs-sidebar--release-window window frame))
            nil)
           (t
            (set-window-dedicated-p window t)
            ;; Fringes cost roughly two columns of a four-column strip. nil
            ;; restores the frame's own widths -- without the else branch the
            ;; window stays fringe-less for the rest of its life, so one collapse
            ;; permanently narrows the expanded sidebar too.
            (if collapsed
                (set-window-fringes window 0 0)
              (set-window-fringes window nil nil))
            ;; `display-buffer-in-side-window's `window-width' request is only
            ;; honoured on a REUSED window with an intact `quit-restore' -- see
            ;; `edmacs-sidebar--enforce-width''s docstring. Every frameset-restored
            ;; window and every window that ever showed a different buffer fails
            ;; that check, so both the collapse and the expand path enforce the
            ;; width themselves rather than trusting the placement call above.
            (edmacs-sidebar--enforce-width window frame width)
            ;; Belt-and-suspenders (matches `edmacs-sidebar-collapse's own
            ;; docstring pattern): the buffer's very first `--redraw' ran from
            ;; `--ensure-buffer' above, before this window existed, so any
            ;; bottom-anchor hook it ran no-op'd against a nil window. Reapply
            ;; the anchor now that the real window -- and its real height --
            ;; exists, rather than waiting on the next
            ;; `window-size-change-functions' firing. A geometry-only reapply,
            ;; not a full `--redraw': that would rerun every section-contributing
            ;; hook a second time on every single show, not just the first.
            (edmacs-sidebar--reapply-bottom-anchor frame)
            window))))
    (run-hook-with-args 'edmacs-sidebar-visibility-functions frame 'shown)))

(defun edmacs-sidebar-reapply-width (frame)
  "Resize FRAME's sidebar window back to `edmacs-sidebar--target-width'.
Joins `edmacs-windows-rebalance-functions' (`SPC w ='): a side window
keeps the absolute width it was created at, and the target itself is
frame-relative through `edmacs-sidebar--clamp-width', so both sides of
the comparison can be stale after the frame moves to another display.
Never creates a window -- a hidden sidebar stays hidden."
  (when-let* ((window (edmacs-sidebar--window frame)))
    (edmacs-sidebar--enforce-width window frame
                                    (edmacs-sidebar--target-width frame))))

(add-hook 'edmacs-windows-rebalance-functions #'edmacs-sidebar-reapply-width)

(defun edmacs-sidebar--release-window (window frame)
  "Stop WINDOW on FRAME being the sidebar's, without ever signalling.
`delete-window' is only correct for a side window that has a parent.
The sidebar buffer in an ordinary window sits in a window the sidebar
does not own, and a window with no parent is the frame's sole window,
whose `delete-window' signals \"Attempt to delete minibuffer or sole
ordinary window\" -- both are released in place instead: side and
protection parameters cleared, dedication dropped, and another buffer
shown. Returns WINDOW when it survived, nil when it was deleted."
  (when (window-live-p window)
    (if (and (window-parameter window 'window-side) (window-parent window))
        (progn (delete-window window) nil)
      (dolist (parameter '(window-side window-slot
                           no-other-window no-delete-other-windows
                           mode-line-format))
        (set-window-parameter window parameter nil))
      (set-window-dedicated-p window nil)
      (let* ((sidebar (window-buffer window))
             (other (other-buffer sidebar t frame)))
        (set-window-buffer window
                           (if (eq other sidebar)
                               (get-buffer-create "*scratch*")
                             other)))
      window)))

(defun edmacs-sidebar-hide (frame)
  "Hide FRAME's sidebar window, if shown.
Deletes the window when the sidebar genuinely owns one; otherwise
releases it in place rather than signalling -- see
`edmacs-sidebar--release-window'. Returns the surviving window, or nil.
Runs `edmacs-sidebar-visibility-functions' with (FRAME `hidden') on every
return path. Interactively, FRAME is always the selected frame."
  (interactive (list (selected-frame)))
  (prog1 (let ((window (edmacs-sidebar--window frame)))
           (when window
             (edmacs-sidebar--release-window window frame)))
    (run-hook-with-args 'edmacs-sidebar-visibility-functions frame 'hidden)))

;; The repaired frame has a main window again but no sidebar; this is the
;; hook `edmacs-windows-normalize-frame' runs to put one back. Safe as a hook
;; member because `edmacs-sidebar-show' reaches
;; `display-buffer-in-side-window' directly rather than through
;; `display-buffer' -- see its docstring. `run-hook-with-args' funcalls
;; this directly with FRAME, never through `call-interactively', so
;; `edmacs-sidebar-show' being a required-argument command is fine here.
(add-hook 'edmacs-windows-frame-repaired-functions #'edmacs-sidebar-show)

;;;###autoload
(defun edmacs-sidebar-toggle (frame)
  "Hide FRAME's sidebar window if shown, else show it.
Interactively, FRAME is always the selected frame."
  (interactive (list (selected-frame)))
  (if (edmacs-sidebar--window frame)
      (edmacs-sidebar-hide frame)
    (edmacs-sidebar-show frame)))

;;;###autoload
(defun edmacs-sidebar-collapse (frame)
  "Narrow FRAME's sidebar to a strip, `edmacs-sidebar--collapsed-width'
columns wide -- collapse means narrow, not hide (three other commands
already hide: `edmacs-sidebar-hide', `edmacs-sidebar-toggle', closing
the window directly). The `edmacs-sidebar-collapsed' frame parameter is
set FIRST, so even a frame whose sidebar has no live window at all
still ends up flagged for the next `edmacs-sidebar-show' (e.g. the
tab-open hook) to open directly at the collapsed width -- no
full-width-then-flash-resize. `edmacs-sidebar-show' picks up the new
width and redraws its buffer via `edmacs-sidebar--ensure-buffer'; the
explicit `edmacs-sidebar--redraw' call after it is belt-and-suspenders,
guaranteeing the collapsed render even on a future refactor of
`--show''s own internals. Interactively, FRAME is always the selected
frame."
  (interactive (list (selected-frame)))
  (set-frame-parameter frame 'edmacs-sidebar-collapsed t)
  (edmacs-sidebar-show frame)
  (edmacs-sidebar--redraw frame))

;;;###autoload
(defun edmacs-sidebar-expand (frame)
  "Restore FRAME's sidebar to its remembered width -- the inverse of
`edmacs-sidebar-collapse'. `--remember-width' refuses to stash anything
while collapsed, so the value `edmacs-sidebar-show' reads back here is
exactly the width from before the collapse. Interactively, FRAME is
always the selected frame."
  (interactive (list (selected-frame)))
  (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)
  (edmacs-sidebar-show frame)
  (edmacs-sidebar--redraw frame))

;;;###autoload
(defun edmacs-sidebar-toggle-collapse (frame)
  "Collapse FRAME's sidebar if expanded, else expand it.
Unlike `edmacs-sidebar-toggle' (hide/show), this never changes whether
the sidebar window exists at all -- only its width and render.
Interactively, FRAME is always the selected frame."
  (interactive (list (selected-frame)))
  (if (frame-parameter frame 'edmacs-sidebar-collapsed)
      (edmacs-sidebar-expand frame)
    (edmacs-sidebar-collapse frame)))

;;;###autoload
(defun edmacs-sidebar-reset-width (frame)
  "Clear FRAME's remembered sidebar width back to `edmacs-sidebar-width'.
Only clears `edmacs-sidebar-remembered-width'; when the sidebar is
currently shown on FRAME, also hides and re-shows it so the reset is
visible immediately rather than waiting for the next hide/show cycle.
A pure reset -- it does not touch `edmacs-sidebar--clamp-width' or the
stash-gating rule in `edmacs-sidebar--remember-width', which prevent
this parameter from being poisoned again going forward.
Interactively, FRAME is always the selected frame."
  (interactive (list (selected-frame)))
  (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
  (when (edmacs-sidebar--window frame)
    (edmacs-sidebar-hide frame)
    (edmacs-sidebar-show frame)))

;; ============================================================================
;; Redraw triggers
;; ============================================================================

(defun edmacs-sidebar--on-tab-select (_from-tab _to-tab)
  "Invalidate the selected frame's sidebar; moves the current-tab marker."
  ;; `tab-bar-tab-post-select-functions' calls with (FROM-TAB TO-TAB), no
  ;; frame slot -- ambient-reads: ok
  (edmacs-sidebar-invalidate (selected-frame)))

(add-hook 'tab-bar-tab-post-select-functions #'edmacs-sidebar--on-tab-select)

(defun edmacs-sidebar--on-tab-group-change (_tab)
  "Invalidate the selected frame's sidebar after `tab-bar-change-tab-group'.
A project row's label and a worktree row's nesting both depend on tab-bar
group membership (`edmacs-sidebar--plan-projects'), so a group change
alone -- with no buffer-list activity at all -- must still redraw the
tree; before this there was no trigger for it whatsoever.
`tab-bar-tab-post-change-group-functions' calls with (TAB), no frame
slot, and -- like `tab-bar-change-tab-group' itself -- always operates
on the selected frame."
  ;; ambient-reads: ok -- see the docstring above.
  (edmacs-sidebar-invalidate (selected-frame)))

(add-hook 'tab-bar-tab-post-change-group-functions #'edmacs-sidebar--on-tab-group-change)

(defun edmacs-sidebar--on-tab-root-set (_root frame)
  "Invalidate FRAME's sidebar after `edmacs-workspaces-set-tab-root' stamps a
new root onto one of its tabs. Registered on workspaces.el's own
`edmacs-workspaces-tab-root-set-functions' seam -- see that variable's
docstring for why this is a hook member rather than a direct call. ROOT
itself is unused: FRAME's whole tree, not just the stamped tab's own
row, may depend on it (a project row derives its main root from
whichever member of its tabs resolves one)."
  (edmacs-sidebar-invalidate frame))

(add-hook 'edmacs-workspaces-tab-root-set-functions #'edmacs-sidebar--on-tab-root-set)

(defun edmacs-sidebar--on-tab-open (_tab &optional frame)
  "Re-show the sidebar in a new tab of FRAME (default selected).
Registered on workspaces.el's `edmacs-workspaces-tab-post-open-functions'
rather than on core's `tab-bar-tab-post-open-functions', which
workspaces.el now solely owns: that seam guarantees the new tab is
already stamped with its worktree root when this runs, so the first tree
drawn for it files it under the right project instead of under none.

`edmacs-sidebar-show', not `edmacs-sidebar-invalidate': `tab-bar-new-tab'
runs `delete-other-windows' before any post-open hook fires, so the new
tab has no side window at all and `edmacs-sidebar--redraw' would no-op
on it. `show' is what recreates the window, and it redraws internally."
  (edmacs-sidebar-show (or frame (selected-frame))))

(add-hook 'edmacs-workspaces-tab-post-open-functions #'edmacs-sidebar--on-tab-open)

(defun edmacs-sidebar--on-tab-pre-close (_tab _last-tab-p)
  "Invalidate after the closing tab is actually removed from `tab-bar-tabs'.
`tab-bar-tab-pre-close-functions' fires BEFORE that removal, so an
invalidation here would still redraw a stale tree if it flushed before
the removal lands; deferred one tick instead, same as before this
routed through `edmacs-sidebar-invalidate'. `frame-live-p' is checked
because the last-tab-p `delete-frame' branch can run and destroy the
frame between this hook firing and the timer executing."
  ;; `tab-bar-tab-pre-close-functions' calls with (TAB LAST-TAB-P), no
  ;; frame slot -- ambient-reads: ok
  (let ((frame (selected-frame)))
    (run-at-time 0 nil
                 (lambda ()
                   (when (frame-live-p frame)
                     (edmacs-sidebar-invalidate frame))))))

(add-hook 'tab-bar-tab-pre-close-functions #'edmacs-sidebar--on-tab-pre-close)

(defun edmacs-sidebar--after-tab-rename (&rest _)
  "Invalidate the selected frame's sidebar after `tab-bar-rename-tab'.
`tab-bar-rename-tab' has no dedicated hook; it always targets the current
tab of the current frame, so this has nothing to key off besides the
selected frame. Advice on a fixed `(&rest _)' signature, same reasoning
as the hooks above."
  ;; ambient-reads: ok -- see the docstring above.
  (edmacs-sidebar-invalidate (selected-frame)))

;; A named function, not a lambda: `advice-add' with a symbol is
;; idempotent, so re-evaluating this file leaves one advice rather than
;; stacking another.
(advice-add 'tab-bar-rename-tab :after #'edmacs-sidebar--after-tab-rename)

;; ============================================================================
;; Hide the tab-bar strip; the sidebar is the model's only visible list
;; ============================================================================
;; Hides the strip without disabling `tab-bar-mode' -- `SPC T' stays intact.
;; Must go through the setter: it is what pushes `tab-bar-lines' to 0 on frames
;; that already exist. sessions.el turns `tab-bar-mode' on before this file
;; loads, so a bare `setq' leaves those frames showing the strip.

(customize-set-variable 'tab-bar-show nil)

;; ============================================================================
;; window-sides-slots: claim LEFT, cap 1
;; ============================================================================
;; One sidebar per frame, so the left column holds exactly one slot.

(edmacs-windows-claim-side 'left 1 'sidebar)

;; ============================================================================
;; SPC t s - toggle
;; ============================================================================
;; Populates keybindings.el's existing "toggle" ignore-stub, the way
;; git.el populates "SPC g" -- see modules/keybindings.el's "t" prefix.
;; Guarded by `with-eval-after-load' (unlike git.el/sessions.el, which load
;; only in a real init.el session) so this file stays loadable standalone
;; under `-Q --batch', which has no `general' -- see sidebar-test.el.

(with-eval-after-load 'general
  (general-define-key
   :states 'normal
   :prefix "SPC t"
   "s" '(edmacs-sidebar-toggle :which-key "toggle sidebar")
   "S" '(edmacs-sidebar-reset-width :which-key "reset sidebar width")
   "c" '(edmacs-sidebar-toggle-collapse :which-key "collapse sidebar")))

;; ============================================================================
;; Desktop - exclude the buffer, regenerate a live one after restore
;; ============================================================================
;; Exclusion alone would leave a desktop-restored window pointing at
;; nothing; regeneration alone would race a still-live window pointing at
;; whatever desktop.el left behind. Both are needed.

(add-to-list 'desktop-modes-not-to-save 'edmacs-sidebar-mode)

(defun edmacs-sidebar--on-desktop-read ()
  "Regenerate every live frame's sidebar after a desktop restore.
A named function, not an anonymous lambda, so tests can invoke exactly
what `desktop-after-read-hook' runs: `desktop-read' itself is a no-op
under `-Q --batch' (\"This function is a no-op when Emacs is running in
batch mode\", per its own docstring), so this is the only way to exercise
this half of the restore path in the ERT suite -- see sidebar-test.el."
  (dolist (f (frame-list))
    (edmacs-sidebar-show f)))

(add-hook 'desktop-after-read-hook #'edmacs-sidebar--on-desktop-read)

;; Under the daemon, `sessions.el's `edmacs-sessions--restore-pending-frameset'
;; restores a stashed frameset from `after-make-frame-functions', deferred
;; one tick, which lands AFTER `desktop-after-read-hook' already fired at
;; boot -- so this needs its own entry on the same hook, deferred the same
;; way. Appended (depth 100, rather than the default which prepends) so it
;; runs after sessions.el's entry (added at load time, before this module
;; loads) and actually observes the frameset having landed on FRAME rather
;; than racing ahead of it.
(defun edmacs-sidebar--regenerate-after-frame (frame)
  "Show FRAME's sidebar once any pending frameset restore has landed on it."
  (run-at-time 0 nil
               (lambda ()
                 (when (frame-live-p frame)
                   (edmacs-sidebar-show frame)))))

(add-hook 'after-make-frame-functions #'edmacs-sidebar--regenerate-after-frame 100)

(provide 'sidebar)
;;; sidebar.el ends here
