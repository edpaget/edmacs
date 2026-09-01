;;; sessions.el --- Tabs, layout persistence, and worktree switching -*- lexical-binding: t -*-

;;; Commentary:
;; Replaces the tmux session layer with built-in Emacs 31 primitives:
;;
;;   - `tab-bar-mode' gives one tab per active rdm worktree.  A tab is a
;;     window configuration, not a process/state boundary (frames, tabs,
;;     and a single frame are all the same Emacs process), but tabs have
;;     the fewest sharp edges of the three: claude-code-ide.el is
;;     explicitly tab-aware, and per-frame is where manzaltu#197 (second+
;;     terminal in another frame stops tracking resizes) and ghostel#504
;;     (a terminal PTY clamps to the smallest window showing it) live.
;;   - `desktop-save-mode' persists each tab's `window-state-get' layout
;;     across restarts with no extra packages; tab-bar.el registers its
;;     own frameset filter for the non-printable parts unconditionally at
;;     load time (see the `(push '(tabs . frameset-filter-tabs) ...)' call
;;     in tab-bar.el).  The phase's chosen topology is one persistent
;;     daemon, though, and desktop.el's own frameset restore is a no-op
;;     against a daemon's placeholder initial frame (there's no real frame
;;     yet to size windows into) -- this module bridges that gap itself;
;;     see the "Bridge desktop-read's daemon-mode frameset skip" section
;;     below for the mechanism and why it can't just call
;;     `desktop-restore-frameset' again later.
;;   - `bufferlo' layers the per-tab buffer lists that desktop.el
;;     deliberately does not persist.
;;
;; Worktree switching needs no new code: Emacs 31's vc.el already ships
;; `vc-switch-working-tree' (C-x v w w) and `vc-working-tree-switch-project'
;; (C-x v w s), and project.el already treats each linked worktree as its
;; own project (`project--submodule-p' deliberately excludes linked
;; worktrees from folding into the parent).  Those raw `C-x v w ...'
;; chords are shadowed in evil normal state by a pre-existing binding
;; unrelated to this phase -- see the "C-x chords" section below for how
;; this module reaches past it -- and `SPC p w'/`SPC T' expose the same
;; commands as a fast, no-delay alternative.
;;
;; perspective.el is deliberately avoided: its own README states it
;; cannot save shell/REPL/compilation buffers, it is incompatible with
;; desktop.el by design, and it has open bugs that lose perspectives when
;; the last emacsclient frame closes.  activities.el, beframe, and any
;; other tmux-replacement package are likewise out of scope for this
;; phase.
;;
;; FORMER SHADOW, NOW EXTENDED: `modules/evil-config.el' binds bare
;; `C-x' in `evil-normal-state-map' to `evil-numbers/dec-at-pt' (a
;; deliberate, pre-existing vim-native mapping mirroring vim's own C-a/C-x
;; increment/decrement-at-point).  Normal state is the default editing
;; state in this config, and because that binding used to be a leaf
;; command (not a nested prefix keymap), pressing `C-x' there terminated
;; the key sequence immediately -- so `C-x t p' (`project-other-tab-command')
;; and every `C-x v w ...' chord this phase names were unreachable from
;; evil normal state without leaving it.  Rather than shadow evil-config's
;; binding with a second, competing `define-key' on that same keymap entry
;; (which would win or lose purely by whether this module happens to load
;; *after* evil-config.el -- a load-order-dependent landmine), this module
;; registers its chords through `edmacs-evil-config-add-c-x-chord', the
;; extension point evil-config.el exposes for exactly this: the
;; worktree/tab chords this phase's ACs literally name now work from
;; normal state, and bare C-x with nothing typed after it still decrements
;; the number at point exactly as before (after a short grace period, to
;; give the dispatch a chance to see whether a chord follows), regardless
;; of module load order.  `SPC T' and `SPC p w' below remain the fast,
;; no-delay path to the same commands.

;;; Code:

;; ============================================================================
;; Tab Bar - one tab per active worktree
;; ============================================================================

(require 'tab-bar)

;; `tab-bar-define-keys' is a defcustom (not a function, despite how the
;; phase that motivated this module phrased it), and it must be set
;; *before* `tab-bar-mode' is turned on since its :set function installs
;; bindings into `tab-bar-mode-map' immediately.  The keys it gates under
;; the default `t' are C-<tab>/C-S-<tab>/C-S-iso-lefttab (confirmed by
;; reading tab-bar.el's `tab-bar--define-keys') -- there is no bare-TAB
;; tab-bar binding in this Emacs 31.1 build to steal from evil.  Setting
;; it to nil means tab-bar never touches those chords at all, leaving
;; them free for evil/general.el; tab switching instead goes through the
;; `SPC T' bindings below.
(setq tab-bar-define-keys nil)

(defvar edmacs-sessions--git-common-dir-cache (make-hash-table :test #'equal)
  "Memoized ROOT -> git-common-dir results from `edmacs-sessions--git-common-dir'.
`tab-bar-tabs' recomputes the *current* tab's name via
`tab-bar-tab-name-function' on essentially every redisplay of the tab
line, not merely on tab creation, so without this cache each worktree
tab would pay a synchronous git subprocess spawn on that same cadence.
A worktree's git-common-dir cannot change during the life of a running
Emacs, so entries are never invalidated. A miss is cached too (as the
symbol `none', since a plain nil can't be told apart from \"not yet
looked up\" in `gethash''s single optional-default arg) so a
worktree git can't identify doesn't get re-shelled-out-to forever.")

(defun edmacs-sessions--git-common-dir-1 (root)
  "Uncached implementation of `edmacs-sessions--git-common-dir' for ROOT.
Every worktree of one repository shares this path (it is the main
checkout's `.git', per git-worktree(1)), so its parent directory names
the repository independent of any individual worktree's own directory
name.

Uses `process-file', not `call-process': ROOT may be a TRAMP remote
directory (project.el and vc.el both support those), and
`call-process' is documented to run in `default-directory' only when
that is local, silently falling back to running the command in `~'
otherwise -- exactly the directory-confusion bug this function exists
to avoid, just relocated to a remote-vs-local split instead of a
stale-`default-directory' one. `process-file' dispatches through
TRAMP for a remote `default-directory' and runs locally otherwise.

For a linked worktree, git prints an already-absolute path here; for
the *main* worktree, though, it prints a path relative to the
directory git was invoked from (typically \".git\"). That expansion to
an absolute path has to happen right here, while `default-directory'
is still bound to ROOT -- a caller expanding the returned string later
against its own, unrelated `default-directory' (e.g. whatever buffer
happens to be selected when tab-bar recomputes the tab name) would
silently resolve it against the wrong base and misname the tab.

When ROOT is remote, git itself only ever prints a bare on-host path
(git has no notion of TRAMP), so an already-\"absolute\" answer like
\"/home/user/repo/.git\" still needs ROOT's own TRAMP method/host
prefix grafted back on by hand -- `expand-file-name' leaves an
already-absolute NAME untouched and would otherwise silently drop the
remote host, resolving to a same-named but purely local path."
  (let ((default-directory root))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil "rev-parse" "--git-common-dir"))
        (let ((raw (string-trim (buffer-string)))
              (remote (file-remote-p root)))
          (cond
           ((not (file-name-absolute-p raw)) (expand-file-name raw root))
           ((and remote (not (file-remote-p raw))) (concat remote raw))
           (t raw)))))))

(defun edmacs-sessions--git-common-dir (root)
  "Return the absolute git common directory for the worktree at ROOT, or nil.
Memoized per ROOT; see `edmacs-sessions--git-common-dir-cache' and
`edmacs-sessions--git-common-dir-1' for why and how."
  (let ((cached (gethash root edmacs-sessions--git-common-dir-cache 'edmacs-sessions--miss)))
    (if (not (eq cached 'edmacs-sessions--miss))
        (and (not (eq cached 'none)) cached)
      (let ((result (edmacs-sessions--git-common-dir-1 root)))
        (puthash root (or result 'none) edmacs-sessions--git-common-dir-cache)
        result))))

(defun edmacs-sessions--tab-name ()
  "Name the current tab after its project/worktree, falling back sanely.
Uses `project-current' so each tab's label reflects the worktree it
holds; when no project is found (e.g. a scratch tab), falls back to
`tab-bar-tab-name-current' default behavior (buffer name of the
selected window).

Two worktrees of *different* repositories can share a directory
basename (e.g. both named `feature-x', or two rdm worktrees named
`roadmap-foundation' from two different rdm projects), which a bare
basename would render as identical, ambiguous tab names. Disambiguate
by prefixing the owning repository's own directory name, derived from
`--git-common-dir' (shared by every worktree of one repo, so it names
the repo rather than the worktree)."
  (if-let* ((proj (project-current))
            (root (project-root proj)))
      (let* ((base (file-name-nondirectory (directory-file-name root)))
             (common (edmacs-sessions--git-common-dir root))
             (repo (and common
                        (file-name-nondirectory
                         (directory-file-name
                          (file-name-directory (directory-file-name common)))))))
        (if (and repo (not (string= repo base)))
            (format "%s/%s" repo base)
          base))
    (tab-bar-tab-name-current)))

(setq tab-bar-tab-name-function #'edmacs-sessions--tab-name)

(tab-bar-mode 1)

;; ============================================================================
;; Desktop - persist tab/window layout across restarts
;; ============================================================================

(require 'desktop)

(setq desktop-dirname (expand-file-name ".cache/desktop/" user-emacs-directory)
      desktop-path (list desktop-dirname)
      desktop-save t
      desktop-restore-frames t
      desktop-load-locked-desktop t
      ;; Restore the first 10 buffers of each tab's list eagerly; the
      ;; rest load lazily on idle. With bufferlo persisting every tab's
      ;; *full* buffer list (not just what's visible), a worktree tab
      ;; left with dozens of buried buffers would otherwise block
      ;; daemon startup reopening all of them up front.
      desktop-restore-eager 10)

(unless (file-directory-p desktop-dirname)
  (make-directory desktop-dirname t))

;; vterm buffers wrap a live PTY that desktop.el cannot resume -- without
;; excluding them, restore would leave a dead, unusable buffer behind.
;; claude-repl-buffer-mode buffers are in the same boat: they front a
;; live claude CLI subprocess (see modules/claude-repl/claude-repl-process.el)
;; that desktop.el has no way to reattach either, so restoring one would
;; produce a read-only-looking transcript that silently can no longer talk
;; to Claude. Default to excluding both; a follow-up task tracks giving
;; claude-repl a proper "resume this project's session" path instead of
;; restoring a transcript with no process behind it (see the task filed
;; alongside this commit).
;; `comint-mode' covers every other PTY/subprocess-backed buffer this
;; config can open (M-x shell, inferior REPLs, etc.): none of them have
;; a live process to reattach to after a restart either, and unlike
;; vterm/claude-repl-buffer it is already loaded (part of Emacs) so no
;; `with-eval-after-load' gate is needed.
(add-to-list 'desktop-modes-not-to-save 'comint-mode)
(with-eval-after-load 'vterm
  (add-to-list 'desktop-modes-not-to-save 'vterm-mode))
(with-eval-after-load 'claude-repl-buffer
  (add-to-list 'desktop-modes-not-to-save 'claude-repl-buffer-mode))

(desktop-save-mode 1)

;; ----------------------------------------------------------------------------
;; Bridge desktop-read's daemon-mode frameset skip to the first real client
;; frame. This phase's topology is explicitly "one Emacs daemon" (see the
;; commentary above), and desktop.el has a documented no-op for exactly that
;; case: `desktop-restoring-frameset-p' (desktop.el) refuses to restore
;; frames/tabs when the selected frame is the daemon's placeholder
;; `terminal-frame' --
;;
;;   (not (and (daemonp) (eq (selected-frame) terminal-frame)))
;;
;; -- because there is no real frame yet to size/place windows into. Nothing
;; in desktop.el or server.el re-attempts the restore once an actual
;; `emacsclient' frame attaches, and worse, `desktop-read' unconditionally
;; sets `desktop-saved-frameset' back to nil right after running
;; `desktop-after-read-hook' -- so by the time a client connects, the saved
;; frameset is already gone even if something tried to restore it later.
;; The only place the frameset can still be read is inside
;; `desktop-after-read-hook' itself, before that reset runs.
;;
;; So: stash it there when running under the daemon guard, then replay it
;; with `frameset-restore' (what `desktop-restore-frameset' itself calls)
;; once `server-after-make-frame-hook' reports a real client frame is
;; selected. `desktop-restore-reuses-frames' defaults to t, so
;; `frameset-restore' reuses that just-connected frame instead of popping up
;; an unwanted extra one.
(require 'server)

(defvar edmacs-sessions--pending-frameset nil
  "Desktop frameset stashed at daemon boot, awaiting the first client frame.
Non-nil only between a daemon's `desktop-read' (which cannot restore
frames onto its placeholder initial frame) and the first `emacsclient'
frame attaching.")

(defun edmacs-sessions--stash-frameset-for-daemon ()
  "Stash `desktop-saved-frameset' when daemon boot skipped restoring it.
Runs on `desktop-after-read-hook', which fires after the frameset is
loaded but before `desktop-read' unconditionally nils it back out."
  (when (and (daemonp)
             desktop-saved-frameset
             (not (desktop-restoring-frameset-p)))
    (setq edmacs-sessions--pending-frameset desktop-saved-frameset)))

(add-hook 'desktop-after-read-hook #'edmacs-sessions--stash-frameset-for-daemon)

(defun edmacs-sessions--restore-pending-frameset-on-client-frame ()
  "Restore a daemon-boot-stashed frameset onto the first client frame.
`server-after-make-frame-hook' selects the client frame before running
this, so `frameset-restore' (with `desktop-restore-reuses-frames'
defaulting to t) reuses it instead of creating a new one. Runs once:
later client frames just get the normal, empty daemon frame."
  (when edmacs-sessions--pending-frameset
    (let ((desktop-saved-frameset edmacs-sessions--pending-frameset))
      (desktop-restore-frameset))
    (setq edmacs-sessions--pending-frameset nil)))

(add-hook 'server-after-make-frame-hook
          #'edmacs-sessions--restore-pending-frameset-on-client-frame)

;; ============================================================================
;; Bufferlo - per-tab buffer lists (desktop.el deliberately omits these)
;; ============================================================================

(use-package bufferlo
  :config
  (bufferlo-mode 1))

;; ============================================================================
;; C-x chords - reach the worktree/tab chords this phase's ACs name
;; ============================================================================
;; See the "FORMER SHADOW, NOW EXTENDED" note above the top of this file.
;; `edmacs-evil-config-add-c-x-chord' (modules/evil-config.el) is the sole
;; owner of the `C-x' entry in `evil-normal-state-map'; registering here
;; instead of a competing `define-key' means these chords work from
;; normal state regardless of whether evil-config.el or this module
;; loads first. This is scoped deliberately narrow (the specific chords
;; this phase's body and ACs name), not a blanket un-shadow of the whole
;; `ctl-x-map' -- broadening that is a separate, unrelated change.
(dolist (chord '(("t p" . project-other-tab-command)
                  ("v w w" . vc-switch-working-tree)
                  ("v w s" . vc-working-tree-switch-project)
                  ("v w k" . vc-kill-other-working-tree-buffers)
                  ("v w a" . vc-apply-to-other-working-tree)
                  ("v w A" . vc-apply-root-to-other-working-tree)))
  (edmacs-evil-config-add-c-x-chord (car chord) (cdr chord)))

;; ============================================================================
;; One tab per worktree - collapse duplicates from re-opening the same one
;; ============================================================================
;; `project-other-tab-command' (bound above at `C-x t p'/`SPC T p') always
;; creates and selects a brand-new tab -- it has no notion of "a tab for
;; this worktree already exists" -- so re-running it against a worktree
;; that already has a tab would otherwise leave two tabs for one
;; worktree, which the phase's "one tab per active worktree" acceptance
;; criterion rules out. `project-other-tab-command' resolves its target
;; project several layers down inside `project--other-place-prefix' and
;; whichever project sub-command the user picks, so pre-empting it with
;; a duplicate-directory check before it runs would mean reimplementing
;; that resolution; advising *around* it and reconciling afterward is
;; simpler and applies uniformly no matter which project sub-command was
;; invoked.
(defun edmacs-sessions--dedupe-tab-after-open (orig-fn &rest args)
  "Collapse a just-opened tab into its pre-existing worktree counterpart.
Runs ORIG-FN (`project-other-tab-command') with ARGS, then -- if the
freshly created and now-current tab's name collides with a tab that
already existed beforehand -- closes the new tab and switches to the
existing one instead, so a worktree that already has a tab never ends
up with a second one."
  (let ((before-tabs (tab-bar-tabs)))
    (apply orig-fn args)
    (let* ((new-name (funcall tab-bar-tab-name-function))
           (dup (seq-find (lambda (tab) (equal (alist-get 'name tab) new-name))
                           before-tabs))
           (dup-index (and dup (tab-bar--tab-index dup))))
      (when dup-index
        (tab-bar-close-tab nil (1+ dup-index))))))

(advice-add 'project-other-tab-command :around
            #'edmacs-sessions--dedupe-tab-after-open)

;; ============================================================================
;; Leader-key bindings
;; ============================================================================
;; Self-registered here (outside the central leader-def in
;; modules/keybindings.el) following the git.el/vterm.el/ai.el convention
;; of modules owning their own SPC-prefixed bindings, rather than the
;; keybindings.el-centralized convention core.el's window-rotation
;; bindings use. Either is an established pattern in this repo; this
;; module picks the self-registering one so it stays fully self-contained.

;; SPC T - tab lifecycle: the fast, no-delay path (the C-x chords
;; above also make `C-x t p' work from normal state, but incur their
;; grace-period timeout).
(general-define-key
 :states 'normal
 :prefix "SPC T"
 "" '(:ignore t :which-key "tabs")
 "p" '(project-other-tab-command :which-key "open worktree in new tab")
 "n" '(tab-bar-new-tab :which-key "new tab")
 "d" '(tab-bar-close-tab :which-key "close tab")
 "r" '(tab-bar-rename-tab :which-key "rename tab")
 "]" '(tab-bar-switch-to-next-tab :which-key "next tab")
 "[" '(tab-bar-switch-to-prev-tab :which-key "previous tab")
 "l" '(tab-bar-switch-to-tab :which-key "switch tab by name"))

;; SPC p w - worktree switching: the fast, no-delay path to the stock
;; vc.el worktree commands (the C-x chords above also make the literal
;; `C-x v w s' etc. chords work from normal state, per the phase's
;; "Done when" wording, but incur their grace-period timeout).
(general-define-key
 :states 'normal
 :prefix "SPC p w"
 "" '(:ignore t :which-key "worktree")
 "w" '(vc-switch-working-tree :which-key "visit file in other worktree")
 "s" '(vc-working-tree-switch-project :which-key "switch worktree (project)")
 "k" '(vc-kill-other-working-tree-buffers :which-key "kill other worktree buffers")
 "a" '(vc-apply-to-other-working-tree :which-key "apply to other worktree")
 "A" '(vc-apply-root-to-other-working-tree :which-key "apply root to other worktree"))

;;; sessions.el ends here
