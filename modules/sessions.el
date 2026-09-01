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
;; unrelated to this phase -- see the note below for how this module
;; makes them reachable there anyway -- and `SPC p w'/`SPC T' expose the
;; same commands as a fast, no-delay alternative.
;;
;; perspective.el is deliberately avoided: its own README states it
;; cannot save shell/REPL/compilation buffers, it is incompatible with
;; desktop.el by design, and it has open bugs that lose perspectives when
;; the last emacsclient frame closes.  activities.el, beframe, and any
;; other tmux-replacement package are likewise out of scope for this
;; phase.
;;
;; FORMER SHADOW, NOW DISPATCHED: `modules/evil-config.el' binds bare
;; `C-x' in `evil-normal-state-map' to `evil-numbers/dec-at-pt' (a
;; deliberate, pre-existing vim-native mapping mirroring vim's own C-a/C-x
;; increment/decrement-at-point).  Normal state is the default editing
;; state in this config, and because that binding used to be a leaf
;; command (not a nested prefix keymap), pressing `C-x' there terminated
;; the key sequence immediately -- so `C-x t p' (`project-other-tab-command')
;; and every `C-x v w ...' chord this phase names were unreachable from
;; evil normal state without leaving it.  Rather than touch evil-numbers'
;; own binding (pre-existing, unrelated to this phase, and rebinding it
;; would cost real muscle memory), the "C-x dispatch" section below
;; redirects C-x through `general-key-dispatch' so both behaviors coexist:
;; the worktree/tab chords this phase's ACs literally name now work from
;; normal state, and bare C-x with nothing typed after it still decrements
;; the number at point exactly as before (after a short grace period, to
;; give the dispatch a chance to see whether a chord follows).  `SPC T'
;; and `SPC p w' below remain the fast, no-delay path to the same
;; commands.

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

(defun edmacs-sessions--git-common-dir (root)
  "Return the git common directory for the worktree at ROOT, or nil.
Every worktree of one repository shares this path (it is the main
checkout's `.git', per git-worktree(1)), so its parent directory names
the repository independent of any individual worktree's own directory
name."
  (let ((default-directory root))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "rev-parse" "--git-common-dir"))
        (string-trim (buffer-string))))))

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
                          (file-name-directory (directory-file-name (expand-file-name common))))))))
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
;; C-x dispatch - un-shadow the worktree/tab chords this phase's ACs name
;; ============================================================================
;; See the "FORMER SHADOW, NOW DISPATCHED" note above the top of this file.
;; `general-key-dispatch' (general.el is already a dependency of this repo
;; via modules/keybindings.el) creates a command that waits, bounded by
;; TIMEOUT, for the next key(s): a match below runs that command; no
;; match (including a timeout with nothing typed) falls back to
;; `evil-numbers/dec-at-pt', simulating any unmatched keys afterward --
;; i.e. exactly today's behavior for every C-x chord this list doesn't
;; know about. This is scoped deliberately narrow (the specific chords
;; this phase's body and ACs name), not a blanket un-shadow of the whole
;; `ctl-x-map' -- broadening that is a separate, unrelated change.
(with-eval-after-load 'evil-numbers
  (define-key evil-normal-state-map (kbd "C-x")
    (general-key-dispatch 'evil-numbers/dec-at-pt
      :timeout 0.4
      :name edmacs-sessions--c-x-dispatch
      :docstring "Decrement number at point (vim's `C-x'), or run one of
this phase's tab/worktree commands when C-x is followed by
`t p'/`v w w'/`v w s'/`v w k'/`v w a'/`v w A'."
      "t p" 'project-other-tab-command
      "v w w" 'vc-switch-working-tree
      "v w s" 'vc-working-tree-switch-project
      "v w k" 'vc-kill-other-working-tree-buffers
      "v w a" 'vc-apply-to-other-working-tree
      "v w A" 'vc-apply-root-to-other-working-tree)))

;; ============================================================================
;; Leader-key bindings
;; ============================================================================
;; Self-registered here (outside the central leader-def in
;; modules/keybindings.el) following the git.el/vterm.el/ai.el convention
;; of modules owning their own SPC-prefixed bindings, rather than the
;; keybindings.el-centralized convention core.el's window-rotation
;; bindings use. Either is an established pattern in this repo; this
;; module picks the self-registering one so it stays fully self-contained.

;; SPC T - tab lifecycle: the fast, no-delay path (the C-x dispatch
;; above also makes `C-x t p' work from normal state, but incurs its
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
;; vc.el worktree commands (the C-x dispatch above also makes the literal
;; `C-x v w s' etc. chords work from normal state, per the phase's
;; "Done when" wording, but incurs its grace-period timeout).
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
