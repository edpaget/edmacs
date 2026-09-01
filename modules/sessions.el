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
;;     in tab-bar.el), so nothing extra needs wiring here.
;;   - `bufferlo' layers the per-tab buffer lists that desktop.el
;;     deliberately does not persist.
;;
;; Worktree switching needs no new code: Emacs 31's vc.el already ships
;; `vc-switch-working-tree' (C-x v w w) and `vc-working-tree-switch-project'
;; (C-x v w s), and project.el already treats each linked worktree as its
;; own project (`project--submodule-p' deliberately excludes linked
;; worktrees from folding into the parent).  Those raw `C-x v w ...' chords
;; are shadowed in evil normal state, though -- see the note below -- so
;; this module also exposes `SPC p w' as the primary, normal-state-safe
;; entry point, and `SPC T' likewise for tab lifecycle management.
;;
;; perspective.el is deliberately avoided: its own README states it
;; cannot save shell/REPL/compilation buffers, it is incompatible with
;; desktop.el by design, and it has open bugs that lose perspectives when
;; the last emacsclient frame closes.  activities.el, beframe, and any
;; other tmux-replacement package are likewise out of scope for this
;; phase.
;;
;; KNOWN SHADOW: `modules/evil-config.el' binds bare `C-x' in
;; `evil-normal-state-map' to `evil-numbers/dec-at-pt' (a deliberate,
;; pre-existing vim-native mapping mirroring vim's own C-a/C-x
;; increment/decrement-at-point).  Normal state is the default editing
;; state in this config, and because that binding is a leaf command (not
;; a nested prefix keymap), pressing `C-x' there terminates the key
;; sequence immediately -- so `C-x t p' (`project-other-tab-command') and
;; every `C-x v w ...' chord below are unreachable from evil normal state.
;; They remain reachable from Insert/Emacs/Motion state and always via
;; `M-x'.  This is out of this phase's scope to fix (rebinding
;; evil-numbers would be a separate, higher-risk change nobody asked
;; for), so the bindings below are the tested, normal-state-safe path,
;; and the raw C-x chords are a documented secondary path.

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

(defun edmacs-sessions--tab-name ()
  "Name the current tab after its project/worktree, falling back sanely.
Uses `project-current' so each tab's label reflects the worktree it
holds; when no project is found (e.g. a scratch tab), falls back to
`tab-bar-tab-name-current' default behavior (buffer name of the
selected window)."
  (if-let* ((proj (project-current))
            (root (project-root proj)))
      (file-name-nondirectory (directory-file-name root))
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
      desktop-load-locked-desktop t)

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
(with-eval-after-load 'vterm
  (add-to-list 'desktop-modes-not-to-save 'vterm-mode))
(with-eval-after-load 'claude-repl-buffer
  (add-to-list 'desktop-modes-not-to-save 'claude-repl-buffer-mode))

(desktop-save-mode 1)

;; ============================================================================
;; Bufferlo - per-tab buffer lists (desktop.el deliberately omits these)
;; ============================================================================

(use-package bufferlo
  :config
  (bufferlo-mode 1))

;; ============================================================================
;; Leader-key bindings
;; ============================================================================
;; Self-registered here (outside the central leader-def in
;; modules/keybindings.el) following the git.el/vterm.el/ai.el convention
;; of modules owning their own SPC-prefixed bindings, rather than the
;; keybindings.el-centralized convention core.el's window-rotation
;; bindings use. Either is an established pattern in this repo; this
;; module picks the self-registering one so it stays fully self-contained.

;; SPC T - tab lifecycle (primary entry point for AC1; see the shadow
;; note above for why this replaces the raw C-x t ... chords).
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

;; SPC p w - worktree switching (primary entry point for AC3; wraps the
;; stock vc.el worktree commands, which are already globally bound under
;; C-x v w but shadowed in evil normal state -- see the note above).
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
