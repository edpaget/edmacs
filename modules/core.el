;;; core.el --- Core Emacs settings -*- lexical-binding: t -*-

;;; Commentary:
;; Basic Emacs settings, performance tweaks, and quality-of-life improvements.

;;; Code:

;; ============================================================================
;; Transient - built and its dependencies force-loaded (not required eagerly)
;; ============================================================================
;; Transient is used by magit and combobulate. `compat'/`cond-let' are its
;; own dependencies and must still be built here regardless of load timing.
;;
;; The eager `(require 'transient)' that used to sit here (with a comment
;; claiming it avoided an unspecified load-order issue) was removed on
;; 2026-09-01 as part of edmacs-performance/phase-2-defer-eager-packages,
;; after re-testing found no reproduction of any such issue: magit is
;; :commands-deferred (modules/git.el) and `require's transient internally
;; the first time any magit command actually loads it, confirmed via a full
;; interactive-equivalent boot of this config followed by `(require 'magit)'
;; and `(call-interactively 'magit-status)' with an empty `*Warnings*'
;; buffer and no error. If a load-order bug against transient ever
;; resurfaces, reproduce it concretely (which package, which function, what
;; error) before re-adding an eager require here -- don't restore this line
;; on suspicion alone.

(straight-use-package 'compat)
(straight-use-package 'cond-let)
(straight-use-package 'transient)

(require 'compat)
(require 'cond-let)

;; ============================================================================
;; Environment Variables from Shell
;; ============================================================================

;; Ensure Emacs uses the same PATH and environment as your shell
;; This is especially important on macOS where GUI apps don't inherit shell env
;; LSP_USE_PLISTS decision (edmacs-performance/phase-5-lsp-and-completion-io):
;; DECLINED. The variable is absent from this repo and from the
;; `exec-path-from-shell-variables' list immediately below. Even if it were
;; added there, `exec-path-from-shell-initialize' (a few lines down) runs
;; during init -- long after straight.el has already byte-compiled lsp-mode,
;; and lsp-mode's plist-vs-hash-table representation is gated at
;; byte-compile time, not read at runtime from the environment. Making
;; LSP_USE_PLISTS take effect would require (a) exporting it in the real
;; shell environment *before* Emacs starts, so it's present at Emacs launch
;; regardless of exec-path-from-shell, and (b) a forced rebuild of lsp-mode
;; (straight-use-package with a fresh build) so the byte-compiled gate picks
;; it up. Judged not worth the operational complexity for the expected gain;
;; not re-adding it here is deliberate, not an oversight.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  ;; Copy these environment variables from shell
  (setq exec-path-from-shell-variables
        '("PATH"
          "MANPATH"
          "ANTHROPIC_API_KEY"  ; For AI assistant
          "JAVA_HOME"
          "GOPATH"
          "CARGO_HOME"
          "RUSTUP_HOME"
          "PYENV_ROOT"
          "NVM_DIR"))

  ;; Use interactive login shell to get full environment
  (setq exec-path-from-shell-arguments '("-l"))

  (exec-path-from-shell-initialize))

;; ============================================================================
;; Environment Integration (mise)
;; ============================================================================

;; Per-project environment and tool versions come from the mise configuration
;; chain: the closest `mise.toml' / `.mise.toml' / `.tool-versions', layered
;; under `~/.config/mise/config.toml'.  mise.el discovers that whole chain via
;; `mise config ls --json' and sets the resulting variables buffer-locally, the
;; same per-buffer model envrc used for direnv, so a process launched from a
;; project buffer sees that project's toolchain.
;;
;; Activated on `after-init' rather than at load time: mise.el shells out to the
;; mise binary (`config ls', `trust --show', `env') and doing that during init
;; costs real startup time for no benefit.  This is the same eager-activation
;; trap envrc had here.
;;
;; Note: on first activation mise.el sets `experimental' to true in the *global*
;; mise config if it is not already, because it relies on experimental CLI
;; surface.  That is a write to ~/.config/mise/config.toml, not to this repo.
(use-package mise
  :hook (after-init . global-mise-mode)
  :config
  ;; Prompt before trusting a newly-seen config rather than blanket-trusting.
  ;; Answering yes runs `mise trust --all', which trusts the whole parent chain
  ;; and subdirectories, not just the one file.
  (setq mise-trust 'ask))

(defun edmacs-mise-trust ()
  "Trust the mise config chain for the current buffer's directory."
  (interactive)
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (if (zerop (call-process "mise" nil nil nil "trust"))
        (progn (message "mise: trusted %s" default-directory)
               (when (fboundp 'mise-update-buffer) (mise-update-buffer)))
      (message "mise: trust failed in %s" default-directory))))

(defun edmacs-mise-untrust ()
  "Stop trusting the mise config chain for the current buffer's directory."
  (interactive)
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (if (zerop (call-process "mise" nil nil nil "trust" "--untrust"))
        (progn (message "mise: untrusted %s" default-directory)
               (when (fboundp 'mise-update-buffer) (mise-update-buffer)))
      (message "mise: untrust failed in %s" default-directory))))

;; Keybindings for manual control (using SPC-e prefix for environment).
;; Key strings are unchanged from the envrc bindings these replace.
(with-eval-after-load 'general
  (general-define-key
   :states '(normal visual)
   :prefix "SPC e"
   "e" '(:ignore t :which-key "environment")
   "ea" '(edmacs-mise-trust :which-key "mise trust")
   "ed" '(edmacs-mise-untrust :which-key "mise untrust")
   "er" '(mise-update-buffer :which-key "mise reload buffer")
   "ep" '(mise-update-dir :which-key "mise reload dir")))

;; ============================================================================
;; Basic Settings
;; ============================================================================

;; UTF-8 encoding everywhere
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Better defaults
(setq-default
 ;; Indentation
 indent-tabs-mode nil           ; Use spaces instead of tabs
 tab-width 4                    ; Set tab width to 4 spaces
 fill-column 80                 ; Set fill column to 80 characters

 ;; Line wrapping
 truncate-lines nil             ; Enable line wrapping
 word-wrap t                    ; Wrap at word boundaries

 ;; Scrolling
 scroll-margin 0                ; Scroll margin
 scroll-preserve-screen-position t

 ;; Misc
 require-final-newline t        ; Always end files with newline
 )

;; ============================================================================
;; Performance
;; ============================================================================
;; Cheap redisplay/perf settings that cost nothing at startup but prevent
;; per-redisplay overhead from compounding with this config's other features
;; (rainbow-delimiters, flycheck, tree-sitter, hl-todo, global-auto-revert,
;; diff-hl, etc.).

;; Undo limits, raised from Emacs's stock 160000/240000 bytes (~156KB/234KB).
;; This config pairs evil's undo with apheleia format-on-save and
;; `lsp-rename' -- both routinely produce a single undo change bigger than
;; the stock limits, which silently truncates history ("Undo history was
;; truncated" in *Messages*) and discards whatever undo branches it can no
;; longer reach. `undo-outer-limit' (the hard per-command cap, default
;; ~24MB) is left at its default -- these two are the soft limits that
;; decide when Emacs starts discarding old undo entries to make room, not
;; the point at which it refuses to record a change at all. These are core
;; Emacs undo variables (not evil- or undo-tree-specific), so they belong
;; here rather than in evil-config.el even though evil's undo system is
;; what mainly benefits.
(setq undo-limit (* 3 1024 1024)          ; ~3MB
      undo-strong-limit (* 16 1024 1024)) ; ~16MB

;; Detect very long lines (minified bundles, lockfiles, wide CSVs) and
;; neutralize expensive per-line features (font-lock, visual-line-mode,
;; rainbow-delimiters, etc.) for that buffer instead of freezing on them.
;; so-long's own so-long-minor-modes list does not include evil, and
;; evil-mode is a globalized minor mode that reasserts evil-local-mode via
;; after-change-major-mode-hook on every major-mode switch (including into
;; so-long-mode), so it has to be added explicitly or it stays fully active
;; in the so-long'd buffer.
(with-eval-after-load 'evil
  (add-to-list 'so-long-minor-modes 'evil-local-mode))
(global-so-long-mode 1)

;; This config is exclusively left-to-right code; skip per-paragraph
;; direction auto-detection and the bracket-pair-algorithm redisplay cost,
;; which lands hardest on bracket-dense buffers where rainbow-delimiters
;; is unconditionally enabled.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; This is an all-Git setup; skip the extra vc-registered probes against
;; RCS/CVS/SVN/SCCS/SRC/Bzr/Hg on every file visit.
(setq vc-handled-backends '(Git))

;; Keep fontification off the typing path.
(setq redisplay-skip-fontification-on-input t)

;; toggle-font-size switches faces across default/fixed-pitch/variable-pitch
;; at runtime, and nerd-icons brings in large glyph-count fonts; don't
;; compact font caches on GC.
(setq inhibit-compacting-font-caches t)

;; Disable startup screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Disable some GUI elements (redundant with early-init, but kept for clarity)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Frame title
(setq frame-title-format '("%b - Emacs"))

;; Yes/No prompts become y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable bell
(setq ring-bell-function 'ignore)

;; ============================================================================
;; File Handling
;; ============================================================================

;; Backup and autosave configuration
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

;; Create backup and auto-save directories if they don't exist
(let ((backup-dir (expand-file-name "backups" user-emacs-directory))
      (auto-save-dir (expand-file-name "auto-save" user-emacs-directory)))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

;; Backup settings
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Auto-save settings
(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;; ============================================================================
;; Recent Files
;; ============================================================================

(use-package recentf
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude
               (expand-file-name "straight" user-emacs-directory))
  (add-to-list 'recentf-exclude
               (expand-file-name "backups" user-emacs-directory))
  (recentf-mode 1))

;; ============================================================================
;; Save History
;; ============================================================================

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;; ============================================================================
;; Save Place
;; ============================================================================

(use-package saveplace
  :config
  (save-place-mode 1))

;; ============================================================================
;; Auto-revert
;; ============================================================================

(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

;; ============================================================================
;; Electric Pair Mode
;; ============================================================================

(use-package elec-pair
  :config
  (electric-pair-mode 1))

;; ============================================================================
;; Show Matching Parentheses
;; ============================================================================

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; ============================================================================
;; Which Key - Show available keybindings
;; ============================================================================

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3
        which-key-separator " → "
        which-key-prefix-prefix "+")
  (which-key-mode 1))

;; ============================================================================
;; Project Management
;; ============================================================================

(use-package project
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-dired "Dired")
          (project-shell "Shell")))

  ;; Every rdm worktree vendors its own straight.el package checkouts
  ;; and per-project caches; never let those be remembered as projects
  ;; in their own right, whether via the walk below, straight.el
  ;; visiting a checkout, or anything else that calls
  ;; `project-remember-project'.
  (setq project-list-exclude '("/straight/repos/" "/\\.eldev/" "/node_modules/"))

  ;; rdm reaps worktrees on disk out from under project.el's memory of
  ;; them; prune entries that no longer exist so stale paths don't
  ;; linger in `project-list-file'. project.el's real symbol for this
  ;; is `project-forget-zombie-projects' (not the
  ;; `project-prune-zombie-projects' name sometimes used informally).
  (when (fboundp 'project-forget-zombie-projects)
    (add-hook 'emacs-startup-hook #'project-forget-zombie-projects))

  ;; One-time walk to register every live rdm worktree with project.el
  ;; so it appears in `project-switch-project' history and
  ;; `project-known-project-roots' without needing to be visited
  ;; interactively first. (This is unrelated to `C-x v w s' /
  ;; `vc-switch-working-tree', which lists worktrees straight from
  ;; `git worktree list' via vc-git.el and never consults
  ;; project-list-file.) Non-recursive: each worktree directory itself
  ;; is the project root one level below `*__worktrees/', so recursing
  ;; further would also walk into and register every vendored package
  ;; checkout under each worktree's straight/repos/.
  (defun edmacs--register-project-worktrees ()
    "Register every rdm worktree under ~/Projects/*__worktrees/ with project.el."
    (ignore-errors
      (dolist (worktrees-dir (file-expand-wildcards
                               (expand-file-name "*__worktrees" "~/Projects")))
        (when (file-directory-p worktrees-dir)
          (project-remember-projects-under worktrees-dir)))))
  (add-hook 'emacs-startup-hook #'edmacs--register-project-worktrees))

;; ============================================================================
;; Display Line Numbers
;; ============================================================================

(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; ============================================================================
;; Highlight Current Line
;; ============================================================================

(global-hl-line-mode 1)

;; ============================================================================
;; Better Help
;; ============================================================================

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

;;; core.el ends here
