;;; git.el --- Git integration -*- lexical-binding: t -*-

;;; Commentary:
;; Git integration using Magit and related packages.

;;; Code:

;; ============================================================================
;; Magit - The best Git interface
;; ============================================================================
;; Note: transient and its dependencies are loaded early in core.el

(use-package magit
  :commands (magit-status magit-diff-unstaged magit-diff-staged magit-commit
             magit-push magit-pull magit-fetch magit-fetch-all magit-log
             magit-log-current magit-branch magit-blame magit-stage-file
             magit-unstage-file)
  :config
  ;; Magit settings
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk t))

;; Git keybindings - defined outside use-package so they're available immediately
(general-define-key
 :states 'normal
 :prefix "SPC g"
 "" '(:ignore t :which-key "git")
 "g" '(magit-status :which-key "status")
 "d" '(magit-diff-unstaged :which-key "diff unstaged")
 "D" '(magit-diff-staged :which-key "diff staged")
 "c" '(magit-commit :which-key "commit")
 "p" '(magit-push :which-key "push")
 "P" '(magit-pull :which-key "pull")
 "f" '(magit-fetch :which-key "fetch")
 "F" '(magit-fetch-all :which-key "fetch all")
 "l" '(magit-log :which-key "log")
 "L" '(magit-log-current :which-key "log current")
 "b" '(magit-branch :which-key "branch")
 "B" '(magit-blame :which-key "blame")
 "s" '(magit-stage-file :which-key "stage file")
 "u" '(magit-unstage-file :which-key "unstage file"))

;; ============================================================================
;; Magit Evil Integration
;; ============================================================================

;; Evil bindings for magit are provided by evil-collection
;; See modules/evil-config.el where evil-collection is configured with magit

;; ============================================================================
;; Diff-hl - Show git diff in the fringe
;; ============================================================================

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)

  ;; Integration with Magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  ;; Keybindings for diff-hl
  (general-define-key
   :states 'normal
   :prefix "SPC g"
   "h" '(:ignore t :which-key "hunk")
   "hn" '(diff-hl-next-hunk :which-key "next hunk")
   "hp" '(diff-hl-previous-hunk :which-key "previous hunk")
   "hr" '(diff-hl-revert-hunk :which-key "revert hunk")
   "hs" '(diff-hl-stage-current-hunk :which-key "stage hunk")))

;; ============================================================================
;; Git Timemachine - Step through git history
;; ============================================================================

(use-package git-timemachine
  :commands git-timemachine
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC g"
   "t" '(git-timemachine :which-key "timemachine"))

  ;; Keybindings in timemachine mode
  (with-eval-after-load 'git-timemachine
    (evil-define-key 'normal git-timemachine-mode-map
      (kbd "C-k") 'git-timemachine-show-previous-revision
      (kbd "C-j") 'git-timemachine-show-next-revision
      (kbd "q") 'git-timemachine-quit)))

;; ============================================================================
;; Git Gutter - Alternative to diff-hl (disabled by default)
;; ============================================================================

;; Uncomment if you prefer git-gutter over diff-hl
;; (use-package git-gutter
;;   :config
;;   (global-git-gutter-mode +1)
;;   (general-define-key
;;    :states 'normal
;;    :prefix "SPC g"
;;    "n" '(git-gutter:next-hunk :which-key "next hunk")
;;    "p" '(git-gutter:previous-hunk :which-key "previous hunk")))

;; ============================================================================
;; Forge - GitHub/GitLab integration (optional)
;; ============================================================================

;; Uncomment to enable Forge for GitHub/GitLab integration
;; (use-package forge
;;   :after magit
;;   :config
;;   (general-define-key
;;    :states 'normal
;;    :prefix "SPC g"
;;    "'" '(forge-dispatch :which-key "forge")))

;;; git.el ends here
