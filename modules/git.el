;;; git.el --- Git integration -*- lexical-binding: t -*-

;;; Commentary:
;; Git integration using Magit and related packages.

;;; Code:

;; ============================================================================
;; Magit - The best Git interface
;; ============================================================================
(use-package magit
  :commands (magit-status magit-diff-unstaged magit-diff-staged magit-commit
             magit-push magit-pull magit-fetch magit-fetch-all magit-log
             magit-log-current magit-branch magit-blame magit-stage-file
             magit-unstage-file)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        ;; Word-level refinement on every hunk is expensive when auto-revert
        ;; refreshes a visible magit-status every 5s. Still available per hunk
        ;; via `magit-diff-toggle-refine-hunk'.
        magit-diff-refine-hunk nil))

;; Outside use-package so bindings exist before magit autoloads.
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
;; Diff-hl - Show git diff in the fringe
;; ============================================================================

(use-package diff-hl
  ;; Loaded by the after-init call below; :defer t just stops use-package
  ;; requiring it here.
  :defer t
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Calling these autoloaded commands is what loads diff-hl.
(add-hook 'after-init-hook
          (lambda ()
            (global-diff-hl-mode)
            (diff-hl-flydiff-mode)
            (diff-hl-margin-mode)))

;; Outside use-package so bindings exist before diff-hl loads.
(general-define-key
 :states 'normal
 :prefix "SPC g"
 "h" '(:ignore t :which-key "hunk")
 "hn" '(diff-hl-next-hunk :which-key "next hunk")
 "hp" '(diff-hl-previous-hunk :which-key "previous hunk")
 "hr" '(diff-hl-revert-hunk :which-key "revert hunk")
 "hs" '(diff-hl-stage-current-hunk :which-key "stage hunk"))

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
