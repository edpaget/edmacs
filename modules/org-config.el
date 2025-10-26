;;; org-config.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org mode setup with modern enhancements and org-roam for note-taking.

;;; Code:

;; ============================================================================
;; Org Mode - Built-in
;; ============================================================================

(use-package org
  :straight (:type built-in)
  :config
  ;; Basic org settings
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-return-follows-link t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-indented t
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  ;; Agenda settings
  (setq org-agenda-files (list org-directory)
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t)

  ;; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Org keybindings with general.el
  (general-define-key
   :states 'normal
   :prefix "SPC o"
   "" '(:ignore t :which-key "org")
   "a" '(org-agenda :which-key "agenda")
   "c" '(org-capture :which-key "capture")
   "l" '(org-store-link :which-key "store link")
   "t" '(org-todo :which-key "todo")
   "s" '(org-schedule :which-key "schedule")
   "d" '(org-deadline :which-key "deadline")
   "e" '(org-export-dispatch :which-key "export")
   "i" '(org-insert-link :which-key "insert link"))

  ;; Local leader for org-mode buffers
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   :prefix ","
   "a" '(org-archive-subtree :which-key "archive")
   "t" '(org-set-tags-command :which-key "tags")
   "p" '(org-priority :which-key "priority")
   "r" '(org-refile :which-key "refile")
   "s" '(org-insert-structure-template :which-key "insert template")))

;; ============================================================================
;; Org Modern - Beautiful org-mode
;; ============================================================================

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace
        org-modern-table nil
        org-modern-keyword nil
        org-modern-checkbox nil
        org-modern-tag nil))

;; ============================================================================
;; Org Appear - Show hidden markup when cursor is on it
;; ============================================================================

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t))

;; ============================================================================
;; Org Roam - Zettelkasten note-taking
;; ============================================================================

(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/org-roam")
        org-roam-v2-ack t)

  :config
  ;; Create org-roam directory if it doesn't exist
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))

  (org-roam-db-autosync-mode)

  ;; Org-roam keybindings
  (general-define-key
   :states 'normal
   :prefix "SPC o"
   "r" '(:ignore t :which-key "roam")
   "rf" '(org-roam-node-find :which-key "find node")
   "ri" '(org-roam-node-insert :which-key "insert node")
   "rc" '(org-roam-capture :which-key "capture")
   "rb" '(org-roam-buffer-toggle :which-key "buffer toggle")
   "rg" '(org-roam-graph :which-key "graph"))

  ;; Local leader for org-roam in org-mode
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   :prefix ","
   "n" '(:ignore t :which-key "roam")
   "ni" '(org-roam-node-insert :which-key "insert node")
   "nt" '(org-roam-tag-add :which-key "add tag")
   "na" '(org-roam-alias-add :which-key "add alias")
   "nb" '(org-roam-buffer-toggle :which-key "buffer toggle")))

;; ============================================================================
;; Org Roam UI - Web interface for org-roam (optional)
;; ============================================================================

;; Uncomment to enable org-roam-ui
;; (use-package org-roam-ui
;;   :after org-roam
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t)
;;
;;   (general-define-key
;;    :states 'normal
;;    :prefix "SPC o r"
;;    "u" '(org-roam-ui-open :which-key "ui")))

;; ============================================================================
;; Org Babel - Execute code blocks
;; ============================================================================

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (js . t)))

  ;; Don't ask for confirmation before evaluating code blocks
  (setq org-confirm-babel-evaluate nil))

;; ============================================================================
;; Org Export Settings
;; ============================================================================

(with-eval-after-load 'org
  (setq org-export-with-smart-quotes t
        org-export-with-toc t
        org-export-with-author t
        org-export-with-email t
        org-export-with-date t))

;;; org-config.el ends here
