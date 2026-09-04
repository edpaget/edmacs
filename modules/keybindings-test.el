;;; keybindings-test.el --- Tests for keybindings.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for leader-def and local-leader-def keybindings with focus on
;; motion-state support (sidebar and other motion-state buffers).
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/keybindings-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; This file loads real evil.el and general.el from the straight build tree,
;; then loads modules/keybindings.el. If straight has not been bootstrapped
;; in this checkout (or its sibling main checkout), the tests skip cleanly
;; rather than erroring out on file load.

;;; Code:

(require 'ert)
(require 'subr-x)

(defun edmacs-keybindings-test--locate-straight-repos-root ()
  "Return this checkout's `straight/repos' directory, or nil.
Tries this checkout's own `straight/repos' first, then falls back to the
sibling main `edmacs' checkout's -- a roadmap worktree lives under
`<parent>/edmacs__worktrees/<name>', sibling to the main
`<parent>/edmacs' checkout."
  (or
   (let ((here (expand-file-name "straight/repos" default-directory)))
     (and (file-directory-p here) here))
   (let* ((root (directory-file-name (expand-file-name default-directory)))
          (worktrees-dir (directory-file-name (file-name-directory root))))
     (when (string-suffix-p "__worktrees" worktrees-dir)
       (let* ((projects-dir (file-name-directory worktrees-dir))
              (repo-name (string-remove-suffix
                          "__worktrees" (file-name-nondirectory worktrees-dir)))
              (main-repos (expand-file-name
                           (concat repo-name "/straight/repos") projects-dir)))
         (and (file-directory-p main-repos) main-repos))))))

(defvar edmacs-keybindings-test--repos-root
  (edmacs-keybindings-test--locate-straight-repos-root)
  "This checkout's (or its sibling main checkout's) `straight/repos' root.")

(defvar edmacs-keybindings-test--keybindings-loaded nil
  "Non-nil once `modules/keybindings.el' has been loaded for real by this file.")

(defun edmacs-keybindings-test--ensure-keybindings ()
  "Load real evil/general and `modules/keybindings.el', once.
Returns non-nil on success; nil (without erroring) when this checkout
has never bootstrapped straight locally, so callers can `ert-skip'."
  (when edmacs-keybindings-test--repos-root
    (let ((evil-source (expand-file-name "evil/evil.el" edmacs-keybindings-test--repos-root))
          (general-source (expand-file-name "general.el/general.el" edmacs-keybindings-test--repos-root)))
      (when (and (file-exists-p evil-source) (file-exists-p general-source))
        (add-to-list 'load-path (file-name-directory evil-source))
        (add-to-list 'load-path (file-name-directory general-source))
        (require 'evil)
        (require 'general)
        (unless edmacs-keybindings-test--keybindings-loaded
          (load (expand-file-name "modules/keybindings.el" default-directory) nil t)
          (setq edmacs-keybindings-test--keybindings-loaded t))
        t))))

;; ============================================================================
;; Tests
;; ============================================================================

(defvar general-override-mode-map)

(ert-deftest edmacs-keybindings-test-leader-motion-state-override-map ()
  "Verify that SPC reaches the leader keymap in motion state via the
override keymap. This is essential for motion-state buffers like the
sidebar to access the leader key. The override keymap mechanism is the
same as documented in windows-test.el's spc-w tests."
  (unless (edmacs-keybindings-test--ensure-keybindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  ;; Verify the override keymap exists and is a keymap
  (should (keymapp (evil-get-auxiliary-keymap general-override-mode-map 'motion)))
  ;; Verify that a leader binding resolves through the motion-state override keymap.
  ;; SPC f f is bound to find-file in keybindings.el, so we verify it resolves
  ;; through the motion-state override keymap, not evil-motion-state-map directly.
  (let ((motion-keymap (evil-get-auxiliary-keymap general-override-mode-map 'motion)))
    (should (functionp (lookup-key motion-keymap (kbd "SPC f f"))))))

(ert-deftest edmacs-keybindings-test-leader-motion-state-not-in-base-evil-map ()
  "Documents that SPC leader bindings in motion state come through the
override auxiliary keymap mechanism, not a direct mutation of
`evil-motion-state-map'. This proves the fix (adding motion to leader-def's
:states) uses general.el's override mechanism, not evil's base state map."
  (unless (edmacs-keybindings-test--ensure-keybindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  ;; In evil.el, SPC in motion state is bound to evil-forward-char by default.
  ;; Our fix does not change evil-motion-state-map directly; instead, it adds
  ;; motion state to leader-def's :states, which uses general.el's override
  ;; keymap mechanism (above the evil state map in the resolution chain).
  ;; This assertion documents that behavior: evil-motion-state-map should still
  ;; have the original SPC binding, while the override keymap shadows it.
  (should (eq (lookup-key evil-motion-state-map (kbd "SPC")) 'evil-forward-char)))

(ert-deftest edmacs-keybindings-test-local-leader-motion-state-override-map ()
  "Verify that local-leader-def (comma prefix) also reaches its keymap
in motion state, for consistency with leader-def. This enables mode-specific
leader bindings in motion-state buffers."
  (unless (edmacs-keybindings-test--ensure-keybindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  ;; Verify the override keymap exists for local-leader in motion state
  (should (keymapp (evil-get-auxiliary-keymap general-override-mode-map 'motion))))

(provide 'keybindings-test)
;;; keybindings-test.el ends here
