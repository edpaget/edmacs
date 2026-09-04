;;; keybindings-test.el --- Tests for keybindings.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for leader-def and local-leader-def keybindings with focus on
;; motion-state support (sidebar and other motion-state buffers).
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/keybindings-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'seq)

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
override keymap. This test checks that general-override-mode-map has
an auxiliary keymap for motion state containing leader bindings."
  (unless (edmacs-keybindings-test--ensure-keybindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  ;; Verify the override keymap exists and is a keymap
  (should (keymapp (evil-get-auxiliary-keymap general-override-mode-map 'motion)))
  ;; Verify that a leader binding resolves through the motion-state override keymap.
  ;; SPC f f is bound to find-file in keybindings.el, so we verify it resolves
  ;; through the motion-state override keymap.
  (let ((motion-keymap (evil-get-auxiliary-keymap general-override-mode-map 'motion)))
    (should (functionp (lookup-key motion-keymap (kbd "SPC f f"))))))

(ert-deftest edmacs-keybindings-test-leader-motion-state-end-to-end ()
  "End-to-end test: verify SPC leader bindings work in a motion-state buffer.
This test activates motion state on a real buffer and uses the full keymap
stack to verify that SPC leader bindings resolve correctly. This proves
the fix (adding motion to leader-def's :states) works in practice."
  (unless (edmacs-keybindings-test--ensure-keybindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (with-temp-buffer
    ;; Enable evil mode and activate motion state
    (evil-local-mode 1)
    (evil-motion-state)
    ;; Verify motion state is active
    (should (eq evil-state 'motion))
    ;; Get the composed keymap stack for motion state
    (let* ((maps (mapcar #'cdr (evil-state-keymaps 'motion)))
           (composed (make-composed-keymap maps)))
      ;; Verify that SPC f f (find-file) resolves through the keymap stack
      (should (functionp (lookup-key composed (kbd "SPC f f"))))
      ;; Verify the resolved binding is find-file
      (should (eq (lookup-key composed (kbd "SPC f f")) 'find-file)))))

(ert-deftest edmacs-keybindings-test-leader-motion-state-not-in-base-evil-map ()
  "Documents that SPC leader bindings in motion state come through the
override auxiliary keymap mechanism, not a direct mutation of
`evil-motion-state-map'. This proves the fix uses general.el's override
mechanism, not evil's base state map."
  (unless (edmacs-keybindings-test--ensure-keybindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  ;; In evil.el, SPC in motion state is bound to evil-forward-char by default.
  ;; Our fix does not change evil-motion-state-map directly; instead, it adds
  ;; motion state to leader-def's :states, which uses general.el's override
  ;; keymap mechanism (above the evil state map in the resolution chain).
  ;; This assertion documents that behavior: evil-motion-state-map should still
  ;; have the original SPC binding, while the override keymap shadows it.
  (should (eq (lookup-key evil-motion-state-map (kbd "SPC")) 'evil-forward-char)))

(ert-deftest edmacs-keybindings-test-local-leader-motion-state-binding ()
  "Verify that local-leader-def (comma prefix) reaches its keymap in motion state.
This test binds a test key through local-leader-def and verifies it's
accessible in a motion-state buffer, proving local-leader-def's motion state
works correctly."
  (unless (edmacs-keybindings-test--ensure-keybindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  ;; Bind a test key through local-leader-def
  (local-leader-def
    "t" '(ignore :which-key "test local-leader motion binding"))
  ;; Verify the binding exists in the motion-state override keymap
  (let ((motion-keymap (evil-get-auxiliary-keymap general-override-mode-map 'motion)))
    (should (keymapp motion-keymap))
    ;; Check that the test binding resolves
    (should (functionp (lookup-key motion-keymap (kbd ", t"))))))

(ert-deftest edmacs-keybindings-test-local-leader-motion-state-end-to-end ()
  "End-to-end test: verify comma (local-leader) bindings work in a motion-state buffer.
This test activates motion state on a real buffer and verifies that a
local-leader binding resolves through the full keymap stack."
  (unless (edmacs-keybindings-test--ensure-keybindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  ;; Bind a test key through local-leader-def
  (local-leader-def
    "m" '(ignore :which-key "test motion local-leader"))
  (with-temp-buffer
    ;; Enable evil mode and activate motion state
    (evil-local-mode 1)
    (evil-motion-state)
    ;; Verify motion state is active
    (should (eq evil-state 'motion))
    ;; Get the composed keymap stack for motion state
    (let* ((maps (mapcar #'cdr (evil-state-keymaps 'motion)))
           (composed (make-composed-keymap maps)))
      ;; Verify that , m (comma + m) resolves through the keymap stack
      (should (functionp (lookup-key composed (kbd ", m"))))
      ;; Verify the resolved binding is ignore (our test binding)
      (should (eq (lookup-key composed (kbd ", m")) 'ignore)))))

(provide 'keybindings-test)
;;; keybindings-test.el ends here
