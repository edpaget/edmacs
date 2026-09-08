;;; java-test.el --- Tests for languages/java.el -*- lexical-binding: t -*-

;;; Commentary:
;; Proves the phase-5 deletion (roadmap-edmacs-builtins) actually removed
;; every jdtls-protocol and debugger keybinding from `java-ts-mode-map', not
;; just from the visible source text: loads the real `evil'/`general'
;; packages from this checkout's (or its sibling main checkout's)
;; `straight/repos', loads the real `modules/languages/java.el', fires its
;; deferred `use-package' bodies the way `rust-test.el' fires rustic's, and
;; walks the resulting keymap with `lookup-key'/`evil-get-auxiliary-keymap'.
;;
;; Run with this file on the command line, from the repository root -- both
;; the module path (via `load-file-name') and the `straight/repos' lookup
;; (via `default-directory') depend on it:
;;
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/languages/java-test.el -f ert-run-tests-batch-and-exit
;;
;; The keymap-walking tests need a bootstrapped straight tree (evil.el,
;; general.el, and the `mvn'/`gradle-mode' packages on disk) to run for
;; real; without one they `ert-skip' rather than silently passing. A
;; worktree's `straight/repos' is empty by design (see .claude/CLAUDE.md,
;; Worktrees) -- run from the main checkout to get real coverage.
;;
;; `mvn.el' and `emacs-gradle-mode' are never actually loaded from disk:
;; both are `provide'd as fake features before their gating `with-eval-
;; after-load'/`:after' triggers fire, exactly as rust-test.el fakes
;; `rustic' -- so this file needs no straight-managed package beyond
;; evil.el and general.el themselves.
;;
;; No test here `cl-letf's a C subr, so the
;; `native-comp-enable-subr-trampolines' guard documented in
;; .claude/CLAUDE.md is deliberately not needed.

;;; Code:

(require 'ert)
(require 'eglot)
(require 'subr-x)
(require 'cl-lib)

(defvar java-test--module
  (expand-file-name
   "java.el"
   (file-name-directory
    (or load-file-name buffer-file-name
        (expand-file-name "modules/languages/java-test.el" default-directory))))
  "This checkout's `modules/languages/java.el'.")

(defun java-test--locate-straight-repos-root ()
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

(defvar java-test--repos-root (java-test--locate-straight-repos-root)
  "This checkout's (or its sibling main checkout's) `straight/repos' root.")

(defvar java-test--real-keymap-deps
  (and java-test--repos-root
       (file-exists-p (expand-file-name "evil/evil.el" java-test--repos-root))
       (file-exists-p (expand-file-name "general.el/general.el" java-test--repos-root)))
  "Non-nil when real evil.el/general.el were found for the keymap-walk tests.")

(if java-test--real-keymap-deps
    (progn
      (add-to-list 'load-path (expand-file-name "evil" java-test--repos-root))
      (add-to-list 'load-path (expand-file-name "general.el" java-test--repos-root))
      (require 'evil)
      (require 'general))
  (unless (fboundp 'general-define-key)
    (defalias 'general-define-key (lambda (&rest _) nil))))

(defvar java-ts-mode-map (make-sparse-keymap))

(load java-test--module nil t)

;; Fire the deferred `use-package' bodies. `mvn' is `provide'd before
;; `java-ts-mode' so the `:demand t' block's internal `(require 'mvn)' is a
;; no-op instead of trying to load a real file; `gradle-mode' fires
;; independently of `java-ts-mode' since its own `:config' is gated only on
;; its own feature.
(provide 'mvn)
(provide 'java-ts-mode)
(provide 'gradle-mode)

(defmacro java-test--skip-unless-real-keymap-deps ()
  "Skip the calling test when real evil.el/general.el were not found."
  `(unless java-test--real-keymap-deps
     (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test")))

(defun java-test--normal-aux-keymap ()
  "The evil normal-state auxiliary keymap general registered on `java-ts-mode-map'."
  (evil-get-auxiliary-keymap java-ts-mode-map 'normal))

(defun java-test--bound-p (aux key)
  "Non-nil when KEY resolves to a real binding (keymap or command) in AUX.
`lookup-key' returns an integer, not nil, for an unbound multi-event KEY
-- \"too long\", per its docstring -- so a plain truthiness check on the
result would wrongly treat every unbound multi-key sequence as bound."
  (let ((binding (lookup-key aux (kbd key))))
    (and binding (not (integerp binding)))))

(defun java-test--collect-commands (keymap)
  "Return every command symbol bound anywhere in KEYMAP, recursively."
  (let (commands)
    (when (keymapp keymap)
      (map-keymap
       (lambda (_event binding)
         (cond
          ((keymapp binding)
           (setq commands (append commands (java-test--collect-commands binding))))
          ((symbolp binding)
           (push binding commands))))
       keymap))
    commands))

;; ============================================================================
;; Keymap-walking tests -- need real evil/general
;; ============================================================================

(ert-deftest java-test-mvn-and-gradle-bindings-survive ()
  "Sanity check that the fixture actually fired mvn/gradle-mode's `:config' --
otherwise the dead-binding tests below would pass vacuously."
  (java-test--skip-unless-real-keymap-deps)
  (let ((aux (java-test--normal-aux-keymap)))
    (should (eq (lookup-key aux (kbd ", m c")) 'mvn-clean))
    (should (eq (lookup-key aux (kbd ", g b")) 'gradle-build))))

(ert-deftest java-test-spc-c-refactor-and-build-groups-gone ()
  "Every `SPC c' which-key group the deleted LSP-Java block owned is gone."
  (java-test--skip-unless-real-keymap-deps)
  (let ((aux (java-test--normal-aux-keymap)))
    (dolist (group '("o" "b" "X" "K" "T" "=" "H"))
      (should-not (java-test--bound-p aux (concat "SPC c " group))))))

(ert-deftest java-test-spc-d-prefix-gone ()
  "The whole `SPC d' debug prefix the deleted dap-mode block owned is gone."
  (java-test--skip-unless-real-keymap-deps)
  (let ((aux (java-test--normal-aux-keymap)))
    (should-not (java-test--bound-p aux "SPC d"))))

(ert-deftest java-test-no-dead-command-symbols-in-keymap ()
  "No command anywhere on `java-ts-mode-map' names a deleted lsp-java-*/dap-*
command -- a stronger, symbol-level check than the prefix-lookup tests above."
  (java-test--skip-unless-real-keymap-deps)
  (let ((aux (java-test--normal-aux-keymap)))
    (should aux)
    (dolist (cmd (java-test--collect-commands aux))
      (should-not (string-match-p "\\`lsp-java-\\|\\`dap-" (symbol-name cmd))))))

;; ============================================================================
;; Source-level and hook/config tests -- no real evil/general needed
;; ============================================================================

(ert-deftest java-test-hook-is-eglot-ensure-not-lsp-deferred ()
  (should (memq 'eglot-ensure java-ts-mode-hook))
  (should-not (memq 'lsp-deferred java-ts-mode-hook)))

(ert-deftest java-test-workspace-configuration-java-section ()
  (should (equal (plist-get (default-value 'eglot-workspace-configuration) :java)
                 '(:import (:gradle (:enabled t) :maven (:enabled t))
                   :maven (:downloadSources t)
                   :autobuild (:enabled t)))))

(ert-deftest java-test-no-lsp-java-or-dap-source-text ()
  "Mirrors the `grep -rn 'lsp-java\\|dap-'' acceptance criterion."
  (should-not (with-temp-buffer
                (insert-file-contents java-test--module)
                (re-search-forward "lsp-java\\|dap-" nil t))))

(provide 'java-test)
;;; java-test.el ends here
