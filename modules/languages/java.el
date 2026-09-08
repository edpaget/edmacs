;;; java.el --- Java language configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Java development setup with eglot/jdtls, navigation-only (see the
;; roadmap-edmacs-builtins phase-5 commit for the accepted feature loss:
;; the jdtls-protocol refactoring/generation commands and the whole
;; debugger integration this file used to wrap). jdtls itself is not
;; bundled: install it via the `[tools.jdtls]' entry in
;; `~/.config/mise/config.toml' (`mise install'), then confirm it resolves
;; through a *login* shell -- `$SHELL -l -c "command -v jdtls"' -- since
;; the daemon's PATH comes from `exec-path-from-shell' in core.el, which
;; only sees a login shell's environment. eglot's bundled
;; `eglot-server-programs' entry for `(java-mode java-ts-mode)' already
;; contacts a bare "jdtls" on PATH; no custom entry is needed once that
;; resolves.
;; This file is loaded on-demand when opening Java files.

;;; Code:

;; ============================================================================
;; Java Mode (built-in)
;; ============================================================================

;; Real .java buffers run `java-ts-mode' (treesit-auto remaps at mode
;; resolution), so this :mode entry is inert. No :config: cc-mode never
;; `(provide 'java-mode)', so it would never run.
(use-package java-mode
  :straight nil
  :mode "\\.java\\'")

(defvar eglot-workspace-configuration)
(declare-function eglot-semantic-tokens-mode "eglot")
(declare-function eglot-managed-p "eglot")

;; jdtls' own settings keys (the "java.*" section VS Code's redhat.java
;; extension uses), not the higher-level wrapper names the former LSP Java
;; package used for the same settings. Global rather than buffer-local, and
;; merged rather than assigned: eglot resolves this in a temp buffer of its
;; own, and every language shares the one plist. Only the settings worth
;; keeping without that wrapper -- build-tool import, downloading dependency
;; sources for navigation, and keeping the workspace built for fresh
;; diagnostics -- survive; the completion/code-lens/signature-help tuning
;; the old package also set is dropped, along with the Google-style
;; formatter (its XML profile was never actually committed to this repo --
;; `find . -iname '*google-style*'' finds nothing even on the pre-migration
;; checkout -- so there was no real formatting behavior to port; `SPC c f'
;; still runs jdtls' own default-style formatter via eglot).
(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                (plist-put (default-value 'eglot-workspace-configuration)
                           :java
                           '(:import (:gradle (:enabled t) :maven (:enabled t))
                             :maven (:downloadSources t)
                             :autobuild (:enabled t)))))

(with-eval-after-load 'java-ts-mode
  (add-hook 'java-ts-mode-hook #'eglot-ensure)

  ;; java-ts-mode ignores cc-mode's c-basic-offset.
  (setq java-ts-mode-indent-offset 4)

  ;; tab-width and indent-tabs-mode are auto-buffer-local; a setq here would
  ;; bind them only in whichever buffer was current at load. Set per buffer.
  (add-hook 'java-ts-mode-hook
            (lambda ()
              (setq tab-width 4
                    indent-tabs-mode nil))))

;; Global via the hook, not java-ts-mode-local: `eglot-semantic-tokens-mode'
;; is a per-buffer minor mode with no per-language switch, so hooking it here
;; turns it on in every eglot-managed buffer, not just Java's. This is where
;; `lsp-semantic-tokens-enable t' above used to live, so the equivalent
;; decision is recorded here even though its effect is global.
;;
;; `eglot-managed-mode-hook' also fires on the *disable* transition (eglot
;; clears its own bookkeeping only after running the hook), so a no-arg call
;; here must be guarded the same way `edmacs--eglot-disable-flycheck' guards
;; its own consumption of this hook in programming.el -- otherwise a
;; shutdown/reconnect turns semantic-tokens-mode back on in a buffer eglot
;; just stopped managing.
(defun edmacs--eglot-enable-semantic-tokens ()
  "Turn on `eglot-semantic-tokens-mode' when eglot manages this buffer."
  (when (eglot-managed-p)
    (eglot-semantic-tokens-mode 1)))

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'edmacs--eglot-enable-semantic-tokens))

;; ============================================================================
;; Maven Integration
;; ============================================================================

(use-package mvn
  ;; `:after' alone still waits for an autoload trigger, and nothing calls
  ;; the mvn commands before the `, m' bindings in :config exist. `:demand t'
  ;; loads it as soon as java-ts-mode does.
  :after java-ts-mode
  :demand t
  :commands (mvn-clean mvn-compile mvn-test)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'java-ts-mode-map
   :prefix ", m"
   "" '(:ignore t :which-key "maven")
   "c" '(mvn-clean :which-key "clean")
   "C" '(mvn-compile :which-key "compile")
   "t" '(mvn-test :which-key "test")
   "p" '(mvn-package :which-key "package")
   "i" '(mvn-install :which-key "install")
   "r" '(mvn-run :which-key "run")))

;; ============================================================================
;; Gradle Integration
;; ============================================================================

(use-package gradle-mode
  :hook (java-ts-mode . gradle-mode)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'java-ts-mode-map
   :prefix ", g"
   "" '(:ignore t :which-key "gradle")
   "b" '(gradle-build :which-key "build")
   "t" '(gradle-test :which-key "test")
   "r" '(gradle-run :which-key "run")
   "e" '(gradle-execute :which-key "execute")))

(provide 'java)
;;; java.el ends here
