;;; java.el --- Java language configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Java development setup with LSP (Eclipse JDT.LS), DAP debugger, and build tools.
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

(with-eval-after-load 'java-ts-mode
  (add-hook 'java-ts-mode-hook #'lsp-deferred)

  ;; java-ts-mode ignores cc-mode's c-basic-offset.
  (setq java-ts-mode-indent-offset 4)

  ;; tab-width and indent-tabs-mode are auto-buffer-local; a setq here would
  ;; bind them only in whichever buffer was current at load. Set per buffer.
  (add-hook 'java-ts-mode-hook
            (lambda ()
              (setq tab-width 4
                    indent-tabs-mode nil))))

;; ============================================================================
;; LSP Java - Eclipse JDT Language Server
;; ============================================================================

(use-package lsp-java
  :after lsp-mode
  :config
  (setq lsp-java-server-install-dir (expand-file-name "lsp/jdtls" user-emacs-directory)
        lsp-java-workspace-dir (expand-file-name "lsp/java-workspace" user-emacs-directory))

  (setq lsp-java-format-settings-url
        (lsp--path-to-uri (expand-file-name "eclipse-java-google-style.xml" user-emacs-directory))
        lsp-java-format-settings-profile "GoogleStyle"
        lsp-java-save-actions-organize-imports t
        lsp-java-autobuild-enabled t
        lsp-java-completion-enabled t
        lsp-java-completion-overwrite t
        lsp-java-completion-guess-method-arguments t
        lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-maven-download-sources t
        lsp-java-implementations-code-lens-enabled t
        lsp-java-references-code-lens-enabled t
        lsp-java-signature-help-enabled t)

  (setq lsp-semantic-tokens-enable t)

  (general-define-key
   :states 'normal
   :keymaps 'java-ts-mode-map
   :prefix "SPC c"
   ;; Organize imports
   "o" '(:ignore t :which-key "organize")
   "oi" '(lsp-java-organize-imports :which-key "imports")

   ;; Build
   "b" '(:ignore t :which-key "build")
   "bb" '(lsp-java-build-project :which-key "build project")
   "bc" '(lsp-java-build-project :which-key "compile")

   ;; Not on r/d/t or R/D: lsp-mode-map (a minor-mode map, so it wins) binds
   ;; those under SPC c in programming.el. X and K are free.
   "X" '(:ignore t :which-key "run")
   "Xr" '(dap-java-run-test-class :which-key "run class")
   "Xm" '(dap-java-run-test-method :which-key "run method")

   "K" '(:ignore t :which-key "debug")
   "Kd" '(dap-java-debug-test-class :which-key "debug class")
   "Km" '(dap-java-debug-test-method :which-key "debug method")

   ;; Tests
   "T" '(:ignore t :which-key "test")
   "Tt" '(dap-java-run-test-method :which-key "test method")
   "Tc" '(dap-java-run-test-class :which-key "test class")

   ;; Refactoring
   "=" '(:ignore t :which-key "refactor")
   "=i" '(lsp-java-add-import :which-key "add import")
   "=u" '(lsp-java-add-unimplemented-methods :which-key "add unimplemented")
   "=g" '(lsp-java-generate-getters-and-setters :which-key "getters/setters")
   "=t" '(lsp-java-generate-to-string :which-key "toString")
   "=e" '(lsp-java-generate-equals-and-hash-code :which-key "equals/hashCode")
   "=o" '(lsp-java-generate-overrides :which-key "overrides")

   ;; Not on h: lsp-mode-map binds SPC c h to hover.
   "H" '(:ignore t :which-key "hierarchy")
   "Ht" '(lsp-java-type-hierarchy :which-key "type hierarchy")))

;; ============================================================================
;; DAP Mode - Debug Adapter Protocol for Java
;; ============================================================================

(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template)
  :config
  (dap-auto-configure-mode)

  (setq dap-auto-configure-features
        '(sessions locals breakpoints expressions repl controls tooltip))

  (general-define-key
   :states 'normal
   :keymaps 'java-ts-mode-map
   :prefix "SPC d"
   "" '(:ignore t :which-key "debug")
   "b" '(dap-breakpoint-toggle :which-key "toggle breakpoint")
   "B" '(dap-breakpoint-condition :which-key "conditional breakpoint")
   "d" '(dap-debug :which-key "debug")
   "l" '(dap-debug-last :which-key "debug last")
   "r" '(dap-debug-recent :which-key "debug recent")
   "e" '(dap-eval :which-key "eval")
   "E" '(dap-eval-region :which-key "eval region")
   "s" '(dap-step-in :which-key "step in")
   "n" '(dap-next :which-key "next")
   "o" '(dap-step-out :which-key "step out")
   "c" '(dap-continue :which-key "continue")
   "q" '(dap-disconnect :which-key "disconnect")
   "u" '(dap-ui-sessions :which-key "ui sessions")
   "h" '(dap-hydra :which-key "hydra")))

;; DAP Java support
(use-package dap-java
  :straight nil
  :after (dap-mode lsp-java))

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

;; ============================================================================
;; Spring Boot support (optional)
;; ============================================================================

;; Uncomment if working with Spring Boot projects
;; (use-package lsp-java-boot
;;   :straight nil
;;   :after lsp-java
;;   :config
;;   (require 'lsp-java-boot))

;;; java.el ends here
