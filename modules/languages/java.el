;;; java.el --- Java language configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Java development setup with LSP (Eclipse JDT.LS), DAP debugger, and build tools.
;; This file is loaded on-demand when opening Java files.

;;; Code:

;; ============================================================================
;; Java Mode (built-in)
;; ============================================================================

;; Configure built-in java-mode before LSP
;;
;; NOTE: real .java buffers already run `java-ts-mode', not `java-mode' --
;; treesit-auto's `set-auto-mode-0' advice installs a buffer-local
;; major-mode-remap-alist entry at mode-resolution time that wins regardless
;; of auto-mode-alist content or ordering. This :mode association is
;; therefore inert for real buffers; it is left as-is because touching mode
;; resolution is out of scope for this fix. Deliberately no :config here:
;; java-mode is provided by cc-mode.el (`(cc-provide 'cc-mode)`), never
;; `(provide 'java-mode)`, so use-package's `eval-after-load 'java-mode'
;; never fires -- confirmed empirically that a :config block on this form
;; never runs, at any time.
(use-package java-mode
  :straight nil
  :mode "\\.java\\'")

;; The mode real .java buffers actually run (see note above). java-ts-mode.el
;; genuinely (provide 'java-ts-mode), so with-eval-after-load reliably fires
;; here -- the same pattern already used by rust.el/clojure.el/go.el/
;; javascript.el for their respective *-ts-mode hooks.
(with-eval-after-load 'java-ts-mode
  ;; Enable LSP for Java
  (add-hook 'java-ts-mode-hook #'lsp-deferred)

  ;; java-ts-mode has its own indent-offset defcustom; it does not consult
  ;; cc-mode's c-basic-offset.  This variable is NOT auto-buffer-local, so a
  ;; plain setq here does reach every java-ts-mode buffer.
  (setq java-ts-mode-indent-offset 4)

  ;; tab-width and indent-tabs-mode must NOT be set here.  Both are
  ;; auto-buffer-local (`local-variable-if-set-p' is t for each), so a plain
  ;; setq inside this load-once block binds them only in whatever buffer
  ;; happened to be current when java-ts-mode.el was loaded -- never in a real
  ;; .java buffer.  Verified by mutation: with `tab-width 7' set in the block
  ;; above, a freshly visited .java buffer still read tab-width=4, inherited
  ;; from modules/core.el's setq-default, and `local-variable-p' was nil.
  ;; That made them dead settings of exactly the kind this phase exists to
  ;; remove.  Setting them from the mode hook binds them per buffer.
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
  ;; Install jdtls server if not present
  (setq lsp-java-server-install-dir (expand-file-name "lsp/jdtls" user-emacs-directory)
        lsp-java-workspace-dir (expand-file-name "lsp/java-workspace" user-emacs-directory))

  ;; Java-specific LSP settings
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

  ;; Enable semantic tokens for better syntax highlighting
  (setq lsp-semantic-tokens-enable t)

  ;; Java-specific keybindings with local leader
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

   ;; Run/Debug/Test. Deliberately NOT on "r"/"d"/"t" -- lsp-mode-map
   ;; (modules/programming.el) defines terminal "SPC c r"/"SPC c d"/
   ;; "SPC c t" bindings (lsp-rename / lsp-find-definition /
   ;; lsp-find-type-definition) on those same keys, and lsp-mode-map is a
   ;; minor-mode keymap that wins over this major-mode keymap once
   ;; lsp-mode is genuinely active in the buffer -- the same collision
   ;; class already fixed for "h" below (capital "H"), confirmed
   ;; empirically via evil-normalize-keymaps: with lsp-mode forced on,
   ;; "SPC c d"/"SPC c r"/"SPC c t" resolved to the lsp-mode-map commands
   ;; and "SPC c d d"/"SPC c r r"/"SPC c t t" were unreachable.
   ;; The obvious next choice, capital "R"/"D"/"T", does NOT work either
   ;; for two of the three: lsp-mode-map also binds capital "SPC c R" to
   ;; lsp-find-references and capital "SPC c D" to lsp-find-declaration
   ;; (programming.el), so only capital "T" was actually free -- confirmed
   ;; empirically the same way ("SPC c R"/"SPC c D" resolved to the
   ;; lsp-mode-map commands, "SPC c R r"/"SPC c D d" were unreachable).
   ;; "X" (eXecute) and "K" are both genuinely unclaimed under "SPC c" by
   ;; either lsp-mode-map or this file's own bindings.
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

   ;; Type hierarchy. Deliberately NOT on "h" -- lsp-ui's generic "SPC c h"
   ;; hover binding (modules/programming.el) lives on lsp-mode-map, a
   ;; minor-mode keymap, which wins over this major-mode keymap once
   ;; lsp-mode is genuinely active in the buffer (confirmed empirically:
   ;; forcing lsp-mode on and calling evil-normalize-keymaps makes
   ;; "SPC c h" resolve to lsp-ui-doc-show, making "SPC c h t" unreachable).
   ;; Capital "H" avoids the collision.
   "H" '(:ignore t :which-key "hierarchy")
   "Ht" '(lsp-java-type-hierarchy :which-key "type hierarchy")))

;; ============================================================================
;; DAP Mode - Debug Adapter Protocol for Java
;; ============================================================================

(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template)
  :config
  ;; Enable DAP features
  (dap-auto-configure-mode)

  ;; DAP UI settings
  (setq dap-auto-configure-features
        '(sessions locals breakpoints expressions repl controls tooltip))

  ;; DAP keybindings
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
  ;; :after alone (without :demand) still defers to autoload-on-command
  ;; semantics -- use-package only wraps the :commands autoloads in
  ;; `eval-after-load', it does not itself `require' the package once
  ;; java-ts-mode loads. Confirmed empirically: after visiting a real
  ;; .java file, (featurep 'mvn) was nil and (key-binding (kbd ", m c"))
  ;; was nil, because nothing ever calls mvn-clean/mvn-compile/mvn-test to
  ;; trigger the autoload, so mvn.el's :config (which defines the ", m"
  ;; keybindings) never ran. `:demand t' forces an actual `(require 'mvn)'
  ;; as soon as java-ts-mode loads, mirroring how dap-mode/lsp-java above
  ;; load unconditionally via `:after lsp-mode'.
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
