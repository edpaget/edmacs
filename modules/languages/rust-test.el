;;; rust-test.el --- Tests for languages/rust.el -*- lexical-binding: t -*-

;;; Commentary:
;; Covers the rust-analyzer workspace configuration `rust.el' merges into
;; the shared eglot plist, and the two rustic settings that pick eglot and
;; keep rustic's own initializationOptions in step with it.
;;
;; Run with this file on the command line, from any `default-directory' --
;; the module path resolves against this file's own directory via
;; `load-file-name', not against cwd:
;;
;;   emacs -Q --batch -l ert -l modules/languages/rust-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; `modules/languages/rust.el' loads to EOF under bare `-Q': its two
;; `use-package' blocks defer, so their `:config' bodies do not run on load.
;; `(provide 'rustic)' below fires the deferred body -- `provide' runs the
;; feature's `after-load-alist' entries, which is exactly what `use-package'
;; registers `:config' as -- without needing rustic or any of its
;; dependencies on `load-path'.
;;
;; No test here `cl-letf's a C subr, so the
;; `native-comp-enable-subr-trampolines' guard documented in
;; .claude/CLAUDE.md is deliberately not needed.

;;; Code:

(require 'ert)
(require 'eglot)

(defvar rust-test--module
  (expand-file-name
   "rust.el"
   (file-name-directory
    (or load-file-name buffer-file-name
        (expand-file-name "modules/languages/rust-test.el" default-directory))))
  "This checkout's `modules/languages/rust.el'.")

(unless (fboundp 'general-define-key)
  (defalias 'general-define-key (lambda (&rest _) nil)))

(load rust-test--module nil t)

;; Fire the deferred `use-package rustic' `:config' body.
(defvar rustic-mode-map (make-sparse-keymap))
(provide 'rustic)

(defun rust-test--section ()
  (plist-get (default-value 'eglot-workspace-configuration) :rust-analyzer))


(ert-deftest rust-test-rustic-drives-eglot ()
  "`rustic-setup-lsp' dispatches on this symbol and calls `eglot-ensure'."
  (should (eq rustic-lsp-client 'eglot)))

(ert-deftest rust-test-rustic-check-command-agrees ()
  "rustic sends check.command as an initializationOption; it must not differ
from the check.command in the workspace configuration."
  (should (equal rustic-lsp-check-command "clippy"))
  (should (equal (plist-get (plist-get (rust-test--section) :check) :command)
                 rustic-lsp-check-command)))

(ert-deftest rust-test-check-command-is-clippy ()
  (should (equal (plist-get (rust-test--section) :check)
                 '(:command "clippy"))))

(ert-deftest rust-test-inlay-hints ()
  (should (equal (plist-get (rust-test--section) :inlayHints)
                 '(:lifetimeElisionHints (:enable "skip_trivial")
                   :closureReturnTypeHints (:enable "always")
                   :parameterHints (:enable :json-false)))))

(ert-deftest rust-test-parameter-hints-are-json-false ()
  "rust-analyzer defaults parameterHints.enable to true, so switching it off
has to serialize as false; nil would go over the wire as null."
  (should (eq (plist-get (plist-get (plist-get (rust-test--section) :inlayHints)
                                    :parameterHints)
                         :enable)
              :json-false)))

(ert-deftest rust-test-server-defaults-are-not-echoed-back ()
  "Settings that merely restate a rust-analyzer default are not sent."
  (let ((hints (plist-get (rust-test--section) :inlayHints)))
    (should-not (plist-member hints :chainingHints))
    (should-not (plist-member hints :expressionAdjustmentHints))
    (should-not (plist-member (plist-get hints :lifetimeElisionHints)
                              :useParameterNames))))

(ert-deftest rust-test-no-dead-rust-ts-mode-block ()
  "The `rust-ts-mode' block never fired -- rustic claims `.rs' outright."
  (should-not (with-temp-buffer
                (insert-file-contents rust-test--module)
                (re-search-forward "add-hook 'rust-ts-mode-hook" nil t))))

(provide 'rust-test)
;;; rust-test.el ends here
