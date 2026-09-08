;;; javascript-test.el --- Tests for languages/javascript.el -*- lexical-binding: t -*-

;;; Commentary:
;; Covers the TypeScript language-server contact resolver and the workspace
;; configuration `javascript.el' merges into the shared eglot plist.
;;
;; Run with this file on the command line, from any `default-directory' --
;; the module path resolves against this file's own directory via
;; `load-file-name', not against cwd:
;;
;;   emacs -Q --batch -l ert -l modules/languages/javascript-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; `modules/languages/javascript.el' loads to EOF under bare `-Q': its three
;; `use-package' blocks carry `:straight nil', an unrecognized keyword with
;; no straight.el loaded, so each prints a benign "Error (use-package):
;; Unrecognized keyword: :straight" to stderr and aborts that block's
;; expansion.  Their `:config' bodies -- the mode hooks -- therefore never
;; run here; hook coverage lives in `scripts/eglot-languages-check.sh'.
;; Everything asserted below is defined at top level.
;;
;; The fake compilers are real scripts on disk rather than `cl-letf' on a C
;; subr, so the `native-comp-enable-subr-trampolines' guard documented in
;; .claude/CLAUDE.md is deliberately not needed here.

;;; Code:

(require 'ert)
(require 'eglot)

;; Bound by `javascript-test--with-clean-cache'; declared special here so a
;; byte-compile of this file alone binds it dynamically, not lexically.
(defvar edmacs-js--typescript-server-cache)

(defvar javascript-test--module
  (expand-file-name
   "javascript.el"
   (file-name-directory
    (or load-file-name buffer-file-name
        (expand-file-name "modules/languages/javascript-test.el"
                          default-directory))))
  "This checkout's `modules/languages/javascript.el'.")

(unless (fboundp 'general-define-key)
  (defalias 'general-define-key (lambda (&rest _) nil)))

(load javascript-test--module nil t)

(defun javascript-test--fake-tsc (dir body)
  "Write an executable `tsc' into DIR whose script text is BODY.
Return DIR, so it can be consed onto `exec-path'."
  (make-directory dir t)
  (let ((script (expand-file-name "tsc" dir)))
    (with-temp-file script
      (insert "#!/bin/sh\n" body "\n"))
    (set-file-modes script #o755))
  dir)

(defmacro javascript-test--with-scratch (var &rest body)
  "Bind VAR to a fresh temp directory, run BODY, then delete the directory."
  (declare (indent 1))
  `(let ((,var (make-temp-file "edmacs-js-test" t)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defmacro javascript-test--with-clean-cache (&rest body)
  "Run BODY against an empty tsc probe cache."
  (declare (indent 0))
  `(let ((edmacs-js--typescript-server-cache (make-hash-table :test #'equal)))
     ,@body))


;;; The contact resolver

(ert-deftest javascript-test-typescript-7-serves-lsp ()
  "A tsc reporting version 7 is itself the server."
  (javascript-test--with-scratch dir
    (javascript-test--with-clean-cache
      (let ((exec-path (cons (javascript-test--fake-tsc
                              dir "echo 'Version 7.0.2'")
                             exec-path)))
        (let ((contact (edmacs-js--typescript-server nil nil)))
          (should (string-suffix-p "tsc" (car contact)))
          (should (equal (cdr contact) '("--lsp" "--stdio"))))))))

(ert-deftest javascript-test-typescript-5-falls-back ()
  "A tsc reporting version 5 has no `--lsp'; the separate server handles it."
  (javascript-test--with-scratch dir
    (javascript-test--with-clean-cache
      (let ((exec-path (cons (javascript-test--fake-tsc
                              dir "echo 'Version 5.9.3'")
                             exec-path)))
        (should (equal (edmacs-js--typescript-server nil nil)
                       '("typescript-language-server" "--stdio")))))))

(ert-deftest javascript-test-no-tsc-falls-back ()
  "With no tsc resolvable at all, the fallback server is chosen."
  (javascript-test--with-clean-cache
    (let ((exec-path (list (make-temp-file "edmacs-js-empty" t))))
      (should (equal (edmacs-js--typescript-server nil nil)
                     '("typescript-language-server" "--stdio"))))))

(ert-deftest javascript-test-failing-tsc-falls-back ()
  "A tsc that cannot answer `--version' must not be trusted with `--lsp'."
  (javascript-test--with-scratch dir
    (javascript-test--with-clean-cache
      (let ((exec-path (cons (javascript-test--fake-tsc dir "exit 1")
                             exec-path)))
        (should (equal (edmacs-js--typescript-server nil nil)
                       '("typescript-language-server" "--stdio")))))))

(ert-deftest javascript-test-follows-buffer-local-exec-path ()
  "The decision follows the buffer-local `exec-path' mise sets per project."
  (javascript-test--with-scratch seven
    (javascript-test--with-scratch five
      (javascript-test--with-clean-cache
        (javascript-test--fake-tsc seven "echo 'Version 7.0.2'")
        (javascript-test--fake-tsc five "echo 'Version 5.9.3'")
        (with-temp-buffer
          (setq-local exec-path (cons seven exec-path))
          (should (equal (cdr (edmacs-js--typescript-server nil nil))
                         '("--lsp" "--stdio"))))
        (with-temp-buffer
          (setq-local exec-path (cons five exec-path))
          (should (equal (edmacs-js--typescript-server nil nil)
                         '("typescript-language-server" "--stdio"))))))))

(ert-deftest javascript-test-probe-is-cached-per-path ()
  "Each resolved tsc is probed once, however many buffers connect."
  (javascript-test--with-scratch dir
    (javascript-test--with-clean-cache
      (let* ((tally (expand-file-name "tally" dir))
             (exec-path (cons (javascript-test--fake-tsc
                               dir (format "echo x >> %s; echo 'Version 7.0.2'"
                                           (shell-quote-argument tally)))
                              exec-path)))
        (dotimes (_ 3) (edmacs-js--typescript-server nil nil))
        (should (equal 1 (with-temp-buffer
                           (insert-file-contents tally)
                           (count-lines (point-min) (point-max)))))))))

(ert-deftest javascript-test-probe-reruns-after-tsc-changes-on-disk ()
  "A tsc upgraded in place -- same path, new mtime -- is probed again."
  (javascript-test--with-scratch dir
    (javascript-test--with-clean-cache
      (let ((exec-path (cons (javascript-test--fake-tsc
                              dir "echo 'Version 5.9.3'")
                             exec-path)))
        (should (equal (edmacs-js--typescript-server nil nil)
                       '("typescript-language-server" "--stdio")))
        (javascript-test--fake-tsc dir "echo 'Version 7.0.2'")
        ;; Rewriting within the same second would leave the mtime unchanged.
        (set-file-times (expand-file-name "tsc" dir)
                        (time-add (current-time) 10))
        (should (equal (cdr (edmacs-js--typescript-server nil nil))
                       '("--lsp" "--stdio")))))))

(ert-deftest javascript-test-contact-arity-is-eglot-compatible ()
  "eglot funcalls a contact function with one or two arguments, never zero."
  (should (>= (cdr (func-arity #'edmacs-js--typescript-server)) 1))
  (should (edmacs-js--typescript-server nil nil)))

(ert-deftest javascript-test-entry-precedes-eglots-own ()
  "Our entry must win the lookup; eglot's own first alternative is a typo."
  (dolist (mode '(typescript-ts-mode tsx-ts-mode js-ts-mode))
    (should (eq (cdr (eglot--lookup-mode mode))
                'edmacs-js--typescript-server))))


;;; Workspace configuration

(ert-deftest javascript-test-typescript-workspace-configuration ()
  (should (equal (plist-get (default-value 'eglot-workspace-configuration)
                            :typescript)
                 '(:suggest (:autoImports t)
                   :preferences (:importModuleSpecifier "relative"
                                 :quoteStyle "single")))))

(ert-deftest javascript-test-javascript-workspace-configuration ()
  (should (equal (plist-get (default-value 'eglot-workspace-configuration)
                            :javascript)
                 '(:suggest (:autoImports t)))))

(ert-deftest javascript-test-no-eslint-settings-survive ()
  "The dropped eslint integration leaves nothing behind in the module."
  (should-not (with-temp-buffer
                (insert-file-contents javascript-test--module)
                (re-search-forward "lsp-eslint" nil t))))

(provide 'javascript-test)
;;; javascript-test.el ends here
