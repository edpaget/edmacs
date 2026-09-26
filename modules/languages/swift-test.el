;;; swift-test.el --- Tests for languages/swift.el -*- lexical-binding: t -*-

;;; Commentary:
;; Covers the `.swift' autoload, the sourcekit-lsp entry swift.el adds to
;; eglot (which has none of its own), the swift-mode hooks, and the
;; swift-format apheleia formatter.
;;
;; Run with this file on the command line, from any `default-directory':
;;
;;   emacs -Q --batch -l ert -l modules/languages/swift-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; `(provide 'swift-mode)' fires the deferred `use-package' `:config' body
;; without swift-mode on `load-path'.  No test here `cl-letf's a C subr.

;;; Code:

(require 'ert)
(require 'eglot)

(defvar swift-test--module
  (expand-file-name
   "swift.el"
   (file-name-directory
    (or load-file-name buffer-file-name
        (expand-file-name "modules/languages/swift-test.el" default-directory))))
  "This checkout's `modules/languages/swift.el'.")

(load swift-test--module nil t)

(defvar swift-mode-hook nil)
(provide 'swift-mode)

(defvar apheleia-formatters nil)
(defvar apheleia-mode-alist nil)
(provide 'apheleia)

(ert-deftest swift-test-mode-autoloads-on-swift-files ()
  (should (eq (assoc-default "Foo.swift" auto-mode-alist #'string-match)
              'swift-mode))
  (should (fboundp 'swift-mode)))

(ert-deftest swift-test-hooks ()
  (should (memq #'eglot-ensure swift-mode-hook))
  (should (memq #'smartparens-mode swift-mode-hook)))

(ert-deftest swift-test-eglot-contacts-sourcekit-lsp ()
  (should (equal (alist-get 'swift-mode eglot-server-programs)
                 '("sourcekit-lsp"))))

(ert-deftest swift-test-apheleia-uses-swift-format ()
  (should (eq (alist-get 'swift-mode apheleia-mode-alist) 'swift-format))
  (should (equal (car (alist-get 'swift-format apheleia-formatters))
                 "swift-format")))

(provide 'swift-test)
;;; swift-test.el ends here
