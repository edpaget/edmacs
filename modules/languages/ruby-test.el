;;; ruby-test.el --- Tests for languages/ruby.el -*- lexical-binding: t -*-

;;; Commentary:
;; Covers the eglot gate on an installed Ruby language server, the hooks on
;; both ruby-ts-mode and its ruby-mode fallback, and the apheleia override
;; away from prettier-ruby.
;;
;; Run with this file on the command line, from any `default-directory':
;;
;;   emacs -Q --batch -l ert -l modules/languages/ruby-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; No test here `cl-letf's a C subr, so the
;; `native-comp-enable-subr-trampolines' guard is not needed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar ruby-test--module
  (expand-file-name
   "ruby.el"
   (file-name-directory
    (or load-file-name buffer-file-name
        (expand-file-name "modules/languages/ruby-test.el" default-directory))))
  "This checkout's `modules/languages/ruby.el'.")

(load ruby-test--module nil t)

;; Seed apheleia's own defaults, then fire ruby.el's deferred override.
(defvar apheleia-mode-alist '((ruby-mode . prettier-ruby)
                              (ruby-ts-mode . prettier-ruby)))
(provide 'apheleia)

(require 'ruby-mode)
(require 'ruby-ts-mode)

(ert-deftest ruby-test-hooks-on-both-modes ()
  (should (memq #'edmacs-ruby-maybe-eglot ruby-ts-mode-hook))
  (should (memq #'edmacs-ruby-maybe-eglot ruby-mode-hook))
  (should (memq #'smartparens-mode ruby-ts-mode-hook)))

(ert-deftest ruby-test-eglot-starts-with-a-server ()
  (let (started)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &rest _) (and (equal name "ruby-lsp") "/bin/ruby-lsp")))
              ((symbol-function 'eglot-ensure) (lambda () (setq started t))))
      (edmacs-ruby-maybe-eglot))
    (should started)))

(ert-deftest ruby-test-eglot-skipped-without-a-server ()
  (let (started)
    (cl-letf (((symbol-function 'executable-find) #'ignore)
              ((symbol-function 'eglot-ensure) (lambda () (setq started t))))
      (edmacs-ruby-maybe-eglot))
    (should-not started)))

(ert-deftest ruby-test-servers-match-eglot ()
  "The gate must look for the executables eglot's own ruby entry contacts."
  (require 'eglot)
  (let* ((entry (cdr (cl-find-if (lambda (e) (and (listp (car e))
                                                  (memq 'ruby-ts-mode (car e))))
                                 eglot-server-programs)))
         (contact (format "%S" entry)))
    (dolist (server edmacs-ruby-language-servers)
      (should (string-search (format "%S" server) contact)))))

(ert-deftest ruby-test-apheleia-uses-rubocop ()
  (should (eq (alist-get 'ruby-mode apheleia-mode-alist) 'rubocop))
  (should (eq (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop)))

(provide 'ruby-test)
;;; ruby-test.el ends here
