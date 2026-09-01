;;; claude-term-test.el --- Tests for claude-term.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage only -- no live ghostel process is spawned here.
;; Run with:
;;   emacs -Q --batch -l ert -l modules/claude-term.el -l modules/claude-term-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; (Loading claude-term.el under `-Q' prints a benign "Unrecognized
;; keyword: :straight" notice from the `use-package ghostel' form, since
;; straight.el is not bootstrapped in this bare batch harness; that error
;; is caught internally by use-package and does not abort the load or
;; affect any test below.)

;;; Code:

(require 'ert)

(ert-deftest claude-term-test-leaf ()
  (should (equal (claude-term--leaf "/foo/bar-baz/") "bar-baz"))
  (should (equal (claude-term--leaf "/foo/bar-baz") "bar-baz")))

(ert-deftest claude-term-test-buffer-name-no-instance ()
  (should (equal (claude-term-buffer-name "/foo/bar-baz/") "*claude-term:bar-baz*")))

(ert-deftest claude-term-test-buffer-name-with-instance ()
  (should (equal (claude-term-buffer-name "/foo/bar-baz/" "2") "*claude-term:bar-baz:2*")))

(ert-deftest claude-term-test-parse-buffer-name-round-trip ()
  (should (equal (claude-term--parse-buffer-name "*claude-term:bar-baz:2*") '("bar-baz" . "2")))
  (should (equal (claude-term--parse-buffer-name "*claude-term:bar-baz*") '("bar-baz" . nil)))
  (should-not (claude-term--parse-buffer-name "*vterm*")))

(ert-deftest claude-term-test-spawn-args-appends-call-args ()
  (let ((claude-term-extra-args '("--foo")))
    (should (equal (claude-term--spawn-args '("--bar")) '("--foo" "--bar")))))

(ert-deftest claude-term-test-spawn-args-nil-extra-args ()
  (let ((claude-term-extra-args nil))
    (should (equal (claude-term--spawn-args nil) nil))
    (should (equal (claude-term--spawn-args '("--bar")) '("--bar")))))

(ert-deftest claude-term-test-spawn-args-nil-call-args ()
  (let ((claude-term-extra-args '("--foo")))
    (should (equal (claude-term--spawn-args nil) '("--foo")))))

;;; claude-term-test.el ends here
