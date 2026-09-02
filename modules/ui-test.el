;;; ui-test.el --- Tests for ui.el -*- lexical-binding: t -*-

;;; Commentary:
;; ui.el's window-management code (the windmove `:around' advice, the main
;; window, the stack, and rotate.el's parameter-preserving advice) moved to
;; modules/windows.el in edmacs-window-management/phase-1-layout-model,
;; along with the three tests that covered it (now
;; edmacs-windows-test-windmove-* in modules/windows-test.el). ui.el retains
;; no window-management code of its own to unit-test, so this file carries
;; none -- 0 tests is this suite's correct, passing state.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/ui.el -l modules/ui-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;;; ui-test.el ends here
