;;; ui-test.el --- Tests for ui.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function/window-parameter coverage only -- no display, no theme.
;; `ui.el' loads cleanly under `-Q --batch' the same way `claude-term.el'
;; does: its `use-package' forms for unavailable packages (nano-modeline,
;; nerd-icons, rotate) fail internally and print a benign
;; "Error (use-package): Cannot load ..." notice without aborting the
;; load or leaving any half-defined state this file's tests depend on.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/ui.el -l modules/ui-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'windmove)

;; ============================================================================
;; windmove reachability: `no-other-window' + `edmacs-windmove-reachable'
;; ============================================================================
;; Regression test for the windmove-allow-all-windows-leaks-to-sidebar fix:
;; `windmove-allow-all-windows' is a single global boolean forwarded
;; verbatim as `window-in-direction''s IGNORE argument, so turning it on
;; (as an earlier pass of ui.el did) makes EVERY `no-other-window' window
;; reachable by a directional windmove -- not just the claude-term panes
;; it was meant for, but also a later side-window module (sidebar.el)
;; that never opted in. The fix replaces the blanket flag with an
;; `:around' advice on `windmove-find-other-window' plus a per-window
;; `edmacs-windmove-reachable' parameter that only claude-term.el sets;
;; this test exercises that advice directly against a real three-window
;; layout, without needing to load claude-term.el or sidebar.el.

(defmacro edmacs-ui-test--with-side-windows (&rest body)
  "Run BODY with `no-other-window' windows on both sides of MAIN.
LEFT carries only `no-other-window' (models sidebar.el's window: never
windmove-reachable). RIGHT carries `no-other-window' AND
`edmacs-windmove-reachable' (models a claude-term.el pane: reachable by
a single windmove direction key despite `no-other-window' blocking
`other-window'/`C-x 1'). MAIN, LEFT, and RIGHT are bound for BODY."
  (declare (indent 0))
  `(save-window-excursion
     (delete-other-windows)
     (let* ((main (selected-window))
            (left (split-window main nil 'left))
            (right (split-window main nil 'right)))
       (set-window-parameter left 'no-other-window t)
       (set-window-parameter right 'no-other-window t)
       (set-window-parameter right 'edmacs-windmove-reachable t)
       (select-window main)
       ,@body)))

(ert-deftest edmacs-ui-test-windmove-allow-all-windows-stays-nil ()
  "The blanket flag is gone -- reachability is per-window now, not global."
  (should (null windmove-allow-all-windows)))

(ert-deftest edmacs-ui-test-windmove-does-not-reach-unmarked-no-other-window ()
  "A `no-other-window' window without the opt-in parameter (sidebar.el's
shape) is never windmove-reachable, even though it is the nearest
window in that direction."
  (edmacs-ui-test--with-side-windows
    (should-not (windmove-find-other-window 'left))))

(ert-deftest edmacs-ui-test-windmove-reaches-marked-no-other-window ()
  "A `no-other-window' window that opts in via `edmacs-windmove-reachable'
(claude-term.el's shape) is still windmove-reachable, preserving the
original claude-term intent this mechanism replaced
`windmove-allow-all-windows' to serve."
  (edmacs-ui-test--with-side-windows
    (should (eq (windmove-find-other-window 'right) right))))

;;; ui-test.el ends here
