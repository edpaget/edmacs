;;; windows-test.el --- Tests for windows.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function/window-parameter coverage -- no display, no theme, no
;; live subprocess.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/windows.el -l modules/windows-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The load-order test near the bottom of this file (AC4) loads
;; `modules/sidebar.el' itself, fixing up `load-path' against the straight
;; build tree first -- it is deliberately NOT passed on the invocation line
;; above, matching how `modules/sidebar-test.el' loads `modules/sidebar.el'.
;; `modules/windows.el' IS on the invocation line above (it must be, since
;; every other test in this file exercises it directly), so that test only
;; has sidebar.el left to load internally. See that test's own comments for
;; the straight-bootstrap skip condition.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'tab-bar)

;; ============================================================================
;; AC1 -- edmacs-window-promote swaps buffers for the three layouts
;; ============================================================================

(ert-deftest edmacs-windows-test-promote-swaps-right-side-window ()
  (save-window-excursion
    (delete-other-windows)
    (let ((main-buf (generate-new-buffer "ewt-main-right"))
          (right-buf (generate-new-buffer "ewt-right")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) main-buf)
            (let* ((main (selected-window))
                   (right (split-window main nil 'right)))
              (set-window-buffer right right-buf)
              (set-window-parameter right 'window-side 'right)
              (set-window-parameter right 'no-other-window t)
              (select-window main)
              (should (eq (edmacs-main-window) main))
              (edmacs-window-promote right)
              (should (eq (window-buffer main) right-buf))
              (should (eq (window-buffer right) main-buf))))
        (kill-buffer main-buf)
        (kill-buffer right-buf)))))

(ert-deftest edmacs-windows-test-promote-swaps-center-split ()
  (save-window-excursion
    (delete-other-windows)
    (let ((main-buf (generate-new-buffer "ewt-main-center"))
          (other-buf (generate-new-buffer "ewt-other-center")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) main-buf)
            (let* ((main (selected-window))
                   (other (split-window main nil 'below)))
              (set-window-buffer other other-buf)
              (select-window main)
              (should (eq (edmacs-main-window) main))
              (edmacs-window-promote other)
              (should (eq (window-buffer main) other-buf))
              (should (eq (window-buffer other) main-buf))))
        (kill-buffer main-buf)
        (kill-buffer other-buf)))))

(ert-deftest edmacs-windows-test-promote-single-window-is-noop ()
  (save-window-excursion
    (delete-other-windows)
    (let* ((main (selected-window))
           (before (window-buffer (edmacs-main-window))))
      (edmacs-window-promote)
      (should (eq (window-buffer main) before)))))

;; ============================================================================
;; AC2 -- edmacs-main-window is identity-based, survives resize/reorder,
;; and falls back to top-left designation
;; ============================================================================

(ert-deftest edmacs-windows-test-main-window-survives-resize-and-reorder ()
  (save-window-excursion
    (delete-other-windows)
    (let* ((w1 (selected-window))
           (w2 (split-window w1 nil 'right))
           (w3 (split-window w1 nil 'below)))
      (edmacs-window-set-main w2)
      (should (eq (edmacs-main-window) w2))
      ;; Changing every window's dimensions must not disturb a lookup keyed
      ;; on the parameter rather than position.
      (window-resize w1 3 nil t)
      (window-resize w3 -2 nil t)
      (should (eq (edmacs-main-window) w2))
      ;; `window-swap-states' swaps parameters (including `edmacs-main')
      ;; between window OBJECTS while both keep their tree position, so the
      ;; parameter relocates onto whichever object was at top-left.
      (let ((topleft (edmacs--topleft-window)))
        (unless (eq topleft w2)
          (window-swap-states w2 topleft))
        (let ((carrier (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                                  (window-list nil 'no-minibuf))))
          (should carrier)
          (should (eq (edmacs-main-window) carrier)))))))

(ert-deftest edmacs-windows-test-main-window-falls-back-to-topleft ()
  (save-window-excursion
    (delete-other-windows)
    (split-window (selected-window) nil 'right)
    (let ((topleft (edmacs--topleft-window)))
      (should-not (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                             (window-list nil 'no-minibuf)))
      (should (eq (edmacs-main-window) topleft))
      ;; The first call's designation persisted the parameter, so a second
      ;; call finds it directly rather than re-deriving it.
      (should (eq (edmacs-main-window) topleft)))))

;; ============================================================================
;; AC3 -- edmacs-main in window-persistent-parameters; survives
;; window-state-get/put and a tab-bar new-tab/switch-back round trip
;; ============================================================================

(ert-deftest edmacs-windows-test-main-in-persistent-parameters ()
  (should (assq 'edmacs-main window-persistent-parameters)))

(ert-deftest edmacs-windows-test-main-survives-state-get-put ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((state (window-state-get (frame-root-window) t)))
      (delete-other-windows)
      (window-state-put state (frame-root-window))
      ;; `window-state-put' creates new window objects, so identity does not
      ;; survive the round trip -- assert via parameter presence instead.
      (should (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                         (window-list nil 'no-minibuf))))))

(ert-deftest edmacs-windows-test-main-survives-tab-new-and-switch-back ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((tabs-before (length (tab-bar-tabs))))
      (unwind-protect
          (progn
            ;; `tab-bar-new-tab' runs `delete-other-windows' on a fresh
            ;; window configuration -- the new tab inherits no main claim.
            (tab-bar-new-tab)
            (should-not (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                                   (window-list nil 'no-minibuf)))
            (tab-bar-switch-to-prev-tab)
            (should (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                               (window-list nil 'no-minibuf))))
        (while (> (length (tab-bar-tabs)) tabs-before)
          (tab-bar-close-tab))))))

;; ============================================================================
;; windmove reachability: `no-other-window' + `edmacs-windmove-reachable'
;; ============================================================================
;; Relocated verbatim (renamed prefix) from ui-test.el: the advice these
;; tests exercise moved to windows.el in this phase, and ui.el no longer
;; carries it. See ui-test.el's own Commentary.

(defmacro edmacs-windows-test--with-side-windows (&rest body)
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

(ert-deftest edmacs-windows-test-windmove-allow-all-windows-stays-nil ()
  "The blanket flag is gone -- reachability is per-window now, not global."
  (should (null windmove-allow-all-windows)))

(ert-deftest edmacs-windows-test-windmove-does-not-reach-unmarked-no-other-window ()
  "A `no-other-window' window without the opt-in parameter (sidebar.el's
shape) is never windmove-reachable, even though it is the nearest
window in that direction."
  (edmacs-windows-test--with-side-windows
    (should-not (windmove-find-other-window 'left))))

(ert-deftest edmacs-windows-test-windmove-reaches-marked-no-other-window ()
  "A `no-other-window' window that opts in via `edmacs-windmove-reachable'
(claude-term.el's shape) is still windmove-reachable, preserving the
original claude-term intent this mechanism replaced
`windmove-allow-all-windows' to serve."
  (edmacs-windows-test--with-side-windows
    (should (eq (windmove-find-other-window 'right) right))))

;; ============================================================================
;; AC4 -- load order: windows.el then sidebar.el leaves window-sides-slots
;; at (1 nil nil nil) -- sidebar's LEFT cap intact, this module's RIGHT nil
;; ============================================================================
;; Duplicated (not required) from sidebar-test.el, to keep each *-test.el
;; file's own Commentary invocation self-contained -- see that file's
;; Commentary for why `magit-section' needs a `load-path' fixup under bare
;; `-Q' and why a broken/never-bootstrapped straight build must skip
;; cleanly rather than error the whole suite out.

(defun edmacs-windows-test--locate-straight-build-root ()
  "Return this checkout's `straight/build' directory, or nil.
Tries this checkout's own `straight/build' first, then falls back to the
sibling main `edmacs' checkout's -- a roadmap worktree lives under
`<parent>/edmacs__worktrees/<name>', sibling to the main
`<parent>/edmacs' checkout, and straight's build cache is per-checkout."
  (or
   (let ((here (expand-file-name "straight/build" default-directory)))
     (and (file-directory-p here) here))
   (let* ((root (directory-file-name (expand-file-name default-directory)))
          (worktrees-dir (directory-file-name (file-name-directory root))))
     (when (string-suffix-p "__worktrees" worktrees-dir)
       (let* ((projects-dir (file-name-directory worktrees-dir))
              (repo-name (string-remove-suffix
                          "__worktrees" (file-name-nondirectory worktrees-dir)))
              (main-build (expand-file-name
                           (concat repo-name "/straight/build") projects-dir)))
         (and (file-directory-p main-build) main-build))))))

(defun edmacs-windows-test--add-magit-section-deps (build-root)
  "Add `magit-section' and its transitive deps under BUILD-ROOT to `load-path'.
cl-lib, eieio, subr-x, format-spec, and cursor-sensor ship with Emacs core
and need no straight resolution; only these do."
  (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section"))
    (let ((dir (expand-file-name dep build-root)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(defvar edmacs-windows-test--build-root
  (edmacs-windows-test--locate-straight-build-root)
  "This checkout's (or its sibling main checkout's) `straight/build' root.")

(if (null edmacs-windows-test--build-root)

    (ert-deftest edmacs-windows-test-window-sides-slots-load-order-unavailable ()
      (ert-skip "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this test"))

  (progn

    (edmacs-windows-test--add-magit-section-deps edmacs-windows-test--build-root)
    (load (expand-file-name "modules/sidebar.el" default-directory) nil t)

    (ert-deftest edmacs-windows-test-window-sides-slots-load-order ()
      (should (equal window-sides-slots '(1 nil nil nil))))))

;;; windows-test.el ends here
