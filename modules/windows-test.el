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
;;
;; The width test near the popup-routing section (AC5) skips unless
;; `modules/claude-term.el' is also passed on the command line -- see that
;; test's own Commentary note for the exact invocation.

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

;; ============================================================================
;; Popup routing, pinning, and quit-restore (this phase)
;; ============================================================================

(defun edmacs-windows-test--nonside-count ()
  "Count the selected frame's non-side windows."
  (length (seq-filter (lambda (w) (not (window-parameter w 'window-side)))
                       (window-list nil 'no-minibuf))))

(defun edmacs-windows-test--right-windows ()
  "Return the selected frame's right-side windows, unsorted."
  (seq-filter (lambda (w) (eq (window-parameter w 'window-side) 'right))
              (window-list nil 'no-minibuf)))

(defun edmacs-windows-test--fresh-named-buffer (name)
  "Return a live buffer named exactly NAME, killing any stray one first.
Loading modules/claude-term.el under `-Q' (AC5's own test) logs
\"Unrecognized keyword\" warnings, which creates a real \"*Warnings*\"
buffer; without this, `generate-new-buffer' on that exact name would
silently get \"*Warnings*<2>\" instead, missing the routed pattern's
`\\'' anchor. \"*Messages*\" is handled separately in the AC1 test below,
since Emacs always has one and it must never be killed."
  (when (get-buffer name)
    (kill-buffer name))
  (generate-new-buffer name))

(defconst edmacs-windows-test--popup-names
  '("*Warnings*" "*Messages*" "*Help*" "*helpful variable: foo*"
    "*compilation*" "*quickrun*" "*Flycheck errors*" "*Backtrace*"
    "*Occur*" "*grep*" "*xref*" "*magit-diff: edmacs*" "*magit-log: edmacs*"
    "*lsp-help*" "*Embark Collect Live*")
  "One representative buffer name per routed `display-buffer-alist' pattern.")

;; ---------------------------------------------------------------------------
;; AC1 -- every routed name lands on right/-1; the center is untouched
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-popup-routes-to-right-slot-minus-1 ()
  (dolist (name edmacs-windows-test--popup-names)
    (save-window-excursion
      (delete-other-windows)
      (let* ((before (edmacs-windows-test--nonside-count))
             ;; "*Messages*" always already exists (Emacs creates it at
             ;; startup); `generate-new-buffer' on that exact name would
             ;; silently get "*Messages*<2>" instead, which the routed
             ;; pattern's `\\'' anchor does not match. Use the real buffer
             ;; for that one case rather than creating (and killing) a
             ;; second one.
             (real-messages (equal name "*Messages*"))
             (buf (if real-messages (get-buffer name) (edmacs-windows-test--fresh-named-buffer name))))
        (unwind-protect
            (let ((win (display-buffer buf)))
              (should win)
              (should (eq (window-parameter win 'window-side) 'right))
              (should (equal (window-parameter win 'window-slot) -1))
              (should (= before (edmacs-windows-test--nonside-count))))
          (unless real-messages (kill-buffer buf)))))))

;; ---------------------------------------------------------------------------
;; AC2 -- two different routed buffers in a row share the same slot -1 window
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-popup-second-routed-buffer-reuses-window ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buf-a (edmacs-windows-test--fresh-named-buffer "*Warnings*"))
          (buf-b (edmacs-windows-test--fresh-named-buffer "*Help*")))
      (unwind-protect
          (let* ((win-a (display-buffer buf-a))
                 (count-after-a (length (window-list nil 'no-minibuf)))
                 (win-b (display-buffer buf-b)))
            (should (eq win-a win-b))
            (should (eq (window-buffer win-a) buf-b))
            (should (= count-after-a (length (window-list nil 'no-minibuf)))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))

(ert-deftest edmacs-windows-test-embark-collect-keeps-mode-line-format-none ()
  "Consolidating Embark's entry into windows.el must not drop the
`(mode-line-format . none)' window-parameter completion.el's own
now-removed entry used to set -- that parameter is Embark's own
rendering concern, not something any routing AC re-tests on its own."
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (edmacs-windows-test--fresh-named-buffer "*Embark Collect Live*")))
      (unwind-protect
          (let ((win (display-buffer buf)))
            (should (eq (window-parameter win 'mode-line-format) 'none)))
        (kill-buffer buf)))))

;; ---------------------------------------------------------------------------
;; AC3 -- agents (0,1,2) + popup (-1) + pin (-2) + a fresh popup (-1 again)
;; ---------------------------------------------------------------------------

(defun edmacs-windows-test--display-agent-pane (buffer slot)
  "Display BUFFER as a right-column stack pane at SLOT, claude-term.el's shape."
  (display-buffer
   buffer
   `((display-buffer-in-side-window)
     (side . right)
     (slot . ,slot)
     (window-parameters . ((no-other-window . t))))))

(ert-deftest edmacs-windows-test-pin-then-fresh-popup-yields-five-windows ()
  (save-window-excursion
    (delete-other-windows)
    (let (bufs)
      (unwind-protect
          (progn
            (dotimes (i 3)
              (let ((b (generate-new-buffer (format "*ewt-agent-%d*" i))))
                (push b bufs)
                (edmacs-windows-test--display-agent-pane b i)))
            (let* ((popup-buf (edmacs-windows-test--fresh-named-buffer "*Warnings*"))
                   (popup-win (display-buffer popup-buf)))
              (push popup-buf bufs)
              (should (equal (window-parameter popup-win 'window-slot) -1))
              (edmacs-stack-pin popup-win)
              ;; Pin deletes the old slot -1 window and creates a new one at
              ;; -2, so right after the pin call slot -1 is vacant again and
              ;; only 4 right-edge windows are live.
              (let ((right (edmacs-windows-test--right-windows)))
                (should (= (length right) 4))
                (should (equal (sort (mapcar (lambda (w) (window-parameter w 'window-slot)) right) #'<)
                                '(-2 0 1 2)))
                (should-not (memq popup-win right))
                (should (window-live-p (get-buffer-window popup-buf t))))
              ;; A second, DIFFERENT popup recreates slot -1 fresh, since
              ;; the right column is uncapped -- five distinct live windows,
              ;; each still reporting the slot it was created with.
              (let* ((help-buf (edmacs-windows-test--fresh-named-buffer "*Help*"))
                     (help-win (display-buffer help-buf)))
                (push help-buf bufs)
                (should (equal (window-parameter help-win 'window-slot) -1))
                (let ((right (edmacs-windows-test--right-windows)))
                  (should (= (length right) 5))
                  (should (equal (sort (mapcar (lambda (w) (window-parameter w 'window-slot)) right) #'<)
                                  '(-2 -1 0 1 2)))
                  (should (window-live-p help-win))
                  (should (window-live-p (get-buffer-window popup-buf t)))))))
        (dolist (b bufs) (when (buffer-live-p b) (kill-buffer b)))))))

;; ---------------------------------------------------------------------------
;; AC4 -- `q' in a popup pane force-deletes it and returns to main
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-quit-window-deletes-popup-and-selects-main ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (buf (edmacs-windows-test--fresh-named-buffer "*Warnings*")))
      (unwind-protect
          (let ((win (display-buffer buf)))
            (select-window win)
            (quit-window nil win)
            (should-not (window-live-p win))
            (should (eq (selected-window) main))
            (should (buffer-live-p buf)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-quit-window-after-two-popups-does-not-resurrect-first ()
  "Regression: a second popup reusing slot -1 leaves a stale
`window-prev-buffers' entry for the first one; without the
`quit-restore-window' advice, stock `quit-window' would restore the
first popup instead of deleting the pane and returning to main."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (buf1 (edmacs-windows-test--fresh-named-buffer "*Warnings*"))
           (buf2 (edmacs-windows-test--fresh-named-buffer "*Help*")))
      (unwind-protect
          (progn
            (display-buffer buf1)
            (let ((win2 (display-buffer buf2)))
              (select-window win2)
              (quit-window nil win2)
              (should-not (window-live-p win2))
              (should (eq (selected-window) main))
              (should (buffer-live-p buf1))
              (should (buffer-live-p buf2))
              ;; Buf1 must not have been resurrected into any window.
              (should-not (get-buffer-window buf1 t))))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

(ert-deftest edmacs-windows-test-quit-window-with-kill-arg-kills-popup-buffer ()
  "Regression: `C-u q' (quit-window with KILL) on a routed popup must
kill the buffer, not just delete the window -- stock `quit-window'
calls `(quit-restore-window window 'kill)' and documents that the
buffer gets killed."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (buf (edmacs-windows-test--fresh-named-buffer "*Warnings*")))
      (let ((win (display-buffer buf)))
        (select-window win)
        (quit-window t win)
        (should-not (window-live-p win))
        (should (eq (selected-window) main))
        (should-not (buffer-live-p buf))))))

;; ---------------------------------------------------------------------------
;; AC5 -- every right-edge window's width tracks `edmacs-stack-width' live
;; ---------------------------------------------------------------------------
;; The agent-pane half of this test needs `modules/claude-term.el' loaded
;; (its own `display-buffer' path is what proves the agent-pane side of
;; this AC already worked before this phase). Loading it under `-Q' prints
;; a benign "Unrecognized keyword: :straight" notice from the
;; `use-package ghostel'/`use-package evil-ghostel' forms -- see
;; claude-term-test.el's own Commentary for why that is harmless here.
;; Run this one test (or the whole file) with:
;;   emacs -Q --batch -l ert -l modules/windows.el -l modules/claude-term.el \
;;         -l modules/windows-test.el -f ert-run-tests-batch-and-exit

(ert-deftest edmacs-windows-test-width-tracks-live-variable-for-agent-and-popup ()
  (if (not (fboundp 'claude-term--display-buffer))
      (ert-skip "modules/claude-term.el not loaded -- see this test's Commentary")
    (save-window-excursion
      (delete-other-windows)
      (let ((agent-buf (generate-new-buffer "*ewt-width-agent*"))
            (popup-buf (edmacs-windows-test--fresh-named-buffer "*Warnings*")))
        (unwind-protect
            (let (agent-w1 popup-w1 agent-w2 popup-w2)
              (let ((edmacs-stack-width 0.4))
                (delete-other-windows)
                (setq agent-w1 (window-total-width (claude-term--display-buffer agent-buf)))
                (setq popup-w1 (window-total-width (display-buffer popup-buf))))
              (delete-other-windows)
              (let ((edmacs-stack-width 0.6))
                (setq agent-w2 (window-total-width (claude-term--display-buffer agent-buf)))
                (setq popup-w2 (window-total-width (display-buffer popup-buf))))
              (should (/= agent-w1 agent-w2))
              (should (/= popup-w1 popup-w2))
              (should (> agent-w2 agent-w1))
              (should (> popup-w2 popup-w1)))
          (kill-buffer agent-buf)
          (kill-buffer popup-buf))))))

;;; windows-test.el ends here
