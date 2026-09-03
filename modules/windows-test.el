;;; windows-test.el --- Tests for windows.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function/window-parameter coverage -- no display, no theme, no
;; live subprocess.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/windows.el -l modules/claude-term.el \
;;         -l modules/windows-test.el -f ert-run-tests-batch-and-exit
;;
;; `modules/claude-term.el' is on the invocation line above because
;; `edmacs-stack-agent-pane-p' (whose wiring this file's sweep tests
;; stub out) and the pane-shaped helpers here mirror that module's
;; buffer-name conventions. Loading it under `-Q' prints a benign
;; "Unrecognized keyword: :straight" notice from the `use-package
;; ghostel'/`use-package evil-ghostel' forms -- see claude-term-test.el's
;; own Commentary for why that is harmless here.
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
;; The "SPC w binding surface" and "C-w window prefix" sections near the
;; bottom load
;; `modules/keybindings.el' itself, fixing up `load-path' against the
;; straight source tree first -- it is deliberately NOT passed on the
;; invocation line above, the same treatment AC4 gives `modules/sidebar.el'.
;; See that section's own comments for the straight-bootstrap skip condition.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'tab-bar)
(require 'cl-lib)

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
            ;; window configuration, but `edmacs-windows--on-tab-open' (this
            ;; phase's `tab-bar-tab-post-open-functions' hook) designates the
            ;; new tab's sole window as main immediately.
            (tab-bar-new-tab)
            (should (seq-find (lambda (w) (window-parameter w 'edmacs-main))
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
  "The blanket flag stays off: sidebar.el's own ACs forbid its left side
window being windmove-reachable, and nothing needs the exemption any
more now that agent panes are ordinary windows."
  (should (null windmove-allow-all-windows)))

(ert-deftest edmacs-windows-test-windmove-does-not-reach-no-other-window ()
  "A `no-other-window' window (sidebar.el's shape) is never
windmove-reachable, even though it is the nearest window in that
direction -- and the `edmacs-windmove-reachable' opt-in that used to
carve agent panes back out of this is gone with the side-window panes
themselves, so marking a window no longer changes the answer."
  (edmacs-windows-test--with-side-windows
    (should-not (windmove-find-other-window 'left))
    (should-not (windmove-find-other-window 'right))))

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

(ert-deftest edmacs-windows-test-quit-restore-window-killing-does-not-kill-buffer ()
  "Regression: stock `quit-restore-window' distinguishes BURY-OR-KILL
`kill' from `killing' -- `killing' means the caller (e.g.
`quit-windows-on', `replace-buffer-in-windows') will kill the buffer
itself, and `quit-restore-window' must not kill it first. Conflating
the two breaks callers such as `quit-windows-on' that process
multiple windows for the same buffer."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (buf (edmacs-windows-test--fresh-named-buffer "*Warnings*")))
      (unwind-protect
          (let ((win (display-buffer buf)))
            (select-window win)
            (quit-restore-window win 'killing)
            (should-not (window-live-p win))
            (should (eq (selected-window) main))
            (should (buffer-live-p buf)))
        (kill-buffer buf)))))

;; ---------------------------------------------------------------------------
;; AC5 -- every right-edge window's width tracks `edmacs-stack-width' live
;; ---------------------------------------------------------------------------
(ert-deftest edmacs-windows-test-popup-width-tracks-live-variable ()
  "A routed popup re-reads `edmacs-stack-width' on every display, so
rebinding the variable resizes the next popup without re-registering any
`display-buffer-alist' entry.  Agent panes are deliberately NOT covered:
they are ordinary windows now (see claude-term.el's \"Pane display\")
and take whatever size the split gives them."
  (save-window-excursion
    (delete-other-windows)
    (let ((popup-buf (edmacs-windows-test--fresh-named-buffer "*Warnings*")))
      (unwind-protect
          (let (popup-w1 popup-w2)
            (let ((edmacs-stack-width 0.4))
              (delete-other-windows)
              (setq popup-w1 (window-total-width (display-buffer popup-buf))))
            (delete-other-windows)
            (let ((edmacs-stack-width 0.6))
              (setq popup-w2 (window-total-width (display-buffer popup-buf))))
            (should (/= popup-w1 popup-w2))
            (should (> popup-w2 popup-w1)))
        (kill-buffer popup-buf)))))

;; ============================================================================
;; Phase 4 -- demote, stack cycling, close, numeric-prefix promote, widen/narrow
;; ============================================================================

(defun edmacs-windows-test--display-claude-term-shaped-pane (buffer slot)
  "Display BUFFER as a right-column stack pane at SLOT.
The shape claude-term.el used to display an agent pane with, kept here
because the stack commands under test (`edmacs-stack-next'/`-prev',
`edmacs-stack-close', numeric-prefix promote) still have to handle a
`no-other-window' side window -- sidebar.el's left window is one, and a
pinned popup can be moved into the column by hand."
  (display-buffer
   buffer
   `((display-buffer-in-side-window)
     (side . right)
     (slot . ,slot)
     (window-parameters . ((no-delete-other-windows . t)
                            (no-other-window . t))))))

(defun edmacs-windows-test--slot-1-window ()
  "Return the selected frame's right-column window at slot -1, or nil."
  (seq-find (lambda (w) (equal (window-parameter w 'window-slot) -1))
            (edmacs-windows-test--right-windows)))

;; ---------------------------------------------------------------------------
;; AC1 -- demote moves main's buffer to the popup slot; promote round-trips it
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-demote-then-promote-round-trips-main-buffer ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (buf-b (edmacs-windows-test--fresh-named-buffer "*ewt-demote-b*"))
           (buf-a (edmacs-windows-test--fresh-named-buffer "*ewt-demote-a*")))
      (unwind-protect
          (progn
            ;; Two switches leave B as the head of `window-prev-buffers',
            ;; ahead of whatever main showed before (e.g. *scratch*).
            (set-window-buffer main buf-b)
            (set-window-buffer main buf-a)
            (edmacs-window-demote)
            (let ((slot-1 (edmacs-windows-test--slot-1-window)))
              (should slot-1)
              (should (eq (window-buffer slot-1) buf-a))
              (should (eq (window-buffer main) buf-b))
              (edmacs-window-promote slot-1)
              (should (eq (window-buffer main) buf-a))
              (should (eq (window-buffer slot-1) buf-b))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))

;; ---------------------------------------------------------------------------
;; AC2 -- stack-next / stack-prev walk the stack in slot order, wrapping
;; through main
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-stack-next-prev-slot-sequence ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((main (edmacs-main-window))
          bufs)
      (unwind-protect
          (progn
            (dotimes (i 3)
              (let ((b (generate-new-buffer (format "*ewt-cycle-%d*" i))))
                (push b bufs)
                (edmacs-windows-test--display-agent-pane b i)))
            (select-window main)
            (let (seq)
              (dotimes (_ 4)
                (edmacs-stack-next)
                (push (window-parameter (selected-window) 'window-slot) seq))
              (should (equal (nreverse seq) '(0 1 2 nil))))
            (select-window main)
            (let (seq)
              (dotimes (_ 4)
                (edmacs-stack-prev)
                (push (window-parameter (selected-window) 'window-slot) seq))
              (should (equal (nreverse seq) '(2 1 0 nil)))))
        (dolist (b bufs) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest edmacs-windows-test-stack-next-prev-empty-stack-stays-on-main ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((main (edmacs-main-window)))
      (edmacs-stack-next)
      (should (eq (selected-window) main))
      (edmacs-stack-prev)
      (should (eq (selected-window) main)))))

;; ---------------------------------------------------------------------------
;; AC3 -- a numeric prefix arg promotes the pane at that index
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-promote-numeric-prefix-selects-indexed-pane ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (main-buf (window-buffer main))
           bufs)
      (unwind-protect
          (progn
            (dotimes (i 3)
              (let ((b (generate-new-buffer (format "*ewt-promote-idx-%d*" i))))
                (push b bufs)
                (edmacs-windows-test--display-agent-pane b i)))
            (let* ((target (nth 2 (edmacs-stack-windows)))
                   (target-buf (window-buffer target)))
              (let ((current-prefix-arg 2))
                (call-interactively #'edmacs-window-promote))
              (should (eq (window-buffer main) target-buf))
              (should (eq (window-buffer target) main-buf))))
        (dolist (b bufs) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest edmacs-windows-test-promote-numeric-prefix-out-of-range-is-noop ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (main-buf (window-buffer main))
           (buf (generate-new-buffer "*ewt-promote-oob*"))
           (win (edmacs-windows-test--display-agent-pane buf 0)))
      (unwind-protect
          (let ((current-prefix-arg 5))
            (call-interactively #'edmacs-window-promote)
            (should (eq (window-buffer main) main-buf))
            (should (eq (window-buffer win) buf)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-promote-numeric-prefix-negative-is-noop ()
  "A negative prefix must not fall through to `nth's CAR-on-negative-index behavior."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (main-buf (window-buffer main))
           (buf (generate-new-buffer "*ewt-promote-negative*"))
           (win (edmacs-windows-test--display-agent-pane buf 0)))
      (unwind-protect
          (let ((current-prefix-arg -1))
            (call-interactively #'edmacs-window-promote)
            (should (eq (window-buffer main) main-buf))
            (should (eq (window-buffer win) buf)))
        (kill-buffer buf)))))

;; ---------------------------------------------------------------------------
;; AC4 -- stack-close deletes an agent pane's window without killing its buffer
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-stack-close-agent-pane-preserves-buffer ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (buf (generate-new-buffer "*ewt-agent-close*"))
           (win (edmacs-windows-test--display-claude-term-shaped-pane buf 0)))
      (unwind-protect
          (progn
            (should win)
            (select-window win)
            (edmacs-stack-close)
            (should-not (window-live-p win))
            (should (buffer-live-p buf))
            (should (eq (selected-window) main)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest edmacs-windows-test-stack-close-on-main-is-noop ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((main (edmacs-main-window)))
      (select-window main)
      (edmacs-stack-close)
      (should (window-live-p main))
      (should (eq (selected-window) main)))))

;; ---------------------------------------------------------------------------
;; `SPC w d' -- delete-or-demote's three branches
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-delete-or-demote-on-main-with-center-split-demotes ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (main-buf (window-buffer main))
           (other (split-window main nil 'below))
           (other-buf (generate-new-buffer "*ewt-dd-other*")))
      (unwind-protect
          (progn
            (set-window-buffer other other-buf)
            (select-window main)
            (edmacs-window-delete-or-demote)
            ;; Demote leaves OTHER's own split untouched and lands main's
            ;; old buffer in the popup slot.
            (should (eq (window-buffer other) other-buf))
            (let ((slot-1 (edmacs-windows-test--slot-1-window)))
              (should slot-1)
              (should (eq (window-buffer slot-1) main-buf))))
        (kill-buffer other-buf)))))

(ert-deftest edmacs-windows-test-delete-or-demote-on-main-without-center-split-is-noop ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (main-buf (window-buffer main)))
      (edmacs-window-delete-or-demote)
      (should (eq (window-buffer main) main-buf))
      (should (window-live-p main))
      (should-not (edmacs-windows-test--right-windows)))))

(ert-deftest edmacs-windows-test-delete-or-demote-off-main-deletes-window ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (other (split-window main nil 'below)))
      (select-window other)
      (edmacs-window-delete-or-demote)
      (should-not (window-live-p other))
      (should (window-live-p main)))))

;; ---------------------------------------------------------------------------
;; AC5 -- widen/narrow are numeric inverses on a fixed 0.05 grid
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-stack-widen-narrow-numeric-inverses ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((edmacs-stack-width 0.40)
          bufs)
      (unwind-protect
          (progn
            (dotimes (i 2)
              (let ((b (generate-new-buffer (format "*ewt-width-%d*" i))))
                (push b bufs)
                (edmacs-windows-test--display-agent-pane b i)))
            (dotimes (_ 3) (edmacs-stack-widen))
            (should (= edmacs-stack-width 0.55))
            (dolist (w (edmacs-stack-windows))
              (should (= (window-total-width w)
                         (round (* 0.55 (window-total-width (frame-root-window)))))))
            (dotimes (_ 3) (edmacs-stack-narrow))
            (should (= edmacs-stack-width 0.40))
            (dolist (w (edmacs-stack-windows))
              (should (= (window-total-width w)
                         (round (* 0.40 (window-total-width (frame-root-window))))))))
        (dolist (b bufs) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest edmacs-windows-test-balance-center-leaves-stack-width-untouched ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((main (edmacs-main-window))
           (buf (generate-new-buffer "*ewt-balance-agent*"))
           (agent-win (edmacs-windows-test--display-agent-pane buf 0))
           (agent-width (window-total-width agent-win))
           (split (split-window main 5 'below)))
      (unwind-protect
          (progn
            (should (/= (window-total-height main) (window-total-height split)))
            (edmacs-stack-balance-center)
            (should (<= (abs (- (window-total-height main) (window-total-height split))) 1))
            (should (= (window-total-width agent-win) agent-width)))
        (kill-buffer buf)))))

;; ---------------------------------------------------------------------------
;; AC6/AC7 -- `SPC w' binding surface: real evil.el/general.el, plus
;; modules/keybindings.el itself
;; ---------------------------------------------------------------------------
;; Same technique `claude-term-registry-test.el' uses for `SPC a': vendored
;; `general.el'/`evil.el' sources added to `load-path' rather than
;; re-implementing general.el's own :states/:prefix dispatch by hand.
;; Duplicated locally (not required) to keep this file's own Commentary
;; invocation self-contained, matching every other *-test.el file's
;; convention in this repo.

(defun edmacs-windows-test--locate-straight-repos-root ()
  "Return this checkout's `straight/repos' directory, or nil.
Tries this checkout's own `straight/repos' first, then falls back to the
sibling main `edmacs' checkout's -- see
`edmacs-windows-test--locate-straight-build-root' above for why a
roadmap worktree needs the fallback."
  (or
   (let ((here (expand-file-name "straight/repos" default-directory)))
     (and (file-directory-p here) here))
   (let* ((root (directory-file-name (expand-file-name default-directory)))
          (worktrees-dir (directory-file-name (file-name-directory root))))
     (when (string-suffix-p "__worktrees" worktrees-dir)
       (let* ((projects-dir (file-name-directory worktrees-dir))
              (repo-name (string-remove-suffix
                          "__worktrees" (file-name-nondirectory worktrees-dir)))
              (main-repos (expand-file-name
                           (concat repo-name "/straight/repos") projects-dir)))
         (and (file-directory-p main-repos) main-repos))))))

(defvar edmacs-windows-test--repos-root
  (edmacs-windows-test--locate-straight-repos-root)
  "This checkout's (or its sibling main checkout's) `straight/repos' root.")

(defvar edmacs-windows-test--keybindings-loaded nil
  "Non-nil once `modules/keybindings.el' has been loaded for real by this file.")

(defun edmacs-windows-test--ensure-spc-w-bindings ()
  "Load real evil/general and `modules/keybindings.el', once.
Returns non-nil on success; nil (without erroring) when this checkout
has never bootstrapped straight locally, so callers can `ert-skip'."
  (when edmacs-windows-test--repos-root
    (let ((evil-source (expand-file-name "evil/evil.el" edmacs-windows-test--repos-root))
          (general-source (expand-file-name "general.el/general.el" edmacs-windows-test--repos-root)))
      (when (and (file-exists-p evil-source) (file-exists-p general-source))
        (add-to-list 'load-path (file-name-directory evil-source))
        (add-to-list 'load-path (file-name-directory general-source))
        (require 'evil)
        (require 'general)
        (unless edmacs-windows-test--keybindings-loaded
          (load (expand-file-name "modules/keybindings.el" default-directory) nil t)
          (setq edmacs-windows-test--keybindings-loaded t))
        t))))

(defconst edmacs-windows-test--spc-w-new-leaves
  '(("-" . edmacs-window-demote)
    ("]" . edmacs-stack-next)
    ("[" . edmacs-stack-prev)
    ("x" . edmacs-stack-close)
    (">" . edmacs-stack-widen)
    ("<" . edmacs-stack-narrow)
    ("S" . edmacs-stack-toggle)
    ("d" . edmacs-window-delete-or-demote)
    ("=" . edmacs-stack-balance-center))
  "This phase's new/changed `SPC w' leaves, key -> intended command.")

(declare-function evil-get-auxiliary-keymap "evil-core")
(declare-function evil-get-minor-mode-keymap "evil-core")
(declare-function evil-state-keymaps "evil-core")
(declare-function evil-define-key* "evil-core")
(declare-function evil-local-mode "evil-core")
(declare-function evil-insert-state "evil-states")
(defvar evil-window-map)
(defvar general-override-mode-map)

(defun edmacs-windows-test--spc-w-keymap ()
  "Return the real keymap `SPC w' resolves into for normal state.
`leader-def' (`modules/keybindings.el') binds with `:keymaps \\='override',
which general.el resolves -- for a given evil state -- into an evil
auxiliary keymap hung off `general-override-mode-map', not into
`evil-normal-state-map' itself; `lookup-key' on this is the equivalent of
`evil-normal-state-map' for `SPC a's directly-bound leaves."
  (evil-get-auxiliary-keymap general-override-mode-map 'normal))

(ert-deftest edmacs-windows-test-spc-w-new-and-changed-leaves-resolve-via-override-map ()
  "Every new/changed `SPC w' leaf resolves through the same keymap a
known-good pre-existing leaf (`ww' -> `other-window') already dispatches
through -- `leader-def''s single `:keymaps \\='override' binding site,
not a second binding mechanism."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (let ((keymap (edmacs-windows-test--spc-w-keymap)))
    (should (eq (lookup-key keymap (kbd "SPC w w")) 'other-window))
    (dolist (pair edmacs-windows-test--spc-w-new-leaves)
      (should (eq (lookup-key keymap (kbd (concat "SPC w " (car pair))))
                  (cdr pair))))))

(ert-deftest edmacs-windows-test-spc-w-not-in-evil-normal-state-map-directly ()
  "Documents why this file resolves `SPC w' through the override auxiliary
keymap rather than `evil-normal-state-map' directly: `leader-def' binds
with `:keymaps \\='override', which general.el/evil put ahead of
`evil-normal-state-map' in the active keymap search order via
`general-override-mode-map' (a minor-mode map), so `evil-normal-state-map'
itself never gains these entries even though the override keymap is what
a real `SPC w -' keypress actually dispatches through. `lookup-key'
returns nil or an integer (a valid-prefix-but-undefined-continuation
marker, per its docstring) here, never the command -- asserted as
\"not the command symbol\" so the two keymaps' differing contents can't
be mistaken for a bug. `SPC w' must keep exactly one binding mechanism, so
this phase does not add a second, redundant `evil-normal-state-map'
binding just to make a direct lookup succeed."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (should-not (eq (lookup-key evil-normal-state-map (kbd "SPC w -"))
                  'edmacs-window-demote)))

(ert-deftest edmacs-windows-test-spc-w-keeps-a-which-key-heading ()
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (require 'which-key)
  ;; general.el records a `:which-key' prefix label as a
  ;; `which-key-replacement-alist' entry keyed on the anchored key
  ;; sequence -- see `general--add-which-key-replacement'.
  (should (seq-find (lambda (entry)
                      (and (consp entry)
                           (consp (car entry))
                           (equal (caar entry) "\\`SPC w\\'")))
                    which-key-replacement-alist)))

(ert-deftest edmacs-windows-test-spc-w-s-v-te-unchanged ()
  "Phase 5 step 4 depends on `SPC w s'/`SPC w v' staying exactly as-is;
`SPC w t e' is the kept alias for `edmacs-window-promote'."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (let ((keymap (edmacs-windows-test--spc-w-keymap)))
    (should (eq (lookup-key keymap (kbd "SPC w s")) 'split-window-below))
    (should (eq (lookup-key keymap (kbd "SPC w v")) 'split-window-right))
    (should (eq (lookup-key keymap (kbd "SPC w t e")) 'edmacs-window-promote))))

;; ---------------------------------------------------------------------------
;; C-w window prefix -- reachable from every state
;; ---------------------------------------------------------------------------
;; Same straight-bootstrap skip condition as the `SPC w' tests above, and the
;; same one-shot `modules/keybindings.el' load.

(defconst edmacs-windows-test--c-w-leaves
  '(("|" . split-window-right)
    ("-" . split-window-below)
    ("H" . evil-window-decrease-width)
    ("L" . evil-window-increase-width)
    ("J" . evil-window-increase-height)
    ("K" . evil-window-decrease-height)
    ("RET" . edmacs-window-promote)
    ("=" . edmacs-stack-balance-center)
    ("x" . edmacs-stack-close)
    ("d" . edmacs-window-delete-or-demote)
    ("m" . edmacs-window-pop-buffer-to-main)
    ("[" . edmacs-stack-prev)
    ("]" . edmacs-stack-next)
    ("<" . edmacs-stack-narrow)
    (">" . edmacs-stack-widen)
    ("S" . edmacs-stack-toggle))
  "The tmux vocabulary layered onto `evil-window-map', key -> command.")

(ert-deftest edmacs-windows-test-c-w-leaves-resolve-in-evil-window-map ()
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (dolist (pair edmacs-windows-test--c-w-leaves)
    (should (eq (lookup-key evil-window-map (kbd (car pair))) (cdr pair)))))

(ert-deftest edmacs-windows-test-c-w-agrees-with-spc-w ()
  "Every command reachable both ways is reachable by the SAME key under
both prefixes -- one vocabulary, two entry points."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (let ((spc-w (edmacs-windows-test--spc-w-keymap)))
    (dolist (key '("=" "x" "d" "m" "[" "]" "<" ">" "S"))
      (should (eq (lookup-key evil-window-map (kbd key))
                  (lookup-key spc-w (kbd (concat "SPC w " key))))))))

(ert-deftest edmacs-windows-test-c-w-is-a-prefix-in-every-state ()
  "`C-w' reaches `evil-window-map' from insert and emacs state, not just
normal/motion -- moving between windows must never require leaving
insert state. Asserted through `evil-get-minor-mode-keymap', the bucket
`evil-state-keymaps' consults AHEAD of the auxiliary maps evil-ghostel
binds its own insert-state `C-w' terminal passthrough into."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (should edmacs-window-prefix-mode)
  (dolist (state '(normal visual insert emacs motion replace operator))
    (let ((map (evil-get-minor-mode-keymap state 'edmacs-window-prefix-mode)))
      (should (keymapp map))
      (should (eq (lookup-key map (kbd "C-w")) evil-window-map))
      ;; and a real leaf resolves through it, not merely the prefix
      (should (eq (lookup-key map (kbd "C-w h")) 'evil-window-left)))))

(ert-deftest edmacs-windows-test-c-w-minor-mode-map-outranks-auxiliary-maps ()
  "The reason `evil-define-minor-mode-key' is used rather than general's
`override' keymap: `evil-state-keymaps' orders minor-mode maps ahead of
the auxiliary maps `evil-define-key*' populates, which is what puts this
binding ahead of evil-ghostel's insert-state `C-w' passthrough inside a
claude-term pane. The probe below stands in for
`evil-ghostel-mode-map' -- an ordinary keymap carrying an
`evil-define-key*' insert-state binding -- and is made active as the
buffer's local map so evil actually collects its auxiliary keymap."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (let ((probe (make-sparse-keymap))
        (mode-map (evil-get-minor-mode-keymap 'insert 'edmacs-window-prefix-mode)))
    (evil-define-key* 'insert probe (kbd "C-w") #'ignore)
    (with-temp-buffer
      (use-local-map probe)
      (evil-local-mode 1)
      (evil-insert-state)
      (let* ((maps (mapcar #'cdr (evil-state-keymaps 'insert)))
             (aux (evil-get-auxiliary-keymap probe 'insert))
             (mode-pos (seq-position maps mode-map))
             (aux-pos (seq-position maps aux)))
        (should mode-pos)
        (should aux-pos)
        (should (< mode-pos aux-pos))
        ;; and the winner really is this module's binding, not the probe's
        (should (eq (lookup-key (make-composed-keymap maps) (kbd "C-w h"))
                    'evil-window-left))))))

;; ============================================================================
;; Persistence: tab-bar hooks, dead-pane sweep, lowest-free-slot allocation
;; (this phase)
;; ============================================================================

;; ---------------------------------------------------------------------------
;; AC1 -- tab-bar-new-tab designates main immediately, no stale side windows
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-new-tab-designates-main-no-stale-panes ()
  "A fresh tab has exactly one non-side window, carrying `edmacs-main', and
no stale RIGHT stack windows. Not asserted as \"exactly one window total\":
when this file's own AC4 load-order section below has loaded the real
`modules/sidebar.el' (this checkout has a bootstrapped straight build),
that module's own independent `tab-bar-tab-post-open-functions' hook
correctly re-shows a LEFT sidebar window in the same new tab -- see this
phase's own Context on the two hooks coordinating, not colliding."
  (save-window-excursion
    (delete-other-windows)
    (let ((tabs-before (length (tab-bar-tabs))))
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (should (= (edmacs-windows-test--nonside-count) 1))
            (should (window-parameter (selected-window) 'edmacs-main))
            (should (null (edmacs-stack-windows))))
        (while (> (length (tab-bar-tabs)) tabs-before)
          (tab-bar-close-tab))))))

;; ---------------------------------------------------------------------------
;; AC2 -- main and stack (slot . buffer-name) pairs round-trip across
;; tab-bar-switch-to-next/prev-tab, as set equality
;; ---------------------------------------------------------------------------

(defun edmacs-windows-test--stack-pairs ()
  "Return the selected frame's stack as (slot . buffer-name) pairs."
  (mapcar (lambda (w) (cons (window-parameter w 'window-slot)
                             (buffer-name (window-buffer w))))
          (edmacs-stack-windows)))

(ert-deftest edmacs-windows-test-stack-round-trips-next-prev-tab ()
  (save-window-excursion
    (delete-other-windows)
    (let* ((tabs-before (length (tab-bar-tabs)))
           (main1-buf (generate-new-buffer "ewt-persist-main-1"))
           (pane1a (generate-new-buffer "ewt-persist-pane-1a"))
           (pane1b (generate-new-buffer "ewt-persist-pane-1b"))
           (main2-buf (generate-new-buffer "ewt-persist-main-2"))
           (pane2a (generate-new-buffer "ewt-persist-pane-2a")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) main1-buf)
            (edmacs-window-set-main (selected-window))
            (edmacs-windows-test--display-claude-term-shaped-pane pane1a 0)
            (edmacs-windows-test--display-claude-term-shaped-pane pane1b 1)
            (let ((tab1-pairs (edmacs-windows-test--stack-pairs)))
              (tab-bar-new-tab)
              (set-window-buffer (selected-window) main2-buf)
              (edmacs-window-set-main (selected-window))
              (edmacs-windows-test--display-claude-term-shaped-pane pane2a 0)
              (let ((tab2-pairs (edmacs-windows-test--stack-pairs)))
                (tab-bar-switch-to-prev-tab)
                (should (eq (window-buffer (edmacs-main-window)) main1-buf))
                (should (seq-set-equal-p (edmacs-windows-test--stack-pairs) tab1-pairs #'equal))
                (tab-bar-switch-to-next-tab)
                (should (eq (window-buffer (edmacs-main-window)) main2-buf))
                (should (seq-set-equal-p (edmacs-windows-test--stack-pairs) tab2-pairs #'equal)))))
        (while (> (length (tab-bar-tabs)) tabs-before)
          (tab-bar-close-tab))
        (dolist (b (list main1-buf pane1a pane1b main2-buf pane2a))
          (when (buffer-live-p b) (kill-buffer b)))))))

;; ---------------------------------------------------------------------------
;; AC3 -- edmacs-stack-sweep-stale-panes deletes dead/stale panes, keeps
;; live popups, re-designates main when missing
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-sweep-deletes-dead-buffer-window ()
  "Killing a dedicated side window's buffer through normal `kill-buffer'
deletes the window automatically (verified empirically -- see
claude-term-test.el's own Commentary on the same point), so the 'dead
buffer, window still live' state this test covers is fabricated via
`cl-letf' on `buffer-live-p' rather than reproduced through a literal
kill-buffer call."
  (save-window-excursion
    (delete-other-windows)
    (let* ((buf (generate-new-buffer "ewt-sweep-dead"))
           (win (edmacs-windows-test--display-claude-term-shaped-pane buf 0))
           (orig-live-p (symbol-function 'buffer-live-p)))
      (unwind-protect
          (progn
            (should (window-live-p win))
            (cl-letf (((symbol-function 'buffer-live-p)
                       (lambda (b) (if (eq b buf) nil (funcall orig-live-p b)))))
              (edmacs-stack-sweep-stale-panes))
            (should-not (window-live-p win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest edmacs-windows-test-sweep-deletes-stale-agent-pane ()
  (save-window-excursion
    (delete-other-windows)
    (let* ((buf (generate-new-buffer "ewt-sweep-agent"))
           (win (edmacs-windows-test--display-claude-term-shaped-pane buf 0)))
      (unwind-protect
          (let ((edmacs-stack-agent-pane-p (lambda (w) (eq w win))))
            (should (window-live-p win))
            (edmacs-stack-sweep-stale-panes)
            (should-not (window-live-p win)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-sweep-deletes-stale-agent-pane-in-an-ordinary-window ()
  "The sweep is not scoped to the stack column. Agent panes are ordinary
windows now (see claude-term.el's \"Pane display\"), so a pane whose
session died has to be swept from wherever a restored frameset put it,
not only from a right side window."
  (save-window-excursion
    (delete-other-windows)
    (let* ((buf (generate-new-buffer "ewt-sweep-agent-ordinary"))
           (win (split-window-right)))
      (set-window-buffer win buf)
      (unwind-protect
          (let ((edmacs-stack-agent-pane-p (lambda (w) (eq w win))))
            (should (window-live-p win))
            (should-not (window-parameter win 'window-side))
            (edmacs-stack-sweep-stale-panes)
            (should-not (window-live-p win)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-sweep-keeps-live-popup-pane ()
  (save-window-excursion
    (delete-other-windows)
    (let ((win (display-buffer-in-side-window
                (get-buffer "*Messages*")
                '((side . right) (slot . -1)))))
      (unwind-protect
          (progn
            (should (window-live-p win))
            (edmacs-stack-sweep-stale-panes)
            (should (window-live-p win))
            (should (eq (window-buffer win) (get-buffer "*Messages*"))))
        (when (window-live-p win) (delete-window win))))))

(ert-deftest edmacs-windows-test-sweep-redesignates-main-when-missing ()
  (save-window-excursion
    (delete-other-windows)
    (dolist (w (window-list nil 'no-minibuf))
      (set-window-parameter w 'edmacs-main nil))
    (should-not (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                           (window-list nil 'no-minibuf)))
    (edmacs-stack-sweep-stale-panes)
    (should (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                       (window-list nil 'no-minibuf)))))

;; ---------------------------------------------------------------------------
;; Agent panes are ordinary windows, so the stack never allocates a slot for
;; one. claude-term.el's own suite owns the pane-placement assertions now
;; (`claude-term-test-display-buffer-creates-an-ordinary-window' and
;; friends); what stays this module's business is that a pane in an
;; ordinary window is still swept when its session is dead -- covered by
;; `edmacs-windows-test-sweep-deletes-stale-agent-pane' above.
;; ---------------------------------------------------------------------------

;; ============================================================================
;; Catch-all tiling (phase 5)
;; ============================================================================

(ert-deftest edmacs-windows-test-fallback-routes-unrouted-buffer-to-right-slot-minus-1 ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "*ewt-anything*"))
          (nonside-count (edmacs-windows-test--nonside-count)))
      (unwind-protect
          (let ((win (display-buffer buf)))
            (should (eq (window-parameter win 'window-side) 'right))
            (should (equal (window-parameter win 'window-slot) -1))
            (should (= (edmacs-windows-test--nonside-count) nonside-count)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-other-window-shape-lands-in-stack ()
  ;; `find-file-other-window'/`xref-find-definitions-other-window'/
  ;; `switch-to-buffer-other-window' all reduce to `(display-buffer buf t)'
  ;; in Emacs 31.1, which `display-buffer' turns into action nil plus
  ;; `(inhibit-same-window . t)'; there is no `display-buffer--other-window-
  ;; action' constant in this version.
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "*ewt-other-window*")))
      (unwind-protect
          (let ((win (display-buffer buf '(nil (inhibit-same-window . t)))))
            (should (eq (window-parameter win 'window-side) 'right)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-switch-to-buffer-obey-display-actions-stays-nil ()
  (should-not switch-to-buffer-obey-display-actions))

(ert-deftest edmacs-windows-test-switch-to-buffer-in-main-does-not-move-window ()
  (save-window-excursion
    (delete-other-windows)
    (let ((main (selected-window))
          (other-buf (generate-new-buffer "*ewt-switch-target*")))
      (unwind-protect
          (progn
            (edmacs-window-set-main main)
            (switch-to-buffer other-buf)
            (should (eq (selected-window) main))
            (should (eq (window-buffer main) other-buf)))
        (kill-buffer other-buf)))))

(ert-deftest edmacs-windows-test-base-action-fallback-excludes-pop-up-window ()
  (should-not (memq #'display-buffer-pop-up-window (car display-buffer-base-action))))

(ert-deftest edmacs-windows-test-center-reuse-p-excludes-other-window-commands ()
  (let ((this-command 'find-file-other-window))
    (should-not (edmacs-windows--center-reuse-p nil nil))))

(ert-deftest edmacs-windows-test-center-reuse-p-includes-plain-file-commands ()
  (let ((this-command 'find-file))
    (should (edmacs-windows--center-reuse-p nil nil))))

(ert-deftest edmacs-windows-test-dired-mode-buffer-stays-center ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "*ewt-dired-stand-in*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq major-mode 'dired-mode))
            (should-not (window-parameter (display-buffer buf) 'window-side)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-magit-status-mode-buffer-stays-center ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "*ewt-magit-status-stand-in*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq major-mode 'magit-status-mode))
            (should-not (window-parameter (display-buffer buf) 'window-side)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-this-command-gated-file-open-stays-center ()
  ;; End-to-end coverage of the third allow-listed condition (AC5): binds
  ;; `this-command' and drives a real `display-buffer' call through
  ;; `display-buffer-alist', not just the bare predicate's return value.
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "*ewt-plain-file-open*"))
          (this-command 'find-file))
      (unwind-protect
          (should-not (window-parameter (display-buffer buf) 'window-side))
        (kill-buffer buf)))))

(defun edmacs-windows-test--with-stack-window-selected (thunk)
  "Select a fresh right-column stack window, then call THUNK.
Used to reproduce the allow-list's dedicated-window fallthrough: every side
window is dedicated to `side', so an action relying on the selected window
(e.g. `display-buffer-same-window') fails whenever a stack window --
reachable in real use via `SPC w j' -- is selected when the command runs."
  (delete-other-windows)
  (let ((stack-buf (generate-new-buffer "*ewt-stack-selected*")))
    (unwind-protect
        (let ((stack-win (display-buffer stack-buf)))
          (select-window stack-win)
          (funcall thunk))
      (kill-buffer stack-buf))))

(ert-deftest edmacs-windows-test-this-command-gated-file-open-stays-center-from-stack-window ()
  (save-window-excursion
    (edmacs-windows-test--with-stack-window-selected
     (lambda ()
       (let ((buf (generate-new-buffer "*ewt-plain-file-open-from-stack*"))
             (this-command 'find-file))
         (unwind-protect
             (should-not (window-parameter (display-buffer buf) 'window-side))
           (kill-buffer buf)))))))

(ert-deftest edmacs-windows-test-dired-mode-buffer-stays-center-from-stack-window ()
  (save-window-excursion
    (edmacs-windows-test--with-stack-window-selected
     (lambda ()
       (let ((buf (generate-new-buffer "*ewt-dired-stand-in-from-stack*")))
         (unwind-protect
             (progn
               (with-current-buffer buf (setq major-mode 'dired-mode))
               (should-not (window-parameter (display-buffer buf) 'window-side)))
           (kill-buffer buf)))))))

(ert-deftest edmacs-windows-test-magit-status-mode-buffer-stays-center-from-stack-window ()
  (save-window-excursion
    (edmacs-windows-test--with-stack-window-selected
     (lambda ()
       (let ((buf (generate-new-buffer "*ewt-magit-status-stand-in-from-stack*")))
         (unwind-protect
             (progn
               (with-current-buffer buf (setq major-mode 'magit-status-mode))
               (should-not (window-parameter (display-buffer buf) 'window-side)))
           (kill-buffer buf)))))))

(ert-deftest edmacs-windows-test-shell-buffer-gets-fixed-slot-and-resists-eviction ()
  (save-window-excursion
    (delete-other-windows)
    (let ((shell-buf (edmacs-windows-test--fresh-named-buffer "*shell*"))
          (popup-buf (generate-new-buffer "*ewt-generic-popup*")))
      (unwind-protect
          (let ((shell-win (display-buffer shell-buf)))
            (should (equal (window-parameter shell-win 'window-slot) -2))
            (display-buffer popup-buf)
            (should (window-live-p shell-win))
            (should (eq (window-buffer shell-win) shell-buf)))
        (kill-buffer popup-buf)
        (when (buffer-live-p shell-buf) (kill-buffer shell-buf))))))

(ert-deftest edmacs-windows-test-pin-skips-occupied-fixed-slot ()
  (save-window-excursion
    (delete-other-windows)
    (let ((fixed-buf (generate-new-buffer "*ewt-fixed-minus-2*"))
          (popup-buf (edmacs-windows-test--fresh-named-buffer "*Warnings*"))
          (edmacs-stack--next-pin-slot -2))
      (unwind-protect
          (progn
            ;; Simulate the cider/*shell* fixed placement at slot -2 out of
            ;; band, without going through display-buffer-alist.
            (display-buffer-in-side-window fixed-buf (edmacs-stack--popup-alist -2))
            (let ((popup-win (display-buffer popup-buf)))
              (should (equal (window-parameter popup-win 'window-slot) -1))
              (edmacs-stack-pin popup-win)
              (let ((pinned (get-buffer-window popup-buf t)))
                (should (equal (window-parameter pinned 'window-slot) -3)))))
        (dolist (b (list fixed-buf popup-buf))
          (when (buffer-live-p b) (kill-buffer b)))))))

;;; windows-test.el ends here
