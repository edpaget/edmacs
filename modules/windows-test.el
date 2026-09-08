;;; windows-test.el --- Tests for windows.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function/window-parameter coverage -- no display, no theme, no
;; live subprocess.
;;
;; Run with:
;;   scripts/run-ert-suite.sh 30 emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/git-common-dir.el -l modules/claude-term.el \
;;         -l modules/workspaces.el -l modules/windows.el \
;;         -l modules/windows-test.el -f edmacs-test-support-run-and-exit
;;
;; `modules/workspaces.el' is on that line because the AC4 load-order test
;; below loads `modules/sidebar.el' for real, leaving sidebar.el's
;; `tab-bar-tab-post-open-functions' entry installed for the rest of the
;; run -- and its redraw calls `edmacs-workspaces-groups'. Without
;; workspaces.el the 13 tests that drive a real `tab-bar-new-tab' after
;; that point fail with a void-function rather than skipping; see
;; .claude/CLAUDE.md on a header invocation not being the complete one by
;; construction.
;;
;; The 30s budget is generous margin over the ~0.3s this suite actually
;; takes once native-comp-enable-subr-trampolines is disabled below (see
;; .claude/CLAUDE.md's Testing section for the 4.2s -> 282s -> ~0.3s
;; incident this wrapper exists to catch a repeat of).
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

;; `buffer-live-p', `delete-frame', `frame-parameter' and `message' are all
;; C subrs this file `cl-letf's; without this guard each redirected subr
;; makes Emacs build a native trampoline via a synchronous compiler
;; subprocess (~28s, almost entirely wall clock). See .claude/CLAUDE.md's
;; Testing section.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

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
      (edmacs-window-promote (selected-window))
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
    (edmacs-test-support-with-tabs-restored
      ;; `tab-bar-new-tab' runs `delete-other-windows' on a fresh
      ;; window configuration, but `edmacs-windows--on-tab-open' (this
      ;; phase's `tab-bar-tab-post-open-functions' hook) designates the
      ;; new tab's sole window as main immediately.
      (tab-bar-new-tab)
      (should (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                         (window-list nil 'no-minibuf)))
      (tab-bar-switch-to-prev-tab)
      (should (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                         (window-list nil 'no-minibuf))))))

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
;; evil-window-* reaches a `no-other-window' neighbour (this phase)
;; ============================================================================
;; `edmacs-windows--reach-no-other-window' is advice on the real
;; `evil-window-left'/`-right'/`-up'/`-down', so these tests need real
;; evil.el, gated behind `edmacs-windows-test--ensure-spc-w-bindings'
;; (defined further down this file) exactly like the "SPC w" section does.

(defun edmacs-windows-test--split-side (direction)
  "Return `split-window's SIDE argument for windmove DIRECTION.
`split-window' spells the vertical sides `above'/`below'; windmove (and
`evil-window-up'/`-down') spell them `up'/`down'."
  (pcase direction
    ('up 'above)
    ('down 'below)
    (_ direction)))

(defconst edmacs-windows-test--evil-window-direction-commands
  '((evil-window-left . left)
    (evil-window-right . right)
    (evil-window-up . up)
    (evil-window-down . down))
  "Each evil directional window command paired with its windmove direction.")

(ert-deftest edmacs-windows-test-evil-window-directions-reach-no-other-window ()
  "Each of the four `evil-window-*' commands reaches a lone
`no-other-window' neighbour in its own direction -- the gap this phase
closes: plain `window-in-direction' (AC1's tests, above) never reaches
one, but `C-w h'/`SPC w h'/`C-h' and their sibling directions must."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (dolist (pair edmacs-windows-test--evil-window-direction-commands)
    (save-window-excursion
      (delete-other-windows)
      (let* ((main (selected-window))
             (neighbor (split-window main nil
                                      (edmacs-windows-test--split-side (cdr pair)))))
        (set-window-parameter neighbor 'no-other-window t)
        (select-window main)
        (funcall (car pair) 1)
        (should (eq (selected-window) neighbor))))))

(ert-deftest edmacs-windows-test-other-window-still-skips-no-other-window ()
  "The advice is scoped to the four evil commands: `other-window' still
treats a frame surrounded by `no-other-window' windows as having exactly
one reachable window, unaffected by this phase."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (save-window-excursion
    (delete-other-windows)
    (let* ((main (selected-window))
           (left (split-window main nil 'left))
           (right (split-window main nil 'right))
           (up (split-window main nil 'above))
           (down (split-window main nil 'below)))
      (dolist (w (list left right up down))
        (set-window-parameter w 'no-other-window t))
      (select-window main)
      (other-window 1)
      (should (eq (selected-window) main)))))

(ert-deftest edmacs-windows-test-evil-window-left-signals-when-no-neighbour-at-all ()
  "When there is no window at all in the requested direction -- not even
a `no-other-window' one -- the advice's retry also finds nothing, so it
must re-signal windmove's original `user-error' rather than swallowing
it into a silent no-op."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (save-window-excursion
    (delete-other-windows)
    (should-error (evil-window-left 1))))

(ert-deftest edmacs-windows-test-evil-window-left-zero-count-stays-put ()
  "A zero-repetition motion (`C-w 0 h' via `evil-window-digit-argument', or
`C-u 0 C-w h') never calls windmove at all -- `(dotimes (_ 0) ...)' is a
no-op -- so it must stay on MAIN rather than being misread as \"blocked\"
and force-jumped into the `no-other-window' neighbour. The advice's retry
must gate on ORIG-FN actually signalling, not merely on the selected
window matching its starting value."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (save-window-excursion
    (delete-other-windows)
    (let* ((main (selected-window))
           (neighbor (split-window main nil 'left)))
      (set-window-parameter neighbor 'no-other-window t)
      (select-window main)
      (evil-window-left 0)
      (should (eq (selected-window) main)))))

(ert-deftest edmacs-windows-test-evil-window-left-reaches-no-other-window-after-state-round-trip ()
  "The reachability fix survives a `window-state-get'/`window-state-put'
round trip -- the shape a daemon restart or `desktop.el' actually
produces (per phase 2's audit note), not just windows built directly by
`split-window' within one test. `window-state-put' creates fresh window
objects, so the restored left window is re-located by its parameters
\(mirroring `edmacs-windows-test-main-survives-state-get-put', above),
never by holding onto the pre-restore object."
  (unless (edmacs-windows-test--ensure-spc-w-bindings)
    (ert-skip "real evil.el/general.el not found in this checkout or its sibling main checkout; bootstrap straight once locally to enable this test"))
  (save-window-excursion
    (delete-other-windows)
    (let* ((main (selected-window))
           (left (split-window main nil 'left)))
      (set-window-parameter left 'no-other-window t)
      (set-window-parameter left 'window-side 'left)
      (select-window main)
      (let ((state (window-state-get (frame-root-window) t)))
        (delete-other-windows)
        (window-state-put state (frame-root-window))
        (let ((restored-left (seq-find (lambda (w)
                                          (and (window-parameter w 'no-other-window)
                                               (eq (window-parameter w 'window-side) 'left)))
                                        (window-list nil 'no-minibuf)))
              (restored-main (seq-find (lambda (w) (not (window-parameter w 'no-other-window)))
                                        (window-list nil 'no-minibuf))))
          (should restored-left)
          (should restored-main)
          (select-window restored-main)
          (evil-window-left 1)
          (should (eq (selected-window) restored-left)))))))

;; ============================================================================
;; AC4 -- load order: windows.el then sidebar.el leaves window-sides-slots
;; at (1 nil nil nil) -- sidebar's LEFT cap intact, this module's RIGHT nil
;; ============================================================================
;; Duplicated (not required) from sidebar-test.el, to keep each *-test.el
;; file's own Commentary invocation self-contained -- see that file's
;; Commentary for why `magit-section' needs a `load-path' fixup under bare
;; `-Q' and why a broken/never-bootstrapped straight build must skip
;; cleanly rather than error the whole suite out.

(defvar edmacs-windows-test--build-root
  (edmacs-test-support-straight-build-root)
  "This checkout's (or its sibling main checkout's) `straight/build' root.")

(defconst edmacs-windows-test--self-file (or load-file-name buffer-file-name))

(if (null edmacs-windows-test--build-root)

    (ert-deftest edmacs-windows-test-window-sides-slots-load-order-unavailable ()
      (edmacs-test-support-report-suite-unavailable
       edmacs-windows-test--self-file
       "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this test"))

  (progn

    (edmacs-test-support-add-magit-section-deps edmacs-windows-test--build-root)
    (load (expand-file-name "modules/sidebar.el" default-directory) nil t)

    (ert-deftest edmacs-windows-test-window-sides-slots-load-order ()
      (should (equal window-sides-slots '(1 nil nil nil)))
      ;; Both edges arrive through the claim API, each naming its owner.
      (should (equal (assq 'left edmacs-windows--side-claims) '(left 1 . sidebar)))
      (should (equal (assq 'right edmacs-windows--side-claims) '(right nil . windows))))))

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

(defun edmacs-windows-test--stack-popup (buffer &optional slot)
  "Display BUFFER directly as a stack popup pane, bypassing the main rule.
Every buffer now goes to MAIN by default, so a test whose SUBJECT is the
stack itself (pin, `quit-restore' behaviour, width) has to build its pane
explicitly. A caller-supplied ACTION outranks `display-buffer-base-action',
and this uses the same alist the push path does, so the pane carries the
real `edmacs-stack-popup' parameter rather than a hand-rolled imitation."
  (display-buffer buffer (edmacs-stack--popup-alist slot)))

(defconst edmacs-windows-test--popup-names
  '("*Warnings*" "*Messages*" "*Help*" "*helpful variable: foo*"
    "*compilation*" "*quickrun*" "*Flycheck errors*" "*Backtrace*"
    "*Occur*" "*grep*" "*xref*" "*magit-diff: edmacs*" "*magit-log: edmacs*"
    "*lsp-help*" "*Embark Collect Live*")
  "One representative buffer name per routed `display-buffer-alist' pattern.")

;; ---------------------------------------------------------------------------
;; AC1 -- every buffer lands in MAIN, whatever it is called
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-every-buffer-lands-in-main ()
  "No buffer name is special. Each of these once had its own routing to
the stack; all of them now take MAIN, which is the whole point of the
uniform rule."
  (dolist (name edmacs-windows-test--popup-names)
    (save-window-excursion
      (delete-other-windows)
      (edmacs-window-set-main (selected-window))
      (let* ((real-messages (equal name "*Messages*"))
             (buf (if real-messages (get-buffer name)
                    (edmacs-windows-test--fresh-named-buffer name))))
        (unwind-protect
            (let ((win (display-buffer buf)))
              (should win)
              (should (eq win (edmacs-main-window)))
              (should (eq (window-buffer win) buf))
              (should-not (window-parameter win 'window-side)))
          (unless real-messages (kill-buffer buf)))))))

;; ---------------------------------------------------------------------------
;; AC2 -- a second buffer pushes the first onto the top of the stack
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-second-buffer-pushes-first-to-stack ()
  "Displaying B after A puts B in main and A on the stack -- they do not
share one pane, which is what the old shared popup slot did."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((buf-a (generate-new-buffer "*ewt-push-a*"))
          (buf-b (generate-new-buffer "*ewt-push-b*")))
      (unwind-protect
          (let ((win-a (display-buffer buf-a)))
            (should (eq win-a (edmacs-main-window)))
            (let ((win-b (display-buffer buf-b)))
              (should (eq win-b (edmacs-main-window)))
              (should (eq (window-buffer (edmacs-main-window)) buf-b))
              ;; A is still visible, in the stack rather than in main.
              (let ((win-for-a (get-buffer-window buf-a)))
                (should win-for-a)
                (should (eq (window-parameter win-for-a 'window-side) 'right)))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))

(ert-deftest edmacs-windows-test-stack-order-is-most-recent-first ()
  "Each displacement takes a slot above the one before it, so the stack
reads newest-at-top. Slots run more-negative-upward."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((bufs (mapcar (lambda (n) (generate-new-buffer (format "*ewt-order-%d*" n)))
                        '(0 1 2))))
      (unwind-protect
          (let ((edmacs-stack--next-pin-slot -2))
            (dolist (b bufs) (display-buffer b))
            (let* ((windows (edmacs-stack-windows))
                   (slots (mapcar (lambda (w) (window-parameter w 'window-slot)) windows)))
              ;; Ascending slot order == top-to-bottom, and the most recently
              ;; displaced buffer is the topmost.
              (should (equal slots (sort (copy-sequence slots) #'<)))
              (should (eq (window-buffer (car windows)) (nth 1 bufs)))))
        (dolist (b bufs) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest edmacs-windows-test-redisplaying-main-pushes-nothing ()
  "Displaying the buffer main already shows must not churn the stack."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((buf (generate-new-buffer "*ewt-noop*")))
      (unwind-protect
          (progn
            (display-buffer buf)
            (let ((before (length (edmacs-stack-windows))))
              (display-buffer buf)
              (should (eq (window-buffer (edmacs-main-window)) buf))
              (should (= before (length (edmacs-stack-windows))))))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-revisiting-a-stacked-buffer-swaps-not-duplicates ()
  "A buffer already in the stack is swapped into main rather than shown
in two windows at once."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((buf-a (generate-new-buffer "*ewt-swap-a*"))
          (buf-b (generate-new-buffer "*ewt-swap-b*")))
      (unwind-protect
          (progn
            (display-buffer buf-a)
            (display-buffer buf-b)
            (display-buffer buf-a)
            (should (eq (window-buffer (edmacs-main-window)) buf-a))
            (should (= 1 (length (get-buffer-window-list buf-a nil t))))
            (should (get-buffer-window buf-b)))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))

(ert-deftest edmacs-windows-test-stack-is-capped-and-evicts-the-bottom ()
  "`edmacs-stack-max-windows' bounds the column; the bottom (oldest) pane
goes, and its buffer stays live."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((bufs (mapcar (lambda (n) (generate-new-buffer (format "*ewt-cap-%d*" n)))
                        '(0 1 2 3 4)))
          (edmacs-stack-max-windows 2))
      (unwind-protect
          (let ((edmacs-stack--next-pin-slot -2))
            (dolist (b bufs) (display-buffer b))
            (should (= 2 (length (edmacs-stack-windows))))
            ;; The first buffer displaced is the first evicted, still alive.
            (should (buffer-live-p (car bufs)))
            (should-not (get-buffer-window (car bufs))))
        (dolist (b bufs) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest edmacs-windows-test-stack-cap-nil-means-unbounded ()
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((bufs (mapcar (lambda (n) (generate-new-buffer (format "*ewt-uncap-%d*" n)))
                        '(0 1 2 3)))
          (edmacs-stack-max-windows nil))
      (unwind-protect
          (let ((edmacs-stack--next-pin-slot -2))
            (dolist (b bufs) (display-buffer b))
            ;; Four displays, each displacing what main held (including the
            ;; buffer it started on), and nothing evicted.
            (should (= 4 (length (edmacs-stack-windows)))))
        (dolist (b bufs) (when (buffer-live-p b) (kill-buffer b)))))))

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
    ;; Every displaced buffer now allocates from this counter, so it has
    ;; drifted far past -2 by the time this test runs; pin the start.
    (let ((edmacs-stack--next-pin-slot -2)
          bufs)
      (unwind-protect
          (progn
            (dotimes (i 3)
              (let ((b (generate-new-buffer (format "*ewt-agent-%d*" i))))
                (push b bufs)
                (edmacs-windows-test--display-agent-pane b i)))
            (let* ((popup-buf (edmacs-windows-test--fresh-named-buffer "*Warnings*"))
                   (popup-win (edmacs-windows-test--stack-popup popup-buf)))
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
                     (help-win (edmacs-windows-test--stack-popup help-buf)))
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
          (let ((win (edmacs-windows-test--stack-popup buf)))
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
            (edmacs-windows-test--stack-popup buf1)
            (let ((win2 (edmacs-windows-test--stack-popup buf2)))
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
      (let ((win (edmacs-windows-test--stack-popup buf)))
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
          (let ((win (edmacs-windows-test--stack-popup buf)))
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

(ert-deftest edmacs-windows-test-balance-center-leaves-a-correct-stack-width-alone ()
  "A stack column already at `edmacs-stack-width' is not disturbed."
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

(ert-deftest edmacs-windows-test-balance-center-restores-a-stale-stack-width ()
  "A stack column left at an old frame's fraction is resized back.
The ultrawide case: a side window keeps the absolute width it was created
at, so a column sized for a narrow frame stays narrow after the frame
grows underneath it. Simulated here by resizing the pane directly, which
leaves the same state."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((edmacs-stack-width 0.40)
          (buf (generate-new-buffer "*ewt-stale-width*")))
      (unwind-protect
          (let* ((agent-win (edmacs-windows-test--display-agent-pane buf 0))
                 (target (round (* 0.40 (window-total-width (frame-root-window))))))
            (should (= (window-total-width agent-win) target))
            (window-resize agent-win (- (round (* 0.4 target)) target) t 'safe)
            (should (/= (window-total-width agent-win) target))
            (edmacs-stack-balance-center)
            (should (= (window-total-width agent-win) target)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-balance-center-runs-the-rebalance-hook ()
  "Members of `edmacs-windows-rebalance-functions' are called with the frame.
The extension point sidebar.el joins to re-apply the left column's width;
windows.el itself owns only the right one."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let* ((seen nil)
           (edmacs-windows-rebalance-functions
            (list (lambda (frame) (push frame seen)))))
      (edmacs-stack-balance-center)
      (should (equal seen (list (selected-frame)))))))

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

(defvar edmacs-windows-test--repos-root
  (edmacs-test-support-straight-repos-root)
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
    ("r" . edmacs-windows-repair-frame)
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
;; Persistence: tab-bar hooks, dead-pane sweep
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
    (edmacs-test-support-with-tabs-restored
      (tab-bar-new-tab)
      (should (= (edmacs-windows-test--nonside-count) 1))
      (should (window-parameter (selected-window) 'edmacs-main))
      (should (null (edmacs-stack-windows))))))

(ert-deftest edmacs-windows-test-tab-switch-does-not-flatten-a-healthy-layout ()
  "The wedge guard on `tab-bar-select-tab' must not touch a usable frame.
It runs on both sides of every tab switch, and `tab-bar-select-tab' SAVES
the outgoing tab's layout -- so a guard that collapsed the frame would have
that collapse written back into the tab, flattening every tab to a single
`*scratch*' window one switch at a time. `edmacs-windows-ensure-main-window'
is a no-op wherever a non-side window already exists, which is what makes it
safe on that path."
  (save-window-excursion
    (let ((tabs-before (length (tab-bar-tabs)))
          (buf (generate-new-buffer "ewt-keeps-layout")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (split-window)
            (set-window-buffer (next-window) buf)
            (let ((before (mapcar (lambda (w) (buffer-name (window-buffer w)))
                                  (window-list nil 'no-minibuf))))
              (should (= 2 (length before)))
              (tab-bar-new-tab)
              (tab-bar-switch-to-prev-tab)
              (should-not (edmacs-windows-frame-wedged-p (selected-frame)))
              ;; The layout came back, not a lone scratch window.
              (should (= 2 (length (window-list nil 'no-minibuf))))
              (should (member (buffer-name buf)
                              (mapcar (lambda (w) (buffer-name (window-buffer w)))
                                      (window-list nil 'no-minibuf))))))
        (while (> (length (tab-bar-tabs)) tabs-before)
          (tab-bar-close-tab))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-restoring-an-all-side-layout-wedges-the-frame ()
  "A tab saved holding only the sidebar restores the frame into the wedged
shape, silently. `window-state-put' accepts such a state, returns normally,
and leaves the frame with no main window -- from which every later
`split-window' recurses (see the sibling test). That shape is reachable
because `tab-bar-new-tab-to' deletes other windows with
`ignore-window-parameters' bound, so a tab created while the sidebar held
point keeps the sidebar as its only window.

This is the precondition the `tab-bar-select-tab' advice repairs."
  (save-window-excursion
    (let ((buf (generate-new-buffer "ewt-allside-ws")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let ((side (display-buffer-in-side-window
                         buf '((side . left) (slot . 0)))))
              (set-window-dedicated-p side t)
              (select-window side)
              (let ((ignore-window-parameters t)) (delete-other-windows)))
            (should (edmacs-windows-frame-wedged-p (selected-frame)))
            (let ((state (window-state-get (frame-root-window) t)))
              ;; Back to an ordinary frame before restoring.
              (set-window-dedicated-p (selected-window) nil)
              (set-window-parameter (selected-window) 'window-side nil)
              (set-window-parameter (selected-window) 'window-slot nil)
              (switch-to-buffer "*scratch*")
              (should-not (edmacs-windows-frame-wedged-p (selected-frame)))
              ;; Restoring the saved all-side layout succeeds and wedges it.
              (window-state-put state (frame-root-window) 'safe)
              (should (edmacs-windows-frame-wedged-p (selected-frame)))
              ;; Which is exactly what the guard undoes -- without dismantling
              ;; the side window it found there.
              (edmacs-windows-ensure-main-window (selected-frame))
              (should-not (edmacs-windows-frame-wedged-p (selected-frame)))
              (should-not (window-parameter (edmacs-main-window) 'window-side))
              (should (seq-find (lambda (w)
                                  (eq (window-parameter w 'window-side) 'left))
                                (window-list nil 'no-minibuf)))))
        (delete-other-windows)
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-repair-clears-the-split-window-self-delegation ()
  "A frame whose only window is a side window makes `split-window' recurse.
Core delegates a root-window split to `window-main-window' whenever the
frame owns any side window; on this shape that call returns the very window
being split, so `split-window' delegates to itself until
`max-lisp-eval-depth' blows. Captured live as
`walk-window-tree-1: Lisp nesting exceeds max-lisp-eval-depth' with 1567
identical `split-window(#<window N on *sidebar*> 2 right nil nil)' frames,
reached from `tab-bar-select-tab' -> `window-state-put'.

`edmacs-windows-repair-frame' removes the precondition: this asserts the
recursion is real on the wedged shape, and gone once repaired. The path
from `tab-bar-select-tab' is not reproducible under `--batch' -- that call
collapses the frame itself before restoring -- so the mechanism is pinned
here directly rather than through a driver that cannot fail."
  (save-window-excursion
    (let ((buf (generate-new-buffer "ewt-wedge-split")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let ((side (display-buffer-in-side-window
                         buf '((side . left) (slot . 0)))))
              (set-window-dedicated-p side t)
              (select-window side)
              (let ((ignore-window-parameters t)) (delete-other-windows))
              ;; The precondition, asserted rather than assumed.
              (should (edmacs-windows-frame-wedged-p (selected-frame)))
              (should (eq (window-main-window) (selected-window)))
              ;; Bounded so the recursion errors promptly instead of running
              ;; to the default depth.
              (should (eq 'recursed
                          (let ((max-lisp-eval-depth 200))
                            (condition-case nil
                                (progn (split-window (frame-root-window) 2 t) nil)
                              (error 'recursed)))))
              (edmacs-windows-ensure-main-window (selected-frame))
              (should-not (edmacs-windows-frame-wedged-p (selected-frame)))
              (should-not (window-parameter (edmacs-main-window) 'window-side))
              (should (window-live-p (split-window (frame-root-window) 2 t)))))
        (delete-other-windows)
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-new-tab-from-a-side-window-keeps-a-real-main ()
  "A tab created while a side window is selected still gets a usable main.
`tab-bar-new-tab-to' binds `ignore-window-parameters' and
`window--sides-inhibit-check' around its `delete-other-windows', so the
SELECTED window survives whatever it is -- side parameters and
`no-delete-other-windows' included. Creating a tab from the sidebar
therefore left the new tab with a dedicated side window as its only
window, `edmacs-main-window' returning it, `display-buffer' able to add
nothing but more side windows, and `walk-window-tree' eventually
exceeding `max-lisp-eval-depth'."
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "ewt-side-newtab")))
      (unwind-protect
          (edmacs-test-support-with-tabs-restored
            (let ((side (display-buffer-in-side-window
                         buf '((side . left) (slot . 0)
                               (window-parameters
                                . ((no-delete-other-windows . t)))))))
              (set-window-dedicated-p side t)
              (select-window side)
              (tab-bar-new-tab)
              (should (>= (edmacs-windows-test--nonside-count) 1))
              (let ((main (edmacs-main-window)))
                (should main)
                (should-not (window-parameter main 'window-side))
                (should-not (window-dedicated-p main)))))
        (kill-buffer buf)))))

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
    (let* ((main1-buf (generate-new-buffer "ewt-persist-main-1"))
           (pane1a (generate-new-buffer "ewt-persist-pane-1a"))
           (pane1b (generate-new-buffer "ewt-persist-pane-1b"))
           (main2-buf (generate-new-buffer "ewt-persist-main-2"))
           (pane2a (generate-new-buffer "ewt-persist-pane-2a")))
      (unwind-protect
          (edmacs-test-support-with-tabs-restored
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
              (edmacs-stack-sweep-stale-panes (selected-frame)))
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
            (edmacs-stack-sweep-stale-panes (selected-frame))
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
            (edmacs-stack-sweep-stale-panes (selected-frame))
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
            (edmacs-stack-sweep-stale-panes (selected-frame))
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
    (edmacs-stack-sweep-stale-panes (selected-frame))
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

(ert-deftest edmacs-windows-test-fallback-routes-any-buffer-to-main ()
  "A buffer nothing knows anything about lands in main, like every other."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((buf (generate-new-buffer "*ewt-anything*")))
      (unwind-protect
          (let ((win (display-buffer buf)))
            (should (eq win (edmacs-main-window)))
            (should (eq (window-buffer win) buf)))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-other-window-shape-still-lands-in-main ()
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
            ;; `inhibit-same-window' cannot apply: main is the destination for
            ;; everything, and it is reached by identity rather than by being
            ;; the selected window.
            (should (eq win (edmacs-main-window))))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-switch-to-buffer-obeys-display-actions ()
  "Switching to a buffer must place it exactly like opening one, which
means routing through `display-buffer-base-action'. Emacs defaults this
to nil, which swaps main's buffer in place and drops the displaced one
out of view entirely."
  (should switch-to-buffer-obey-display-actions))

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

(ert-deftest edmacs-windows-test-base-action-order-recovers-before-side-windows ()
  "Order is load-bearing, not incidental: `display-buffer-in-side-window'
always succeeds, so any recover action placed after it is dead code and
the wedge comes straight back."
  (should (equal (car display-buffer-base-action)
                 (list #'edmacs-windows--display-in-main
                       #'edmacs-windows--display-buffer-in-recovered-main
                       #'display-buffer-in-side-window))))

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

(ert-deftest edmacs-windows-test-pin-skips-an-occupied-slot ()
  (save-window-excursion
    (delete-other-windows)
    (let ((fixed-buf (generate-new-buffer "*ewt-fixed-minus-2*"))
          (popup-buf (edmacs-windows-test--fresh-named-buffer "*Warnings*"))
          (edmacs-stack--next-pin-slot -2))
      (unwind-protect
          (progn
            ;; Occupy slot -2 directly, so the pin counter must step past it.
            (display-buffer-in-side-window fixed-buf (edmacs-stack--popup-alist -2))
            (let ((popup-win (edmacs-windows-test--stack-popup popup-buf)))
              (should (equal (window-parameter popup-win 'window-slot) -1))
              (edmacs-stack-pin popup-win)
              (let ((pinned (get-buffer-window popup-buf t)))
                (should (equal (window-parameter pinned 'window-slot) -3)))))
        (dolist (b (list fixed-buf popup-buf))
          (when (buffer-live-p b) (kill-buffer b)))))))

;; ---------------------------------------------------------------------------
;; edmacs-quit-window-or-buffer -- `:q' closes a buffer, never the frame
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-quit-deletes-window-when-split ()
  "With a center split `:q' behaves as vim does -- it closes the selected
window and leaves the buffer alone."
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "ewt-quit-split")))
      (unwind-protect
          (let ((other (split-window-right)))
            (set-window-buffer other buf)
            (select-window other)
            (should (edmacs--center-split-p))
            (edmacs-quit-window-or-buffer)
            (should-not (window-live-p other))
            ;; The window went; the buffer did not.
            (should (buffer-live-p buf)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest edmacs-windows-test-quit-kills-buffer-in-last-window ()
  "The case that sent `evil-quit' to `delete-frame': one ordinary window
left, so there is no window to close. The buffer is killed and the
window keeps its slot rather than the frame going away."
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "ewt-quit-last"))
          (frames (length (frame-list))))
      (unwind-protect
          (progn
            (switch-to-buffer buf)
            (should-not (edmacs--center-split-p))
            (edmacs-quit-window-or-buffer)
            (should-not (buffer-live-p buf))
            (should (window-live-p (selected-window)))
            (should (= (length (frame-list)) frames)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest edmacs-windows-test-quit-never-deletes-a-frame ()
  "Whatever branch it takes, `:q' must not reach `delete-frame' -- that is
what drops the daemon out of the Dock."
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "ewt-quit-no-delete-frame"))
          (deleted 0))
      (unwind-protect
          (cl-letf (((symbol-function 'delete-frame)
                     (lambda (&rest _) (setq deleted (1+ deleted)))))
            (switch-to-buffer buf)
            (edmacs-quit-window-or-buffer)
            (should (= deleted 0)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest edmacs-windows-test-quit-closes-a-side-window ()
  "A sidebar is not an ordinary window and does not count toward the split
test, so `:q' in one closes that window outright."
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "ewt-quit-side")))
      (unwind-protect
          (let ((side (display-buffer-in-side-window
                       buf '((side . right) (slot . 0)))))
            (select-window side)
            (should (window-parameter side 'window-side))
            (edmacs-quit-window-or-buffer)
            (should-not (window-live-p side))
            (should (buffer-live-p buf)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest edmacs-windows-test-quit-force-discards-unsaved-changes ()
  "`:q!' drops modifications rather than prompting, and only on the branch
that actually kills the buffer."
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "ewt-quit-force")))
      (unwind-protect
          (progn
            (switch-to-buffer buf)
            (with-current-buffer buf (insert "unsaved") (should (buffer-modified-p)))
            (edmacs-quit-window-or-buffer t)
            (should-not (buffer-live-p buf)))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))))))

(ert-deftest edmacs-windows-test-quit-finishes-a-waiting-emacsclient ()
  "A buffer a blocking `emacsclient FILE' waits on is finished, not killed
-- that is what releases the client."
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (generate-new-buffer "ewt-quit-server"))
          (edited 0))
      (unwind-protect
          (cl-letf (((symbol-function 'server-edit)
                     (lambda (&rest _) (setq edited (1+ edited))))
                    ((symbol-function 'server-buffer-done) #'ignore))
            (switch-to-buffer buf)
            (with-current-buffer buf (setq-local server-buffer-clients '(t)))
            (edmacs-quit-window-or-buffer)
            (should (= edited 1))
            (should (buffer-live-p buf)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;; ============================================================================
;; Phase 2 AC1/AC2 -- the placement registry
;; ============================================================================

(defmacro edmacs-windows-test--with-scratch-registry (&rest body)
  "Run BODY with the placement registry and its alist entries let-bound.
Every declaration BODY makes is discarded on exit, so a throwaway
placement can never leak into the live registry the AC2 sweep checks."
  (declare (indent 0))
  `(let ((display-buffer-alist display-buffer-alist)
         (edmacs-windows--placements edmacs-windows--placements)
         (edmacs-windows--owned-alist-entries edmacs-windows--owned-alist-entries))
     ,@body))

(defconst edmacs-windows-test--windows-el-placements
  '()
  "The placements windows.el itself declares, in declaration order.
git.el adds `magit-diff-log' eagerly, for eleven after a real init;
vterm.el's `vterm' and languages/clojure.el's `cider-repl' are declared
from a deferred `use-package' `:config', so they join once their package
actually loads, for thirteen.")

(ert-deftest edmacs-windows-test-registry-declares-no-placements ()
  "Nothing declares through `edmacs-windows-place'. A placement here would
reintroduce the per-class unpredictability the uniform rule removed, so
this asserts the registry stays empty rather than listing what is in it."
  (should (equal (mapcar #'car (edmacs-windows-placements))
                 edmacs-windows-test--windows-el-placements)))

(ert-deftest edmacs-windows-test-place-registers-every-role ()
  "Each role resolves to its own destination, with the real
`display-buffer-base-action' in force -- so `ordinary' proves it beats
the side-window fallback rather than merely never meeting it."
  (edmacs-windows-test--with-scratch-registry
    (let ((split-height-threshold 0)
          (split-width-threshold nil))
      (dolist (case '((main "ewt-role-main" nil nil)
                      (stack-fixed "ewt-role-fixed" right -2)
                      (ordinary "ewt-role-ordinary" nil nil)
                      (bottom "ewt-role-bottom" nil nil)))
        (pcase-let ((`(,role ,name ,side ,slot) case))
          (save-window-excursion
            (delete-other-windows)
            (let ((buf (edmacs-windows-test--fresh-named-buffer name)))
              (unwind-protect
                  (progn
                    (apply #'edmacs-windows-place 'ewt-role
                           :match (concat "\\`" (regexp-quote name) "\\'")
                           :as role
                           (when (eq role 'stack-fixed) '(:slot -2)))
                    (let ((win (display-buffer buf)))
                      (should (window-live-p win))
                      (should (eq (window-parameter win 'window-side) side))
                      (should (equal (window-parameter win 'window-slot) slot))
                      (when (eq role 'main)
                        (should (eq win (edmacs-main-window))))
                      (when (eq role 'ordinary)
                        (should-not (window-dedicated-p win))
                        (should-not (window-parameter win 'no-other-window)))
                      (when (eq role 'bottom)
                        (should (= (nth 3 (window-edges win))
                                   (nth 3 (window-edges (frame-root-window))))))))
                (kill-buffer buf)))))))))

(ert-deftest edmacs-windows-test-place-stack-role-lands-on-slot-minus-1 ()
  (edmacs-windows-test--with-scratch-registry
    (save-window-excursion
      (delete-other-windows)
      (let ((buf (edmacs-windows-test--fresh-named-buffer "ewt-role-stack")))
        (unwind-protect
            (progn
              (edmacs-windows-place 'ewt-role-stack
                :match "\\`ewt-role-stack\\'" :as 'stack
                :params '((mode-line-format . none)))
              (let ((win (display-buffer buf)))
                (should (eq (window-parameter win 'window-side) 'right))
                (should (equal (window-parameter win 'window-slot) -1))
                (should (eq (window-parameter win 'mode-line-format) 'none))))
          (kill-buffer buf))))))

(ert-deftest edmacs-windows-test-place-replaces-a-redeclared-name ()
  "Re-loading a module must not double its entries."
  (edmacs-windows-test--with-scratch-registry
    (let ((before (length (edmacs-windows-placements))))
      (edmacs-windows-place 'ewt-dup :match "\\`ewt-dup\\'" :as 'ordinary)
      (should (= (1+ before) (length (edmacs-windows-placements))))
      (edmacs-windows-place 'ewt-dup :match "\\`ewt-dup\\'" :as 'bottom)
      (should (= (1+ before) (length (edmacs-windows-placements))))
      (should (= (length (edmacs-windows-placements))
                 (length edmacs-windows--owned-alist-entries)))
      (should (eq 'bottom (plist-get (alist-get 'ewt-dup (edmacs-windows-placements)) :as))))))

(ert-deftest edmacs-windows-test-place-validates-its-spec ()
  (edmacs-windows-test--with-scratch-registry
    (should-error (edmacs-windows-place 'ewt-bad :match "x" :as 'sideways))
    (should-error (edmacs-windows-place 'ewt-bad :as 'ordinary))
    (should-error (edmacs-windows-place 'ewt-bad :match "x" :as 'stack :slot -3))
    (should-error (edmacs-windows-place 'ewt-bad :match "x" :as 'ordinary :height 0.3))))

;; ---------------------------------------------------------------------------
;; AC2 -- nothing registered merely restates the default
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-no-placement-duplicates-the-default ()
  (should-not (seq-find #'edmacs-windows--redundant-p (edmacs-windows-placements))))

(ert-deftest edmacs-windows-test-place-signals-on-a-redundant-declaration ()
  (edmacs-windows-test--with-scratch-registry
    (should-error (edmacs-windows-place 'ewt-redundant :match "\\`\\*ewt-x\\*\\'" :as 'stack))
    (should (edmacs-windows-place 'ewt-redundant :match "\\`\\*ewt-x\\*\\'"
                                  :as 'stack :override t))))

;; ============================================================================
;; Phase 2 AC3 -- window-sides-slots is claimed by edge name, by one writer
;; ============================================================================

(ert-deftest edmacs-windows-test-claim-side-leaves-other-edges-untouched ()
  (dolist (case '((left 0 (1 8 7 6)) (top 1 (9 1 7 6))
                  (right 2 (9 8 1 6)) (bottom 3 (9 8 7 1))))
    (pcase-let ((`(,edge ,_index ,expected) case))
      (let ((window-sides-slots '(9 8 7 6))
            (edmacs-windows--side-claims nil))
        (edmacs-windows-claim-side edge 1 'ewt-a)
        (should (equal window-sides-slots expected))))))

(ert-deftest edmacs-windows-test-claim-side-conflict-signals ()
  (let ((window-sides-slots '(9 8 7 6))
        (edmacs-windows--side-claims nil))
    (edmacs-windows-claim-side 'left 1 'ewt-a)
    ;; A second claimant wanting a different value is a hard error.
    (should-error (edmacs-windows-claim-side 'left 2 'ewt-b))
    ;; The same claimant may change its mind.
    (edmacs-windows-claim-side 'left 3 'ewt-a)
    (should (equal window-sides-slots '(3 8 7 6)))
    ;; A duplicate claim at the standing value is a harmless no-op.
    (edmacs-windows-claim-side 'left 3 'ewt-b)
    (should (equal window-sides-slots '(3 8 7 6)))
    (should-error (edmacs-windows-claim-side 'sideways 1 'ewt-a))))

(ert-deftest edmacs-windows-test-window-sides-slots-has-one-writer ()
  "A third writer added later fails here rather than silently winning."
  (let ((hits 0))
    (dolist (file '("windows.el" "sidebar.el" "vterm.el" "git.el"
                    "claude-term.el" "claude-term-registry.el" "sessions.el"))
      (let ((path (expand-file-name (concat "modules/" file) default-directory)))
        (when (file-readable-p path)
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (while (re-search-forward "(setq[q]?-default?\\s-+window-sides-slots\\|(setq\\s-+window-sides-slots" nil t)
              (setq hits (1+ hits))
              (should (equal file "windows.el"))
              ;; ...and inside `edmacs-windows-claim-side', not beside it.
              (let ((here (point)))
                (should (re-search-backward "^(defun \\([^ ]+\\)" nil t))
                (should (equal (match-string 1) "edmacs-windows-claim-side"))
                (goto-char here)))))))
    (should (= hits 1))))

;; ============================================================================
;; Phase 2 AC4 -- every layout window parameter survives a real
;; window-state-get/window-state-put round trip
;; ============================================================================

(defun edmacs-windows-test--layout-round-trip ()
  "Build a laid-out frame, round-trip it through `window-state-get'/`-put'.
Uses the exact calls a frameset restore makes (`frameset.el's
WRITABLE-non-nil get and `safe' put), not a `window-configuration' --
`tab-bar-select-tab' only reaches this path after a saved session is
restored, which is where the parameter loss actually bit."
  (delete-other-windows)
  (edmacs-window-set-main (selected-window))
  (display-buffer-in-side-window
   (edmacs-windows-test--fresh-named-buffer "ewt-rt-sidebar")
   '((side . left) (slot . 0)
     (window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))))
  ;; Real stack panes: the subject is whether their parameters survive the
  ;; round trip, so they must be built as panes rather than sent to main.
  (edmacs-windows-test--stack-popup
   (edmacs-windows-test--fresh-named-buffer "*Warnings*") -3)
  (edmacs-windows-test--stack-popup
   (edmacs-windows-test--fresh-named-buffer "*Embark Collect Live*") -1)
  (let ((state (window-state-get (frame-root-window) t)))
    (delete-other-windows)
    (window-state-put state (frame-root-window) 'safe)
    state))

(defun edmacs-windows-test--window-showing (name)
  "Return the selected frame's window showing the buffer named NAME."
  (seq-find (lambda (w) (equal (buffer-name (window-buffer w)) name))
            (window-list nil 'no-minibuf)))

(ert-deftest edmacs-windows-test-layout-parameters-survive-a-state-round-trip ()
  (save-window-excursion
    (unwind-protect
        (progn
          (edmacs-windows-test--layout-round-trip)
          (let ((sidebar (edmacs-windows-test--window-showing "ewt-rt-sidebar"))
                (popup (edmacs-windows-test--window-showing "*Embark Collect Live*")))
            (should sidebar)
            (should (eq (window-parameter sidebar 'window-side) 'left))
            (should (equal (window-parameter sidebar 'window-slot) 0))
            (should (window-parameter sidebar 'no-other-window))
            (should (window-parameter sidebar 'no-delete-other-windows))
            (should popup)
            (should (eq (window-parameter popup 'window-side) 'right))
            (should (equal (window-parameter popup 'window-slot) -1))
            (should (window-parameter popup 'edmacs-stack-popup))
            (should (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                              (window-list nil 'no-minibuf)))))
      (dolist (name '("ewt-rt-sidebar" "*Embark Collect Live*"))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest edmacs-windows-test-state-get-output-stays-printable ()
  "Registering a parameter puts its value into the desktop file, which
desktop.el writes with `prin1' -- so the whole owned set must read back."
  (save-window-excursion
    (unwind-protect
        (let ((state (edmacs-windows-test--layout-round-trip)))
          (should (read (prin1-to-string state))))
      (dolist (name '("ewt-rt-sidebar" "*Embark Collect Live*"))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest edmacs-windows-test-owned-layout-parameters-are-registered ()
  (dolist (parameter '(edmacs-main no-other-window no-delete-other-windows
                       edmacs-stack-popup mode-line-format window-side window-slot))
    (should (eq 'writable (alist-get parameter window-persistent-parameters))))
  ;; `window-preserved-size's value carries a live buffer object, which must
  ;; never reach the printed desktop file.
  (should-not (assq 'window-preserved-size window-persistent-parameters))
  ;; `quit-restore's value embeds live window/buffer/marker objects for the
  ;; same reason -- see the comment above the `dolist' registering these
  ;; parameters. sidebar.el's `edmacs-sidebar--enforce-width' compensates at
  ;; the point of use instead of persisting this parameter.
  (should-not (assq 'quit-restore window-persistent-parameters)))

(ert-deftest edmacs-windows-test-quit-restore-advice-still-fires-after-a-round-trip ()
  "Losing `edmacs-stack-popup' across a restore would drop `q' back to
stock `quit-restore-window', which can resurrect a stale prior buffer."
  (save-window-excursion
    (unwind-protect
        (progn
          (edmacs-windows-test--layout-round-trip)
          (let ((popup (edmacs-windows-test--window-showing "*Embark Collect Live*")))
            (should popup)
            (quit-restore-window popup)
            (should-not (window-live-p popup))
            (should (eq (selected-window) (edmacs-main-window)))))
      (dolist (name '("ewt-rt-sidebar" "*Embark Collect Live*"))
        (when (get-buffer name) (kill-buffer name))))))


;; ============================================================================
;; Shape repair -- a frame with nothing but side windows
;; ============================================================================
;; The batch frame is effectively 80x25 for splitting purposes no matter
;; what `set-frame-width' reports, so a sized `(split-window w 40 'right)'
;; signals "Window ... too small for splitting". The three wedged shapes
;; below all come from `edmacs-test-support-make-wedged-frame' (nil,
;; `dedicated', `sole'), which uses sizeless splits only.

(ert-deftest edmacs-windows-test-core-sides-check-passes-a-mainless-frame ()
  "Pins why an edmacs-side guard is needed at all: core reads a frame of
nothing but side windows as a VALID side-window configuration.
`window-main-window' falls back to `frame-root-window' rather than
returning nil, so `window--sides-check-failed's own \"no main window\"
branch is unreachable and `window--sides-check' resets nothing --
while `edmacs-main-window', which only counts leaf non-side windows,
correctly reports nil."
  (save-window-excursion
    (cl-destructuring-bind (left right) (edmacs-test-support-make-wedged-frame)
      (should-not (window--sides-check-failed (selected-frame)))
      (window--sides-check (selected-frame))
      (should (eq (window-parameter left 'window-side) 'left))
      (should (eq (window-parameter right 'window-side) 'right))
      (should (eq (window-main-window) (frame-root-window)))
      (should (window-main-window))
      (should-not (edmacs-main-window))
      (should (edmacs-windows-frame-wedged-p (selected-frame))))))

(ert-deftest edmacs-windows-test-repair-frame-rebuilds-main-from-wedged-tree ()
  "Both wedged windows lose their side parameters; the survivor becomes an
ordinary main window. A left side window may legitimately be back
afterwards -- `edmacs-windows-frame-repaired-functions' re-shows the
sidebar whenever `modules/sidebar.el' is loaded into the session -- so
the assertions are about the repaired main window, not about the frame
being side-window-free."
  (save-window-excursion
    (cl-destructuring-bind (_left right) (edmacs-test-support-make-wedged-frame)
      (let ((main (edmacs-windows-repair-frame (selected-frame))))
        (should (window-live-p main))
        (should (eq main (edmacs-main-window)))
        (should-not (window-live-p right))
        (dolist (parameter '(window-side window-slot
                             no-other-window no-delete-other-windows))
          (should-not (window-parameter main parameter)))
        (should-not (window-dedicated-p main))
        (should-not (edmacs-windows-frame-wedged-p (selected-frame)))
        ;; Idempotent: a second call is the healthy-frame no-op.
        (should (eq (edmacs-windows-repair-frame (selected-frame)) main))))))

(ert-deftest edmacs-windows-test-repair-frame-strips-every-parameter ()
  "The survivor becomes main, so it must not keep a stack pane's styling.
`edmacs-windows-place' declares `embark-collect' with
`(mode-line-format . none)', and every popup carries `edmacs-stack-popup';
both are registered in `window-persistent-parameters', so a leftover would
re-persist through each `window-state' round trip and leave main
mode-line-less for whatever buffer is reused into it next."
  (save-window-excursion
    (cl-destructuring-bind (left right) (edmacs-test-support-make-wedged-frame)
      ;; The popup is the undedicated window, so repair's survivor search
      ;; prefers it over the dedicated sidebar -- it becomes main.
      (set-window-dedicated-p left t)
      (set-window-parameter right 'mode-line-format 'none)
      (set-window-parameter right 'edmacs-stack-popup t)
      (let ((main (edmacs-windows-repair-frame (selected-frame))))
        (should (window-live-p main))
        (should (eq main (edmacs-main-window)))
        (should-not (window-parameter main 'mode-line-format))
        (should-not (window-parameter main 'edmacs-stack-popup))
        ;; Nothing but the designation itself survives.
        (should (equal (mapcar #'car
                               (seq-remove (lambda (cell) (null (cdr cell)))
                                           (window-parameters main)))
                       '(edmacs-main)))))))

(ert-deftest edmacs-windows-test-repair-frame-evicts-a-dedicated-only-buffer ()
  "When every window was dedicated, the survivor is holding a buffer that
belongs elsewhere -- the sidebar's -- so main gets *scratch* instead."
  (save-window-excursion
    (let ((buf (generate-new-buffer "ewt-wedged-dedicated")))
      (unwind-protect
          (cl-destructuring-bind (left right)
              (edmacs-test-support-make-wedged-frame)
            (set-window-buffer left buf)
            (dolist (w (list left right)) (set-window-dedicated-p w t))
            (let ((main (edmacs-windows-repair-frame (selected-frame))))
              (should (window-live-p main))
              (should-not (eq (window-buffer main) buf))
              (should (equal (buffer-name (window-buffer main)) "*scratch*"))))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-repair-frame-runs-the-repaired-hook ()
  (save-window-excursion
    (let* ((seen nil)
           (edmacs-windows-frame-repaired-functions
            (list (lambda (frame) (push frame seen)))))
      (edmacs-test-support-make-wedged-frame)
      (edmacs-windows-repair-frame (selected-frame))
      (should (equal seen (list (selected-frame))))
      ;; A healthy frame does not re-run it.
      (edmacs-windows-repair-frame (selected-frame))
      (should (equal seen (list (selected-frame)))))))

(ert-deftest edmacs-windows-test-repair-frame-is-a-no-op-when-reentrant ()
  "The hook may itself reach repair; the guard must make that a no-op
rather than a recursion."
  (save-window-excursion
    (edmacs-test-support-make-wedged-frame)
    (let ((edmacs-windows--repairing t))
      (should-not (edmacs-windows-repair-frame (selected-frame)))
      (should (edmacs-windows-frame-wedged-p (selected-frame))))))

(ert-deftest edmacs-windows-test-wedged-p-excludes-child-and-minibuffer-frames ()
  "A corfu-style child frame and a minibuffer-only frame are never wedged:
neither is expected to hold a main window, and repairing one would
collapse a popup. Batch cannot make either kind (`make-frame' has no
terminal, and `parent-frame' cannot name the frame itself), so the two
frame parameters are stubbed instead."
  (save-window-excursion
    (edmacs-test-support-make-wedged-frame)
    (should (edmacs-windows-frame-wedged-p (selected-frame)))
    (let ((real (symbol-function 'frame-parameter)))
      (dolist (stub '((parent-frame . t) (minibuffer . only)))
        (cl-letf (((symbol-function 'frame-parameter)
                   (lambda (frame parameter)
                     (if (eq parameter (car stub))
                         (cdr stub)
                       (funcall real frame parameter)))))
          (should-not (edmacs-windows-frame-wedged-p (selected-frame)))
          ;; And repair is the healthy-frame no-op on such a frame.
          (should-not (edmacs-windows-repair-frame (selected-frame))))))
    ;; Still wedged once the stubs are gone -- nothing was repaired.
    (should (edmacs-windows-frame-wedged-p (selected-frame)))))

(ert-deftest edmacs-windows-test-display-buffer-on-mainless-frame-lands-in-center ()
  "The wedge's real symptom: `display-buffer' used to hand an unrouted
buffer a THIRD side window (or, on the sole-sidebar shape, a whole new
frame) because `display-buffer-in-side-window' always succeeds."
  (save-window-excursion
    (let ((buf (generate-new-buffer "ewt-unrouted"))
          (frames (length (frame-list))))
      (unwind-protect
          (progn
            (edmacs-test-support-make-wedged-frame)
            (let ((win (display-buffer buf)))
              (should (window-live-p win))
              (should-not (window-parameter win 'window-side))
              (should (eq win (edmacs-main-window)))
              (should (eq (window-buffer win) buf))
              (should (= (length (frame-list)) frames))
              (should (zerop (edmacs-windows-test--right-windows-count)))))
        (kill-buffer buf)))))

(defun edmacs-windows-test--right-windows-count ()
  "Number of right side windows on the selected frame."
  (length (seq-filter (lambda (w) (eq (window-parameter w 'window-side) 'right))
                      (window-list nil 'no-minibuf))))

(ert-deftest edmacs-windows-test-display-buffer-on-healthy-frame-uses-main ()
  "The recover action must be invisible on a frame that has a main window."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((buf (generate-new-buffer "ewt-routed")))
      (unwind-protect
          (let ((win (display-buffer buf '(nil (inhibit-same-window . t)))))
            (should (eq win (edmacs-main-window))))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-sweep-stale-panes-repairs-a-wedged-frame ()
  (save-window-excursion
    (edmacs-test-support-make-wedged-frame)
    (let ((main (edmacs-stack-sweep-stale-panes (selected-frame))))
      (should (window-live-p main))
      (should (eq main (edmacs-main-window)))
      (should-not (edmacs-windows-frame-wedged-p (selected-frame))))))

(ert-deftest edmacs-windows-test-degraded-paths-never-leave-main-nil ()
  "Each of these used to no-op or signal on a mainless frame, stranding
point in a `no-other-window' side window with no way back."
  (dolist (command '(edmacs-stack-close
                     edmacs-window-delete-or-demote
                     edmacs-stack-toggle))
    (save-window-excursion
      (edmacs-test-support-make-wedged-frame)
      (funcall command)
      (should (edmacs-main-window))
      (should-not (edmacs-windows-frame-wedged-p (selected-frame))))))

(ert-deftest edmacs-windows-test-degraded-paths-from-the-dedicated-sidebar ()
  "The same three commands invoked from the window repair is about to
delete. Each resolves main BEFORE reading `selected-window', so none of
them acts on a stale window object: reading it first left
`edmacs-window-delete-or-demote' comparing a dead window against the new
main, taking the delete branch, and signalling on the repaired main
window itself."
  (dolist (command '(edmacs-stack-close
                     edmacs-window-delete-or-demote
                     edmacs-stack-toggle))
    (save-window-excursion
      (edmacs-test-support-make-wedged-frame 'dedicated)
      (funcall command)
      (should (edmacs-main-window))
      (should (window-live-p (edmacs-main-window)))
      (should-not (edmacs-windows-frame-wedged-p (selected-frame))))))

(ert-deftest edmacs-windows-test-repair-frame-rebuilds-main-from-a-sole-side-window ()
  "Repair on the parentless shape: there is no sibling to collapse onto,
so the sole window itself is stripped, un-dedicated and evicted to
*scratch* -- and no new frame is popped to escape it."
  (save-window-excursion
    (let ((frames (length (frame-list)))
          (window (edmacs-test-support-make-wedged-frame 'sole)))
      (should-not (window-parent window))
      (should (edmacs-windows-frame-wedged-p (selected-frame)))
      (let ((main (edmacs-windows-repair-frame (selected-frame))))
        (should (window-live-p main))
        (should (eq main (edmacs-main-window)))
        (dolist (parameter '(window-side window-slot
                             no-other-window no-delete-other-windows))
          (should-not (window-parameter main parameter)))
        (should-not (window-dedicated-p main))
        (should (equal (buffer-name (window-buffer main)) "*scratch*"))
        (should-not (edmacs-windows-frame-wedged-p (selected-frame)))
        (should (= (length (frame-list)) frames))))))

(ert-deftest edmacs-windows-test-display-buffer-on-a-sole-side-window-frame-lands-in-center ()
  "The same recovery as the two-window wedge, on the shape where
`display-buffer-fallback-action' would otherwise reach
`display-buffer-pop-up-frame': an unchanged frame count is what proves
it was never reached."
  (save-window-excursion
    (let ((buf (generate-new-buffer "ewt-sole-unrouted"))
          (frames (length (frame-list))))
      (unwind-protect
          (progn
            (edmacs-test-support-make-wedged-frame 'sole)
            (let ((win (display-buffer buf)))
              (should (window-live-p win))
              (should-not (window-parameter win 'window-side))
              (should (eq win (edmacs-main-window)))
              (should (eq (window-buffer win) buf))
              (should (zerop (edmacs-windows-test--right-windows-count)))
              (should (= (length (frame-list)) frames))))
        (kill-buffer buf)))))

(ert-deftest edmacs-windows-test-degraded-paths-from-a-sole-side-window ()
  "Each of the three commands invoked on the parentless shape, where a
bare `delete-window' signals and `delete-other-windows' is a no-op.
Nothing here is wrapped in `ignore-errors': an uncaught signal is the
regression this asserts against."
  (dolist (command '(edmacs-stack-close
                     edmacs-window-delete-or-demote
                     edmacs-stack-toggle))
    (save-window-excursion
      (edmacs-test-support-make-wedged-frame 'sole)
      (funcall command)
      (should (window-live-p (edmacs-main-window)))
      (should-not (edmacs-windows-frame-wedged-p (selected-frame))))))

(ert-deftest edmacs-windows-test-stack-toggle-reports-the-no-state-signal ()
  "The signal the `condition-case' in `edmacs-stack-toggle' exists for.
On a healthy frame that has never shown a side window there is neither a
side window to stash nor a saved `window-state' to restore, so core's
restore branch errors \"No side windows state found\". The wedged-frame
cases above never reach it -- repair re-shows a real side window first,
so the stash branch always wins -- which is why this asserts the
precondition with `should-error' before calling the command."
  (let ((saved (frame-parameter nil 'window-state)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-frame-parameter nil 'window-state nil)
          (should-not (window-with-parameter 'window-side))
          (should-not (edmacs-windows-frame-wedged-p (selected-frame)))
          (should-error (window-toggle-side-windows) :type 'error)
          (let (messages)
            (cl-letf (((symbol-function 'message)
                       (lambda (format &rest args)
                         (push (apply #'format format args) messages))))
              ;; Unwrapped: an error escaping here fails the test, which is
              ;; the assertion this case is for.
              (edmacs-stack-toggle))
            (should (seq-find (lambda (m)
                                (string-match-p "No side windows state found" m))
                              messages)))
          (should (window-live-p (edmacs-main-window)))
          (should-not (edmacs-windows-frame-wedged-p (selected-frame))))
      (set-frame-parameter nil 'window-state saved))))

(ert-deftest edmacs-windows-test-switch-to-buffer-in-dedicated-window-is-pop ()
  "Read only by `switch-to-buffer's interactive spec: nil hard-errors in
the sidebar and every stack pane while the non-interactive call already
falls through to `pop-to-buffer'. The other values un-dedicate the
target window, which would break the sidebar's own contract."
  (should (eq switch-to-buffer-in-dedicated-window 'pop)))


;; ---------------------------------------------------------------------------
;; A popup that comes and goes leaves no displaced copy behind
;; ---------------------------------------------------------------------------

(ert-deftest edmacs-windows-test-quit-restore-leaves-no-displaced-copy ()
  "A popup taking main and then being quit does not strand main's buffer.
The popup pushes main's buffer onto the stack on the way in;
`quit-restore-window' puts that same buffer back in main on the way out
and knows nothing about the pushed copy. Without the dedupe sweep the
frame is left showing one buffer in two windows -- \"the buffer I was
looking at just appeared in a new window and nothing else changed\" --
and every later popup adds another."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((work (generate-new-buffer "*ewt-dedupe-work*"))
          (popup (generate-new-buffer "*ewt-dedupe-popup*")))
      (unwind-protect
          (progn
            (display-buffer work)
            (should (eq (window-buffer (edmacs-main-window)) work))
            (display-buffer popup)
            (should (eq (window-buffer (edmacs-main-window)) popup))
            (should (seq-find (lambda (w) (eq (window-buffer w) work))
                              (edmacs-stack-windows)))
            (quit-restore-window (get-buffer-window popup) 'bury)
            (should (eq (window-buffer (edmacs-main-window)) work))
            (should (= 1 (length (get-buffer-window-list work nil nil)))))
        (kill-buffer work)
        (kill-buffer popup)))))

(ert-deftest edmacs-windows-test-dedupe-frame-keeps-distinct-stack-panes ()
  "The sweep only ever deletes a window showing main's own buffer."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((a (generate-new-buffer "*ewt-dedupe-a*"))
          (b (generate-new-buffer "*ewt-dedupe-b*")))
      (unwind-protect
          (progn
            (edmacs-windows-test--display-agent-pane a 0)
            (edmacs-windows-test--display-agent-pane b 1)
            (edmacs-windows-dedupe-frame (selected-frame))
            (should (= 2 (length (edmacs-stack-windows))))
            ;; Now main shows A too: the stack copy is the one that goes.
            (set-window-buffer (edmacs-main-window) a)
            (edmacs-windows-dedupe-frame (selected-frame))
            (should (equal (list b)
                           (mapcar #'window-buffer (edmacs-stack-windows)))))
        (kill-buffer a)
        (kill-buffer b)))))

(ert-deftest edmacs-windows-test-dedupe-frame-keeps-the-higher-of-two-stack-copies ()
  "Two stack panes on one buffer collapse to the higher one.
The residue a duplicate leaves behind once the swap branch has moved it:
main takes a stack pane's buffer and hands its own -- already duplicated
-- back to that slot."
  (save-window-excursion
    (delete-other-windows)
    (edmacs-window-set-main (selected-window))
    (let ((a (generate-new-buffer "*ewt-dedupe-twice*"))
          (b (generate-new-buffer "*ewt-dedupe-other*")))
      (unwind-protect
          (let (high low)
            (edmacs-windows-test--display-agent-pane b 0)
            (setq high (edmacs-windows-test--display-agent-pane a -2))
            (setq low (edmacs-windows-test--display-agent-pane a -1))
            (should (= 3 (length (edmacs-stack-windows))))
            (edmacs-windows-dedupe-frame (selected-frame))
            (should (window-live-p high))
            (should-not (window-live-p low))
            (should (= 2 (length (edmacs-stack-windows)))))
        (kill-buffer a)
        (kill-buffer b)))))

;;; windows-test.el ends here
