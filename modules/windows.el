;;; windows.el --- Master-and-stack window management -*- lexical-binding: t -*-

;;; Commentary:
;; One explicit MAIN window per frame/tab plus a right-hand STACK (a side
;; window column) -- the dwm/xmonad master-and-stack model.  `edmacs-main'
;; is a real window parameter, not a derived position: `edmacs-main-window'
;; looks a window up by parameter first and only falls back to designating
;; the top-left non-side window when nothing in the frame claims it yet.
;;
;; This module owns the RIGHT element of `window-sides-slots' (nil, i.e.
;; uncapped); `modules/sidebar.el' owns LEFT and must never be clobbered by
;; this module or vice versa -- both use the same nth-rebuild
;; read-modify-write so neither can stomp the other regardless of load
;; order.
;;
;; Every popup buffer (Warnings, Messages, Help/helpful, compilation,
;; Flycheck, Backtrace, Occur, grep/xref, Embark Collect, magit diff/log,
;; lsp-help) routes through one `display-buffer-alist' block onto the
;; shared right-column slot -1. `edmacs-stack-pin' moves the current popup
;; to its own slot when it should outlive the next one; a `quit-restore-
;; window' advice makes `q' in any popup delete the pane and return to
;; `edmacs-main-window' rather than risk restoring a stale prior popup.
;;
;; Run the ERT suite with:
;;   emacs -Q --batch -l ert -l modules/windows.el -l modules/windows-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'windmove)
;; Defines `transpose-dedicated-windows' and `window-layout-transpose';
;; neither is preloaded, so this require -- not just the autoload --
;; keeps the `setq' below from tripping the byte-compiler's
;; free-variable check.
(require 'window-x)

;; ============================================================================
;; Main window: explicit state via a window parameter
;; ============================================================================

;; `writable', not bare `t': `window-state-put' checks the persistent-
;; parameter's type tag before restoring a value, and only `writable'
;; passes that check for an arbitrary Lisp value like `t'.
(add-to-list 'window-persistent-parameters '(edmacs-main . writable))

(defun edmacs--topleft-window ()
  "Return the frame's top-left window that is not a side window.
Used only as `edmacs-main-window's fallback designation target when no
window in the frame yet carries the `edmacs-main' parameter."
  (seq-find (lambda (w) (not (window-parameter w 'window-side)))
            (window-list nil 'no-minibuf (frame-first-window))))

(defun edmacs-main-window ()
  "Return the selected frame's main window.
Looks up whichever live, non-side window carries the `edmacs-main'
parameter (by identity, not position) and returns it. If nothing in the
frame claims it yet, designates the top-left non-side window as main --
setting the parameter so the designation sticks -- and returns it. Returns
nil only when the frame has no non-side windows at all."
  (or (seq-find (lambda (w) (window-parameter w 'edmacs-main))
                (window-list nil 'no-minibuf))
      (let ((win (edmacs--topleft-window)))
        (when win
          (set-window-parameter win 'edmacs-main t))
        win)))

(defun edmacs-window-set-main (&optional window)
  "Mark WINDOW (default the selected window) as the frame's main window.
Clears the `edmacs-main' parameter from every other window on WINDOW's
frame first, so exactly one window ever carries it."
  (interactive)
  (let ((window (or window (selected-window))))
    (dolist (w (window-list (window-frame window) 'no-minibuf))
      (set-window-parameter w 'edmacs-main nil))
    (set-window-parameter window 'edmacs-main t)))

(defun edmacs--swap-window-buffers (w1 w2)
  "Exchange the buffers shown in W1 and W2.
Uses `window-swap-states' for ordinary windows; a side window keeps its
side and slot and only trades buffers. A non-claude buffer moved into a
side window clears `no-other-window' so window navigation still reaches it."
  (if (or (window-parameter w1 'window-side) (window-parameter w2 'window-side))
      (let ((b1 (window-buffer w1)) (b2 (window-buffer w2)))
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (dolist (w (list w1 w2))
          (when (and (window-parameter w 'window-side)
                     (not (string-prefix-p "*claude-term" (buffer-name (window-buffer w)))))
            (set-window-parameter w 'no-other-window nil))))
    (window-swap-states w1 w2)))

(defun edmacs-window-promote (&optional window)
  "Swap WINDOW's buffer into the main window and select the main window.
Like dwm's zoom or tmux's promote. Side windows (the right-hand column
claude-term and *Warnings* use) count as stack windows: promoting from
one puts its buffer in main and the old main buffer in that pane. From
the main window itself, swap with the first stack window."
  (interactive)
  (let* ((window (or window (selected-window)))
         (main (edmacs-main-window))
         (other (if (eq window main)
                    (seq-find (lambda (w) (not (eq w main)))
                              (window-list nil 'no-minibuf main))
                  window)))
    (when other
      (edmacs--swap-window-buffers main other)
      ;; `window-swap-states' swaps non-side window-parameters along with
      ;; the buffers, so without this re-stamp `edmacs-main' would migrate
      ;; onto OTHER (the window that used to hold it) instead of staying
      ;; on the geometric main slot.
      (edmacs-window-set-main main))
    (select-window main)))

(defun edmacs-window-pop-buffer-to-main (buffer)
  "Show BUFFER in the main window and select it.
If BUFFER is already visible in a stack window, swap it into main; if
it is in a side window, close that side window."
  (interactive (list (read-buffer "Pop to main: " (other-buffer) t)))
  (let ((window (get-buffer-window buffer)))
    (if (and window (not (eq window (edmacs-main-window))))
        (edmacs-window-promote window)
      (let ((main (edmacs-main-window)))
        (set-window-buffer main buffer)
        (select-window main)))))

;; Nothing marks a window dedicated today, but this heads off a `user-error'
;; from `window-layout-transpose' the moment something does.
(setq transpose-dedicated-windows t)

;; claude-term panes set `no-other-window' to stay out of `other-window'
;; cycling; directional moves (SPC w h/j/k/l) should still reach them.
;; `windmove-allow-all-windows' can't express that on its own -- it's a
;; single global boolean forwarded as `window-in-direction''s IGNORE
;; argument, so turning it on makes every `no-other-window' window
;; windmove-reachable, sidebar.el's own side window included, which
;; that module's own acceptance criteria forbid. So the flag stays at
;; its default (nil) and reachability is opt-in per window instead: a
;; window sets its own `edmacs-windmove-reachable' parameter (see
;; claude-term.el) to be found on a direction search that would
;; otherwise stop at `no-other-window'.
(advice-add 'windmove-find-other-window :around
            (lambda (orig dir &optional arg window)
              (or (funcall orig dir arg window)
                  (let* ((windmove-allow-all-windows t)
                         (found (funcall orig dir arg window)))
                    (and found
                         (window-parameter found 'edmacs-windmove-reachable)
                         found)))))

;; ============================================================================
;; The stack: the right-hand side-window column
;; ============================================================================

(defcustom edmacs-stack-width 0.4
  "Fractional width of the right-hand stack column, relative to the frame.
`window-sides-vertical' is nil by default, so left and right side
windows form ONE column stacked vertically and share a single width --
several agent panes are stacked top-to-bottom in that column, not laid
out side by side. This value is therefore a per-column width shared by
every stacked pane, not a per-pane budget."
  :type 'number
  :group 'windows)

(defun edmacs-stack--resize-width (window)
  "Resize WINDOW's total width to the live value of `edmacs-stack-width'.
A function-valued `window-width' action-alist entry: `window--display-buffer'
calls this for a freshly created side window instead of the numberp branch.
Unlike a plain number spliced into a `display-buffer-alist' entry at
`defcustom'/`add-to-list' time, this re-reads the variable on every call, so
rebinding `edmacs-stack-width' takes effect on the next popup or agent pane
without re-registering any alist entry."
  (let ((new-width (round (* edmacs-stack-width
                              (window-total-width (frame-root-window window))))))
    (ignore-errors
      (window-resize window (- new-width (window-total-width window)) t 'safe))))

(defun edmacs-stack--popup-alist (&optional slot extra-params)
  "Return a `display-buffer-alist' action list for a stack popup.
Every routed popup buffer shares this one shape: a right side window at
SLOT (default the shared popup slot -1), sized live by
`edmacs-stack--resize-width', tagged `edmacs-stack-popup' so
`edmacs-stack-pin' and the `quit-restore-window' advice below can
recognize it. EXTRA-PARAMS, when given, are additional window-parameters
conses, e.g. Embark's `(mode-line-format . none)'.
SLOT must be a real argument here, not filled in afterward by mutating
the returned alist: a backquote form with no unquote in a given branch
compiles to one shared literal list reused across every call, so
mutating a slot-less template's `slot' entry in place (as `edmacs-stack-pin'
once tried to) would silently corrupt every other alist built from the
same template, registered display-buffer-alist entries included."
  `((display-buffer-in-side-window)
    (side . right)
    (slot . ,(or slot -1))
    (window-width . edmacs-stack--resize-width)
    (preserve-size . (t . nil))
    (window-parameters . ((edmacs-stack-popup . t) ,@extra-params))))

;; Every popup buffer below shares slot -1: `display-buffer-in-side-window'
;; reuses an existing side window whose `window-slot' matches the requested
;; slot, so a second popup replaces the first instead of stacking beside it
;; (see `edmacs-stack-pin' below for pulling one out of that shared slot).
;; Prefixes, not exact names, are used where the real buffer name carries a
;; variable suffix -- helpful's "*helpful variable: foo*", magit's
;; "*magit-diff: reponame*" and "*magit-log: reponame*". Revision-mode and
;; process-mode magit buffers are intentionally not routed here.
(dolist (pattern '("\\`\\*Warnings\\*\\'"
                    "\\`\\*Messages\\*\\'"
                    "\\`\\*Help\\*\\'"
                    "\\`\\*helpful "
                    "\\`\\*compilation\\*\\'"
                    "\\`\\*quickrun\\*\\'"
                    "\\`\\*Flycheck errors\\*\\'"
                    "\\`\\*Backtrace\\*\\'"
                    "\\`\\*Occur\\*\\'"
                    "\\`\\*grep\\*\\'"
                    "\\`\\*xref\\*\\'"
                    "\\`\\*magit-diff: "
                    "\\`\\*magit-log: "
                    "\\`\\*lsp-help\\*\\'"))
  (add-to-list 'display-buffer-alist (cons pattern (edmacs-stack--popup-alist))))

;; Embark's live/completions buffers keep the `(mode-line-format . none)'
;; window-parameter completion.el's own now-removed entry used to set.
(add-to-list 'display-buffer-alist
             (cons "\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   (edmacs-stack--popup-alist nil '((mode-line-format . none)))))

(defun edmacs-stack-windows ()
  "Return the selected frame's stack windows: right side windows, by slot."
  (sort (seq-filter (lambda (w) (eq (window-parameter w 'window-side) 'right))
                     (window-list nil 'no-minibuf))
        (lambda (a b) (< (or (window-parameter a 'window-slot) 0)
                          (or (window-parameter b 'window-slot) 0)))))

;; `window-sides-slots' element order is LEFT TOP RIGHT BOTTOM. Rebuilt as a
;; fresh list (never `setcar'-mutated) so this can never clobber LEFT, which
;; `modules/sidebar.el' owns via the identical pattern -- see AC4 in this
;; roadmap's phase-1 body. RIGHT is nil: no cap, so a fresh slot always
;; creates a new window and `display-buffer-in-side-window' never silently
;; steals an existing pane (see this roadmap's DECISIONS for why a numeric
;; cap is actively dangerous here). `edmacs-stack-pin' below depends on this
;; staying uncapped.
(setq window-sides-slots
      (list (nth 0 window-sides-slots) (nth 1 window-sides-slots)
            nil (nth 3 window-sides-slots)))

(defvar edmacs-stack--next-pin-slot -2
  "Next negative right-column slot `edmacs-stack-pin' will allocate.
Decrements on every call so repeated pins never collide with each other
or with the shared popup slot -1.")

(defun edmacs-stack--allocate-pin-slot ()
  "Return the next unused pin slot and advance the counter."
  (prog1 edmacs-stack--next-pin-slot
    (setq edmacs-stack--next-pin-slot (1- edmacs-stack--next-pin-slot))))

(defun edmacs-stack-pin (&optional window)
  "Relocate WINDOW's buffer out of the shared popup slot to its own slot.
WINDOW (default the selected window) must be a right-column stack
window. Its buffer is redisplayed in a freshly allocated negative slot
-- -2, -3, ... -- and WINDOW itself is deleted. Because the right
column is uncapped (see `window-sides-slots' above), the redisplay
always creates a genuinely new window rather than reusing an existing
one, so a later popup landing back in slot -1 can never steal this
pane. The new window keeps the `edmacs-stack-popup' parameter, so `q'
still deletes it via the advice below."
  (interactive)
  (let ((window (or window (selected-window))))
    (unless (eq (window-parameter window 'window-side) 'right)
      (user-error "edmacs-stack-pin: %s is not a right-column stack window" window))
    (let* ((buffer (window-buffer window))
           (slot (edmacs-stack--allocate-pin-slot))
           (alist (edmacs-stack--popup-alist slot)))
      ;; Calling the action function directly, not `display-buffer', is
      ;; required here: BUFFER's own name (e.g. "*Warnings*") still matches
      ;; one of the slot -1 `display-buffer-alist' entries above, and
      ;; `display-buffer' always merges that alist in ahead of an explicit
      ;; ACTION argument -- its `slot' entry would silently win over ours
      ;; and land the "pinned" window right back in the shared slot.
      (let ((new (display-buffer-in-side-window buffer alist)))
        (delete-window window)
        (when new (select-window new))))))

(defun edmacs-stack--quit-restore-window (orig-fn &optional window bury-or-kill)
  "Force-delete a stack popup window; delegate to ORIG-FN for everything else.
Two different popups sharing slot -1 in succession leave a stale
`window-prev-buffers' entry for the first one on that window; stock
`quit-restore-window' (ORIG-FN) would then take its switch-to-prev-buffer
fallback and resurrect the first popup instead of deleting the pane.
Popup windows -- tagged `edmacs-stack-popup' by `edmacs-stack--popup-alist'
-- never want that: `q' always deletes the window and returns to main."
  (let ((window (window-normalize-window window)))
    (if (and (eq (window-parameter window 'window-side) 'right)
             (window-parameter window 'edmacs-stack-popup))
        (let ((main (edmacs-main-window)))
          (delete-window window)
          (when main (select-window main)))
      (funcall orig-fn window bury-or-kill))))

(advice-add 'quit-restore-window :around #'edmacs-stack--quit-restore-window)

;; ============================================================================
;; Window Rotation (tmux layout replacement)
;; ============================================================================

;; rotate.el moves buffers between windows with `set-window-buffer' or by
;; rebuilding windows, never `window-swap-states', so window parameters that
;; style a buffer (e.g. Embark's `(mode-line-format . none)' from
;; completion.el) stay behind on the old window. Snapshot them before the
;; rotate and move them to whichever window shows the buffer afterward,
;; stripping stale ones from windows that changed buffer. Advised at top
;; level so `window-layout-transpose', which is native, is covered even
;; before rotate autoloads.
;;
;; Parameters that describe the window slot itself (`quit-restore' and
;; friends) must not travel with the buffer: swapping them makes `q' restore
;; the wrong thing.
(defconst edmacs--rotate-window-identity-parameters
  '(quit-restore quit-restore-prev window-side window-slot no-other-window clone-of)
  "Window parameters describing the window slot, not buffer styling.
Left untouched by `edmacs--rotate-preserve-window-parameters' rather
than migrated along with whatever buffer happens to occupy the window
when a rotate/transpose runs.")

(defun edmacs--rotate-capture-window-parameters ()
  "Snapshot window-parameters for the selected frame's windows.
Returns (BY-BUFFER . BY-WINDOW): the same parameter alists keyed by each
window's buffer (to reapply) and by the window itself (to strip). Keys in
`edmacs--rotate-window-identity-parameters' are omitted."
  (let (by-buffer by-window)
    (dolist (w (window-list nil nil (minibuffer-window)))
      (let (params)
        (dolist (param (window-parameters w))
          (unless (memq (car param) edmacs--rotate-window-identity-parameters)
            (push param params)))
        (when params
          (push (cons (window-buffer w) params) by-buffer)
          (push (cons w params) by-window))))
    (cons by-buffer by-window)))

(defun edmacs--rotate-restore-window-parameters (captured)
  "Transfer CAPTURED window-parameters with the buffer they styled.
CAPTURED is from `edmacs--rotate-capture-window-parameters'. For each
live window, strip parameters it carried whose value its current buffer
is not entitled to, then fill in any the buffer had captured. Both steps
work per key, since `set-window-buffer' never clears a window's own
parameters."
  (let ((by-buffer (car captured))
        (by-window (cdr captured)))
    (dolist (w (window-list nil nil (minibuffer-window)))
      (let ((current-params (alist-get (window-buffer w) by-buffer)))
        (dolist (param (alist-get w by-window))
          (unless (equal (cdr param) (alist-get (car param) current-params))
            (set-window-parameter w (car param) nil)))
        (dolist (param current-params)
          (unless (window-parameter w (car param))
            (set-window-parameter w (car param) (cdr param))))))))

(defun edmacs--rotate-preserve-window-parameters (orig-fn &rest args)
  "Preserve per-buffer window-parameters across a rotate/transpose ORIG-FN."
  (let ((captured (edmacs--rotate-capture-window-parameters)))
    (prog1 (apply orig-fn args)
      (edmacs--rotate-restore-window-parameters captured))))

(dolist (fn '(rotate-window rotate-main-vertical rotate-main-horizontal
              rotate-layout window-layout-transpose))
  (advice-add fn :around #'edmacs--rotate-preserve-window-parameters))

;; rotate's two "main" layouts (`rotate-main-vertical'/`rotate-main-horizontal')
;; always rebuild the layout with one window at the top-left; re-stamp
;; `edmacs-main' onto that window once the rebuild (and the parameter
;; preservation above) has landed, so master-and-stack's notion of "main"
;; tracks rotate's own. Added after the advice above, so it wraps outermost
;; and runs its stamping once that advice's own restore has finished.
(dolist (fn '(rotate-main-vertical rotate-main-horizontal))
  (advice-add fn :around
              (lambda (orig-fn &rest args)
                (prog1 (apply orig-fn args)
                  (edmacs-window-set-main (edmacs--topleft-window))))))

(provide 'windows)
;;; windows.el ends here
