;;; windows.el --- Master-and-stack window management -*- lexical-binding: t -*-

;;; Commentary:
;; One explicit MAIN window per frame/tab plus a right-hand STACK (a side
;; window column) -- the dwm/xmonad master-and-stack model.  `edmacs-main'
;; is a real window parameter, not a derived position: `edmacs-main-window'
;; is a pure lookup of whichever window carries it, and
;; `edmacs-windows-designate-main' is the writer that stamps the top-left
;; non-side window when nothing in the frame claims it yet.
;;
;; ONE placement rule, with no exceptions: every buffer takes MAIN, and
;; whatever MAIN was showing is pushed onto the top of the stack. An agent
;; pane, `magit-status', a vterm, the CIDER REPL, `*Warnings*', dired and an
;; ordinary file all land in the same place, so "where did that go?" has one
;; answer rather than one per buffer class. `switch-to-buffer' obeys it too.
;;
;; The stack is capped by `edmacs-stack-max-windows' (nil for no limit):
;; each displacement takes a slot of its own, so without a bound the column
;; grows once per buffer switch until every pane is a single line. A push
;; past the cap closes the BOTTOM pane -- the least recently displaced --
;; and never kills its buffer.
;;
;; There is no per-buffer placement mechanism at all: a placement would
;; reintroduce exactly the per-class unpredictability the uniform rule
;; removed.
;;
;; `window-sides-slots' has one writer, `edmacs-windows-claim-side': edges
;; are claimed by name, a second claimant signals, and this module claims
;; RIGHT (nil, i.e. uncapped) while `modules/sidebar.el' claims LEFT.
;;
;; `edmacs-stack-pin' moves the current popup to its own slot when it
;; should outlive the next one; a `quit-restore-window' advice makes `q' in
;; any popup delete the pane and return to `edmacs-main-window' rather than
;; risk restoring a stale prior popup.
;;
;; No buffer is ever shown twice on one frame: a popup that took MAIN and
;; was then quit leaves the copy it displaced stranded in the stack, so
;; `edmacs-windows-dedupe-frame' deletes it.
;;
;; `edmacs-stack-balance-center' (`SPC w =') restores the frame's
;; proportions -- a side window keeps the absolute width it was created at,
;; which a plain `balance-windows' can never fix.
;;
;; `edmacs-windows-repair-frame' exists because core's own guard cannot
;; fire on the one shape that matters here: `window-main-window' falls back
;; to `frame-root-window', so it never returns nil and `window--sides-check'
;; reads a frame whose every window is a side window as a VALID side
;; configuration.  Repair applies core's own remedy -- reset every
;; `window-side' -- and re-designates main.
;;
;; Run the ERT suite with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/claude-term.el -l modules/windows.el \
;;         -l modules/windows-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'windmove)
;; Defines `transpose-dedicated-windows' and `window-layout-transpose';
;; neither is preloaded, so this require -- not just the autoload --
;; keeps the `setq' below from tripping the byte-compiler's
;; free-variable check.
(require 'window-x)
(require 'tab-bar)

;; ============================================================================
;; window.el internals: one shim each, so a core change has one call site
;; ============================================================================

(defun edmacs-windows--display-buffer-in (buffer window type alist)
  "Show BUFFER in WINDOW the way `display-buffer' would, and return WINDOW.
Wraps window.el's internal `window--display-buffer', which is what applies
an action ALIST's `window-width'/`preserve-size'/`window-parameters' to a
window an action function picked itself. TYPE is `reuse', `window' or
`frame'. Checked against Emacs 30; the signature last changed in Emacs 28,
which dropped the trailing DEDICATED and pop-up-frame arguments -- if a
later release moves it again, this is the one place to fix."
  (window--display-buffer buffer window type alist))

(defmacro edmacs-windows--with-sides-check-inhibited (&rest body)
  "Run BODY with window.el's side-window consistency check suppressed.
Binds the internal `window--sides-inhibit-check', which core itself binds
around every side-window construction path. Needed while rebuilding a
wedged frame: the intermediate shapes are exactly the ones the check
rejects. Checked against Emacs 30."
  (declare (indent 0))
  `(let ((window--sides-inhibit-check t))
     ,@body))

;; ============================================================================
;; Main window: explicit state via a window parameter
;; ============================================================================

;; The layout parameters this config owns. `window--state-put-2' nils every
;; parameter before reassigning the saved ones, so an unregistered one is
;; actively cleared by a frameset restore, not merely not saved. `writable',
;; not bare `t', is what lets an arbitrary Lisp value through.
;; `window-preserved-size' is deliberately absent: its value carries a live
;; buffer object, which must never reach the printed desktop file.
;; `quit-restore' is deliberately absent too, for the same reason: its value
;; embeds live window, buffer, and marker objects (the PREV-WINDOW/THIS-BUFFER
;; slots, and, once a window has shown a different buffer, that buffer's own
;; marker). Persisting it would also do nothing for the other shape that
;; drops `display-buffer-in-side-window's width request -- a reused window
;; whose slot 1 already holds a displaced buffer's quadruple -- which arises
;; from live buffer recreation, not from restore. See
;; `edmacs-sidebar--enforce-width' in sidebar.el for the fix at the point of
;; use instead.
(dolist (parameter '(edmacs-main
                     no-other-window
                     no-delete-other-windows
                     edmacs-stack-popup
                     mode-line-format
                     window-side
                     window-slot))
  (add-to-list 'window-persistent-parameters (cons parameter 'writable)))

(defun edmacs-windows--non-side-windows (&optional frame start)
  "Return FRAME's live, non-minibuffer windows that are not side windows.
START, when given, is the window `window-list' walks from, which is how
`edmacs--topleft-window' gets frame order rather than cycle order."
  (seq-remove (lambda (w) (window-parameter w 'window-side))
              (window-list frame 'no-minibuf start)))

(defun edmacs--topleft-window (&optional frame)
  "Return FRAME's top-left window that is not a side window.
FRAME defaults to the selected frame. Walking from `frame-first-window'
is what makes this frame order rather than `window-list's cycle order,
so the result is the geometric top-left slot."
  (car (edmacs-windows--non-side-windows frame (frame-first-window frame))))

(defun edmacs-main-window (&optional frame)
  "Return FRAME's main window, or nil.
A pure lookup: returns whichever live, non-minibuffer window of FRAME
carries the `edmacs-main' parameter (by identity, not position), and nil
when nothing carries it. FRAME defaults to the selected frame. Never
writes -- `edmacs-windows-designate-main' is the writer that stamps a
frame which has no main window yet."
  (seq-find (lambda (w) (window-parameter w 'edmacs-main))
            (window-list frame 'no-minibuf)))

(defun edmacs-window-set-main (window)
  "Mark WINDOW as its frame's main window.
Clears the `edmacs-main' parameter from every other window on WINDOW's
frame first, so exactly one window ever carries it. Interactively,
WINDOW is always the selected window."
  (interactive (list (selected-window)))
  (dolist (w (window-list (window-frame window) 'no-minibuf))
    (set-window-parameter w 'edmacs-main nil))
  (set-window-parameter window 'edmacs-main t))

(defun edmacs-windows-designate-main (&optional frame)
  "Return FRAME's main window, designating the top-left one when there is none.
The writer half of `edmacs-main-window': every caller that needs a main
window to exist -- rather than merely reporting whether one does -- goes
through this. FRAME defaults to the selected frame. Designation routes
through `edmacs-window-set-main', so at most one window ever carries the
parameter, and through `edmacs--topleft-window', which filters out side
windows -- so a wedged frame (nothing but side windows) returns nil
rather than having its sidebar stamped as main."
  (or (edmacs-main-window frame)
      (let ((win (edmacs--topleft-window frame)))
        (when win (edmacs-window-set-main win))
        win)))

(defun edmacs-windows-main-window-of (frame)
  "Return FRAME's main window, or nil.
The frame-explicit entry point other modules call. FRAME is required on
purpose: a sidebar redraw or a tab stamp can run for a frame that is not
the selected one, and spelling the frame out is what keeps that mix-up
out of the call site."
  (edmacs-main-window frame))

(defun edmacs-windows--ws-main-leaf-params (ws)
  "Return (MARKED . LEAF-PARAMS) for the leaf in WS marked `edmacs-main'.
The shared recursion behind `edmacs-windows-ws-main-leaf' and
`edmacs-windows-ws-main-buffer-names'; LEAF-PARAMS is the matched
`(leaf . PARAMS)' node's own parameter alist, nil when the tree holds no
leaf at all. See `edmacs-windows-ws-main-leaf' for what WS is and what
MARKED means."
  (pcase ws
    (`(leaf . ,params)
     (let* ((leaf-params (alist-get 'parameters params))
            (marked (and (consp leaf-params) (alist-get 'edmacs-main leaf-params))))
       (cons (and marked t) params)))
    (`(,(or 'vc 'hc) . ,rest)
     ;; FOUND and FIRST must stay distinct all the way up: a subtree's
     ;; first-leaf fallback is recorded with a nil MARKED flag, so an
     ;; ancestor can never mistake it for a genuine mark and let an
     ;; unmarked stack subtree overwrite the real main leaf.
     (let (found first)
       (dolist (child rest)
         (when (and (consp child) (memq (car child) '(leaf vc hc)))
           (let* ((result (edmacs-windows--ws-main-leaf-params child))
                  (marked (car result)) (params (cdr result)))
             (unless first (setq first (cons nil params)))
             (when (and marked (not found)) (setq found (cons t params))))))
       (or found first (cons nil nil))))
    (_ (cons nil nil))))

(defun edmacs-windows-ws-main-leaf (ws)
  "Return (MARKED . PREV-BUFFERS) for the leaf in WS marked `edmacs-main'.
WS is the STATE TREE half of a tab's serialized `ws' field -- the `cdr'
of `window-state-get's raw (CONSTRAINTS-ALIST . STATE-TREE) return.
Recurses through `vc'/`hc' combination nodes. MARKED is non-nil only when
a leaf genuinely carried the parameter; when nothing in the whole tree
does, the first leaf's buffers come back with MARKED nil (a tab created
before this convention existed, or one with no main window for any other
reason). PREV-BUFFERS is a list of (NAME START POINT), the writable form
`window-state-get's WRITABLE argument produces.

Lives here rather than in a consumer because `edmacs-main' is this
module's own leaf key: reading a serialized layout for it is the same
question `edmacs-main-window' answers for a live one."
  (let ((result (edmacs-windows--ws-main-leaf-params ws)))
    (cons (car result) (alist-get 'prev-buffers (cdr result)))))

(defun edmacs-windows-ws-main-buffer-names (ws)
  "Return the buffer names WS's main leaf knows, most-specific first.
The leaf's own displayed buffer name, then the names in its
`prev-buffers' -- the order a caller wanting to identify what that
window was showing should try them in. Leaf selection is
`edmacs-windows-ws-main-leaf's, unchanged.

Only strings are returned: a `ws' captured without `window-state-get's
WRITABLE argument holds live buffer objects instead of names, and this
answers about a SERIALIZED layout, where they are always names."
  (let* ((params (cdr (edmacs-windows--ws-main-leaf-params ws)))
         (own (car-safe (alist-get 'buffer params)))
         (prev (mapcar #'car-safe (alist-get 'prev-buffers params))))
    (seq-filter #'stringp (cons own prev))))

(defun edmacs--swap-window-buffers (w1 w2)
  "Exchange the buffers shown in W1 and W2.
Uses `window-swap-states' for ordinary windows; a side window keeps its
side and slot and only trades buffers. A buffer swapped into a side
window clears `no-other-window' so window navigation still reaches it."
  (if (or (window-parameter w1 'window-side) (window-parameter w2 'window-side))
      (let ((b1 (window-buffer w1)) (b2 (window-buffer w2)))
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (dolist (w (list w1 w2))
          (when (window-parameter w 'window-side)
            (set-window-parameter w 'no-other-window nil))))
    (window-swap-states w1 w2)))

(defun edmacs-window-promote (window)
  "Swap WINDOW's buffer into the main window and select the main window.
Like dwm's zoom or tmux's promote. Side windows (the right-hand popup
column *Warnings* and friends use) count as stack windows: promoting
from one puts its buffer in main and the old main buffer in that pane.
From the main window itself, swap with the first stack window.
Interactively, WINDOW is the selected window, unless a numeric prefix
arg N is given, in which case it is the Nth window of
`edmacs-stack-windows' (0-based); an N past the end of the stack, or a
negative N, is a no-op with a message rather than an error."
  (interactive
   (list (if current-prefix-arg
             (let* ((idx (prefix-numeric-value current-prefix-arg))
                    (win (and (>= idx 0) (nth idx (edmacs-stack-windows)))))
               (or win
                   (progn
                     (message "edmacs-window-promote: no stack window at index %d" idx)
                     :noop)))
           (selected-window))))
  (unless (eq window :noop)
    (let* ((main (edmacs-windows-designate-main))
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
      (select-window main))))

(defun edmacs-window-pop-buffer-to-main (buffer)
  "Show BUFFER in the main window and select it.
If BUFFER is already visible in a stack window, swap it into main; if
it is in a side window, close that side window."
  (interactive (list (read-buffer "Pop to main: " (other-buffer) t)))
  (let ((window (get-buffer-window buffer)))
    (if (and window (not (eq window (edmacs-windows-designate-main))))
        (edmacs-window-promote window)
      (let ((main (edmacs-windows-designate-main)))
        (set-window-buffer main buffer)
        (select-window main)))))

(defun edmacs--previous-buffer-for (window)
  "Return a buffer to show in WINDOW once its current buffer is demoted.
Prefers the first live `window-prev-buffers' entry that differs from
WINDOW's current buffer, then `other-buffer', then the scratch buffer --
so demoting never leaves WINDOW showing a dead buffer."
  (let ((current (window-buffer window)))
    (or (seq-some (lambda (entry)
                     (let ((buf (car entry)))
                       (and (buffer-live-p buf) (not (eq buf current)) buf)))
                   (window-prev-buffers window))
        (let ((ob (other-buffer current t)))
          (and (buffer-live-p ob) (not (eq ob current)) ob))
        (get-scratch-buffer-create))))

(defun edmacs-window-demote ()
  "Move MAIN's buffer into the stack's shared popup slot.
Displays main's current buffer in the shared right-column popup slot
(see `edmacs-stack--popup-alist') and replaces main's buffer with
`edmacs--previous-buffer-for's result. `edmacs-window-promote' on that
slot is the inverse: it swaps the two buffers straight back."
  (interactive)
  (let ((main (edmacs-windows-designate-main)))
    (when main
      (let ((buf (window-buffer main))
            (replacement (edmacs--previous-buffer-for main)))
        (display-buffer-in-side-window buf (edmacs-stack--popup-alist))
        (set-window-buffer main replacement)
        (select-window main)))))

;; Nothing marks a window dedicated today, but this heads off a `user-error'
;; from `window-layout-transpose' the moment something does.
(setopt transpose-dedicated-windows t)

;; `windmove-allow-all-windows' stays at its default (nil), so a window
;; carrying `no-other-window' -- sidebar.el's left side window is the only
;; one left that does -- is skipped by `other-window' and by raw
;; `windmove-left'/`-right'/`-up'/`-down', matching sidebar.el's own
;; acceptance criteria. The four `evil-window-*' commands are the one
;; exception: the advice just below retries into a `no-other-window'
;; neighbour when the underlying windmove call left the selected window
;; unchanged, so `C-w h'/`SPC w h'/`C-h' (all three route through
;; `evil-window-left') can still reach the sidebar.

(defun edmacs-windows--reach-no-other-window (direction orig-fn args)
  "Run ORIG-FN with ARGS, then retry DIRECTION past a `no-other-window' block.
An `:around' advice body for `evil-window-DIRECTION'. windmove signals a
`user-error' rather than returning nil when it is blocked at a
`no-other-window' boundary, so ORIG-FN's error is caught, not just its
return value inspected. The retry -- `window-in-direction' with IGNORE
non-nil -- only runs when ORIG-FN actually errored and left the selected
window exactly what it was before: a zero-count motion (`C-w 0 h') never
calls windmove at all and must stay a no-op, a multi-count motion that
moved partway before erroring, or a plain nonexistent-direction error
with no neighbour at all, all re-signal ORIG-FN's original error
unchanged rather than being silently swallowed or misread as a block."
  (let ((before (selected-window))
        handled
        signalled)
    (condition-case err
        (apply orig-fn args)
      (error (setq signalled err)))
    (when (and signalled (eq (selected-window) before))
      (let ((target (window-in-direction direction before t)))
        (when target
          (select-window target)
          (setq handled t))))
    (when (and (not handled) signalled)
      (signal (car signalled) (cdr signalled)))))

(defun edmacs-windows--reach-left (orig-fn &rest args)
  "`:around' `evil-window-left': retry left past a `no-other-window' block."
  (edmacs-windows--reach-no-other-window 'left orig-fn args))

(defun edmacs-windows--reach-right (orig-fn &rest args)
  "`:around' `evil-window-right': retry right past a `no-other-window' block."
  (edmacs-windows--reach-no-other-window 'right orig-fn args))

(defun edmacs-windows--reach-up (orig-fn &rest args)
  "`:around' `evil-window-up': retry up past a `no-other-window' block."
  (edmacs-windows--reach-no-other-window 'up orig-fn args))

(defun edmacs-windows--reach-down (orig-fn &rest args)
  "`:around' `evil-window-down': retry down past a `no-other-window' block."
  (edmacs-windows--reach-no-other-window 'down orig-fn args))

;; `evil-window-left'/`-right' and `-up'/`-down' are the only entry points
;; advised -- `other-window', `C-x 1' and `display-buffer' keep skipping
;; the sidebar, matching sidebar.el's ACs. Named functions, not lambdas:
;; `advice-add' with a symbol is idempotent, so re-evaluating this file
;; leaves one advice rather than stacking another.
(with-eval-after-load 'evil
  (dolist (pair '((evil-window-left . edmacs-windows--reach-left)
                  (evil-window-right . edmacs-windows--reach-right)
                  (evil-window-up . edmacs-windows--reach-up)
                  (evil-window-down . edmacs-windows--reach-down)))
    (advice-add (car pair) :around (cdr pair))))

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

;; ============================================================================
;; The one placement rule
;; ============================================================================

;; Every buffer is displayed the same way, with no per-buffer exceptions:
;; it takes MAIN, and whatever MAIN was showing is pushed to the top of the
;; right-hand stack. An agent pane, `magit-status', a vterm, `*Help*' and an
;; ordinary file all land in the same place, so "where did that go?" has one
;; answer rather than one per buffer class.

(defcustom edmacs-stack-max-windows 3
  "How many windows the right-hand stack may hold, or nil for no limit.
Each displaced buffer takes a slot of its own, so without a cap the
column grows once per buffer switch and every pane shrinks toward one
line -- unusable, and it destroys the predictability the uniform rule
exists for. When a push would exceed this, the BOTTOM pane is closed:
slots run more-negative-upward, so the bottom is the largest slot, which
is the least recently displaced buffer. Closing a pane never kills its
buffer; it just stops showing it."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'windows)

(defun edmacs-stack--evict-to-cap ()
  "Close bottom stack panes until `edmacs-stack-max-windows' is satisfied.
Returns the buffers whose panes were closed, oldest first."
  (let (evicted)
    (when edmacs-stack-max-windows
      (let ((windows (edmacs-stack-windows)))
        (while (> (length windows) edmacs-stack-max-windows)
          ;; `edmacs-stack-windows' sorts ascending by slot, so the LAST
          ;; entry has the largest slot: the bottom of the column.
          (let ((victim (car (last windows))))
            (push (window-buffer victim) evicted)
            (delete-window victim)
            (setq windows (edmacs-stack-windows))))))
    (nreverse evicted)))

(defun edmacs-windows--push-main-to-stack (main)
  "Push MAIN's current buffer onto the top of the right-hand stack.
Allocates a fresh slot rather than reusing the shared popup slot, so a
displaced buffer never evicts the one displaced before it -- the stack
grows downward from the top, which is what makes the order meaningful.
Slots run negative for the right column, and more negative is higher, so
`edmacs-stack--allocate-pin-slot's descending counter is itself the push."
  (let ((buf (window-buffer main)))
    (when (buffer-live-p buf)
      (prog1 (display-buffer-in-side-window
              buf (cdr (edmacs-stack--popup-alist (edmacs-stack--allocate-pin-slot))))
        (edmacs-stack--evict-to-cap)))))

(defun edmacs-windows--display-in-main (buffer alist)
  "Display BUFFER in `edmacs-main-window', displacing what was there.
The single destination for every `display-buffer' call. Three cases, in
order:

  - BUFFER already IS main's buffer: reuse main, push nothing (so
    redisplaying the current buffer is not a way to churn the stack).
  - BUFFER is already on this frame in another window: swap it with
    main rather than pushing, so it is never shown twice at once.
  - Otherwise: push main's buffer to the top of the stack, then show
    BUFFER in main.

Returns nil when the frame has no usable main window, letting the base
action fall through to its recover and side-window entries."
  (let ((main (edmacs-windows-designate-main)))
    (when (and main (not (window-dedicated-p main)))
      (let ((existing (get-buffer-window buffer (window-frame main))))
        (cond
         ((eq (window-buffer main) buffer)
          (edmacs-windows--display-buffer-in buffer main 'reuse alist))
         ((and existing (not (eq existing main)))
          (edmacs--swap-window-buffers main existing)
          ;; `window-swap-states' carries non-side parameters across, so
          ;; re-stamp or `edmacs-main' migrates off the geometric main slot.
          (edmacs-window-set-main main)
          main)
         (t
          (edmacs-windows--push-main-to-stack main)
          (edmacs-windows--display-buffer-in buffer main 'reuse alist)))))))

;; The recover entry keeps a frame with no main window left from falling
;; through to Emacs's own `display-buffer-fallback-action' (as far as
;; `display-buffer-pop-up-frame'); the side-window entry is the last resort
;; when even recovery fails.
(setq display-buffer-base-action
      (cons (list #'edmacs-windows--display-in-main
                  #'edmacs-windows--display-buffer-in-recovered-main
                  #'display-buffer-in-side-window)
            (cdr (edmacs-stack--popup-alist))))

;; Routes `switch-to-buffer' through the base action above, so switching to a
;; buffer places it exactly like opening one. Emacs defaults this to nil,
;; which swaps main's buffer in place and drops the displaced one out of
;; view entirely.
(setq switch-to-buffer-obey-display-actions t)

;; Read only by `switch-to-buffer's interactive spec. Left nil, `:b' and
;; `SPC b b' hard-error in the sidebar and in every stack pane, while the
;; non-interactive call already falls through to `pop-to-buffer'; `pop'
;; makes the two agree. The other values un-dedicate the target window --
;; `prompt' after asking -- which would break the sidebar's own contract.
(setq switch-to-buffer-in-dedicated-window 'pop)

;; ============================================================================
;; No per-buffer placements
;; ============================================================================
;; Deliberately none, and there is no mechanism left to declare one with.
;; Every buffer -- agent panes, `magit-status', vterm, `*Warnings*', dired,
;; an ordinary file -- goes through `edmacs-windows--display-in-main' above.
;; A per-class exception would reintroduce exactly the unpredictability the
;; uniform rule exists to remove.

(defun edmacs-stack-windows (&optional frame)
  "Return FRAME's stack windows -- its right side windows -- ordered by slot.
FRAME defaults to the selected frame."
  (sort (seq-filter (lambda (w) (eq (window-parameter w 'window-side) 'right))
                     (window-list frame 'no-minibuf))
        (lambda (a b) (< (or (window-parameter a 'window-slot) 0)
                          (or (window-parameter b 'window-slot) 0)))))

;; ============================================================================
;; One buffer, one window: the displaced-copy sweep
;; ============================================================================
;; A transient popup takes MAIN like everything else does, pushing main's
;; buffer onto the stack. `quit-restore-window' then puts that same buffer
;; back in main and the pushed copy is stranded there for good -- so every
;; popup that comes and goes leaves the stack one window wider, each showing
;; whatever main happened to hold at the time. `--display-in-main' already
;; refuses to show a buffer twice; this holds that invariant against the
;; restore path, which never goes through it.

(defvar edmacs-windows--deduping nil
  "Non-nil while `edmacs-windows-dedupe-frame' is deleting a window.
`delete-window' runs `window-buffer-change-functions' again, so without
this the sweep re-enters itself once per window it deletes.")

(defun edmacs-windows-dedupe-frame (frame)
  "Delete FRAME's stack windows showing a buffer already displayed on it.
Main wins over the stack, and among stack windows the higher pane wins --
`edmacs-stack-windows' is slot-ordered, so the copy that goes is always
the one further down the column."
  (when (and (frame-live-p frame) (not edmacs-windows--deduping))
    (let* ((edmacs-windows--deduping t)
           (main (edmacs-main-window frame))
           (seen (and main (list (window-buffer main)))))
      (dolist (window (edmacs-stack-windows frame))
        (when (and (window-live-p window) (not (eq window main)))
          (let ((buffer (window-buffer window)))
            (if (memq buffer seen)
                (ignore-errors (delete-window window))
              (push buffer seen))))))))

;; The net for every other restore path (`bury-buffer', `switch-to-prev-buffer',
;; a frameset put back). `window-buffer-change-functions' runs from redisplay,
;; so it never fires under `--batch' -- the `quit-restore-window' advice below
;; covers the path that actually strands a copy, and this catches the rest a
;; frame later.
;;
;; Deliberately kept synchronous here, unlike workspaces.el's own
;; `window-buffer-change-functions' member (`--on-window-buffer-change'),
;; which defers its stray-visit sweep to a timer: that sweep can select a
;; different tab and display a buffer in a different window, real work
;; redisplay-time code must never do, so it has no choice but to defer.
;; This sweep only ever *deletes* a window -- `delete-window' is itself
;; redisplay-safe (`quit-restore-window's own advice above already calls
;; `edmacs-windows-dedupe-frame' straight from its own hook-adjacent path)
;; -- so there is nothing here that needs the same deferral, and adding one
;; would only let another stack push land between this hook's firing and
;; its own eventual timer, growing the very duplicate this sweep exists to
;; remove. The `--deduping' guard above is what actually matters for
;; reentrancy (`delete-window' re-fires this same hook), and stays either way.
(add-hook 'window-buffer-change-functions #'edmacs-windows-dedupe-frame)

;; ============================================================================
;; window-sides-slots: one writer, claimed by edge name
;; ============================================================================

(defconst edmacs-windows--side-edges '(left top right bottom)
  "Element order of `window-sides-slots'.")

(defvar edmacs-windows--side-claims nil
  "Alist of (EDGE VALUE . CLAIMANT) for every claimed side-window edge.")

(defun edmacs-windows-claim-side (edge value &optional claimant)
  "Claim EDGE of `window-sides-slots' for VALUE on behalf of CLAIMANT.
EDGE is one of `edmacs-windows--side-edges'; VALUE is that edge's slot
cap (nil means uncapped). CLAIMANT defaults to the loading file's base
name. Signals when a different claimant already holds EDGE at a
different VALUE, so a second owner is a loud failure rather than a
silent overwrite. Unclaimed edges keep whatever value they already had."
  (unless (memq edge edmacs-windows--side-edges)
    (error "edmacs-windows-claim-side: %S is not one of %S"
           edge edmacs-windows--side-edges))
  (let* ((claimant (or claimant
                       (intern (file-name-base
                                (or load-file-name buffer-file-name "unknown")))))
         (standing (assq edge edmacs-windows--side-claims)))
    (when (and standing
               (not (eq (cddr standing) claimant))
               (not (equal (cadr standing) value)))
      (error "edmacs-windows-claim-side: %s is already claimed by %s (=%S); %s wants %S"
             edge (cddr standing) (cadr standing) claimant value))
    (if standing
        (setcdr standing (cons value claimant))
      (setq edmacs-windows--side-claims
            (append edmacs-windows--side-claims
                    (list (cons edge (cons value claimant))))))
    (setq window-sides-slots
          (let ((index -1))
            (mapcar (lambda (e)
                      (setq index (1+ index))
                      (let ((claim (assq e edmacs-windows--side-claims)))
                        (if claim (cadr claim) (nth index window-sides-slots))))
                    edmacs-windows--side-edges)))))

;; RIGHT stays uncapped so a fresh slot always creates a new window and
;; `display-buffer-in-side-window' never silently steals an existing pane;
;; `edmacs-stack-pin' below depends on that.
(edmacs-windows-claim-side 'right nil 'windows)

(defvar edmacs-stack--next-pin-slot -2
  "Next negative right-column slot `edmacs-stack-pin' will allocate.
Decrements on every call so repeated pins never collide with each other
or with the shared popup slot -1.")

(defun edmacs-stack--allocate-pin-slot ()
  "Return the next unused pin slot and advance the counter past it.
Skips any slot already carried by a live stack window -- e.g. the fixed
-2 slot cider/*shell* pin themselves to -- rather than handing out a
slot `display-buffer-in-side-window' would just reuse instead of
creating fresh."
  (let ((used (mapcar (lambda (w) (window-parameter w 'window-slot))
                       (edmacs-stack-windows))))
    (while (memq edmacs-stack--next-pin-slot used)
      (setq edmacs-stack--next-pin-slot (1- edmacs-stack--next-pin-slot)))
    (prog1 edmacs-stack--next-pin-slot
      (setq edmacs-stack--next-pin-slot (1- edmacs-stack--next-pin-slot)))))

(defun edmacs-stack-pin (window)
  "Relocate WINDOW's buffer out of the shared popup slot to its own slot.
WINDOW must be a right-column stack window. Its buffer is redisplayed
in a freshly allocated negative slot -- -2, -3, ... -- and WINDOW
itself is deleted. Because the right column is uncapped (see
`window-sides-slots' above), the redisplay always creates a genuinely
new window rather than reusing an existing one, so a later popup
landing back in slot -1 can never steal this pane. The new window
keeps the `edmacs-stack-popup' parameter, so `q' still deletes it via
the advice below. Interactively, WINDOW is always the selected window."
  (interactive (list (selected-window)))
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
      (when new (select-window new)))))

(defun edmacs-stack--quit-restore-window (orig-fn &optional window bury-or-kill)
  "Force-delete a stack popup window; delegate to ORIG-FN for everything else.
Two different popups sharing slot -1 in succession leave a stale
`window-prev-buffers' entry for the first one on that window; stock
`quit-restore-window' (ORIG-FN) would then take its switch-to-prev-buffer
fallback and resurrect the first popup instead of deleting the pane.
Popup windows -- tagged `edmacs-stack-popup' by `edmacs-stack--popup-alist'
-- never want that: `q' always deletes the window and returns to main.
BURY-OR-KILL is still honored: `kill' (e.g. `C-u q') kills the buffer
after the window is gone, matching stock `quit-restore-window'. `killing'
means the buffer will be killed elsewhere (e.g. by `quit-windows-on' or
`replace-buffer-in-windows', per their docstrings) -- this function must
not kill it itself, only stock `kill' does that.

A popup that took MAIN goes through ORIG-FN instead, which restores the
buffer main was showing before the popup displaced it -- leaving the
pushed copy stranded in the stack. `edmacs-windows-dedupe-frame' clears
it here rather than waiting on the redisplay-time sweep, which never
runs at all under `--batch'."
  (let ((window (window-normalize-window window)))
    (if (and (eq (window-parameter window 'window-side) 'right)
             (window-parameter window 'edmacs-stack-popup))
        (let ((main (edmacs-windows-designate-main))
              (buf (window-buffer window)))
          (delete-window window)
          (when main (select-window main))
          (when (eq bury-or-kill 'kill)
            (kill-buffer buf)))
      (let ((frame (window-frame window)))
        (funcall orig-fn window bury-or-kill)
        (edmacs-windows-dedupe-frame frame)))))

(advice-add 'quit-restore-window :around #'edmacs-stack--quit-restore-window)

;; ============================================================================
;; Master-and-stack moves: cycling, closing, and resizing the stack
;; ============================================================================

(defun edmacs-stack-next ()
  "Select the next stack window in slot order, wrapping through main.
From main, selects the first stack window (a no-op when the stack is
empty). From the last stack window, wraps back to main."
  (interactive)
  (let* ((stack (edmacs-stack-windows))
         (main (edmacs-windows-designate-main)))
    (if (eq (selected-window) main)
        (when stack (select-window (car stack)))
      (let ((rest (cdr (memq (selected-window) stack))))
        (if rest
            (select-window (car rest))
          (when main (select-window main)))))))

(defun edmacs-stack-prev ()
  "Select the previous stack window in slot order, wrapping through main.
The exact reverse traversal of `edmacs-stack-next'."
  (interactive)
  (let* ((stack (edmacs-stack-windows))
         (main (edmacs-windows-designate-main)))
    (if (eq (selected-window) main)
        (when stack (select-window (car (last stack))))
      (let ((pos (seq-position stack (selected-window))))
        (if (and pos (> pos 0))
            (select-window (nth (1- pos) stack))
          (when main (select-window main)))))))

(defun edmacs-stack-close ()
  "Close the selected window without killing its buffer.
Never acts on `edmacs-main-window' itself. Deletes the window -- an
agent pane's live session buffer, in particular, is never killed -- then
selects main."
  (interactive)
  (let* ((main (or (edmacs-main-window) (edmacs-windows-repair-frame (selected-frame))))
         (window (selected-window)))
    (when (and (not (eq window main)) (window-live-p window))
      (ignore-errors (delete-window window)))
    (when (and main (window-live-p main))
      (select-window main))))

(defun edmacs--center-split-p ()
  "Return non-nil when the frame has more than one non-side window."
  (> (length (edmacs-windows--non-side-windows)) 1))

(defun edmacs-window-delete-or-demote ()
  "On main, demote if a center split exists, else message; elsewhere, delete.
Replaces plain `delete-window' on `SPC w d': deleting `edmacs-main-window'
outright would leave the frame without one, so main demotes instead."
  (interactive)
  ;; Resolve main before reading `selected-window': repairing a wedged frame
  ;; can delete the window that was selected when the command was invoked.
  (let* ((main (or (edmacs-main-window) (edmacs-windows-repair-frame (selected-frame))))
         (window (selected-window)))
    (if (eq window main)
        (if (edmacs--center-split-p)
            (edmacs-window-demote)
          (message "edmacs-window-delete-or-demote: no center split to demote into"))
      (ignore-errors (delete-window window)))))

;; ============================================================================
;; Quitting: `:q' closes a buffer, never the frame
;; ============================================================================

(defun edmacs-quit-window-or-buffer (&optional force)
  "Close the selected window, or kill its buffer when it is the frame's last.
Overrides `evil-quit', so this is what `:q', `:wq', `:x', `ZQ' and
`C-w q' all reach.

Vim's `:q' closes a window, but Emacs cannot close a frame's last one,
so `evil-quit' falls through to `delete-frame' -- which under the daemon
takes the last visible frame, and Emacs' place in the Dock with it (see
`edmacs-ns-close-frame'). That reads as \"Emacs quit\" rather than
\"buffer closed\". This never deletes a frame:

  - a side window (the sidebar) is closed outright;
  - so is the selected window while a center split remains, as in vim;
  - otherwise the frame's last ordinary window keeps its slot and its
    buffer is killed, leaving the next buffer on display.

A buffer some blocking `emacsclient FILE' is still waiting on is
finished rather than killed -- releasing that client is the one branch
of `evil-quit's own cascade worth keeping.

FORCE (the `!' of `:q!') discards unsaved changes, and only on the
branches that would otherwise prompt. It is read from `evil-ex-bang'
rather than an argument because an `:override' advice supplies the
interactive form evil's ex layer calls this through."
  (interactive (list (bound-and-true-p evil-ex-bang)))
  (cond
   ((or (window-parameter (selected-window) 'window-side)
        (edmacs--center-split-p))
    (delete-window))
   ((and (bound-and-true-p server-buffer-clients)
         (fboundp 'server-edit)
         (fboundp 'server-buffer-done))
    (if force (server-buffer-done (current-buffer)) (server-edit)))
   (t
    (when force (set-buffer-modified-p nil))
    (kill-current-buffer))))

;; `:q', `:wq' and `:x' all funnel through `evil-quit', as do `ZQ' and
;; `C-w q'; one override covers every spelling rather than redefining each
;; ex command. `:qa' is deliberately left alone -- quitting Emacs is what it
;; is for.
(with-eval-after-load 'evil
  (advice-add 'evil-quit :override #'edmacs-quit-window-or-buffer))

(defun edmacs-stack--apply-width ()
  "Resize every live stack window to the current `edmacs-stack-width'."
  (dolist (w (edmacs-stack-windows))
    (edmacs-stack--resize-width w)))

(defun edmacs-stack--round-to-grid (value)
  "Round VALUE to the nearest 1/20.
Naive repeated +/-0.05 arithmetic on a float can land a hair off its
target (0.5500000000000001 and the like); this keeps `edmacs-stack-width'
on the exact grid `edmacs-stack-widen'/`edmacs-stack-narrow' step by."
  (/ (float (round (* value 20))) 20))

(defun edmacs-stack--set-width (new-width)
  "Set `edmacs-stack-width' to NEW-WIDTH, grid-rounded and clamped, and resize."
  (setq edmacs-stack-width
        (max 0.05 (min 0.95 (edmacs-stack--round-to-grid new-width))))
  (edmacs-stack--apply-width))

(defun edmacs-stack-widen ()
  "Widen the stack column by 0.05 and resize every stack window to match."
  (interactive)
  (edmacs-stack--set-width (+ edmacs-stack-width 0.05)))

(defun edmacs-stack-narrow ()
  "Narrow the stack column by 0.05 and resize every stack window to match."
  (interactive)
  (edmacs-stack--set-width (- edmacs-stack-width 0.05)))

(defvar edmacs-windows-rebalance-functions nil
  "Abnormal hook run with the selected FRAME by `edmacs-stack-balance-center'.
The extension point for a side column this module does not own --
`modules/sidebar.el' joins it to re-apply the left sidebar's width. Runs
after the stack column has been resized and before the center split is
balanced, so a member may resize its own window without having its work
undone. A member must not call `display-buffer'.")

(defun edmacs-stack-balance-center ()
  "Restore the frame's proportions: side columns first, then the center.
A side window keeps the absolute width it was created at, so moving a
frame to a wider display leaves the stack column at whatever fraction of
the OLD frame it happened to be -- the one thing a plain `balance-windows'
can never fix, since `window-main-window' (the frame's non-side subtree
root) deliberately excludes every side window. So the stack column is
resized back to `edmacs-stack-width' first, `edmacs-windows-rebalance-functions'
is given its chance at the other side columns, and only then are the
center windows balanced against whatever width is left."
  (interactive)
  (edmacs-stack--apply-width)
  (run-hook-with-args 'edmacs-windows-rebalance-functions (selected-frame))
  (let ((root (window-main-window)))
    (when root
      (balance-windows root))))

(defun edmacs-stack-toggle ()
  "Toggle visibility of the frame's side windows.
Repairs a mainless frame first: `window-toggle-side-windows's delete
branch runs `delete-other-windows' on `window-main-window', which on such
a frame is the root rather than a real main window -- so it deletes
nothing while still stashing the broken tree for the next toggle to
restore. The restore branch's own \"no side windows state\" signal is
reported rather than propagated."
  (interactive)
  (edmacs-windows-repair-frame (selected-frame))
  (condition-case err
      (window-toggle-side-windows)
    (error (message "edmacs-stack-toggle: %s" (error-message-string err)))))

;; ============================================================================
;; Persistence: the desktop-restore dead-pane sweep
;; ============================================================================

(defvar edmacs-stack-agent-pane-p #'ignore
  "Predicate for a window whose agent session has died.
Called with one live window on the swept frame; a non-nil return means
`edmacs-stack-sweep-stale-panes' should delete it rather than show it as
a stale pane. Default `#\\='ignore' never matches, which keeps this
module free of any claude-term dependency; `modules/claude-term-registry.el'
wires the real liveness check (buffer-shaped-like-an-agent-pane plus a
dead process) onto this variable at load time.")

(defun edmacs-stack-sweep-stale-panes (frame)
  "Delete FRAME's dead panes.
Meant to run right after a desktop restore: `window-state-put' recreates
whatever windows the saved frameset had, but an agent pane's process
cannot survive a restart and a popup's buffer may not have been saved at
all, so a plain state restore alone can leave stale panes behind.
Deletes a right stack window whose buffer is no longer live, and any
window -- agent panes are ordinary windows, so this is not restricted to
the stack -- for which `edmacs-stack-agent-pane-p' reports its session
has died; everything else, a popup with a live buffer included, is left
alone.
Finishes by calling `edmacs-windows-repair-frame', which re-designates
main when the `edmacs-main' parameter did not survive the restore and
rebuilds the tree outright when the restore left FRAME with no non-side
window to designate. This is the single entry point for \"an external
process left this frame in an unknown state\"; callers that only need the
shape repaired -- and must not delete a dead agent pane -- call
`edmacs-windows-repair-frame' directly instead."
  (dolist (w (window-list frame 'no-minibuf))
    (when (and (window-live-p w)
               (or (and (eq (window-parameter w 'window-side) 'right)
                        (not (buffer-live-p (window-buffer w))))
                   (funcall edmacs-stack-agent-pane-p w)))
      (ignore-errors (delete-window w))))
  (edmacs-windows-repair-frame frame))

;; ============================================================================
;; Shape repair: a frame must always have a main window
;; ============================================================================

;; Core detects an invalid side-window configuration and self-heals it, but
;; not this one. `window-main-window' (window.el) walks for a non-side
;; window with a side-window sibling and, finding none, falls back to
;; `frame-root-window' -- so it never returns nil, which makes
;; `window--sides-check-failed's own "Frame %s has no main window" branch
;; unreachable. Its remaining clauses only check side-parameter
;; CONSISTENCY, which a frame of nothing but side windows satisfies, so
;; `window--sides-check' reports that frame valid and resets nothing.
;; Two things compound it: the check runs only from `window--check'
;; (`delete-window', `split-window', `window-state-put' ...), never at side
;; window creation, and every construction path binds
;; `window--sides-inhibit-check' while it builds.
;; `edmacs-windows-designate-main' has nothing it may stamp on exactly the
;; frames core calls healthy, so the guard has to live here.

(defvar edmacs-windows-frame-repaired-functions nil
  "Abnormal hook run with the repaired FRAME by `edmacs-windows-repair-frame'.
The extension point for anything that must be re-established once a frame
regains a main window -- `modules/sidebar.el' joins it to re-show the left
sidebar. A hook function must not call `display-buffer': repair can run
from inside a `display-buffer' action function, so a member that re-enters
`display-buffer' recurses. Calling `display-buffer-in-side-window'
directly, as `edmacs-sidebar-show' does, is safe.")

(defvar edmacs-windows--repairing nil
  "Non-nil while `edmacs-windows-repair-frame' is rebuilding a frame.
Makes a nested repair -- from the repaired hook, or from a
`display-buffer' the hook triggers -- return the frame unchanged.")

(defun edmacs-windows-frame-wedged-p (frame)
  "Non-nil when FRAME has no main window.
True exactly when every window on FRAME is a side window: nothing can be
designated main, and `display-buffer' has nowhere to put a buffer but
another side window.
Child frames (corfu-style popups) and minibuffer-only frames are never
wedged -- neither is expected to hold a main window."
  (and (frame-live-p frame)
       (null (frame-parameter frame 'parent-frame))
       (not (eq (frame-parameter frame 'minibuffer) 'only))
       (null (edmacs-windows--non-side-windows frame))))

(defun edmacs-windows-repair-frame (frame)
  "Give FRAME back a main window and return it.
The remedy for the shape core reads as valid (see above): every window a
side window, so nothing can be designated main and `display-buffer' can
only add more side windows. Goes past core's own `window--sides-check'
remedy of resetting every `window-side': clears every window parameter on
every window, then collapses the frame to one ordinary, undedicated
window and designates it main via `edmacs-window-set-main'.

Clearing wholesale rather than a named few is deliberate -- the survivor
becomes the frame's main window, and any parameter a stack placement left
on it (`mode-line-format', `edmacs-stack-popup', anything added later)
would otherwise style main as the popup it used to be, and re-persist
through `window-persistent-parameters'.

Destructive to slot layout: `window-slot' goes with the rest, so a pinned
stack pane loses its slot. The frame was already unusable, and
`edmacs-stack--next-pin-slot' is not rewound, so later pins still
allocate fresh slots.

On a healthy frame, on a child or minibuffer-only frame, and
re-entrantly, this only designates -- it rebuilds nothing, and returns
whatever window carries `edmacs-main' once that designation has run.
Finishes by running `edmacs-windows-frame-repaired-functions' with FRAME.
Interactively, FRAME is always the selected frame."
  (interactive (list (selected-frame)))
  (cond
   ((not (frame-live-p frame)) nil)
   ((or edmacs-windows--repairing (not (edmacs-windows-frame-wedged-p frame)))
    (when (called-interactively-p 'interactive)
      (message "edmacs-windows-repair-frame: layout is healthy"))
    ;; Designate, not merely look up: a frameset restore can bring back a
    ;; healthy tree whose `edmacs-main' parameter did not survive, and
    ;; `edmacs-stack-sweep-stale-panes' documents this call as the thing
    ;; that gives such a frame its main window back.
    (edmacs-windows-designate-main frame))
   (t
    (let ((edmacs-windows--repairing t))
      (with-selected-frame frame
        (let* ((windows (window-list frame 'no-minibuf))
               (free (seq-find (lambda (w) (not (window-dedicated-p w))) windows))
               (survivor (or free (car windows)))
               (ignore-window-parameters t))
          (edmacs-windows--with-sides-check-inhibited
            ;; Every parameter, not a named few: a stack placement can leave
            ;; `mode-line-format', `edmacs-stack-popup' or any later marker
            ;; on the window that becomes main, and a popup's styling must
            ;; not outlive the popup.
            (dolist (w windows)
              (dolist (parameter (mapcar #'car (window-parameters w)))
                (set-window-parameter w parameter nil)))
            (set-window-dedicated-p survivor nil)
            ;; Every window was dedicated, so the survivor is holding a
            ;; buffer that belongs somewhere else -- the sidebar's, usually,
            ;; which the repaired hook is about to re-show in a side window.
            (unless free
              (set-window-buffer survivor (get-buffer-create "*scratch*")))
            (delete-other-windows survivor)
            (edmacs-window-set-main survivor)))
        (run-hook-with-args 'edmacs-windows-frame-repaired-functions frame)
        ;; The survivor was stamped by `edmacs-window-set-main' just above,
        ;; so the pure lookup is what is wanted here.
        (edmacs-main-window frame))))))

;; A root-window split on a frame that owns any side window is delegated by
;; core's `split-window' to `window-main-window' -- and on a frame whose ONLY
;; window is a side window that is the very same window, so it delegates to
;; itself until `max-lisp-eval-depth' blows. (`edmacs-stack-toggle' documents
;; the same `window-main-window'-returns-the-root trap.) `window-state-put'
;; also accepts a saved layout of only side windows without complaint, so a
;; tab saved in that shape restores the frame straight into it.
;;
;; The remedy here is deliberately NOT `edmacs-windows-repair-frame'. That one
;; is for a frame whose layout is past saving: it clears every window
;; parameter and collapses the frame to a single window. Run from a tab
;; switch it destroys the layout of the tab being left -- which
;; `tab-bar-select-tab' then SAVES -- and of the tab being entered, flattening
;; every tab to one `*scratch*' window a switch at a time.

(defun edmacs-windows-ensure-main-window (frame)
  "Give FRAME a main window if it has none, without dismantling anything.
A no-op on any frame that already has a non-side window, so this is safe on
the hot path of every tab switch. Where `edmacs-windows-repair-frame'
collapses a frame to one bare window, this only adds the window that is
missing and leaves every side window -- the sidebar included -- as it was.

Splits with `ignore-window-parameters' bound, because the split it needs is
exactly the one core would otherwise delegate to `window-main-window' and so
back to itself."
  (when (and (frame-live-p frame) (edmacs-windows-frame-wedged-p frame))
    (with-selected-frame frame
      (let ((new (ignore-errors
                   (let ((ignore-window-parameters t)
                         (window--sides-inhibit-check t))
                     (split-window (frame-root-window frame) nil 'right)))))
        (when (window-live-p new)
          (dolist (parameter (mapcar #'car (window-parameters new)))
            (set-window-parameter new parameter nil))
          (set-window-dedicated-p new nil)
          (set-window-buffer new (get-buffer-create "*scratch*"))
          (edmacs-window-set-main new)
          (select-window new)))))
  (when (frame-live-p frame)
    (with-selected-frame frame (edmacs-main-window))))

(defun edmacs-windows--ensure-main-around-tab-select (&rest _)
  "Ensure a main window exists before and after `tab-bar-select-tab'.
Before, because the incoming layout is restored into this frame; after,
because `window-state-put' can restore a layout that is only side windows."
  ;; `tab-bar-select-tab' takes a tab number, never a frame -- ambient-reads: ok
  (edmacs-windows-ensure-main-window (selected-frame)))

(advice-add 'tab-bar-select-tab :before
            #'edmacs-windows--ensure-main-around-tab-select)
(advice-add 'tab-bar-select-tab :after
            #'edmacs-windows--ensure-main-around-tab-select)

(defun edmacs-windows--display-buffer-in-recovered-main (buffer alist)
  "Display BUFFER in a main window recovered from a wedged frame.
Returns nil on any frame that still has a non-side window, which is what
leaves every existing routing decision untouched: this only ever fires on
the frame `edmacs-windows-frame-wedged-p' reports, where the alternative
is `display-buffer-in-side-window' adding yet another side window to a
frame that has nothing else. ALIST is the `display-buffer' action alist.

`display-buffer' action functions take a fixed (BUFFER ALIST) signature,
never a frame -- `(selected-frame)' here is a forced read, not a default
this function chose."
  ;; ambient-reads: ok -- see the docstring above.
  (when (edmacs-windows-frame-wedged-p (selected-frame))
    (let ((main (edmacs-windows-repair-frame (selected-frame)))) ;; ambient-reads: ok
      (when (window-live-p main)
        (edmacs-windows--display-buffer-in buffer main 'reuse alist)))))

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
(defun edmacs-windows--rotate-restamp-main (orig-fn &rest args)
  "Run ORIG-FN with ARGS, then re-stamp `edmacs-main' on the top-left window."
  (prog1 (apply orig-fn args)
    (edmacs-window-set-main (edmacs--topleft-window))))

(dolist (fn '(rotate-main-vertical rotate-main-horizontal))
  (advice-add fn :around #'edmacs-windows--rotate-restamp-main))

(provide 'windows)
;;; windows.el ends here
