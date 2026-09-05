;;; claude-term-test.el --- Tests for claude-term.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage only -- no live ghostel process is spawned here.
;; See claude-term-live-test.el for coverage of the kill/restart/exit
;; lifecycle, which drives claude-term--exec against real (non-ghostel)
;; subprocesses instead.
;; Run with:
;;   scripts/run-ert-suite.sh 15 emacs -Q --batch -l ert -l modules/claude-term.el \
;;         -l modules/claude-term-test.el -f ert-run-tests-batch-and-exit
;;
;; The 15s budget is generous margin over the well-under-1s this suite takes
;; once native-comp-enable-subr-trampolines is disabled below -- this file
;; had the same un-guarded trampoline gap as windows-test.el's 4.2s -> 282s
;; incident (see .claude/CLAUDE.md's Testing section).
;;
;; (Loading claude-term.el under `-Q' prints a benign "Unrecognized
;; keyword: :straight" notice from each of the `use-package ghostel' and
;; `use-package evil-ghostel' forms, since straight.el is not bootstrapped
;; in this bare batch harness; that error is caught internally by
;; use-package and does not abort the load or affect any test below --
;; evil-ghostel itself is never actually loaded, so
;; `claude-term--configure-evil-escape' below is tested against the plain
;; `defvar' declared in claude-term.el, not the real package.)
;;
;; Placement is windows.el's `agent-pane' declaration now, not an action
;; list this module writes, so the coverage for "an agent pane must not
;; land in the stack" lives in windows-test.el against the real registry.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; `signal-process' and `kill-process' are C subrs this file `cl-letf's;
;; without this guard each redirected subr makes Emacs build a native
;; trampoline via a synchronous compiler subprocess (~28s, almost entirely
;; wall clock). See .claude/CLAUDE.md's Testing section and frames-test.el's
;; precedent.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

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

;; ============================================================================
;; Evil integration
;; ============================================================================

(ert-deftest claude-term-test-configure-evil-escape-sets-buffer-locally ()
  (with-temp-buffer
    (rename-buffer "*claude-term:demo*")
    (claude-term--configure-evil-escape)
    (should (eq (buffer-local-value 'evil-ghostel-escape (current-buffer)) 'evil))
    ;; `claude-term-mode' is what AC2's C-g binding is actually keyed to
    ;; (see the section comment near `claude-term-send-escape' in
    ;; claude-term.el); confirm it comes on alongside the escape-routing
    ;; value in the same guarded branch.
    (should (bound-and-true-p claude-term-mode))))

(ert-deftest claude-term-test-configure-evil-escape-ignores-unrelated-buffer ()
  (with-temp-buffer
    (rename-buffer "*scratch-unrelated*")
    (claude-term--configure-evil-escape)
    (should-not (local-variable-p 'evil-ghostel-escape))
    (should-not (bound-and-true-p claude-term-mode))))

(ert-deftest claude-term-test-send-escape-sends-raw-escape-in-claude-term-buffer ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'ghostel-send-string)
               (lambda (s) (push s calls))))
      (with-temp-buffer
        (rename-buffer "*claude-term:demo*")
        (claude-term-send-escape)))
    (should (equal calls '("\e")))))

(ert-deftest claude-term-test-send-escape-falls-through-to-C-g-elsewhere ()
  "In a non-claude-term ghostel buffer, C-g must not become a raw ESC.
Exercises `claude-term-send-escape's own internal buffer-name guard in
isolation (defense in depth); the real scoping mechanism -- C-g is only
ever bound to this command via the marker minor mode `claude-term-mode',
which is buffer-locally off outside claude-term buffers -- is proven at
the level of real evil key-binding resolution by
`claude-term-live-test-real-evil-ghostel-c-g-scoped-to-claude-term-buffers-only'
in claude-term-live-test.el, since a plain call here never drives evil's
keymap machinery at all."
  (let ((escape-calls nil)
        (c-g-calls 0))
    (cl-letf (((symbol-function 'ghostel-send-string)
               (lambda (s) (push s escape-calls)))
              ((symbol-function 'ghostel-send-C-g)
               (lambda () (cl-incf c-g-calls))))
      (with-temp-buffer
        (rename-buffer "*ghostel:some-other-terminal*")
        (claude-term-send-escape)))
    (should (equal escape-calls nil))
    (should (= c-g-calls 1))))

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

;; ============================================================================
;; Pane display
;; ============================================================================
;; These tests create REAL windows through `display-buffer', which works
;; fine under plain `emacs -Q --batch' with no display and no ghostel or
;; straight bootstrap needed.  Each test starts from a clean window layout
;; via `claude-term-test--with-fresh-layout' and kills every buffer it
;; creates in an `unwind-protect'.
;;
;; The whole point of the section is that an agent pane is an ORDINARY
;; window: it is not a side window, so `balance-windows' reaches it (the
;; regression these tests exist for), it cycles under `other-window', and
;; `delete-other-windows' closes it without touching the session.

(defmacro claude-term-test--with-fresh-layout (&rest body)
  "Run BODY from a single-window layout with no stray side windows.
Also pins `split-window-sensibly''s two thresholds so a split actually
happens: the batch frame is 80x25, below BOTH stock thresholds
\(`split-width-threshold' 160, `split-height-threshold' 80), so
`display-buffer-pop-up-window' would decline and `display-buffer' would
silently fall through to reusing the one existing window -- making every
assertion below about a freshly created pane vacuous. Pinned to a
below-split (height 0, width nil) rather than left to chance so the
geometry each test measures is deterministic."
  (declare (indent 0))
  `(save-window-excursion
     (let ((split-height-threshold 0)
           (split-width-threshold nil))
       (delete-other-windows)
       (dolist (w (window-list nil 'no-minibuf))
         (when (window-parameter w 'window-side)
           (ignore-errors (delete-window w))))
       ,@body)))

(ert-deftest claude-term-test-display-buffer-creates-an-ordinary-window ()
  "An agent pane carries none of the side-window parameters, so
`balance-windows' -- which only ever rebalances `window-main-window''s
non-side subtree -- reaches it."
  (claude-term-test--with-fresh-layout
    (let ((buf (generate-new-buffer "claude-term-test-ordinary")))
      (unwind-protect
          (let ((win (claude-term--display-buffer buf)))
            (should (window-live-p win))
            (should-not (window-parameter win 'window-side))
            (should-not (window-parameter win 'window-slot))
            (should-not (window-parameter win 'no-other-window))
            (should-not (window-parameter win 'no-delete-other-windows))
            (should-not (window-dedicated-p win))
            (should (eq (window-frame win) (selected-frame))))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-display-buffer-redisplay-reuses-the-same-window ()
  "Redisplaying a live session returns its existing window instead of
splitting a second one for the same buffer."
  (claude-term-test--with-fresh-layout
    (let ((buf (generate-new-buffer "claude-term-test-reuse")))
      (unwind-protect
          (let* ((first (claude-term--display-buffer buf))
                 (before (length (window-list nil 'no-minibuf)))
                 (second (claude-term--display-buffer buf)))
            (should (eq first second))
            (should (equal (length (window-list nil 'no-minibuf)) before)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-display-buffer-two-sessions-get-two-windows ()
  (claude-term-test--with-fresh-layout
    (let ((buf1 (generate-new-buffer "claude-term-test-two-1"))
          (buf2 (generate-new-buffer "claude-term-test-two-2")))
      (unwind-protect
          (let ((win1 (claude-term--display-buffer buf1))
                (win2 (claude-term--display-buffer buf2)))
            (should (window-live-p win1))
            (should (window-live-p win2))
            (should-not (eq win1 win2)))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

(ert-deftest claude-term-test-pane-cycles-under-other-window ()
  "The `no-other-window' parameter is gone, so a pane is an ordinary stop
for `other-window' and for a windmove direction search."
  (claude-term-test--with-fresh-layout
    (let ((buf (generate-new-buffer "claude-term-test-cycle")))
      (unwind-protect
          (let* ((ordinary (selected-window))
                 (win (claude-term--display-buffer buf)))
            (should-not (window-no-other-p win))
            (select-window ordinary)
            (other-window 1)
            (should (eq (selected-window) win)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-balance-windows-resizes-a-pane ()
  "The regression this move exists for: `balance-windows' equalizes a
deliberately lopsided layout that includes an agent pane.  As a side
window the pane was invisible to it and kept whatever size it had."
  (claude-term-test--with-fresh-layout
    (let ((buf (generate-new-buffer "claude-term-test-balance")))
      (unwind-protect
          (let* ((win (claude-term--display-buffer buf))
                 (other (seq-find (lambda (w) (not (eq w win)))
                                  (window-list nil 'no-minibuf))))
            (should (window-live-p win))
            (should other)
            ;; Skew the split, then balance it back.
            (window-resize win 3 nil 'safe)
            (should-not (equal (window-total-height win)
                               (window-total-height other)))
            (balance-windows)
            (should (<= (abs (- (window-total-height win)
                                (window-total-height other)))
                        1)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-delete-other-windows-closes-a-pane ()
  "`no-delete-other-windows' is gone: the pane's WINDOW goes, its buffer
and session do not."
  (claude-term-test--with-fresh-layout
    (let ((buf (generate-new-buffer "claude-term-test-delete-other")))
      (unwind-protect
          (let ((ordinary (selected-window)))
            (claude-term--display-buffer buf)
            (select-window ordinary)
            (delete-other-windows)
            (should (equal (length (window-list nil 'no-minibuf)) 1))
            (should (buffer-live-p buf)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-pop-to-window-selects-the-pane ()
  (claude-term-test--with-fresh-layout
    (let ((buf (generate-new-buffer "claude-term-test-pop")))
      (unwind-protect
          (let ((win (claude-term--pop-to-window buf)))
            (should (eq (selected-window) win))
            (should (eq (window-buffer win) buf)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-palette-covers-every-ansi-slot ()
  "The pinned palette names all sixteen `ghostel-color-*' faces, once each.
ghostel is not loaded in this harness, so the face names are compared
against a literal list rather than `ghostel-color-palette'."
  (let ((faces (mapcar #'car claude-term-palette)))
    (should (equal (sort (copy-sequence faces) #'string<)
                   (sort (list 'ghostel-color-black
                               'ghostel-color-red
                               'ghostel-color-green
                               'ghostel-color-yellow
                               'ghostel-color-blue
                               'ghostel-color-magenta
                               'ghostel-color-cyan
                               'ghostel-color-white
                               'ghostel-color-bright-black
                               'ghostel-color-bright-red
                               'ghostel-color-bright-green
                               'ghostel-color-bright-yellow
                               'ghostel-color-bright-blue
                               'ghostel-color-bright-magenta
                               'ghostel-color-bright-cyan
                               'ghostel-color-bright-white)
                         #'string<)))))

(ert-deftest claude-term-test-palette-colors-are-hex ()
  (dolist (hex (append (mapcar #'cdr claude-term-palette)
                       (list claude-term-default-foreground
                             claude-term-default-background)))
    (should (string-match-p "\\`#[0-9a-f]\\{6\\}\\'" hex))))

(ert-deftest claude-term-test-terminate-signals-child-behind-a-pipe-handle ()
  "A native-PTY session is terminated by signaling its child, not the handle.
On that path -- ghostel's default for every local spawn --
`ghostel--process' is a pipe process standing in for the real child, and
`kill-process' rejects it outright.  The live-test harness stubs
`ghostel-exec' with a real subprocess, so only a pipe handle exercises
this."
  (let ((pipe (make-pipe-process :name "claude-term-test-pipe" :noquery t))
        (signalled nil))
    (unwind-protect
        (with-temp-buffer
          (should (eq (process-type pipe) 'pipe))
          (should-error (kill-process pipe))
          (setq-local ghostel--process pipe)
          (setq-local ghostel--pid 4242)
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (pid sig) (setq signalled (cons pid sig)))))
            (claude-term--terminate))
          (should (equal signalled '(4242 . KILL))))
      (delete-process pipe))))

(ert-deftest claude-term-test-terminate-uses-kill-process-for-a-real-handle ()
  "The Emacs PTY path (remote spawns, and the live-test harness) still
goes through `kill-process' -- `ghostel--process' is a real subprocess
there, and its pid is the one Emacs already owns."
  (let ((killed nil))
    (with-temp-buffer
      (setq-local ghostel--process (start-process "claude-term-test-real" nil "sleep" "60"))
      (set-process-query-on-exit-flag ghostel--process nil)
      (setq-local ghostel--pid (process-id ghostel--process))
      (unwind-protect
          (cl-letf (((symbol-function 'kill-process)
                     (lambda (proc) (setq killed proc))))
            (claude-term--terminate)
            (should (eq killed ghostel--process)))
        (delete-process ghostel--process)))))

(ert-deftest claude-term-test-kill-terminates-a-pipe-backed-session ()
  "`claude-term-kill' reaches a native-PTY session's child without erroring."
  (let* ((buf (generate-new-buffer "claude-term-test-kill"))
         (pipe (make-pipe-process :name "claude-term-test-kill-pipe"
                                  :buffer buf :noquery t))
         (signalled nil))
    (unwind-protect
        (with-current-buffer buf
          (setq-local ghostel--process pipe)
          (setq-local ghostel--pid 4243)
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (pid sig) (setq signalled (cons pid sig)))))
            (claude-term-kill buf))
          (should (equal signalled '(4243 . KILL))))
      (delete-process pipe)
      (kill-buffer buf))))

;;; claude-term-test.el ends here
