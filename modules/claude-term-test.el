;;; claude-term-test.el --- Tests for claude-term.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage only -- no live ghostel process is spawned here.
;; See claude-term-live-test.el for coverage of the kill/restart/exit
;; lifecycle, which drives claude-term--exec against real (non-ghostel)
;; subprocesses instead.
;; Run with:
;;   emacs -Q --batch -l ert -l modules/claude-term.el -l modules/claude-term-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; (Loading claude-term.el under `-Q' prints a benign "Unrecognized
;; keyword: :straight" notice from each of the `use-package ghostel' and
;; `use-package evil-ghostel' forms, since straight.el is not bootstrapped
;; in this bare batch harness; that error is caught internally by
;; use-package and does not abort the load or affect any test below --
;; evil-ghostel itself is never actually loaded, so
;; `claude-term--configure-evil-escape' below is tested against the plain
;; `defvar' declared in claude-term.el, not the real package.)

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

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
;; Side-window display
;; ============================================================================
;; These tests create REAL windows through `display-buffer-in-side-window',
;; which works fine under plain `emacs -Q --batch' with no display and no
;; ghostel or straight bootstrap needed (verified empirically before
;; writing these).  Each test starts from a clean window layout via
;; `claude-term-test--with-fresh-slots' and kills every buffer it creates
;; in an `unwind-protect', which also tears down the side window showing
;; it -- confirmed live: killing a side window's buffer deletes the
;; window automatically, no explicit `delete-window' needed.

(defmacro claude-term-test--with-fresh-slots (&rest body)
  "Run BODY with a clean window layout: no stray right side windows.
`claude-term--allocate-slot' scans live windows directly (see
`claude-term--occupied-slots') rather than a counter, so a fresh
allocation only needs an empty starting layout."
  (declare (indent 0))
  `(save-window-excursion
     (delete-other-windows)
     (dolist (w (window-list nil 'no-minibuf))
       (when (eq (window-parameter w 'window-side) 'right)
         (ignore-errors (delete-window w))))
     ,@body))

(ert-deftest claude-term-test-allocate-slot-stable-and-reused ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf1 (generate-new-buffer "claude-term-test-slot-1"))
          (buf2 (generate-new-buffer "claude-term-test-slot-2")))
      (unwind-protect
          (progn
            ;; buf1 must actually occupy a window -- the allocator now
            ;; scans live windows, not a counter, so an un-displayed
            ;; buffer never occupies a slot.
            (let ((win1 (claude-term--display-buffer buf1)))
              (should (equal (window-parameter win1 'window-slot) 0)))
            ;; Re-allocating for the same buffer returns the same slot
            ;; rather than drawing a fresh one.
            (should (equal (claude-term--allocate-slot buf1) 0))
            (should (equal (claude-term--allocate-slot buf2) 1))
            (should (equal (claude-term--allocate-slot buf2) 1)))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

(ert-deftest claude-term-test-allocate-slot-reuses-lowest-free-slot-after-close ()
  "Closing the pane at slot 1 frees it for the next allocation, instead of
the column growing forever -- the fix for plan-review task
`window-stack-slot-counter-and-eviction'. `delete-window', not
`kill-buffer': `edmacs-stack-close' (modules/windows.el) never kills an
agent pane's buffer, only its window."
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf0 (generate-new-buffer "claude-term-test-reuse-0"))
          (buf1 (generate-new-buffer "claude-term-test-reuse-1"))
          (buf2 (generate-new-buffer "claude-term-test-reuse-2"))
          (buf3 (generate-new-buffer "claude-term-test-reuse-3")))
      (unwind-protect
          (progn
            (let ((win0 (claude-term--display-buffer buf0))
                  (win1 (claude-term--display-buffer buf1))
                  (win2 (claude-term--display-buffer buf2)))
              (should (equal (window-parameter win0 'window-slot) 0))
              (should (equal (window-parameter win1 'window-slot) 1))
              (should (equal (window-parameter win2 'window-slot) 2))
              (delete-window win1))
            (should (equal (claude-term--allocate-slot buf3) 1)))
        (dolist (b (list buf0 buf1 buf2 buf3))
          (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest claude-term-test-redisplay-does-not-hijack-slot-reused-by-another-buffer ()
  "A's pane closes (window only, buffer stays live per `edmacs-stack-close'
semantics); its freed slot goes to a new buffer B; redisplaying A later
must draw a fresh slot rather than trust A's stale cached slot and
silently steal B's window out from under B -- the collision this
allocator's own freed-slot reuse would otherwise reopen."
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (bufa (generate-new-buffer "claude-term-test-hijack-a"))
          (bufb (generate-new-buffer "claude-term-test-hijack-b")))
      (unwind-protect
          (progn
            (let ((wina (claude-term--display-buffer bufa)))
              (should (equal (window-parameter wina 'window-slot) 0))
              (delete-window wina))
            (let ((winb (claude-term--display-buffer bufb)))
              (should (equal (window-parameter winb 'window-slot) 0))
              ;; A's cached slot (0) is now B's -- redisplaying A must not
              ;; reuse it and must leave B's window and buffer untouched.
              (let ((wina2 (claude-term--display-buffer bufa)))
                (should (not (equal (window-parameter wina2 'window-slot) 0)))
                (should (window-live-p winb))
                (should (eq (window-buffer winb) bufb)))))
        (dolist (b (list bufa bufb))
          (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest claude-term-test-redisplay-does-not-duplicate-window-on-stale-cross-frame-slot ()
  "`claude-term--slot' is a plain buffer-local, not frame-scoped, so
displaying a buffer on a second frame can overwrite its cache to a slot
number that happens to be free back on this frame.  Simulate exactly
that overwrite -- buffer's real window stays live at slot 0, its cache
is forced to 1 (free on this frame) -- and confirm redisplaying the
buffer reuses its existing window instead of trusting the stale-but-free
cache and opening a second one."
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-cross-frame-stale")))
      (unwind-protect
          (let ((win1 (claude-term--display-buffer buf)))
            (should (equal (window-parameter win1 'window-slot) 0))
            (with-current-buffer buf
              (setq claude-term--slot 1))
            (let ((win2 (claude-term--display-buffer buf)))
              (should (eq win2 win1))
              (should (window-live-p win1))
              (should (= (length (get-buffer-window-list buf nil (selected-frame))) 1))))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-display-buffer-three-agents-stacked ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (bufs (list (generate-new-buffer "claude-term-test-agent-1")
                      (generate-new-buffer "claude-term-test-agent-2")
                      (generate-new-buffer "claude-term-test-agent-3"))))
      (unwind-protect
          (let ((windows (mapcar #'claude-term--display-buffer bufs)))
            (should (= (length windows) 3))
            (dolist (win windows)
              (should (window-live-p win))
              (should (eq (window-parameter win 'window-side) 'right))
              ;; Fractional width, not claude-code-ide.el's 100-column
              ;; default: well under 60% of the (80-column batch) frame.
              (should (< (window-width win) (* 0.6 (frame-width)))))
            ;; Three distinct slots, in creation order.
            (should (equal (mapcar (lambda (w) (window-parameter w 'window-slot)) windows)
                            '(0 1 2))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest claude-term-test-display-buffer-redisplay-reuses-slot ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-redisplay")))
      (unwind-protect
          (let* ((win1 (claude-term--display-buffer buf))
                 (slot1 (window-parameter win1 'window-slot))
                 (win2 (claude-term--display-buffer buf))
                 (slot2 (window-parameter win2 'window-slot)))
            (should (equal slot1 slot2)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-delete-other-windows-preserves-side-window ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-c-x-1")))
      (unwind-protect
          (let* ((win (claude-term--display-buffer buf))
                 (count-before (length (window-list))))
            (delete-other-windows)
            (should (= (length (window-list)) count-before))
            (should (window-live-p win))
            (should (eq (window-buffer win) buf)))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-toggle-side-windows-preserves-size ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-toggle")))
      (unwind-protect
          (let* ((win (claude-term--display-buffer buf))
                 (width (window-width win)))
            (window-toggle-side-windows)
            (window-toggle-side-windows)
            (let ((win2 (get-buffer-window buf)))
              (should win2)
              (should (<= (abs (- (window-width win2) width)) 1))))
        (kill-buffer buf)))))

(ert-deftest claude-term-test-display-buffer-dedicated-for-rotate ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-dedicated")))
      (unwind-protect
          (let ((win (claude-term--display-buffer buf)))
            ;; `rotate--window-list' filters with `(cl-remove-if
            ;; #'window-dedicated-p ...)'; side windows return the
            ;; symbol `side', which is non-nil.  See the two tests below
            ;; for coverage that actually runs rotate.el's real commands
            ;; against a side window rather than only asserting the
            ;; property its filter depends on.
            (should (window-dedicated-p win)))
        (kill-buffer buf)))))

;; ----------------------------------------------------------------------------
;; rotate.el, for real
;; ----------------------------------------------------------------------------
;; The dedication check above is cheap but only a proxy: it never runs an
;; actual rotate.el command.  These two tests load the real, installed
;; `rotate.el' (no straight bootstrap required -- it is a single
;; self-contained file depending only on the built-in `cl-lib') and drive
;; its two distinct code paths -- `rotate-window' (buffer-list swap) and
;; `rotate-main-vertical' (delete-and-rebuild via `rotate--refresh-window',
;; the more destructive of the two: it deletes every window
;; `rotate--window-list' returns and splits fresh ones from the selected
;; window) -- against a real layout containing a side window from
;; `claude-term--display-buffer'.  `rotate-skip-dedicated-windows' is bound
;; to `t' here to mirror the value `modules/ui.el' forces at boot; that is
;; the exact property under test, so these would fail without it.

(defun claude-term-test--locate-real-rotate ()
  "Return the path to the real `rotate.el' straight build, or nil.
Tries this checkout's own straight/build first -- present once this
worktree has itself been opened as a real Emacs config and straight has
bootstrapped it -- then falls back to the sibling main `edmacs' checkout's
straight/build: this repo's roadmap worktrees live under
`<parent>/edmacs__worktrees/<name>', sibling to the main `<parent>/edmacs'
checkout, and straight's build cache is per-checkout, not shared, so a
freshly created worktree has no build of its own yet.  Returns nil rather
than erroring when neither is found -- e.g. a machine that has never
bootstrapped this repo -- so the caller can skip with a clear message
instead of failing."
  (or
   (let ((here (expand-file-name "straight/build/rotate/rotate.el" default-directory)))
     (and (file-exists-p here) here))
   (let* ((root (directory-file-name (expand-file-name default-directory)))
          (worktrees-dir (directory-file-name (file-name-directory root))))
     (when (string-suffix-p "__worktrees" worktrees-dir)
       (let* ((projects-dir (file-name-directory worktrees-dir))
              (repo-name (string-remove-suffix
                          "__worktrees" (file-name-nondirectory worktrees-dir)))
              (main-rotate (expand-file-name
                            (concat repo-name "/straight/build/rotate/rotate.el")
                            projects-dir)))
         (and (file-exists-p main-rotate) main-rotate))))))

(defun claude-term-test--ensure-real-rotate ()
  "Load the real `rotate.el', skipping the calling test if unavailable."
  (unless (featurep 'rotate)
    (let ((path (claude-term-test--locate-real-rotate)))
      (unless path
        (ert-skip "rotate.el's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree \
in a real Emacs session) to enable this test"))
      (load path nil t))))

(ert-deftest claude-term-test-rotate-window-preserves-side-window ()
  (claude-term-test--ensure-real-rotate)
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (rotate-skip-dedicated-windows t)
          (main-buf (generate-new-buffer "claude-term-test-rotate-window-main"))
          (other-buf (generate-new-buffer "claude-term-test-rotate-window-other"))
          (side-buf (generate-new-buffer "claude-term-test-rotate-window-side")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) main-buf)
            (let ((main-win (selected-window))
                  (other-win (split-window (selected-window) nil 'below)))
              (set-window-buffer other-win other-buf)
              (select-window main-win)
              (let ((side-win (claude-term--display-buffer side-buf)))
                (should side-win)
                (select-window main-win)
                ;; The real command, not a re-implementation of its filter.
                (rotate-window)
                (should (window-live-p side-win))
                (should (eq (window-buffer side-win) side-buf))
                (should (eq (window-parameter side-win 'window-side) 'right))
                ;; The two ordinary windows' buffers did swap -- confirms
                ;; the command actually ran rather than no-op'ing.
                (should (equal (list (window-buffer main-win) (window-buffer other-win))
                                (list other-buf main-buf))))))
        (kill-buffer main-buf)
        (kill-buffer other-buf)
        (kill-buffer side-buf)
        (delete-other-windows)))))

(ert-deftest claude-term-test-rotate-main-vertical-preserves-side-window ()
  (claude-term-test--ensure-real-rotate)
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (rotate-skip-dedicated-windows t)
          (main-buf (generate-new-buffer "claude-term-test-rotate-layout-main"))
          (other-buf (generate-new-buffer "claude-term-test-rotate-layout-other"))
          (side-buf (generate-new-buffer "claude-term-test-rotate-layout-side")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) main-buf)
            (let* ((main-win (selected-window))
                   (other-win (split-window main-win nil 'below)))
              (set-window-buffer other-win other-buf)
              (select-window main-win)
              (let ((side-win (claude-term--display-buffer side-buf)))
                (should side-win)
                (select-window main-win)
                ;; `rotate-main-vertical' goes through
                ;; `rotate--refresh-window', which deletes every window
                ;; `rotate--window-list' returns and rebuilds the layout
                ;; from scratch -- the code path most likely to disturb a
                ;; side window if the dedication filter were not in effect.
                (rotate-main-vertical)
                (should (window-live-p side-win))
                (should (eq (window-buffer side-win) side-buf))
                (should (eq (window-parameter side-win 'window-side) 'right))
                (should (window-no-other-p side-win))
                ;; `other-win' was deleted and rebuilt by
                ;; `rotate--refresh-window' -- confirms the destructive
                ;; rebuild actually ran rather than no-op'ing -- yet both
                ;; ordinary buffers still have exactly one window each in
                ;; the new arrangement.
                (should-not (window-live-p other-win))
                (should (get-buffer-window main-buf))
                (should (get-buffer-window other-buf)))))
        (kill-buffer main-buf)
        (kill-buffer other-buf)
        (kill-buffer side-buf)
        (delete-other-windows)))))

(ert-deftest claude-term-test-no-other-window-excludes-side-window ()
  (claude-term-test--with-fresh-slots
    (let ((window-sides-slots '(nil nil 3 nil))
          (buf (generate-new-buffer "claude-term-test-no-other")))
      (unwind-protect
          (let* ((ordinary (selected-window))
                 (win (claude-term--display-buffer buf)))
            (should (window-no-other-p win))
            (select-window ordinary)
            (other-window 1)
            (should-not (eq (selected-window) win))
            (should-not (eq (window-in-direction 'right) win)))
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
