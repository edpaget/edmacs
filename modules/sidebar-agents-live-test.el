;;; sidebar-agents-live-test.el --- Live tests for sidebar-agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; Real frames, real timers, and real (but stubbed) `tmux'/`osascript'
;; binaries on `exec-path' -- the genuinely environment-dependent half of
;; this phase's acceptance criteria that modules/sidebar-agents-test.el's
;; pure suite cannot exercise: a real second frame's focus/tab state, and
;; the literal async `start-process' calls this module issues actually
;; reaching a real subprocess rather than a stubbed Lisp function.
;;
;; sidebar.el and frames.el ARE loaded for real here (unlike the pure
;; suite) -- this file needs `edmacs-sidebar-mode'/`edmacs-sidebar-show'
;; for real sidebar buffers/windows, and stubs only
;; `edmacs-frames-open-worktree-tab' (frames.el's own real
;; implementation shells out to git and manages real tab-bar state well
;; beyond what these tests are about).
;;
;; A second real frame needs a controlling terminal -- absent under
;; plain `-Q --batch', present under `script -q /dev/null emacs -Q
;; --batch ...' -- so those tests skip cleanly under the plain
;; invocation, following sidebar-test.el's own documented convention.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/sidebar-agents-live-test.el -f ert-run-tests-batch-and-exit
;;
;; To actually exercise the second-frame tests:
;;   script -q /dev/null emacs -Q --batch -l ert \
;;         -l modules/git-common-dir.el -l modules/sidebar-agents-live-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

(setq native-comp-enable-subr-trampolines nil)

;; ==========================================================================
;; Loading sidebar.el + sidebar-agents.el for real (mirrors sidebar-test.el)
;; ==========================================================================

(defun edmacs-sidebar-agents-live-test--locate-straight-build-root ()
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

(defun edmacs-sidebar-agents-live-test--add-magit-section-deps (build-root)
  (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section"))
    (let ((dir (expand-file-name dep build-root)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(defvar edmacs-sidebar-agents-live-test--build-root
  (edmacs-sidebar-agents-live-test--locate-straight-build-root))

(if (null edmacs-sidebar-agents-live-test--build-root)

    (ert-deftest edmacs-sidebar-agents-live-test-magit-section-unavailable ()
      (ert-skip "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (edmacs-sidebar-agents-live-test--add-magit-section-deps
     edmacs-sidebar-agents-live-test--build-root)

    ;; frames.el is not loaded (see this file's own Commentary); its two
    ;; symbols sidebar.el's redraw path touches are stood in for, the
    ;; same way sidebar-test.el does for its own worktree-render tests.
    (defvar edmacs-frames--worktrees-cache (make-hash-table :test #'equal))
    (defun edmacs-worktrees-for-repo (common) (gethash common edmacs-frames--worktrees-cache))
    (defun edmacs-frames--tab-for-root (root &optional frame)
      (seq-find (lambda (tab) (equal (alist-get 'edmacs-root tab) root))
                (tab-bar-tabs frame)))
    (defun edmacs-frames--tab-root (tab) (alist-get 'edmacs-root tab))
    (defvar edmacs-sidebar-agents-live-test--open-worktree-tab-calls nil)
    (defun edmacs-frames-open-worktree-tab (dir)
      (push dir edmacs-sidebar-agents-live-test--open-worktree-tab-calls)
      (select-frame-set-input-focus (selected-frame))
      (let ((root (file-truename dir)))
        (unless (edmacs-frames--tab-for-root root (selected-frame))
          (tab-bar-new-tab)
          (push (cons 'edmacs-root root) (cdr (tab-bar--current-tab-find))))
        (let ((tab (edmacs-frames--tab-for-root root (selected-frame))))
          (tab-bar-select-tab (1+ (tab-bar--tab-index tab (tab-bar-tabs) (selected-frame)))))))

    (load (expand-file-name "modules/sidebar.el" default-directory) nil t)
    (load (expand-file-name "modules/agents.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar-agents.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Shared helpers
    ;; ==========================================================================

    (defun edmacs-sidebar-agents-live-test--wait-until (predicate timeout)
      (let ((deadline (+ (float-time) timeout)))
        (while (and (< (float-time) deadline) (not (funcall predicate)))
          (sit-for 0.05))
        (funcall predicate)))

    (cl-defun edmacs-sidebar-agents-live-test--make-agent
        (&key (root "/repo/wt/") (instance "%1") (status 'working)
              (status-ts (float-time)) (title "Claude Code") (source 'workmux)
              (locator nil) (unread nil))
      (make-edmacs-agent :key (cons root instance) :root root :instance instance
                          :status status :status-ts status-ts :updated-ts status-ts
                          :title title :source source :locator locator :unread unread))

    (defmacro edmacs-sidebar-agents-live-test--with-clean-state (&rest body)
      (declare (indent 0))
      `(let ((edmacs-agents--table (make-hash-table :test #'equal))
             (edmacs-agents-changed-hook nil)
             (edmacs-sidebar-agents--last-state (make-hash-table :test #'equal))
             (edmacs-sidebar-agents--attention-cache nil)
             (edmacs-sidebar-agents--attention-cursor 0)
             (edmacs-sidebar-agents--pending-notification nil)
             (edmacs-sidebar-agents--coalesce-timer nil)
             (edmacs-sidebar-agents--elapsed-timer nil)
             (edmacs-sidebar-agents-show-all nil)
             (edmacs-sidebar-agents-live-test--open-worktree-tab-calls nil))
         (unwind-protect
             (progn ,@body)
           (when (timerp edmacs-sidebar-agents--coalesce-timer)
             (cancel-timer edmacs-sidebar-agents--coalesce-timer))
           (when (timerp edmacs-sidebar-agents--elapsed-timer)
             (cancel-timer edmacs-sidebar-agents--elapsed-timer)))))

    (defun edmacs-sidebar-agents-live-test--cleanup-sidebar (frame)
      (edmacs-sidebar-hide frame)
      (let ((buf (edmacs-sidebar--buffer frame)))
        (when (buffer-live-p buf) (kill-buffer buf))
        (set-frame-parameter frame 'edmacs-sidebar-buffer nil)))

    ;; A shim binary directory: one executable script per fake command,
    ;; each appending its own argv (one per line, tab-separated) to a
    ;; log file so a test can assert exactly what was invoked, without
    ;; ever touching a real `tmux'/`osascript' on this machine.
    (defun edmacs-sidebar-agents-live-test--make-shim (name)
      "Write an executable NAME shim into a fresh directory; return (DIR . LOGFILE)."
      (let* ((dir (make-temp-file "edmacs-sidebar-agents-shim-" t))
             (log (expand-file-name "calls.log" dir))
             (bin (expand-file-name name dir)))
        (with-temp-file bin
          (insert "#!/bin/sh\n")
          (insert (format "printf '%%s\\n' \"$*\" >> %s\n" (shell-quote-argument log))))
        (set-file-modes bin #o755)
        (cons dir log)))

    (defun edmacs-sidebar-agents-live-test--shim-calls (log)
      "Return LOG's lines, or nil if it doesn't exist yet."
      (when (file-exists-p log)
        (with-temp-buffer
          (insert-file-contents log)
          (split-string (buffer-string) "\n" t))))

    (defun edmacs-sidebar-agents-live-test--make-second-frame-or-skip ()
      "Same convention as sidebar-test.el's own helper of the same shape."
      (condition-case e
          (let ((frame (make-frame '((window-system . nil)
                                      (tty . "/dev/tty")
                                      (tty-type . "xterm")))))
            (unless (frame-live-p frame)
              (ert-skip "could not create a second frame in this batch environment"))
            frame)
        (error (ert-skip (format "could not create a second frame in this \
batch environment (no controlling terminal? run under `script -q /dev/null \
emacs ...' to exercise this test): %s" e)))))

    ;; ==========================================================================
    ;; AC2 -- RET visit: raise/select tab, tmux window+pane, un-bold
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-live-test-visit-opens-tab-tmux-and-unbolds ()
      "RET on a bold (unread-done) workmux agent row opens/selects its
worktree tab, issues the two real async tmux `start-process' calls (via
a stub `tmux' on `exec-path', never blocking), and a subsequent redraw
shows the row no longer bold."
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let* ((shim (edmacs-sidebar-agents-live-test--make-shim "tmux"))
               (exec-path (cons (car shim) exec-path))
               (agent (edmacs-sidebar-agents-live-test--make-agent
                       :root "/repo/wt/" :status 'done :unread t
                       :locator (list :pane-id "%9" :session "sess" :window "win"))))
          (puthash (edmacs-agent-key agent) agent edmacs-agents--table)
          (unwind-protect
              (progn
                (edmacs-sidebar-show (selected-frame))
                (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    ;; Wrapped in its own outer section rather than
                    ;; inserted as a second top-level call: an
                    ;; unwrapped top-level `magit-insert-section' call
                    ;; becomes `magit-root-section' itself and is
                    ;; skipped by property-tagging (see
                    ;; sidebar-test.el's own comment on this same
                    ;; magit-section quirk), so this ensures
                    ;; `magit-current-section' below finds it via a
                    ;; real text property, not the root-fallback.
                    (magit-insert-section (edmacs-sidebar-agents-live-test-root)
                      (magit-insert-section (edmacs-sidebar-agent agent)
                        (magit-insert-heading (propertize "row" 'face 'bold)))))
                  (goto-char (point-max))
                  (forward-line -1)
                  (should (text-property-any (point-min) (point-max) 'face 'bold))
                  (edmacs-sidebar-agents-visit))
                (should (member "/repo/wt/" edmacs-sidebar-agents-live-test--open-worktree-tab-calls))
                (should (eq 'idle (edmacs-agent-status agent)))
                (should (edmacs-sidebar-agents-live-test--wait-until
                         (lambda ()
                           (>= (length (edmacs-sidebar-agents-live-test--shim-calls (cdr shim))) 2))
                         5.0))
                ;; Both are independent async subprocesses -- issued in
                ;; window-then-pane order from Lisp, but not guaranteed
                ;; to *complete* (and so log) in that order -- so this
                ;; checks both happened, not their relative order.
                (let ((calls (edmacs-sidebar-agents-live-test--shim-calls (cdr shim))))
                  (should (seq-some (lambda (c) (string-match-p "select-window -t sess:win" c)) calls))
                  (should (seq-some (lambda (c) (string-match-p "select-pane -t %9" c)) calls)))
                ;; Un-bolds: the row's own agent struct is now `idle', and a
                ;; fresh render of it carries no bold face.
                (with-temp-buffer
                  (magit-section-mode)
                  (let ((inhibit-read-only t))
                    (edmacs-sidebar-agents--insert-row agent))
                  (should-not (text-property-any (point-min) (point-max) 'face 'bold))))
            (edmacs-sidebar-agents-live-test--cleanup-sidebar (selected-frame))
            (ignore-errors (delete-directory (car shim) t))))))

    (ert-deftest edmacs-sidebar-agents-live-test-visit-claude-term-source-selects-side-window ()
      "An in-Emacs (phase 9) agent row selects its side window instead of
touching tmux at all."
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let* ((agent (edmacs-sidebar-agents-live-test--make-agent
                       :root "/repo/wt/" :source 'claude-term :locator "the-buffer"))
               (selected nil))
          (puthash (edmacs-agent-key agent) agent edmacs-agents--table)
          (cl-letf (((symbol-function 'claude-term--pop-to-side-window)
                     (lambda (buf) (setq selected buf))))
            (edmacs-sidebar-agents--visit-source-extra agent)
            (should (equal "the-buffer" selected))))))

    ;; ==========================================================================
    ;; AC4 -- desktop notification gating on real frame focus, via a real
    ;; (stubbed) `osascript' on `exec-path'
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-live-test-done-notifies-only-when-all-frames-unfocused ()
      "A transition to `done' with `frame-focus-state' stubbed nil for
every frame fires the real `osascript' shim exactly once; with one
frame reporting focus, it fires zero times. `frame-focus-state' itself
is stubbed rather than genuinely fought over -- a real batch/tty frame
has no window-system focus concept to manipulate -- so this is a
faithful, deterministic exercise of the actual gating function against
the actual subprocess path, matching the file_map's own documented
scope for this test."
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let* ((shim (edmacs-sidebar-agents-live-test--make-shim "osascript"))
               (exec-path (cons (car shim) exec-path))
               (edmacs-sidebar-agents-coalesce-seconds 0.1)
               (agent (edmacs-sidebar-agents-live-test--make-agent :status 'done :unread t)))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'frame-focus-state) (lambda (_f) nil)))
                  (edmacs-sidebar-agents--on-transition agent 'working 'done))
                (should (edmacs-sidebar-agents-live-test--wait-until
                         (lambda () (edmacs-sidebar-agents-live-test--shim-calls (cdr shim)))
                         3.0))
                (should (= 1 (length (edmacs-sidebar-agents-live-test--shim-calls (cdr shim)))))
                (delete-file (cdr shim))
                (cl-letf (((symbol-function 'frame-list) (lambda () (list (selected-frame))))
                          ((symbol-function 'frame-focus-state) (lambda (_f) t)))
                  (edmacs-sidebar-agents--on-transition agent 'working 'done))
                (sit-for 0.5)
                (should-not (edmacs-sidebar-agents-live-test--shim-calls (cdr shim))))
            (ignore-errors (delete-directory (car shim) t))))))

    (ert-deftest edmacs-sidebar-agents-live-test-waiting-notifies-regardless-of-focus ()
      "A transition to `waiting' fires the shim whether frames report
focused or not -- a permission prompt always notifies."
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let* ((shim (edmacs-sidebar-agents-live-test--make-shim "osascript"))
               (exec-path (cons (car shim) exec-path))
               (edmacs-sidebar-agents-coalesce-seconds 0.1)
               (agent (edmacs-sidebar-agents-live-test--make-agent :status 'waiting)))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'frame-focus-state) (lambda (_f) t)))
                  (edmacs-sidebar-agents--on-transition agent 'working 'waiting))
                (should (edmacs-sidebar-agents-live-test--wait-until
                         (lambda () (edmacs-sidebar-agents-live-test--shim-calls (cdr shim)))
                         3.0)))
            (ignore-errors (delete-directory (car shim) t))))))

    ;; ==========================================================================
    ;; AC5 -- ALL AGENTS toggle across two live frames
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-live-test-toggle-all-affects-both-frames ()
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let* ((f1 (selected-frame))
               (f2 (edmacs-sidebar-agents-live-test--make-second-frame-or-skip))
               (agent (edmacs-sidebar-agents-live-test--make-agent
                       :root "/repo/wt/" :status 'waiting :title "cross-frame-agent")))
          (puthash (edmacs-agent-key agent) agent edmacs-agents--table)
          (unwind-protect
              (progn
                (edmacs-sidebar-show f1)
                (with-selected-frame f2 (edmacs-sidebar-show f2))
                (edmacs-sidebar-agents-toggle-all)
                (with-current-buffer (edmacs-sidebar--buffer f1)
                  (should (string-match-p "ALL AGENTS" (buffer-string)))
                  (should (string-match-p "cross-frame-agent" (buffer-string))))
                (with-current-buffer (edmacs-sidebar--buffer f2)
                  (should (string-match-p "ALL AGENTS" (buffer-string))))
                (edmacs-sidebar-agents-toggle-all)
                (with-current-buffer (edmacs-sidebar--buffer f1)
                  (should-not (string-match-p "ALL AGENTS" (buffer-string))))
                (with-current-buffer (edmacs-sidebar--buffer f2)
                  (should-not (string-match-p "ALL AGENTS" (buffer-string)))))
            (edmacs-sidebar-agents-live-test--cleanup-sidebar f1)
            (when (frame-live-p f2) (delete-frame f2))))))

    ;; ==========================================================================
    ;; AC1 -- 30s elapsed timer starts only while visible + working agent
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-live-test-elapsed-timer-starts-and-stops-with-visibility ()
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let* ((edmacs-sidebar-agents-elapsed-interval 100)
               (agent (edmacs-sidebar-agents-live-test--make-agent :status 'working)))
          (unwind-protect
              (progn
                (should-not edmacs-sidebar-agents--elapsed-timer)
                (edmacs-sidebar-show (selected-frame))
                (puthash (edmacs-agent-key agent) agent edmacs-agents--table)
                ;; `edmacs-agents-changed-hook' is dynamically let-bound
                ;; to nil by `--with-clean-state' for isolation, so the
                ;; handler is called directly rather than through the
                ;; (locally empty) hook.
                (edmacs-sidebar-agents--on-agents-changed (list (edmacs-agent-key agent)))
                (should (timerp edmacs-sidebar-agents--elapsed-timer))
                (edmacs-sidebar-hide (selected-frame))
                (should-not edmacs-sidebar-agents--elapsed-timer))
            (edmacs-sidebar-agents-live-test--cleanup-sidebar (selected-frame))))))

    )) ; end of build-root-found branch

;;; sidebar-agents-live-test.el ends here
