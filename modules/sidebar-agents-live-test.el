;;; sidebar-agents-live-test.el --- Live tests for sidebar-agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; Real frames, real timers, and real (but stubbed) `tmux'/`osascript'
;; binaries on `exec-path' -- the genuinely environment-dependent half of
;; this phase's acceptance criteria that modules/sidebar-agents-test.el's
;; pure suite cannot exercise: a real second frame's focus/tab state, and
;; the literal async `start-process' calls this module issues actually
;; reaching a real subprocess rather than a stubbed Lisp function.
;;
;; sidebar.el and windows.el ARE loaded for real here (unlike the pure
;; suite) -- this file needs `edmacs-sidebar-mode'/`edmacs-sidebar-show'
;; for real sidebar buffers/windows, and stubs only
;; `edmacs-workspaces-open-worktree' (workspaces.el's own real
;; implementation shells out to git and manages real tab-bar state well
;; beyond what these tests are about).
;;
;; A second real frame needs a controlling terminal -- absent under
;; plain `-Q --batch', present under `script -q /dev/null emacs -Q
;; --batch ...' -- so those tests skip cleanly under the plain
;; invocation, following sidebar-test.el's own documented convention.
;;
;; edmacs-sidebar roadmap phase 9 also loads claude-term.el,
;; claude-term-registry.el and claude-term-agents.el here (real, not
;; stubbed) and adds one test that spawns a real claude-term session
;; via `claude-term--exec' -- following claude-term-registry-live-test.el's
;; own fake-ghostel-exec stub pattern, duplicated locally rather than
;; shared, per that file's own stated no-cross-load-order-dependency
;; convention -- so `edmacs-sidebar-agents-visit' is exercised against a
;; REAL, adapter-produced `edmacs-agent' row instead of the synthetic
;; struct-and-mocked-registry stub every other claude-term test in this
;; file (and in sidebar-agents-test.el) uses. Closes the "not
;; end-to-end exercisable until edmacs-sidebar roadmap phase 9" caveat
;; named in sidebar-agents.el's own rename/kill docstrings.
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

    ;; workspaces.el is not loaded (see this file's own Commentary); the
    ;; small pure lookups sidebar.el's redraw/activate path calls are real,
    ;; unstubbed reimplementations of its own logic against real tab-bar
    ;; primitives -- the same convention sidebar-test.el uses for its own
    ;; worktree-render tests (see that file's own Commentary).
    (defconst edmacs-sidebar-agents-live-test--root-parameter 'edmacs-workspace-root)
    (defun edmacs-workspaces-groups (&optional frame)
      (delete-dups (delq nil (mapcar (lambda (tab) (funcall tab-bar-tab-group-function tab))
                                       (tab-bar-tabs (or frame (selected-frame)))))))
    (defun edmacs-workspaces-tabs-in-group (group &optional frame)
      (when group (seq-filter (lambda (tab) (equal (funcall tab-bar-tab-group-function tab) group))
                               (tab-bar-tabs (or frame (selected-frame))))))
    (defun edmacs-workspaces-tab-root (tab) (alist-get edmacs-sidebar-agents-live-test--root-parameter tab))
    (defun edmacs-workspaces-current-group (&optional frame)
      (when-let* ((tab (assq 'current-tab
                             (frame-parameter (or frame (selected-frame)) 'tabs))))
        (funcall tab-bar-tab-group-function tab)))
    (defun edmacs-workspaces-find-tab (group root &optional frame)
      (seq-find (lambda (tab) (and (equal (funcall tab-bar-tab-group-function tab) group)
                                    (equal (edmacs-workspaces-tab-root tab) root)))
                (tab-bar-tabs (or frame (selected-frame)))))
    (defun edmacs-workspaces-select-tab (group root &optional frame)
      (let* ((target (or frame (selected-frame))) (tab (edmacs-workspaces-find-tab group root target)))
        (when tab
          (let ((number (1+ (tab-bar--tab-index tab (tab-bar-tabs target) target))))
            (if frame (with-selected-frame frame (tab-bar-select-tab number)) (tab-bar-select-tab number))))
        tab))
    (defun edmacs-workspaces-classify-root (root) (ignore root) nil)
    ;; Overrides git-common-dir.el's real (loaded) function: none of this
    ;; file's fixture roots name a real git worktree, and none of these
    ;; tests care about main/roadmap/task classification -- returning nil
    ;; unconditionally keeps `edmacs-sidebar--derive-main-root' from ever
    ;; attempting a real (and here, pointlessly failing) `git' subprocess.
    (defun edmacs-git-common-dir (_root) nil)
    (defvar edmacs-sidebar-agents-live-test--open-worktree-tab-calls nil)
    (defun edmacs-workspaces-open-worktree (dir)
      (push dir edmacs-sidebar-agents-live-test--open-worktree-tab-calls)
      (select-frame-set-input-focus (selected-frame))
      (let* ((root (file-truename dir))
             (group (file-name-nondirectory (directory-file-name root))))
        (unless (edmacs-workspaces-find-tab group root (selected-frame))
          (tab-bar-new-tab)
          (setf (alist-get edmacs-sidebar-agents-live-test--root-parameter
                            (cdr (tab-bar--current-tab-find)))
                root)
          (tab-bar-change-tab-group group))
        (edmacs-workspaces-select-tab group root (selected-frame))))

    ;; windows.el first: sidebar.el `require's it for `edmacs-windows-claim-side'.
    (load (expand-file-name "modules/windows.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar.el" default-directory) nil t)
    (load (expand-file-name "modules/agents.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar-agents.el" default-directory) nil t)
    (load (expand-file-name "modules/claude-term.el" default-directory) nil t)
    (load (expand-file-name "modules/claude-term-registry.el" default-directory) nil t)
    (load (expand-file-name "modules/claude-term-agents.el" default-directory) nil t)

    ;; The real `inheritenv' package is not bootstrapped under `-Q
    ;; --batch' (straight.el is absent), but `claude-term--exec'
    ;; unconditionally wraps its `ghostel-exec' call in `(inheritenv
    ;; ...)' -- see claude-term-live-test.el's identical shim.
    (unless (fboundp 'inheritenv)
      (defmacro inheritenv (&rest body) `(progn ,@body)))

    ;; `claude-term.el' declares `ghostel--process' via a bare `(defvar
    ;; ghostel--process)' for byte-compiler purposes only; production
    ;; installs its buffer-local nil default by `require'ing the real
    ;; `ghostel' package inside `claude-term--ensure-ghostel', stubbed
    ;; to a no-op below. Install the same nil default directly here --
    ;; mirrors claude-term-registry-live-test.el's identical setup.
    (unless (default-boundp 'ghostel--process)
      (setq-default ghostel--process nil))

    ;; ==========================================================================
    ;; Phase 9: fake `ghostel-exec' stub, duplicated from
    ;; claude-term-registry-live-test.el rather than shared (see that
    ;; file's own Commentary for why) -- attaches a real dummy `sleep'
    ;; process so `claude-term--exec''s real registry put/remove call
    ;; sites, and therefore this phase's real create/remove hooks, run
    ;; end to end without a real `claude'/`ghostel' subprocess.
    ;; ==========================================================================

    (defvar edmacs-sidebar-agents-live-test--claude-term-spawn-log nil)

    (defun edmacs-sidebar-agents-live-test--fake-ghostel-sentinel (process event)
      (let ((buf (process-buffer process)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (run-hook-with-args 'ghostel-exit-functions buf event))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (unless ghostel-kill-buffer-on-exit
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert "\n[Process exited]\n"))))))))

    (defun edmacs-sidebar-agents-live-test--fake-ghostel-exec (buffer program args)
      (push (list buffer program args) edmacs-sidebar-agents-live-test--claude-term-spawn-log)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)) (erase-buffer))
        (let ((proc (start-process "edmacs-sidebar-agents-live-test-claude-term" buffer
                                    "sleep" "3600")))
          (set-process-sentinel proc #'edmacs-sidebar-agents-live-test--fake-ghostel-sentinel)
          (set-process-query-on-exit-flag proc nil)
          (setq-local ghostel--process proc)
          proc)))

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
              (status-ts (float-time)) (title "Claude Code") (source 'claude-term)
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
    ;; AC2 -- RET visit: claude-term row pops to its own window
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-live-test-visit-claude-term-source-pops-to-its-window ()
      "An in-Emacs (phase 9) agent row pops to its own window instead of
touching tmux at all."
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let* ((agent (edmacs-sidebar-agents-live-test--make-agent
                       :root "/repo/wt/" :source 'claude-term :locator "the-buffer"))
               (selected nil))
          (puthash (edmacs-agent-key agent) agent edmacs-agents--table)
          (cl-letf (((symbol-function 'claude-term--pop-to-window)
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

    (ert-deftest edmacs-sidebar-agents-live-test-elapsed-timer-rearms-after-firing ()
      "The elapsed-render timer keeps ticking every interval, not just
once -- regression test for the one-shot-timer rearm bug: `timerp'
stays non-nil on an already-fired one-shot timer object (it has simply
left `timer-list'), so a rearm guard that only checks `timerp' would
silently never fire again while conditions stay tick-worthy."
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let* ((edmacs-sidebar-agents-elapsed-interval 0.05)
               (agent (edmacs-sidebar-agents-live-test--make-agent :status 'working))
               (redraw-count 0))
          (puthash (edmacs-agent-key agent) agent edmacs-agents--table)
          (unwind-protect
              (progn
                (edmacs-sidebar-show (selected-frame))
                (cl-letf* ((orig (symbol-function 'edmacs-sidebar-agents--redraw-all))
                           ((symbol-function 'edmacs-sidebar-agents--redraw-all)
                            (lambda () (cl-incf redraw-count) (funcall orig))))
                  (edmacs-sidebar-agents--ensure-elapsed-timer)
                  (should (edmacs-sidebar-agents-live-test--wait-until
                           (lambda () (>= redraw-count 2)) 3.0))))
            (edmacs-sidebar-agents-live-test--cleanup-sidebar (selected-frame))))))

    ;; ==========================================================================
    ;; Point preservation across a heartbeat-only redraw (agent rows)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-live-test-point-survives-heartbeat-redraw ()
      "A heartbeat-only redraw that replaces an agent's struct with a
fresh one under the same key (mirroring `edmacs-agents-set-status',
which always builds a new struct rather than mutating one in place)
still resolves point back onto the same agent row afterward, rather
than degrading to the enclosing worktree row: `edmacs-sidebar-agent-
section's `magit-section-ident-value' specializer (sidebar-agents.el)
keys identity on `edmacs-agent-key', not the raw (and now different)
struct, so the row's `magit-section-ident' stays stable across the
refresh and `magit-section-goto-successor' finds the row itself."
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let ((agent (edmacs-sidebar-agents-live-test--make-agent
                      :root "/repo/wt/" :status 'working :status-ts (float-time)))
              (tab-count-before (length (tab-bar-tabs))))
          (puthash (edmacs-agent-key agent) agent edmacs-agents--table)
          (unwind-protect
              (progn
                (tab-bar-new-tab)
                (setf (alist-get edmacs-sidebar-agents-live-test--root-parameter
                                  (cdr (tab-bar--current-tab-find)))
                      "/repo/wt/")
                (tab-bar-change-tab-group "repo")
                (edmacs-sidebar-show (selected-frame))
                (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                  (goto-char (oref (edmacs-sidebar--find-agent-section (edmacs-agent-key agent))
                                    start))
                  ;; `--capture-positions' reads each window's own
                  ;; `window-point', which redisplay (never run under
                  ;; `-Q --batch') would otherwise sync from the
                  ;; buffer's actual point on its own.
                  (when-let* ((window (edmacs-sidebar--window (selected-frame))))
                    (set-window-point window (point)))
                  (should (eq (oref (magit-current-section) type) 'edmacs-sidebar-agent))
                  (let ((refreshed (edmacs-sidebar-agents-live-test--make-agent
                                    :root (edmacs-agent-root agent)
                                    :instance (edmacs-agent-instance agent)
                                    :status (edmacs-agent-status agent)
                                    :status-ts (+ (edmacs-agent-status-ts agent) 5)
                                    :title (edmacs-agent-title agent)
                                    :source (edmacs-agent-source agent))))
                    (puthash (edmacs-agent-key refreshed) refreshed edmacs-agents--table))
                  (edmacs-sidebar--redraw (selected-frame))
                  (should (eq (oref (magit-current-section) type) 'edmacs-sidebar-agent))
                  (should (equal (edmacs-agent-key (oref (magit-current-section) value))
                                 (edmacs-agent-key agent)))))
            (edmacs-sidebar-agents-live-test--cleanup-sidebar (selected-frame))
            (while (> (length (tab-bar-tabs)) tab-count-before)
              (tab-bar-close-tab))))))

    ;; ==========================================================================
    ;; Phase 9 -- a REAL claude-term row, visited and reaped end to end
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-live-test-real-claude-term-row-visit-and-kill ()
      "A session spawned via the real `claude-term--exec' registers a real
source=`claude-term' `edmacs-agent' row (via phase 9's
`claude-term-registry-create-functions' -> `claude-term-agents--on-create'
chain, not a synthetic stub); `edmacs-sidebar-agents-visit' on that row
selects the real session buffer's window and clears its unread flag;
and killing the session (real `claude-term-kill' -> `claude-term--on-exit'
-> `claude-term-registry-remove' -> `claude-term-registry-remove-functions')
removes the row from `edmacs-agents--table'."
      (edmacs-sidebar-agents-live-test--with-clean-state
        (let* ((claude-term-registry--table (make-hash-table :test #'equal))
               (edmacs-sidebar-agents-live-test--claude-term-spawn-log nil)
               (root (file-name-as-directory
                      (make-temp-file "edmacs-sidebar-agents-live-test-ct-root-" t)))
               (instance "ghostel-live-1")
               (buf (get-buffer-create (claude-term-buffer-name root instance))))
          (unwind-protect
              (cl-letf (((symbol-function 'claude-term--ensure-ghostel) #'ignore)
                        ((symbol-function 'ghostel-exec)
                         #'edmacs-sidebar-agents-live-test--fake-ghostel-exec))
                (claude-term--exec buf root instance nil)
                (let* ((key (edmacs-agents--key root instance))
                       (agent (gethash key edmacs-agents--table)))
                  (should agent)
                  (should (eq (edmacs-agent-source agent) 'claude-term))
                  (should (eq (edmacs-agent-locator agent) buf))
                  ;; Force it unread so the visit's clearing below is a
                  ;; real assertion, not vacuous against the freshly
                  ;; spawned `idle' default. Rebuilds via `make-edmacs-agent'
                  ;; and `puthash', like every other test in this file,
                  ;; rather than `setf' on the struct directly -- a plain
                  ;; struct-slot `setf' written inside a test nested in
                  ;; this file's own `if'/`progn' guard around a missing
                  ;; `magit-section' build can hit a real (if obscure)
                  ;; `gv'/`cl-defstruct' inlining gap in that specific
                  ;; nesting shape and signal a spurious `void-function
                  ;; (setf edmacs-agent-unread)' -- unrelated to anything
                  ;; this phase's production code does (its own `setf'
                  ;; calls live in plain top-level `defun's, unaffected).
                  (puthash key
                           (make-edmacs-agent
                            :key (edmacs-agent-key agent) :root (edmacs-agent-root agent)
                            :instance (edmacs-agent-instance agent)
                            :status (edmacs-agent-status agent)
                            :status-ts (edmacs-agent-status-ts agent)
                            :updated-ts (edmacs-agent-updated-ts agent)
                            :title (edmacs-agent-title agent)
                            :source (edmacs-agent-source agent)
                            :locator (edmacs-agent-locator agent)
                            :unread t)
                           edmacs-agents--table)
                  (setq agent (gethash key edmacs-agents--table))
                  (edmacs-sidebar-show (selected-frame))
                  (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                    (let ((inhibit-read-only t))
                      (goto-char (point-max))
                      ;; Wrapped in an outer section: an unwrapped
                      ;; top-level `magit-insert-section' call becomes
                      ;; `magit-root-section' itself -- see the earlier
                      ;; visit test's own comment on this same quirk.
                      (magit-insert-section (edmacs-sidebar-agents-live-test-root)
                        (magit-insert-section (edmacs-sidebar-agent agent)
                          (magit-insert-heading "row"))))
                    (goto-char (point-max))
                    (forward-line -1)
                    (edmacs-sidebar-agents-visit))
                  (should (eq (window-buffer (selected-window)) buf))
                  (should-not (edmacs-agent-unread (gethash key edmacs-agents--table)))
                  (claude-term-kill buf)
                  (should (edmacs-sidebar-agents-live-test--wait-until
                           (lambda () (not (buffer-live-p buf))) 3.0))
                  (should-not (gethash key edmacs-agents--table))))
            (edmacs-sidebar-agents-live-test--cleanup-sidebar (selected-frame))
            (when (buffer-live-p buf) (kill-buffer buf))
            (ignore-errors (delete-directory root t))))))

    )) ; end of build-root-found branch

;;; sidebar-agents-live-test.el ends here
