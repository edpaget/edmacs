;;; claude-term-live-test.el --- Simulated live-process regression tests -*- lexical-binding: t -*-

;;; Commentary:
;; claude-term-test.el intentionally covers only pure helpers, since no
;; `ghostel' package is bootstrapped in this repository's straight
;; lockfile yet (see modules/claude-term.el's commentary on
;; `use-package ghostel').  That leaves the kill/restart/exit lifecycle
;; -- the phase's actual "Done when" clause -- verified only by manual
;; M-x use.  This file closes that gap as far as a batch harness can:
;; it drives `claude-term--exec', `claude-term-kill', and
;; `claude-term-restart' against REAL Emacs subprocesses (plain `sleep'
;; stand-ins for `claude') through Emacs's genuine async process
;; machinery, with `ghostel-exec' replaced by a stub whose sentinel is a
;; faithful re-implementation of ghostel.el's own `ghostel--sentinel'
;; tail (lisp/ghostel.el:3821-3841 in dakra/ghostel, fetched from
;; GitHub main 2026-09-01 -- no lockfile pin exists yet to read
;; instead).  That tail is exactly what a prior code review found this
;; module's restart path was racing: `ghostel-exit-functions' runs
;; first, and THEN, unconditionally, if `ghostel-kill-buffer-on-exit'
;; is nil, a "[Process exited]" banner is stamped into the buffer --
;; regardless of whether the hook already attached a new live process.
;; `claude-term-live-test-restart-no-stray-banner' below reproduces
;; that race with a real process and asserts the banner never survives
;; into the restarted buffer.
;;
;; What this does NOT cover: the real `ghostel' package's terminal
;; rendering, `ghostel-mode', or the actual `claude' CLI -- those need
;; an interactive Emacs and are out of scope for a batch harness.
;;
;; Two further tests close gaps the same way, when this checkout's own
;; straight repos happen to be present on disk (true on any machine
;; that has actually run edmacs/eldev at least once locally, though not
;; in a bare checkout or a fresh worktree -- both `ert-skip', not
;; silently pass, when the source is absent):
;;
;; - `claude-term-live-test-real-evil-ghostel-escape-dispatches-to-evil'
;;   reads `evil-ghostel--escape's actual, unmodified `defun' form
;;   straight out of the vendored source with `read' and `eval's just
;;   that one form -- not the whole file, whose other top-level forms
;;   reference `ghostel-mode-map' and other infrastructure this harness
;;   does not bootstrap.  That exercises the REAL package's dispatch
;;   logic (not `claude-term.el's byte-compiler `defvar' stand-in for
;;   `evil-ghostel-escape') and proves the buffer-local `'evil value
;;   `claude-term--configure-evil-escape' sets actually routes ESC to
;;   `evil-force-normal-state', never to the terminal.
;; - `claude-term-live-test-full-transcript-remains-searchable' spawns a
;;   real subprocess through `claude-term--exec' that prints far more
;;   lines than a typical window height, and proves the whole output
;;   stays present and reachable by `search-backward' -- the primitive
;;   `isearch' is built on -- rather than being cleared or truncated.
;;   This is the load-bearing property the phase's inline-renderer
;;   decision (`"tui": "default"', the companion dotfiles-repo commit)
;;   depends on for isearch to traverse the whole transcript.
;; - `claude-term-live-test-claude-cli-editor-mode-not-vim' reads every
;;   settings-tier file the Claude CLI resolves `editorMode' from and
;;   asserts none of them sets it to `"vim"'. Unlike the two tests
;;   above, this needs no stub: it reads real files on disk (skipping
;;   ones that don't exist) rather than driving a subprocess or the
;;   vendored package source.
;; - `claude-term-live-test-exec-locals-survive-real-ghostel-mode' drives
;;   the REAL `ghostel-exec' and `ghostel-mode' (not a stub) through a
;;   first spawn, proving `claude-term--exec's own buffer-locals survive
;;   `ghostel-mode's `kill-all-local-variables' wipe. It never loads
;;   `evil-ghostel', so it says nothing about the ESC-routing hook
;;   below -- and the escape-dispatch test above never drives a real
;;   `ghostel-mode-hook' chain at all, so neither proves the two hooks'
;;   depth ordering actually holds together in one live buffer.
;; - `claude-term-live-test-real-ghostel-mode-hook-ordering-sets-evil-escape'
;;   closes exactly that join: it drives the REAL `ghostel-mode-hook'
;;   chain -- this module's `claude-term--configure-evil-escape',
;;   already permanently on the hook at depth -90 once `claude-term.el'
;;   loads, PLUS a real `evil-ghostel-mode' added at the default depth
;;   for the test's duration (mirroring `use-package evil-ghostel's
;;   `:hook (ghostel-mode . evil-ghostel-mode)' form, which never fires
;;   under this bare batch harness) -- through one real first spawn, and
;;   asserts the buffer-local `evil-ghostel--escape-mode' comes out
;;   `'evil, not the global default `'auto. That is only possible if the
;;   depth -90 hook actually finishes before `evil-ghostel-mode' reads
;;   `evil-ghostel-escape' on enable.
;; - `claude-term-live-test-real-evil-ghostel-c-g-dispatches-to-send-escape'
;;   closes the matching gap on the C-g/interrupt side: with the real
;;   `claude-term-mode' and `evil-ghostel-mode' both active in a real
;;   `ghostel-mode' claude-term buffer and evil actually in insert state,
;;   `(key-binding (kbd "C-g"))' -- exactly what a real keypress resolves
;;   through, via evil's minor-mode/auxiliary/emulation keymaps, not a
;;   plain `lookup-key' against a base map -- must return
;;   `claude-term-send-escape', and invoking it must call
;;   `ghostel-send-string' with a literal ESC.
;; - `claude-term-live-test-real-evil-ghostel-c-g-scoped-to-claude-term-buffers-only'
;;   closes the "and only those" half of AC2: in a real `evil-ghostel-mode'
;;   buffer that is NOT a claude-term buffer (so `claude-term-mode' is
;;   never turned on), the same real key-binding resolution for C-g must
;;   NOT return `claude-term-send-escape' -- it must fall through to
;;   ghostel's own default (`ghostel-send-C-g', bound on `ghostel-mode-map').
;;   Before this, `claude-term-send-escape' was bound directly on the
;;   shared, package-global `evil-ghostel-mode-map', so `(key-binding (kbd
;;   "C-g"))' resolved to it in EVERY `evil-ghostel-mode' buffer regardless
;;   of buffer identity, and only this command's own internal
;;   `claude-term--parse-buffer-name' branch (exercised in isolation by
;;   `claude-term-test-send-escape-falls-through-to-C-g-elsewhere') kept
;;   behavior correct -- real key-binding resolution itself did not scope
;;   the override. The fix binds C-g via `evil-define-minor-mode-key' on
;;   the marker minor mode `claude-term-mode' instead of `evil-define-key*'
;;   on `evil-ghostel-mode-map', so the binding is reachable only while
;;   that mode is buffer-locally on.
;;
;; Run:
;;   emacs -Q --batch -l ert -l modules/claude-term.el \
;;         -l modules/claude-term-registry.el \
;;         -l modules/claude-term-live-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; `claude-term-registry.el' must load ahead of this file: phase 4 wired
;; registry put/touch/remove calls directly into `claude-term--exec',
;; the entry point, and `claude-term--on-exit' (see that module's own
;; Commentary), so every one of those call sites here now needs the
;; real registry functions defined, not merely forward-declared.

(require 'ert)
(require 'cl-lib)

;; The real `inheritenv' package is not bootstrapped under `-Q --batch'
;; (straight.el is absent), but `claude-term--exec' unconditionally
;; wraps its `ghostel-exec' call in `(inheritenv ...)'.  Production
;; startup always bootstraps straight first (see claude-term.el's own
;; commentary on this exact gap for `inheritenv' itself), so this shim
;; is test-only: a plain `progn' is exactly what `inheritenv' reduces
;; to for a local, non-Tramp process, which is all this harness spawns.
(unless (fboundp 'inheritenv)
  (defmacro inheritenv (&rest body)
    `(progn ,@body)))

(defvar claude-term-live-test--spawn-log nil
  "List of (BUFFER PROGRAM . ARGS) recorded by the `ghostel-exec' stub.")

(defun claude-term-live-test--fake-ghostel-sentinel (process event)
  "Re-implementation of ghostel.el's `ghostel--sentinel' tail for PROCESS/EVENT.
Runs `ghostel-exit-functions' first, then -- unconditionally, exactly as
real ghostel does -- stamps a \"[Process exited]\" banner into the
buffer when `ghostel-kill-buffer-on-exit' is nil, whether or not the
hook already replaced the buffer's live process."
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

(defun claude-term-live-test--fake-exec (buffer program args)
  "Stub for `ghostel-exec': attach a real dummy process to BUFFER.
Ignores PROGRAM beyond logging it; always spawns \"sleep 3600\" as a
stand-in long-lived process.  Erases BUFFER first, mirroring
`ghostel-exec' -> `ghostel--init-buffer's `erase-buffer' -- this is
what discards the stray exit banner on a real restart."
  (push (list buffer program args) claude-term-live-test--spawn-log)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)) (erase-buffer))
    (let ((proc (start-process "claude-term-live-test" buffer "sleep" "3600")))
      (set-process-sentinel proc #'claude-term-live-test--fake-ghostel-sentinel)
      ;; Batch Emacs has no minibuffer to answer `kill-buffer's "has a
      ;; running process, kill it?" query, which `end-of-file's on
      ;; stdin; these are throwaway test processes, never query.
      (set-process-query-on-exit-flag proc nil)
      (setq-local ghostel--process proc)
      proc)))

(defun claude-term-live-test--wait-until (pred &optional timeout)
  "Pump the process event loop until PRED is non-nil or TIMEOUT elapses.
TIMEOUT defaults to 2 seconds.  Returns PRED's final value."
  (let ((deadline (+ (float-time) (or timeout 2))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defmacro claude-term-live-test--with-stubs (&rest body)
  "Run BODY with `ghostel-exec' and `claude-term--ensure-ghostel' stubbed.
`claude-term--ensure-ghostel' is stubbed to a no-op since the real
`ghostel' package is not installed in this batch harness (see
Commentary); `ghostel-exec' is replaced with the real-subprocess stub
above."
  `(let ((claude-term-live-test--spawn-log nil))
     (cl-letf (((symbol-function 'claude-term--ensure-ghostel) #'ignore)
               ((symbol-function 'ghostel-exec) #'claude-term-live-test--fake-exec))
       ,@body)))

(defmacro claude-term-live-test--with-buffer (var name &rest body)
  "Bind VAR to a fresh buffer named NAME for BODY, killing it (and any
live process still attached) afterward regardless of how BODY exits."
  (declare (indent 2))
  `(let ((,var (generate-new-buffer ,name)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p ,var)
         (ignore-errors
           (let ((proc (buffer-local-value 'ghostel--process ,var)))
             (when (process-live-p proc) (kill-process proc))))
         (kill-buffer ,var)))))

(ert-deftest claude-term-live-test-kill-cleans-up-buffer ()
  "`claude-term-kill' leaves no stale buffer once the sentinel fires."
  (claude-term-live-test--with-stubs
   (claude-term-live-test--with-buffer buf "*claude-term-live-test:kill*"
     (claude-term--exec buf (temporary-file-directory) nil nil)
     (should (process-live-p (buffer-local-value 'ghostel--process buf)))
     (claude-term-kill buf)
     (should (claude-term-live-test--wait-until
              (lambda () (not (buffer-live-p buf)))))
     (should-not (get-buffer "*claude-term-live-test:kill*")))))

(ert-deftest claude-term-live-test-plain-exit-cleans-up ()
  "A process exiting on its own (not via `claude-term-kill') is cleaned up
identically -- `claude-term--on-exit' is the single point of truth
regardless of how the process died."
  (claude-term-live-test--with-stubs
   (claude-term-live-test--with-buffer buf "*claude-term-live-test:plain-exit*"
     (claude-term--exec buf (temporary-file-directory) nil nil)
     (let ((proc (buffer-local-value 'ghostel--process buf)))
       (should (process-live-p proc))
       (kill-process proc))
     (should (claude-term-live-test--wait-until
              (lambda () (not (buffer-live-p buf))))))))

(ert-deftest claude-term-live-test-restart-no-stray-banner ()
  "Restarting a live session leaves the buffer's NAME and identity intact,
attaches a genuinely new live process, and -- the prior code review's
finding -- never lets ghostel's unconditional post-hook \"[Process
exited]\" banner survive into the restarted buffer."
  (claude-term-live-test--with-stubs
   (claude-term-live-test--with-buffer buf "*claude-term-live-test:restart*"
     (claude-term--exec buf (temporary-file-directory) nil '("--x"))
     (let ((old-pid (process-id (buffer-local-value 'ghostel--process buf))))
       (claude-term-restart buf)
       (should (claude-term-live-test--wait-until
                (lambda ()
                  (and (buffer-live-p buf)
                       (process-live-p (buffer-local-value 'ghostel--process buf))
                       (not (eql (process-id (buffer-local-value 'ghostel--process buf))
                                 old-pid))))))
       ;; Give any further pending sentinel/timer activity a moment to
       ;; settle before asserting on final buffer state.
       (accept-process-output nil 0.3)
       (should (buffer-live-p buf))
       (should (equal (buffer-name buf) "*claude-term-live-test:restart*"))
       (should-not (buffer-local-value 'claude-term--restarting buf))
       (with-current-buffer buf
         (should-not (string-match-p "Process exited" (buffer-string))))
       ;; The restart re-exec must reuse the args frozen at original
       ;; spawn, never re-resolve them -- two log entries, same args.
       (should (equal (length claude-term-live-test--spawn-log) 2))
       (should (equal (nth 2 (nth 0 claude-term-live-test--spawn-log)) '("--x")))
       (should (equal (nth 2 (nth 1 claude-term-live-test--spawn-log)) '("--x")))))))

(ert-deftest claude-term-live-test-restart-noop-while-in-flight ()
  "Firing `claude-term-restart' twice in immediate succession is a no-op
the second time -- no second `kill-process' call, no duplicate re-exec."
  (claude-term-live-test--with-stubs
   (claude-term-live-test--with-buffer buf "*claude-term-live-test:double-restart*"
     (claude-term--exec buf (temporary-file-directory) nil nil)
     (claude-term-restart buf)
     (should (buffer-local-value 'claude-term--restarting buf))
     ;; Second call while the flag is still set: must not error and must
     ;; not attempt another `kill-process' on an already-dying process.
     (claude-term-restart buf)
     (should (claude-term-live-test--wait-until
              (lambda ()
                (and (buffer-live-p buf)
                     (process-live-p (buffer-local-value 'ghostel--process buf))))))
     (accept-process-output nil 0.3)
     ;; Exactly two spawns total: the original plus the one restart.
     (should (equal (length claude-term-live-test--spawn-log) 2)))))

;; ============================================================================
;; Entry point (`claude-term') coverage
;; ============================================================================
;; Everything above drives `claude-term--exec' directly.  The tests below
;; stub `project-current'/`project-root' as well, so the real `claude-term'
;; command itself is exercised end to end: root resolution threaded into
;; `default-directory' and the spawned process's argv, the
;; existing-live-session switch branch, and the leaf-collision
;; instance-resolution branch.

;; `claude-term.el' declares `ghostel--process' via a bare `(defvar
;; ghostel--process)' for byte-compiler purposes only; its buffer-local nil
;; default normally comes from `require'ing the real `ghostel' package
;; inside `claude-term--ensure-ghostel', which the entry point calls before
;; ever creating a buffer.  That require is stubbed to a no-op by
;; `claude-term-live-test--with-stubs', so install the same nil default
;; here -- needed by `claude-term''s own live-process check on a brand-new
;; buffer, ahead of any call to `claude-term--exec'.
(unless (default-boundp 'ghostel--process)
  (setq-default ghostel--process nil))

(defmacro claude-term-live-test--with-project (root &rest body)
  "Stub `project-current'/`project-root' to report ROOT for BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'project-current)
              (lambda (&rest _) 'claude-term-live-test--fake-project))
             ((symbol-function 'project-root)
              (lambda (_proj) ,root)))
     ,@body))

(defmacro claude-term-live-test--cleanup-named-buffers (names &rest body)
  "Run BODY, then kill (and any live process of) each buffer in NAMES.
Unlike `claude-term-live-test--with-buffer', these buffers are created
by `claude-term' itself under names it computes, not by the test, so
cleanup happens by name afterward rather than via a single
`generate-new-buffer' call wrapped in `unwind-protect'."
  (declare (indent 1))
  `(unwind-protect
       (progn ,@body)
     (dolist (name ,names)
       (let ((buf (get-buffer name)))
         (when (buffer-live-p buf)
           (ignore-errors
             (let ((proc (buffer-local-value 'ghostel--process buf)))
               (when (process-live-p proc) (kill-process proc))))
           (kill-buffer buf))))))

(ert-deftest claude-term-live-test-entry-point-threads-root-and-args ()
  "`claude-term' resolves the project root and final args at the entry
point, spawns into a buffer whose `default-directory' is that root, and
passes `claude-term-extra-args' merged with the call's own extra args
through to the spawned process -- exercising the real `claude-term'
command, not just `claude-term--exec' directly."
  (claude-term-live-test--with-stubs
   (let* ((root (file-name-as-directory
                 (make-temp-file "claude-term-entry-test-" t)))
          (claude-term-extra-args '("--foo"))
          (name (claude-term-buffer-name root)))
     (unwind-protect
         (claude-term-live-test--with-project root
           (claude-term-live-test--cleanup-named-buffers (list name)
             (claude-term nil '("--bar"))
             (let ((buf (get-buffer name)))
               (should buf)
               (should (process-live-p (buffer-local-value 'ghostel--process buf)))
               (with-current-buffer buf
                 (should (equal (file-truename default-directory) (file-truename root))))
               (should (equal (length claude-term-live-test--spawn-log) 1))
               (should (equal (nth 2 (car claude-term-live-test--spawn-log))
                               '("--foo" "--bar"))))))
       (ignore-errors (delete-directory root t))))))

(ert-deftest claude-term-live-test-entry-point-switches-to-existing-live-session ()
  "A second `claude-term' call for the same project/instance while the
first session is still live switches to it instead of respawning."
  (claude-term-live-test--with-stubs
   (let* ((root (file-name-as-directory
                 (make-temp-file "claude-term-entry-test-" t)))
          (name (claude-term-buffer-name root)))
     (unwind-protect
         (claude-term-live-test--with-project root
           (claude-term-live-test--cleanup-named-buffers (list name)
             (claude-term)
             (let ((first-buf (get-buffer name)))
               (claude-term)
               (should (eq (current-buffer) first-buf))
               ;; Still only one spawn -- the second call switched to the
               ;; live buffer, it did not re-exec.
               (should (equal (length claude-term-live-test--spawn-log) 1)))))
       (ignore-errors (delete-directory root t))))))

(ert-deftest claude-term-live-test-entry-point-resolves-instance-collision ()
  "Two different projects that happen to share a leaf directory name do
not collide on the bare-leaf buffer name -- the second spawn is
auto-disambiguated into an instance slot instead of reusing or
clobbering the first project's live buffer."
  (claude-term-live-test--with-stubs
   (let* ((parent1 (make-temp-file "claude-term-collision-1-" t))
          (parent2 (make-temp-file "claude-term-collision-2-" t))
          (root1 (file-name-as-directory (expand-file-name "proj" parent1)))
          (root2 (file-name-as-directory (expand-file-name "proj" parent2)))
          (name1 (claude-term-buffer-name root1))
          (name2-collided (claude-term-buffer-name root2))
          (name2-resolved (claude-term-buffer-name root2 "2")))
     (make-directory root1 t)
     (make-directory root2 t)
     ;; Both roots share the bare leaf "proj" -- this is the collision the
     ;; test exercises, not an incidental setup detail.
     (should (equal name1 name2-collided))
     (unwind-protect
         (claude-term-live-test--cleanup-named-buffers (list name1 name2-resolved)
           (claude-term-live-test--with-project root1 (claude-term))
           (claude-term-live-test--with-project root2 (claude-term))
           (let ((buf1 (get-buffer name1)))
             (should buf1)
             (should (equal (buffer-local-value 'claude-term--root buf1) root1)))
           (let ((buf2 (get-buffer name2-resolved)))
             (should buf2)
             (should (equal (buffer-local-value 'claude-term--root buf2) root2)))
           (should (equal (length claude-term-live-test--spawn-log) 2)))
       (ignore-errors (delete-directory parent1 t))
       (ignore-errors (delete-directory parent2 t))))))

;; ============================================================================
;; Real evil-ghostel escape dispatch (source-extracted, when available)
;; ============================================================================
;; See this file's Commentary for why this reads the real `defun' form
;; out of the vendored source instead of loading the whole file or
;; reimplementing the dispatch logic.

(defconst claude-term-live-test--evil-ghostel-source
  (expand-file-name
   "../straight/repos/ghostel/extensions/evil-ghostel/evil-ghostel.el"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the real evil-ghostel.el, when this checkout has fetched it.
Computed relative to this file (modules/claude-term-live-test.el) so it
resolves correctly regardless of which worktree/checkout runs the
tests; straight installs package repos inside the Emacs config
directory itself (see modules/claude-term.el's own commentary), so this
is the same path production `use-package evil-ghostel' resolves to.")

(defun claude-term-live-test--load-real-escape-defun ()
  "Eval the real, unmodified `evil-ghostel--escape' `defun' form.
Reads it out of `claude-term-live-test--evil-ghostel-source' with
`read' rather than loading the whole file (see Commentary). Returns
non-nil on success; nil (without erroring) when the source file is not
present on disk."
  (when (file-exists-p claude-term-live-test--evil-ghostel-source)
    (with-temp-buffer
      (insert-file-contents claude-term-live-test--evil-ghostel-source)
      (goto-char (point-min))
      (when (re-search-forward "^(defun evil-ghostel--escape " nil t)
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t)
        t))))

(ert-deftest claude-term-live-test-real-evil-ghostel-escape-dispatches-to-evil ()
  "With `evil-ghostel--escape-mode' `'evil' -- exactly what the real
`evil-ghostel-mode' copies from `evil-ghostel-escape' on enable, which
`claude-term--configure-evil-escape' sets buffer-locally to `'evil' --
the REAL `evil-ghostel--escape' (loaded from the vendored source, not
reimplemented) must call the evil insert-state escape path and must
never reach the terminal path.  `evil-ghostel--escape' short-circuits
`ghostel-alt-screen-p' entirely when the mode is not `'auto', so no
alt-screen stub is needed for this branch.

`evil-insert-state-map' is stubbed here with the SAME binding core
evil itself installs (`(define-key evil-insert-state-map [escape]
\\='evil-normal-state)', evil-maps.el:483) rather than an empty keymap,
so `evil-ghostel--escape's `(commandp cmd)' check finds a real command
and takes the `(call-interactively cmd)' branch -- the branch
production actually takes once evil is loaded (this module's
`use-package evil-ghostel' declares `:after (ghostel evil)').  An
empty keymap would leave `cmd' nil and silently exercise the
`evil-force-normal-state' fallback instead, which is not the path
production runs and would give false assurance if that fallback ever
broke.  This test asserts both that `evil-normal-state' fired and that
the fallback did not."
  (if (not (claude-term-live-test--load-real-escape-defun))
      (ert-skip "No local straight checkout of ghostel/evil-ghostel found (see claude-term-live-test--evil-ghostel-source); run again after eldev/init.el has bootstrapped it once locally to exercise this.")
    (defvar evil-ghostel--escape-mode)
    (defvar evil-insert-state-map)
    (let* ((evil-ghostel--escape-mode 'evil)
           (evil-insert-state-map (make-sparse-keymap))
           (normal-state-calls 0)
           (force-normal-state-calls 0))
      (define-key evil-insert-state-map [escape] 'evil-normal-state)
      (cl-letf (((symbol-function 'evil-normal-state)
                 (lambda () (interactive) (cl-incf normal-state-calls)))
                ((symbol-function 'evil-force-normal-state)
                 (lambda () (interactive) (cl-incf force-normal-state-calls)))
                ((symbol-function 'ghostel--on-user-input)
                 (lambda () (error "evil-ghostel--escape reached the terminal path with mode 'evil")))
                ((symbol-function 'ghostel--send-encoded)
                 (lambda (&rest _) (error "evil-ghostel--escape reached the terminal path with mode 'evil"))))
        (evil-ghostel--escape)
        (should (= normal-state-calls 1))
        (should (= force-normal-state-calls 0))))))

;; ============================================================================
;; Full transcript remains searchable (AC4 support)
;; ============================================================================

(defun claude-term-live-test--fake-exec-transcript (buffer program args)
  "Stub for `ghostel-exec' spawning a real process that prints 200 lines.
Stands in for ghostel's own real terminal-output insertion (ghostel
itself is not bootstrapped here -- see this file's Commentary); relies
on Emacs's default process filter, which inserts a process's output
into its buffer when no explicit filter is set. Used only by
`claude-term-live-test-full-transcript-remains-searchable'."
  (push (list buffer program args) claude-term-live-test--spawn-log)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)) (erase-buffer))
    (let ((proc (start-process
                 "claude-term-live-test-transcript" buffer
                 "sh" "-c"
                 "i=1; while [ $i -le 200 ]; do echo \"transcript-line-$i\"; i=$((i+1)); done; sleep 3600")))
      (set-process-sentinel proc #'claude-term-live-test--fake-ghostel-sentinel)
      (set-process-query-on-exit-flag proc nil)
      (setq-local ghostel--process proc)
      proc)))

(ert-deftest claude-term-live-test-full-transcript-remains-searchable ()
  "The whole transcript stays in the buffer as ordinary text once a
session's output exceeds a screenful, and `search-backward' -- the
primitive `isearch' is built on -- finds text that has scrolled past
point.  This is the load-bearing property behind AC4 (isearch traverses
the whole transcript): the phase's inline-renderer decision means
claude-term buffers are never cleared to a single visible screen the
way an alt-screen TUI buffer would be."
  (cl-letf (((symbol-function 'claude-term--ensure-ghostel) #'ignore)
            ((symbol-function 'ghostel-exec) #'claude-term-live-test--fake-exec-transcript))
    (let ((claude-term-live-test--spawn-log nil))
      (claude-term-live-test--with-buffer buf "*claude-term-live-test:transcript*"
        (claude-term--exec buf (temporary-file-directory) nil nil)
        (should (claude-term-live-test--wait-until
                 (lambda ()
                   (with-current-buffer buf
                     (string-match-p "transcript-line-200" (buffer-string))))
                 5))
        (with-current-buffer buf
          (should (string-match-p "transcript-line-1\n" (buffer-string)))
          (goto-char (point-max))
          (should (search-backward "transcript-line-1\n" nil t)))))))

;; ============================================================================
;; Claude CLI editor mode stays out of vim (cross-repo prerequisite)
;; ============================================================================
;; The phase body's "disable Claude CLI's own vi-mode via /config ->
;; editor mode `normal'" prerequisite looked, on an earlier pass, like it
;; had no on-disk artifact at all -- `/config' seemed to be the only
;; interface to it. Reading the installed CLI binary's own bundled
;; source (`strings -a "$(which claude)"') tells a fuller story:
;; `editorMode' IS a real settings key, schema enum `["normal" "vim"]',
;; with a DEFAULT of `"normal"' baked into the binary -- the CLI's own
;; resolver falls back to that default when no settings tier sets the
;; key. See modules/claude-term.el's commentary just above
;; `claude-term-send-escape' for the full citation. This test is the
;; regression guard for that finding: it checks every settings tier the
;; CLI reads for an explicit `"editorMode": "vim"' override, rather than
;; treating the whole prerequisite as permanently unverifiable. It is
;; NOT a substitute for the one-time interactive `/config' check -- a
;; future CLI version changing its baked-in default is a gap this test
;; cannot see, only an explicit "vim" override left in a settings file.

(defconst claude-term-live-test--editor-mode-setting-files
  (list (expand-file-name "~/.claude.json")
        (expand-file-name "~/.claude/settings.json")
        (expand-file-name "~/.claude/settings.local.json")
        (expand-file-name "../.claude/settings.json"
                           (file-name-directory (or load-file-name buffer-file-name)))
        (expand-file-name "../.claude/settings.local.json"
                           (file-name-directory (or load-file-name buffer-file-name))))
  "Every settings-tier file the Claude CLI reads `editorMode' from.
Precedence among tiers does not matter for this test -- it only checks
whether any of them sets an explicit \"vim\" override, not what the
resolved value is.")

(defun claude-term-live-test--file-sets-editor-mode-vim-p (file)
  "Non-nil when FILE exists, parses as JSON, and sets editorMode to vim.
Any read or parse failure is treated as \"does not set it\" -- this
test's job is to catch an explicit override, not to validate the
settings file's syntax."
  (and (file-exists-p file)
       (let ((json (condition-case nil
                        (with-temp-buffer
                          (insert-file-contents file)
                          (json-parse-buffer :object-type 'alist))
                      (error nil))))
         (and (listp json)
              (equal (alist-get 'editorMode json) "vim")))))

(ert-deftest claude-term-live-test-claude-cli-editor-mode-not-vim ()
  "No local settings tier overrides the Claude CLI's `editorMode' to
`vim'. With no override present, the CLI's own baked-in default of
`\"normal\"' governs (see this file's Commentary above and
modules/claude-term.el's commentary near `claude-term-send-escape'),
which is what keeps evil-ghostel's ESC handling from fighting the CLI's
own vi-mode (the \"triple-ESC\" symptom in
manzaltu/claude-code-ide.el#52)."
  (require 'json)
  (let ((offenders
         (seq-filter #'claude-term-live-test--file-sets-editor-mode-vim-p
                      claude-term-live-test--editor-mode-setting-files)))
    (should (null offenders))))

;; ---------------------------------------------------------------------------
;; Real `ghostel-mode' lifecycle: the first-spawn `kill-all-local-variables' wipe
;; ---------------------------------------------------------------------------

(defvar claude-term-live-test--ghostel-source
  (expand-file-name
   "../straight/repos/ghostel/lisp/ghostel.el"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the real ghostel.el, when this checkout has fetched it.
Resolved the same way as `claude-term-live-test--evil-ghostel-source'.")

(defconst claude-term-live-test--ghostel-native-fns
  '(ghostel--new ghostel--set-size ghostel--set-palette
    ghostel--set-default-colors ghostel--set-bold-config
    ghostel--redraw ghostel--write-vt ghostel--write-pty
    ghostel--mode-enabled ghostel--alt-screen-p
    ghostel--module-version ghostel--encode-key)
  "Native-module leaf functions stubbed by the real-`ghostel-mode' test.
Only leaves: every one is implemented in ghostel-module.dylib, which a
batch harness cannot load.  All Lisp control flow between
`ghostel-exec' and `ghostel-mode' runs for real, which is the entire
point of the test below.")

(defconst claude-term-live-test--evil-source
  (expand-file-name "../straight/repos/evil/evil.el"
                     (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the real evil.el, when this checkout has fetched it.
Resolved the same way as `claude-term-live-test--evil-ghostel-source';
`evil-ghostel.el' itself `require's `evil', so both need to be on
`load-path' to load the real `evil-ghostel-mode' minor mode (as opposed
to just `read'-and-`eval'-ing one `defun' form out of its source, which
is all `claude-term-live-test--load-real-escape-defun' above needs).")

(defun claude-term-live-test--real-evil-ghostel-stack-available-p ()
  "Non-nil when this checkout has real ghostel, evil, and evil-ghostel sources."
  (and (file-exists-p claude-term-live-test--ghostel-source)
       (file-exists-p claude-term-live-test--evil-ghostel-source)
       (file-exists-p claude-term-live-test--evil-source)))

(defun claude-term-live-test--load-real-evil-ghostel-stack ()
  "Add load-path entries for, and `require', the real ghostel/evil/evil-ghostel.
Callers must first confirm
`claude-term-live-test--real-evil-ghostel-stack-available-p'."
  (add-to-list 'load-path (file-name-directory claude-term-live-test--ghostel-source))
  (add-to-list 'load-path (file-name-directory claude-term-live-test--evil-ghostel-source))
  (add-to-list 'load-path (file-name-directory claude-term-live-test--evil-source))
  (require 'ghostel)
  (require 'evil)
  (require 'evil-ghostel))

(ert-deftest claude-term-live-test-exec-locals-survive-real-ghostel-mode ()
  "`claude-term--exec's buffer-locals must survive a FIRST spawn.

This is the one test that drives the REAL `ghostel-exec' and the REAL
`ghostel-mode', rather than `claude-term-live-test--fake-exec'.  That
distinction is the whole test: `ghostel-exec' -> `ghostel--init-buffer'
runs `(unless (derived-mode-p \\='ghostel-mode) (ghostel-mode))', and
`ghostel-mode' derives from `fundamental-mode', so a buffer's first
spawn runs `kill-all-local-variables' and discards every buffer-local
set before the call.  Every stub in this file bypasses that path and so
cannot see the failure.

Regression guard for the defect tracked by rdm task
`claude-term-exec-buffer-local-wipe': with the setup ordered before
`ghostel-exec', every assertion below fails --
`claude-term--root'/`--instance'/`--args' come back nil,
`ghostel-kill-buffer-on-exit' reverts to its global default of t,
`ghostel-buffer-name-function' is left unpinned (whatever global value,
including a global OSC-title renamer, happened to be in effect) rather
than buffer-locally nil, and `claude-term--on-exit' is never
registered -- which left `claude-term-restart' unable to re-exec and
`claude-term--on-exit' never running in production, while every
stubbed test still passed."
  (unless (file-exists-p claude-term-live-test--ghostel-source)
    (ert-skip "real ghostel.el not present in this checkout"))
  (add-to-list 'load-path
               (file-name-directory claude-term-live-test--ghostel-source))
  (require 'ghostel)
  (let ((stubbed nil)
        (buf (get-buffer-create "*claude-term-live-test-real-mode*")))
    (unwind-protect
        (progn
          ;; Stub only the native leaves, and only those not already
          ;; bound, so a machine with a compiled module still exercises
          ;; the real ones.
          (dolist (fn claude-term-live-test--ghostel-native-fns)
            (unless (fboundp fn)
              (push fn stubbed)
              (fset fn (lambda (&rest _) nil))))
          (cl-letf (((symbol-function 'claude-term--ensure-ghostel)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ghostel--load-module)
                     (lambda (&rest _) t))
                    ((symbol-function 'ghostel--spawn-pty)
                     (lambda (&rest _)
                       (let ((proc (start-process
                                    "claude-term-live-test-real" (current-buffer)
                                    "sleep" "3600")))
                         (set-process-query-on-exit-flag proc nil)
                         (setq-local ghostel--process proc)
                         proc))))
            (claude-term--exec buf (temporary-file-directory) "2" '("--x")))
          (with-current-buffer buf
            ;; Guard the guard: if this ever stops being the real mode,
            ;; the assertions below would pass vacuously.
            (should (derived-mode-p 'ghostel-mode))
            (should (equal claude-term--root (temporary-file-directory)))
            (should (equal claude-term--instance "2"))
            (should (equal claude-term--args '("--x")))
            (should-not ghostel-kill-buffer-on-exit)
            (should (local-variable-p 'ghostel-buffer-name-function))
            (should-not ghostel-buffer-name-function)
            (should (local-variable-p 'ghostel-exit-functions))
            (should (memq #'claude-term--on-exit ghostel-exit-functions))))
      (dolist (fn stubbed) (fmakunbound fn))
      (when-let* ((proc (get-buffer-process buf))) (delete-process proc))
      (kill-buffer buf))))

;; ---------------------------------------------------------------------------
;; Real ghostel-mode-hook chain: depth -90 ordering vs. C-g reachability
;; ---------------------------------------------------------------------------
;; The two tests above each drive one real half of AC1/AC2 in isolation:
;; `claude-term-live-test-real-evil-ghostel-escape-dispatches-to-evil'
;; proves the real `evil-ghostel--escape' defun, given
;; `evil-ghostel--escape-mode' already `'evil, but never drives a real
;; `ghostel-mode-hook' chain to get there; `claude-term-live-test-exec-locals-survive-real-ghostel-mode'
;; drives a real `ghostel-mode-hook' chain, but never loads `evil-ghostel'
;; at all. Neither proves that this module's `claude-term--configure-evil-escape'
;; (permanently on `ghostel-mode-hook' at depth -90) and a real, hooked-on
;; `evil-ghostel-mode' interact correctly in the same live buffer, nor
;; that the `evil-define-key*' call installing `claude-term-send-escape'
;; on `evil-ghostel-mode-map' actually produces a reachable C-g binding.
;; The two tests below close those joins.

(ert-deftest claude-term-live-test-real-ghostel-mode-hook-ordering-sets-evil-escape ()
  "The real `ghostel-mode-hook' chain must leave `evil-ghostel--escape-mode'
`'evil after a first spawn, with this module's `claude-term--configure-evil-escape'
\(already on the hook at depth -90 once `claude-term.el' loaded\) and a
real `evil-ghostel-mode' \(added at the default depth here, mirroring
`use-package evil-ghostel's `:hook' form, which does not fire under
this bare batch harness\) both actually present on `ghostel-mode-hook'.

That value is only reachable if the depth -90 hook runs to completion
--and its `setq-local' survives-- before `evil-ghostel-mode's own
enable body reads the global `evil-ghostel-escape' default \(`'auto\)
into its buffer-local `evil-ghostel--escape-mode'. A regression here
\(wrong depth, or a mode-hook reordering upstream\) would silently leave
this buffer's ESC routed to `'auto -- which sends it to the terminal
outside an alt-screen app, exactly the bug this phase exists to fix."
  (unless (claude-term-live-test--real-evil-ghostel-stack-available-p)
    (ert-skip "real evil/ghostel/evil-ghostel sources not all present in this checkout"))
  (claude-term-live-test--load-real-evil-ghostel-stack)
  (let ((stubbed nil)
        (buf (get-buffer-create "*claude-term:live-test-hook-order*")))
    (unwind-protect
        (progn
          (dolist (fn claude-term-live-test--ghostel-native-fns)
            (unless (fboundp fn)
              (push fn stubbed)
              (fset fn (lambda (&rest _) nil))))
          ;; Mirrors `use-package evil-ghostel's `:hook (ghostel-mode .
          ;; evil-ghostel-mode)' form, at the default (non-negative)
          ;; depth, exactly as `add-hook' installs it without an
          ;; explicit DEPTH argument.
          (add-hook 'ghostel-mode-hook #'evil-ghostel-mode)
          (cl-letf (((symbol-function 'claude-term--ensure-ghostel)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ghostel--load-module)
                     (lambda (&rest _) t))
                    ((symbol-function 'ghostel--spawn-pty)
                     (lambda (&rest _)
                       (let ((proc (start-process
                                    "claude-term-live-test-hook-order" (current-buffer)
                                    "sleep" "3600")))
                         (set-process-query-on-exit-flag proc nil)
                         (setq-local ghostel--process proc)
                         proc))))
            (claude-term--exec buf (temporary-file-directory) nil nil))
          (with-current-buffer buf
            ;; Guard the guard: if `evil-ghostel-mode' never actually
            ;; turned on, or this buffer's name never matched
            ;; `claude-term--configure-evil-escape's own guard, the
            ;; escape-mode assertion below would pass vacuously.
            (should (derived-mode-p 'ghostel-mode))
            (should (bound-and-true-p evil-ghostel-mode))
            (should (eq evil-ghostel--escape-mode 'evil))))
      (remove-hook 'ghostel-mode-hook #'evil-ghostel-mode)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (bound-and-true-p evil-ghostel-mode) (evil-ghostel-mode -1))))
      (dolist (fn stubbed) (fmakunbound fn))
      (when-let* ((proc (get-buffer-process buf))) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest claude-term-live-test-real-evil-ghostel-c-g-dispatches-to-send-escape ()
  "`claude-term-send-escape' must be C-g's REACHABLE insert-state binding
in a real claude-term buffer, and invoking it must send a literal ESC.

`(key-binding (kbd \"C-g\"))' is what a real keypress resolves through
-- evil's minor-mode/auxiliary/emulation-mode-map-alist machinery, not a
plain `lookup-key' against a base map directly (which returns nil even
when the binding is live, since `evil-define-minor-mode-key' installs
state bindings into a separate keymap keyed by the mode symbol, not a
literal keymap object). This buffer's name matches
`claude-term--buffer-name-regexp', so the real `ghostel-mode-hook' chain
(`claude-term--configure-evil-escape' at depth -90) turns on the marker
minor mode `claude-term-mode', which is what C-g's binding is actually
keyed to -- see
`claude-term-live-test-real-evil-ghostel-c-g-scoped-to-claude-term-buffers-only'
immediately below for the negative case proving that scoping is real."
  (unless (claude-term-live-test--real-evil-ghostel-stack-available-p)
    (ert-skip "real evil/ghostel/evil-ghostel sources not all present in this checkout"))
  (claude-term-live-test--load-real-evil-ghostel-stack)
  (let ((stubbed nil)
        (buf (get-buffer-create "*claude-term:live-test-c-g*")))
    (unwind-protect
        (progn
          (dolist (fn claude-term-live-test--ghostel-native-fns)
            (unless (fboundp fn)
              (push fn stubbed)
              (fset fn (lambda (&rest _) nil))))
          (add-hook 'ghostel-mode-hook #'evil-ghostel-mode)
          (cl-letf (((symbol-function 'claude-term--ensure-ghostel)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ghostel--load-module)
                     (lambda (&rest _) t))
                    ((symbol-function 'ghostel--spawn-pty)
                     (lambda (&rest _)
                       (let ((proc (start-process
                                    "claude-term-live-test-c-g" (current-buffer)
                                    "sleep" "3600")))
                         (set-process-query-on-exit-flag proc nil)
                         (setq-local ghostel--process proc)
                         proc))))
            (claude-term--exec buf (temporary-file-directory) nil nil))
          (with-current-buffer buf
            (evil-local-mode 1)
            (evil-insert-state)
            ;; Guard the guard: if evil-ghostel-mode never turned on, or
            ;; evil never actually reached insert state, the binding
            ;; lookup below would not be exercising the real path.
            (should (derived-mode-p 'ghostel-mode))
            (should (bound-and-true-p evil-ghostel-mode))
            (should (eq evil-state 'insert))
            (should (eq (key-binding (kbd "C-g")) #'claude-term-send-escape))
            (let ((sent nil))
              (cl-letf (((symbol-function 'ghostel-send-string)
                         (lambda (s) (push s sent)))
                        ((symbol-function 'ghostel-send-C-g)
                         (lambda () (push :c-g sent))))
                (call-interactively #'claude-term-send-escape))
              (should (equal sent '("\e"))))))
      (remove-hook 'ghostel-mode-hook #'evil-ghostel-mode)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (bound-and-true-p evil-local-mode) (evil-local-mode -1))
          (when (bound-and-true-p evil-ghostel-mode) (evil-ghostel-mode -1))
          (when (bound-and-true-p claude-term-mode) (claude-term-mode -1))))
      (dolist (fn stubbed) (fmakunbound fn))
      (when-let* ((proc (get-buffer-process buf))) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest claude-term-live-test-real-evil-ghostel-c-g-scoped-to-claude-term-buffers-only ()
  "C-g must NOT resolve to `claude-term-send-escape' outside claude-term buffers.

This is the negative half of AC2's \"and only those\" clause, proven at
the level of real evil key-binding resolution rather than only
`claude-term-send-escape's own internal dispatch branch (already
covered, for a plain non-claude-term buffer name, by
`claude-term-test-send-escape-falls-through-to-C-g-elsewhere' -- but
that test calls the command directly, never driving real evil keymap
resolution at all).

This buffer's name does NOT match `claude-term--buffer-name-regexp', so
`claude-term--configure-evil-escape' (on `ghostel-mode-hook' at depth
-90) declines to turn on `claude-term-mode' for it, even though
`evil-ghostel-mode' itself is still turned on (mirroring
`use-package evil-ghostel's global `:hook', which activates the minor
mode for every ghostel buffer regardless of program). With
`claude-term-mode' off, real key-binding resolution for insert-state
C-g must fall through past evil entirely to `ghostel-mode-map's own
default, `ghostel-send-C-g' -- proving the scoping lives in which
keymap the binding is installed on, not merely in a guard inside the
command."
  (unless (claude-term-live-test--real-evil-ghostel-stack-available-p)
    (ert-skip "real evil/ghostel/evil-ghostel sources not all present in this checkout"))
  (claude-term-live-test--load-real-evil-ghostel-stack)
  (let ((stubbed nil)
        (buf (get-buffer-create "*ghostel:live-test-c-g-elsewhere*")))
    (unwind-protect
        (progn
          (dolist (fn claude-term-live-test--ghostel-native-fns)
            (unless (fboundp fn)
              (push fn stubbed)
              (fset fn (lambda (&rest _) nil))))
          (add-hook 'ghostel-mode-hook #'evil-ghostel-mode)
          (cl-letf (((symbol-function 'claude-term--ensure-ghostel)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ghostel--load-module)
                     (lambda (&rest _) t))
                    ((symbol-function 'ghostel--spawn-pty)
                     (lambda (&rest _)
                       (let ((proc (start-process
                                    "claude-term-live-test-c-g-elsewhere" (current-buffer)
                                    "sleep" "3600")))
                         (set-process-query-on-exit-flag proc nil)
                         (setq-local ghostel--process proc)
                         proc))))
            (claude-term--exec buf (temporary-file-directory) nil nil))
          (with-current-buffer buf
            (evil-local-mode 1)
            (evil-insert-state)
            ;; Guard the guard: `evil-ghostel-mode' is still on (the
            ;; global-activation side effect this phase's plan calls
            ;; out), but this buffer's name must not have triggered the
            ;; claude-term-only guard.
            (should (derived-mode-p 'ghostel-mode))
            (should (bound-and-true-p evil-ghostel-mode))
            (should-not (bound-and-true-p claude-term-mode))
            (should (eq evil-state 'insert))
            (should-not (eq (key-binding (kbd "C-g")) #'claude-term-send-escape))
            (should (eq (key-binding (kbd "C-g")) #'ghostel-send-C-g))))
      (remove-hook 'ghostel-mode-hook #'evil-ghostel-mode)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (bound-and-true-p evil-local-mode) (evil-local-mode -1))
          (when (bound-and-true-p evil-ghostel-mode) (evil-ghostel-mode -1))))
      (dolist (fn stubbed) (fmakunbound fn))
      (when-let* ((proc (get-buffer-process buf))) (delete-process proc))
      (kill-buffer buf))))

(provide 'claude-term-live-test)
;;; claude-term-live-test.el ends here
