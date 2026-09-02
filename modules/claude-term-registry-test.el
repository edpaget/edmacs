;;; claude-term-registry-test.el --- Tests for claude-term-registry.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage only -- no live ghostel or `claude' process is
;; spawned here.  See claude-term-registry-live-test.el for coverage of
;; the real registry put/touch/remove call sites wired into
;; claude-term.el's kill/restart/exit lifecycle, and of the real
;; `ghostel-pre-spawn-hook' -> `claude-term-registry--set-instance-env'
;; wiring, both of which need a real subprocess to observe.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/claude-term.el \
;;         -l modules/claude-term-registry.el \
;;         -l modules/claude-term-registry-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; ============================================================================
;; Registry key / put / get / remove / touch
;; ============================================================================

(ert-deftest claude-term-registry-test-put-get-truename-dedup ()
  "Two path strings that resolve to the same `file-truename', with the
same instance, collapse to ONE registry entry -- the claude-code-ide.el
sharp edge named in the phase body (it matches on `expand-file-name',
not `file-truename', so a symlinked path to the same worktree registers
as a second, distinct session there)."
  (let* ((claude-term-registry--table (make-hash-table :test #'equal))
         (real-dir (directory-file-name
                    (make-temp-file "claude-term-registry-test-real-" t)))
         (link-path (concat real-dir "-link"))
         (buf (generate-new-buffer "claude-term-registry-test-dedup")))
    (unwind-protect
        (progn
          (make-symbolic-link real-dir link-path)
          (claude-term-registry-put (file-name-as-directory real-dir) nil buf)
          (claude-term-registry-put (file-name-as-directory link-path) nil buf)
          (should (= (hash-table-count claude-term-registry--table) 1))
          (should (= (length (claude-term-registry-sessions)) 1)))
      (kill-buffer buf)
      (ignore-errors (delete-file link-path))
      (ignore-errors (delete-directory real-dir t)))))

(ert-deftest claude-term-registry-test-get-remove-touch ()
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf (generate-new-buffer "claude-term-registry-test-grt"))
        (root "/tmp/claude-term-registry-test-grt-root/"))
    (unwind-protect
        (progn
          (claude-term-registry-put root "x" buf)
          (should (claude-term-registry-get root "x"))
          (setf (claude-term-session-last-used (claude-term-registry-get root "x")) 1.0)
          (claude-term-registry-touch root "x")
          (should (> (claude-term-session-last-used (claude-term-registry-get root "x")) 1.0))
          (claude-term-registry-remove root "x")
          (should-not (claude-term-registry-get root "x")))
      (kill-buffer buf))))

(ert-deftest claude-term-registry-test-put-populates-process-field ()
  "The registry's struct literally holds a PROCESS field (per the phase
body's own \"buffer, process, instance name, and last-used time\" data
shape), snapshotted from the buffer's own `ghostel--process' at put-time.
A buffer with no buffer-local `ghostel--process' at all -- e.g. a bare
test buffer never passed through `claude-term--exec' -- gets nil rather
than a `void-variable' error."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf (generate-new-buffer "claude-term-registry-test-process-field")))
    (unwind-protect
        (progn
          (claude-term-registry-put "/tmp/ctr-proc-none/" nil buf)
          (should-not (claude-term-session-process
                       (claude-term-registry-get "/tmp/ctr-proc-none/" nil)))
          (with-current-buffer buf (setq-local ghostel--process 'fake-process))
          (claude-term-registry-put "/tmp/ctr-proc-some/" nil buf)
          (should (eq (claude-term-session-process
                       (claude-term-registry-get "/tmp/ctr-proc-some/" nil))
                      'fake-process)))
      (kill-buffer buf))))

(ert-deftest claude-term-registry-test-touch-is-noop-for-unregistered-session ()
  (let ((claude-term-registry--table (make-hash-table :test #'equal)))
    (should-not (claude-term-registry-touch "/tmp/no-such-root/" nil))))

(ert-deftest claude-term-registry-test-sessions-reaps-dead-buffers ()
  "A buffer killed directly (bypassing `claude-term--on-exit') is dropped
from `claude-term-registry-sessions', not surfaced as a phantom row."
  (let* ((claude-term-registry--table (make-hash-table :test #'equal))
         (live-buf (generate-new-buffer "claude-term-registry-test-live"))
         (dead-buf (generate-new-buffer "claude-term-registry-test-dead")))
    (unwind-protect
        (progn
          (claude-term-registry-put "/tmp/ctr-reap-live/" nil live-buf)
          (claude-term-registry-put "/tmp/ctr-reap-dead/" nil dead-buf)
          (kill-buffer dead-buf)
          (should (= (hash-table-count claude-term-registry--table) 2))
          (let ((sessions (claude-term-registry-sessions)))
            (should (= (length sessions) 1))
            (should (eq (claude-term-session-buffer (car sessions)) live-buf)))
          (should (= (hash-table-count claude-term-registry--table) 1)))
      (when (buffer-live-p live-buf) (kill-buffer live-buf)))))

(ert-deftest claude-term-registry-test-two-instances-one-root-distinct-keys ()
  "AC4: two named instances under one root register under distinct
(root . instance) keys and both appear, with their instance names, from
`claude-term-registry-sessions'/`claude-term--session-label'."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf-a (generate-new-buffer "claude-term-registry-test-instance-a"))
        (buf-b (generate-new-buffer "claude-term-registry-test-instance-b"))
        (root "/tmp/claude-term-registry-test-root/"))
    (unwind-protect
        (progn
          (claude-term-registry-put root "a" buf-a)
          (claude-term-registry-put root "b" buf-b)
          (should (= (hash-table-count claude-term-registry--table) 2))
          (let* ((sessions (claude-term-registry-sessions))
                 (labels (mapcar #'claude-term--session-label sessions)))
            (should (= (length sessions) 2))
            (should (seq-find (lambda (l) (string-match-p (regexp-quote ":a (") l)) labels))
            (should (seq-find (lambda (l) (string-match-p (regexp-quote ":b (") l)) labels))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

;; ============================================================================
;; State accessor / sort comparator seams
;; ============================================================================

(ert-deftest claude-term-registry-test-stub-state-always-idle ()
  (let ((session (make-claude-term-session :root "/tmp/x/" :instance nil
                                            :buffer nil :last-used 0.0)))
    (should (eq (claude-term-registry--stub-state session) 'idle))
    (should (eq (funcall claude-term-registry-state-accessor session) 'idle))))

(ert-deftest claude-term-registry-test-sort-is-mru-by-default ()
  "AC8: the picker's candidate order is most-recent-first by default via
`claude-term-registry-sort-function', and reassigning that variable
changes the order without touching `claude-term--read-session's code."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (bufs (list (generate-new-buffer "claude-term-registry-test-sort-1")
                    (generate-new-buffer "claude-term-registry-test-sort-2")
                    (generate-new-buffer "claude-term-registry-test-sort-3"))))
    (unwind-protect
        (progn
          (claude-term-registry-put "/tmp/ctr-sort-root-1/" nil (nth 0 bufs))
          (claude-term-registry-put "/tmp/ctr-sort-root-2/" nil (nth 1 bufs))
          (claude-term-registry-put "/tmp/ctr-sort-root-3/" nil (nth 2 bufs))
          ;; Stamp distinct, known last-used times, independent of
          ;; whatever wall-clock order the `put' calls above happened to
          ;; run in.
          (dolist (s (claude-term-registry-sessions))
            (setf (claude-term-session-last-used s)
                  (pcase (claude-term-session-root s)
                    ("/tmp/ctr-sort-root-1/" 100)
                    ("/tmp/ctr-sort-root-2/" 300)
                    ("/tmp/ctr-sort-root-3/" 200))))
          (cl-letf (((symbol-function 'claude-term--session-label)
                     (lambda (s) (claude-term-session-root s))))
            (let (captured)
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt candidates &rest _)
                           (setq captured candidates)
                           (car candidates))))
                (claude-term--read-session "prompt: "))
              (should (equal captured '("/tmp/ctr-sort-root-2/"
                                        "/tmp/ctr-sort-root-3/"
                                        "/tmp/ctr-sort-root-1/"))))
            ;; Swap the comparator to least-recently-used-first --
            ;; `claude-term--read-session' itself is never touched.
            (let ((claude-term-registry-sort-function
                   (lambda (a b) (< (claude-term-session-last-used a)
                                     (claude-term-session-last-used b))))
                  captured)
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt candidates &rest _)
                           (setq captured candidates)
                           (car candidates))))
                (claude-term--read-session "prompt: "))
              (should (equal captured '("/tmp/ctr-sort-root-1/"
                                        "/tmp/ctr-sort-root-3/"
                                        "/tmp/ctr-sort-root-2/"))))))
      (mapc #'kill-buffer bufs))))

(ert-deftest claude-term-registry-test-read-session-returns-selected-struct ()
  "`claude-term--read-session' returns the SESSION struct the user picked,
not merely the label string `completing-read' returned."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf1 (generate-new-buffer "claude-term-registry-test-rs-1"))
        (buf2 (generate-new-buffer "claude-term-registry-test-rs-2")))
    (unwind-protect
        (progn
          (claude-term-registry-put "/tmp/ctr-rs-root-a/" nil buf1)
          (claude-term-registry-put "/tmp/ctr-rs-root-b/" nil buf2)
          (let* ((sessions (claude-term-registry-sessions))
                 (target (car sessions))
                 (label (claude-term--session-label target)))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) label)))
              (should (eq (claude-term--read-session "prompt: ") target)))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest claude-term-registry-test-read-session-empty-user-errors ()
  (let ((claude-term-registry--table (make-hash-table :test #'equal)))
    (should-error (claude-term--read-session "prompt: ") :type 'user-error)))

;; ============================================================================
;; claude-term-kill / claude-term-restart's shared registry-based picker
;; ============================================================================
;; `claude-term--read-buffer' (claude-term.el) is the one place
;; `claude-term-kill' and `claude-term-restart' resolve a target buffer
;; when called with no BUFFER argument from OUTSIDE a claude-term
;; buffer -- it delegates to this file's `claude-term--read-session' for
;; that prompting branch. The tests below drive `claude-term-kill' and
;; `claude-term-restart' themselves (not `claude-term--read-buffer' or
;; `claude-term--read-session' directly) from a non-claude-term current
;; buffer, so the actual bufferless/cross-project prompting path each
;; command takes in real use is exercised end to end, distinguishing the
;; target session the user picked from an unrelated sibling that must be
;; left untouched.

(ert-deftest claude-term-registry-test-kill-bufferless-uses-shared-picker ()
  "`(claude-term-kill)' called with no BUFFER argument from a plain,
non-claude-term current buffer resolves its target via the shared
`claude-term--read-buffer' -> `claude-term--read-session' picker, and
kills only the picked session's process -- an unrelated registered
sibling is left completely untouched."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (target-buf (generate-new-buffer "claude-term-registry-test-kill-target"))
        (other-buf (generate-new-buffer "claude-term-registry-test-kill-other"))
        (caller-buf (generate-new-buffer "claude-term-registry-test-kill-caller"))
        (killed nil))
    (unwind-protect
        (progn
          (with-current-buffer target-buf (setq-local ghostel--process 'target-process))
          (with-current-buffer other-buf (setq-local ghostel--process 'other-process))
          (claude-term-registry-put "/tmp/ctr-kill-picker-target/" nil target-buf)
          (claude-term-registry-put "/tmp/ctr-kill-picker-other/" nil other-buf)
          (with-current-buffer caller-buf
            (should-not (claude-term--parse-buffer-name (buffer-name)))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (claude-term--session-label
                          (claude-term-registry-get "/tmp/ctr-kill-picker-target/" nil))))
                      ((symbol-function 'process-live-p)
                       (lambda (proc) (eq proc 'target-process)))
                      ((symbol-function 'claude-term--terminate)
                       (lambda () (push ghostel--process killed))))
              (claude-term-kill)))
          (should (equal killed '(target-process))))
      (mapc #'kill-buffer (list target-buf other-buf caller-buf)))))

(ert-deftest claude-term-registry-test-restart-bufferless-uses-shared-picker ()
  "`(claude-term-restart)' called with no BUFFER argument from a plain,
non-claude-term current buffer resolves its target via the same shared
picker, and re-execs only the picked (dead-process) session -- an
unrelated registered sibling's buffer-locals are never even read."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (target-buf (generate-new-buffer "claude-term-registry-test-restart-target"))
        (other-buf (generate-new-buffer "claude-term-registry-test-restart-other"))
        (caller-buf (generate-new-buffer "claude-term-registry-test-restart-caller"))
        (reexeced nil))
    (unwind-protect
        (progn
          (with-current-buffer target-buf
            (setq-local claude-term--root "/tmp/ctr-restart-picker-target/")
            (setq-local claude-term--instance nil)
            (setq-local claude-term--args nil)
            (setq-local claude-term--restarting nil)
            (setq-local ghostel--process 'target-process))
          (with-current-buffer other-buf
            (setq-local claude-term--root "/tmp/ctr-restart-picker-other/")
            (setq-local claude-term--instance nil)
            (setq-local claude-term--args nil)
            (setq-local claude-term--restarting nil)
            (setq-local ghostel--process 'other-process))
          (claude-term-registry-put "/tmp/ctr-restart-picker-target/" nil target-buf)
          (claude-term-registry-put "/tmp/ctr-restart-picker-other/" nil other-buf)
          (with-current-buffer caller-buf
            (should-not (claude-term--parse-buffer-name (buffer-name)))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (claude-term--session-label
                          (claude-term-registry-get "/tmp/ctr-restart-picker-target/" nil))))
                      ((symbol-function 'process-live-p) (lambda (_proc) nil))
                      ((symbol-function 'claude-term--exec)
                       (lambda (buf &rest _) (push buf reexeced))))
              (claude-term-restart)))
          (should (equal reexeced (list target-buf))))
      (mapc #'kill-buffer (list target-buf other-buf caller-buf)))))

;; ============================================================================
;; Repo name / elapsed time / label
;; ============================================================================

(ert-deftest claude-term-registry-test-repo-name-fallback-non-git-root ()
  "A root git can't identify (a non-git directory, reachable here without
needing a real pruned worktree) falls back to the bare leaf name rather
than erroring the whole label."
  (let ((root (file-name-as-directory
               (make-temp-file "claude-term-registry-test-nogit-" t)))
        (edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'process-file) (lambda (&rest _) 128)))
          (should (equal (claude-term-registry--repo-name root) (claude-term--leaf root))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest claude-term-registry-test-elapsed-string-just-now ()
  (let ((session (make-claude-term-session :root "/tmp/x/" :instance nil
                                            :buffer nil :last-used (float-time))))
    (should (equal (claude-term-registry--elapsed-string session) "0s"))))

(ert-deftest claude-term-registry-test-label-includes-instance-when-present ()
  (let ((buf (generate-new-buffer "claude-term-registry-test-label")))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-term-registry--repo-name)
                   (lambda (_root) "some-repo")))
          (let ((with-instance (make-claude-term-session
                                 :root "/tmp/proj-leaf/" :instance "b"
                                 :buffer buf :last-used (float-time)))
                (without-instance (make-claude-term-session
                                   :root "/tmp/proj-leaf/" :instance nil
                                   :buffer buf :last-used (float-time))))
            (should (string-match-p "proj-leaf:b (some-repo)"
                                    (claude-term--session-label with-instance)))
            (should (string-match-p "proj-leaf (some-repo)"
                                    (claude-term--session-label without-instance)))
            (should-not (string-match-p "proj-leaf:"
                                        (claude-term--session-label without-instance)))))
      (kill-buffer buf))))

;; ============================================================================
;; Rename
;; ============================================================================

(ert-deftest claude-term-registry-test-rename-migrates-key-preserves-fields ()
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf (generate-new-buffer "claude-term-registry-test-rename"))
        (root "/tmp/ctr-rename-root/"))
    (unwind-protect
        (progn
          (claude-term-registry-put root "old" buf)
          (let ((last-used (claude-term-session-last-used
                             (claude-term-registry-get root "old"))))
            (claude-term-registry-rename root "old" "new")
            (should-not (claude-term-registry-get root "old"))
            (let ((renamed (claude-term-registry-get root "new")))
              (should renamed)
              (should (eq (claude-term-session-buffer renamed) buf))
              (should (equal (claude-term-session-last-used renamed) last-used))
              (should (equal (claude-term-session-instance renamed) "new")))))
      (kill-buffer buf))))

(ert-deftest claude-term-registry-test-rename-rejects-same-root-collision ()
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf-a (generate-new-buffer "claude-term-registry-test-rename-a"))
        (buf-b (generate-new-buffer "claude-term-registry-test-rename-b"))
        (root "/tmp/ctr-rename-collision/"))
    (unwind-protect
        (progn
          (claude-term-registry-put root "a" buf-a)
          (claude-term-registry-put root "b" buf-b)
          (should-error (claude-term-registry-rename root "a" "b") :type 'user-error)
          ;; The rejected attempt leaves the sibling instance "b" -- and
          ;; instance "a" itself -- completely untouched.
          (should (eq (claude-term-session-buffer (claude-term-registry-get root "b")) buf-b))
          (should (claude-term-registry-get root "a")))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest claude-term-registry-test-rename-to-same-instance-is-a-noop-not-a-collision ()
  "Renaming an instance to its OWN current label must not spuriously
`user-error' as if colliding with itself."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf (generate-new-buffer "claude-term-registry-test-rename-self"))
        (root "/tmp/ctr-rename-self/"))
    (unwind-protect
        (progn
          (claude-term-registry-put root "a" buf)
          (claude-term-registry-rename root "a" "a")
          (should (claude-term-registry-get root "a")))
      (kill-buffer buf))))

(ert-deftest claude-term-registry-test-rename-rejects-cross-root-leaf-collision ()
  "Renaming to an instance label already used by an UNRELATED project's
live buffer that merely shares ROOT's leaf directory name is rejected
via `user-error' -- `claude-term-registry-rename's own collision check
is scoped to THIS root, so it cannot see a different root's buffer; left
unhandled, `rename-buffer' would silently uniquify the on-disk name
(e.g. a `<2>' suffix) out of `claude-term--buffer-name-regexp's reach,
with no error at all. The rejection happens before anything is
touched: the buffer being renamed keeps its original registry entry and
buffer-local instance."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf (generate-new-buffer "*claude-term:shared-leaf*"))
        ;; An unrelated project's live claude-term buffer -- same leaf,
        ;; different root -- deliberately never registered in THIS
        ;; test's registry table, mirroring a session this Emacs
        ;; instance happens to have open that this test never put.
        (other-buf (generate-new-buffer "*claude-term:shared-leaf:taken*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local claude-term--root "/tmp/ctr-rename-cross-root-a/shared-leaf/")
            (setq-local claude-term--instance nil))
          (claude-term-registry-put "/tmp/ctr-rename-cross-root-a/shared-leaf/" nil buf)
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "taken")))
            (should-error (claude-term-rename buf) :type 'user-error))
          (should (claude-term-registry-get "/tmp/ctr-rename-cross-root-a/shared-leaf/" nil))
          (should-not (buffer-local-value 'claude-term--instance buf))
          (should (equal (buffer-name buf) "*claude-term:shared-leaf*")))
      (kill-buffer buf)
      (kill-buffer other-buf))))

(ert-deftest claude-term-registry-test-rename-same-root-collision-via-command-gets-correct-message ()
  "Driving the same-root collision through `claude-term-rename' itself
(not just `claude-term-registry-rename' directly) must surface the
registry-level, root-scoped \"instance already exists for this root\"
message -- NOT the buffer-name check's cross-root \"an unrelated
project's session sharing this leaf name\" wording, which is factually
wrong when the collision is against a SIBLING instance of the very
same worktree. Before the fix, `claude-term-rename's own buffer-name
collision guard ran unconditionally and intercepted this case first,
making the correctly-worded registry error unreachable through the
only public entry point a user actually invokes (SPC a r)."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf-a (generate-new-buffer "*claude-term:shared-worktree:a*"))
        (buf-b (generate-new-buffer "*claude-term:shared-worktree:b*"))
        (root "/tmp/ctr-rename-cmd-same-root/shared-worktree/"))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (setq-local claude-term--root root)
            (setq-local claude-term--instance "a"))
          (with-current-buffer buf-b
            (setq-local claude-term--root root)
            (setq-local claude-term--instance "b"))
          (claude-term-registry-put root "a" buf-a)
          (claude-term-registry-put root "b" buf-b)
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "b")))
            (let ((err (should-error (claude-term-rename buf-a) :type 'user-error)))
              (should (string-match-p "instance b already exists for this root" (cadr err)))
              (should-not (string-match-p "unrelated project" (cadr err)))))
          ;; Sibling instance "b" -- and "a" itself -- are left untouched.
          (should (eq (claude-term-session-buffer (claude-term-registry-get root "b")) buf-b))
          (should (equal (buffer-local-value 'claude-term--instance buf-a) "a")))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest claude-term-registry-test-rename-into-stale-dead-instance-slot-succeeds ()
  "A ROOT/NEW-INSTANCE registry entry left behind by a buffer that was
killed directly (bypassing `claude-term--on-exit' /
`claude-term-registry-remove') must not block a legitimate rename into
that now-vacated instance slot -- `claude-term-registry-sessions' already
documents this direct-kill-buffer scenario as expected and self-heals
against it; the rename path must reap the same way, not falsely report
the dead instance as still occupied."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf-a (generate-new-buffer "claude-term-registry-test-rename-stale-a"))
        (dead-buf (generate-new-buffer "claude-term-registry-test-rename-stale-dead"))
        (root "/tmp/ctr-rename-stale-slot/"))
    (unwind-protect
        (progn
          (claude-term-registry-put root "a" buf-a)
          (claude-term-registry-put root "b" dead-buf)
          (kill-buffer dead-buf)
          (setq dead-buf nil)
          (claude-term-registry-rename root "a" "b")
          (should-not (claude-term-registry-get root "a"))
          (let ((renamed (claude-term-registry-get root "b")))
            (should renamed)
            (should (eq (claude-term-session-buffer renamed) buf-a))))
      (kill-buffer buf-a))))

;; ============================================================================
;; EDMACS_AGENT_INSTANCE pre-spawn env hook
;; ============================================================================

(ert-deftest claude-term-registry-test-set-instance-env-guards-non-claude-term-buffer ()
  "The global `ghostel-pre-spawn-hook' must not leak a value into a plain,
non-claude-term ghostel session someone opens directly."
  (let ((process-environment (copy-sequence process-environment)))
    (with-temp-buffer
      (rename-buffer "*ghostel:some-other-terminal*")
      (claude-term-registry--set-instance-env)
      (should-not (getenv "EDMACS_AGENT_INSTANCE")))))

(ert-deftest claude-term-registry-test-set-instance-env-sets-default-for-unnamed-instance ()
  "The default (unnamed) instance still exports a defined
`EDMACS_AGENT_INSTANCE' value, never leaving the variable unset."
  (let ((process-environment (copy-sequence process-environment)))
    (with-temp-buffer
      (rename-buffer "*claude-term:leaf*")
      (claude-term-registry--set-instance-env)
      (should (equal (getenv "EDMACS_AGENT_INSTANCE") "default")))))

(ert-deftest claude-term-registry-test-set-instance-env-sets-named-instance ()
  (let ((process-environment (copy-sequence process-environment)))
    (with-temp-buffer
      (rename-buffer "*claude-term:leaf:b*")
      (claude-term-registry--set-instance-env)
      (should (equal (getenv "EDMACS_AGENT_INSTANCE") "b")))))

;; ============================================================================
;; SPC a command bodies (F1: exercise the command functions themselves,
;; not merely their SPC a keybindings)
;; ============================================================================

(ert-deftest claude-term-registry-test-jump-selects-and-touches-session ()
  "`claude-term-jump' -- the actual command SPC a j invokes -- displays
the chosen session's buffer in a (real) side window, selecting it, and
touches its last-used time. Wires the separately-tested
`claude-term--read-session' picker primitive onto
`claude-term--pop-to-side-window'/`claude-term-registry-touch', neither
of which was previously exercised by any test calling `claude-term-jump'
itself."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (claude-term--next-slot 0)
        (window-sides-slots '(nil nil 3 nil))
        (buf (generate-new-buffer "*claude-term:jump-leaf*"))
        (root "/tmp/ctr-jump-root/"))
    (unwind-protect
        (progn
          (claude-term-registry-put root nil buf)
          (setf (claude-term-session-last-used (claude-term-registry-get root nil)) 1.0)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (claude-term--session-label (claude-term-registry-get root nil)))))
            (claude-term-jump))
          (should (eq (window-buffer (selected-window)) buf))
          (should (> (claude-term-session-last-used (claude-term-registry-get root nil)) 1.0)))
      (kill-buffer buf))))

(ert-deftest claude-term-registry-test-new-session-delegates-to-claude-term ()
  "`claude-term-new-session' -- the actual command SPC a n invokes --
delegates directly to the existing `claude-term' entry point with the
given instance."
  (let (captured)
    (cl-letf (((symbol-function 'claude-term)
               (lambda (&optional instance &rest _) (setq captured instance))))
      (claude-term-new-session "b")
      (should (equal captured "b")))))

(ert-deftest claude-term-registry-test-list-sessions-populates-tabulated-list ()
  "`claude-term-list-sessions' -- the actual command SPC a L invokes --
builds a `claude-term-session-list-mode' buffer whose
`tabulated-list-entries' reflect every registered session."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf (generate-new-buffer "claude-term-registry-test-list-session")))
    (unwind-protect
        (progn
          (claude-term-registry-put "/tmp/ctr-list-root/" "x" buf)
          (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
            (claude-term-list-sessions))
          (with-current-buffer (get-buffer "*claude-term-sessions*")
            (should (derived-mode-p 'claude-term-session-list-mode))
            (should (= (length tabulated-list-entries) 1))
            (should (eq (car (car tabulated-list-entries))
                        (claude-term-registry-get "/tmp/ctr-list-root/" "x")))))
      (kill-buffer buf)
      (when (get-buffer "*claude-term-sessions*")
        (kill-buffer "*claude-term-sessions*")))))

(ert-deftest claude-term-registry-test-toggle-pane-calls-window-toggle-side-windows ()
  "`claude-term-toggle-pane' -- the actual command SPC a w invokes -- is a
thin wrapper around the stock `window-toggle-side-windows'."
  (let (called)
    (cl-letf (((symbol-function 'window-toggle-side-windows)
               (lambda () (setq called t))))
      (claude-term-toggle-pane)
      (should called))))

(ert-deftest claude-term-registry-test-show-all-displays-every-live-session ()
  "`claude-term-show-all' -- the actual command SPC a A invokes -- calls
`claude-term--display-buffer' on every registered, live session's
buffer."
  (let ((claude-term-registry--table (make-hash-table :test #'equal))
        (buf1 (generate-new-buffer "claude-term-registry-test-show-all-1"))
        (buf2 (generate-new-buffer "claude-term-registry-test-show-all-2"))
        displayed)
    (unwind-protect
        (progn
          (claude-term-registry-put "/tmp/ctr-show-all-1/" nil buf1)
          (claude-term-registry-put "/tmp/ctr-show-all-2/" nil buf2)
          (cl-letf (((symbol-function 'claude-term--display-buffer)
                     (lambda (b) (push b displayed))))
            (claude-term-show-all))
          (should (= (length displayed) 2))
          (should (memq buf1 displayed))
          (should (memq buf2 displayed)))
      (kill-buffer buf1)
      (kill-buffer buf2))))

;; ============================================================================
;; SPC a binding surface after the claude-repl retirement
;; ============================================================================
;; Drives the REAL `general-define-key' and real evil keymap resolution
;; -- vendored `general.el'/`evil.el' sources added to `load-path', the
;; same technique claude-term-live-test.el uses for real ghostel/evil/
;; evil-ghostel sources -- rather than re-implementing general.el's own
;; :states/:prefix dispatch by hand.
;;
;; This started life as a collision check against modules/ai.el's ten
;; claude-repl bindings, which had to coexist with this module's eight.
;; claude-repl is now retired and ai.el's whole keybinding block is gone,
;; so the interesting property inverted: those ten letters (and the "p"
;; approval-policy submenu) must now resolve to NOTHING. A binding form
;; left behind would resolve to a removed command and fail at press time,
;; which no grep over the source would catch.
;;
;; ai.el is checked at the source level rather than loaded: `require'ing
;; it would drag in markdown-mode and olivetti for a check that only
;; needs to know no binding form naming a deleted command survives.

(defconst claude-term-registry-test--evil-source
  (expand-file-name "../straight/repos/evil/evil.el"
                     (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the real evil.el, when this checkout has fetched it.")

(defconst claude-term-registry-test--general-source
  (expand-file-name "../straight/repos/general.el/general.el"
                     (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the real general.el, when this checkout has fetched it.")

(defconst claude-term-registry-test--ai-el-source
  (expand-file-name "ai.el" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the real modules/ai.el, sibling to this test file.")

(defconst claude-term-registry-test--retired-spc-a-keys
  '("a" "I" "b" "c" "s" "k" "K" "l" "i" "t" "p")
  "Every `SPC a' leaf key modules/ai.el bound to a claude-repl command.
\"p\" was the approval-policy submenu prefix rather than a leaf.  All of
them must be unbound now that the module is deleted.")

(defun claude-term-registry-test--load-real-general-and-evil ()
  "Add load-path entries for, and `require', the real evil and general.
Returns non-nil on success; nil (without erroring) when either source
is missing from this checkout -- a machine that has never bootstrapped
straight locally -- so the caller can `ert-skip' instead of failing."
  (when (and (file-exists-p claude-term-registry-test--evil-source)
             (file-exists-p claude-term-registry-test--general-source))
    (add-to-list 'load-path (file-name-directory claude-term-registry-test--evil-source))
    (add-to-list 'load-path (file-name-directory claude-term-registry-test--general-source))
    (require 'evil)
    (require 'general)
    t))

(ert-deftest claude-term-registry-test-spc-a-bindings-resolve ()
  "This module's eight SPC a leaf keys resolve to its own commands.
`SPC a TAB' stays unbound: the attention-ordered comparator that binding
would need is not part of this module's surface."
  (unless (claude-term-registry-test--load-real-general-and-evil)
    (ert-skip "real evil.el/general.el not found in this checkout; bootstrap straight once locally to enable this test"))
  ;; This module's own `with-eval-after-load 'general' bindings
  ;; (claude-term-registry.el, loaded via -l before this test file) were
  ;; deferred until 'general' actually loaded -- `require' above just
  ;; fired them for real.
  (let ((ours '(("n" . claude-term-new-session)
                ("j" . claude-term-jump)
                ("L" . claude-term-list-sessions)
                ("w" . claude-term-toggle-pane)
                ("A" . claude-term-show-all)
                ("x" . claude-term-kill)
                ("X" . claude-term-kill-all)
                ("r" . claude-term-rename))))
    (dolist (pair ours)
      (should (eq (lookup-key evil-normal-state-map (kbd (concat "SPC a " (car pair))))
                  (cdr pair)))))
  (should-not (lookup-key evil-normal-state-map (kbd "SPC a TAB"))))

(ert-deftest claude-term-registry-test-retired-claude-repl-keys-are-unbound ()
  "None of modules/ai.el's former claude-repl SPC a keys still resolves.
Each would now point at a command deleted with the module, so pressing
it would fail at the keymap level -- something no grep over the sources
would catch."
  (unless (claude-term-registry-test--load-real-general-and-evil)
    (ert-skip "real evil.el/general.el not found in this checkout; bootstrap straight once locally to enable this test"))
  (dolist (key claude-term-registry-test--retired-spc-a-keys)
    (should-not (lookup-key evil-normal-state-map (kbd (concat "SPC a " key))))))

(ert-deftest claude-term-registry-test-spc-a-keeps-a-which-key-heading ()
  "This module supplies the `SPC a' which-key heading ai.el used to own.
ai.el's `\"\" (:ignore t :which-key \"ai/claude\")' entry was the only
thing labelling the prefix; deleting it without adopting one here would
silently leave `SPC a' headless, a regression invisible to `lookup-key'."
  (unless (claude-term-registry-test--load-real-general-and-evil)
    (ert-skip "real evil.el/general.el not found in this checkout; bootstrap straight once locally to enable this test"))
  (require 'which-key)
  ;; general.el records a `:which-key' prefix label as a
  ;; `which-key-replacement-alist' entry keyed on the anchored key
  ;; sequence -- see `general--add-which-key-replacement'.
  (should (seq-find (lambda (entry)
                      (and (consp entry)
                           (consp (car entry))
                           (equal (caar entry) "\\`SPC a\\'")))
                    which-key-replacement-alist)))

(ert-deftest claude-term-registry-test-ai-el-names-no-deleted-command ()
  "modules/ai.el's source mentions claude-repl nowhere.
`init.el' loads ai.el unconditionally, so a surviving
`(require \\='claude-repl-core)' would break startup outright and a
surviving binding form would install a void command."
  (unless (file-exists-p claude-term-registry-test--ai-el-source)
    (ert-skip "modules/ai.el not found next to this test file"))
  (with-temp-buffer
    (insert-file-contents claude-term-registry-test--ai-el-source)
    (goto-char (point-min))
    (should-not (re-search-forward "claude-repl" nil t))))

;;; claude-term-registry-test.el ends here
