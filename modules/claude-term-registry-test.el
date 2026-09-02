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
;;   emacs -Q --batch -l ert -l modules/claude-term.el \
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
;; Repo name / elapsed time / label
;; ============================================================================

(ert-deftest claude-term-registry-test-repo-name-fallback-non-git-root ()
  "A root git can't identify (a non-git directory, reachable here without
needing a real pruned worktree) falls back to the bare leaf name rather
than erroring the whole label."
  (let ((root (file-name-as-directory
               (make-temp-file "claude-term-registry-test-nogit-" t)))
        (claude-term-registry--repo-name-cache (make-hash-table :test #'equal)))
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
;; SPC a binding-collision surface (AC7)
;; ============================================================================
;; Drives the REAL `general-define-key' and real evil keymap resolution
;; -- vendored `general.el'/`evil.el' sources added to `load-path', the
;; same technique claude-term-live-test.el uses for real ghostel/evil/
;; evil-ghostel sources -- rather than re-implementing general.el's own
;; :states/:prefix dispatch by hand.
;;
;; ai.el's own plain "SPC a" `general-define-key' call is extracted and
;; `eval'd straight out of its unmodified source with `read', exactly
;; `claude-term-live-test--load-real-escape-defun's technique in
;; claude-term-live-test.el -- rather than loading the whole file via
;; `require', since ai.el's own `(require \\='claude-repl-core)' would
;; otherwise drag in markdown-mode and the whole claude-repl module for
;; no reason a collision check needs: a `general-define-key' call's MAPS
;; are quoted `(COMMAND :which-key STRING)' lists, so the command
;; symbols never need to be `fbound' for `define-key' to install them.

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

(defun claude-term-registry-test--eval-real-ai-el-spc-a-form ()
  "Eval the real, unmodified plain \"SPC a\" `general-define-key' call from
modules/ai.el. See this section's Commentary for why source-extraction,
not a full `require', is used. Returns non-nil on success; nil when
`claude-term-registry-test--ai-el-source' is not present."
  (when (file-exists-p claude-term-registry-test--ai-el-source)
    (with-temp-buffer
      (insert-file-contents claude-term-registry-test--ai-el-source)
      (goto-char (point-min))
      (when (re-search-forward
             "(general-define-key\n   :states 'normal\n   :prefix \"SPC a\"\n" nil t)
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t)
        t))))

(ert-deftest claude-term-registry-test-spc-a-bindings-do-not-collide-with-ai-el ()
  "None of this module's eight SPC a leaf keys collides with modules/ai.el's
existing claude-repl bindings, and `SPC a TAB' stays unbound -- phase 5
owns that binding, together with the attention-ordered comparator it
needs."
  (unless (claude-term-registry-test--load-real-general-and-evil)
    (ert-skip "real evil.el/general.el not found in this checkout; bootstrap straight once locally to enable this test"))
  (unless (claude-term-registry-test--eval-real-ai-el-spc-a-form)
    (ert-skip "modules/ai.el not found next to this test file"))
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
                ("r" . claude-term-rename)))
        (ai-el '(("a" . claude-repl-ask)
                 ("I" . claude-repl-interrupt-and-ask)
                 ("b" . claude-repl-open-buffer)
                 ("c" . claude-repl-clear-buffer)
                 ("s" . claude-repl-process-start-current-project)
                 ("k" . claude-repl-process-kill-current-project)
                 ("K" . claude-repl-process-kill-all)
                 ("l" . claude-repl-show-processes)
                 ("i" . claude-repl-process-status-current-project)
                 ("t" . claude-repl-test-prompt))))
    (dolist (pair ours)
      (should (eq (lookup-key evil-normal-state-map (kbd (concat "SPC a " (car pair))))
                  (cdr pair))))
    ;; The reverse direction: none of ai.el's own commands got silently
    ;; overwritten by this module's bindings either.
    (dolist (pair ai-el)
      (should (eq (lookup-key evil-normal-state-map (kbd (concat "SPC a " (car pair))))
                  (cdr pair))))
    (should-not (lookup-key evil-normal-state-map (kbd "SPC a TAB")))))

;;; claude-term-registry-test.el ends here
