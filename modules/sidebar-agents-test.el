;;; sidebar-agents-test.el --- Tests for sidebar-agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; Like sidebar-test.el, sidebar-agents.el's own `(require 'magit-section)'
;; needs `load-path' fixed against the straight build tree before it can
;; even be parsed under `-Q --batch' -- so this file carries the same
;; kind of self-contained invocation:
;;
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el -l modules/agents.el \
;;         -l modules/sidebar-agents-test.el -f ert-run-tests-batch-and-exit
;;
;; agents.el is loaded for real (plain elisp, no external deps) so tests
;; can construct real `edmacs-agent' structs and mutate the real table;
;; frames.el and sidebar.el are NOT loaded (mirroring sidebar-test.el's
;; own module-boundary convention) -- every function sidebar-agents.el
;; calls into either is stubbed via `cl-letf'.
;;
;; Every test isolates the mutable module state it touches
;; (`edmacs-agents--table', `edmacs-sidebar-agents--last-state',
;; `edmacs-sidebar-agents--attention-cache'/`-cursor', notification/timer
;; state) via `edmacs-sidebar-agents-test--with-clean-state', the same
;; convention `edmacs-agents-test--with-clean-state' uses.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; Disabled for the same reason sidebar-test.el disables it: `advice-add'
;; on a primitive (this file's own `--osascript-notify'/tmux stub tests,
;; and `edmacs-sidebar-hide' advice at sidebar-agents.el load time) can
;; otherwise spawn a real native-comp trampoline-compiler subprocess.
(setq native-comp-enable-subr-trampolines nil)

(defun edmacs-sidebar-agents-test--locate-straight-build-root ()
  "Return this checkout's (or its sibling main checkout's) `straight/build'.
Identical logic to `edmacs-sidebar-test--locate-straight-build-root'."
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

(defun edmacs-sidebar-agents-test--add-magit-section-deps (build-root)
  "Add `magit-section' and its transitive deps under BUILD-ROOT to `load-path'."
  (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section"))
    (let ((dir (expand-file-name dep build-root)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(defvar edmacs-sidebar-agents-test--build-root
  (edmacs-sidebar-agents-test--locate-straight-build-root))

(if (null edmacs-sidebar-agents-test--build-root)

    (ert-deftest edmacs-sidebar-agents-test-magit-section-unavailable ()
      (ert-skip "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (edmacs-sidebar-agents-test--add-magit-section-deps edmacs-sidebar-agents-test--build-root)

    ;; sidebar-agents.el's own forward `declare-function's for frames.el
    ;; and sidebar.el are byte-compile hygiene only; real stand-ins are
    ;; provided here so its top-level `(setq edmacs-sidebar-worktree-...)'
    ;; and `add-hook'/`advice-add' calls have something real to touch,
    ;; the same way sidebar-test.el pre-populates `tab-bar-tabs' rather
    ;; than loading frames.el for real.
    (defvar edmacs-sidebar-worktree-label-suffix-function #'ignore)
    (defvar edmacs-sidebar-worktree-section-functions nil)
    (defvar edmacs-sidebar-extra-section-functions nil)
    (defvar edmacs-sidebar-header-line-function #'ignore)
    (defvar edmacs-sidebar-collapsed-section-functions nil)
    (defvar edmacs-sidebar-force-text-glyphs nil)
    (defun edmacs-sidebar--redraw (_frame) nil)
    (defun edmacs-sidebar--window (_frame) nil)
    (defun edmacs-sidebar-hide (&optional _frame) nil)
    (defun edmacs-sidebar--fit (label width)
      "Stub: truncate LABEL with a trailing … to fit WIDTH columns.
Mirrors sidebar.el's implementation for tests."
      (let ((width (max 0 width)))
        (cond
         ((<= width 0) "")
         ((<= (string-width label) width) label)
         (t (concat (truncate-string-to-width label (max 0 (1- width))) "…")))))
    (defun edmacs-frames-open-worktree-tab (_dir) nil)
    (defun claude-term-registry-rename (_root _old _new) nil)

    (load (expand-file-name "modules/agents.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar-agents.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Test helpers
    ;; ==========================================================================

    (defmacro edmacs-sidebar-agents-test--with-clean-state (&rest body)
      "Run BODY with fresh agent/module state, restored after."
      (declare (indent 0))
      `(let ((edmacs-agents--table (make-hash-table :test #'equal))
             (edmacs-agents-changed-hook nil)
             (edmacs-sidebar-agents--last-state (make-hash-table :test #'equal))
             (edmacs-sidebar-agents--attention-cache nil)
             (edmacs-sidebar-agents--attention-cursor 0)
             (edmacs-sidebar-agents--pending-notification nil)
             (edmacs-sidebar-agents--coalesce-timer nil)
             (edmacs-sidebar-agents--elapsed-timer nil)
             (edmacs-sidebar-agents-show-all nil))
         (unwind-protect
             (progn ,@body)
           (when (timerp edmacs-sidebar-agents--coalesce-timer)
             (cancel-timer edmacs-sidebar-agents--coalesce-timer))
           (when (timerp edmacs-sidebar-agents--elapsed-timer)
             (cancel-timer edmacs-sidebar-agents--elapsed-timer)))))

    (cl-defun edmacs-sidebar-agents-test--make-agent
        (&key (root "/repo/wt/") (instance "%1") (status 'working)
              (status-ts (float-time)) (title "Claude Code") (source 'claude-term)
              (locator nil) (unread nil))
      (let ((key (cons root instance)))
        (make-edmacs-agent :key key :root root :instance instance :status status
                            :status-ts status-ts :updated-ts status-ts :title title
                            :source source :locator locator :unread unread)))

    (defun edmacs-sidebar-agents-test--put (agent)
      (puthash (edmacs-agent-key agent) agent edmacs-agents--table)
      agent)

    ;; ==========================================================================
    ;; Glyphs
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-glyph-fallback-ascii ()
      "With no nerd-icons, every status falls back to plain ASCII."
      (should (equal "*" (edmacs-sidebar-agents--glyph 'working)))
      (should (equal "?" (edmacs-sidebar-agents--glyph 'waiting)))
      (should (equal "v" (edmacs-sidebar-agents--glyph 'done)))
      (should (equal "o" (edmacs-sidebar-agents--glyph 'idle)))
      (should (equal "?" (edmacs-sidebar-agents--glyph 'some-unknown-status))))

    (ert-deftest edmacs-sidebar-agents-test-glyph-prefers-nerd-icons-when-available ()
      "When `nerd-icons' is (simulated) present, its icon wins over ASCII."
      (cl-letf (((symbol-function 'nerd-icons-faicon) (lambda (_name) "NERD-WORKING"))
                ((symbol-function 'nerd-icons-octicon)
                 (lambda (name) (format "NERD-%s" name))))
        (let ((features (cons 'nerd-icons features)))
          (cl-letf (((symbol-function 'featurep)
                     (lambda (f) (or (memq f features) nil))))
            (should (equal "NERD-WORKING" (edmacs-sidebar-agents--glyph 'working)))
            (should (equal "NERD-nf-oct-comment" (edmacs-sidebar-agents--glyph 'waiting)))))))

    (ert-deftest edmacs-sidebar-agents-test-glyph-nerd-icon-error-falls-back ()
      "A nerd-icons call that errors falls back to ASCII, never signals."
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'fboundp) (lambda (f) (eq f 'nerd-icons-faicon)))
                ((symbol-function 'nerd-icons-faicon) (lambda (_name) (error "boom"))))
        (should (equal "*" (edmacs-sidebar-agents--glyph 'working)))))

    ;; ==========================================================================
    ;; Elapsed-time string
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-elapsed-string-units ()
      (let ((now (float-time)))
        (should (equal "0s" (edmacs-sidebar-agents--elapsed-string now)))
        (should (equal "5s" (edmacs-sidebar-agents--elapsed-string (- now 5))))
        (should (equal "2m" (edmacs-sidebar-agents--elapsed-string (- now 130))))
        (should (equal "3h" (edmacs-sidebar-agents--elapsed-string (- now 10900))))
        (should (equal "2d" (edmacs-sidebar-agents--elapsed-string (- now 200000))))))

    (ert-deftest edmacs-sidebar-agents-test-elapsed-string-non-numeric-is-blank ()
      (should (equal "" (edmacs-sidebar-agents--elapsed-string nil)))
      (should (equal "" (edmacs-sidebar-agents--elapsed-string "not-a-number"))))

    ;; ==========================================================================
    ;; Worktree truename matching
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-for-root-matches-by-truename ()
      "`--for-root' (the live match path `--on-worktree-section'/
`--label-suffix' actually call) finds an agent by truename-normalized
root, not raw string equality -- so a tabless worktree's agent is found
even when its own root string differs from the worktree list's."
      (edmacs-sidebar-agents-test--with-clean-state
        (cl-letf (((symbol-function 'file-truename)
                   (lambda (p) (if (equal p "/repo/wt-b") "/real/wt-b" p))))
          (let ((agent (edmacs-sidebar-agents-test--put
                        (edmacs-sidebar-agents-test--make-agent :root "/repo/wt-b"))))
            (should (equal (list agent) (edmacs-sidebar-agents--for-root "/real/wt-b")))
            (should-not (edmacs-sidebar-agents--for-root "/repo/wt-b"))))))

    (ert-deftest edmacs-sidebar-agents-test-for-root-no-match-is-empty ()
      (edmacs-sidebar-agents-test--with-clean-state
        (cl-letf (((symbol-function 'file-truename) #'identity))
          (edmacs-sidebar-agents-test--put
           (edmacs-sidebar-agents-test--make-agent :root "/gone/deleted-wt"))
          (should-not (edmacs-sidebar-agents--for-root "/repo")))))

    ;; ==========================================================================
    ;; Attention comparator / attention-list
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-compare-orders-by-state-rank ()
      (let ((waiting (edmacs-sidebar-agents-test--make-agent :status 'waiting))
            (unread-done (edmacs-sidebar-agents-test--make-agent :status 'done :unread t))
            (working (edmacs-sidebar-agents-test--make-agent :status 'working))
            (idle (edmacs-sidebar-agents-test--make-agent :status 'idle)))
        (should (edmacs-sidebar-agents--compare waiting unread-done))
        (should-not (edmacs-sidebar-agents--compare unread-done waiting))
        (should (edmacs-sidebar-agents--compare unread-done working))
        (should (edmacs-sidebar-agents--compare working idle))
        ;; A read (non-unread) `done' row sorts with the "else" bucket, same
        ;; as idle -- pinned to identical status-ts/title so this checks
        ;; the rank bucket only, not the tie-break the differing default
        ;; `(float-time)' timestamps would otherwise resolve it by.
        (let ((read-done (edmacs-sidebar-agents-test--make-agent
                           :status 'done :unread nil :status-ts 42 :title "x"))
              (idle-2 (edmacs-sidebar-agents-test--make-agent
                       :status 'idle :status-ts 42 :title "x")))
          (should-not (edmacs-sidebar-agents--compare read-done idle-2))
          (should-not (edmacs-sidebar-agents--compare idle-2 read-done)))))

    (ert-deftest edmacs-sidebar-agents-test-compare-tie-breaks-on-ts-then-title ()
      (let ((a (edmacs-sidebar-agents-test--make-agent :status 'waiting :status-ts 100 :title "b"))
            (b (edmacs-sidebar-agents-test--make-agent :status 'waiting :status-ts 200 :title "a")))
        ;; Earlier status-ts sorts first even though its title sorts later.
        (should (edmacs-sidebar-agents--compare a b))
        (should-not (edmacs-sidebar-agents--compare b a)))
      (let ((a (edmacs-sidebar-agents-test--make-agent :status 'waiting :status-ts 100 :title "a"))
            (b (edmacs-sidebar-agents-test--make-agent :status 'waiting :status-ts 100 :title "b")))
        (should (edmacs-sidebar-agents--compare a b))
        (should-not (edmacs-sidebar-agents--compare b a))))

    (ert-deftest edmacs-sidebar-agents-test-attention-list-filters-and-sorts ()
      (edmacs-sidebar-agents-test--with-clean-state
        (let ((waiting (edmacs-sidebar-agents-test--make-agent
                         :root "/r1/" :instance "%1" :status 'waiting :status-ts 200))
              (unread-done (edmacs-sidebar-agents-test--make-agent
                            :root "/r2/" :instance "%2" :status 'done :unread t :status-ts 50))
              (working (edmacs-sidebar-agents-test--make-agent
                        :root "/r3/" :instance "%3" :status 'working))
              (read-done (edmacs-sidebar-agents-test--make-agent
                          :root "/r4/" :instance "%4" :status 'done :unread nil)))
          (mapc #'edmacs-sidebar-agents-test--put (list working read-done waiting unread-done))
          (should (equal (list waiting unread-done) (edmacs-sidebar-agents--attention-list))))))

    ;; ==========================================================================
    ;; Per-worktree rendering (AC1)
    ;; ==========================================================================

    (defmacro edmacs-sidebar-agents-test--with-sidebar-buffer (&rest body)
      "Run BODY in a fresh `magit-section-mode' temp buffer.
`magit-section-mode' directly, not `edmacs-sidebar-mode' -- sidebar.el
is deliberately not loaded by this suite (see this file's own
Commentary on the module-boundary convention), and every
`magit-insert-section' call needs is buffer-local state
`magit-section-mode' itself sets up."
      (declare (indent 0))
      `(with-temp-buffer
         (magit-section-mode)
         (let ((inhibit-read-only t))
           ,@body)))

    (ert-deftest edmacs-sidebar-agents-test-insert-for-worktree-renders-glyph-title-elapsed ()
      (edmacs-sidebar-agents-test--with-clean-state
        (let ((agent (edmacs-sidebar-agents-test--make-agent
                      :root "/repo/wt/" :status 'working :title "Claude Code"
                      :status-ts (float-time))))
          (edmacs-sidebar-agents-test--with-sidebar-buffer
            (edmacs-sidebar-agents--insert-group "/repo/wt/" (list agent))
            (let ((text (buffer-string)))
              (should (string-match-p (regexp-quote "*") text))
              (should (string-match-p "Claude Code" text))
              (should (string-match-p "0s" text)))))))

    (ert-deftest edmacs-sidebar-agents-test-insert-row-unread-done-is-bold ()
      "`bold' is layered ON TOP of the `done' status face (phase 8), not a
bare `bold' symbol replacing it -- see the dedicated status-face tests
below for the exact layered shape."
      (let ((agent (edmacs-sidebar-agents-test--make-agent :status 'done :unread t)))
        (edmacs-sidebar-agents-test--with-sidebar-buffer
          (edmacs-sidebar-agents--insert-row agent)
          (let ((face (get-text-property (point-min) 'face)))
            (should (or (eq face 'bold) (and (listp face) (memq 'bold face))))))))

    (ert-deftest edmacs-sidebar-agents-test-insert-row-working-is-not-bold ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent :status 'working)))
        (edmacs-sidebar-agents-test--with-sidebar-buffer
          (edmacs-sidebar-agents--insert-row agent)
          (should-not (text-property-any (point-min) (point-max) 'face 'bold)))))

    (ert-deftest edmacs-sidebar-agents-test-on-worktree-section-noop-when-no-agents ()
      "A worktree with zero matched agents inserts nothing -- an
agent-less worktree's row is byte-for-byte unchanged from before this
phase, which is exactly what keeps sidebar-test.el's own worktree-count
assertions passing untouched."
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--with-sidebar-buffer
          (edmacs-sidebar-agents--on-worktree-section "/repo/no-agents/" t)
          (should (= (point-min) (point-max))))))

    (ert-deftest edmacs-sidebar-agents-test-label-suffix-counts-agents-for-root ()
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/repo/wt/" :instance "%1"))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/repo/wt/" :instance "%2"))
        (should (equal " (2)" (edmacs-sidebar-agents--label-suffix "/repo/wt/")))
        (should-not (edmacs-sidebar-agents--label-suffix "/repo/empty/"))))

    ;; ==========================================================================
    ;; ALL AGENTS section (AC5, pure half -- multi-frame half is in the
    ;; live suite)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-all-section-hidden-by-default ()
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put (edmacs-sidebar-agents-test--make-agent))
        (edmacs-sidebar-agents-test--with-sidebar-buffer
          (edmacs-sidebar-agents--insert-all-section (selected-frame))
          (should (= (point-min) (point-max))))))

    (ert-deftest edmacs-sidebar-agents-test-all-section-lists-in-attention-order ()
      (edmacs-sidebar-agents-test--with-clean-state
        (setq edmacs-sidebar-agents-show-all t)
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1"
                                                  :status 'idle :title "idle-one"))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r2/" :instance "%2"
                                                  :status 'waiting :title "waiting-one"))
        (edmacs-sidebar-agents-test--with-sidebar-buffer
          (edmacs-sidebar-agents--insert-all-section (selected-frame))
          (should (string-match-p "ALL AGENTS" (buffer-string)))
          (should (< (progn (goto-char (point-min))
                             (search-forward "waiting-one"))
                     (progn (goto-char (point-min))
                            (search-forward "idle-one")))))))

    (ert-deftest edmacs-sidebar-agents-test-toggle-all-flips-and-redraws ()
      (edmacs-sidebar-agents-test--with-clean-state
        (let ((redraw-calls 0))
          (cl-letf (((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (_frame) (setq redraw-calls (1+ redraw-calls)))))
            (should-not edmacs-sidebar-agents-show-all)
            (edmacs-sidebar-agents-toggle-all)
            (should edmacs-sidebar-agents-show-all)
            (should (> redraw-calls 0))
            (setq redraw-calls 0)
            (edmacs-sidebar-agents-toggle-all)
            (should-not edmacs-sidebar-agents-show-all)
            (should (> redraw-calls 0))))))

    ;; ==========================================================================
    ;; Visiting (AC2) -- pure dispatch/side-effect-selection coverage;
    ;; the live suite covers the real frame-raise/tab-select/redraw and
    ;; the real async tmux calls.
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-visit-common-opens-tab-marks-read-redraws ()
      (edmacs-sidebar-agents-test--with-clean-state
        (let* ((agent (edmacs-sidebar-agents-test--make-agent
                       :root "/repo/wt/" :status 'done :unread t))
               (opened nil) (redraws 0))
          (edmacs-sidebar-agents-test--put agent)
          (cl-letf (((symbol-function 'edmacs-frames-open-worktree-tab)
                     (lambda (dir) (push dir opened)))
                    ((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (_frame) (setq redraws (1+ redraws)))))
            (edmacs-sidebar-agents--visit-common agent)
            (should (equal '("/repo/wt/") opened))
            (should (eq 'idle (edmacs-agent-status agent)))
            (should-not (edmacs-agent-unread agent))
            (should (> redraws 0))))))

    (ert-deftest edmacs-sidebar-agents-test-visit-source-extra-claude-term-pops-to-its-window ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent
                    :source 'claude-term :locator "fake-buffer"))
            (selected nil))
        (cl-letf (((symbol-function 'claude-term--pop-to-window)
                   (lambda (buf) (setq selected buf))))
          (edmacs-sidebar-agents--visit-source-extra agent)
          (should (equal "fake-buffer" selected)))))

    (ert-deftest edmacs-sidebar-agents-test-visit-source-extra-nil-source-is-silent-noop ()
      "A :source nil row has no jump target: the pcase has no catch-all,
so this must return without calling anything and without signalling."
      (let ((agent (edmacs-sidebar-agents-test--make-agent :source nil))
            (pop-called nil) (proc-called nil))
        (cl-letf (((symbol-function 'claude-term--pop-to-window)
                   (lambda (&rest _) (setq pop-called t)))
                  ((symbol-function 'start-process)
                   (lambda (&rest _) (setq proc-called t))))
          (edmacs-sidebar-agents--visit-source-extra agent)
          (should-not pop-called)
          (should-not proc-called))))

    (ert-deftest edmacs-sidebar-agents-test-visit-nil-source-opens-tab-marks-read-without-signalling ()
      "The full RET visit on a :source nil row still runs the common half
-- open the worktree tab, mark it read -- and never signals, even though
the source-specific jump half is a no-op."
      (let* ((agent (edmacs-sidebar-agents-test--make-agent :source nil :root "/repo/wt/"))
             (opened nil) (marked-key nil))
        (cl-letf (((symbol-function 'edmacs-frames-open-worktree-tab)
                   (lambda (dir) (push dir opened)))
                  ((symbol-function 'edmacs-agents-mark-read)
                   (lambda (key) (setq marked-key key)))
                  ((symbol-function 'edmacs-sidebar-agents--redraw-all)
                   (lambda () nil)))
          (edmacs-sidebar-agents--visit-common agent)
          (edmacs-sidebar-agents--visit-source-extra agent)
          (should (equal '("/repo/wt/") opened))
          (should (equal (edmacs-agent-key agent) marked-key)))))

    (ert-deftest edmacs-sidebar-agents-test-visit-no-section-reports ()
      "`edmacs-sidebar-agents-visit' signals `user-error' rather than doing
nothing when there is no section at point at all -- the direct-call
counterpart of edmacs-sidebar.el's own no-silent-no-op fix, since RET
never reaches this function without one (`edmacs-sidebar-visit-at-point'
only dispatches here for an `edmacs-sidebar-agent' section type)."
      (edmacs-sidebar-agents-test--with-sidebar-buffer
        (should-error (edmacs-sidebar-agents-visit) :type 'user-error)))

    (ert-deftest edmacs-sidebar-agents-test-visit-nil-value-reports ()
      "A section whose VALUE is nil -- `edmacs-sidebar-agents--insert-row'
itself never constructs one this way, but a degenerate/direct-call
construction could -- also reports rather than silently doing nothing."
      (edmacs-sidebar-agents-test--with-sidebar-buffer
        (magit-insert-section (edmacs-sidebar-agent nil)
          (insert "row\n"))
        (goto-char (point-min))
        (should-error (edmacs-sidebar-agents-visit) :type 'user-error)))

    ;; ==========================================================================
    ;; SPC a TAB attention cycling (AC3)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-goto-attention-cycles-then-reports-none ()
      (edmacs-sidebar-agents-test--with-clean-state
        (let* ((waiting (edmacs-sidebar-agents-test--make-agent
                         :root "/r1/" :instance "%1" :status 'waiting :status-ts 100))
               (unread-done (edmacs-sidebar-agents-test--make-agent
                             :root "/r2/" :instance "%2" :status 'done :unread t :status-ts 200))
               (visited nil) (messages nil))
          (edmacs-sidebar-agents-test--put waiting)
          (edmacs-sidebar-agents-test--put unread-done)
          (cl-letf (((symbol-function 'edmacs-sidebar-agents--visit-common)
                     (lambda (agent) (push agent visited)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
            (edmacs-sidebar-agents-goto-attention)
            (edmacs-sidebar-agents-goto-attention)
            (edmacs-sidebar-agents-goto-attention)
            (setq visited (nreverse visited))
            (should (eq waiting (nth 0 visited)))
            (should (eq unread-done (nth 1 visited)))
            (should (= 2 (length visited)))
            (should (member "no agent wants you" messages))
            ;; A fresh waiting agent appearing resets the cursor rather
            ;; than being skipped by an already-exhausted index.
            (setq visited nil)
            (let ((fresh (edmacs-sidebar-agents-test--make-agent
                          :root "/r3/" :instance "%3" :status 'waiting :status-ts 300)))
              ;; Resolve the two originals so the new attention set is
              ;; unambiguous -- this test is about reset-on-change, not
              ;; sort order among several still-live entries.
              (remhash (edmacs-agent-key waiting) edmacs-agents--table)
              (remhash (edmacs-agent-key unread-done) edmacs-agents--table)
              (edmacs-sidebar-agents-test--put fresh)
              (edmacs-sidebar-agents-goto-attention)
              (should (equal (list fresh) visited)))))))

    ;; ==========================================================================
    ;; Notification transitions + coalescing (AC4, the pure/timer half --
    ;; the live suite covers the real `osascript' shim and real frames)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-transition-to-waiting-always-notifies ()
      (edmacs-sidebar-agents-test--with-clean-state
        (cl-letf (((symbol-function 'frame-focus-state) (lambda (_f) t)))  ; focused
          (let ((queued nil))
            (cl-letf (((symbol-function 'edmacs-sidebar-agents--queue-notify)
                       (lambda (agent) (push agent queued))))
              (let ((agent (edmacs-sidebar-agents-test--make-agent :status 'waiting)))
                (edmacs-sidebar-agents--on-transition agent 'working 'waiting)
                (should (equal (list agent) queued))))))))

    (ert-deftest edmacs-sidebar-agents-test-transition-to-done-notifies-only-when-unfocused ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent :status 'done :unread t)))
        (let ((queued nil))
          (cl-letf (((symbol-function 'edmacs-sidebar-agents--queue-notify)
                     (lambda (a) (push a queued)))
                    ((symbol-function 'edmacs-sidebar-agents--all-frames-unfocused-p)
                     (lambda () t)))
            (edmacs-sidebar-agents--on-transition agent 'working 'done)
            (should (equal (list agent) queued))))
        (let ((queued nil))
          (cl-letf (((symbol-function 'edmacs-sidebar-agents--queue-notify)
                     (lambda (a) (push a queued)))
                    ((symbol-function 'edmacs-sidebar-agents--all-frames-unfocused-p)
                     (lambda () nil)))
            (edmacs-sidebar-agents--on-transition agent 'working 'done)
            (should-not queued)))))

    (ert-deftest edmacs-sidebar-agents-test-transition-to-working-or-idle-never-notifies ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent :status 'working))
            (called nil))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--queue-notify)
                   (lambda (_a) (setq called t))))
          (edmacs-sidebar-agents--on-transition agent 'idle 'working)
          (should-not called)
          (edmacs-sidebar-agents--on-transition agent 'working 'idle)
          (should-not called))))

    (ert-deftest edmacs-sidebar-agents-test-all-frames-unfocused-handles-dead-and-erroring-frames ()
      (cl-letf (((symbol-function 'frame-list) (lambda () (list 'dead-frame 'live-frame)))
                ((symbol-function 'frame-live-p) (lambda (f) (eq f 'live-frame)))
                ((symbol-function 'frame-focus-state)
                 (lambda (f) (if (eq f 'live-frame) nil (error "should never be called")))))
        (should (edmacs-sidebar-agents--all-frames-unfocused-p))))

    (defun edmacs-sidebar-agents-test--wait-until (predicate timeout)
      "Pump the event loop until PREDICATE or TIMEOUT -- same convention as
`edmacs-agents-test--wait-until'/`edmacs-agents-live-test--wait-until'."
      (let ((deadline (+ (float-time) timeout)))
        (while (and (< (float-time) deadline) (not (funcall predicate)))
          (sit-for 0.02))
        (funcall predicate)))

    (ert-deftest edmacs-sidebar-agents-test-notify-coalesces-burst-into-one-call ()
      "Several transitions inside the coalescing window collapse into one
call to `edmacs-sidebar-agents-notify-function' -- a REAL timer, not a
stubbed one, matching agents.el's own real-timer test convention."
      (edmacs-sidebar-agents-test--with-clean-state
        (let* ((edmacs-sidebar-agents-coalesce-seconds 0.2)
               (calls nil)
               (edmacs-sidebar-agents-notify-function
                (lambda (title body) (push (cons title body) calls)))
               (agent1 (edmacs-sidebar-agents-test--make-agent
                        :root "/repo/wt/" :title "one" :status 'waiting))
               (agent2 (edmacs-sidebar-agents-test--make-agent
                        :root "/repo/wt/" :title "two" :status 'waiting)))
          (edmacs-sidebar-agents--queue-notify agent1)
          (edmacs-sidebar-agents--queue-notify agent2)
          (should (edmacs-sidebar-agents-test--wait-until (lambda () calls) 2.0))
          (should (= 1 (length calls)))
          ;; The most recent queued transition is the one that survives.
          (should (equal "two" (caar calls))))))

    (ert-deftest edmacs-sidebar-agents-test-observe-changes-drops-removed-key-no-notify ()
      (edmacs-sidebar-agents-test--with-clean-state
        (let ((key (cons "/repo/wt/" "%1")) (called nil))
          (puthash key 'working edmacs-sidebar-agents--last-state)
          (cl-letf (((symbol-function 'edmacs-sidebar-agents--on-transition)
                     (lambda (&rest _) (setq called t))))
            (edmacs-sidebar-agents--observe-changes (list key))
            (should-not called)
            (should-not (gethash key edmacs-sidebar-agents--last-state))))))

    ;; ==========================================================================
    ;; Elapsed-render timer arm/disarm (AC1's 30s-tick condition)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-should-tick-requires-working-and-visible ()
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :status 'working))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--any-sidebar-visible-p)
                   (lambda () t)))
          (should (edmacs-sidebar-agents--should-tick-p)))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--any-sidebar-visible-p)
                   (lambda () nil)))
          (should-not (edmacs-sidebar-agents--should-tick-p)))))

    (ert-deftest edmacs-sidebar-agents-test-should-tick-false-with-no-working-agent ()
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :status 'idle))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--any-sidebar-visible-p)
                   (lambda () t)))
          (should-not (edmacs-sidebar-agents--should-tick-p)))))

    (ert-deftest edmacs-sidebar-agents-test-ensure-elapsed-timer-arms-and-disarms ()
      (edmacs-sidebar-agents-test--with-clean-state
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--should-tick-p) (lambda () t)))
          (edmacs-sidebar-agents--ensure-elapsed-timer)
          (should (timerp edmacs-sidebar-agents--elapsed-timer)))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--should-tick-p) (lambda () nil)))
          (edmacs-sidebar-agents--ensure-elapsed-timer)
          (should-not edmacs-sidebar-agents--elapsed-timer))))

    (ert-deftest edmacs-sidebar-agents-test-ensure-elapsed-timer-idempotent-when-already-armed ()
      (edmacs-sidebar-agents-test--with-clean-state
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--should-tick-p) (lambda () t)))
          (edmacs-sidebar-agents--ensure-elapsed-timer)
          (let ((first edmacs-sidebar-agents--elapsed-timer))
            (edmacs-sidebar-agents--ensure-elapsed-timer)
            (should (eq first edmacs-sidebar-agents--elapsed-timer))))))

    ;; ==========================================================================
    ;; on-agents-changed wiring: one call does observe + redraw + timer-check
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-on-agents-changed-runs-all-three-steps ()
      (edmacs-sidebar-agents-test--with-clean-state
        (let ((observed nil) (redrawn nil) (checked nil))
          (cl-letf (((symbol-function 'edmacs-sidebar-agents--observe-changes)
                     (lambda (keys) (setq observed keys)))
                    ((symbol-function 'edmacs-sidebar-agents--redraw-all)
                     (lambda () (setq redrawn t)))
                    ((symbol-function 'edmacs-sidebar-agents--ensure-elapsed-timer)
                     (lambda () (setq checked t))))
            (edmacs-sidebar-agents--on-agents-changed (list "k1"))
            (should (equal (list "k1") observed))
            (should redrawn)
            (should checked)))))

    ;; ==========================================================================
    ;; Status faces (phase 8, AC2)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-status-face-per-row ()
      (let ((waiting (edmacs-sidebar-agents-test--make-agent :status 'waiting))
            (done (edmacs-sidebar-agents-test--make-agent :status 'done :unread nil))
            (working (edmacs-sidebar-agents-test--make-agent :status 'working))
            (idle (edmacs-sidebar-agents-test--make-agent :status 'idle)))
        (edmacs-sidebar-agents-test--with-sidebar-buffer
          (edmacs-sidebar-agents--insert-row waiting)
          (edmacs-sidebar-agents--insert-row done)
          (edmacs-sidebar-agents--insert-row working)
          (edmacs-sidebar-agents--insert-row idle)
          (goto-char (point-min))
          (should (eq (get-text-property (point) 'face) 'edmacs-sidebar-agent-waiting-face))
          (forward-line 1)
          (should (eq (get-text-property (point) 'face) 'edmacs-sidebar-agent-done-face))
          (forward-line 1)
          (should (eq (get-text-property (point) 'face) 'edmacs-sidebar-agent-working-face))
          (forward-line 1)
          (should (eq (get-text-property (point) 'face) 'edmacs-sidebar-agent-idle-face)))))

    (ert-deftest edmacs-sidebar-agents-test-unread-done-layers-bold-over-done-face ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent :status 'done :unread t)))
        (edmacs-sidebar-agents-test--with-sidebar-buffer
          (edmacs-sidebar-agents--insert-row agent)
          (should (equal (get-text-property (point-min) 'face)
                          (list 'bold 'edmacs-sidebar-agent-done-face))))))

    ;; ==========================================================================
    ;; Glyph fallback respects `edmacs-sidebar-force-text-glyphs' (phase 8)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-glyph-force-text-overrides-nerd-icons ()
      (cl-letf (((symbol-function 'nerd-icons-faicon) (lambda (_name) "NERD-WORKING"))
                ((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'fboundp) (lambda (f) (eq f 'nerd-icons-faicon)))
                (edmacs-sidebar-force-text-glyphs t))
        (should (equal "*" (edmacs-sidebar-agents--glyph 'working)))))

    ;; ==========================================================================
    ;; edmacs-sidebar-agents--claude-term-session
    ;; ==========================================================================
    ;; Stubs only `claude-term-registry-get' -- a plain defun this module
    ;; does not own but that carries no struct-accessor compiler macro, so
    ;; it stays a safe `cl-letf' target regardless of load order. This is
    ;; the coverage that used to ride along inside the -rename/-kill tests
    ;; above before they moved to stubbing `--claude-term-buffer' instead.

    (ert-deftest edmacs-sidebar-agents-test-claude-term-session-calls-registry-get-with-root-and-instance ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent
                    :root "/repo/wt/" :instance "%1" :source 'claude-term))
            (get-calls nil))
        (cl-letf (((symbol-function 'claude-term-registry-get)
                   (lambda (root instance) (push (cons root instance) get-calls) 'sidebar-agents-test-fake-session)))
          (should (eq (edmacs-sidebar-agents--claude-term-session agent)
                      'sidebar-agents-test-fake-session))
          (should (equal get-calls '(("/repo/wt/" . "%1")))))))

    (ert-deftest edmacs-sidebar-agents-test-claude-term-session-no-session-user-errors ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent
                    :root "/repo/wt/" :instance "%1" :source 'claude-term)))
        (cl-letf (((symbol-function 'claude-term-registry-get) (lambda (&rest _) nil)))
          (should-error (edmacs-sidebar-agents--claude-term-session agent) :type 'user-error))))

    ;; ==========================================================================
    ;; edmacs-sidebar-agents-rename (phase 8)
    ;; ==========================================================================
    ;; The `claude-term' branch is necessarily exercised against a synthetic
    ;; `edmacs-agent' struct and mocked registry/rename functions, not a real
    ;; registry -- end-to-end coverage needs edmacs-claude-terminal's
    ;; claude-term rows, which land in this table only once phase 9 does.

    (ert-deftest edmacs-sidebar-agents-test-rename-claude-term-delegates-to-claude-term-rename ()
      "Delegates to `claude-term-rename' on the buffer
`edmacs-sidebar-agents--claude-term-buffer' resolves -- not just
`claude-term-registry-rename' -- so the buffer-local `claude-term--instance'
and the buffer's own name stay in sync with the registry (see
`edmacs-sidebar-agents-rename's docstring for why a registry-only update
would desync `claude-term--on-exit's deregistration lookup).
Stubs the adapter itself rather than the foreign `claude-term-session-buffer'
accessor it wraps: that accessor is a `cl-defstruct' reader, and loading
`claude-term-registry.el' before this file's own `(load ...)' of
`sidebar-agents.el' (as happens when a caller adds
`-l modules/claude-term-registry.el' to this suite's invocation) makes the
compiler inline it into a type-check-plus-`aref' that a bare `cl-letf'
stub on the accessor can no longer intercept. `cl-letf' on this module's
own plain defun is immune to that inlining in either load order."
      (let ((agent (edmacs-sidebar-agents-test--make-agent
                    :root "/repo/wt/" :instance "%1" :source 'claude-term))
            (buffer-calls nil)
            (rename-calls nil))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--claude-term-buffer)
                   (lambda (a) (push a buffer-calls) 'sidebar-agents-test-fake-buffer))
                  ((symbol-function 'claude-term-rename)
                   (lambda (buffer) (push buffer rename-calls)))
                  ((symbol-function 'edmacs-sidebar-agents--redraw-all) #'ignore))
          (edmacs-sidebar-agents-rename agent)
          (should (equal buffer-calls (list agent)))
          (should (equal rename-calls '(sidebar-agents-test-fake-buffer))))))

    (ert-deftest edmacs-sidebar-agents-test-rename-claude-term-no-session-user-errors ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent :source 'claude-term)))
        (cl-letf (((symbol-function 'claude-term-registry-get) (lambda (&rest _) nil)))
          (should-error (edmacs-sidebar-agents-rename agent) :type 'user-error))))

    (ert-deftest edmacs-sidebar-agents-test-rename-non-claude-term-user-errors ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent :source nil)))
        (should-error (edmacs-sidebar-agents-rename agent) :type 'user-error)))

    ;; ==========================================================================
    ;; edmacs-sidebar-agents-kill (phase 8)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-kill-claude-term-calls-claude-term-kill ()
      "Stubs the `edmacs-sidebar-agents--claude-term-buffer' adapter rather
than the foreign `claude-term-session-buffer' accessor -- see the sibling
rename test's docstring for why: this suite must behave identically
whether or not `claude-term-registry.el' happens to be loaded first."
      (let ((agent (edmacs-sidebar-agents-test--make-agent
                    :root "/repo/wt/" :instance "%1" :source 'claude-term))
            (buffer-calls nil)
            (kill-calls nil))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'edmacs-sidebar-agents--claude-term-buffer)
                   (lambda (a) (push a buffer-calls) 'sidebar-agents-test-fake-buffer))
                  ((symbol-function 'claude-term-kill)
                   (lambda (buffer) (push buffer kill-calls)))
                  ((symbol-function 'edmacs-sidebar-agents--redraw-all) #'ignore))
          (edmacs-sidebar-agents-kill agent)
          (should (equal buffer-calls (list agent)))
          (should (equal kill-calls '(sidebar-agents-test-fake-buffer))))))

    (ert-deftest edmacs-sidebar-agents-test-kill-declines-confirmation-does-nothing ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent :source 'claude-term))
            (called nil))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                  ((symbol-function 'claude-term-registry-get) (lambda (&rest _) (setq called t) nil)))
          (edmacs-sidebar-agents-kill agent)
          (should-not called))))

    (ert-deftest edmacs-sidebar-agents-test-kill-unsupported-source-user-errors ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent :source nil)))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (should-error (edmacs-sidebar-agents-kill agent) :type 'user-error))))

    ;; ==========================================================================
    ;; Header-line roll-up (phase 8)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-header-line-roll-up-counts-per-status ()
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'working))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r2/" :instance "%1" :status 'waiting))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r3/" :instance "%1" :status 'done))
        (should (equal "  [1 working, 1 waiting, 1 done]"
                        (edmacs-sidebar-agents--header-line (selected-frame))))))

    (ert-deftest edmacs-sidebar-agents-test-header-line-nil-when-no-agents ()
      (edmacs-sidebar-agents-test--with-clean-state
        (should-not (edmacs-sidebar-agents--header-line (selected-frame)))))

    (ert-deftest edmacs-sidebar-agents-test-header-line-assigned-to-sidebar-extension-point ()
      (should (eq edmacs-sidebar-header-line-function #'edmacs-sidebar-agents--header-line)))

    ;; ==========================================================================
    ;; Collapsed sidebar section (phase 10)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-empty-when-no-agents ()
      "When no agents are tracked, collapsed section inserts nothing into buffer."
      (edmacs-sidebar-agents-test--with-clean-state
        (with-temp-buffer
          (edmacs-sidebar-agents--collapsed-section (selected-frame) 40)
          (should (equal "" (buffer-string))))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-no-idle-agents ()
      "Idle agents are filtered out; inserts nothing when only idle agents exist."
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'idle))
        (with-temp-buffer
          (edmacs-sidebar-agents--collapsed-section (selected-frame) 40)
          (should (equal "" (buffer-string))))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-formats-non-idle-agents ()
      "Non-idle agents render as glyph + first-letter-of-status, one per line."
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'working))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r2/" :instance "%1" :status 'waiting))
        (with-temp-buffer
          (edmacs-sidebar-agents--collapsed-section (selected-frame) 40)
          (let ((buf-str (buffer-string)))
            ;; Should have multiple lines, one per non-idle agent
            (should-not (string-empty-p buf-str))
            ;; Should contain status indicators
            (should (string-match "w\\|\\*" buf-str))))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-respects-width ()
      "Each agent line fits within WIDTH columns using string-width measurement."
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'working))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r2/" :instance "%1" :status 'waiting))
        (with-temp-buffer
          (edmacs-sidebar-agents--collapsed-section (selected-frame) 20)
          (let ((lines (split-string (buffer-string) "\n" t)))
            ;; Each line should fit within width
            (dolist (line lines)
              (should (<= (string-width line) 20)))))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-applies-face ()
      "Each agent line gets its status face applied."
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'waiting))
        (with-temp-buffer
          (edmacs-sidebar-agents--collapsed-section (selected-frame) 40)
          ;; Check that buffer contains text with face properties
          (let ((buf-str (buffer-string)))
            (should-not (string-empty-p buf-str))
            ;; Text should have face property applied somewhere
            (should (> (length (text-properties-at 0 buf-str)) 0))))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-sorts-by-attention ()
      "Agents appear in attention order (waiting before working via --compare)."
      (edmacs-sidebar-agents-test--with-clean-state
        ;; Create agents in non-attention order to verify sorting
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'working))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r2/" :instance "%1" :status 'waiting))
        (with-temp-buffer
          (edmacs-sidebar-agents--collapsed-section (selected-frame) 40)
          ;; Split buffer into lines and verify waiting comes before working
          (let ((lines (split-string (buffer-string) "\n" t)))
            (should (>= (length lines) 2))
            ;; Find which line has waiting ("w") and which has working ("*" or "?")
            (let ((waiting-pos (seq-position lines "w" (lambda (line str)
                                                          (string-match-p str line))))
                  (working-pos (seq-position lines "\\*" (lambda (line str)
                                                           (string-match-p str line)))))
              ;; When both are present, waiting must come before working
              (when (and waiting-pos working-pos)
                (should (< waiting-pos working-pos))))))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-fits-at-real-width ()
      "At the real production collapsed width (4 columns), glyphs stay recognizable."
      (edmacs-sidebar-agents-test--with-clean-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'working))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r2/" :instance "%1" :status 'waiting))
        (with-temp-buffer
          (edmacs-sidebar-agents--collapsed-section (selected-frame) 4)
          (let ((buf-str (buffer-string)))
            ;; Should contain status indicators (w for waiting or other chars for working)
            (should (string-match-p "[w*?]" buf-str))
            ;; Each line must fit within the real width
            (dolist (line (split-string buf-str "\n" t))
              (should (<= (string-width line) 4)))))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-registered-on-hook ()
      "`edmacs-sidebar-agents--collapsed-section' is registered on the
`edmacs-sidebar-collapsed-section-functions' hook at the head (prepended)."
      (should (memq #'edmacs-sidebar-agents--collapsed-section
                     edmacs-sidebar-collapsed-section-functions)))

    )) ; end of build-root-found branch

;;; sidebar-agents-test.el ends here
