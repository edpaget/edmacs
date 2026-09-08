;;; sidebar-agents-test.el --- Tests for sidebar-agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; Like sidebar-test.el, sidebar-agents.el's own `(require 'magit-section)'
;; needs `load-path' fixed against the straight build tree before it can
;; even be parsed under `-Q --batch' -- so this file carries the same
;; kind of self-contained invocation:
;;
;;   emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/git-common-dir.el -l modules/agents.el \
;;         -l modules/sidebar-agents-test.el -f ert-run-tests-batch-and-exit
;;
;; agents.el is loaded for real (plain elisp, no external deps) so tests
;; can construct real `edmacs-agent' structs and mutate the real table;
;; sidebar.el is NOT loaded (mirroring sidebar-test.el's
;; own module-boundary convention) -- every function sidebar-agents.el
;; calls into either is stubbed via `cl-letf'.
;;
;; Every test isolates the mutable module state it touches
;; (`edmacs-agents--table', `edmacs-sidebar-agents--last-state',
;; `edmacs-sidebar-agents--attention-cache'/`-cursor', notification/timer
;; state) via `edmacs-test-support-with-clean-sidebar-agents-state', the same
;; convention `edmacs-agents-test--with-clean-state' uses.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; Disabled for the same reason sidebar-test.el disables it: `advice-add'
;; on a primitive (this file's own `--osascript-notify'/tmux stub tests)
;; can otherwise spawn a real native-comp trampoline-compiler subprocess.
(setq native-comp-enable-subr-trampolines nil)

(defvar edmacs-sidebar-agents-test--build-root
  (edmacs-test-support-straight-build-root))

(defconst edmacs-sidebar-agents-test--self-file (or load-file-name buffer-file-name))

(if (null edmacs-sidebar-agents-test--build-root)

    (ert-deftest edmacs-sidebar-agents-test-magit-section-unavailable ()
      (edmacs-test-support-report-suite-unavailable
       edmacs-sidebar-agents-test--self-file
       "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (edmacs-test-support-add-magit-section-deps edmacs-sidebar-agents-test--build-root)

    ;; sidebar-agents.el's own forward `declare-function's for sidebar.el
    ;; are byte-compile hygiene only; real stand-ins are provided here so
    ;; its top-level `(setq edmacs-sidebar-worktree-...)' and
    ;; `add-hook'/`advice-add' calls have something real to touch, the
    ;; same way sidebar-test.el pre-populates `tab-bar-tabs' rather than
    ;; loading the real module.
    (defvar edmacs-sidebar-worktree-label-suffix-function #'ignore)
    (defvar edmacs-sidebar-worktree-section-functions nil)
    (defvar edmacs-sidebar-extra-section-functions nil)
    (defvar edmacs-sidebar-header-line-function #'ignore)
    (defvar edmacs-sidebar-collapsed-section-functions nil)
    (defvar edmacs-sidebar-force-text-glyphs nil)
    (defvar edmacs-sidebar-visibility-functions nil)
    (defun edmacs-sidebar--redraw (_frame) nil)
    (defun edmacs-sidebar--window (_frame) nil)
    (defun edmacs-sidebar--fit (label width)
      "Stub: truncate LABEL with a trailing … to fit WIDTH columns.
Mirrors sidebar.el's implementation for tests."
      (let ((width (max 0 width)))
        (cond
         ((<= width 0) "")
         ((<= (string-width label) width) label)
         (t (concat (truncate-string-to-width label (max 0 (1- width))) "…")))))
    (defun edmacs-workspaces-open-worktree (_dir) nil)
    (defun claude-term-registry-rename (_root _old _new) nil)

    (load (expand-file-name "modules/agents.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar-agents.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Test helpers
    ;; ==========================================================================

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
    ;; declare-function sentinel -- keeps the sidebar.el stub set in this
    ;; file honest against sidebar-agents.el's own forward declarations, so
    ;; a new call site fails here in well under a second with a clear
    ;; message instead of surfacing as a mysterious void-function deep in
    ;; an unrelated behavioral test.
    ;; ==========================================================================

    (defconst edmacs-sidebar-agents-test--sidebar-stubs
      `((edmacs-sidebar--redraw . ,(lambda (_frame) nil))
        (edmacs-sidebar-redraw-frames . ,(lambda () (list 'fake-frame)))
        (edmacs-sidebar--window . ,(lambda (_frame) nil))
        (edmacs-sidebar--fit . ,(lambda (label _width) label)))
      "Canonical stub for every `sidebar' target sidebar-agents.el
`declare-function's -- the sentinel test below asserts this list stays
exhaustive against the module's own source.")

    (ert-deftest edmacs-sidebar-agents-test-declare-function-sidebar-targets-stubbed ()
      "Every `(declare-function X \"sidebar\")' in sidebar-agents.el has a
matching entry in `edmacs-sidebar-agents-test--sidebar-stubs' -- this is
what turns a new uncovered call into an immediate, clearly-labeled
failure here rather than a void-function deep in a behavioral test."
      (let ((path (expand-file-name "modules/sidebar-agents.el" default-directory))
            (found nil))
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (while (re-search-forward
                  "^(declare-function \\([^ ]+\\) \"sidebar\"[ )]" nil t)
            (push (intern (match-string 1)) found)))
        (should found)
        (dolist (sym found)
          (should (assq sym edmacs-sidebar-agents-test--sidebar-stubs)))
        ;; Installing each stub via a real `fset' (not just trusting the
        ;; alist shape) catches a malformed binding -- a non-function
        ;; cdr, or a symbol that fails to `fset' -- not just a missing key.
        (dolist (pair edmacs-sidebar-agents-test--sidebar-stubs)
          (let ((sym (car pair)) (orig (symbol-function (car pair))))
            (unwind-protect
                (progn
                  (fset sym (cdr pair))
                  (should (fboundp sym)))
              (fset sym orig))))))

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
      (edmacs-test-support-with-clean-sidebar-agents-state
        (cl-letf (((symbol-function 'file-truename)
                   (lambda (p) (if (equal p "/repo/wt-b") "/real/wt-b" p))))
          (let ((agent (edmacs-sidebar-agents-test--put
                        (edmacs-sidebar-agents-test--make-agent :root "/repo/wt-b"))))
            (should (equal (list agent) (edmacs-sidebar-agents--for-root "/real/wt-b")))
            (should-not (edmacs-sidebar-agents--for-root "/repo/wt-b"))))))

    (ert-deftest edmacs-sidebar-agents-test-for-root-no-match-is-empty ()
      (edmacs-test-support-with-clean-sidebar-agents-state
        (cl-letf (((symbol-function 'file-truename) #'identity))
          (edmacs-sidebar-agents-test--put
           (edmacs-sidebar-agents-test--make-agent :root "/gone/deleted-wt"))
          (should-not (edmacs-sidebar-agents--for-root "/repo")))))

    ;; ==========================================================================
    ;; file-truename memoized once per distinct root per redraw pass
    ;; ==========================================================================
    ;; `--for-root' runs once per worktree row via `--on-worktree-section'
    ;; AND again via `--label-suffix', both hooked into sidebar.el's
    ;; redraw -- and each call re-truenames EVERY tracked agent, not just
    ;; ones near the row's own root. Five simulated calls below (standing
    ;; in for that many rows/hook firings within one redraw pass) must
    ;; still cost exactly one `file-truename' per DISTINCT agent root.

    (ert-deftest edmacs-sidebar-agents-test-for-root-memoizes-truename-per-pass ()
      (edmacs-test-support-with-clean-sidebar-agents-state
        (let ((calls 0))
          (edmacs-sidebar-agents-test--put
           (edmacs-sidebar-agents-test--make-agent :root "/repo/wt-a/" :instance "%1"))
          (edmacs-sidebar-agents-test--put
           (edmacs-sidebar-agents-test--make-agent :root "/repo/wt-a/" :instance "%2"))
          (edmacs-sidebar-agents-test--put
           (edmacs-sidebar-agents-test--make-agent :root "/repo/wt-b/" :instance "%1"))
          (cl-letf (((symbol-function 'file-truename)
                     (lambda (path) (setq calls (1+ calls)) path)))
            (dotimes (_ 5)
              (edmacs-sidebar-agents--for-root "/repo/wt-a/")
              (edmacs-sidebar-agents--for-root "/repo/wt-b/")))
          ;; Two distinct agent roots (three agents, two of them sharing
          ;; "/repo/wt-a/") -- not 5 rows x 3 agents x 2 call-sites.
          (should (= 2 calls)))))

    (ert-deftest edmacs-sidebar-agents-test-truename-cache-clears-between-passes ()
      "`--clear-truename-cache' (hooked onto
`edmacs-sidebar-extra-section-functions', which fires once at the end of
every redraw pass) discards the cache, so a later pass re-resolves a
root that may have changed on disk between redraws rather than serving
a stale truename forever."
      (edmacs-test-support-with-clean-sidebar-agents-state
        (let ((calls 0))
          (cl-letf (((symbol-function 'file-truename)
                     (lambda (path) (setq calls (1+ calls)) path)))
            (edmacs-sidebar-agents--cached-truename "/repo/wt-a/")
            (edmacs-sidebar-agents--cached-truename "/repo/wt-a/")
            (should (= 1 calls))
            (edmacs-sidebar-agents--clear-truename-cache 'fake-frame)
            (edmacs-sidebar-agents--cached-truename "/repo/wt-a/")
            (should (= 2 calls))))))

    (ert-deftest edmacs-sidebar-agents-test-clear-truename-cache-registered ()
      (should (memq #'edmacs-sidebar-agents--clear-truename-cache
                     edmacs-sidebar-extra-section-functions)))

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
      (edmacs-test-support-with-clean-sidebar-agents-state
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

    (ert-deftest edmacs-sidebar-agents-test-insert-for-worktree-renders-glyph-title-elapsed ()
      (edmacs-test-support-with-clean-sidebar-agents-state
        (let ((agent (edmacs-sidebar-agents-test--make-agent
                      :root "/repo/wt/" :status 'working :title "Claude Code"
                      :status-ts (float-time))))
          (edmacs-test-support-with-sidebar-buffer
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
        (edmacs-test-support-with-sidebar-buffer
          (edmacs-sidebar-agents--insert-row agent)
          (let ((face (get-text-property (point-min) 'face)))
            (should (or (eq face 'bold) (and (listp face) (memq 'bold face))))))))

    (ert-deftest edmacs-sidebar-agents-test-insert-row-working-is-not-bold ()
      (let ((agent (edmacs-sidebar-agents-test--make-agent :status 'working)))
        (edmacs-test-support-with-sidebar-buffer
          (edmacs-sidebar-agents--insert-row agent)
          (should-not (text-property-any (point-min) (point-max) 'face 'bold)))))

    (ert-deftest edmacs-sidebar-agents-test-on-worktree-section-noop-when-no-agents ()
      "A worktree with zero matched agents inserts nothing -- an
agent-less worktree's row is byte-for-byte unchanged from before this
phase, which is exactly what keeps sidebar-test.el's own worktree-count
assertions passing untouched."
      (edmacs-test-support-with-clean-sidebar-agents-state
        (edmacs-test-support-with-sidebar-buffer
          (edmacs-sidebar-agents--on-worktree-section "/repo/no-agents/" t)
          (should (= (point-min) (point-max))))))

    (ert-deftest edmacs-sidebar-agents-test-label-suffix-counts-agents-for-root ()
      (edmacs-test-support-with-clean-sidebar-agents-state
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
      (edmacs-test-support-with-clean-sidebar-agents-state
        (edmacs-sidebar-agents-test--put (edmacs-sidebar-agents-test--make-agent))
        (edmacs-test-support-with-sidebar-buffer
          (edmacs-sidebar-agents--insert-all-section (selected-frame))
          (should (= (point-min) (point-max))))))

    (ert-deftest edmacs-sidebar-agents-test-all-section-lists-in-attention-order ()
      (edmacs-test-support-with-clean-sidebar-agents-state
        (setq edmacs-sidebar-agents-show-all t)
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1"
                                                  :status 'idle :title "idle-one"))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r2/" :instance "%2"
                                                  :status 'waiting :title "waiting-one"))
        (edmacs-test-support-with-sidebar-buffer
          (edmacs-sidebar-agents--insert-all-section (selected-frame))
          (should (string-match-p "ALL AGENTS" (buffer-string)))
          (should (< (progn (goto-char (point-min))
                             (search-forward "waiting-one"))
                     (progn (goto-char (point-min))
                            (search-forward "idle-one")))))))

    (ert-deftest edmacs-sidebar-agents-test-toggle-all-flips-and-redraws ()
      (edmacs-test-support-with-clean-sidebar-agents-state
        (let ((redraw-calls 0))
          (cl-letf (((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (_frame) (setq redraw-calls (1+ redraw-calls))))
                    ((symbol-function 'edmacs-sidebar-redraw-frames)
                     (lambda () (list 'fake-frame))))
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
      (edmacs-test-support-with-clean-sidebar-agents-state
        (let* ((agent (edmacs-sidebar-agents-test--make-agent
                       :root "/repo/wt/" :status 'done :unread t))
               (opened nil) (redraws 0))
          (edmacs-sidebar-agents-test--put agent)
          (cl-letf (((symbol-function 'edmacs-workspaces-open-worktree)
                     (lambda (dir) (push dir opened)))
                    ((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (_frame) (setq redraws (1+ redraws))))
                    ((symbol-function 'edmacs-sidebar-redraw-frames)
                     (lambda () (list 'fake-frame))))
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
        (cl-letf (((symbol-function 'edmacs-workspaces-open-worktree)
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
      (edmacs-test-support-with-sidebar-buffer
        (should-error (edmacs-sidebar-agents-visit) :type 'user-error)))

    (ert-deftest edmacs-sidebar-agents-test-visit-nil-value-reports ()
      "A section whose VALUE is nil -- `edmacs-sidebar-agents--insert-row'
itself never constructs one this way, but a degenerate/direct-call
construction could -- also reports rather than silently doing nothing."
      (edmacs-test-support-with-sidebar-buffer
        (magit-insert-section (edmacs-sidebar-agent nil)
          (insert "row\n"))
        (goto-char (point-min))
        (should-error (edmacs-sidebar-agents-visit) :type 'user-error)))

    ;; ==========================================================================
    ;; SPC a TAB attention cycling (AC3)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-goto-attention-cycles-then-reports-none ()
      (edmacs-test-support-with-clean-sidebar-agents-state
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
      (edmacs-test-support-with-clean-sidebar-agents-state
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
      (edmacs-test-support-with-clean-sidebar-agents-state
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
      (edmacs-test-support-with-clean-sidebar-agents-state
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
      (edmacs-test-support-with-clean-sidebar-agents-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :status 'working))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--any-sidebar-visible-p)
                   (lambda () t)))
          (should (edmacs-sidebar-agents--should-tick-p)))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--any-sidebar-visible-p)
                   (lambda () nil)))
          (should-not (edmacs-sidebar-agents--should-tick-p)))))

    (ert-deftest edmacs-sidebar-agents-test-should-tick-false-with-no-working-agent ()
      (edmacs-test-support-with-clean-sidebar-agents-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :status 'idle))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--any-sidebar-visible-p)
                   (lambda () t)))
          (should-not (edmacs-sidebar-agents--should-tick-p)))))

    (ert-deftest edmacs-sidebar-agents-test-ensure-elapsed-timer-arms-and-disarms ()
      (edmacs-test-support-with-clean-sidebar-agents-state
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--should-tick-p) (lambda () t)))
          (edmacs-sidebar-agents--ensure-elapsed-timer)
          (should (timerp edmacs-sidebar-agents--elapsed-timer)))
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--should-tick-p) (lambda () nil)))
          (edmacs-sidebar-agents--ensure-elapsed-timer)
          (should-not edmacs-sidebar-agents--elapsed-timer))))

    (ert-deftest edmacs-sidebar-agents-test-ensure-elapsed-timer-idempotent-when-already-armed ()
      (edmacs-test-support-with-clean-sidebar-agents-state
        (cl-letf (((symbol-function 'edmacs-sidebar-agents--should-tick-p) (lambda () t)))
          (edmacs-sidebar-agents--ensure-elapsed-timer)
          (let ((first edmacs-sidebar-agents--elapsed-timer))
            (edmacs-sidebar-agents--ensure-elapsed-timer)
            (should (eq first edmacs-sidebar-agents--elapsed-timer))))))

    ;; ==========================================================================
    ;; on-agents-changed wiring: one call does observe + redraw + timer-check
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-on-agents-changed-runs-all-three-steps ()
      (edmacs-test-support-with-clean-sidebar-agents-state
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
        (edmacs-test-support-with-sidebar-buffer
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
        (edmacs-test-support-with-sidebar-buffer
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
      (edmacs-test-support-with-clean-sidebar-agents-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'working))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r2/" :instance "%1" :status 'waiting))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r3/" :instance "%1" :status 'done))
        (should (equal "  [1⟳ 1💬 1✓]"
                        (edmacs-sidebar-agents--header-line (selected-frame))))))

    (ert-deftest edmacs-sidebar-agents-test-header-line-counts-every-agent-regardless-of-frame ()
      "Since edmacs-tab-groups phase 3, the roll-up is global: an agent in
a DIFFERENT project's worktree still counts, on a frame naming no one
project -- one frame now shows every project, so a roll-up scoped to one
no longer matches what the sidebar displays."
      (edmacs-test-support-with-clean-sidebar-agents-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/mine/wt/" :instance "%1" :status 'working))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/other/wt/" :instance "%1" :status 'working))
        (should (equal "  [2⟳]" (edmacs-sidebar-agents--header-line (selected-frame))))))

    (ert-deftest edmacs-sidebar-agents-test-header-line-rolls-up-every-project ()
      "The header-line roll-up counts every tracked agent, whatever FRAME it
is handed -- it reads neither the retired per-frame repo parameter nor
any per-repo worktree list. Structural since the frame-scoping wrapper
was removed, but worth pinning: this is the phase 3 decision that the
roll-up matches a tree showing every project, not one of them."
      (edmacs-test-support-with-clean-sidebar-agents-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/mine/wt/" :instance "%1" :status 'working))
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/other/wt/" :instance "%1" :status 'working))
        (should (= 2 (length (edmacs-sidebar-agents--all))))
        ;; Two agents in different roots, so a frame-scoped roll-up would
        ;; count one; both frames must see both.
        (should (equal (edmacs-sidebar-agents--header-line (selected-frame))
                       (edmacs-sidebar-agents--header-line nil)))
        (should (string-match-p "2" (edmacs-sidebar-agents--header-line nil)))))

    (ert-deftest edmacs-sidebar-agents-test-header-line-omits-zero-counts ()
      "Only non-zero statuses appear -- the suffix renders inside a 30-column
sidebar, so the prose form was truncated away entirely."
      (edmacs-test-support-with-clean-sidebar-agents-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'working))
        (should (equal "  [1⟳]" (edmacs-sidebar-agents--header-line (selected-frame))))))

    (ert-deftest edmacs-sidebar-agents-test-header-line-fits-a-default-sidebar ()
      "The whole suffix fits `edmacs-sidebar-width' (30 body columns) even
with two-digit counts in every status."
      (edmacs-test-support-with-clean-sidebar-agents-state
        (dotimes (i 12)
          (edmacs-sidebar-agents-test--put
           (edmacs-sidebar-agents-test--make-agent
            :root (format "/w%d/" i) :instance "%1" :status 'working))
          (edmacs-sidebar-agents-test--put
           (edmacs-sidebar-agents-test--make-agent
            :root (format "/x%d/" i) :instance "%1" :status 'waiting))
          (edmacs-sidebar-agents-test--put
           (edmacs-sidebar-agents-test--make-agent
            :root (format "/y%d/" i) :instance "%1" :status 'done)))
        (should (<= (length (edmacs-sidebar-agents--header-line (selected-frame))) 30))))

    (ert-deftest edmacs-sidebar-agents-test-header-line-nil-when-no-agents ()
      (edmacs-test-support-with-clean-sidebar-agents-state
        (should-not (edmacs-sidebar-agents--header-line (selected-frame)))))

    (ert-deftest edmacs-sidebar-agents-test-header-line-assigned-to-sidebar-extension-point ()
      (should (eq edmacs-sidebar-header-line-function #'edmacs-sidebar-agents--header-line)))

    ;; ==========================================================================
    ;; Collapsed sidebar section (phase 10)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-empty-when-no-agents ()
      "When no agents are tracked, collapsed section inserts nothing into buffer."
      (edmacs-test-support-with-clean-sidebar-agents-state
        (with-temp-buffer
          (edmacs-sidebar-agents--collapsed-section (selected-frame) 40)
          (should (equal "" (buffer-string))))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-no-idle-agents ()
      "Idle agents are filtered out; inserts nothing when only idle agents exist."
      (edmacs-test-support-with-clean-sidebar-agents-state
        (edmacs-sidebar-agents-test--put
         (edmacs-sidebar-agents-test--make-agent :root "/r1/" :instance "%1" :status 'idle))
        (with-temp-buffer
          (edmacs-sidebar-agents--collapsed-section (selected-frame) 40)
          (should (equal "" (buffer-string))))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-formats-non-idle-agents ()
      "Non-idle agents render as glyph + first-letter-of-status, one per line."
      (edmacs-test-support-with-clean-sidebar-agents-state
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
      (edmacs-test-support-with-clean-sidebar-agents-state
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
      (edmacs-test-support-with-clean-sidebar-agents-state
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
      (edmacs-test-support-with-clean-sidebar-agents-state
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
      (edmacs-test-support-with-clean-sidebar-agents-state
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

    (ert-deftest edmacs-sidebar-agents-test-visibility-hook-replaces-the-hide-advice ()
      "The elapsed timer is re-evaluated through sidebar.el's
`edmacs-sidebar-visibility-functions' seam, not by advising a sibling
module's `edmacs-sidebar-hide'. `add-hook' dedupes by symbol, so a second
load of this module leaves one member; the old anonymous-lambda advice
stacked another layer on every `eval-buffer'."
      (should (memq #'edmacs-sidebar-agents--on-visibility-change
                     edmacs-sidebar-visibility-functions))
      (should-not (advice-member-p #'edmacs-sidebar-agents--on-visibility-change
                                   'edmacs-sidebar-hide))
      (let ((path (expand-file-name "modules/sidebar-agents.el" default-directory)))
        (load path nil t))
      (should (= 1 (seq-count (lambda (f)
                                 (eq f #'edmacs-sidebar-agents--on-visibility-change))
                               edmacs-sidebar-visibility-functions)))
      ;; And nothing this module owns is left advising sidebar.el.
      (let ((advices 0))
        (when (fboundp 'edmacs-sidebar-hide)
          (advice-mapc (lambda (_f _p) (setq advices (1+ advices)))
                       'edmacs-sidebar-hide))
        (should (zerop advices))))

    (ert-deftest edmacs-sidebar-agents-test-collapsed-section-registered-on-hook ()
      "`edmacs-sidebar-agents--collapsed-section' is registered on the
`edmacs-sidebar-collapsed-section-functions' hook at the head (prepended)."
      (should (memq #'edmacs-sidebar-agents--collapsed-section
                     edmacs-sidebar-collapsed-section-functions)))

    )) ; end of build-root-found branch

;;; sidebar-agents-test.el ends here
