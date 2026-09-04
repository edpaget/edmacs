;;; agents-test.el --- Tests for agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; Mostly pure-function coverage -- no real subprocess, timer, or watch
;; is involved. `edmacs-agents-init' is exercised directly here (it is
;; idempotent, safe to call repeatedly): every test let-binds
;; `edmacs-agents--table' to a fresh hash table, the same isolation
;; convention `claude-term-registry-test.el' uses for its own table, so
;; no test reads or mutates any real state.
;;
;; The mode-line construct-level tests are the exception: they load the
;; real `nano-modeline' package dynamically from the straight build
;; root (this checkout's, falling back to the sibling main checkout's),
;; the same convention `claude-usage-test.el' uses, and `ert-skip' with
;; a clear message when neither is populated.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/agents.el -l modules/agents-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; ============================================================================
;; nano-modeline straight-build helpers (mirrors
;; claude-usage-test--locate-straight-build-root /
;; claude-usage-test--ensure-nano-modeline / claude-usage-test--construct-has-segment-p
;; -- no shared test-helper module exists in this repo, so every
;; *-test.el duplicates its own copy by convention)
;; ============================================================================

(defun edmacs-agents-test--locate-straight-build-root ()
  "Return this checkout's `straight/build' directory, or nil.
Tries this checkout's own `straight/build' first, then falls back to the
sibling main `edmacs' checkout's `straight/build' -- see
`edmacs-sidebar-test--locate-straight-build-root' for the identical
worktree-vs-sibling-main-checkout rationale."
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

(defun edmacs-agents-test--ensure-nano-modeline ()
  "Load the real `nano-modeline', skipping the calling test if unavailable.
nano-modeline needs only `cl-lib' beyond Emacs core, so a single
`load-path' entry under the straight build root is enough."
  (unless (featurep 'nano-modeline)
    (let* ((root (edmacs-agents-test--locate-straight-build-root))
           (dir (and root (expand-file-name "nano-modeline" root))))
      (unless (and dir (file-directory-p dir))
        (ert-skip (format "nano-modeline's straight build was not found at \
%s; bootstrap straight once (open this worktree in a real Emacs session) to \
enable this test" (or dir "<no straight build root>"))))
      (let ((load-path (cons dir load-path)))
        (require 'nano-modeline)))))

(defun edmacs-agents-test--construct-has-segment-p (form)
  "Non-nil when FORM contains a cons `equal' to
`(edmacs-agents-mode-line-segment)'. Structural rather than evaluated,
for lines that cannot be rendered
outside their own major mode -- `nano-modeline-term-shell-mode' calls
`term-in-char-mode', which needs a live term buffer."
  (cond
   ((equal form '(edmacs-agents-mode-line-segment)) t)
   ((consp form)
    (or (edmacs-agents-test--construct-has-segment-p (car form))
        (edmacs-agents-test--construct-has-segment-p (cdr form))))
   (t nil)))

(defmacro edmacs-agents-test--with-clean-state (&rest body)
  "Run BODY with a fresh agent table and changed hook."
  (declare (indent 0))
  `(let ((edmacs-agents--table (make-hash-table :test #'equal))
         (edmacs-agents-changed-hook nil))
     ,@body))

;; ============================================================================
;; edmacs-agents--compute-unread (the one shared transition rule)
;; ============================================================================

(ert-deftest edmacs-agents-test-compute-unread ()
  "`edmacs-agents--compute-unread' is the single, pure definition of the
unread rule every writer delegates to: `done' sets it, `working'/`waiting'
clears it, anything else (including a fresh row and a done -> done
refresh) preserves whatever it already was."
  ;; Fresh row (no prior state): OLD-STATUS and OLD-UNREAD are nil.
  (should (eq (edmacs-agents--compute-unread nil 'done nil) t))
  (should (eq (edmacs-agents--compute-unread nil 'working nil) nil))
  (should (eq (edmacs-agents--compute-unread nil 'waiting nil) nil))
  ;; Transition into `done' from something else: sets unread regardless
  ;; of what it was before.
  (should (eq (edmacs-agents--compute-unread 'working 'done nil) t))
  (should (eq (edmacs-agents--compute-unread 'waiting 'done t) t))
  ;; Transition into `working'/`waiting': always clears, regardless of
  ;; the prior status or unread value.
  (should (eq (edmacs-agents--compute-unread 'done 'working t) nil))
  (should (eq (edmacs-agents--compute-unread 'idle 'waiting t) nil))
  ;; done -> done refresh (timestamp-only): preserves, does not re-set.
  (should (eq (edmacs-agents--compute-unread 'done 'done nil) nil))
  (should (eq (edmacs-agents--compute-unread 'done 'done t) t))
  ;; Any other transition (e.g. into `idle'): preserves old-unread.
  (should (eq (edmacs-agents--compute-unread 'working 'idle t) t))
  (should (eq (edmacs-agents--compute-unread 'working 'idle nil) nil)))

;; ============================================================================
;; edmacs-agents-init
;; ============================================================================

(ert-deftest edmacs-agents-test-init-arms-no-timer ()
  "`edmacs-agents-init' arms no timer -- its body is exactly the
mode-line splice, so `timer-list' is unchanged across the call."
  (edmacs-agents-test--with-clean-state
    (let ((global-mode-string nil)
          (edmacs-agents--mode-line-string ""))
      (unwind-protect
          (let ((before (copy-sequence timer-list)))
            (edmacs-agents-init)
            (should (equal timer-list before)))
        (remove-hook 'edmacs-agents-changed-hook #'edmacs-agents--refresh-mode-line)))))

;; ============================================================================
;; edmacs-agents-set-status
;; ============================================================================

(ert-deftest edmacs-agents-test-set-status-fires-hook-once ()
  "Each call to `edmacs-agents-set-status' fires the changed hook exactly once."
  (edmacs-agents-test--with-clean-state
    (let* ((root (make-temp-file "edmacs-agents-test-root-" t))
           (fire-count 0))
      (add-hook 'edmacs-agents-changed-hook (lambda (_keys) (cl-incf fire-count)))
      (edmacs-agents-set-status root 'waiting)
      (should (= fire-count 1))
      (should (= (hash-table-count edmacs-agents--table) 1))
      (edmacs-agents-set-status root 'working)
      (should (= fire-count 2))
      (should (= (hash-table-count edmacs-agents--table) 1))
      (let (row)
        (maphash (lambda (_k r) (setq row r)) edmacs-agents--table)
        (should (eq (edmacs-agent-status row) 'working))))))

(ert-deftest edmacs-agents-test-set-status-ambiguous-instance-errors ()
  "`edmacs-agents-set-status' with no INSTANCE and two existing rows
under the same root signals a clear `user-error' instead of guessing."
  (edmacs-agents-test--with-clean-state
    (let* ((root (make-temp-file "edmacs-agents-test-root-" t)))
      (edmacs-agents-set-status root 'waiting "a")
      (edmacs-agents-set-status root 'waiting "b")
      (should-error (edmacs-agents-set-status root 'working) :type 'user-error))))

;; ============================================================================
;; Nothing reaps on age any more
;; ============================================================================

(ert-deftest edmacs-agents-test-no-age-based-reap ()
  "A row created through the public `edmacs-agents-set-status' writer
survives no matter how old its heartbeat is -- there is no sweep any
more, so a `waiting' or unread-`done' row is never silently discarded
for sitting quiet."
  (edmacs-agents-test--with-clean-state
    (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t))))
      (edmacs-agents-set-status root 'waiting)
      (let (key row)
        (maphash (lambda (k r) (setq key k row r)) edmacs-agents--table)
        ;; Force the heartbeat far into the past, as if the row had sat
        ;; quietly in `waiting' for hours with no further explicit call.
        (setf (edmacs-agent-updated-ts row) (- (float-time) 999999))
        (edmacs-agents-init)
        (should (gethash key edmacs-agents--table))
        (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'waiting))))))

;; ============================================================================
;; Unread transitions
;; ============================================================================

(ert-deftest edmacs-agents-test-unread-transition ()
  "A transition into `done' sets `unread'; `edmacs-agents-mark-read'
clears it and moves the row to `idle'."
  (edmacs-agents-test--with-clean-state
    (let* ((root (make-temp-file "edmacs-agents-test-root-" t)))
      (edmacs-agents-set-status root 'working)
      (let (key)
        (maphash (lambda (k _r) (setq key k)) edmacs-agents--table)
        (edmacs-agents-set-status root 'done)
        (should (edmacs-agent-unread (gethash key edmacs-agents--table)))
        (edmacs-agents-mark-read key)
        (should-not (edmacs-agent-unread (gethash key edmacs-agents--table)))
        (should (eq (edmacs-agent-status (gethash key edmacs-agents--table)) 'idle))))))

(ert-deftest edmacs-agents-test-unread-refresh-does-not-retrigger ()
  "A done -> done timestamp-only refresh (status never left `done' in
between) does not spuriously re-set `unread' once cleared -- contrast
with `edmacs-agents-test-unread-transition', where the row passes back
through `idle' (via `edmacs-agents-mark-read') before going `done'
again, which IS a fresh transition and DOES re-set `unread'."
  (edmacs-agents-test--with-clean-state
    (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (key (edmacs-agents--key root "%1"))
           (row (make-edmacs-agent :key key :root root :instance "%1"
                                    :status 'done :status-ts 100 :updated-ts 100
                                    :title "t" :source 'claude-term :locator nil
                                    :unread (edmacs-agents--compute-unread nil 'done nil))))
      (edmacs-agents--upsert row)
      (should (edmacs-agent-unread (gethash key edmacs-agents--table)))
      ;; Cleared by some means other than mark-read, while status stays
      ;; `done' throughout -- e.g. a future "peek" affordance.
      (setf (edmacs-agent-unread (gethash key edmacs-agents--table)) nil)
      ;; Refresh: status still `done', so this is not a fresh transition.
      (let ((refreshed (gethash key edmacs-agents--table)))
        (setf (edmacs-agent-status-ts refreshed) 150
              (edmacs-agent-updated-ts refreshed) 150
              (edmacs-agent-unread refreshed)
              (edmacs-agents--compute-unread 'done 'done (edmacs-agent-unread refreshed)))
        (edmacs-agents--upsert refreshed))
      (should-not (edmacs-agent-unread (gethash key edmacs-agents--table))))))

(ert-deftest edmacs-agents-test-mark-read-bumps-status-ts ()
  "`edmacs-agents-mark-read' clears unread, sets status to `idle', and
bumps STATUS-TS to (approximately) now."
  (edmacs-agents-test--with-clean-state
    (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (key (edmacs-agents--key root "%1"))
           (row (make-edmacs-agent :key key :root root :instance "%1"
                                    :status 'done :status-ts 100 :updated-ts 100
                                    :title "t" :source 'claude-term :locator nil
                                    :unread t)))
      (edmacs-agents--upsert row)
      (edmacs-agents-mark-read key)
      (let ((updated (gethash key edmacs-agents--table)))
        (should (eq (edmacs-agent-status updated) 'idle))
        (should-not (edmacs-agent-unread updated))
        (should (< (abs (- (edmacs-agent-status-ts updated) (float-time))) 5))))))

;; ============================================================================
;; Mode-line roll-up
;; ============================================================================

(ert-deftest edmacs-agents-test-mode-line-string ()
  "The roll-up string counts working/unread-done/waiting rows, and is
empty once the table has none of those."
  (edmacs-agents-test--with-clean-state
    (let ((mk (lambda (instance status unread)
                (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
                       (key (edmacs-agents--key root instance)))
                  (puthash key (make-edmacs-agent :key key :root root :instance instance
                                                   :status status :status-ts 1 :updated-ts 1
                                                   :title "t" :source 'claude-term :locator nil
                                                   :unread unread)
                           edmacs-agents--table)))))
      (funcall mk "%1" 'working nil)
      (funcall mk "%2" 'working nil)
      (funcall mk "%3" 'done t)
      (funcall mk "%4" 'done nil) ; done but already read: not counted
      (funcall mk "%5" 'waiting nil)
      (should (equal (edmacs-agents--mode-line-string-compute) "[2⟳ 1✓ 1💬]")))
    (clrhash edmacs-agents--table)
    (should (equal (edmacs-agents--mode-line-string-compute) ""))))

(defmacro edmacs-agents-test--with-mode-line-state (&rest body)
  "Run BODY with the real mode-line globals saved and restored, and the
`:filter-args' advice + changed-hook removed afterward regardless of
how BODY exits."
  (declare (indent 0))
  `(let ((edmacs-agents-test--saved-string edmacs-agents--mode-line-string)
         (edmacs-agents-test--saved-default (default-value 'mode-line-format)))
     (unwind-protect
         (progn ,@body)
       (advice-remove 'nano-modeline-footer
                       #'edmacs-agents--nano-modeline-footer-filter-args)
       (remove-hook 'edmacs-agents-changed-hook #'edmacs-agents--refresh-mode-line)
       (setq-default mode-line-format edmacs-agents-test--saved-default)
       (setq edmacs-agents--mode-line-string edmacs-agents-test--saved-string))))

(ert-deftest edmacs-agents-test-mode-line-reaches-nano-construct ()
  "`edmacs-agents--ensure-mode-line' wires the roll-up into the real
nano-modeline `:eval' construct: in the default line (re-baked at
install time, since ui.el bakes it long before `edmacs-agents-init'
runs), and in a plain buffer that was already alive before install and
never baked a mode-line of its own -- dired, magit, the sidebar,
`*claude-usage*' all fall in that second category, inheriting whatever
`(default-value \\='mode-line-format)' holds rather than freezing their
own copy -- rather than merely landing a symbol in `global-mode-string',
a channel nano-modeline never reads (asserted unchanged below).

`format-mode-line' cannot be used here: it returns \"\" under `--batch',
so every assertion evaluates `(cadr ...)' of the construct directly."
  (edmacs-agents-test--ensure-nano-modeline)
  (edmacs-agents-test--with-clean-state
    (let ((buf (generate-new-buffer " *edmacs-agents-test-pre-existing*"))
          (nano-modeline-position #'nano-modeline-footer)
          (global-mode-string 'edmacs-agents-test--untouched-sentinel))
      (unwind-protect
          (edmacs-agents-test--with-mode-line-state
            (edmacs-agents--ensure-mode-line)
            ;; Never wrote to the dead channel.
            (should (eq global-mode-string 'edmacs-agents-test--untouched-sentinel))
            ;; Plant a sentinel after install: the changed-hook recompute
            ;; would otherwise clobber it the other way round.
            (setq edmacs-agents--mode-line-string "ZZAGENTSZZ")
            ;; A second buffer, baked fresh after install.
            (with-temp-buffer (nano-modeline-text-mode t))
            (should (string-match-p
                     "ZZAGENTSZZ" (eval (cadr (default-value 'mode-line-format)) t)))
            (with-current-buffer buf
              (should (string-match-p "ZZAGENTSZZ" (eval (cadr mode-line-format) t)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest edmacs-agents-test-filter-args-two-argument-and-idempotent ()
  "The `:filter-args' function handles the two-argument `nano-modeline-footer'
call -- the shape `edmacs-modeline-ghostel-mode',
`nano-modeline-message-mode' and `nano-modeline-term-mode' all use --
without mutating the shared RIGHT literal, and re-applying it to its
own output is a no-op (no doubled element across a re-install)."
  (let* ((right (list '(nano-modeline-window-dedicated)))
         (right-before (copy-tree right))
         (out (edmacs-agents--nano-modeline-footer-filter-args
               (list (list '(nano-modeline-buffer-status)) right))))
    (should (= (length out) 3))
    (should (equal (nth 1 out)
                   '((nano-modeline-window-dedicated)
                     (edmacs-agents-mode-line-segment))))
    (should (null (nth 2 out)))
    (should (equal right right-before))
    (should (equal (edmacs-agents--nano-modeline-footer-filter-args out) out))))

(ert-deftest edmacs-agents-test-mode-line-in-ghostel-and-term-lines ()
  "The roll-up reaches `nano-modeline-message-mode' and
`nano-modeline-term-mode', both of which call `nano-modeline-footer'
with no DEFAULT argument -- the same two-argument shape
`edmacs-modeline-ghostel-mode' (ui.el's wrapper for every claude-term
agent pane, the exact buffers this roll-up summarizes) uses; ui.el
itself is not loaded here since it bootstraps straight's `use-package'
machinery, so it is exercised opportunistically when already loaded
rather than required for this test to pass."
  (edmacs-agents-test--ensure-nano-modeline)
  (edmacs-agents-test--with-clean-state
    (let ((nano-modeline-position #'nano-modeline-footer))
      (edmacs-agents-test--with-mode-line-state
        (edmacs-agents--ensure-mode-line)
        (setq edmacs-agents--mode-line-string "ZZAGENTSZZ")
        (with-temp-buffer
          (nano-modeline-message-mode)
          (should (string-match-p "ZZAGENTSZZ" (eval (cadr mode-line-format) t))))
        (with-temp-buffer
          (nano-modeline-term-mode)
          (should (edmacs-agents-test--construct-has-segment-p mode-line-format)))
        (when (fboundp 'edmacs-modeline-ghostel-mode)
          (with-temp-buffer
            (edmacs-modeline-ghostel-mode)
            (should (string-match-p "ZZAGENTSZZ" (eval (cadr mode-line-format) t)))))))))

;; ============================================================================
;; Tabulated-list view
;; ============================================================================

(ert-deftest edmacs-agents-test-list-entries ()
  "`edmacs-agents--list-entries' returns one `tabulated-list-entries' row
per table row, keyed by the row's own key, with the unread `done' flag
rendered as a trailing `*' and a read `idle' row rendered without one."
  (edmacs-agents-test--with-clean-state
    (let* ((root (file-truename (make-temp-file "edmacs-agents-test-root-" t)))
           (done-key (edmacs-agents--key root "%1"))
           (idle-key (edmacs-agents--key root "%2")))
      (puthash done-key (make-edmacs-agent
                          :key done-key :root root :instance "%1"
                          :status 'done :status-ts 1 :updated-ts 1
                          :title "Fixing the thing" :source 'claude-term
                          :locator nil :unread t)
               edmacs-agents--table)
      (puthash idle-key (make-edmacs-agent
                          :key idle-key :root root :instance "%2"
                          :status 'idle :status-ts 1 :updated-ts 1
                          :title "Already read" :source 'claude-term
                          :locator nil :unread nil)
               edmacs-agents--table)
      (let ((entries (edmacs-agents--list-entries)))
        (should (= (length entries) 2))
        (should (assoc done-key entries))
        (should (assoc idle-key entries))
        (let ((done-cols (cadr (assoc done-key entries)))
              (idle-cols (cadr (assoc idle-key entries))))
          (should (equal (aref done-cols 0) "%1"))
          (should (equal (aref done-cols 1) root))
          (should (equal (aref done-cols 2) "%1"))
          (should (equal (aref done-cols 3) "done*"))
          (should (equal (aref done-cols 4) "Fixing the thing"))
          (should (equal (aref idle-cols 3) "idle"))
          (should (equal (aref idle-cols 4) "Already read")))))))

(ert-deftest edmacs-agents-test-list-entries-empty ()
  "An empty table produces an empty entries list, not an error."
  (edmacs-agents-test--with-clean-state
    (should (equal (edmacs-agents--list-entries) nil))))

(provide 'agents-test)
;;; agents-test.el ends here
