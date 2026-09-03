;;; claude-term-agents-test.el --- Tests for claude-term-agents.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage only -- no real ghostel/`claude' subprocess, no
;; real frame or window beyond what `generate-new-buffer'/`display-buffer'
;; give for free in batch. See sidebar-agents-live-test.el's
;; `edmacs-sidebar-agents-live-test-real-claude-term-row-visit-and-kill'
;; for the real-subprocess, real-registry end-to-end coverage this file
;; cannot exercise on its own.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/claude-term.el \
;;         -l modules/claude-term-registry.el \
;;         -l modules/agents.el \
;;         -l modules/claude-term-agents.el \
;;         -l modules/claude-term-agents-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; `(defvar ghostel-progress-function)' in claude-term-agents.el only
;; proclaims the symbol special for THAT file's remaining lexical
;; scope, not globally -- this file needs its own bare `defvar' before
;; `let'-binding it below, or the bind becomes an ordinary lexical
;; shadow and `claude-term-agents--install-progress-handler' (reading
;; the real dynamic variable) signals `void-variable'.
(defvar ghostel-progress-function)

(defmacro claude-term-agents-test--with-clean-state (&rest body)
  "Run BODY with a fresh agent table and changed hook, isolated from any
other suite's or this machine's real state."
  (declare (indent 0))
  `(let ((edmacs-agents--table (make-hash-table :test #'equal))
         (edmacs-agents-changed-hook nil))
     ,@body))

(cl-defun claude-term-agents-test--seed-workmux-row
    (&key (root "/repo/wt/") (instance "%9") (status 'working))
  "Insert a stubbed `workmux'-sourced row under ROOT/INSTANCE and return it."
  (let* ((key (edmacs-agents--key root instance))
         (row (make-edmacs-agent
               :key key :root root :instance instance
               :status status :status-ts (float-time) :updated-ts (float-time)
               :title "tmux pane" :source 'workmux
               :locator (list :pane-id instance :session "s1" :window "w1")
               :unread nil)))
    (puthash key row edmacs-agents--table)
    row))

;; ============================================================================
;; Row lifecycle: create/remove hooks
;; ============================================================================

(ert-deftest claude-term-agents-test-on-create-adds-correctly-keyed-row ()
  "The create-functions handler adds a `claude-term'-sourced, `idle' row
keyed exactly like `edmacs-agents--key' would build it, with LOCATOR set
to the spawning buffer."
  (claude-term-agents-test--with-clean-state
    (let* ((root (make-temp-file "claude-term-agents-test-root-" t))
           (buf (generate-new-buffer "claude-term-agents-test-buf")))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root "ghostel-1" buf)
            (let* ((key (edmacs-agents--key root "ghostel-1"))
                   (row (gethash key edmacs-agents--table)))
              (should row)
              (should (equal (edmacs-agent-key row) key))
              (should (equal (edmacs-agent-root row) (file-truename root)))
              (should (equal (edmacs-agent-instance row) "ghostel-1"))
              (should (eq (edmacs-agent-status row) 'idle))
              (should (eq (edmacs-agent-source row) 'claude-term))
              (should (eq (edmacs-agent-locator row) buf))
              (should (equal (edmacs-agent-title row) "ghostel-1"))
              (should-not (edmacs-agent-unread row))))
        (kill-buffer buf)
        (ignore-errors (delete-directory root t))))))

(ert-deftest claude-term-agents-test-on-create-truenames-a-non-truename-root ()
  "ROOT arriving un-truename'd (as `claude-term-session's own ROOT field
is documented to be) still produces a row whose ROOT field, and whose
table key, are truename'd -- matching `edmacs-agents--key's own
normalization, so `edmacs-agents-set-status' lookups against the
truename'd form still find it."
  (claude-term-agents-test--with-clean-state
    (let* ((real-dir (directory-file-name
                       (make-temp-file "claude-term-agents-test-real-" t)))
           (link-path (concat real-dir "-link"))
           (buf (generate-new-buffer "claude-term-agents-test-link-buf")))
      (unwind-protect
          (progn
            (make-symbolic-link real-dir link-path)
            (claude-term-agents--on-create (file-name-as-directory link-path)
                                            "i1" buf)
            (let ((row (gethash (edmacs-agents--key (file-name-as-directory link-path) "i1")
                                 edmacs-agents--table)))
              (should row)
              (should (equal (edmacs-agent-root row)
                              (file-truename (file-name-as-directory real-dir))))))
        (kill-buffer buf)
        (ignore-errors (delete-file link-path))
        (ignore-errors (delete-directory real-dir t))))))

(ert-deftest claude-term-agents-test-on-create-refresh-is-idempotent-upsert ()
  "A second create call for the same ROOT/INSTANCE (a restart's re-exec)
overwrites the same key rather than adding a second row."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf1 (generate-new-buffer "claude-term-agents-test-buf1"))
           (buf2 (generate-new-buffer "claude-term-agents-test-buf2")))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root "i1" buf1)
            (claude-term-agents--on-create root "i1" buf2)
            (should (= 1 (hash-table-count edmacs-agents--table)))
            (should (eq (edmacs-agent-locator
                         (gethash (edmacs-agents--key root "i1") edmacs-agents--table))
                        buf2)))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

(ert-deftest claude-term-agents-test-on-remove-deletes-only-that-row ()
  "The remove-functions handler deletes exactly the targeted row and
leaves a stubbed workmux row under the same root untouched."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf")))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root "ghostel-1" buf)
            (let ((workmux-row (claude-term-agents-test--seed-workmux-row :root root)))
              (claude-term-agents--on-remove root "ghostel-1")
              (should-not (gethash (edmacs-agents--key root "ghostel-1") edmacs-agents--table))
              (should (equal workmux-row (gethash (edmacs-agent-key workmux-row)
                                                   edmacs-agents--table)))))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-on-remove-nil-instance-finds-normalized-row ()
  "Removing with a nil INSTANCE (the default session ending) must find
the row `claude-term-agents--on-create' stored under the normalized
key, not under a literal nil the row was never keyed by."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf")))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root nil buf)
            (claude-term-agents--on-remove root nil)
            (should (= 0 (hash-table-count edmacs-agents--table))))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-on-remove-unknown-key-is-a-no-op ()
  "Removing a ROOT/INSTANCE with no matching row does not error -- a
session killed while a status update against the same row races."
  (claude-term-agents-test--with-clean-state
    (should-not (claude-term-agents--on-remove "/no/such/root/" "ghost"))
    ;; Doubled: idempotent even when called twice in a row.
    (should-not (claude-term-agents--on-remove "/no/such/root/" "ghost"))))

;; ============================================================================
;; Row lifecycle: rename hook (the registry's third mutating entry point,
;; alongside put/remove -- see `claude-term-registry-rename-functions')
;; ============================================================================

(ert-deftest claude-term-agents-test-on-rename-re-keys-preserving-status ()
  "The rename-functions handler moves the row from the old key to the new
one, preserving its STATUS/UNREAD rather than resetting to `idle' -- a
rename mid-`working' must not silently discard that state."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf")))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root "old" buf)
            (let ((row (gethash (edmacs-agents--key root "old") edmacs-agents--table)))
              (setf (edmacs-agent-status row) 'waiting
                    (edmacs-agent-unread row) t))
            (claude-term-agents--on-rename root "old" "new")
            (should-not (gethash (edmacs-agents--key root "old") edmacs-agents--table))
            (let ((row (gethash (edmacs-agents--key root "new") edmacs-agents--table)))
              (should row)
              (should (equal (edmacs-agent-key row) (edmacs-agents--key root "new")))
              (should (equal (edmacs-agent-instance row) "new"))
              (should (equal (edmacs-agent-title row) "new"))
              (should (eq (edmacs-agent-status row) 'waiting))
              (should (edmacs-agent-unread row))
              (should (eq (edmacs-agent-locator row) buf))))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-on-rename-unknown-old-key-is-a-no-op ()
  "Renaming a ROOT/OLD-INSTANCE with no matching row does not error and
does not fabricate a new row -- e.g. a rename racing ahead of this
file's own create listener."
  (claude-term-agents-test--with-clean-state
    (should-not (claude-term-agents--on-rename "/no/such/root/" "old" "new"))
    (should (zerop (hash-table-count edmacs-agents--table)))))

(ert-deftest claude-term-agents-test-on-rename-leaves-other-rows-untouched ()
  "Renaming one claude-term row does not disturb a stubbed workmux row
under the same root."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf"))
           (workmux-row (claude-term-agents-test--seed-workmux-row :root root)))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root "old" buf)
            (claude-term-agents--on-rename root "old" "new")
            (should (equal workmux-row (gethash (edmacs-agent-key workmux-row)
                                                 edmacs-agents--table))))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-registry-rename-fires-through-real-hook-chain ()
  "Not just the handler called directly: `claude-term-registry-rename'
itself, through the real `claude-term-registry-rename-functions' hook
this file's `add-hook' registers at load time, moves the mirrored row --
closing the gap where the registry's rename entry point bypassed both
of the other two hooks by direct `remhash'/`puthash'."
  (claude-term-agents-test--with-clean-state
    (let ((claude-term-registry--table (make-hash-table :test #'equal))
          (root (make-temp-file "claude-term-agents-test-rename-root-" t))
          (buf (generate-new-buffer "claude-term-agents-test-rename-buf")))
      (unwind-protect
          (progn
            (claude-term-registry-put root "old" buf)
            (let ((row (gethash (edmacs-agents--key root "old") edmacs-agents--table)))
              (should row)
              (setf (edmacs-agent-status row) 'working))
            (claude-term-registry-rename root "old" "new")
            (should-not (gethash (edmacs-agents--key root "old") edmacs-agents--table))
            (let ((row (gethash (edmacs-agents--key root "new") edmacs-agents--table)))
              (should row)
              (should (equal (edmacs-agent-instance row) "new"))
              (should (eq (edmacs-agent-status row) 'working))))
        (kill-buffer buf)
        (ignore-errors (delete-directory root t))))))

;; ============================================================================
;; edmacs-agents-set-status: `remove' status, and instance targeting
;; ============================================================================

(ert-deftest claude-term-agents-test-set-status-flips-only-the-claude-term-row ()
  "`edmacs-agents-set-status' with an explicit INSTANCE flips only that
row; a workmux row under the same root is byte-identical afterward."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf"))
           (workmux-row (claude-term-agents-test--seed-workmux-row :root root)))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root "ghostel-1" buf)
            (edmacs-agents-set-status root 'waiting "ghostel-1")
            (should (eq 'waiting (edmacs-agent-status
                                   (gethash (edmacs-agents--key root "ghostel-1")
                                            edmacs-agents--table))))
            (should (equal workmux-row (gethash (edmacs-agent-key workmux-row)
                                                 edmacs-agents--table))))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-on-create-default-instance-key-agrees-with-hook ()
  "A session launched with no INSTANCE (nil) is what
`claude-term-registry--set-instance-env' always resolves to
`claude-term-registry--default-instance-label' \(never leaves
`EDMACS_AGENT_INSTANCE' unset\), so the ported status hook always calls
`edmacs-agents-set-status' with that literal string, never with nil.
The row `claude-term-agents--on-create' stores for a nil INSTANCE must
therefore already be keyed under that label, so the very first status
update the hook sends UPDATES this row instead of creating a second,
`:source' nil phantom one alongside it."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf")))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root nil buf)
            (should (= 1 (hash-table-count edmacs-agents--table)))
            (edmacs-agents-set-status root 'waiting
                                       claude-term-registry--default-instance-label)
            ;; Still exactly one row -- the hook's update landed on the
            ;; adapter's own row, not a freshly fabricated one.
            (should (= 1 (hash-table-count edmacs-agents--table)))
            (let ((row (gethash (edmacs-agents--key
                                  root claude-term-registry--default-instance-label)
                                 edmacs-agents--table)))
              (should row)
              (should (eq (edmacs-agent-source row) 'claude-term))
              (should (eq (edmacs-agent-locator row) buf))
              (should (eq (edmacs-agent-status row) 'waiting))))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-on-create-nil-instance-keeps-raw-instance-field ()
  "The row's stored INSTANCE field stays the raw nil `claude-term-registry-put'
was actually called with -- not the normalized display label used for
the KEY/TITLE -- because `edmacs-sidebar-agents--claude-term-session'
feeds this field straight into `claude-term-registry-get', whose own
key (`claude-term-registry--key') is never normalized: a default
session is registered, and stays registered, under a literal nil."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf")))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root nil buf)
            (let ((row (gethash (edmacs-agents--key
                                  root claude-term-registry--default-instance-label)
                                 edmacs-agents--table)))
              (should row)
              (should-not (edmacs-agent-instance row))
              (should (equal (edmacs-agent-title row)
                              claude-term-registry--default-instance-label))))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-set-status-no-instance-sole-default-row-updates-it ()
  "`edmacs-agents-set-status' called with NO instance argument at all,
against a root with exactly one (default-instance) claude-term row,
must resolve via that row's own KEY rather than its raw (nil) INSTANCE
field -- deriving from the field would look up a key this row was
never stored under and spawn a duplicate, the same class of bug as the
hook-driven case this phase closes."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf")))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root nil buf)
            (edmacs-agents-set-status root 'working)
            (should (= 1 (hash-table-count edmacs-agents--table)))
            (let ((row (gethash (edmacs-agents--key
                                  root claude-term-registry--default-instance-label)
                                 edmacs-agents--table)))
              (should row)
              (should (eq (edmacs-agent-status row) 'working))))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-set-status-remove-deletes-targeted-row-only ()
  "`edmacs-agents-set-status' with STATUS `remove' deletes exactly the
resolved row (the fix for the internal enum gap this phase closes) and
leaves a workmux row under the same root in the table."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf"))
           (workmux-row (claude-term-agents-test--seed-workmux-row :root root)))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root "ghostel-1" buf)
            (edmacs-agents-set-status root 'remove "ghostel-1")
            (should-not (gethash (edmacs-agents--key root "ghostel-1") edmacs-agents--table))
            (should (gethash (edmacs-agent-key workmux-row) edmacs-agents--table)))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-set-status-remove-unknown-row-is-a-no-op ()
  "STATUS `remove' against a CWD/INSTANCE with no matching row does not
error and creates nothing."
  (claude-term-agents-test--with-clean-state
    (edmacs-agents-set-status "/repo/wt/" 'remove "no-such-instance")
    (should (= 0 (hash-table-count edmacs-agents--table)))))

(ert-deftest claude-term-agents-test-set-status-ambiguous-without-instance-errors ()
  "Two rows (of any source) under the same root with no INSTANCE given
still raises the existing ambiguity `user-error' -- no claude-term-only
source-filtering is added by this phase."
  (claude-term-agents-test--with-clean-state
    (let* ((root "/repo/wt/")
           (buf (generate-new-buffer "claude-term-agents-test-buf")))
      (unwind-protect
          (progn
            (claude-term-agents--on-create root "ghostel-1" buf)
            (claude-term-agents-test--seed-workmux-row :root root :instance "%9")
            (should-error (edmacs-agents-set-status root 'waiting) :type 'user-error))
        (kill-buffer buf)))))

;; ============================================================================
;; Ghostel OSC 9;4 progress -> title suffix
;; ============================================================================

(defmacro claude-term-agents-test--with-fake-session (root instance status &rest body)
  "Run BODY in a fresh buffer with `claude-term--root'/`-instance' set to
ROOT/INSTANCE, and a matching STATUS row already in the (already
let-bound) `edmacs-agents--table'."
  (declare (indent 3))
  (let ((root-var (gensym "root")) (instance-var (gensym "instance")))
    `(let* ((,root-var ,root)
            (,instance-var ,instance)
            (buf (generate-new-buffer "claude-term-agents-test-progress-buf")))
       (unwind-protect
           (with-current-buffer buf
             (setq-local claude-term--root ,root-var)
             (setq-local claude-term--instance ,instance-var)
             (claude-term-agents--on-create ,root-var ,instance-var buf)
             ;; The row is keyed by the NORMALIZED instance (see
             ;; `claude-term-agents--on-create'), which differs from a
             ;; nil INSTANCE-VAR -- look it up the same way
             ;; `claude-term-agents--update-progress-title' does.
             (setf (edmacs-agent-status
                    (gethash (edmacs-agents--key
                              ,root-var
                              (claude-term-agents--normalize-instance ,instance-var))
                             edmacs-agents--table))
                   ,status)
             ,@body)
         (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-progress-set-while-working-appends-suffix ()
  (claude-term-agents-test--with-clean-state
    (claude-term-agents-test--with-fake-session "/repo/wt/" "i1" 'working
      (claude-term-agents--update-progress-title 'set 42)
      (should (equal "i1 42%"
                      (edmacs-agent-title (gethash (edmacs-agents--key "/repo/wt/" "i1")
                                                    edmacs-agents--table)))))))

(ert-deftest claude-term-agents-test-progress-set-with-nil-instance-still-renders ()
  "A legitimately nil INSTANCE (the default, non-multi-instance session)
must not be treated as an absent binding: the progress suffix still
renders for it, the same as for a named instance. `when-let*' folding
INSTANCE itself into its binding chain would silently drop this whole
feature for the ordinary single-session case, and the row's TITLE must
already have been defaulted away from a bare nil at create time (see
`claude-term-agents--on-create'), or `string-match' inside
`claude-term-agents--strip-progress-suffix' signals instead of
rendering anything at all."
  (claude-term-agents-test--with-clean-state
    (claude-term-agents-test--with-fake-session "/repo/wt/" nil 'working
      (claude-term-agents--update-progress-title 'set 42)
      (should (equal (format "%s 42%%" claude-term-registry--default-instance-label)
                      (edmacs-agent-title
                       (gethash (edmacs-agents--key
                                 "/repo/wt/" claude-term-registry--default-instance-label)
                                edmacs-agents--table)))))))

(ert-deftest claude-term-agents-test-progress-repeated-set-replaces-not-accumulates ()
  (claude-term-agents-test--with-clean-state
    (claude-term-agents-test--with-fake-session "/repo/wt/" "i1" 'working
      (claude-term-agents--update-progress-title 'set 10)
      (claude-term-agents--update-progress-title 'set 90)
      (should (equal "i1 90%"
                      (edmacs-agent-title (gethash (edmacs-agents--key "/repo/wt/" "i1")
                                                    edmacs-agents--table)))))))

(ert-deftest claude-term-agents-test-progress-non-working-row-strips-suffix ()
  "A `set' report against a row that is not `working' (e.g. it finished
mid-flight) strips any stale suffix instead of rendering a new one."
  (claude-term-agents-test--with-clean-state
    (claude-term-agents-test--with-fake-session "/repo/wt/" "i1" 'working
      (claude-term-agents--update-progress-title 'set 42)
      (setf (edmacs-agent-status (gethash (edmacs-agents--key "/repo/wt/" "i1")
                                           edmacs-agents--table))
            'done)
      (claude-term-agents--update-progress-title 'set 55)
      (should (equal "i1"
                      (edmacs-agent-title (gethash (edmacs-agents--key "/repo/wt/" "i1")
                                                    edmacs-agents--table)))))))

(ert-deftest claude-term-agents-test-progress-remove-state-strips-suffix ()
  (claude-term-agents-test--with-clean-state
    (claude-term-agents-test--with-fake-session "/repo/wt/" "i1" 'working
      (claude-term-agents--update-progress-title 'set 42)
      (claude-term-agents--update-progress-title 'remove nil)
      (should (equal "i1"
                      (edmacs-agent-title (gethash (edmacs-agents--key "/repo/wt/" "i1")
                                                    edmacs-agents--table)))))))

(ert-deftest claude-term-agents-test-progress-non-claude-term-buffer-is-a-no-op ()
  "A plain ghostel buffer with no `claude-term--root' set (any other
ghostel session) is silently ignored -- no error, no table mutation."
  (claude-term-agents-test--with-clean-state
    (with-temp-buffer
      (should-not (claude-term-agents--update-progress-title 'set 42))
      (should (= 0 (hash-table-count edmacs-agents--table))))))

(ert-deftest claude-term-agents-test-progress-no-row-yet-is-a-no-op ()
  "A progress report racing ahead of the registry `put' (row not yet
created) is silently ignored."
  (claude-term-agents-test--with-clean-state
    (let ((buf (generate-new-buffer "claude-term-agents-test-early-buf")))
      (unwind-protect
          (with-current-buffer buf
            (setq-local claude-term--root "/repo/wt/")
            (setq-local claude-term--instance "i1")
            (should-not (claude-term-agents--update-progress-title 'set 42)))
        (kill-buffer buf)))))

(ert-deftest claude-term-agents-test-progress-handler-chains-to-previous ()
  "The installed handler calls the previously-chained function first,
then still updates the row."
  (claude-term-agents-test--with-clean-state
    (claude-term-agents-test--with-fake-session "/repo/wt/" "i1" 'working
      (let* ((calls nil)
             (claude-term-agents--chained-progress-function
              (lambda (state progress) (push (cons state progress) calls))))
        (claude-term-agents--progress-handler 'set 42)
        (should (equal calls '((set . 42))))
        (should (equal "i1 42%"
                        (edmacs-agent-title (gethash (edmacs-agents--key "/repo/wt/" "i1")
                                                      edmacs-agents--table))))))))

(ert-deftest claude-term-agents-test-progress-handler-chained-error-does-not-block-update ()
  "An error in the previously-chained handler is swallowed, not
propagated -- the row update still happens."
  (claude-term-agents-test--with-clean-state
    (claude-term-agents-test--with-fake-session "/repo/wt/" "i1" 'working
      (let ((claude-term-agents--chained-progress-function (lambda (&rest _) (error "boom"))))
        (claude-term-agents--progress-handler 'set 42)
        (should (equal "i1 42%"
                        (edmacs-agent-title (gethash (edmacs-agents--key "/repo/wt/" "i1")
                                                      edmacs-agents--table))))))))

(ert-deftest claude-term-agents-test-progress-handler-no-chained-function-is-fine ()
  "A nil `claude-term-agents--chained-progress-function' (no prior
handler was installed, or `ghostel-progress-function' was nil) is a
no-op for the chained half, not an error."
  (claude-term-agents-test--with-clean-state
    (claude-term-agents-test--with-fake-session "/repo/wt/" "i1" 'working
      (let ((claude-term-agents--chained-progress-function nil))
        (claude-term-agents--progress-handler 'set 42)
        (should (equal "i1 42%"
                        (edmacs-agent-title (gethash (edmacs-agents--key "/repo/wt/" "i1")
                                                      edmacs-agents--table))))))))

(ert-deftest claude-term-agents-test-install-progress-handler-is-idempotent ()
  "Installing the wrapper when it is already the current
`ghostel-progress-function' does not re-chain onto itself."
  (let* ((sentinel (lambda (&rest _) nil))
         (ghostel-progress-function sentinel)
         (claude-term-agents--chained-progress-function nil))
    (claude-term-agents--install-progress-handler)
    (should (eq ghostel-progress-function #'claude-term-agents--progress-handler))
    (should (eq claude-term-agents--chained-progress-function sentinel))
    ;; A second install call must not chain the wrapper onto itself.
    (claude-term-agents--install-progress-handler)
    (should (eq claude-term-agents--chained-progress-function sentinel))))

(provide 'claude-term-agents-test)
;;; claude-term-agents-test.el ends here
