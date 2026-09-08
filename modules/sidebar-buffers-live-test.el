;;; sidebar-buffers-live-test.el --- Live tests for sidebar-buffers.el -*- lexical-binding: t -*-

;;; Commentary:
;; Real frames, real tabs, real `bufferlo' (loaded straight from the
;; straight build tree), and real file buffers under a fresh temp
;; directory per test -- the genuinely environment-dependent half of
;; this phase's acceptance criteria that sidebar-buffers-test.el's pure
;; suite cannot exercise: `window-prev-buffers' ordering off a live
;; window, `bufferlo-buffer-list' per-tab/per-frame isolation, RET/[/]/d/s
;; driving real `tab-bar'/`windows.el' state, and the debounce timer.
;;
;; sidebar.el, windows.el, and workspaces.el are ALL loaded for real
;; (unlike the pure suite): every enumeration/lookup function sidebar.el's
;; worktree redraw path calls (`edmacs-workspaces-groups'/`-tabs-in-group'/
;; `-tab-root'/`-current-group'/`-find-tab'/`-select-tab') runs unstubbed
;; against real tab-bar state. Only `edmacs-git-common-dir',
;; `edmacs-workspaces-group-name', `edmacs-workspaces-classify-root' and
;; `edmacs-workspaces-open-worktree' are overridden -- via `cl-letf'
;; scoped to `edmacs-sidebar-buffers-live-test--with-scenario's body,
;; applied AFTER the real `load' below rather than as top-level `defun's
;; the load would otherwise clobber (the exact silent-clobber bug this
;; file once had). See the block comment just above that `load' for what
;; each override does and why the real, git-common-dir-based
;; implementation cannot resolve any of this file's fixture roots (fresh
;; temp directories, never real git worktrees) at all.
;;
;; agents.el and sidebar-agents.el are ALSO loaded for real here, purely
;; so one test (`edmacs-sidebar-live-test-composed-worktree-render-
;; never-shell-out') can register a real agent alongside a real buffer
;; and guard the composed render -- a worktree row with both the
;; `agents' group and the buffer tree nested under it -- against any
;; subprocess primitive. Neither sidebar-agents-test.el nor
;; sidebar-buffers-test.el composes the other module in, and
;; sidebar-test.el's own no-shellout guard loads sidebar.el alone, so
;; this composed shape was otherwise never exercised under any guard.
;;
;; A second real frame needs a controlling terminal -- absent under
;; plain `-Q --batch', present under `scripts/pty-ert.sh emacs -Q
;; --batch ...' -- so the two tests needing one (`per-frame-isolation',
;; `toggle-is-frame-local') skip cleanly under the plain invocation,
;; following sidebar-test.el's own documented convention; that plain
;; invocation is this file's primary, CI-equivalent check.
;; `scripts/pty-ert.sh' is what actually exercises them.
;;
;; Root-caused (edmacs-sidebar-polish phase 14, after this Commentary's
;; earlier "bufferlo pty sharing" / "`#<dead frame>' lifecycle" guesses
;; both turned out wrong -- f2 was always alive and its parameters were
;; always correctly isolated): `--make-second-frame-or-skip's `(make-frame
;; ...)' call SELECTS the frame it creates -- confirmed by wrapping the
;; real `make-frame' with an `:around' advice during an actual
;; `script'-driven run of this file and printing `(eq new-frame
;; (selected-frame))' immediately after each of these two tests' own
;; call, which read `t' both times. Neither test's body used to
;; re-select the original frame afterward, so for the rest of each test
;; `f2' and `(selected-frame)' named the SAME frame object, not two
;; distinct ones: `toggle-is-frame-local' toggled `(selected-frame)'
;; (=f2) and then read that exact toggle back off `f2', so the
;; `should-not' failed by construction; `per-frame-isolation' did all of
;; its "frame 1" setup and its final `(edmacs-sidebar-show
;; (selected-frame))' against that same aliased frame, so `text1' and
;; `text2' inevitably rendered the identical single frame. This was a
;; property of `make-frame' on a new tty in `--batch' generally, not of
;; `script' sharing one pty with the caller specifically -- confirmed
;; independently with two frames on two separately-allocated ptys (no
;; shared session at all): each `make-frame' call still selected the
;; frame it had just created, while every
;; `frame-parameter'/`set-frame-parameter' call this module's real
;; `edmacs-sidebar-buffers-toggle-flat'/`edmacs-sidebar-show' made across
;; those two genuinely independent frames stayed perfectly isolated
;; (flat toggled on one left the other's parameter nil; each frame's
;; rendered buffer list showed only its own file). So: a test-harness
;; artifact of this second-frame technique under `--batch', not a defect
;; in this module's frame scoping.
;;
;; Both tests now capture the original frame BEFORE calling
;; `--make-second-frame-or-skip' and thread that explicit variable
;; through every subsequent read/write instead of relying on
;; `(selected-frame)', so they genuinely exercise two distinct frames
;; and pass under a real pty -- no assertion is excused here
;; any more. sidebar-buffers-test.el's
;; `toggle-flat-scopes-every-call-to-selected-frame' and
;; `on-worktree-section-scopes-frame-parameter-to-explicit-frame' remain
;; as a second, environment-independent line of coverage for the same
;; invariant, simulating two frames without needing a real second tty
;; frame at all.
;;
;; Neither originates in `edmacs-sidebar-hide' -- the "Attempt to delete
;; minibuffer or sole ordinary window" signal these tests once worked
;; around is fixed at the source (`edmacs-sidebar--release-window'),
;; which is why the `ignore-errors' wrapper around the hide call in
;; `--reset-frame' is gone.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/git-common-dir.el \
;;         -l modules/sidebar-buffers-live-test.el -f edmacs-sidebar-buffers-live-test-run-and-exit
;;
;; To also exercise the second-frame tests:
;;   scripts/pty-ert.sh emacs -Q --batch -l ert -l modules/test-support.el \
;;         -l modules/git-common-dir.el -l modules/sidebar-buffers-live-test.el \
;;         -f edmacs-sidebar-buffers-live-test-run-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'dired)
(require 'thingatpt)

(setq native-comp-enable-subr-trampolines nil)

;; ==========================================================================
;; Loading bufferlo + windows.el + sidebar.el + sidebar-buffers.el for real
;; ==========================================================================

(defvar edmacs-sidebar-buffers-live-test--build-root
  (edmacs-test-support-straight-build-root))

(defconst edmacs-sidebar-buffers-live-test--self-file
  (or load-file-name buffer-file-name))

(if (or (null edmacs-sidebar-buffers-live-test--build-root)
        (not (file-directory-p (expand-file-name
                                 "bufferlo" edmacs-sidebar-buffers-live-test--build-root))))

    (ert-deftest edmacs-sidebar-buffers-live-test-deps-unavailable ()
      (edmacs-test-support-report-suite-unavailable
       edmacs-sidebar-buffers-live-test--self-file
       "magit-section's or bufferlo's straight build was not found in \
this checkout or its sibling main checkout; bootstrap straight once (open this \
worktree in a real Emacs session) to enable this suite"))

  (progn

    (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section" "bufferlo"))
      (let ((dir (expand-file-name dep edmacs-sidebar-buffers-live-test--build-root)))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))

    (require 'bufferlo)
    (bufferlo-mode 1)
    (require 'magit-section)

    ;; workspaces.el IS loaded for real below, alongside windows.el and
    ;; sidebar.el -- its pure enumeration/lookup functions
    ;; (`edmacs-workspaces-groups'/`-tabs-in-group'/`-tab-root'/
    ;; `-current-group'/`-find-tab'/`-select-tab') run unstubbed against
    ;; real tab-bar state. Three of its functions are overridden, via
    ;; `cl-letf' scoped to `edmacs-sidebar-buffers-live-test--with-scenario's
    ;; body (the sessions.el override-after-real-load convention -- a
    ;; top-level `defun' here would just be clobbered by the real `load'
    ;; below, which is the exact bug this rewrite fixes):
    ;;
    ;; - `edmacs-git-common-dir' -- none of this file's fixture roots (fresh
    ;;   temp directories) name a real git worktree, so returning nil
    ;;   unconditionally keeps `edmacs-sidebar--derive-main-root' from ever
    ;;   attempting a real (and here, pointlessly failing) `git' subprocess
    ;;   -- critical for this file's own composed no-shellout guard test.
    ;; - `edmacs-workspaces-group-name' -- resolved by scanning tabs for
    ;;   one whose stamped root equals ROOT, returning its real
    ;;   `tab-bar-tab-group-function' group, since the real implementation
    ;;   (git-common-dir-based) can never resolve a fixture root at all.
    ;;   `edmacs-sidebar-buffers--tab-number-for-root' (sidebar-buffers.el)
    ;;   needs this to resolve RET/`[`/`]` against a real open tab.
    ;; - `edmacs-workspaces-classify-root' -- ROOT classifies `main' when it
    ;;   is the first root ever stamped for its group, else nil; the real
    ;;   implementation is also git-common-dir-based and could never
    ;;   classify a fixture root `main' at all, which would leave a
    ;;   single-tab project's own project row unable to resolve its own
    ;;   open tab (`edmacs-sidebar--section-tab-number' needs a `main'-
    ;;   classified tab's root on that row, not nil) -- exactly what
    ;;   `edmacs-sidebar-rename-at-point' exercises on the composed-render
    ;;   guard test below.
    ;; - `edmacs-workspaces-open-worktree' -- a no-op; this file never
    ;;   exercises cross-project worktree opening.
    ;;
    ;; The registered-worktrees table is inert -- kept only so this file's
    ;; many existing `--register-worktrees' call sites keep working; no
    ;; production code reads it any more.
    (defvar edmacs-sidebar-buffers-live-test--registered-worktrees
      (make-hash-table :test #'equal))
    ;; The project group a scenario's tabs are filed under. Was the frames
    ;; model's per-frame repo parameter; a suite-local dynamic value now,
    ;; there being no per-frame repo to read.
    (defvar edmacs-sidebar-buffers-live-test--group nil)
    (defconst edmacs-sidebar-buffers-live-test--root-parameter 'edmacs-workspace-root)
    ;; GROUP -> the first ROOT `--stamp-current-tab-root' ever stamped for
    ;; it, treated as that group's `main' worktree by the classify-root
    ;; override below -- the real, git-common-dir-based classification can
    ;; never resolve a fixture root (a fresh temp directory, never a real
    ;; git worktree) at all.
    (defvar edmacs-sidebar-buffers-live-test--group-main-roots (make-hash-table :test #'equal))

    (defun edmacs-sidebar-buffers-live-test--group-name-override (root)
      "Return ROOT's already-assigned tab-bar group, found by scanning
every live frame's tabs for one whose stamped root equals ROOT -- see
this file's Commentary above on why the real, git-common-dir-based
`edmacs-workspaces-group-name' cannot resolve a fixture root at all."
      (catch 'edmacs-sidebar-buffers-live-test--group-found
        (dolist (frame (frame-list))
          (dolist (tab (tab-bar-tabs frame))
            (when (equal (alist-get edmacs-sidebar-buffers-live-test--root-parameter tab) root)
              (throw 'edmacs-sidebar-buffers-live-test--group-found
                     (funcall tab-bar-tab-group-function tab)))))
        nil))

    (defun edmacs-sidebar-buffers-live-test--classify-root-override (root)
      "See `edmacs-sidebar-buffers-live-test--group-main-roots'."
      (catch 'edmacs-sidebar-buffers-live-test--classify-found
        (maphash (lambda (_group main)
                   (when (equal main root)
                     (throw 'edmacs-sidebar-buffers-live-test--classify-found 'main)))
                 edmacs-sidebar-buffers-live-test--group-main-roots)
        nil))

    (defmacro edmacs-sidebar-buffers-live-test--with-workspaces-overrides (&rest body)
      "Run BODY with the three workspaces.el overrides described in this
file's Commentary in effect."
      (declare (indent 0))
      `(cl-letf (((symbol-function 'edmacs-git-common-dir) (lambda (_root) nil))
                 ((symbol-function 'edmacs-workspaces-group-name)
                  #'edmacs-sidebar-buffers-live-test--group-name-override)
                 ((symbol-function 'edmacs-workspaces-classify-root)
                  #'edmacs-sidebar-buffers-live-test--classify-root-override)
                 ((symbol-function 'edmacs-workspaces-open-worktree) (lambda (_dir) nil)))
         ,@body))

    (load (expand-file-name "modules/windows.el" default-directory) nil t)
    ;; init.el loads workspaces.el before sidebar.el; sidebar.el's
    ;; `edmacs-sidebar-redraw-frames' and `edmacs-sidebar-show' both call
    ;; `edmacs-workspaces-frame-usable-p', so the same order is required here.
    (load (expand-file-name "modules/workspaces.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar-buffers.el" default-directory) nil t)
    ;; Also loaded here (not just in sidebar-agents-live-test.el) so this
    ;; file can compose a real worktree render with BOTH sidebar-buffers.el's
    ;; buffer tree and sidebar-agents.el's agent group nested under the same
    ;; row -- see the composed no-shellout guard test below.
    (load (expand-file-name "modules/agents.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar-agents.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Shared helpers
    ;; ==========================================================================

    (defun edmacs-sidebar-buffers-live-test--make-root ()
      "Return the truename of a fresh, empty temp directory."
      (file-truename (make-temp-file "edmacs-sb-live-test-" t)))

    (defun edmacs-sidebar-buffers-live-test--write-file (root relpath &optional content)
      "Create ROOT/RELPATH (and its parent directories), return the full path."
      (let ((full (expand-file-name relpath root)))
        (make-directory (file-name-directory full) t)
        (with-temp-file full (insert (or content relpath)))
        full))

    (defun edmacs-sidebar-buffers-live-test--register-worktrees (common worktrees)
      "WORKTREES is an alist of (NAME . ROOT), keyed by a repo's common dir."
      (puthash common worktrees edmacs-sidebar-buffers-live-test--registered-worktrees))

    (defun edmacs-sidebar-buffers-live-test--stamp-current-tab-root (root)
      "Stamp ROOT as the selected frame's current tab's workspace root, and
mark the selected window as that tab's main window. The tab's GROUP
comes from `edmacs-sidebar-buffers-live-test--group', which every call
site sets immediately before calling this: a shared value across several
`--stamp-current-tab-root' calls in one test puts every root under the
SAME project row, which is what most scenarios here want. Also renames the tab to
ROOT's own leaf directory name, matching what a real
`edmacs-workspaces--open-tab' always does -- without this, a bare
`tab-bar-new-tab' inherits the CURRENT BUFFER's name (tab-bar.el's own
auto-naming), which can coincidentally match a filename this module
renders lower down and confuse a test's own text-matching assertions."
      (let ((group (or edmacs-sidebar-buffers-live-test--group
                        "edmacs-sidebar-buffers-live-test-group")))
        (setf (alist-get edmacs-sidebar-buffers-live-test--root-parameter
                          (cdr (tab-bar--current-tab-find)))
              root)
        (tab-bar-rename-tab (file-name-nondirectory (directory-file-name root)))
        (tab-bar-change-tab-group group)
        (unless (gethash group edmacs-sidebar-buffers-live-test--group-main-roots)
          (puthash group root edmacs-sidebar-buffers-live-test--group-main-roots)))
      (edmacs-window-set-main (selected-window)))

    (defun edmacs-sidebar-buffers-live-test--close-extra-tabs (n)
      "Close tabs beyond the first N in the selected frame."
      (while (> (length (tab-bar-tabs)) n)
        (tab-bar-close-tab (length (tab-bar-tabs)))))

    (defun edmacs-sidebar-buffers-live-test--kill-buffers-under (root)
      "Kill every live buffer whose file or `default-directory' is under ROOT."
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (let ((path (or (buffer-local-value 'buffer-file-name buf)
                           (buffer-local-value 'default-directory buf))))
            (when (and path (string-prefix-p root (file-truename path)))
              (kill-buffer buf))))))

    (defun edmacs-sidebar-buffers-live-test--reset-frame (frame)
      "Undo every frame-level trace this suite's scenarios leave behind."
      (edmacs-sidebar-hide frame)
      (let ((buf (edmacs-sidebar--buffer frame)))
        (when (buffer-live-p buf) (kill-buffer buf))
        (set-frame-parameter frame 'edmacs-sidebar-buffer nil))
      (setq edmacs-sidebar-buffers-live-test--group nil)
      (set-frame-parameter frame 'edmacs-sidebar-buffers-flat nil)
      (clrhash edmacs-sidebar-buffers-live-test--registered-worktrees)
      ;; Group strings like "/repo/.git" are reused verbatim across many
      ;; tests' own scenarios; without this a later test's root could be
      ;; wrongly classified `main' by a stale entry an earlier test left
      ;; behind under the same group string.
      (clrhash edmacs-sidebar-buffers-live-test--group-main-roots))

    (defmacro edmacs-sidebar-buffers-live-test--with-scenario (roots &rest body)
      "Run BODY with a clean single-tab frame, then unwind: close any
extra tabs, kill every buffer under any of ROOTS (a list of root
directories), delete those directories, and reset frame-level state.
BODY runs with this file's `--with-workspaces-overrides' in effect."
      (declare (indent 1))
      `(let ((edmacs-sidebar-buffers-live-test--roots ,roots))
         (unwind-protect
             (edmacs-sidebar-buffers-live-test--with-workspaces-overrides ,@body)
           (edmacs-sidebar-buffers-live-test--close-extra-tabs 1)
           (dolist (root edmacs-sidebar-buffers-live-test--roots)
             (edmacs-sidebar-buffers-live-test--kill-buffers-under root)
             (ignore-errors (delete-directory root t)))
           (edmacs-sidebar-buffers-live-test--reset-frame (selected-frame))
           (unless (get-buffer "*scratch*")
             (get-scratch-buffer-create))
           (switch-to-buffer (get-buffer-create "*scratch*")))))

    (defconst edmacs-sidebar-buffers-live-test--tab-row-regexp "^[●○⋯] "
      "sidebar.el's own tab-row marker prefix (see `edmacs-sidebar--tab-label').
A tab row's label tracks whatever buffer that tab last showed, so its
text can coincidentally collide with a file name this module renders
lower down -- every row THIS module inserts is indented instead (never
starts with one of these markers), so filtering lines matching this
out keeps assertions from false-matching a tab row.")

    (defun edmacs-sidebar-buffers-live-test--sidebar-text (frame)
      "Return FRAME's sidebar buffer's text, with every tab-row line removed."
      (with-current-buffer (edmacs-sidebar--buffer frame)
        (mapconcat #'identity
                   (seq-remove (lambda (line)
                                 (string-match-p edmacs-sidebar-buffers-live-test--tab-row-regexp line))
                               (split-string (buffer-string) "\n"))
                   "\n")))

    (defun edmacs-sidebar-buffers-live-test--goto-text (frame text)
      "Move point in FRAME's sidebar buffer to the first occurrence of TEXT
on a row this module itself rendered (never a sidebar.el tab row)."
      (with-current-buffer (edmacs-sidebar--buffer frame)
        (goto-char (point-min))
        (let (found)
          (while (and (not found) (re-search-forward (regexp-quote text) nil t))
            (if (save-excursion
                  (goto-char (line-beginning-position))
                  (looking-at-p edmacs-sidebar-buffers-live-test--tab-row-regexp))
                (goto-char (line-end-position))
              (setq found t)))
          (unless found (error "edmacs-sidebar-buffers-live-test--goto-text: %S not found" text)))))

    (defun edmacs-sidebar-buffers-live-test--visible-rows (frame)
      "Return FRAME's sidebar buffer's text, tab-row lines removed and every
`invisible' span elided -- what a human would actually see rendered,
restricted to rows this module itself is responsible for."
      (with-current-buffer (edmacs-sidebar--buffer frame)
        (let ((pos (point-min)) (out nil))
          (while (< pos (point-max))
            (if (get-char-property pos 'invisible)
                (setq pos (or (next-single-char-property-change pos 'invisible)
                              (point-max)))
              (push (buffer-substring pos (1+ pos)) out)
              (setq pos (1+ pos))))
          (mapconcat #'identity
                     (seq-remove (lambda (line)
                                   (string-match-p edmacs-sidebar-buffers-live-test--tab-row-regexp line))
                                 (split-string (apply #'concat (nreverse out)) "\n"))
                     "\n"))))

    (defun edmacs-sidebar-buffers-live-test--find-buffers-root-section (root)
      "Return the `edmacs-sidebar-buffers-root' section for ROOT, in the
current buffer, or nil. sidebar.el renders EVERY worktree's row (and its
buffers subsection) on every redraw regardless of which tab is current
\(folded, never omitted) -- so isolation between two worktrees' own
subsections must be checked by SCOPING to each one's own section span,
never by a whole-buffer text search, which would always see both."
      (catch 'found
        (edmacs-sidebar--map-sections
         magit-root-section
         (lambda (section)
           (when (and (eq (oref section type) 'edmacs-sidebar-buffers-root)
                      (slot-boundp section 'value)
                      (equal (oref section value) root))
             (throw 'found section))))
        nil))

    (defun edmacs-sidebar-buffers-live-test--subsection-text (frame root)
      "Return the raw text (including any currently-folded/invisible spans)
of ROOT's own buffers subsection in FRAME's sidebar buffer."
      (with-current-buffer (edmacs-sidebar--buffer frame)
        (let ((section (edmacs-sidebar-buffers-live-test--find-buffers-root-section root)))
          (should section)
          (buffer-substring (oref section start) (oref section end)))))

    ;; ==========================================================================
    ;; AC1 -- directory-tree grouping/ordering
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-live-test-ac1-shape ()
      "sessions.el, ui.el, init.el opened in that order, then switch to a
compilation buffer -- the buffers subsection groups modules/ (ui.el then
sessions.el, most-recently-visited first), then init.el, then the dimmed
special buffer trailing, exactly AC1's own worked example.

AC1's original wording used `*Messages*' as the trailing special row.
`edmacs-sidebar-buffers--listable-p' is an allowlist that deliberately
excludes it (chrome, not work -- see its docstring), so a compilation
buffer stands in: not file-like, so it still takes the dimmed special
row, but listable via `edmacs-sidebar-buffers-interactive-modes'."
      (let ((root (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (let ((sessions (edmacs-sidebar-buffers-live-test--write-file root "modules/sessions.el"))
                (ui (edmacs-sidebar-buffers-live-test--write-file root "modules/ui.el"))
                (init (edmacs-sidebar-buffers-live-test--write-file root "init.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "repo" root)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
            (find-file sessions)
            (find-file ui)
            (find-file init)
            ;; Not under ROOT, so `--with-scenario's unwind does not reach it.
            (unwind-protect
                (progn
                  (with-current-buffer (get-buffer-create "*compilation*")
                    (compilation-mode))
                  (switch-to-buffer (get-buffer "*compilation*"))
                  (edmacs-sidebar-show (selected-frame))
                  (let ((text (edmacs-sidebar-buffers-live-test--sidebar-text (selected-frame))))
                    (should (string-match-p
                             (rx "modules/" (* anychar) "ui.el" (* anychar) "sessions.el"
                                 (* anychar) "init.el" (* anychar) "*compilation*")
                             text))
                    ;; The compilation row is dimmed; ui.el/sessions.el/init.el are not.
                    (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                      (should (text-property-any
                               (point-min) (point-max) 'face 'edmacs-sidebar-buffers-special-face))
                      (goto-char (point-min))
                      (search-forward "modules/")
                      (search-forward "init.el")
                      (should-not (get-text-property (1- (point)) 'face)))))
              (when (get-buffer "*compilation*")
                (kill-buffer "*compilation*")))))))

    (ert-deftest edmacs-sidebar-buffers-live-test-ac1-chain-flatten-and-fold ()
      "A directory chain ending in a lone file collapses to one row
\(the 'claude-repl/ claude-repl-buffer.el' example); a real branch three
levels deep is folded by default (its children invisible) while the
first two rendered levels stay expanded."
      (let ((root (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (let ((chain (edmacs-sidebar-buffers-live-test--write-file
                        root "modules/claude-repl/claude-repl-buffer.el"))
                (a (edmacs-sidebar-buffers-live-test--write-file root "p/fileA.el"))
                (b (edmacs-sidebar-buffers-live-test--write-file root "p/q/fileB.el"))
                (c (edmacs-sidebar-buffers-live-test--write-file root "p/q/r/fileC.el"))
                (d (edmacs-sidebar-buffers-live-test--write-file root "p/q/r/fileD.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "repo" root)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
            (find-file chain) (find-file a) (find-file b) (find-file c) (find-file d)
            (edmacs-sidebar-show (selected-frame))
            (let ((text (edmacs-sidebar-buffers-live-test--sidebar-text (selected-frame))))
              (should (string-match-p "claude-repl/ claude-repl-buffer.el" text))
              (should (string-match-p "r/" text))
              (should (string-match-p "fileC.el" text))) ; present, just invisible
            (let ((visible (edmacs-sidebar-buffers-live-test--visible-rows (selected-frame))))
              (should-not (string-match-p "fileC.el" visible))
              (should-not (string-match-p "fileD.el" visible))
              (should (string-match-p "fileB.el" visible))
              (should (string-match-p "fileA.el" visible)))))))

    (ert-deftest edmacs-sidebar-buffers-live-test-ac1-non-current-tab-folded ()
      "Only the frame's currently-selected tab's buffers subsection starts
expanded; a background tab's own subsection exists (content present)
but is entirely invisible until expanded."
      (let ((r1 (edmacs-sidebar-buffers-live-test--make-root))
            (r2 (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list r1 r2)
          (let ((x (edmacs-sidebar-buffers-live-test--write-file r1 "x.el"))
                (z (edmacs-sidebar-buffers-live-test--write-file r2 "z.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "r1" r1) (cons "r2" r2)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r1)
            (find-file x)
            (let ((tab-bar-new-tab-choice "*scratch*"))
              (tab-bar-new-tab))
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r2)
            (find-file z)
            (edmacs-sidebar-show (selected-frame))
            (let ((text (edmacs-sidebar-buffers-live-test--sidebar-text (selected-frame)))
                  (visible (edmacs-sidebar-buffers-live-test--visible-rows (selected-frame))))
              (should (string-match-p "x.el" text))
              (should-not (string-match-p "x.el" visible))
              (should (string-match-p "z.el" visible)))))))

    (ert-deftest edmacs-sidebar-buffers-live-test-ac1-non-current-tab-recency-order ()
      "A background tab's own ordering comes from its serialized `ws'
tree's `prev-buffers', not alphabetical fallback: opening d/a.el, then
d/z.el, then d/m.el (leaving m.el as the tab's own selected buffer)
before switching away must render z.el ahead of a.el ahead of m.el --
alphabetical order (a, m, z) would mean `--main-window-prev-names' fed
`--ws-main-prev-buffers' the wrong (unwrapped) shape and silently lost
every rank, exactly the regression this test guards against."
      (let ((r1 (edmacs-sidebar-buffers-live-test--make-root))
            (r2 (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list r1 r2)
          (let ((w (edmacs-sidebar-buffers-live-test--write-file r1 "w.el"))
                (a (edmacs-sidebar-buffers-live-test--write-file r2 "d/a.el"))
                (m (edmacs-sidebar-buffers-live-test--write-file r2 "d/m.el"))
                (z (edmacs-sidebar-buffers-live-test--write-file r2 "d/z.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "r1" r1) (cons "r2" r2)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r1)
            (find-file w)
            (let ((tab-bar-new-tab-choice "*scratch*"))
              (tab-bar-new-tab))
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r2)
            (find-file a) (find-file z) (find-file m)
            (tab-bar-select-tab 1)
            (edmacs-sidebar-show (selected-frame))
            (let ((text (edmacs-sidebar-buffers-live-test--subsection-text (selected-frame) r2)))
              (should (string-match-p
                       (rx "z.el" (* anychar) "a.el" (* anychar) "m.el")
                       text)))))))

    ;; ==========================================================================
    ;; AC2 -- RET visit, [ / ] parity with previous-buffer/next-buffer, markers
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-live-test-ret-shows-buffer-in-main-window ()
      (let ((root (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (let ((a (edmacs-sidebar-buffers-live-test--write-file root "a.el"))
                (b (edmacs-sidebar-buffers-live-test--write-file root "b.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "repo" root)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
            (find-file a)
            (find-file b)
            (edmacs-sidebar-show (selected-frame))
            (edmacs-sidebar-buffers-live-test--goto-text (selected-frame) "a.el")
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (edmacs-sidebar-buffers-visit))
            (should (equal "a.el" (buffer-name (window-buffer (edmacs-main-window)))))))))

    (ert-deftest edmacs-sidebar-buffers-live-test-ret-on-other-tab-switches-tab ()
      "RET on a row belonging to a DIFFERENT (non-current) tab selects
that tab first, then shows the buffer in its main window."
      (let ((r1 (edmacs-sidebar-buffers-live-test--make-root))
            (r2 (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list r1 r2)
          (let ((y (edmacs-sidebar-buffers-live-test--write-file r1 "y.el"))
                (z (edmacs-sidebar-buffers-live-test--write-file r2 "z.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "r1" r1) (cons "r2" r2)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r1)
            (find-file y)
            (let ((tab-bar-new-tab-choice "*scratch*"))
              (tab-bar-new-tab))
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r2)
            (find-file z)
            ;; Back to tab 1 (r1) so r2's row is now the non-current one.
            (tab-bar-select-tab 1)
            (edmacs-sidebar-show (selected-frame))
            (edmacs-sidebar-buffers-live-test--goto-text (selected-frame) "z.el")
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (edmacs-sidebar-buffers-visit))
            (should (= 1 (tab-bar--current-tab-index))) ; 0-based: tab 2
            (should (equal "z.el" (buffer-name (window-buffer (edmacs-main-window)))))))))

    (ert-deftest edmacs-sidebar-buffers-live-test-bracket-keys-match-next-previous-buffer ()
      "`[' walks the exact deterministic sequence `previous-buffer' itself
would from a known open order (a.el, b.el, c.el -- so `previous-buffer'
from c.el goes to b.el, then to a.el), with point following to the
resulting row each time; `]' then walks the same sequence back via
`next-buffer'. Computed from the known open order directly, rather than
probing with a live `previous-buffer'/`next-buffer' round-trip first --
that would itself mutate the very `window-prev-buffers'/`-next-buffers'
state under test."
      (let ((root (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (let ((a (edmacs-sidebar-buffers-live-test--write-file root "a.el"))
                (b (edmacs-sidebar-buffers-live-test--write-file root "b.el"))
                (c (edmacs-sidebar-buffers-live-test--write-file root "c.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "repo" root)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
            (find-file a) (find-file b) (find-file c)
            (edmacs-sidebar-show (selected-frame))
            (edmacs-sidebar-buffers-live-test--goto-text (selected-frame) "c.el")
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (edmacs-sidebar-buffers-prev (selected-frame)))
            (should (equal "b.el" (buffer-name (window-buffer (edmacs-main-window)))))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (should (string-match-p "b\\.el" (thing-at-point 'line t))))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (edmacs-sidebar-buffers-prev (selected-frame)))
            (should (equal "a.el" (buffer-name (window-buffer (edmacs-main-window)))))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (should (string-match-p "a\\.el" (thing-at-point 'line t))))
            ;; `]' now walks forward again, back through b.el to c.el.
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (edmacs-sidebar-buffers-next (selected-frame)))
            (should (equal "b.el" (buffer-name (window-buffer (edmacs-main-window)))))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (edmacs-sidebar-buffers-next (selected-frame)))
            (should (equal "c.el" (buffer-name (window-buffer (edmacs-main-window)))))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (should (string-match-p "c\\.el" (thing-at-point 'line t))))))))

    (ert-deftest edmacs-sidebar-buffers-live-test-markers-modified-and-selected ()
      (let ((root (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (let ((a (edmacs-sidebar-buffers-live-test--write-file root "a.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "repo" root)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
            (find-file a)
            (with-current-buffer (get-file-buffer a) (set-buffer-modified-p t))
            (edmacs-sidebar-show (selected-frame))
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (should (hash-table-p edmacs-sidebar-buffers--row-overlays))
              (let ((ov (gethash (get-file-buffer a) edmacs-sidebar-buffers--row-overlays)))
                (should ov)
                (should (equal "*" (overlay-get ov 'after-string)))
                (should (equal "● " (overlay-get ov 'before-string)))
                (should (eq 'edmacs-sidebar-buffers-selected-face (overlay-get ov 'face)))))))))

    ;; ==========================================================================
    ;; `3bac609' guard -- selecting the sidebar window itself must not refresh
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-live-test-on-window-change-guards-sidebar-focus ()
      "`edmacs-sidebar-buffers--on-window-change' (added in `3bac609' so an
evil motion into the sidebar would not re-run the marker refresh, which
read as the cursor jumping) is a real two-window, single-frame
behavior -- unlike this file's `--frame-local'/`--isolation' tests, it
needs no second tty frame at all: `edmacs-sidebar-show' already puts a
second, real window of THIS frame on screen, and selecting it is
exactly what an evil window-motion into the sidebar does. Drives the
hook the same way redisplay does (`run-hook-with-args', one FRAME
argument -- see `window-selection-change-functions's own doc) rather
than waiting on redisplay's own timing, so the assertion is
deterministic under both plain `--batch' and a real graphical frame
\(`scripts/gui-ert.sh'), and confirms the two-sided contract that AC4's
re-examination of this guard actually depends on: no-op while the
sidebar itself is selected, refresh again the moment focus leaves."
      (let ((root (edmacs-sidebar-buffers-live-test--make-root))
            (calls 0))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (let ((a (edmacs-sidebar-buffers-live-test--write-file root "a.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "repo" root)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
            (find-file a)
            (let ((main (selected-window)))
              (edmacs-sidebar-show (selected-frame))
              (should (eq (selected-window) main))
              (let ((sidebar-window (edmacs-sidebar--window (selected-frame))))
                (should (window-live-p sidebar-window))
                (should-not (eq sidebar-window main))
                (advice-add 'edmacs-sidebar-buffers--refresh-markers :before
                            (lambda (&rest _) (setq calls (1+ calls)))
                            '((name . edmacs-sidebar-buffers-live-test--count-refresh)))
                (unwind-protect
                    (progn
                      ;; Focus moves INTO the sidebar -- the guard must no-op.
                      (select-window sidebar-window)
                      (run-hook-with-args 'window-selection-change-functions (selected-frame))
                      (should (= calls 0))
                      ;; Focus moves back OUT -- refresh runs normally again.
                      (select-window main)
                      (run-hook-with-args 'window-selection-change-functions (selected-frame))
                      (should (> calls 0)))
                  (advice-remove 'edmacs-sidebar-buffers--refresh-markers
                                 'edmacs-sidebar-buffers-live-test--count-refresh))))))))

    ;; ==========================================================================
    ;; AC3 -- s toggles tree vs. flat, per-frame
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-live-test-toggle-flat-and-back ()
      (let ((root (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (let ((a (edmacs-sidebar-buffers-live-test--write-file root "modules/a.el"))
                (b (edmacs-sidebar-buffers-live-test--write-file root "b.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "repo" root)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
            (find-file a) (find-file b)
            (edmacs-sidebar-show (selected-frame))
            (should (string-match-p "modules/" (edmacs-sidebar-buffers-live-test--sidebar-text (selected-frame))))
            (edmacs-sidebar-buffers-toggle-flat (selected-frame))
            (should (frame-parameter (selected-frame) 'edmacs-sidebar-buffers-flat))
            (should-not (string-match-p "modules/" (edmacs-sidebar-buffers-live-test--sidebar-text (selected-frame))))
            (should (string-match-p "a.el" (edmacs-sidebar-buffers-live-test--sidebar-text (selected-frame))))
            (edmacs-sidebar-buffers-toggle-flat (selected-frame))
            (should-not (frame-parameter (selected-frame) 'edmacs-sidebar-buffers-flat))
            (should (string-match-p "modules/" (edmacs-sidebar-buffers-live-test--sidebar-text (selected-frame))))))))

    (ert-deftest edmacs-sidebar-buffers-live-test-toggle-is-frame-local ()
      "`make-frame' selects the frame it creates, so f1 must be captured
before calling the helper and re-selected afterward -- otherwise f2 and
`(selected-frame)' alias to the same frame and this test cannot fail
regardless of whether the toggle is actually frame-local (see this
file's own Commentary above)."
      (let* ((f1 (selected-frame))
             (f2 (edmacs-test-support-make-second-frame-or-skip)))
        (unwind-protect
            (progn
              (select-frame f1 'norecord)
              (edmacs-sidebar-buffers-toggle-flat (selected-frame))
              (should (frame-parameter f1 'edmacs-sidebar-buffers-flat))
              (should-not (frame-parameter f2 'edmacs-sidebar-buffers-flat))
              (edmacs-sidebar-buffers-toggle-flat (selected-frame)))
          (edmacs-sidebar-buffers-live-test--reset-frame f1)
          (edmacs-sidebar-buffers-live-test--reset-frame f2)
          (delete-frame f2))))

    ;; ==========================================================================
    ;; AC4 -- debounce coalesces buffer-list-update-hook firings
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-live-test-debounce-coalesces-bursts ()
      "Several firings within the debounce window collapse into exactly
one tracked, pending redraw -- verified deterministically (cancel the
one timer `--schedule-redraw' left behind and fire its callback
directly) rather than via a real `sit-for' wait: this whole batch
process shares one global `timer-list', and other tests' own
buffer-list churn (via the real, non-removed-there hook) can leave
their own still-pending redraw timers behind for a few milliseconds,
which a `sit-for'-based wait here could pick up too, inflating the
count for reasons having nothing to do with THIS test's own coalescing
claim. The real `buffer-list-update-hook' entry is removed for the
duration for the same reason: this test's own buffer churn must not
add another contender either."
      (let ((edmacs-sidebar-buffers-debounce-seconds 0.05)
            (redraw-count 0))
        (unwind-protect
            (progn
              (remove-hook 'buffer-list-update-hook #'edmacs-sidebar-buffers--schedule-redraw)
              (setq edmacs-sidebar-buffers--redraw-timer nil)
              (cl-letf (((symbol-function 'edmacs-sidebar--redraw)
                         (lambda (_frame) (setq redraw-count (1+ redraw-count)))))
                (dotimes (_ 10) (edmacs-sidebar-buffers--schedule-redraw))
                (should (= 0 redraw-count))
                (should (timerp edmacs-sidebar-buffers--redraw-timer))
                ;; Ten calls left exactly one pending timer tracked --
                ;; fire it directly, standing in for its own eventual
                ;; real firing.
                (let ((tm edmacs-sidebar-buffers--redraw-timer))
                  (cancel-timer tm)
                  (funcall (timer--function tm)))
                (should (= 1 redraw-count))))
          (add-hook 'buffer-list-update-hook #'edmacs-sidebar-buffers--schedule-redraw)
          (when (timerp edmacs-sidebar-buffers--redraw-timer)
            (cancel-timer edmacs-sidebar-buffers--redraw-timer))
          (setq edmacs-sidebar-buffers--redraw-timer nil))))

    (ert-deftest edmacs-sidebar-buffers-live-test-tab-switch-with-30-buffers-is-fast ()
      "Opening 30 nested file buffers in one tab keeps a later tab switch
fast: the redraw this triggers (`edmacs-sidebar--on-tab-select') is pure
in-memory work, never a subprocess. Measured with `elp' (the Emacs Lisp
Profiler, not wall-clock `benchmark-run') directly around
`edmacs-sidebar--redraw' itself -- isolating this module's own cost
from `tab-bar-select-tab's unrelated bookkeeping -- against the AC's
literal 50ms bound; `benchmark-run' still wraps the whole
`tab-bar-select-tab' call as a coarser, generously-bounded sanity
check on top."
      (require 'elp)
      (let ((root (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (edmacs-sidebar-buffers-live-test--register-worktrees
           "/repo/.git" (list (cons "repo" root)))
          (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
          (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
          (dotimes (i 30)
            (find-file (edmacs-sidebar-buffers-live-test--write-file
                        root (format "d%d/d%d/f%d.el" (% i 4) (% i 3) i))))
          (edmacs-sidebar-show (selected-frame))
          (let ((tab-bar-new-tab-choice "*scratch*"))
            (tab-bar-new-tab))
          (unwind-protect
              (progn
                (elp-instrument-function 'edmacs-sidebar--redraw)
                (elp-reset-function 'edmacs-sidebar--redraw)
                (let ((elapsed (car (benchmark-run 1 (tab-bar-select-tab 1)))))
                  (should (< elapsed 0.2))) ; generous, machine-independent sanity bound
                (let* ((info (get 'edmacs-sidebar--redraw elp-timer-info-property))
                       (calls (aref info 0))
                       (average (and (> calls 0) (/ (aref info 1) calls))))
                  (should (= calls 1))
                  ;; The AC's own literal bound, checked with a real profiler
                  ;; rather than a wall-clock proxy.
                  (should (< average 0.05))))
            (elp-restore-function 'edmacs-sidebar--redraw)))))

    ;; ==========================================================================
    ;; AC5 -- isolation: per-tab and per-frame
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-live-test-per-tab-isolation ()
      "Two tabs, one frame, disjoint files: neither tab's buffers subsection
ever lists the other's files, whichever tab is currently selected."
      (let ((r1 (edmacs-sidebar-buffers-live-test--make-root))
            (r2 (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list r1 r2)
          (let ((x (edmacs-sidebar-buffers-live-test--write-file r1 "x.el"))
                (y (edmacs-sidebar-buffers-live-test--write-file r1 "y.el"))
                (z (edmacs-sidebar-buffers-live-test--write-file r2 "z.el"))
                (w (edmacs-sidebar-buffers-live-test--write-file r2 "w.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "r1" r1) (cons "r2" r2)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r1)
            (find-file x) (find-file y)
            (let ((tab-bar-new-tab-choice "*scratch*"))
              (tab-bar-new-tab))
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r2)
            (find-file z) (find-file w)
            (edmacs-sidebar-show (selected-frame))
            ;; r2 (tab 2) is current here. sidebar.el renders BOTH
            ;; worktrees' rows on every redraw regardless of which tab is
            ;; current (the other one just folded, never omitted), so
            ;; isolation is checked by scoping to each root's OWN
            ;; subsection span -- a whole-buffer text search would always
            ;; see both worktrees' rows and could never fail this check.
            (let ((r1-text (edmacs-sidebar-buffers-live-test--subsection-text (selected-frame) r1))
                  (r2-text (edmacs-sidebar-buffers-live-test--subsection-text (selected-frame) r2)))
              (should (string-match-p "z.el" r2-text))
              (should (string-match-p "w.el" r2-text))
              (should-not (string-match-p "x.el" r2-text))
              (should-not (string-match-p "y.el" r2-text))
              (should (string-match-p "x.el" r1-text))
              (should (string-match-p "y.el" r1-text))
              (should-not (string-match-p "z.el" r1-text))
              (should-not (string-match-p "w.el" r1-text)))
            (tab-bar-select-tab 1)
            ;; r1 (tab 1) is current now -- re-check both scoped
            ;; subsections again, exercising the ws-tree ordering path
            ;; for r2 (now the non-current tab).
            (let ((r1-text (edmacs-sidebar-buffers-live-test--subsection-text (selected-frame) r1))
                  (r2-text (edmacs-sidebar-buffers-live-test--subsection-text (selected-frame) r2)))
              (should (string-match-p "x.el" r1-text))
              (should (string-match-p "y.el" r1-text))
              (should-not (string-match-p "z.el" r1-text))
              (should-not (string-match-p "w.el" r1-text))
              (should (string-match-p "z.el" r2-text))
              (should (string-match-p "w.el" r2-text))
              (should-not (string-match-p "x.el" r2-text))
              (should-not (string-match-p "y.el" r2-text)))))))

    (ert-deftest edmacs-sidebar-buffers-live-test-per-frame-isolation ()
      "Two real frames, same-numbered tabs, different files: catches a
frame-argument mixup in the `bufferlo-buffer-list' call site that a
single-frame test cannot. `make-frame' selects the frame it creates, so
f1 must be captured before calling the helper and its own setup done
under an explicit `with-selected-frame' -- otherwise f2 and
`(selected-frame)' alias to the same frame and \"frame 1\"'s setup below
would silently run against f2 too (see this file's own Commentary
above).

Since edmacs-tab-groups phase 3's singleton-buffer collapse, showing
the sidebar on f2 redraws the very buffer f1 was just shown in -- so
this now asserts each frame's OWN redraw reflects that frame's own tab
list correctly, captured immediately after that frame's own `--show'
and before the other frame's redraw overwrites it, rather than that
the two frames hold simultaneously-distinct buffers (no longer true by
design; see `edmacs-sidebar-test-singleton-buffer-shared-across-frames'
in sidebar-test.el). Still catches the same frame-argument-mixup bug
class: a wrong FRAME threaded into `bufferlo-buffer-list' renders the
wrong tab's buffers regardless of how many buffers exist at once."
      (let* ((f1 (selected-frame))
             (f2 (edmacs-test-support-make-second-frame-or-skip))
             (r1 (edmacs-sidebar-buffers-live-test--make-root))
             (r2 (edmacs-sidebar-buffers-live-test--make-root)))
        (unwind-protect
            (let ((p (edmacs-sidebar-buffers-live-test--write-file r1 "p.el"))
                  (q (edmacs-sidebar-buffers-live-test--write-file r2 "q.el")))
              (edmacs-sidebar-buffers-live-test--register-worktrees
               "/repo1/.git" (list (cons "r1" r1)))
              (edmacs-sidebar-buffers-live-test--register-worktrees
               "/repo2/.git" (list (cons "r2" r2)))
              (with-selected-frame f1
                (setq edmacs-sidebar-buffers-live-test--group "/repo1/.git")
                (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r1)
                (find-file p)
                (edmacs-sidebar-show f1))
              (let ((text1 (edmacs-sidebar-buffers-live-test--sidebar-text f1)))
                (should (string-match-p "p.el" text1))
                (should-not (string-match-p "q.el" text1)))
              (with-selected-frame f2
                (setq edmacs-sidebar-buffers-live-test--group "/repo2/.git")
                (edmacs-sidebar-buffers-live-test--stamp-current-tab-root r2)
                (find-file q)
                (edmacs-sidebar-show f2))
              (let ((text2 (edmacs-sidebar-buffers-live-test--sidebar-text f2)))
                (should (string-match-p "q.el" text2))
                (should-not (string-match-p "p.el" text2))))
          (edmacs-sidebar-buffers-live-test--kill-buffers-under r1)
          (edmacs-sidebar-buffers-live-test--kill-buffers-under r2)
          (ignore-errors (delete-directory r1 t))
          (ignore-errors (delete-directory r2 t))
          (edmacs-sidebar-buffers-live-test--reset-frame f1)
          (edmacs-sidebar-buffers-live-test--reset-frame f2)
          (delete-frame f2)
          (select-frame f1 'norecord)
          (switch-to-buffer (get-buffer-create "*scratch*")))))

    ;; ==========================================================================
    ;; d -- kill the buffer at point
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-live-test-kill-removes-buffer-and-row ()
      (let ((root (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (let ((a (edmacs-sidebar-buffers-live-test--write-file root "a.el"))
                (b (edmacs-sidebar-buffers-live-test--write-file root "b.el")))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "repo" root)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
            (find-file a) (find-file b)
            (edmacs-sidebar-show (selected-frame))
            (edmacs-sidebar-buffers-live-test--goto-text (selected-frame) "a.el")
            (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
              (edmacs-sidebar-buffers-kill (selected-frame)))
            (should-not (get-file-buffer a))
            (should-not (string-match-p "a\\.el"
                                        (edmacs-sidebar-buffers-live-test--sidebar-text (selected-frame))))))))

    ;; ==========================================================================
    ;; Composed render path -- worktree row + agent group + buffer tree,
    ;; all nested together, under the same subprocess-signal guard as
    ;; sidebar-test.el's `edmacs-sidebar-test-redraw-and-hooks-never-shell-out'.
    ;; That suite's own Commentary loads sidebar.el alone, so it never
    ;; registers sidebar-agents.el's/sidebar-buffers.el's
    ;; `edmacs-sidebar-worktree-section-functions' hooks and never actually
    ;; renders the nested sections a real repo frame does -- this file
    ;; already loads sidebar.el + windows.el + bufferlo for real, and now
    ;; agents.el + sidebar-agents.el too (above), so it can close that gap
    ;; without inventing a fourth test file.
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-live-test-composed-worktree-render-never-shell-out ()
      "Drives a real worktree row whose rendered section tree actually
nests both sidebar-agents.el's `agents' group and sidebar-buffers.el's
per-tab buffer tree -- the composed shape a real repo frame renders,
which neither module's own pure suite (each loads only sidebar.el) nor
this file's other tests (no agent ever registered) exercises under a
subprocess guard. Deliberately excludes agent-visit/tmux-jump
\(sidebar-agents.el's own documented `start-process' exception, already
covered by its own tests\) is not reachable from this loop's own
render/navigation/rename path."
      (let ((root (edmacs-sidebar-buffers-live-test--make-root)))
        (edmacs-sidebar-buffers-live-test--with-scenario (list root)
          (let* ((a (edmacs-sidebar-buffers-live-test--write-file root "a.el"))
                 (agent-key (edmacs-agents--key root "1"))
                 (violations nil)
                 (guarded '(call-process call-process-region process-file
                            start-process start-file-process make-process)))
            (edmacs-sidebar-buffers-live-test--register-worktrees
             "/repo/.git" (list (cons "repo" root)))
            (setq edmacs-sidebar-buffers-live-test--group "/repo/.git")
            (edmacs-sidebar-buffers-live-test--stamp-current-tab-root root)
            (find-file a)
            (puthash agent-key
                     (make-edmacs-agent :key agent-key :root root :instance "1"
                                         :status 'working :status-ts (float-time)
                                         :updated-ts (float-time)
                                         :title "composed-render-agent"
                                         :source 'claude-term :locator nil :unread nil)
                     edmacs-agents--table)
            (unwind-protect
                (progn
                  (edmacs-sidebar-show (selected-frame))
                  ;; Confirm the composed shape is actually there before
                  ;; guarding it -- a guard around a render that silently
                  ;; skipped the nested sections would prove nothing.
                  (let ((text (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                                (buffer-string))))
                    (should (string-match-p "composed-render-agent" text))
                    (should (string-match-p "a\\.el" text)))
                  (dolist (fn guarded)
                    (advice-add fn :before
                                (lambda (&rest _) (push fn violations))
                                `((name . ,(intern (format "edmacs-sidebar-live-test--guard-%s" fn))))))
                  (cl-letf (((symbol-function 'read-from-minibuffer)
                             (lambda (&rest _) "edmacs-sidebar-live-test-composed-renamed")))
                    (dotimes (_ 50)
                      (edmacs-sidebar--redraw (selected-frame))
                      (edmacs-sidebar-redraw (selected-frame))
                      (with-current-buffer (edmacs-sidebar--buffer (selected-frame))
                        (goto-char (point-min))
                        (edmacs-sidebar-move-to-next-worktree)
                        (edmacs-sidebar-move-to-prev-worktree)
                        (edmacs-sidebar-rename-at-point))))
                  (sleep-for 0.2)
                  (sit-for 0)
                  (should-not violations))
              (dolist (fn guarded)
                (advice-remove fn (intern (format "edmacs-sidebar-live-test--guard-%s" fn))))
              (remhash agent-key edmacs-agents--table)
              (ignore-errors (tab-bar-rename-tab "")))))))

    ))

(defun edmacs-sidebar-buffers-live-test-run-and-exit ()
  "Run this suite, undo any timer/buffer/`tab-bar-mode' it leaks, then exit.
Point `-f' at this instead of `ert-run-tests-batch-and-exit' directly:
that function calls `kill-emacs' itself, and `kill-emacs' does not run
Lisp `unwind-protect' cleanups up its caller's stack -- wrapping ITS call
in `edmacs-test-support-with-hermetic-state' would never actually run the
cleanup. Calling the non-exiting `ert-run-tests-batch' inside the
hermetic-state form, then exiting afterward with the same status
`ert-run-tests-batch-and-exit' would have used, gets both properties."
  (let (stats)
    (edmacs-test-support-with-hermetic-state
      (setq stats (ert-run-tests-batch nil)))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

;;; sidebar-buffers-live-test.el ends here
