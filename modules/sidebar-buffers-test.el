;;; sidebar-buffers-test.el --- Tests for sidebar-buffers.el -*- lexical-binding: t -*-

;;; Commentary:
;; Like sidebar-agents-test.el, sidebar-buffers.el's own
;; `(require 'magit-section)' needs `load-path' fixed against the
;; straight build tree before it can even be parsed under `-Q --batch' --
;; so this file carries the same kind of self-contained invocation:
;;
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/sidebar-buffers-test.el -f ert-run-tests-batch-and-exit
;;
;; sidebar.el and frames.el are NOT loaded (mirroring sidebar-agents-test.el's
;; own module-boundary convention); every function sidebar-buffers.el
;; calls into either is stubbed via `cl-letf' or a plain stand-in
;; `defun'. This suite covers the pure tree/rank/path machinery;
;; end-to-end rendering against real frames/tabs/bufferlo is
;; sidebar-buffers-live-test.el's job.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'dired)

;; See sidebar-test.el's own Commentary for why this is needed: `advice-add'
;; on a primitive can otherwise spawn a real native-comp trampoline
;; subprocess the first time this file's own stubs are installed.
(setq native-comp-enable-subr-trampolines nil)

(defun edmacs-sidebar-buffers-test--locate-straight-build-root ()
  "Same logic as sidebar-agents-test.el's own helper of the same shape."
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

(defun edmacs-sidebar-buffers-test--add-magit-section-deps (build-root)
  (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section"))
    (let ((dir (expand-file-name dep build-root)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(defvar edmacs-sidebar-buffers-test--build-root
  (edmacs-sidebar-buffers-test--locate-straight-build-root))

(if (null edmacs-sidebar-buffers-test--build-root)

    (ert-deftest edmacs-sidebar-buffers-test-magit-section-unavailable ()
      (ert-skip "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (edmacs-sidebar-buffers-test--add-magit-section-deps edmacs-sidebar-buffers-test--build-root)

    ;; sidebar-buffers.el's own forward `declare-function's for
    ;; sidebar.el/frames.el/windows.el/bufferlo are byte-compile hygiene
    ;; only; real stand-ins are provided here so its top-level `add-hook'
    ;; calls have something real to touch, the same way
    ;; sidebar-agents-test.el pre-populates its own extension-point vars.
    (defvar edmacs-sidebar-worktree-section-functions nil)
    (defvar edmacs-sidebar-extra-section-functions nil)
    (defvar edmacs-sidebar-force-text-glyphs nil)
    (defun edmacs-sidebar--buffer (_frame) nil)
    (defun edmacs-sidebar--redraw (_frame) nil)
    (defun edmacs-sidebar--find-buffer-section (_name) nil)
    (defun edmacs-frames--tab-for-root (_root &optional _frame) nil)
    (defun edmacs-main-window () nil)
    (defun edmacs-window-pop-buffer-to-main (_buffer) nil)
    (defun bufferlo-buffer-list (&optional _frame _tabnum _include-hidden) nil)

    (load (expand-file-name "modules/sidebar-buffers.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Helpers: temp buffers with a fake `buffer-file-name'/`dired-directory'
    ;; ==========================================================================

    (cl-defun edmacs-sidebar-buffers-test--file-buffer (path &key modified)
      "Return a fresh buffer whose `buffer-file-name' is PATH (need not
exist on disk -- classification and path relativization never stat it)."
      (let ((buf (generate-new-buffer (file-name-nondirectory path))))
        (with-current-buffer buf
          (setq buffer-file-name path)
          (when modified (set-buffer-modified-p t)))
        buf))

    (cl-defun edmacs-sidebar-buffers-test--dired-buffer (dir)
      (let ((buf (generate-new-buffer (format "dired-%s" dir))))
        (with-current-buffer buf
          (setq default-directory dir)
          (setq dired-directory dir)
          (dired-mode))
        buf))

    (defmacro edmacs-sidebar-buffers-test--with-buffers (bindings &rest body)
      "Bind each (VAR BUF) in BINDINGS, run BODY, then kill every BUF."
      (declare (indent 1))
      `(let ,(mapcar #'car bindings)
         ,@(mapcar (lambda (b) `(setq ,(car b) ,(cadr b))) bindings)
         (unwind-protect (progn ,@body)
           ,@(mapcar (lambda (b) `(when (buffer-live-p ,(car b)) (kill-buffer ,(car b))))
                      bindings))))

    ;; ==========================================================================
    ;; Classification
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-test-file-like-p ()
      (edmacs-sidebar-buffers-test--with-buffers
          ((f (edmacs-sidebar-buffers-test--file-buffer "/repo/modules/ui.el"))
           (d (edmacs-sidebar-buffers-test--dired-buffer "/repo/modules/"))
           (m (generate-new-buffer "*Messages*")))
        (should (edmacs-sidebar-buffers--file-like-p f))
        (should (edmacs-sidebar-buffers--file-like-p d))
        (should-not (edmacs-sidebar-buffers--file-like-p m))))

    (ert-deftest edmacs-sidebar-buffers-test-file-like-p-dead-buffer ()
      (let ((buf (generate-new-buffer "temp")))
        (kill-buffer buf)
        (should-not (edmacs-sidebar-buffers--file-like-p buf))))

    ;; ==========================================================================
    ;; Path relativization
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-test-path-relative ()
      (edmacs-sidebar-buffers-test--with-buffers
          ((f (edmacs-sidebar-buffers-test--file-buffer "/repo/modules/ui.el")))
        (should (equal "modules/ui.el" (edmacs-sidebar-buffers--path f "/repo/")))))

    (ert-deftest edmacs-sidebar-buffers-test-path-root-level ()
      (edmacs-sidebar-buffers-test--with-buffers
          ((f (edmacs-sidebar-buffers-test--file-buffer "/repo/init.el")))
        (should (equal "init.el" (edmacs-sidebar-buffers--path f "/repo/")))))

    (ert-deftest edmacs-sidebar-buffers-test-path-outside-root ()
      (edmacs-sidebar-buffers-test--with-buffers
          ((f (edmacs-sidebar-buffers-test--file-buffer "/elsewhere/foo.txt")))
        (let ((rel (edmacs-sidebar-buffers--path f "/repo/")))
          (should (edmacs-sidebar-buffers--outside-root-p rel)))))

    ;; ==========================================================================
    ;; Rank
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-test-rank-present-and-absent ()
      (edmacs-sidebar-buffers-test--with-buffers
          ((a (generate-new-buffer "A")) (b (generate-new-buffer "B")))
        (let ((prev-names (list (buffer-name a) (buffer-name b))))
          (should (= 0 (edmacs-sidebar-buffers--rank a prev-names)))
          (should (= 1 (edmacs-sidebar-buffers--rank b prev-names)))
          (should (= most-positive-fixnum
                     (edmacs-sidebar-buffers--rank (generate-new-buffer "C") prev-names))))))

    ;; ==========================================================================
    ;; Tree build + chain flattening + sibling sort
    ;; ==========================================================================

    (defun edmacs-sidebar-buffers-test--dir-node (tree seg)
      (seq-find (lambda (n) (and (eq (plist-get n :kind) 'dir) (equal (plist-get n :seg) seg)))
                tree))

    (ert-deftest edmacs-sidebar-buffers-test-build-tree-groups-by-directory ()
      (edmacs-sidebar-buffers-test--with-buffers
          ((ui (edmacs-sidebar-buffers-test--file-buffer "/repo/modules/ui.el"))
           (sessions (edmacs-sidebar-buffers-test--file-buffer "/repo/modules/sessions.el"))
           (init (edmacs-sidebar-buffers-test--file-buffer "/repo/init.el")))
        (let* ((tree (edmacs-sidebar-buffers--build-tree (list ui sessions init) "/repo/"))
               (modules (edmacs-sidebar-buffers-test--dir-node tree "modules")))
          (should modules)
          (should (= 2 (length (plist-get modules :children))))
          (should (seq-find (lambda (n) (and (eq (plist-get n :kind) 'file)
                                              (eq (plist-get n :buf) init)))
                             tree)))))

    (ert-deftest edmacs-sidebar-buffers-test-flatten-chain-merges-and-collapses-lone-file ()
      "The literal 'claude-repl/ claude-repl-buffer.el' example: a/b/ with
only a lone file collapses into ONE file section."
      (edmacs-sidebar-buffers-test--with-buffers
          ((f (edmacs-sidebar-buffers-test--file-buffer
               "/repo/modules/claude-repl/claude-repl-buffer.el")))
        (let* ((tree (edmacs-sidebar-buffers--build-tree (list f) "/repo/"))
               (flat (edmacs-sidebar-buffers--flatten-chains tree)))
          (should (= 1 (length flat)))
          (let ((node (car flat)))
            (should (eq (plist-get node :kind) 'file))
            (should (equal "modules/claude-repl/ claude-repl-buffer.el" (plist-get node :name)))
            (should (eq (plist-get node :buf) f))))))

    (ert-deftest edmacs-sidebar-buffers-test-flatten-chain-stops-at-branch ()
      "A chain ending in a real branch (two files) stays a dir section,
its segment fully chain-merged."
      (edmacs-sidebar-buffers-test--with-buffers
          ((a (edmacs-sidebar-buffers-test--file-buffer "/repo/a/b/one.el"))
           (b (edmacs-sidebar-buffers-test--file-buffer "/repo/a/b/two.el")))
        (let* ((tree (edmacs-sidebar-buffers--build-tree (list a b) "/repo/"))
               (flat (edmacs-sidebar-buffers--flatten-chains tree)))
          (should (= 1 (length flat)))
          (let ((node (car flat)))
            (should (eq (plist-get node :kind) 'dir))
            (should (equal "a/b" (plist-get node :seg)))
            (should (= 2 (length (plist-get node :children))))))))

    (ert-deftest edmacs-sidebar-buffers-test-sort-siblings-dirs-before-files ()
      "AC1's own worked example: a directory sorts ahead of a same-level
file even when the file was visited more recently (init.el's rank 0 vs.
modules/'s aggregate rank 1)."
      (edmacs-sidebar-buffers-test--with-buffers
          ((ui (edmacs-sidebar-buffers-test--file-buffer "/repo/modules/ui.el"))
           (sessions (edmacs-sidebar-buffers-test--file-buffer "/repo/modules/sessions.el"))
           (init (edmacs-sidebar-buffers-test--file-buffer "/repo/init.el")))
        ;; prev-buffers order (most-recent-first): init.el, ui.el, sessions.el.
        (let* ((prev-names (list (buffer-name init) (buffer-name ui) (buffer-name sessions)))
               (tree (edmacs-sidebar-buffers--flatten-chains
                      (edmacs-sidebar-buffers--build-tree (list ui sessions init) "/repo/")))
               (sorted (edmacs-sidebar-buffers--sort-siblings tree prev-names)))
          (should (= 2 (length sorted)))
          (should (eq (plist-get (nth 0 sorted) :kind) 'dir))
          (should (equal "modules" (plist-get (nth 0 sorted) :seg)))
          (should (eq (plist-get (nth 1 sorted) :kind) 'file))
          (should (eq (plist-get (nth 1 sorted) :buf) init))
          ;; Within modules/: ui.el (rank 1) before sessions.el (rank 2).
          (let ((children (edmacs-sidebar-buffers--sort-siblings
                            (plist-get (nth 0 sorted) :children) prev-names)))
            (should (eq (plist-get (nth 0 children) :buf) ui))
            (should (eq (plist-get (nth 1 children) :buf) sessions))))))

    ;; ==========================================================================
    ;; Flat-mode ordering
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-test-sort-bufs-prev-names-order ()
      (edmacs-sidebar-buffers-test--with-buffers
          ((a (edmacs-sidebar-buffers-test--file-buffer "/repo/a.el"))
           (b (generate-new-buffer "*Messages*"))
           (c (edmacs-sidebar-buffers-test--file-buffer "/repo/c.el")))
        (let* ((prev-names (list (buffer-name c) (buffer-name a) (buffer-name b)))
               (sorted (edmacs-sidebar-buffers--sort-bufs (list a b c) prev-names)))
          (should (equal (list c a b) sorted)))))

    (ert-deftest edmacs-sidebar-buffers-test-sort-bufs-unranked-go-last-alphabetically ()
      (edmacs-sidebar-buffers-test--with-buffers
          ((a (edmacs-sidebar-buffers-test--file-buffer "/repo/zzz.el"))
           (b (edmacs-sidebar-buffers-test--file-buffer "/repo/aaa.el")))
        (let ((sorted (edmacs-sidebar-buffers--sort-bufs (list a b) nil)))
          (should (equal (list b a) sorted)))))

    ;; ==========================================================================
    ;; Visible-marker overlay: glyph + face (phase 8, AC2)
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-test-decorate-overlay-visible-marker-has-glyph-and-face ()
      "The visible-marker `before-string' carries
`edmacs-sidebar-buffer-visible-face' and the current
nerd-icons-availability state's glyph/fallback -- ASCII fallback here,
since `nerd-icons' is not loaded by this standalone suite."
      (edmacs-sidebar-buffers-test--with-buffers
          ((buf (edmacs-sidebar-buffers-test--file-buffer "/repo/a.el")))
        (with-temp-buffer
          (let ((ov (make-overlay (point-min) (point-min))))
            (cl-letf (((symbol-function 'get-buffer-window) (lambda (_buf _frame) t))
                      ((symbol-function 'frame-selected-window) (lambda (_frame) nil)))
              (edmacs-sidebar-buffers--decorate-overlay ov buf 'fake-frame))
            (let ((before (overlay-get ov 'before-string)))
              (should (equal (concat edmacs-sidebar-buffers--visible-glyph-fallback " ") before))
              (should (eq (get-text-property 0 'face before) 'edmacs-sidebar-buffer-visible-face)))))))

    (ert-deftest edmacs-sidebar-buffers-test-decorate-overlay-invisible-marker-is-blank ()
      (edmacs-sidebar-buffers-test--with-buffers
          ((buf (edmacs-sidebar-buffers-test--file-buffer "/repo/a.el")))
        (with-temp-buffer
          (let ((ov (make-overlay (point-min) (point-min))))
            (cl-letf (((symbol-function 'get-buffer-window) (lambda (_buf _frame) nil))
                      ((symbol-function 'frame-selected-window) (lambda (_frame) nil)))
              (edmacs-sidebar-buffers--decorate-overlay ov buf 'fake-frame))
            (should (equal "  " (overlay-get ov 'before-string)))))))

    (ert-deftest edmacs-sidebar-buffers-test-visible-glyph-force-text-overrides-nerd-icons ()
      (cl-letf (((symbol-function 'nerd-icons-octicon) (lambda (_name) "NERD-EYE"))
                ((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'fboundp) (lambda (f) (eq f 'nerd-icons-octicon)))
                (edmacs-sidebar-force-text-glyphs t))
        (should (equal edmacs-sidebar-buffers--visible-glyph-fallback
                        (edmacs-sidebar-buffers--visible-glyph)))))

    ;; ==========================================================================
    ;; RET -- visiting a buffer row (no-silent-no-op coverage)
    ;; ==========================================================================

    (defmacro edmacs-sidebar-buffers-test--with-sidebar-buffer (&rest body)
      "Run BODY in a fresh `magit-section-mode' temp buffer -- mirrors
sidebar-agents-test.el's own helper of the same shape."
      (declare (indent 0))
      `(with-temp-buffer
         (magit-section-mode)
         (let ((inhibit-read-only t))
           ,@body)))

    (ert-deftest edmacs-sidebar-buffers-test-visit-no-section-reports ()
      "`edmacs-sidebar-buffers-visit' signals `user-error' rather than doing
nothing when there is no section at point at all -- the direct-call
counterpart of sidebar.el's own no-silent-no-op fix, since RET never
reaches this function without a buffer-file/-special section
(`edmacs-sidebar-visit-at-point' gates on section type first)."
      (edmacs-sidebar-buffers-test--with-sidebar-buffer
        (should-error (edmacs-sidebar-buffers-visit) :type 'user-error)))

    (ert-deftest edmacs-sidebar-buffers-test-visit-file-row-without-root-reports ()
      "A buffer-file row constructed with no enclosing
`edmacs-sidebar-buffers-root' ancestor -- a degenerate/direct-call
construction that never occurs from the real redraw, which always wraps
rows in a root section -- has no tab identity to resolve and reports
rather than silently doing nothing."
      (edmacs-sidebar-buffers-test--with-buffers
          ((buf (edmacs-sidebar-buffers-test--file-buffer "/repo/a.el")))
        (edmacs-sidebar-buffers-test--with-sidebar-buffer
          (magit-insert-section (edmacs-sidebar-buffers-file buf)
            (insert "row\n"))
          (goto-char (point-min))
          (should-error (edmacs-sidebar-buffers-visit) :type 'user-error))))

    ;; ==========================================================================
    ;; Frame-locality: every `set-frame-parameter'/`frame-parameter' call
    ;; the toggle and the per-worktree render make is explicitly scoped to
    ;; the frame it was handed, never an implicit `(selected-frame)'/nil
    ;; fallback. Durable, environment-independent coverage of the same
    ;; invariant sidebar-buffers-live-test.el's `toggle-is-frame-local'/
    ;; `per-frame-isolation' exercise with a real second tty frame (see
    ;; that file's own Commentary for why those two need `script' and are
    ;; excused there) -- simulated here with two distinct sentinel "frame"
    ;; objects (never real frame values, so no controlling terminal is
    ;; needed) by fully replacing `frame-parameter'/`set-frame-parameter'
    ;; rather than delegating to the real primitives, which would reject a
    ;; non-live-frame argument outright.
    ;; ==========================================================================

    (ert-deftest edmacs-sidebar-buffers-test-toggle-flat-scopes-every-call-to-selected-frame ()
      "`edmacs-sidebar-buffers-toggle-flat' reads `(selected-frame)' once and
must thread that exact value into every `frame-parameter',
`set-frame-parameter', and `edmacs-sidebar--redraw' call it makes --
never falling back to an implicit nil/selected-frame default partway
through."
      (dolist (sentinel (list (make-symbol "frame-a") (make-symbol "frame-b")))
        (let (calls)
          (cl-letf (((symbol-function 'selected-frame) (lambda () sentinel))
                    ((symbol-function 'frame-parameter)
                     (lambda (frame param) (push (list 'frame-parameter frame param) calls) nil))
                    ((symbol-function 'set-frame-parameter)
                     (lambda (frame param value)
                       (push (list 'set-frame-parameter frame param value) calls) value))
                    ((symbol-function 'edmacs-sidebar--redraw)
                     (lambda (frame) (push (list 'redraw frame) calls) nil)))
            (edmacs-sidebar-buffers-toggle-flat (selected-frame))
            (should calls)
            (dolist (call calls)
              (should (eq (nth 1 call) sentinel)))))))

    (ert-deftest edmacs-sidebar-buffers-test-on-worktree-section-scopes-frame-parameter-to-explicit-frame ()
      "`edmacs-sidebar-buffers--on-worktree-section' takes FRAME as an
explicit argument (unlike the interactive toggle, it has no
`(selected-frame)' of its own to fall back to) and its `frame-parameter'
read of the flat flag must use exactly that argument, for either of two
distinct simulated frames -- catching a hop that silently swapped in nil
or a module-global instead of the FRAME actually passed in."
      (dolist (sentinel (list (make-symbol "frame-a") (make-symbol "frame-b")))
        (let (calls)
          (cl-letf (((symbol-function 'frame-parameter)
                     (lambda (frame param) (push (list frame param) calls) nil))
                    ((symbol-function 'bufferlo-buffer-list) (lambda (&rest _) nil))
                    ((symbol-function 'edmacs-frames--tab-for-root) (lambda (&rest _) nil)))
            (edmacs-sidebar-buffers-test--with-sidebar-buffer
              ;; Nested under an outer root, matching how the real
              ;; `edmacs-sidebar--redraw' always wraps this call -- called
              ;; bare, this section IS the magit root and a non-current
              ;; tab's `hidden' slot then hits `magit-section-hide's
              ;; "cannot hide root section" guard.
              (magit-insert-section (edmacs-sidebar-root nil nil)
                (edmacs-sidebar-buffers--on-worktree-section "/tmp/edmacs-sb-test-root" t sentinel 1)))
            (should calls)
            (dolist (call calls)
              (should (eq (car call) sentinel)))))))

    ;; ==========================================================================
    ;; AC2 -- buffers-root fold state survives its enclosing tab's number
    ;; shifting (the same cons-shape fragility AC1's worktree-row fix
    ;; addresses, now for this section's own value)
    ;; ==========================================================================

    (defun edmacs-sidebar-buffers-test--find-child-of-type (parent type)
      "Depth-first search PARENT's descendant `magit-section's for the
first one of TYPE, or nil."
      (catch 'found
        (dolist (child (oref parent children))
          (when (eq (oref child type) type)
            (throw 'found child))
          (when-let* ((found (edmacs-sidebar-buffers-test--find-child-of-type child type)))
            (throw 'found found)))
        nil))

    (ert-deftest edmacs-sidebar-buffers-test-buffers-root-fold-survives-tab-number-churn ()
      "`edmacs-sidebar-buffers-root's section value is the bare ROOT
string, not a `(ROOT . TAB-NUMBER)' cons, so its `magit-section-ident'
stays stable when the enclosing worktree's open tab shifts index (e.g.
another tab closing ahead of it) -- the same tab-number-churn fragility
AC1's worktree-row fix addresses. Fold it, rebuild the same buffer with
a DIFFERENT TAB-NUMBER (simulating that shift), and confirm
`magit-section-cached-visibility' -- magit-section.el's own free
mechanism, keyed on ident -- restores it hidden, with no bespoke
fold-preservation code of this phase's own."
      (cl-letf (((symbol-function 'edmacs-frames--tab-for-root)
                 (lambda (&rest _) (cons 'current-tab nil))))
        (with-temp-buffer
          (magit-section-mode)
          (let ((inhibit-read-only t))
            (magit-insert-section (edmacs-sidebar-root nil nil)
              (edmacs-sidebar-buffers--on-worktree-section "/repo/wt/" t (selected-frame) 1))
            (let ((root-sec (edmacs-sidebar-buffers-test--find-child-of-type
                              magit-root-section 'edmacs-sidebar-buffers-root)))
              (should root-sec)
              (should (eq nil (oref root-sec hidden)))
              (magit-section-hide root-sec))
            (erase-buffer)
            (magit-insert-section (edmacs-sidebar-root nil nil)
              (edmacs-sidebar-buffers--on-worktree-section "/repo/wt/" t (selected-frame) 2))
            (let ((root-sec (edmacs-sidebar-buffers-test--find-child-of-type
                              magit-root-section 'edmacs-sidebar-buffers-root)))
              (should root-sec)
              (should (eq t (oref root-sec hidden))))))))

    ))

(ert-deftest edmacs-sidebar-buffers-test-listable-p-admits-work ()
  "Files, dired, live-process and compilation buffers list."
  (let ((file (generate-new-buffer "notes.md"))
        (comp (generate-new-buffer "*compilation*")))
    (unwind-protect
        (progn
          (with-current-buffer file (setq buffer-file-name "/tmp/notes.md"))
          (with-current-buffer comp (setq major-mode 'compilation-mode))
          (should (edmacs-sidebar-buffers--listable-p file))
          (should (edmacs-sidebar-buffers--listable-p comp)))
      (mapc (lambda (b) (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b))
            (list file comp)))))

(ert-deftest edmacs-sidebar-buffers-test-listable-p-rejects-chrome ()
  "The sidebar's own buffer, which-key, help and internals never list."
  (let ((sidebar (generate-new-buffer "*sidebar: edmacs - Emacs*"))
        (wk (generate-new-buffer "*which-key*"))
        (help (generate-new-buffer "*Help*"))
        (internal (generate-new-buffer " *hidden*")))
    (unwind-protect
        (progn
          (with-current-buffer sidebar (setq major-mode 'edmacs-sidebar-mode))
          (with-current-buffer help (setq major-mode 'help-mode))
          (dolist (b (list sidebar wk help internal))
            (should-not (edmacs-sidebar-buffers--listable-p b))))
      (mapc #'kill-buffer (list sidebar wk help internal)))))
