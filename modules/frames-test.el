;;; frames-test.el --- Tests for frames.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage only -- no extra frame is created here (that
;; needs a controlling terminal; see frames-live-test.el). Frame-list
;; scanning helpers are exercised by stubbing `frame-list'/`frame-live-p'/
;; `frame-parameter' via `cl-letf' rather than spawning real frames, so
;; this suite runs under plain `-Q --batch' with no pty.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/windows.el -l modules/frames.el \
;;         -l modules/frames-test.el -f ert-run-tests-batch-and-exit
;;
;; `modules/windows.el' is on that line because `edmacs-frames--reset-to-spare'
;; now calls `edmacs-windows-repair-frame' to get a non-side reset target;
;; without it the reset test below fails with a void-function rather than
;; skipping.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; `display-graphic-p', `frame-initial-p' and `daemonp' are subrs, and
;; `cl-letf' on a subr makes Emacs build a native trampoline whose compile
;; fails under `-Q' when the eln-cache is not writable. Nothing here needs
;; one -- frames.el is loaded from source, so its callers read the stub
;; straight out of the symbol's function cell. (sessions-test.el precedent.)
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

;; ============================================================================
;; edmacs-frames--repo-of
;; ============================================================================
;; The main-worktree/repo-name string derivation on top of a resolved
;; git-common-dir lives in `modules/git-common-dir.el' (shared with
;; sessions.el and claude-term-registry.el) and is covered by
;; `modules/git-common-dir-test.el', not here.

(ert-deftest edmacs-frames-test-repo-of-uses-cache-no-shellout ()
  "A cache hit for the resolved root never calls the uncached resolver.
`project-current' is stubbed to report no project for DIR, so
`edmacs-frames--repo-of' falls back to DIR itself as the cache key --
exactly the path exercised without depending on this worktree's own
real git ancestry."
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal))
        (dir "/fake/no-such/worktree/"))
    (puthash dir "/repo/.git" edmacs-git-common-dir-cache)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'edmacs-git-common-dir-1)
               (lambda (&rest _) (error "should not shell out"))))
      (should (equal (edmacs-frames--repo-of dir) "/repo/.git")))))

(ert-deftest edmacs-frames-test-repo-of-nil-on-miss ()
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal))
        (dir "/fake/no-such/worktree/"))
    (puthash dir 'none edmacs-git-common-dir-cache)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'edmacs-git-common-dir-1)
               (lambda (&rest _) (error "should not shell out"))))
      (should-not (edmacs-frames--repo-of dir)))))

(ert-deftest edmacs-frames-test-repo-of-uses-project-root-as-cache-key ()
  "When DIR is inside a known project, the PROJECT ROOT is the cache key.
Matches the convention `edmacs-sessions--tab-name' and
`claude-term-registry--repo-name' already use, so lookups from any file
inside the same worktree share one cache entry."
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
    (puthash "/repo/" "/repo/.git" edmacs-git-common-dir-cache)
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) 'fake-project))
              ((symbol-function 'project-root)
               (lambda (_) "/repo/")))
      (should (equal (edmacs-frames--repo-of "/repo/deeply/nested/file-dir/")
                     "/repo/.git")))))

;; ============================================================================
;; edmacs-frames-for-repo / --spare-frame / --repo-frames / --only-frame-p
;; ============================================================================
;; Fake frames are plain symbols; `frame-list', `frame-live-p', and
;; `frame-parameter' are stubbed to treat them as an alist of parameters,
;; so no real frame is ever created here.

(defvar edmacs-frames-test--frames nil
  "Alist of (FRAME-SYMBOL . PARAMS) the faked frame primitives read.")

(defun edmacs-frames-test--fake-param (frame key default)
  "Return FRAME's fake KEY parameter, or DEFAULT when it carries none.
A nil FRAME means the selected frame, matching every real frame
primitive's own optional-FRAME convention."
  (let ((params (cdr (assq (or frame (selected-frame))
                           edmacs-frames-test--frames))))
    (if (assq key params) (alist-get key params) default)))

(defmacro edmacs-frames-test--with-fake-frames (alist &rest body)
  "Run BODY with frame-scanning primitives faked from ALIST.
ALIST is a list of (FRAME-SYMBOL . PARAMS-ALIST). Beyond the frame
parameters under test, these entries drive the faked primitives, each
with the default an existing caller that never mentions it gets:
`visible' (`frame-visible-p', t), `graphic' (`display-graphic-p', t),
`initial' (`frame-initial-p', nil), `wedged'
(`edmacs-windows-frame-wedged-p', nil) and `tab-count' (the number of
tabs `tab-bar-tabs' reports, 1). `daemonp' reports non-nil throughout,
so `edmacs-frames-frame-usable-p's daemon-placeholder exclusion is the
shape actually exercised rather than short-circuited.

`set-frame-parameter' writes back into ALIST, so a test that drives
reconciliation can read the demotion afterward -- pass a freshly
consed ALIST, never a quoted literal, when BODY can mutate it."
  (declare (indent 1))
  `(let ((edmacs-frames-test--frames ,alist))
     (cl-letf (((symbol-function 'frame-list)
                (lambda () (mapcar #'car edmacs-frames-test--frames)))
               ((symbol-function 'frame-live-p)
                (lambda (f) (assq f edmacs-frames-test--frames)))
               ((symbol-function 'frame-visible-p)
                (lambda (f) (edmacs-frames-test--fake-param f 'visible t)))
               ((symbol-function 'display-graphic-p)
                (lambda (&optional f) (edmacs-frames-test--fake-param f 'graphic t)))
               ((symbol-function 'frame-initial-p)
                (lambda (f) (edmacs-frames-test--fake-param f 'initial nil)))
               ((symbol-function 'daemonp) (lambda (&rest _) t))
               ((symbol-function 'edmacs-windows-frame-wedged-p)
                (lambda (&optional f) (edmacs-frames-test--fake-param f 'wedged nil)))
               ((symbol-function 'tab-bar-tabs)
                (lambda (&optional f)
                  (make-list (edmacs-frames-test--fake-param f 'tab-count 1) 'tab)))
               ((symbol-function 'set-frame-parameter)
                (lambda (f param value)
                  (let ((entry (assq (or f (selected-frame))
                                     edmacs-frames-test--frames)))
                    (when entry (setf (alist-get param (cdr entry)) value)))))
               ((symbol-function 'frame-parameter)
                (lambda (f param) (edmacs-frames-test--fake-param f param nil))))
       ,@body)))


(ert-deftest edmacs-frames-test-for-repo-finds-matching-frame ()
  (edmacs-frames-test--with-fake-frames
      '((fa . ((edmacs-repo . "/r1/.git")))
        (fb . ((edmacs-repo . "/r2/.git"))))
    (should (eq (edmacs-frames-for-repo "/r2/.git") 'fb))
    (should-not (edmacs-frames-for-repo "/r3/.git"))))

(ert-deftest edmacs-frames-test-spare-frame-is-repo-less-one ()
  (edmacs-frames-test--with-fake-frames
      '((fa . ((edmacs-repo . "/r1/.git")))
        (fb . nil))
    (should (eq (edmacs-frames--spare-frame) 'fb))))

(ert-deftest edmacs-frames-test-spare-frame-nil-when-none ()
  (edmacs-frames-test--with-fake-frames
      '((fa . ((edmacs-repo . "/r1/.git"))))
    (should-not (edmacs-frames--spare-frame))))

(ert-deftest edmacs-frames-test-repo-frames-excludes-repo-less ()
  (edmacs-frames-test--with-fake-frames
      '((fa . ((edmacs-repo . "/r1/.git") (name . "r1")))
        (fb . nil)
        (fc . ((edmacs-repo . "/r2/.git") (name . "r2"))))
    (should (equal (edmacs-frames--repo-frames) '(fa fc)))))

(ert-deftest edmacs-frames-test-only-frame-p ()
  (edmacs-frames-test--with-fake-frames '((fa . nil))
    (should (edmacs-frames--only-frame-p 'fa)))
  (edmacs-frames-test--with-fake-frames '((fa . nil) (fb . nil))
    (should-not (edmacs-frames--only-frame-p 'fa))))

(ert-deftest edmacs-frames-test-only-frame-p-ignores-corfu-style-child-frame ()
  "A corfu-style completion popup must never count as a second frame.
`corfu--hide-frame' only calls `make-frame-invisible' -- the popup frame
stays `frame-live-p' but invisible, and carries a `parent-frame'
parameter, for the rest of the session once it has ever shown once."
  (edmacs-frames-test--with-fake-frames
      '((fa . nil)
        (corfu . ((visible . nil) (parent-frame . fa))))
    (should (edmacs-frames--only-frame-p 'fa)))
  ;; Even a momentarily-VISIBLE popup is still a child frame, not a
  ;; second real one.
  (edmacs-frames-test--with-fake-frames
      '((fa . nil)
        (corfu . ((visible . t) (parent-frame . fa))))
    (should (edmacs-frames--only-frame-p 'fa))))

;; ============================================================================
;; edmacs-frames-tab-in-own-repo-p -- the tab-name repo-prefix-drop predicate
;; ============================================================================
;; Exercised against the real selected frame (a real, if degenerate, frame
;; even under `-Q --batch') rather than a fake one -- no stubbing needed
;; for a single frame-parameter read.

(ert-deftest edmacs-frames-test-tab-in-own-repo-p-match ()
  (let ((frame (selected-frame)))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'edmacs-repo "/repo/.git")
          (should (edmacs-frames-tab-in-own-repo-p "/repo/.git" frame))
          (should-not (edmacs-frames-tab-in-own-repo-p "/other/.git" frame))
          (should-not (edmacs-frames-tab-in-own-repo-p nil frame)))
      (set-frame-parameter frame 'edmacs-repo nil))))

;; ============================================================================
;; edmacs-frames--cached-common-dir -- cache-hit-only, never shells out
;; ============================================================================

(ert-deftest edmacs-frames-test-cached-common-dir-hit-and-miss ()
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
    (puthash "/repo/sub/" "/repo/.git" edmacs-git-common-dir-cache)
    (should (equal (edmacs-frames--cached-common-dir "/repo/sub/") "/repo/.git"))
    (should-not (edmacs-frames--cached-common-dir "/repo/never-looked-up/"))))

(ert-deftest edmacs-frames-test-cached-common-dir-none-sentinel-is-a-miss ()
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
    (puthash "/not/a/repo/" 'none edmacs-git-common-dir-cache)
    (should-not (edmacs-frames--cached-common-dir "/not/a/repo/"))))

(ert-deftest edmacs-frames-test-cached-common-dir-trailing-slash-tolerant ()
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
    (puthash "/repo/" "/repo/.git" edmacs-git-common-dir-cache)
    (should (equal (edmacs-frames--cached-common-dir "/repo") "/repo/.git"))
    (should (equal (edmacs-frames--cached-common-dir "/repo/") "/repo/.git"))))

(ert-deftest edmacs-frames-test-cached-common-dir-never-shells-out ()
  (let ((edmacs-git-common-dir-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'edmacs-git-common-dir-1)
               (lambda (&rest _) (error "should not shell out")))
              ((symbol-function 'edmacs-git-common-dir)
               (lambda (&rest _) (error "should not shell out"))))
      (should-not (edmacs-frames--cached-common-dir "/never/looked/up/")))))

;; ============================================================================
;; Frame eligibility -- usable / healthy
;; ============================================================================

(ert-deftest edmacs-frames-test-frame-usable-p-excludes-daemon-placeholder ()
  "The daemon's initial tty frame is never usable while a GUI frame exists.
That is the frame `desktop--check-dont-save' already refuses to save and
that nothing can ever be displayed on."
  (edmacs-frames-test--with-fake-frames
      '((gui . ((graphic . t)))
        (f1 . ((graphic . nil) (initial . t))))
    (should (edmacs-frames-frame-usable-p 'gui))
    (should-not (edmacs-frames-frame-usable-p 'f1))))

(ert-deftest edmacs-frames-test-frame-usable-p-excludes-child-frame ()
  "A corfu-style popup is a frame by construction, never a repo frame."
  (edmacs-frames-test--with-fake-frames
      '((gui . ((graphic . t)))
        (popup . ((graphic . t) (parent-frame . gui))))
    (should-not (edmacs-frames-frame-usable-p 'popup))))

(ert-deftest edmacs-frames-test-frame-usable-p-allows-tty-in-a-tty-only-session ()
  "With no graphical frame anywhere, a tty frame is still adoptable.
The disjunct that keeps this repo's own tty-only batch harnesses (and a
genuinely terminal-only Emacs) working."
  (edmacs-frames-test--with-fake-frames
      '((tty-a . ((graphic . nil)))
        (tty-b . ((graphic . nil) (initial . t))))
    (should (edmacs-frames-frame-usable-p 'tty-a))
    ;; The daemon's own placeholder stays excluded even here.
    (should-not (edmacs-frames-frame-usable-p 'tty-b))))

(ert-deftest edmacs-frames-test-frame-healthy-p-rejects-a-wedged-frame ()
  (edmacs-frames-test--with-fake-frames
      '((ok . ((graphic . t)))
        (wedged . ((graphic . t) (wedged . t))))
    (should (edmacs-frames--frame-healthy-p 'ok))
    (should-not (edmacs-frames--frame-healthy-p 'wedged))
    ;; Wedged is still USABLE -- windows.el repairs it on its next
    ;; `display-buffer', so a lookup may still return it as a last resort.
    (should (edmacs-frames-frame-usable-p 'wedged))))

(ert-deftest edmacs-frames-test-spare-frame-skips-tty-initial-frame ()
  "The placeholder is repo-less, so without the usability gate it is the
first spare every `emacsclient -e'-driven open would find and adopt."
  (edmacs-frames-test--with-fake-frames
      '((f1 . ((graphic . nil) (initial . t)))
        (gui . ((graphic . t))))
    (should (eq (edmacs-frames--spare-frame) 'gui)))
  (edmacs-frames-test--with-fake-frames
      '((f1 . ((graphic . nil) (initial . t)))
        (other-gui . ((graphic . t) (edmacs-repo . "/r1/.git"))))
    (should-not (edmacs-frames--spare-frame))))

;; ============================================================================
;; edmacs-frames--make-frame -- the daemon's GUI-frame dispatch
;; ============================================================================
;; The counterpart to excluding the tty placeholder from spare adoption:
;; with no spare left to adopt, `edmacs-frames-open' falls through to this,
;; and a bare `make-frame' under a daemon (`window-system' nil) would build
;; exactly the frame that exclusion exists to keep repos off.

(ert-deftest edmacs-frames-test-make-frame-uses-the-gui-maker-under-a-daemon ()
  "Under a daemon the frame must come from sessions.el's GUI maker.
`make-frame' is not merely a worse choice there -- it names no window
system, so it yields another tty placeholder, and an `emacsclient -e'
open would stamp its repo onto a frame that can display nothing."
  (let (calls)
    (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
              ((symbol-function 'edmacs-sessions--make-gui-frame)
               (lambda () (push 'gui calls) 'gui-frame))
              ((symbol-function 'make-frame)
               (lambda (&rest _) (push 'bare calls) 'tty-frame)))
      (should (eq (edmacs-frames--make-frame) 'gui-frame)))
    ;; Not just "returned the right frame" -- `make-frame' was never reached.
    (should (equal calls '(gui)))))

(ert-deftest edmacs-frames-test-make-frame-falls-back-to-plain-make-frame ()
  "Outside a daemon `window-system' is already right, so nothing is consulted."
  (let (calls)
    (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) nil))
              ((symbol-function 'edmacs-sessions--make-gui-frame)
               (lambda () (push 'gui calls) 'gui-frame))
              ((symbol-function 'make-frame)
               (lambda (&rest _) (push 'bare calls) 'tty-frame)))
      (should (eq (edmacs-frames--make-frame) 'tty-frame)))
    (should (equal calls '(bare)))))

(ert-deftest edmacs-frames-test-make-frame-falls-back-when-gui-maker-is-absent ()
  "frames.el must stay loadable without sessions.el -- this very suite
loads it that way -- so the daemon branch is `fboundp'-guarded."
  (let ((saved (and (fboundp 'edmacs-sessions--make-gui-frame)
                    (symbol-function 'edmacs-sessions--make-gui-frame))))
    (unwind-protect
        (progn
          (fmakunbound 'edmacs-sessions--make-gui-frame)
          (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
                    ((symbol-function 'make-frame) (lambda (&rest _) 'tty-frame)))
            (should (eq (edmacs-frames--make-frame) 'tty-frame))))
      (when saved (fset 'edmacs-sessions--make-gui-frame saved)))))

(ert-deftest edmacs-frames-test-make-frame-falls-back-when-gui-maker-fails ()
  "`edmacs-sessions--make-gui-frame' warns and returns nil when the window
system refuses the frame; a repo frame is still better than none."
  (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
            ((symbol-function 'edmacs-sessions--make-gui-frame) (lambda () nil))
            ((symbol-function 'make-frame) (lambda (&rest _) 'tty-frame)))
    (should (eq (edmacs-frames--make-frame) 'tty-frame))))

;; ============================================================================
;; edmacs-frames-for-repo -- healthy first, duplicates reconciled
;; ============================================================================

(ert-deftest edmacs-frames-test-for-repo-prefers-healthy-graphical-frame ()
  "Not `seq-find's first match: the tty placeholder and the wedged frame
both come first in `frame-list' and both must lose to the healthy one."
  (cl-letf (((symbol-function 'display-warning) #'ignore))
    (let ((frames (list (cons 'f1 (list (cons 'edmacs-repo "/r1/.git")
                                        (cons 'graphic nil)
                                        (cons 'initial t)))
                        (cons 'wedged (list (cons 'edmacs-repo "/r1/.git")
                                            (cons 'graphic t)
                                            (cons 'wedged t)))
                        (cons 'healthy (list (cons 'edmacs-repo "/r1/.git")
                                             (cons 'graphic t))))))
      (edmacs-frames-test--with-fake-frames frames
        (should (eq (edmacs-frames-for-repo "/r1/.git") 'healthy))
        ;; The losers stop answering for this repo at all.
        (should-not (alist-get 'edmacs-repo (alist-get 'f1 frames)))
        (should-not (alist-get 'edmacs-repo (alist-get 'wedged frames)))))))

(ert-deftest edmacs-frames-test-for-repo-returns-a-lone-wedged-frame ()
  "Repair is destructive to slot layout, so a lookup returns the wedged
frame rather than repairing it -- windows.el recovers it on its next
`display-buffer'."
  (edmacs-frames-test--with-fake-frames
      (list (cons 'wedged (list (cons 'edmacs-repo "/r1/.git")
                                (cons 'graphic t)
                                (cons 'wedged t))))
    (should (eq (edmacs-frames-for-repo "/r1/.git") 'wedged))))

(ert-deftest edmacs-frames-test-for-repo-demotes-a-lone-unusable-frame ()
  "The concrete U3 repair: a tty placeholder wrongly back-filled with a
repo answers nil and loses the stamp, rather than being handed to
`select-frame-set-input-focus'."
  (cl-letf (((symbol-function 'display-warning) #'ignore))
    (let ((frames (list (cons 'f1 (list (cons 'edmacs-repo "/r1/.git")
                                        (cons 'name "r1")
                                        (cons 'graphic nil)
                                        (cons 'initial t)))
                        (cons 'gui (list (cons 'graphic t))))))
      (edmacs-frames-test--with-fake-frames frames
        (should-not (edmacs-frames-for-repo "/r1/.git"))
        (should-not (alist-get 'edmacs-repo (alist-get 'f1 frames)))
        (should-not (alist-get 'name (alist-get 'f1 frames)))))))

(ert-deftest edmacs-frames-test-reconcile-demotes-duplicate-repo-frames ()
  "Two healthy frames claiming one repo: the one holding more tabs
survives, the other is un-named rather than deleted, and a second run
changes nothing."
  (let ((warnings 0))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (setq warnings (1+ warnings))))
              ((symbol-function 'delete-frame)
               (lambda (&rest _) (error "reconciliation must never delete a frame"))))
      (let ((frames (list (cons 'one-tab (list (cons 'edmacs-repo "/r1/.git")
                                               (cons 'name "r1")
                                               (cons 'graphic t)
                                               (cons 'tab-count 1)))
                          (cons 'two-tabs (list (cons 'edmacs-repo "/r1/.git")
                                                (cons 'name "r1")
                                                (cons 'graphic t)
                                                (cons 'tab-count 2))))))
        (edmacs-frames-test--with-fake-frames frames
          (should (eq (edmacs-frames--reconcile-repo-frames "/r1/.git") 'two-tabs))
          (should (equal (alist-get 'edmacs-repo (alist-get 'two-tabs frames))
                         "/r1/.git"))
          (should-not (alist-get 'edmacs-repo (alist-get 'one-tab frames)))
          (should-not (alist-get 'name (alist-get 'one-tab frames)))
          (should (= warnings 1))
          ;; Idempotent: nothing left to demote, no second warning.
          (should (eq (edmacs-frames--reconcile-repo-frames "/r1/.git") 'two-tabs))
          (should (= warnings 1)))))))

(ert-deftest edmacs-frames-test-reconcile-is-a-no-op-for-a-single-match ()
  (cl-letf (((symbol-function 'display-warning)
             (lambda (&rest _) (error "nothing to demote here"))))
    (edmacs-frames-test--with-fake-frames
        (list (cons 'only (list (cons 'edmacs-repo "/r1/.git") (cons 'graphic t))))
      (should (eq (edmacs-frames--reconcile-repo-frames "/r1/.git") 'only))
      (should-not (edmacs-frames--reconcile-repo-frames "/nobody/.git")))))

;; ============================================================================
;; Stamped identity -- nothing is sniffed from a window-state blob
;; ============================================================================

(ert-deftest edmacs-frames-test-no-window-state-sniffing-remains ()
  "Both derivations this phase deleted stay deleted.
`edmacs-frames--ws-selected-buffer-name' pcase-matched shapes a real
tab's `ws' field never has, and `edmacs-frames--tab-window-buffer' read
the GLOBALLY selected window and let `edmacs-frames--tab-root' cache
another frame's answer onto this frame's tab forever."
  (should-not (fboundp 'edmacs-frames--ws-selected-buffer-name))
  (should-not (fboundp 'edmacs-frames--tab-window-buffer))
  (let ((text (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "modules/frames.el" default-directory))
                (buffer-string))))
    (should-not (string-match-p "(alist-get 'ws " text))))

(ert-deftest edmacs-frames-test-tab-root-stored-property-wins ()
  (let ((tab '(tab (edmacs-root . "/stored/root/"))))
    (should (equal (edmacs-frames--tab-root tab) "/stored/root/"))))

(ert-deftest edmacs-frames-test-tab-root-is-a-pure-read ()
  "An unstamped tab reads nil -- no derivation, and nothing cached onto it."
  (let ((tab (list 'current-tab (cons 'ws '((min-height . 4) (leaf))))))
    (cl-letf (((symbol-function 'window-buffer)
               (lambda (&rest _) (error "a tab root is never derived here"))))
      (should-not (edmacs-frames--tab-root tab)))
    (should-not (assq 'edmacs-root (cdr tab)))))

(ert-deftest edmacs-frames-test-stamp-replaces-rather-than-shadows ()
  "Re-stamping must REPLACE `edmacs-root', not `push' a second cons in
front of it: `tab-bar--tab' copies every unrecognized tab parameter
forward on each switch, so a shadowed stale entry would be duplicated
into the desktop file forever."
  (let* ((frame (selected-frame))
         (saved (frame-parameter frame 'tabs))
         (tab (list 'current-tab (cons 'edmacs-root "/old/"))))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'tabs (list tab))
          (edmacs-frames--stamp-current-tab-root "/new/" frame)
          (should (equal (alist-get 'edmacs-root (cdr tab)) "/new/"))
          (should (= 1 (seq-count (lambda (e) (eq (car-safe e) 'edmacs-root))
                                  (cdr tab)))))
      (set-frame-parameter frame 'tabs saved))))

(ert-deftest edmacs-frames-test-stamp-frame-tabs-stamps-only-an-unstamped-tab ()
  "The restore-walk entry point: stamp a tab with no root, leave a
stamped one strictly alone (never re-derive over a stored answer)."
  (let* ((frame (selected-frame))
         (saved (frame-parameter frame 'tabs))
         (bare (list 'current-tab))
         (stamped (list 'current-tab (cons 'edmacs-root "/kept/"))))
    (unwind-protect
        (cl-letf (((symbol-function 'edmacs-frames--derive-root)
                   (lambda (_frame) "/derived/")))
          (set-frame-parameter frame 'tabs (list bare))
          (edmacs-frames-stamp-frame-tabs frame)
          (should (equal (alist-get 'edmacs-root (cdr bare)) "/derived/"))
          (set-frame-parameter frame 'tabs (list stamped))
          (edmacs-frames-stamp-frame-tabs frame)
          (should (equal (alist-get 'edmacs-root (cdr stamped)) "/kept/")))
      (set-frame-parameter frame 'tabs saved))))

(ert-deftest edmacs-frames-test-post-open-stamps-the-pending-root ()
  "When this module asked for the tab, the INTENDED root is stamped --
never one derived from the buffer the new tab inherited."
  (let* ((frame (selected-frame))
         (saved (frame-parameter frame 'tabs))
         (tab (list 'current-tab)))
    (unwind-protect
        (cl-letf (((symbol-function 'edmacs-frames--derive-root)
                   (lambda (_frame) "/inherited/"))
                  ((symbol-function 'edmacs-frames--reconcile-tab-after-open) #'ignore))
          (set-frame-parameter frame 'tabs (list tab))
          (let ((edmacs-frames--pending-tab-root "/intended/"))
            (edmacs-frames--on-tab-post-open tab))
          (should (equal (alist-get 'edmacs-root (cdr tab)) "/intended/")))
      (set-frame-parameter frame 'tabs saved))))

(ert-deftest edmacs-frames-test-post-open-ignores-another-frames-tab ()
  "`tab-bar-tabs' auto-creates a default tab, and runs this hook, for a
frame it never names. Stamping that one from the SELECTED frame's
buffer is exactly the cross-frame derivation this phase removed."
  (let* ((frame (selected-frame))
         (saved (frame-parameter frame 'tabs))
         (mine (list 'current-tab (cons 'edmacs-root "/mine/")))
         (theirs (list 'current-tab)))
    (unwind-protect
        (cl-letf (((symbol-function 'edmacs-frames--derive-root)
                   (lambda (_frame) "/selected-frames-buffer/"))
                  ((symbol-function 'edmacs-frames--reconcile-tab-after-open) #'ignore))
          (set-frame-parameter frame 'tabs (list mine))
          (edmacs-frames--on-tab-post-open theirs)
          (should-not (alist-get 'edmacs-root (cdr theirs))))
      (set-frame-parameter frame 'tabs saved))))

(ert-deftest edmacs-frames-test-post-select-repairs-an-unstamped-tab ()
  "The repair path for a tab restored from a pre-stamp desktop file."
  (let* ((frame (selected-frame))
         (saved (frame-parameter frame 'tabs))
         (bare (list 'current-tab))
         (stamped (list 'current-tab (cons 'edmacs-root "/kept/"))))
    (unwind-protect
        (cl-letf (((symbol-function 'edmacs-frames--derive-root)
                   (lambda (_frame) "/derived/")))
          (set-frame-parameter frame 'tabs (list bare))
          (edmacs-frames--on-tab-post-select nil nil)
          (should (equal (alist-get 'edmacs-root (cdr bare)) "/derived/"))
          (set-frame-parameter frame 'tabs (list stamped))
          (edmacs-frames--on-tab-post-select nil nil)
          (should (equal (alist-get 'edmacs-root (cdr stamped)) "/kept/")))
      (set-frame-parameter frame 'tabs saved))))

(ert-deftest edmacs-frames-test-edmacs-root-survives-frameset-tab-filter ()
  "The desktop half of \"every tab is stamped\": `frameset-filter-tabs'
strips only the `wc' family on save, so a background tab's own
`edmacs-root' (and its `ws') round-trip through the desktop file and
need no re-derivation on restore."
  (let* ((tabs (list (list 'tab
                           (cons 'name "wt")
                           (cons 'edmacs-root "/wt/")
                           (cons 'ws '((min-height . 4) (leaf)))
                           (cons 'wc 'unprintable)
                           (cons 'wc-point 1)
                           (cons 'wc-bl nil))))
         (saved (car (frameset-filter-tabs tabs nil nil t))))
    (should (equal (alist-get 'edmacs-root saved) "/wt/"))
    (should (alist-get 'ws saved))
    (should-not (assq 'wc saved))
    (should-not (assq 'wc-point saved))
    (should-not (assq 'wc-bl saved))))

;; ============================================================================
;; AC10 -- sessions.el's commentary no longer argues against frames
;; ============================================================================

(ert-deftest edmacs-frames-test-sessions-commentary-corrected ()
  "sessions.el's Commentary drops the old tabs-over-frames rationale and
records the real constraint, with the corrected upstream citations."
  (let ((text (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "modules/sessions.el" default-directory))
                (buffer-string))))
    ;; The stale premise this phase's own body names as having been fixed
    ;; upstream: claude-code-ide.el's tab-awareness argument, once used to
    ;; justify avoiding frames outright.
    (should-not (string-match-p "per-frame is where manzaltu" text))
    (should-not (string-match-p "explicitly tab-aware" text))
    (should (string-match-p "window-adjust-process-window-size-smallest" text))
    (should (string-match-p "manzaltu#197" text))
    (should (string-match-p "ghostel#504" text))))

;; ============================================================================
;; AC1 -- edmacs-frames--worktrees-compute / edmacs-worktrees-for-repo
;; ============================================================================

(ert-deftest edmacs-frames-test-worktrees-compute-shape ()
  "COMMON's worktrees become (NAME . TRUENAME-ROOT), main worktree included."
  (cl-letf (((symbol-function 'edmacs-frames--repo-worktrees)
             (lambda (_common) '("/repo/" "/repo/../wt-a/" "/repo/../wt-b/")))
            ((symbol-function 'file-truename) #'identity))
    (should (equal (edmacs-frames--worktrees-compute "/repo/.git")
                   '(("repo" . "/repo/")
                     ("wt-a" . "/repo/../wt-a/")
                     ("wt-b" . "/repo/../wt-b/"))))))

(ert-deftest edmacs-frames-test-worktrees-for-repo-returns-cached-value-verbatim ()
  "A populated cache is returned without ever recomputing."
  (let ((edmacs-frames--worktrees-cache (make-hash-table :test #'equal)))
    (puthash "/repo/.git" '(("repo" . "/repo/")) edmacs-frames--worktrees-cache)
    (cl-letf (((symbol-function 'edmacs-frames--worktrees-compute)
               (lambda (&rest _) (error "should not recompute on a cache hit"))))
      (should (equal (edmacs-worktrees-for-repo "/repo/.git")
                     '(("repo" . "/repo/")))))))

(ert-deftest edmacs-frames-test-worktrees-for-repo-miss-is-nil-never-computes ()
  "The direct regression test for the strict-pure-cache-read contract:
a cache miss is an empty list, never a fallback compute or refresh."
  (let ((edmacs-frames--worktrees-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'edmacs-frames--worktrees-compute)
               (lambda (&rest _) (error "must not compute on a cache miss")))
              ((symbol-function 'edmacs-frames--worktrees-refresh)
               (lambda (&rest _) (error "must not refresh on a cache miss"))))
      (should-not (edmacs-worktrees-for-repo "/never/seen/.git")))))

(ert-deftest edmacs-frames-test-worktrees-refresh-populates-cache-and-redraws-matching-frames ()
  "Refresh writes the cache and redraws every frame for COMMON, no others --
covering the two-frames-same-repo edge case via `edmacs-frames--frames-for-repo-common'."
  (let ((edmacs-frames--worktrees-cache (make-hash-table :test #'equal))
        (redrawn nil))
    (edmacs-frames-test--with-fake-frames
        '((fa . ((edmacs-repo . "/repo/.git")))
          (fb . ((edmacs-repo . "/other/.git")))
          (fc . ((edmacs-repo . "/repo/.git"))))
      (cl-letf (((symbol-function 'edmacs-frames--worktrees-compute)
                 (lambda (_common) '(("repo" . "/repo/"))))
                ((symbol-function 'edmacs-sidebar--redraw)
                 (lambda (frame) (push frame redrawn))))
        (edmacs-frames--worktrees-refresh "/repo/.git")
        (should (equal (gethash "/repo/.git" edmacs-frames--worktrees-cache)
                       '(("repo" . "/repo/"))))
        (should (equal (sort (mapcar #'symbol-name redrawn) #'string<)
                       '("fa" "fc")))))))

(ert-deftest edmacs-frames-test-worktrees-refresh-safe-catches-and-preserves-cache ()
  "A signal from `edmacs-frames--worktrees-refresh' (e.g. the underlying
`vc-git-known-other-working-trees' subprocess call failing) is caught
rather than propagated, and leaves the cache exactly as it was --
covering the blocking finding that an uncaught error here would abort
`edmacs-frames-open' mid frame-setup with `edmacs-repo' already stamped."
  (let ((edmacs-frames--worktrees-cache (make-hash-table :test #'equal))
        (messages nil))
    (puthash "/repo/.git" '(("repo" . "/repo/")) edmacs-frames--worktrees-cache)
    (cl-letf (((symbol-function 'edmacs-frames--worktrees-compute)
               (lambda (_common) (error "transient git failure")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (should-not (condition-case err
                      (progn (edmacs-frames--worktrees-refresh-safe "/repo/.git") nil)
                    (error err)))
      (should (equal (gethash "/repo/.git" edmacs-frames--worktrees-cache)
                     '(("repo" . "/repo/"))))
      (should (= 1 (length messages)))
      (should (string-match-p "worktree refresh failed for /repo/\\.git" (car messages))))))

;; ============================================================================
;; AC3 -- debounce and lazy watch creation
;; ============================================================================

(ert-deftest edmacs-frames-test-schedule-worktrees-refresh-debounces ()
  "A burst of scheduling calls collapses to exactly one refresh, and never
leaves more than one pending timer per COMMON -- i.e. no polling timer."
  (let ((edmacs-frames--worktree-refresh-timers (make-hash-table :test #'equal))
        (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'edmacs-frames--worktrees-refresh)
                   (lambda (_common) (setq calls (1+ calls)))))
          (edmacs-frames--schedule-worktrees-refresh "/repo/.git")
          (edmacs-frames--schedule-worktrees-refresh "/repo/.git")
          (edmacs-frames--schedule-worktrees-refresh "/repo/.git")
          (should (= 1 (hash-table-count edmacs-frames--worktree-refresh-timers)))
          (sleep-for 0.7)
          (sit-for 0)
          (should (= 1 calls))
          (should (= 0 (hash-table-count edmacs-frames--worktree-refresh-timers))))
      (maphash (lambda (_k timer) (ignore-errors (cancel-timer timer)))
                edmacs-frames--worktree-refresh-timers))))

(ert-deftest edmacs-frames-test-ensure-worktrees-watch-direct-when-dir-exists ()
  "When `worktrees' already exists, it is watched directly, first try."
  (let ((edmacs-frames--worktree-watches (make-hash-table :test #'equal))
        (added nil))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
              ((symbol-function 'file-notify-add-watch)
               (lambda (dir flags _cb) (push (cons dir flags) added) 'desc-1)))
      (edmacs-frames--ensure-worktrees-watch "/repo/.git/")
      (should (equal (gethash "/repo/.git/" edmacs-frames--worktree-watches) 'desc-1))
      (should (= 1 (length added)))
      (should (equal (caar added) (expand-file-name "worktrees" "/repo/.git/")))
      ;; No-op the second time -- a watch is already stored.
      (edmacs-frames--ensure-worktrees-watch "/repo/.git/")
      (should (= 1 (length added))))))

(ert-deftest edmacs-frames-test-ensure-worktrees-watch-parent-then-upgrades ()
  "Absent `worktrees' watches the parent, filters to that filename, then
upgrades to a direct watch and refreshes once it actually appears."
  (let ((edmacs-frames--worktree-watches (make-hash-table :test #'equal))
        (added nil)
        (removed nil)
        (refreshed nil)
        (dir-exists nil))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) dir-exists))
              ((symbol-function 'file-notify-add-watch)
               (lambda (dir _flags cb)
                 (let ((desc (intern (format "desc-%d" (1+ (length added))))))
                   (push (list dir cb desc) added)
                   desc)))
              ((symbol-function 'file-notify-rm-watch)
               (lambda (desc) (push desc removed)))
              ((symbol-function 'edmacs-frames--worktrees-refresh)
               (lambda (common) (push common refreshed))))
      (edmacs-frames--ensure-worktrees-watch "/repo/.git/")
      (should (= 1 (length added)))
      (should (equal (nth 0 (car added)) "/repo/.git/"))
      ;; An unrelated `.git/index' write must not trigger anything.
      (funcall (nth 1 (car added)) (list 'desc-1 'changed "/repo/.git/index"))
      (should (= 1 (length added)))
      (should-not refreshed)
      ;; `worktrees' itself appearing upgrades to a direct watch and refreshes.
      (setq dir-exists t)
      (funcall (nth 1 (car added))
               (list 'desc-1 'created (expand-file-name "worktrees" "/repo/.git/")))
      (should (= 2 (length added)))
      (should (equal (nth 0 (car added)) (expand-file-name "worktrees" "/repo/.git/")))
      (should removed)
      (should (equal refreshed '("/repo/.git/"))))))

(ert-deftest edmacs-frames-test-teardown-worktrees-watch-cancels-both ()
  "Teardown cancels the pending debounce timer and removes the file-notify
watch, but leaves the cache entry itself in place."
  (let ((edmacs-frames--worktree-watches (make-hash-table :test #'equal))
        (edmacs-frames--worktree-refresh-timers (make-hash-table :test #'equal))
        (edmacs-frames--worktrees-cache (make-hash-table :test #'equal))
        (rm-called nil))
    (puthash "/repo/.git/" 'desc-1 edmacs-frames--worktree-watches)
    (puthash "/repo/.git/" '(("repo" . "/repo/")) edmacs-frames--worktrees-cache)
    (let ((timer (run-at-time 10 nil #'ignore)))
      (puthash "/repo/.git/" timer edmacs-frames--worktree-refresh-timers)
      (unwind-protect
          (cl-letf (((symbol-function 'file-notify-rm-watch)
                     (lambda (d) (setq rm-called d))))
            (edmacs-frames--teardown-worktrees-watch "/repo/.git/"))
        (ignore-errors (cancel-timer timer)))
      (should (eq rm-called 'desc-1))
      (should-not (gethash "/repo/.git/" edmacs-frames--worktree-watches))
      (should-not (gethash "/repo/.git/" edmacs-frames--worktree-refresh-timers))
      ;; Deliberately not evicted -- see the function's own commentary.
      (should (gethash "/repo/.git/" edmacs-frames--worktrees-cache)))))

(ert-deftest edmacs-frames-test-maybe-teardown-watch-wired-to-delete-frame-functions ()
  "The shared teardown helper must run on ANY frame deletion, not only
`edmacs-frames--close-last-tab' -- `edmacs-ns-close-frame' in sessions.el
deletes a frame directly, bypassing `tab-bar-close-last-tab-choice'
entirely, so `delete-frame-functions' is the only hook both paths share."
  (should (memq #'edmacs-frames--maybe-teardown-watch-for-frame
                delete-frame-functions)))

(ert-deftest edmacs-frames-test-maybe-teardown-watch-tears-down-last-frame ()
  "A direct `delete-frame'-style call on a repo's only frame reaps its
watch, independent of `tab-bar-close-last-tab-choice'."
  (edmacs-frames-test--with-fake-frames
      '((fa . ((edmacs-repo . "/repo/.git/"))))
    (let ((edmacs-frames--worktree-watches (make-hash-table :test #'equal))
          (edmacs-frames--worktree-refresh-timers (make-hash-table :test #'equal))
          (torn-down nil))
      (cl-letf (((symbol-function 'edmacs-frames--teardown-worktrees-watch)
                 (lambda (common) (push common torn-down))))
        (edmacs-frames--maybe-teardown-watch-for-frame 'fa))
      (should (equal torn-down '("/repo/.git/"))))))

(ert-deftest edmacs-frames-test-maybe-teardown-watch-spares-shared-repo ()
  "A sibling frame still on the same repo (transient spare-frame reuse)
keeps the watch alive -- only the truly-last frame for a repo tears it
down."
  (edmacs-frames-test--with-fake-frames
      '((fa . ((edmacs-repo . "/repo/.git/")))
        (fb . ((edmacs-repo . "/repo/.git/"))))
    (let ((torn-down nil))
      (cl-letf (((symbol-function 'edmacs-frames--teardown-worktrees-watch)
                 (lambda (common) (push common torn-down))))
        (edmacs-frames--maybe-teardown-watch-for-frame 'fa))
      (should-not torn-down))))

(ert-deftest edmacs-frames-test-maybe-teardown-watch-noop-for-repo-less-frame ()
  "A repo-less frame (e.g. the daemon's spare/scratch frame) has no
`edmacs-repo' to tear anything down for."
  (edmacs-frames-test--with-fake-frames '((fa . nil))
    (let ((torn-down nil))
      (cl-letf (((symbol-function 'edmacs-frames--teardown-worktrees-watch)
                 (lambda (common) (push common torn-down))))
        (edmacs-frames--maybe-teardown-watch-for-frame 'fa))
      (should-not torn-down))))

;; ============================================================================
;; AC4 -- edmacs-frames-open calls refresh/watch exactly once per new repo
;; ============================================================================

(ert-deftest edmacs-frames-test-open-first-frame-refreshes-worktrees-once ()
  "The first `edmacs-frames-open' for a repo refreshes+arms its watch exactly
once; a second call for the already-open repo does neither again.
Uses the suite's own real (selected) frame as the \"spare\" frame
`edmacs-frames-open' adopts -- its window/buffer-touching side effects
are themselves stubbed out, so nothing here needs a second real frame."
  (let ((frame (selected-frame))
        (refresh-calls 0) (watch-calls 0) (existing nil))
    (unwind-protect
        (cl-letf (((symbol-function 'edmacs-frames--repo-of) (lambda (_dir) "/repo/.git"))
                  ((symbol-function 'edmacs-frames-for-repo) (lambda (_common) existing))
                  ((symbol-function 'edmacs-frames--spare-frame) (lambda () frame))
                  ((symbol-function 'edmacs-git-common-dir-main-worktree)
                   (lambda (_common) "/repo/"))
                  ((symbol-function 'edmacs-git-common-dir-repo-name)
                   (lambda (_common) "repo"))
                  ((symbol-function 'edmacs-frames--worktrees-refresh)
                   (lambda (_common) (setq refresh-calls (1+ refresh-calls))))
                  ((symbol-function 'edmacs-frames--ensure-worktrees-watch)
                   (lambda (_common) (setq watch-calls (1+ watch-calls))))
                  ((symbol-function 'edmacs-frames--visit-root) #'ignore)
                  ((symbol-function 'edmacs-frames--stamp-current-tab-root) #'ignore)
                  ((symbol-function 'edmacs-frames-stamp-frame-tabs) #'ignore)
                  ((symbol-function 'tab-bar-rename-tab) #'ignore)
                  ((symbol-function 'edmacs-sidebar-show) #'ignore)
                  ((symbol-function 'select-frame-set-input-focus) #'ignore)
                  ((symbol-function 'delete-other-windows) #'ignore))
          (edmacs-frames-open "/repo/")
          (should (= 1 refresh-calls))
          (should (= 1 watch-calls))
          (setq existing frame)
          (edmacs-frames-open "/repo/")
          (should (= 1 refresh-calls))
          (should (= 1 watch-calls)))
      (set-frame-parameter frame 'edmacs-repo nil)
      (set-frame-parameter frame 'name nil))))

(ert-deftest edmacs-frames-test-open-first-frame-survives-worktrees-compute-error ()
  "A raw signal out of `edmacs-frames--worktrees-compute' (the underlying
`vc-git-known-other-working-trees' subprocess call) must not abort
`edmacs-frames-open' partway through: the dired visit, tab stamping,
rename, and sidebar display that follow the refresh call all still run.
Without `edmacs-frames--worktrees-refresh-safe' this signal propagates
out of `edmacs-frames-open' with `edmacs-repo' already stamped, leaving
a half-built frame that `edmacs-frames-for-repo' would keep re-selecting
forever."
  (let ((frame (selected-frame))
        (edmacs-frames--worktrees-cache (make-hash-table :test #'equal))
        (visited nil) (shown nil))
    (unwind-protect
        (cl-letf (((symbol-function 'edmacs-frames--repo-of) (lambda (_dir) "/repo/.git"))
                  ((symbol-function 'edmacs-frames-for-repo) (lambda (_common) nil))
                  ((symbol-function 'edmacs-frames--spare-frame) (lambda () frame))
                  ((symbol-function 'edmacs-git-common-dir-main-worktree)
                   (lambda (_common) "/repo/"))
                  ((symbol-function 'edmacs-git-common-dir-repo-name)
                   (lambda (_common) "repo"))
                  ((symbol-function 'edmacs-frames--worktrees-compute)
                   (lambda (_common) (error "transient git failure")))
                  ((symbol-function 'edmacs-frames--ensure-worktrees-watch) #'ignore)
                  ((symbol-function 'edmacs-frames--visit-root)
                   (lambda (_dir) (setq visited t)))
                  ((symbol-function 'edmacs-frames--stamp-current-tab-root) #'ignore)
                  ((symbol-function 'tab-bar-rename-tab) #'ignore)
                  ((symbol-function 'edmacs-sidebar-show)
                   (lambda (_frame) (setq shown t)))
                  ((symbol-function 'select-frame-set-input-focus) #'ignore)
                  ((symbol-function 'delete-other-windows) #'ignore))
          (should (eq frame (edmacs-frames-open "/repo/")))
          (should visited)
          (should shown)
          (should-not (edmacs-worktrees-for-repo "/repo/.git")))
      (set-frame-parameter frame 'edmacs-repo nil)
      (set-frame-parameter frame 'name nil))))

;; ============================================================================
;; Fullscreen policy
;; ============================================================================
;; Still no real frame: `display-graphic-p' joins the faked primitives, and
;; the deferring `run-at-time' is stubbed so the timer body can be run
;; synchronously and inspected.

(defmacro edmacs-frames-test--with-fullscreen-frames (alist &rest body)
  "Run BODY over fake frames ALIST with `display-graphic-p' faked too.
A fake frame counts as graphical when its params carry a non-nil
`graphic' entry, so the policy's own `display-graphic-p' gate is
exercised without ever opening a GUI frame."
  (declare (indent 1))
  `(edmacs-frames-test--with-fake-frames ,alist
     (cl-letf (((symbol-function 'display-graphic-p)
                (lambda (&optional f) (frame-parameter f 'graphic))))
       ,@body)))

(ert-deftest edmacs-frames-test-fullscreen-target-for-plain-graphical-frame ()
  (let ((edmacs-frames-fullscreen 'fullboth))
    (edmacs-frames-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (should (eq (edmacs-frames--fullscreen-target 'fa) 'fullboth)))))

(ert-deftest edmacs-frames-test-fullscreen-target-nil-when-policy-disabled ()
  (let ((edmacs-frames-fullscreen nil))
    (edmacs-frames-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (should-not (edmacs-frames--fullscreen-target 'fa)))))

(ert-deftest edmacs-frames-test-fullscreen-target-nil-for-tty-frame ()
  "The daemon's own tty placeholder and every `emacsclient -t' frame:
`fullscreen' means nothing there and is mangled by frameset's tty
shelving on the way into a desktop file. This gate is the reason the
policy is a hook rather than an entry in `default-frame-alist', which
those frames read too."
  (let ((edmacs-frames-fullscreen 'fullboth))
    (edmacs-frames-test--with-fullscreen-frames '((f1 . ((graphic . nil))))
      (should-not (edmacs-frames--fullscreen-target 'f1)))))

(ert-deftest edmacs-frames-test-fullscreen-policy-is-not-in-default-frame-alist ()
  "Pins the decision the test above documents: no `fullscreen' entry may
be added to `default-frame-alist', or the tty frames would inherit it."
  (should-not (assq 'fullscreen default-frame-alist)))

(ert-deftest edmacs-frames-test-fullscreen-target-nil-for-child-frame ()
  "A corfu/posframe-style completion popup is a graphical frame by
construction and must keep the size its owner gave it."
  (let ((edmacs-frames-fullscreen 'fullboth))
    (edmacs-frames-test--with-fullscreen-frames
        '((fa . ((graphic . t)))
          (popup . ((graphic . t) (parent-frame . fa))))
      (should-not (edmacs-frames--fullscreen-target 'popup)))))

(ert-deftest edmacs-frames-test-fullscreen-target-nil-when-already-there ()
  (let ((edmacs-frames-fullscreen 'fullboth))
    (edmacs-frames-test--with-fullscreen-frames
        '((fa . ((graphic . t) (fullscreen . fullboth))))
      (should-not (edmacs-frames--fullscreen-target 'fa)))
    ;; A frame at some OTHER fullscreen value still needs correcting.
    (edmacs-frames-test--with-fullscreen-frames
        '((fa . ((graphic . t) (fullscreen . maximized))))
      (should (eq (edmacs-frames--fullscreen-target 'fa) 'fullboth)))))

(ert-deftest edmacs-frames-test-fullscreen-target-nil-for-dead-frame ()
  (let ((edmacs-frames-fullscreen 'fullboth))
    (edmacs-frames-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (should-not (edmacs-frames--fullscreen-target 'gone)))))

(ert-deftest edmacs-frames-test-apply-fullscreen-defers-then-sets ()
  "Nothing is set inside the creation hook itself -- a frame is not fully
mapped there, and the NS port drops a fullscreen toggle sent to an
unmapped window -- only from the zero-delay timer."
  (let ((edmacs-frames-fullscreen 'fullboth)
        (deferred nil) (set-calls nil))
    (edmacs-frames-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest _) (setq deferred fn) nil))
                ((symbol-function 'set-frame-parameter)
                 (lambda (f param value) (push (list f param value) set-calls))))
        (edmacs-frames-apply-fullscreen 'fa)
        (should deferred)
        (should-not set-calls)
        (funcall deferred)
        (should (equal set-calls '((fa fullscreen fullboth))))))))

(ert-deftest edmacs-frames-test-apply-fullscreen-schedules-nothing-when-ineligible ()
  (let ((edmacs-frames-fullscreen 'fullboth)
        (scheduled 0))
    (edmacs-frames-test--with-fullscreen-frames
        '((f1 . ((graphic . nil)))
          (fa . ((graphic . t) (fullscreen . fullboth))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq scheduled (1+ scheduled)) nil)))
        (edmacs-frames-apply-fullscreen 'f1)
        (edmacs-frames-apply-fullscreen 'fa)
        (should (= scheduled 0))))))

(ert-deftest edmacs-frames-test-apply-fullscreen-rechecks-target-in-timer ()
  "The frame can be deleted -- or reach the target by another route --
between the creation hook and the timer, so the timer body re-checks
instead of setting a parameter on a frame that no longer qualifies."
  (let ((calls 0) (deferred nil) (set-calls nil))
    (cl-letf (((symbol-function 'edmacs-frames--fullscreen-target)
               (lambda (_frame) (setq calls (1+ calls)) (and (= calls 1) 'fullboth)))
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest _) (setq deferred fn) nil))
              ((symbol-function 'set-frame-parameter)
               (lambda (&rest args) (push args set-calls))))
      (edmacs-frames-apply-fullscreen 'fa)
      (funcall deferred)
      (should (= calls 2))
      (should-not set-calls))))

(ert-deftest edmacs-frames-test-apply-fullscreen-warns-instead-of-signalling ()
  "An error out of the timer body would reach a frameless daemon's top
level, which exits Emacs 255 (see core.el); it is warned about instead."
  (let ((edmacs-frames-fullscreen 'fullboth)
        (deferred nil) (warnings nil))
    (edmacs-frames-test--with-fullscreen-frames '((fa . ((graphic . t))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest _) (setq deferred fn) nil))
                ((symbol-function 'set-frame-parameter)
                 (lambda (&rest _) (error "NS refused the toggle")))
                ((symbol-function 'display-warning)
                 (lambda (&rest args) (push args warnings))))
        (edmacs-frames-apply-fullscreen 'fa)
        (funcall deferred)
        (should (= 1 (length warnings)))
        (should (eq (car (car warnings)) 'edmacs-frames))))))

(ert-deftest edmacs-frames-test-fullscreen-startup-covers-every-live-frame ()
  "`after-make-frame-functions' never fires for a non-daemon Emacs's own
initial frame -- the only frame a plain `emacs' start has -- so the
policy is applied from `emacs-startup-hook' as well."
  (let ((applied nil))
    (edmacs-frames-test--with-fullscreen-frames
        '((fa . ((graphic . t))) (fb . ((graphic . t))))
      (cl-letf (((symbol-function 'edmacs-frames-apply-fullscreen)
                 (lambda (frame) (push frame applied))))
        (edmacs-frames--apply-fullscreen-at-startup)
        (should (equal (nreverse applied) '(fa fb)))))))

(ert-deftest edmacs-frames-test-fullscreen-is-wired-to-both-hooks ()
  (should (memq #'edmacs-frames-apply-fullscreen after-make-frame-functions))
  (should (memq #'edmacs-frames--apply-fullscreen-at-startup emacs-startup-hook)))


;; ============================================================================
;; edmacs-frames--reset-to-spare -- the last-tab reset completes
;; ============================================================================

(ert-deftest edmacs-frames-test-close-last-tab-reset-completes-from-a-side-window ()
  "With the sidebar selected, the old reset ran `switch-to-buffer' against
a dedicated window (which pops into a right side window) and then
`delete-other-windows' from a side window, signalling \"Cannot make side
window the only window\" -- abandoning `edmacs-repo', the frame name and
the tab rename half-done. State is now cleared first and the window reset
targets a repaired main window."
  (let* ((frame (selected-frame))
         (original-name (frame-parameter frame 'name))
         (renamed nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-frame-parameter frame 'edmacs-repo "/repo/.git")
          (set-frame-parameter frame 'name "repo")
          (let ((window (selected-window)))
            (set-window-parameter window 'window-side 'left)
            (set-window-parameter window 'window-slot 0)
            (set-window-dedicated-p window t))
          (cl-letf (((symbol-function 'edmacs-frames--only-frame-p) (lambda (_f) t))
                    ((symbol-function 'edmacs-frames--maybe-teardown-watch-for-frame)
                     #'ignore)
                    ((symbol-function 'tab-bar-rename-tab)
                     (lambda (name &optional _n) (setq renamed name))))
            (edmacs-frames--close-last-tab nil))
          (should-not (frame-parameter frame 'edmacs-repo))
          ;; Clearing `name' hands the frame back to Emacs' own auto-naming
          ;; ("F1", "F2", ...); what matters is that "repo" is gone.
          (should-not (equal (frame-parameter frame 'name) "repo"))
          (should (equal renamed "emacs"))
          (let ((main (edmacs-main-window)))
            (should (window-live-p main))
            (should-not (window-parameter main 'window-side))
            (should-not (window-dedicated-p main))
            (should (equal (buffer-name (window-buffer main)) "*scratch*")))
          ;; The old pair's two failure modes, asserted structurally: a
          ;; non-interactive `switch-to-buffer' on the dedicated sidebar
          ;; popped *scratch* into a RIGHT side window, and the
          ;; `delete-other-windows' that followed ran from a side window.
          (should (zerop (length (seq-filter
                                  (lambda (w)
                                    (eq (window-parameter w 'window-side) 'right))
                                  (window-list frame 'no-minibuf))))))
      (set-frame-parameter frame 'edmacs-repo nil)
      (set-frame-parameter frame 'name original-name))))

(ert-deftest edmacs-frames-test-close-last-tab-clears-state-even-if-windows-fail ()
  "The state clearing must not be hostage to the window work: a signalling
repair is warned about, not propagated, and the three frame-level
resets have already landed by then."
  (let* ((frame (selected-frame))
         (original-name (frame-parameter frame 'name))
         (renamed nil))
    (unwind-protect
        (save-window-excursion
          (set-frame-parameter frame 'edmacs-repo "/repo/.git")
          (set-frame-parameter frame 'name "repo")
          (cl-letf (((symbol-function 'edmacs-frames--only-frame-p) (lambda (_f) t))
                    ((symbol-function 'edmacs-frames--maybe-teardown-watch-for-frame)
                     #'ignore)
                    ((symbol-function 'tab-bar-rename-tab)
                     (lambda (name &optional _n) (setq renamed name)))
                    ((symbol-function 'edmacs-windows-repair-frame)
                     (lambda (&optional _frame) (error "boom")))
                    ((symbol-function 'display-warning) #'ignore))
            (edmacs-frames--close-last-tab nil))
          (should-not (frame-parameter frame 'edmacs-repo))
          (should-not (equal (frame-parameter frame 'name) "repo"))
          (should (equal renamed "emacs")))
      (set-frame-parameter frame 'edmacs-repo nil)
      (set-frame-parameter frame 'name original-name))))

(ert-deftest edmacs-frames-test-reset-to-spare-warns-instead-of-propagating ()
  "The `condition-case' around the window work must report through
`display-warning' rather than let the signal escape into
`tab-bar-close-last-tab-choice', and the three frame-level resets ahead
of it must already have landed by the time it fires."
  (let* ((frame (selected-frame))
         (original-name (frame-parameter frame 'name))
         (renamed nil)
         (warnings nil))
    (unwind-protect
        (save-window-excursion
          (set-frame-parameter frame 'edmacs-repo "/repo/.git")
          (set-frame-parameter frame 'name "repo")
          (cl-letf (((symbol-function 'tab-bar-rename-tab)
                     (lambda (name &optional _n) (setq renamed name)))
                    ((symbol-function 'edmacs-windows-repair-frame)
                     (lambda (&optional _frame) (error "boom")))
                    ((symbol-function 'display-warning)
                     (lambda (type message &optional _level &rest _)
                       (push (cons type message) warnings))))
            (edmacs-frames--reset-to-spare frame))
          (should (= (length warnings) 1))
          (should (eq (car (car warnings)) 'edmacs-frames))
          (should (string-match-p "reset-to-spare" (cdr (car warnings))))
          (should (string-match-p "boom" (cdr (car warnings))))
          (should-not (frame-parameter frame 'edmacs-repo))
          (should-not (equal (frame-parameter frame 'name) "repo"))
          (should (equal renamed "emacs")))
      (set-frame-parameter frame 'edmacs-repo nil)
      (set-frame-parameter frame 'name original-name))))

;; ============================================================================
;; edmacs-frames--current-frame-common
;; ============================================================================

(ert-deftest edmacs-frames-test-current-frame-common-ignores-ambient-current-buffer ()
  "The no-`edmacs-repo' fallback in `edmacs-frames--current-frame-common'
must derive FRAME's own content-window buffer's `default-directory' via
`buffer-local-value', never plain ambient `default-directory' -- the same
bug class the sibling `edmacs-sessions--tab-name-for-frame' fix targets
(see `edmacs-sessions-test-tab-name-for-frame-ignores-ambient-current-buffer'
in sessions-test.el). Proven here by asserting `edmacs-frames--repo-of' is
called with FRAME's own buffer's directory even while a different
directory's buffer is ambiently current."
  (let* ((frame (selected-frame))
         (saved-repo (frame-parameter frame 'edmacs-repo))
         (frame-dir (file-name-as-directory
                     (make-temp-file "edmacs-frames-test-framedir-" t)))
         (ambient-dir (file-name-as-directory
                       (make-temp-file "edmacs-frames-test-ambientdir-" t)))
         (frame-buf (generate-new-buffer "edmacs-frames-test-frame-buf"))
         (ambient-buf (generate-new-buffer "edmacs-frames-test-ambient-buf"))
         (dirs-queried nil))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'edmacs-repo nil)
          (with-current-buffer frame-buf (setq default-directory frame-dir))
          (with-current-buffer ambient-buf (setq default-directory ambient-dir))
          (set-window-buffer (frame-selected-window frame) frame-buf)
          (with-current-buffer ambient-buf
            (cl-letf (((symbol-function 'edmacs-frames--frame-content-window)
                       (lambda (f) (frame-selected-window f)))
                      ((symbol-function 'edmacs-frames--repo-of)
                       (lambda (dir) (push dir dirs-queried) "resolved-common")))
              (should (equal (edmacs-frames--current-frame-common frame)
                              "resolved-common"))
              (should (equal dirs-queried (list frame-dir))))))
      (set-frame-parameter frame 'edmacs-repo saved-repo)
      (delete-directory frame-dir t)
      (delete-directory ambient-dir t)
      (kill-buffer frame-buf)
      (kill-buffer ambient-buf))))

(ert-deftest edmacs-frames-test-current-frame-common-prefers-stamped-repo ()
  "A frame already claimed by `edmacs-frames-open' answers from its own
`edmacs-repo' parameter and never falls back to a buffer derivation at
all -- the buffer-derivation branch is for the unclaimed-frame case only."
  (let* ((frame (selected-frame))
         (saved-repo (frame-parameter frame 'edmacs-repo)))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'edmacs-repo "/stamped/.git")
          (cl-letf (((symbol-function 'edmacs-frames--repo-of)
                     (lambda (&rest _) (error "should not derive from a buffer"))))
            (should (equal (edmacs-frames--current-frame-common frame)
                            "/stamped/.git"))))
      (set-frame-parameter frame 'edmacs-repo saved-repo))))

;; ============================================================================
;; edmacs-frames--read-worktree
;; ============================================================================
;; `edmacs-frames-open-worktree-tab's every existing test (frames-live-
;; test.el) calls it as a plain function with an explicit DIR, bypassing its
;; `(interactive (list (edmacs-frames--read-worktree current-prefix-arg
;; (selected-frame))))' spec -- and `edmacs-frames--read-worktree' itself --
;; entirely. These prove the interactive path's own FRAME plumbing directly.

(ert-deftest edmacs-frames-test-read-worktree-prefix-skips-frame-lookup ()
  "A PREFIX arg falls straight to `read-directory-name', never consulting
FRAME (or `edmacs-frames--current-frame-common') at all."
  (cl-letf (((symbol-function 'edmacs-frames--current-frame-common)
             (lambda (&rest _) (error "should not consult FRAME under a prefix arg")))
            ((symbol-function 'read-directory-name)
             (lambda (&rest _) "/picked/")))
    (should (equal (edmacs-frames--read-worktree t (selected-frame)) "/picked/"))))

(ert-deftest edmacs-frames-test-read-worktree-no-common-falls-back ()
  "No PREFIX, but FRAME has no resolvable repo (an unclaimed frame with no
project-bearing buffer either): fall back to `read-directory-name', still
without ever calling `completing-read'."
  (cl-letf (((symbol-function 'edmacs-frames--current-frame-common)
             (lambda (frame) (should (eq frame (selected-frame))) nil))
            ((symbol-function 'completing-read)
             (lambda (&rest _) (error "should not offer a worktree list with no common dir")))
            ((symbol-function 'read-directory-name)
             (lambda (&rest _) "/picked/")))
    (should (equal (edmacs-frames--read-worktree nil (selected-frame)) "/picked/"))))

(ert-deftest edmacs-frames-test-read-worktree-offers-frames-own-worktrees ()
  "No PREFIX, FRAME resolves a common dir: `completing-read' is offered
FRAME's OWN worktrees (via `edmacs-frames--repo-worktrees'), proving the
FRAME argument -- not an ambient selected-frame read -- drives the lookup."
  (let ((other-frame 'a-different-frame))
    (cl-letf (((symbol-function 'edmacs-frames--current-frame-common)
               (lambda (frame) (should (eq frame other-frame)) "/repo/.git"))
              ((symbol-function 'edmacs-frames--repo-worktrees)
               (lambda (common) (should (equal common "/repo/.git")) '("/wt-a/" "/wt-b/")))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (equal collection '("/wt-a/" "/wt-b/")))
                 "/wt-a/")))
      (should (equal (edmacs-frames--read-worktree nil other-frame) "/wt-a/")))))

(provide 'frames-test)
;;; frames-test.el ends here
