;;; frames-test.el --- Tests for frames.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage only -- no extra frame is created here (that
;; needs a controlling terminal; see frames-live-test.el). Frame-list
;; scanning helpers are exercised by stubbing `frame-list'/`frame-live-p'/
;; `frame-parameter' via `cl-letf' rather than spawning real frames, so
;; this suite runs under plain `-Q --batch' with no pty.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el -l modules/frames.el \
;;         -l modules/frames-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

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

(defmacro edmacs-frames-test--with-fake-frames (alist &rest body)
  "Run BODY with frame-scanning primitives faked from ALIST.
ALIST is a list of (FRAME-SYMBOL . PARAMS-ALIST); a PARAMS-ALIST entry
for `visible' controls `frame-visible-p' and defaults to t when absent,
so existing callers that never mention visibility keep behaving as a
plain live, visible frame."
  (declare (indent 1))
  `(let ((frames--alist ,alist))
     (cl-letf (((symbol-function 'frame-list)
                (lambda () (mapcar #'car frames--alist)))
               ((symbol-function 'frame-live-p)
                (lambda (f) (assq f frames--alist)))
               ((symbol-function 'frame-visible-p)
                (lambda (f)
                  (let ((params (cdr (assq f frames--alist))))
                    (if (assq 'visible params) (alist-get 'visible params) t))))
               ((symbol-function 'frame-parameter)
                (lambda (f param)
                  (if f
                      (alist-get param (cdr (assq f frames--alist)))
                    (alist-get param (cdr (assq (selected-frame) frames--alist)))))))
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
          (should (edmacs-frames-tab-in-own-repo-p "/repo/.git"))
          (should-not (edmacs-frames-tab-in-own-repo-p "/other/.git"))
          (should-not (edmacs-frames-tab-in-own-repo-p nil)))
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
;; edmacs-frames--ws-selected-buffer-name / --tab-root
;; ============================================================================

(ert-deftest edmacs-frames-test-ws-selected-buffer-name-single-leaf ()
  (should (equal (edmacs-frames--ws-selected-buffer-name
                  '(leaf (buffer "*foo*" (selected . t))))
                 "*foo*")))

(ert-deftest edmacs-frames-test-ws-selected-buffer-name-picks-selected-leaf ()
  (should (equal (edmacs-frames--ws-selected-buffer-name
                  '(hc (leaf (buffer "*a*"))
                       (leaf (buffer "*b*" (selected . t)))))
                 "*b*")))

(ert-deftest edmacs-frames-test-ws-selected-buffer-name-falls-back-to-first ()
  (should (equal (edmacs-frames--ws-selected-buffer-name
                  '(vc (leaf (buffer "*a*"))
                       (leaf (buffer "*b*"))))
                 "*a*")))

(ert-deftest edmacs-frames-test-tab-root-stored-property-wins ()
  (let ((tab '(tab (edmacs-root . "/stored/root/"))))
    (should (equal (edmacs-frames--tab-root tab) "/stored/root/"))))

(ert-deftest edmacs-frames-test-tab-root-derives-and-stamps-current-tab ()
  (let ((buf (generate-new-buffer "*frames-test-tab-root*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (setq default-directory "/derived/root/"))
          (cl-letf (((symbol-function 'window-buffer)
                     (lambda (&rest _) buf)))
            (let ((tab (list 'current-tab)))
              (should (equal (edmacs-frames--tab-root tab) "/derived/root/"))
              ;; Stamped in place for next time -- no re-derivation needed.
              (should (equal (alist-get 'edmacs-root (cdr tab)) "/derived/root/")))))
      (kill-buffer buf))))

(ert-deftest edmacs-frames-test-tab-root-nil-when-buffer-gone ()
  (let ((tab '(tab (ws (leaf (buffer "*frames-test-nonexistent*"))))))
    (should-not (edmacs-frames--tab-root tab))))

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

(provide 'frames-test)
;;; frames-test.el ends here
