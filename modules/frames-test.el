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
  "Run BODY with `frame-list'/`frame-live-p'/`frame-parameter' faked from ALIST.
ALIST is a list of (FRAME-SYMBOL . PARAMS-ALIST)."
  (declare (indent 1))
  `(let ((frames--alist ,alist))
     (cl-letf (((symbol-function 'frame-list)
                (lambda () (mapcar #'car frames--alist)))
               ((symbol-function 'frame-live-p)
                (lambda (f) (assq f frames--alist)))
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

(provide 'frames-test)
;;; frames-test.el ends here
