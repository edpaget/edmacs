;;; sessions-live-test.el --- sessions.el tests needing a real frameset -*- lexical-binding: t -*-

;;; Commentary:
;; sessions-test.el covers `edmacs-sessions--finish-frameset-restore' with
;; every per-frame step stubbed, which pins the walk's eligibility gate and
;; its call ORDER but proves nothing about what a restored tab ends up
;; carrying. This suite closes that gap: it drives a REAL
;; `desktop-restore-frameset' over a real `frameset-save', then runs the
;; walk with the real `edmacs-frames-stamp-frame-tabs' and asserts the tab
;; comes back with a real `edmacs-root' -- and that the back-fill which
;; reads those roots then resolves the frame's `edmacs-repo' from them.
;; That whole chain is the desktop half of "every tab-creating path stamps
;; `edmacs-root'"; the plain `tab-bar-new-tab' half lives in
;; frames-live-test.el.
;;
;; Unlike sessions-test.el this loads the REAL frames.el (and windows.el,
;; which frames.el's healthy-frame predicate calls into), because the
;; function under test is precisely the frames.el/sessions.el seam. Only
;; the two steps with an external dependency are stubbed:
;; `edmacs-sessions--ensure-sidebar' (needs magit-section) and
;; `edmacs-sessions--ensure-worktree-tracking' (arms a real `file-notify'
;; watch). Stamping, back-fill and title regeneration all run for real.
;;
;; No frame is created here. `frameset-restore' reuses the ambient frame
;; every Emacs has, even under `-Q --batch' -- which is also why every
;; frame parameter and tab this suite touches is restored afterward.
;;
;; sessions.el cannot be `-l'-loaded standalone (see sessions-test.el's own
;; Commentary): `general' comes off this checkout's -- or its sibling main
;; checkout's -- `straight/build', and `edmacs-evil-config-add-c-x-chord'
;; is stubbed. One benign "Cannot load bufferlo" notice is expected.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/sessions-live-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)
(require 'desktop)
(require 'frameset)
(require 'tab-bar)

;; Same native-comp-trampoline hazard sessions-test.el disables this for.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defun edmacs-sessions-live-test--locate-straight-build-root ()
  "Return this checkout's (or its sibling main checkout's) `straight/build'.
Same worktree-vs-sibling-main-checkout fallback as sessions-test.el's."
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

(defvar edmacs-sessions-live-test--build-root
  (edmacs-sessions-live-test--locate-straight-build-root))

(if (null edmacs-sessions-live-test--build-root)

    (ert-deftest edmacs-sessions-live-test-general-unavailable ()
      (ert-skip "general's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (add-to-list 'load-path
                 (expand-file-name "general" edmacs-sessions-live-test--build-root))
    (require 'general)

    (unless (fboundp 'edmacs-evil-config-add-c-x-chord)
      (defun edmacs-evil-config-add-c-x-chord (&rest _)
        "Stub for tests: sessions.el's real target lives in evil-config.el."
        nil))

    ;; windows.el before frames.el: `edmacs-frames--frame-healthy-p' calls
    ;; `edmacs-windows-frame-wedged-p', and an absent one would silently
    ;; make every frame look healthy here.
    (load (expand-file-name "modules/windows.el" default-directory) nil t)
    (load (expand-file-name "modules/frames.el" default-directory) nil t)
    (load (expand-file-name "modules/sessions.el" default-directory) nil t)

    ;; ========================================================================
    ;; Test helpers
    ;; ========================================================================

    (defun edmacs-sessions-live-test--make-git-repo (dir)
      "Init a real, one-commit git repo at DIR."
      (make-directory dir t)
      (let ((default-directory (file-name-as-directory dir)))
        (call-process "git" nil nil nil "init" "-q")
        (call-process "git" nil nil nil "config" "user.email" "t@t.com")
        (call-process "git" nil nil nil "config" "user.name" "t")
        (write-region "x" nil (expand-file-name "f.txt" dir))
        (call-process "git" nil nil nil "add" "-A")
        (call-process "git" nil nil nil "commit" "-q" "-m" "init")))

    (defmacro edmacs-sessions-live-test--with-restored-frame (frame &rest body)
      "Run BODY, then put FRAME back the way this suite found it.
The ambient frame is shared with every other test in this process, so its
repo parameters, its tab list and its current tab's own `edmacs-root'
stamp are all rolled back. The stamp is restored separately because tab
parameters are mutated in the cons the frame already holds -- putting the
saved tab LIST back would hand over the very conses BODY just wrote to."
      (declare (indent 1))
      (let ((f (gensym "frame")) (tabs (gensym "tabs")) (root (gensym "root")))
        `(let* ((,f ,frame)
                (,tabs (frame-parameter ,f 'tabs))
                (,root (edmacs-frames--tab-root
                        (tab-bar--current-tab-find nil ,f))))
           (unwind-protect
               (progn ,@body)
             (dolist (p '(edmacs-repo edmacs-repo-missing name))
               (set-frame-parameter ,f p nil))
             (set-frame-parameter ,f 'tabs ,tabs)
             ;; A nil ROOT removes the entry outright rather than storing nil,
             ;; so an unstamped tab goes back to being unstamped.
             (setf (alist-get 'edmacs-root
                              (cdr (tab-bar--current-tab-find nil ,f))
                              nil t)
                   ,root)))))

    ;; ========================================================================
    ;; The desktop half of "every tab-creating path stamps `edmacs-root'"
    ;; ========================================================================

    (ert-deftest edmacs-sessions-live-test-desktop-restore-stamps-tab-root ()
      "A tab restored from a real frameset comes back stamped, and the
back-fill then reads that stamp.

The regression this pins is the whole reason the walk stamps FIRST: a
desktop file written before the stamp was mandatory carries no
`edmacs-root' at all, `edmacs-frames--tab-root' is now a pure read that
will not guess one, and `edmacs-sessions--frame-tab-roots' bails on the
first unresolved tab -- so without `edmacs-frames-stamp-frame-tabs'
running ahead of `edmacs-sessions--backfill-repo-param' such a frame
would silently decline back-fill forever. Every step here is the real
function except the sidebar (needs magit-section) and the worktree watch
(arms a real `file-notify')."
      ;; `file-truename' up front: the derived root is a truename, and on
      ;; macOS `make-temp-file' hands back the /var symlink to /private/var.
      (let* ((sandbox (file-name-as-directory
                       (file-truename (make-temp-file "edmacs-sessions-live" t))))
             (repo (file-name-as-directory (expand-file-name "repo" sandbox)))
             (frame (selected-frame))
             (buffer (generate-new-buffer "edmacs-sessions-live-content")))
        (unwind-protect
            (edmacs-sessions-live-test--with-restored-frame frame
              (edmacs-sessions-live-test--make-git-repo repo)
              (with-current-buffer buffer (setq default-directory repo))
              (with-selected-frame frame
                (delete-other-windows)
                (set-window-buffer (frame-selected-window frame) buffer))
              ;; A desktop file written before the stamp was mandatory.
              (setf (alist-get 'edmacs-root
                               (cdr (tab-bar--current-tab-find nil frame))
                               nil t)
                    nil)
              (set-frame-parameter frame 'edmacs-repo nil)

              ;; The real round trip the daemon's restore timer performs.
              (let ((desktop-saved-frameset (frameset-save (list frame)))
                    (desktop-restore-frames t)
                    (desktop-restore-reuses-frames t))
                (with-selected-frame frame (desktop-restore-frameset)))
              (should (frame-live-p frame))

              ;; Nothing derives identity any more, so the restored tab is
              ;; genuinely unstamped at this point -- if it were not, the
              ;; assertion below would pass for the wrong reason.
              (should-not (edmacs-frames--tab-root
                           (tab-bar--current-tab-find nil frame)))

              (cl-letf (((symbol-function 'edmacs-sessions--ensure-sidebar)
                         #'ignore)
                        ((symbol-function
                          'edmacs-sessions--ensure-worktree-tracking)
                         #'ignore))
                (edmacs-sessions--finish-frameset-restore))

              (should (equal (edmacs-frames--tab-root
                              (tab-bar--current-tab-find nil frame))
                             (file-truename repo)))
              ;; Exactly one stamp: `tab-bar--tab' copies unrecognized tab
              ;; parameters forward on every switch, so a shadowed second
              ;; cons would outlive the session and be re-persisted.
              (should (= 1 (seq-count (lambda (cell) (eq (car-safe cell) 'edmacs-root))
                                      (cdr (tab-bar--current-tab-find nil frame)))))
              ;; Stamp-first is load-bearing, not cosmetic: the back-fill
              ;; resolved the frame's repo from the root written above.
              (should (equal (frame-parameter frame 'edmacs-repo)
                             (edmacs-frames--repo-of repo))))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (delete-directory sandbox t))))

    (ert-deftest edmacs-sessions-live-test-restore-walk-skips-an-unusable-frame ()
      "The gate holds against the real predicate, not just a stubbed one.
`edmacs-frames-frame-usable-p' is loaded for real here, so this is the
end-to-end form of sessions-test.el's stubbed placeholder test: with the
ambient frame reported as the daemon's initial one, the walk must leave
it entirely alone."
      (let ((frame (selected-frame)))
        (edmacs-sessions-live-test--with-restored-frame frame
          (set-frame-parameter frame 'edmacs-repo nil)
          (setf (alist-get 'edmacs-root
                           (cdr (tab-bar--current-tab-find nil frame))
                           nil t)
                nil)
          (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) t))
                    ((symbol-function 'frame-initial-p) (lambda (_f) t))
                    ((symbol-function 'edmacs-sessions--ensure-sidebar) #'ignore)
                    ((symbol-function 'edmacs-sessions--ensure-worktree-tracking)
                     #'ignore))
            (should-not (edmacs-sessions--restorable-frame-p frame))
            (edmacs-sessions--finish-frameset-restore))
          (should-not (edmacs-frames--tab-root
                       (tab-bar--current-tab-find nil frame)))
          (should-not (frame-parameter frame 'edmacs-repo)))))

    ;; ========================================================================
    ;; edmacs-sidebar-polish -- `edmacs-sidebar-collapsed' round-trips a real
    ;; frameset save/restore, exactly like `edmacs-repo' above
    ;; ========================================================================

    (ert-deftest edmacs-sessions-live-test-frameset-round-trips-sidebar-collapsed ()
      "`edmacs-sidebar-collapsed' is pinned into `frameset-filter-alist'
alongside `edmacs-repo' (see sessions.el) with the same pass-through
\(non-`:never') action, so it must survive the same real
`frameset-save'/`desktop-restore-frameset' round trip `edmacs-repo'
already does -- unlike the colour/geometry parameters this file
deliberately drops on restore."
      (let ((frame (selected-frame)))
        (edmacs-sessions-live-test--with-restored-frame frame
          (unwind-protect
              (progn
                (set-frame-parameter frame 'edmacs-sidebar-collapsed t)
                (let ((desktop-saved-frameset (frameset-save (list frame)))
                      (desktop-restore-frames t)
                      (desktop-restore-reuses-frames t))
                  (with-selected-frame frame (desktop-restore-frameset)))
                (should (frame-live-p frame))
                (should (frame-parameter frame 'edmacs-sidebar-collapsed)))
            (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)))))

    (provide 'sessions-live-test)))
;;; sessions-live-test.el ends here
