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
;;
;; Two tests here need a REAL graphical frame -- they drive the daemon's
;; own restore bridge and count graphical frames afterward, which a batch
;; frame cannot answer. They `ert-skip' in batch; run them with:
;;
;;   scripts/gui-ert.sh modules/sessions-live-test.el
;;
;; from the MAIN checkout (never a worktree: see CLAUDE.md on
;; `--init-directory'). That script starts a throwaway daemon of its own
;; and never touches the user's.
;;
;; None of this exercises the launchd daemon, the Dock, or NS hide/show --
;; those need a real GUI login session and are not ERT-testable. The
;; daemon-and-Dock-frame roadmap phase's AC4 is the manual checklist that
;; covers them (daemon-starts-at-login, close-hides-instead-of-quits,
;; Dock-click-restores, `emacsclient -c' behavior, and restart-restores-
;; session); see that phase for the pass/fail steps.

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
    ;; The new model, and what `edmacs-sessions--stash-frameset-for-daemon'
    ;; now puts the desktop frameset through before stashing it.
    (load (expand-file-name "modules/workspaces.el" default-directory) nil t)
    (load (expand-file-name "modules/sessions.el" default-directory) nil t)

    ;; workspaces.el installs a `window-buffer-change-functions' entry that
    ;; schedules a stray-visit sweep on a zero-delay timer, and this suite
    ;; both changes window buffers and waits on timers -- a sweep firing
    ;; between a test's setup and its assertions would move the very windows
    ;; under test. Its own kill switch turns off both the scheduler and the
    ;; sweep; workspaces-test.el covers that machinery directly.
    (setq edmacs-workspaces-stray-visit-relocate nil)

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
                (edmacs-sessions--finish-frameset-restore frame))

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
            (edmacs-sessions--finish-frameset-restore frame))
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


    ;; ========================================================================
    ;; AC1 -- groups, per-tab roots and the selected tab survive a real
    ;; frameset save/restore
    ;; ========================================================================

    (defmacro edmacs-sessions-live-test--with-scratch-tabs (frame &rest body)
      "Run BODY on FRAME with a fresh single-tab tab list, restored afterward."
      (declare (indent 1))
      (let ((f (gensym "frame")) (saved (gensym "tabs")))
        `(let* ((,f ,frame)
                (,saved (frame-parameter ,f 'tabs)))
           (unwind-protect
               (progn
                 (set-frame-parameter ,f 'tabs nil)
                 (tab-bar-tabs ,f)
                 ,@body)
             (set-frame-parameter ,f 'tabs ,saved)))))

    (defun edmacs-sessions-live-test--build-workspace-tabs (frame)
      "Build three grouped, rooted tabs on FRAME and return their (GROUP . ROOT)s.
Two projects, one of them with two worktrees -- the shape a migrated
desktop produces. The roots are synthetic: nothing here resolves them
through git, they are only the `equal'-compared keys
`edmacs-workspaces-find-tab' matches on."
      (let ((specs '(("edmacs" . "/w/edmacs/")
                     ("edmacs" . "/w/edmacs__worktrees/roadmap-x/")
                     ("cloudcitydotgay" . "/w/cloudcitydotgay/"))))
        (with-selected-frame frame
          (let ((first t))
            (dolist (spec specs)
              (unless first (tab-bar-new-tab))
              (setq first nil)
              (edmacs-workspaces-set-tab-root (cdr spec) frame)
              (edmacs-workspaces-assign-group (car spec) nil frame))))
        specs))

    (ert-deftest edmacs-sessions-live-test-frameset-round-trips-groups-and-roots ()
      "AC1: every project group, every worktree tab within it, each tab's
`edmacs-workspace-root' and the previously selected tab all come back
from a real `frameset-save'/`desktop-restore-frameset' round trip.

Two upstream facts carry this and are pinned here rather than argued:
`frameset-filter-tabs' strips only the `wc-*' keys on save, so `group'
and a custom root parameter survive; and the `group' parameter needs no
stamping of this module's own -- tab-bar persists it itself."
      (let ((frame (selected-frame)))
        (edmacs-sessions-live-test--with-scratch-tabs frame
          (let* ((specs (edmacs-sessions-live-test--build-workspace-tabs frame))
                 (selected (nth 1 specs)))
            (edmacs-workspaces-select-tab (car selected) (cdr selected) frame)
            (let ((name (alist-get 'name (cdr (tab-bar--current-tab-find nil frame)))))
              (let ((desktop-saved-frameset (frameset-save (list frame)))
                    (desktop-restore-frames t)
                    (desktop-restore-reuses-frames t))
                ;; Genuinely gone before the restore, so nothing below can
                ;; pass by simply never having been cleared.
                (set-frame-parameter frame 'tabs nil)
                (with-selected-frame frame (desktop-restore-frameset)))
              (should (frame-live-p frame))
              (should (equal (sort (copy-sequence (edmacs-workspaces-groups frame))
                                   #'string<)
                             '("cloudcitydotgay" "edmacs")))
              (should (= 2 (length (edmacs-workspaces-tabs-in-group "edmacs" frame))))
              (dolist (spec specs)
                (should (edmacs-workspaces-find-tab (car spec) (cdr spec) frame)))
              (should (equal (alist-get 'name (cdr (tab-bar--current-tab-find nil frame)))
                             name)))))))

    (ert-deftest edmacs-sessions-live-test-migrated-frameset-restores-groups-and-roots ()
      "The same assertions over a frameset that went through the real
migration first: two frames-model states in, one frame with both
projects' tabs out. This is the desktop half of AC1/AC2 end to end --
`workspaces-test.el' proves the transform, this proves Emacs restores
what the transform produced."
      (let ((frame (selected-frame)))
        (edmacs-sessions-live-test--with-scratch-tabs frame
          (let* ((saved
                  (progn
                    (with-selected-frame frame
                      (edmacs-workspaces-set-tab-root "/w/edmacs/" frame))
                    ;; A frames-model desktop: the root under the LEGACY
                    ;; parameter, no group anywhere, one frame per repo.
                    (setf (alist-get 'edmacs-workspace-root
                                     (cdr (tab-bar--current-tab-find nil frame))
                                     nil t)
                          nil)
                    (setf (alist-get 'edmacs-root
                                     (cdr (tab-bar--current-tab-find nil frame)))
                          "/w/edmacs/")
                    (frameset-save (list frame))))
                 (state (car (frameset-states saved)))
                 (other (cons (append '((frameset--id . "1111-2222-3333-4444")
                                        (tabs (current-tab (edmacs-root . "/w/cloud/")
                                                           (name . "cloud")
                                                           (explicit-name . t))))
                                      (seq-remove
                                       (lambda (cell)
                                         (memq (car-safe cell) '(frameset--id tabs)))
                                       (car state)))
                              (cdr state)))
                 (fs (progn (setf (frameset-states saved)
                                  (list state other))
                            saved))
                 (migrated
                  (cl-letf (((symbol-function 'edmacs-workspaces-group-name)
                             (lambda (root)
                               (if (string-prefix-p "/w/edmacs" root) "edmacs" "cloud"))))
                    (edmacs-workspaces-migrate-frameset fs))))
            (should (= 1 (length (frameset-states migrated))))
            (let ((desktop-saved-frameset migrated)
                  (desktop-restore-frames t)
                  (desktop-restore-reuses-frames t))
              (set-frame-parameter frame 'tabs nil)
              (with-selected-frame frame (desktop-restore-frameset)))
            (should (frame-live-p frame))
            (should (equal (sort (copy-sequence (edmacs-workspaces-groups frame)) #'string<)
                           '("cloud" "edmacs")))
            (should (edmacs-workspaces-find-tab "edmacs" "/w/edmacs/" frame))
            (should (edmacs-workspaces-find-tab "cloud" "/w/cloud/" frame))
            ;; The legacy parameter is gone from every restored tab.
            (should-not (seq-some (lambda (tab) (alist-get 'edmacs-root (cdr tab)))
                                  (tab-bar-tabs frame)))))))

    ;; ========================================================================
    ;; AC5 -- bufferlo already scopes buffers per TAB, with no configuration
    ;; ========================================================================

    (ert-deftest edmacs-sessions-live-test-bufferlo-scopes-buffers-per-tab ()
      "Verified, not configured: the bare `(bufferlo-mode 1)' sessions.el
already enables scopes the buffer list per TAB, so one frame holding
every project is as isolated as one frame per project was. bufferlo does
this through `tab-bar-tab-post-open-functions' and its own advice on
`tab-bar-select-tab'; nothing in it is frame-per-repo specific."
      (let ((dir (expand-file-name "bufferlo" edmacs-sessions-live-test--build-root)))
        (unless (file-directory-p dir)
          (ert-skip "bufferlo's straight build was not found in this checkout \
or its sibling main checkout"))
        (add-to-list 'load-path dir)
        (require 'bufferlo)
        (let ((frame (selected-frame))
              (a (generate-new-buffer "edmacs-live-bufferlo-a"))
              (b (generate-new-buffer "edmacs-live-bufferlo-b")))
          (unwind-protect
              (edmacs-sessions-live-test--with-scratch-tabs frame
                (bufferlo-mode 1)
                (with-selected-frame frame
                  (delete-other-windows)
                  ;; Both tabs start from the same neutral buffer: a new tab
                  ;; inherits the one it was created from, so opening each
                  ;; test buffer AFTER both tabs exist is what makes the
                  ;; assertions below about scope rather than about creation
                  ;; order.
                  (switch-to-buffer (get-buffer-create "*scratch*"))
                  (tab-bar-new-tab)
                  (switch-to-buffer b)
                  (tab-bar-select-tab 1)
                  (switch-to-buffer a)
                  ;; A buffer opened in one tab does not appear in the other's.
                  (should (memq a (bufferlo-buffer-list frame)))
                  (should-not (memq b (bufferlo-buffer-list frame)))
                  (tab-bar-select-tab 2)
                  (should (memq b (bufferlo-buffer-list frame)))
                  (should-not (memq a (bufferlo-buffer-list frame)))
                  ;; Switching back restores the first tab's list.
                  (tab-bar-select-tab 1)
                  (should (memq a (bufferlo-buffer-list frame)))
                  (should-not (memq b (bufferlo-buffer-list frame)))))
            (bufferlo-mode -1)
            (when (buffer-live-p a) (kill-buffer a))
            (when (buffer-live-p b) (kill-buffer b))))))

    ;; ========================================================================
    ;; AC4 -- GUI-only: the real bridge, on a real graphical frame
    ;; ========================================================================

    (defun edmacs-sessions-live-test--skip-unless-graphic ()
      "Skip unless this Emacs has a real graphical frame.
`emacs --batch' has no window system at all, so `display-graphic-p' is
nil for every frame and neither `frameset-restore''s frame reuse nor a
count of graphical frames means anything there."
      (unless (display-graphic-p)
        (ert-skip "needs a real graphical frame: run \
`scripts/gui-ert.sh modules/sessions-live-test.el' from the main checkout")))

    (defmacro edmacs-sessions-live-test--drive-bridge (frameset &rest body)
      "Stash FRAMESET, run the real restore bridge on the selected frame, BODY.
The bridge defers its work onto a zero-delay timer, so the `sit-for'
below is what actually runs it; the two steps with an external
dependency -- the sidebar (needs magit-section) and the worktree watch
\(arms a real `file-notify') -- are stubbed for the duration, exactly as
the non-GUI tests above stub them."
      (declare (indent 1))
      `(let ((edmacs-sessions--pending-frameset ,frameset))
         (cl-letf (((symbol-function 'edmacs-sessions--ensure-sidebar) #'ignore)
                   ((symbol-function 'edmacs-sessions--ensure-worktree-tracking)
                    #'ignore))
           (edmacs-sessions--restore-pending-frameset (selected-frame))
           (sit-for 0.3)
           ,@body)))

    (ert-deftest edmacs-sessions-live-test-bridge-restores-into-one-gui-frame ()
      "AC4: a migrated frameset holds ONE state, so `frameset-restore'
reuses the frame it was handed and creates no second one. The frame
count is the whole point of this test, and it is unfalsifiable in batch
-- where `display-graphic-p' is nil for every frame, so the count is
always zero."
      (edmacs-sessions-live-test--skip-unless-graphic)
      (let ((frame (selected-frame)))
        (edmacs-sessions-live-test--with-restored-frame frame
          (edmacs-sessions-live-test--with-scratch-tabs frame
            (edmacs-workspaces-set-tab-root "/w/edmacs/" frame)
            (edmacs-workspaces-assign-group "edmacs" nil frame)
            (let ((migrated (edmacs-workspaces-migrate-frameset
                             (frameset-save (list frame)))))
              (edmacs-sessions-live-test--drive-bridge migrated
                (should (= 1 (seq-count (lambda (f)
                                          (and (frame-live-p f) (display-graphic-p f)))
                                        (frame-list))))
                (should (frame-live-p frame))
                (should (edmacs-workspaces-find-tab "edmacs" "/w/edmacs/" frame))))))))

    (ert-deftest edmacs-sessions-live-test-bridge-restores-a-selectable-folded-tab ()
      "A tab folded out of a second frame carries that frame's whole window
state as its `ws', and `tab-bar-select-tab' puts it back -- a restored
tab has no live `wc', so `ws' is the only layout it has. Selecting one
must yield real windows, not an empty frame."
      (edmacs-sessions-live-test--skip-unless-graphic)
      (let ((frame (selected-frame)))
        (edmacs-sessions-live-test--with-restored-frame frame
          (edmacs-sessions-live-test--with-scratch-tabs frame
            (edmacs-workspaces-set-tab-root "/w/edmacs/" frame)
            (edmacs-workspaces-assign-group "edmacs" nil frame)
            (let* ((saved (frameset-save (list frame)))
                   (state (car (frameset-states saved)))
                   (other (cons (append '((frameset--id . "1111-2222-3333-4444")
                                          (tabs (current-tab
                                                 (edmacs-workspace-root . "/w/cloud/")
                                                 (group . "cloud")
                                                 (name . "cloud")
                                                 (explicit-name . t))))
                                        (seq-remove
                                         (lambda (cell)
                                           (memq (car-safe cell) '(frameset--id tabs)))
                                         (car state)))
                                (cdr state)))
                   (migrated (progn (setf (frameset-states saved) (list state other))
                                    (edmacs-workspaces-migrate-frameset saved))))
              (edmacs-sessions-live-test--drive-bridge migrated
                (should (= 1 (seq-count (lambda (f)
                                          (and (frame-live-p f) (display-graphic-p f)))
                                        (frame-list))))
                (let ((folded (edmacs-workspaces-select-tab "cloud" "/w/cloud/" frame)))
                  (should folded)
                  (should (equal (edmacs-workspaces-tab-root
                                  (tab-bar--current-tab-find nil frame))
                                 "/w/cloud/"))
                  (should (window-live-p (frame-selected-window frame))))))))))

    (provide 'sessions-live-test)))
;;; sessions-live-test.el ends here
