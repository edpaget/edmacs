;;; window-geometry-live-test.el --- Geometry tests for the sidebar side window -*- lexical-binding: t -*-

;;; Commentary:
;; The suite this repo did not have: assertions about how WIDE a window
;; actually ends up, as opposed to whether it exists.  Every test here
;; fails on a checkout that predates the geometry fixes; the two
;; characterisation tests instead pin Emacs's own rules so a future
;; Emacs that changes them is caught rather than silently believed.
;;
;; Two tiers, because the two halves of window geometry have different
;; environment requirements:
;;
;;   Tier 1 -- runs under plain `-Q --batch'.  Everything about WHEN
;;   `display-buffer-in-side-window' resizes a window it reuses, and
;;   about `window-resize' refusing to go below `window-min-width'.
;;   Measured identically in `--batch', under a pty, and on a real NS
;;   GUI frame: these rules are frame-type independent (see FINDINGS.md).
;;
;;   Tier 2 -- needs a real graphical frame, so it skips under `--batch'
;;   and runs via `scripts/gui-ert.sh', which starts a throwaway daemon,
;;   makes an NS frame and runs ERT inside it.  Fringes and scroll bars
;;   exist only there: a batch frame reports `(0 0 nil nil)' from
;;   `window-fringes' no matter what `left-fringe' is set to, so the gap
;;   between `window-total-width' and `window-body-width' is always
;;   exactly 1 in batch and is 2..5 on a GUI frame depending on that
;;   frame's `frame-char-width', fringe pixels and scroll-bar width.
;;
;; Tier 1 invocation (this is the CI-equivalent one):
;;
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/window-geometry-live-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Tier 2 invocation:
;;
;;   scripts/gui-ert.sh modules/window-geometry-live-test.el
;;
;; Like sidebar-test.el, this file is NOT given sidebar.el on the command
;; line -- it fixes `load-path' against the straight build tree itself so
;; sidebar.el's own `(require 'magit-section)' resolves, and skips the
;; whole suite when no bootstrapped straight tree can be found.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; See sidebar-test.el's own header for why: `advice-add' on a primitive
;; subr makes a natively-compiled Emacs spawn a trampoline-compiling
;; subprocess, which magit-section and evil both trigger on load.
(setq native-comp-enable-subr-trampolines nil)

(defun edmacs-geometry-test--locate-straight-build-root ()
  "Return this checkout's `straight/build' directory, or the sibling main
checkout's.  Same worktree-vs-main fallback as sidebar-test.el: a
worktree lives at `<parent>/edmacs__worktrees/<name>', sibling to the
main `<parent>/edmacs' checkout, and only the main checkout has a
populated package tree."
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

(defvar edmacs-geometry-test--build-root
  (edmacs-geometry-test--locate-straight-build-root))

(if (null edmacs-geometry-test--build-root)

    (ert-deftest edmacs-geometry-test-straight-unavailable ()
      (ert-skip "no bootstrapped straight/build found in this checkout or its \
sibling main checkout; sidebar.el's `magit-section' dependency cannot be \
resolved"))

  (progn

    (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section"))
      (let ((dir (expand-file-name dep edmacs-geometry-test--build-root)))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    (load (expand-file-name "modules/windows.el" default-directory) nil t)
    (load (expand-file-name "modules/sidebar.el" default-directory) nil t)

    ;; ======================================================================
    ;; Helpers
    ;; ======================================================================

    (defun edmacs-geometry-test--reset-frame (frame)
      "Return FRAME to a single plain window, releasing any side window."
      (dolist (w (window-list frame 'never))
        (when (window-parameter w 'window-side)
          (set-window-dedicated-p w nil)
          (dolist (p '(window-side window-slot no-other-window
                       no-delete-other-windows mode-line-format
                       quit-restore quit-restore-prev))
            (set-window-parameter w p nil))))
      (delete-other-windows (frame-first-window frame)))

    (defun edmacs-geometry-test--cleanup (frame)
      "Drop FRAME's sidebar window, buffer and geometry frame parameters."
      (ignore-errors (edmacs-sidebar-hide frame))
      (let ((buf (edmacs-sidebar--buffer frame)))
        (when (buffer-live-p buf) (kill-buffer buf)))
      (set-frame-parameter frame 'edmacs-sidebar-buffer nil)
      (set-frame-parameter frame 'edmacs-sidebar-collapsed nil)
      (set-frame-parameter frame 'edmacs-sidebar-remembered-width nil)
      (edmacs-geometry-test--reset-frame frame))

    (defmacro edmacs-geometry-test--with-sidebar (frame-var window-var &rest body)
      "Show a sidebar on the selected frame, bind FRAME-VAR/WINDOW-VAR, run BODY.
Cleans up unconditionally.  Skips when the sidebar window cannot be
created at all (a frame too narrow for a side window plus a main one)."
      (declare (indent 2))
      `(let ((,frame-var (selected-frame)))
         (unwind-protect
             (let ((,window-var (edmacs-sidebar-show ,frame-var)))
               (unless (window-live-p ,window-var)
                 (ert-skip "no side window could be created on this frame"))
               ,@body)
           (edmacs-geometry-test--cleanup ,frame-var))))

    (defun edmacs-geometry-test--poison-quit-restore (window kind)
      "Put WINDOW's `quit-restore' parameter into one of the shapes seen live.
KIND `none' is what a `frameset'/desktop restore leaves behind --
`quit-restore' is not in `window-persistent-parameters', so a restored
window simply has none.  KIND `other' is what showing a DIFFERENT buffer
in the window leaves behind: `display-buffer-record-window' replaces the
symbol `window' in slot 1 with a quadruple describing the buffer that was
displaced, and a later same-buffer reuse only ever rewrites slot 0."
      (pcase kind
        ('none (set-window-parameter window 'quit-restore nil))
        ('other (set-window-parameter
                 window 'quit-restore
                 (list 'same
                       (list (window-buffer window) 1 (copy-marker 1) 40)
                       (selected-window) (window-buffer window))))))

    (defun edmacs-geometry-test--graphical-p ()
      (and (display-graphic-p) (frame-parameter nil 'left-fringe)))

    ;; ======================================================================
    ;; Characterisation -- Emacs's own rules, pinned so an upgrade is caught
    ;; ======================================================================

    (ert-deftest edmacs-geometry-test-reuse-resize-requires-quit-restore-window ()
      "`display-buffer-in-side-window' resizes a window it REUSES only when
that window's `quit-restore' parameter has the symbol `window' in slot 1.
This is the whole mechanism behind the sidebar's over-wide collapsed
strip, and it is identical in `--batch', under a pty and on a real NS
GUI frame -- batch does NOT lie about it."
      (let ((frame (selected-frame))
            (buf (get-buffer-create " *geometry-characterise*")))
        (unwind-protect
            (let* ((alist-wide '((side . left) (slot . 0) (window-width . 30)))
                   (alist-thin '((side . left) (slot . 0) (window-width . 12)))
                   (w (display-buffer-in-side-window buf alist-wide)))
              (unless (window-live-p w)
                (ert-skip "no side window could be created on this frame"))
              (set-window-dedicated-p w t)
              (should (equal 'window (nth 1 (window-parameter w 'quit-restore))))
              ;; Intact quit-restore: the reuse DOES resize.
              (display-buffer-in-side-window buf alist-thin)
              (should (= 12 (window-total-width w)))
              ;; Cleared quit-restore: the reuse silently does not.
              (edmacs-geometry-test--poison-quit-restore w 'none)
              (display-buffer-in-side-window buf alist-wide)
              (should (= 12 (window-total-width w)))
              ;; `(other ...)'-shaped quit-restore: likewise silently does not.
              (edmacs-geometry-test--poison-quit-restore w 'other)
              (display-buffer-in-side-window buf alist-wide)
              (should (= 12 (window-total-width w))))
          (edmacs-geometry-test--reset-frame frame)
          (kill-buffer buf))))

    (ert-deftest edmacs-geometry-test-window-resize-below-min-width-signals ()
      "`window-resize' refuses to shrink a window past `window-min-width'
without an IGNORE argument -- and it SIGNALS rather than returning nil,
so a caller that wraps it in `ignore-errors' turns the refusal silent."
      (let ((frame (selected-frame))
            (buf (get-buffer-create " *geometry-minwidth*")))
        (unwind-protect
            (let ((w (display-buffer-in-side-window
                      buf '((side . left) (slot . 0) (window-width . 30)))))
              (unless (window-live-p w)
                (ert-skip "no side window could be created on this frame"))
              (set-window-dedicated-p w t)
              (should (= 10 window-min-width))
              (should-error (window-resize w (- 5 (window-total-width w)) t nil)
                            :type 'error)
              (should (= 30 (window-total-width w)))
              ;; `safe' is the only IGNORE value that works on every frame
              ;; type -- see the GUI-only test below for why plain `t' is not
              ;; enough.
              (window-resize w (- 5 (window-total-width w)) t 'safe)
              (should (= 5 (window-total-width w))))
          (edmacs-geometry-test--reset-frame frame)
          (kill-buffer buf))))

    (ert-deftest edmacs-geometry-test-gui-resize-ignore-t-is-not-enough ()
      "On a graphical frame the fringes and scroll bar occupy real pixels, so
IGNORE=t -- which drops only `window-min-width' -- still cannot reach a
5-column total; only IGNORE=`safe' can.  Under `--batch' both succeed,
which is exactly the class of false positive this suite exists to stop."
      (unless (edmacs-geometry-test--graphical-p)
        (ert-skip "needs a graphical frame; run via scripts/gui-ert.sh"))
      (let ((frame (selected-frame))
            (buf (get-buffer-create " *geometry-gui-minwidth*")))
        (unwind-protect
            (let ((w (display-buffer-in-side-window
                      buf '((side . left) (slot . 0) (window-width . 30)))))
              (unless (window-live-p w)
                (ert-skip "no side window could be created on this frame"))
              (set-window-dedicated-p w t)
              (should-error (window-resize w (- 5 (window-total-width w)) t t)
                            :type 'error)
              (should (= 30 (window-total-width w)))
              (window-resize w (- 5 (window-total-width w)) t 'safe)
              (should (= 5 (window-total-width w))))
          (edmacs-geometry-test--reset-frame frame)
          (kill-buffer buf))))

    ;; ======================================================================
    ;; Regressions -- these FAIL on the checkout that shipped the wide strip
    ;; ======================================================================

    (ert-deftest edmacs-geometry-test-collapse-narrows-a-restored-side-window ()
      "AC: collapsing must narrow the sidebar even when its window carries no
`quit-restore' parameter.  That is the state every desktop-restored frame
is in -- `quit-restore' is not persisted by `frameset' -- and it is the
state in which `edmacs-sidebar-show's `display-buffer-in-side-window'
call silently declines to resize.  Fails on a checkout whose
`edmacs-sidebar-show' relies on that call alone to set the width."
      (edmacs-geometry-test--with-sidebar frame window
        (let ((wide (window-total-width window)))
          (should (> wide 15))
          (edmacs-geometry-test--poison-quit-restore window 'none)
          (edmacs-sidebar-collapse frame)
          (let ((w (edmacs-sidebar--side-window frame)))
            (should (window-live-p w))
            (should (< (window-total-width w) wide))
            (should (= edmacs-sidebar--collapsed-width (window-body-width w)))))))

    (ert-deftest edmacs-geometry-test-collapse-narrows-a-reused-side-window ()
      "Same AC, for the other live shape: a window whose `quit-restore' slot 1
is the displaced-buffer quadruple rather than the symbol `window'.  A
sidebar buffer that was ever recreated (the `<2>' uniquification path)
leaves the window permanently in this shape, so collapse never worked
again on that frame."
      (edmacs-geometry-test--with-sidebar frame window
        (let ((wide (window-total-width window)))
          (edmacs-geometry-test--poison-quit-restore window 'other)
          (edmacs-sidebar-collapse frame)
          (let ((w (edmacs-sidebar--side-window frame)))
            (should (window-live-p w))
            (should (< (window-total-width w) wide))
            (should (= edmacs-sidebar--collapsed-width (window-body-width w)))))))

    (ert-deftest edmacs-geometry-test-collapsed-strip-body-width-survives-chrome ()
      "AC: a collapsed strip's producers get a width equal to the window's
real `window-body-width'.  `edmacs-sidebar-show' compensates for chrome
with a hard-coded `1+', which is only ever right when chrome costs
exactly one column -- true in `--batch', false on every GUI frame
measured (2 columns at char-width 9 with 8px fringes, 5 at char-width 7
with a 17px scroll bar).

Batch cannot grow real fringes, but `set-window-margins' shaves
`window-body-width' the same way and IS honoured in batch, so it stands
in as a chrome surrogate: with a 2-column left margin the `1+' is off by
two, and the producers are formatted for a width the window does not
have."
      (edmacs-geometry-test--with-sidebar frame window
        (set-window-margins window 2 0)
        (edmacs-sidebar-collapse frame)
        (let ((w (edmacs-sidebar--side-window frame)))
          (should (window-live-p w))
          (should (= edmacs-sidebar--collapsed-width (window-body-width w)))
          (should (= edmacs-sidebar--collapsed-width
                     (edmacs-sidebar--strip-width frame))))))

    (ert-deftest edmacs-geometry-test-gui-collapsed-strip-body-width ()
      "The same AC without the surrogate: on a real graphical frame the
collapsed strip's `window-body-width' must equal
`edmacs-sidebar--collapsed-width'.  Fails on a checkout that requests
`(1+ edmacs-sidebar--collapsed-width)' as a TOTAL width."
      (unless (edmacs-geometry-test--graphical-p)
        (ert-skip "needs a graphical frame; run via scripts/gui-ert.sh"))
      (edmacs-geometry-test--with-sidebar frame window
        (edmacs-sidebar-collapse frame)
        (let ((w (edmacs-sidebar--side-window frame)))
          (should (window-live-p w))
          (should (= edmacs-sidebar--collapsed-width (window-body-width w)))
          (should (= edmacs-sidebar--collapsed-width
                     (edmacs-sidebar--strip-width frame))))))

    (ert-deftest edmacs-geometry-test-enforce-width-signal-propagates ()
      "AC: a genuine `window-resize' failure inside
`edmacs-sidebar--enforce-width' must signal, not vanish. A wrapper that
caught it (e.g. `ignore-errors', as the `spike/live-frame-harness'
candidate did) would return silently here instead. Exercises both WIDTH
forms `edmacs-sidebar-show' passes -- the plain integer (expanded case)
and the `(body-columns . N)' cons (collapsed case)."
      (edmacs-geometry-test--with-sidebar frame window
        (cl-letf (((symbol-function 'window-resize)
                   (lambda (&rest _) (signal 'error '("stubbed resize refusal")))))
          (should-error (edmacs-sidebar--enforce-width window frame 12)
                        :type 'error)
          (should-error
           (edmacs-sidebar--enforce-width window frame (cons 'body-columns 4))
           :type 'error))))

    (ert-deftest edmacs-geometry-test-gui-expand-restores-fringes ()
      "AC: expanding must undo the collapse's `set-window-fringes 0 0'.
Nothing does, so an expanded sidebar keeps zero fringes for the life of
the frame -- observed live on 2026-09-04 in a daemon frame sitting at
width 32 with `edmacs-sidebar-collapsed' nil and `window-fringes'
reporting `(0 0 nil nil)' while a sibling frame's sidebar reported
`(8 8 nil nil)'."
      (unless (edmacs-geometry-test--graphical-p)
        (ert-skip "needs a graphical frame; run via scripts/gui-ert.sh"))
      (edmacs-geometry-test--with-sidebar frame window
        (let ((default (list (frame-parameter frame 'left-fringe)
                             (frame-parameter frame 'right-fringe))))
          (should (> (car default) 0))
          (edmacs-sidebar-collapse frame)
          (edmacs-sidebar-expand frame)
          (let ((w (edmacs-sidebar--side-window frame)))
            (should (window-live-p w))
            (should (equal default (seq-take (window-fringes w) 2)))))))))

(provide 'window-geometry-live-test)
;;; window-geometry-live-test.el ends here
