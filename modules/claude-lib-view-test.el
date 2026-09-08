;;; claude-lib-view-test.el --- Tests for claude-lib-view.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tier-1 batch coverage of the rendering substrate: mode dispatch, spec
;; validation, the caps, the key bindings, id/isearch/sort preservation
;; around an SVG cell, and the guarantee that the return value never
;; carries the buffer's contents.
;;
;; Everything here runs under plain `-Q --batch'.  Pixel assertions
;; cannot: `image-size' signals "Window system frame should be used"
;; there, so a batch test that reaches for it FAILS rather than skips.
;; Those live in `modules/claude-lib-view-live-test.el' behind a
;; `display-graphic-p' gate, run through `scripts/gui-ert.sh'.
;;
;; Narrow invocation -- the substrate alone.  Two tests SKIP under it
;; (the window-placement pair, which needs windows.el) and one skips
;; without evil, so this is not the invocation to judge coverage by:
;;
;;   scripts/run-ert-suite.sh 60 \
;;     emacs -Q --batch -l ert \
;;           -l modules/claude-lib-view.el \
;;           -l modules/claude-lib-view-test.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; FULL invocation -- the one to run before landing.  Adds windows.el
;; for the display/eviction and inherited-`q' tests and claude-lib.el
;; for the shared output ceiling.  The evil test additionally needs a
;; populated `straight/build', which a worktree does not have: run this
;; from the MAIN CHECKOUT to drive the skip count to zero.
;;
;;   scripts/run-ert-suite.sh 60 \
;;     emacs -Q --batch -l ert \
;;           -l modules/windows.el \
;;           -l modules/claude-lib.el \
;;           -l modules/claude-lib-view.el \
;;           -l modules/claude-lib-view-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'claude-lib-view)

;; `cl-letf' on a C subr forces a synchronous native-comp trampoline
;; build -- ~28s of wall clock per target on a cold eln-cache, with the
;; suite still reporting a clean pass. See this repo's CLAUDE.md; the
;; canonical comment is in `modules/sessions-test.el'.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(declare-function evil-initial-state "evil-core")
(declare-function evil-get-auxiliary-keymap "evil-core")
(declare-function edmacs-main-window "windows")
(declare-function edmacs-stack-windows "windows")
(defvar edmacs-stack-max-windows)
(defvar edmacs-claude-lib-max-output-bytes)

;; ============================================================================
;; Helpers
;; ============================================================================

(defun claude-lib-view-test--straight-build-root ()
  "Return a populated `straight/build', this checkout's or the main one's.
Same worktree-vs-main fallback as `modules/window-geometry-live-test.el':
a worktree lives at `<parent>/edmacs__worktrees/<name>', sibling to the
main `<parent>/edmacs' checkout, and only the main checkout has a
populated package tree."
  (or (let ((here (expand-file-name "straight/build" default-directory)))
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

(defun claude-lib-view-test--load-evil ()
  "Load `evil' from a bootstrapped straight tree, returning non-nil on success.
`evil' is not on a `-Q' `load-path', and a worktree has no populated
package tree of its own -- so this reaches for the main checkout's,
exactly as the geometry suite does, rather than demanding evil on the
command line."
  (or (featurep 'evil)
      (let ((build (claude-lib-view-test--straight-build-root)))
        (when build
          (dolist (dep '("evil" "goto-chg" "compat" "annalist"))
            (let ((dir (expand-file-name dep build)))
              (when (file-directory-p dir) (add-to-list 'load-path dir))))
          (require 'evil nil t)))))

(defun claude-lib-view-test--source ()
  "Return the text of `claude-lib-view.el'."
  (let ((file (or (let ((here (expand-file-name "modules/claude-lib-view.el"
                                                default-directory)))
                    (and (file-exists-p here) here))
                  (ignore-errors (find-library-name "claude-lib-view")))))
    (unless file
      (ert-skip "cannot locate modules/claude-lib-view.el from this directory"))
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun claude-lib-view-test--images ()
  "Return every `image' display property value in the current buffer, in order."
  (let ((pos (point-min)) found)
    (while (setq pos (next-single-property-change pos 'display))
      (let ((value (get-text-property pos 'display)))
        (when (and (consp value) (eq (car value) 'image))
          (push value found))))
    (nreverse found)))

(defmacro claude-lib-view-test--with-views (&rest body)
  "Run BODY, killing every `*claude-view: ...*' buffer afterwards."
  (declare (indent 0) (debug t))
  `(unwind-protect (progn ,@body)
     (dolist (buf (buffer-list))
       (when (string-prefix-p "*claude-view: " (buffer-name buf))
         (kill-buffer buf)))))

(defun claude-lib-view-test--count (needle haystack)
  "Return how many times NEEDLE occurs in HAYSTACK."
  (let ((n 0) (start 0))
    (while (string-match (regexp-quote needle) haystack start)
      (setq n (1+ n) start (match-end 0)))
    n))

(defun claude-lib-view-test--rows (n)
  "Return N plain rows."
  (let (rows)
    (dotimes (i n)
      (push (list :id (intern (format "id-%d" i))
                  :cells (list (format "row-%d" i)))
            rows))
    (nreverse rows)))

(defun claude-lib-view-test--require-windows ()
  "Skip unless `modules/windows.el' is loaded."
  (unless (fboundp 'edmacs-main-window)
    (ert-skip "needs modules/windows.el on the command line (see this file's header)")))

;; ============================================================================
;; AC1 -- one function, two modes, one substrate
;; ============================================================================

(ert-deftest claude-lib-view-test-render-dispatches-three-modes ()
  "One entry point picks `rows', `unified' or `image' from the data itself."
  (claude-lib-view-test--with-views
    (let* ((rows-result
            (claude-lib-render "plain" :name "plain"
                               :columns [("A" 10 t)]
                               :rows '((:id a :cells ("alpha")))))
           (unified-result
            (claude-lib-render "barred" :name "barred"
                               :columns [("A" 10 t)]
                               :rows '((:id a :cells ("alpha") :bar 1.0))))
           (image-result
            (claude-lib-render "pictured" :name "pictured"
                               :image '(:width 40 :height 10
                                        :elements ((rect :x 0 :y 0 :width 40 :height 10))))))
      (dolist (pair (list (cons rows-result 'rows)
                          (cons unified-result 'unified)
                          (cons image-result 'image)))
        (with-current-buffer (plist-get (car pair) :buffer)
          (should (eq claude-lib--view-render-mode (cdr pair)))
          (should (eq (plist-get (car pair) :mode) (cdr pair)))
          ;; ONE major mode, not three.
          (should (eq major-mode 'claude-lib-view-mode))))
      (should-error (claude-lib-render "nothing at all") :type 'user-error))))

(ert-deftest claude-lib-view-test-validate-rejects-bad-spec ()
  "A malformed spec is a `user-error' naming the row, and builds no buffer."
  (claude-lib-view-test--with-views
    (let ((cases
           (list
            ;; cell count vs column count
            (list :columns [("A" 10 t) ("B" 10 t)]
                  :rows '((:id a :cells ("only-one"))))
            ;; duplicate id
            (list :columns [("A" 10 t)]
                  :rows '((:id a :cells ("x")) (:id a :cells ("y"))))
            ;; unrecognised location
            (list :columns [("A" 10 t)]
                  :rows '((:id a :cells ("x") :location 42))))))
      (dolist (spec cases)
        (let ((err (should-error (apply #'claude-lib-render "t" :name "t" spec)
                                 :type 'user-error)))
          (should (string-match-p "row [0-9]+" (error-message-string err))))
        (should-not (get-buffer (claude-lib--view-buffer-name "t")))))
    (should-error (claude-lib-render "" :name "t" :columns [("A" 10 t)]
                                     :rows '((:id a :cells ("x"))))
                  :type 'user-error)
    (should-not (get-buffer (claude-lib--view-buffer-name "t")))))

;; ============================================================================
;; AC2 -- evil normal state, RET-to-source, isearchable
;; ============================================================================

(ert-deftest claude-lib-view-test-mode-body-runs-without-evil ()
  "The `fboundp' guard leaves the mode usable when evil is absent."
  (claude-lib-view-test--with-views
    (let ((result (cl-letf (((symbol-function 'evil-set-initial-state) nil))
                    ;; Not `cl-letf' on a subr: `evil-set-initial-state'
                    ;; is Lisp, so no native-comp trampoline is built.
                    (claude-lib-render "no evil" :name "noevil"
                                       :columns [("A" 10 t)]
                                       :rows '((:id a :cells ("x")))))))
      (with-current-buffer (plist-get result :buffer)
        (should (eq major-mode 'claude-lib-view-mode))))))

(ert-deftest claude-lib-view-test-initial-state-is-normal ()
  "Entering the mode puts evil in normal state -- not insert, not emacs."
  (unless (claude-lib-view-test--load-evil)
    (ert-skip "no bootstrapped straight/build holding `evil' in this checkout or \
its sibling main checkout"))
  (claude-lib-view-test--with-views
    (claude-lib-render "stateful" :name "stateful" :display nil
                       :columns [("A" 10 t)]
                       :rows '((:id a :cells ("x"))))
    ;; Asserted through `evil-initial-state', not an alist: evil records
    ;; initial states in per-state mode LISTS (`evil-normal-state-modes'),
    ;; and there is no `evil-initial-state-alist' to read.
    (should (eq (evil-initial-state 'claude-lib-view-mode) 'normal))))

(ert-deftest claude-lib-view-test-ret-is-bound-plainly ()
  "RET on the mode map reaches `claude-lib-view-visit' when evil is absent."
  (should (eq (lookup-key claude-lib-view-mode-map (kbd "RET"))
              #'claude-lib-view-visit)))

(ert-deftest claude-lib-view-test-ret-and-gd-bound-in-normal-state ()
  "RET and `gd' are also installed in evil's normal-state auxiliary map.
A plain `define-key' alone is DEAD in normal state: evil installs its
state maps through `emulation-mode-map-alists', consulted before the
buffer's local map, and `evil-normal-state-map' binds RET."
  (unless (claude-lib-view-test--load-evil)
    (ert-skip "no bootstrapped straight/build holding `evil' in this checkout or \
its sibling main checkout"))
  (let ((aux (evil-get-auxiliary-keymap claude-lib-view-mode-map 'normal)))
    (should (eq (lookup-key aux (kbd "RET")) #'claude-lib-view-visit))
    (should (eq (lookup-key aux (kbd "gd")) #'claude-lib-view-visit))))

(ert-deftest claude-lib-view-test-visit-jumps-to-source ()
  "RET's command opens the row's file at its line, leaving the view live."
  (let ((file (make-temp-file "claude-lib-view-" nil ".txt"
                              "one\ntwo\nthree\nfour\n")))
    (unwind-protect
        (claude-lib-view-test--with-views
          (let* ((result (claude-lib-render "sources" :name "sources"
                                            :columns [("A" 12 t)]
                                            :rows `((:id a :cells ("first")
                                                     :location (,file . 3))
                                                    (:id b :cells ("second")))))
                 (view (get-buffer (plist-get result :buffer))))
            (with-current-buffer view
              (goto-char (point-min))
              (let ((target (claude-lib-view-visit)))
                (with-current-buffer target
                  (should (equal (file-truename (buffer-file-name))
                                 (file-truename file)))
                  (should (= (line-number-at-pos) 3)))
                (kill-buffer target)))
            (should (buffer-live-p view))))
      (delete-file file))))

(ert-deftest claude-lib-view-test-visit-errors-are-user-errors ()
  "A row with no location, and one whose file is gone, both `user-error'."
  (let ((file (make-temp-file "claude-lib-view-" nil ".txt" "one\n")))
    (claude-lib-view-test--with-views
      (let* ((result (claude-lib-render "sources" :name "sources"
                                        :columns [("A" 12 t)]
                                        :rows `((:id a :cells ("nowhere"))
                                                (:id b :cells ("gone")
                                                 :location (,file . 1)))))
             (view (get-buffer (plist-get result :buffer))))
        (with-current-buffer view
          (goto-char (point-min))
          (should-error (claude-lib-view-visit) :type 'user-error)
          (delete-file file)
          (forward-line 1)
          (let ((err (should-error (claude-lib-view-visit) :type 'user-error)))
            (should (string-match-p "no longer exists"
                                    (error-message-string err)))))))))

(ert-deftest claude-lib-view-test-labels-stay-real-buffer-text ()
  "Row labels are searchable text; only the bar cell carries a display property."
  (claude-lib-view-test--with-views
    (let ((result (claude-lib-render "suites" :name "suites"
                                     :columns [("Suite" 20 t)]
                                     :rows '((:id w :cells ("windows-test") :bar 282.0)
                                             (:id u :cells ("ui-test") :bar 12.4)))))
      (with-current-buffer (plist-get result :buffer)
        (goto-char (point-min))
        (should (search-forward "windows-test" nil t))
        ;; The label's own text carries no display property: the image
        ;; replaces only its own cell.
        (goto-char (point-min))
        (search-forward "windows-test")
        (should-not (get-text-property (match-beginning 0) 'display))))))

;; ============================================================================
;; AC3 -- the image mode renders a real SVG
;; ============================================================================

(ert-deftest claude-lib-view-test-image-mode-carries-svg-property ()
  "The standalone image is a real `:type svg' display property with N elements."
  (claude-lib-view-test--with-views
    (let* ((result (claude-lib-render "gc pauses"
                                      :name "gc"
                                      :image '(:width 200 :height 60
                                               :elements ((rect :x 0 :y 0 :width 200 :height 60)
                                                          (rect :x 4 :y 4 :width 10 :height 20)
                                                          (rect :x 20 :y 4 :width 10 :height 40)))))
           (buffer (get-buffer (plist-get result :buffer))))
      (with-current-buffer buffer
        (let ((images (claude-lib-view-test--images)))
          (should (= (length images) 1))
          (let ((image (car images)))
            (should (eq (plist-get (cdr image) :type) 'svg))
            (should (= 3 (claude-lib-view-test--count
                          "<rect" (plist-get (cdr image) :data))))))
        (should claude-lib--view-image-svg)
        (should (= 3 (claude-lib--view-element-count claude-lib--view-image-svg))))
      ;; The summary restates what the picture shows.
      (should (string-match-p "image 200x60, 3 elements" (plist-get result :summary))))))

(ert-deftest claude-lib-view-test-svg-has-single-xmlns ()
  "A caller-supplied `:xmlns' is collapsed away.
`svg-create' already emits one; a duplicate is markup Emacs displays and
`rsvg-convert' rejects with \"Attribute xmlns redefined\", so the round
trip would fail on a picture that looks fine on screen."
  (let* ((dom (svg-create 20 10 :xmlns "http://www.w3.org/2000/svg"))
         (_ (svg-rectangle dom 0 0 20 10 :fill "blue"))
         (built (claude-lib--view-build-svg dom))
         (markup (with-temp-buffer (svg-print built) (buffer-string))))
    (should (= 1 (claude-lib-view-test--count "xmlns=" markup)))
    ;; `xmlns:xlink' is svg.el's own second declaration and is fine --
    ;; only a REDEFINED `xmlns' is what rsvg-convert rejects.
    (should (= 1 (claude-lib-view-test--count "xmlns:xlink=" markup)))))

;; ============================================================================
;; AC4 -- the unified shape does both in one buffer
;; ============================================================================

(ert-deftest claude-lib-view-test-unified-row-and-bar ()
  "One buffer is navigable AND illustrated, and stays so across a numeric sort."
  (claude-lib-view-test--with-views
    (let* ((result (claude-lib-render "suite wall clock" :name "suites"
                                      :columns [("Suite" 20 t)]
                                      :rows '((:id w :cells ("windows-test") :bar 282.0)
                                              (:id u :cells ("ui-test") :bar 12.4)
                                              (:id s :cells ("sidebar-test") :bar 9.0))))
           (buffer (get-buffer (plist-get result :buffer))))
      (with-current-buffer buffer
        ;; (a) an svg lives in each bar cell after `tabulated-list-print'
        (let ((images (claude-lib-view-test--images)))
          (should (= (length images) 3))
          (should (seq-every-p (lambda (i) (eq (plist-get (cdr i) :type) 'svg)) images)))
        ;; (b) row ids still resolve, so RET-to-source is unaffected
        (goto-char (point-min))
        (should (eq (tabulated-list-get-id) 'w))
        (forward-line 1)
        (should (eq (tabulated-list-get-id) 'u))
        ;; (c) the label is isearchable and carries no display property
        (goto-char (point-min))
        (should (search-forward "windows-test" nil t))
        ;; (d) the numeric column sorts NUMERICALLY and the images survive
        (tabulated-list-sort 1)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "sidebar-test\\(.\\|\n\\)*ui-test\\(.\\|\n\\)*windows-test"
                                  text)))
        (should (= 3 (length (claude-lib-view-test--images))))
        (goto-char (point-min))
        (should (eq (tabulated-list-get-id) 's))))))

(ert-deftest claude-lib-view-test-bar-max-is-derived-and-degenerate-safe ()
  "`:bar-max' comes from the rows when omitted; a zero/nil max renders a track."
  (claude-lib-view-test--with-views
    ;; A value equal to the derived max fills the whole track.
    (let ((full (claude-lib--view-bar-svg 282.0 282.0 120 12))
          (part (claude-lib--view-bar-svg 12.4 282.0 120 12)))
      (should (= 120 (dom-attr (nth 1 (dom-children full)) 'width)))
      (should (< 0 (dom-attr (nth 1 (dom-children part)) 'width) 120)))
    ;; Every bar nil, or a zero max: the track alone, no division by zero.
    (let ((zero (claude-lib--view-bar-svg 5 0 120 12))
          (none (claude-lib--view-bar-svg nil 100 120 12)))
      (should (= 1 (claude-lib--view-element-count zero)))
      (should (= 1 (claude-lib--view-element-count none))))
    ;; A value over max clamps rather than overflowing the track.
    (should (= 120 (dom-attr (nth 1 (dom-children
                                       (claude-lib--view-bar-svg 500 100 120 12)))
                             'width)))
    ;; A negative value emits no fill rect at all, rather than a
    ;; negative-width one that `rsvg-convert' may reject.
    (should (= 1 (claude-lib--view-element-count
                  (claude-lib--view-bar-svg -5 100 120 12))))
    (let ((result (claude-lib-render "all nil" :name "allnil"
                                     :columns [("A" 10 t)]
                                     :bar-column 1
                                     :rows '((:id a :cells ("x"))
                                             (:id b :cells ("y"))))))
      (should (eq (plist-get result :mode) 'unified)))))

(ert-deftest claude-lib-view-test-bar-threshold-adds-one-element-and-clamps ()
  "A `:bar-threshold' contributes exactly one extra dom element, clamped into range."
  (let ((plain (claude-lib--view-bar-svg 50 100 120 12))
        (marked (claude-lib--view-bar-svg 50 100 120 12 40)))
    (should (= (1+ (claude-lib--view-element-count plain))
              (claude-lib--view-element-count marked))))
  ;; A threshold above :bar-max clamps to the bar's full width rather
  ;; than erroring or drawing off-canvas.
  (let* ((dom (claude-lib--view-bar-svg 50 100 120 12 500))
         (line (car (last (dom-children dom)))))
    (should (= 120 (dom-attr line 'x1)))
    (should (= 120 (dom-attr line 'x2))))
  ;; A nil/zero threshold on a nil/zero max still draws at x=0, not NaN.
  (let* ((dom (claude-lib--view-bar-svg nil 0 120 12 5))
         (line (car (last (dom-children dom)))))
    (should (= 0 (dom-attr line 'x1)))))

(ert-deftest claude-lib-view-test-bar-threshold-needs-a-bar ()
  "`:bar-threshold' with no `:bar' anywhere is a `user-error', not a silent no-op."
  (claude-lib-view-test--with-views
    (should-error
     (claude-lib-render "rows only" :name "threshold-no-bar"
                        :columns [("A" 10 t)]
                        :bar-threshold 5
                        :rows '((:id a :cells ("x"))))
     :type 'user-error)
    (should-error
     (claude-lib-render "image only" :name "threshold-image"
                        :bar-threshold 5
                        :image '(:width 10 :height 10
                                 :elements ((rect :x 0 :y 0 :width 10 :height 10))))
     :type 'user-error)))

;; ============================================================================
;; AC5 -- the inherited `q', not a reimplemented one
;; ============================================================================

(ert-deftest claude-lib-view-test-q-is-inherited-not-rebound ()
  "The substrate binds no `q'; `quit-window' arrives through the parent chain."
  (let ((own (copy-keymap claude-lib-view-mode-map)))
    (set-keymap-parent own nil)
    (should-not (lookup-key own "q")))
  (should (eq (lookup-key claude-lib-view-mode-map "q") #'quit-window))
  ;; And the module says so in source, so nobody "fixes" the deliberate
  ;; divergence from claude-usage.el's `q' -> `bury-buffer'.
  (should (string-match-p "binds no `q'" (claude-lib-view-test--source))))

(ert-deftest claude-lib-view-test-quit-returns-to-main ()
  "`q' in the view closes the pane and leaves no stranded duplicate."
  (claude-lib-view-test--require-windows)
  (claude-lib-view-test--with-views
    (let ((decoy (get-buffer-create "*claude-lib-view-test-decoy*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (display-buffer decoy)
            (let* ((result (claude-lib-render "quitting" :name "quitting"
                                              :columns [("A" 10 t)]
                                              :rows '((:id a :cells ("x")))))
                   (view (get-buffer (plist-get result :buffer)))
                   (window (get-buffer-window view)))
              (should (window-live-p window))
              (with-selected-window window (quit-window))
              (should-not (get-buffer-window view))
              (should (window-live-p (edmacs-main-window)))
              ;; No buffer shows in two windows at once: the advice's
              ;; ORIG-FN branch ran `edmacs-windows-dedupe-frame'.
              (let ((shown (mapcar #'window-buffer
                                   (window-list (selected-frame) 'no-minibuf))))
                (should (= (length shown) (length (seq-uniq shown)))))))
        (kill-buffer decoy)
        (delete-other-windows)))))

;; ============================================================================
;; AC6 -- a summary and a buffer name, never the contents
;; ============================================================================

(ert-deftest claude-lib-view-test-return-value-is-a-summary ()
  "The return value names the buffer and carries none of its contents."
  (claude-lib-view-test--with-views
    ;; The extremal rows ARE named on purpose -- restating "max
    ;; windows-test 282.0" is what lets the model retain what it
    ;; rendered. Naming two rows is a summary; carrying all of them
    ;; would be contents, which is what the sentinel row pins.
    (let ((result (claude-lib-render "suite wall clock" :name "ret"
                                     :columns [("Suite" 30 t)]
                                     :rows '((:id w :cells ("windows-test") :bar 282.0)
                                             (:id m :cells ("SENTINEL-ROW-TEXT") :bar 40.0)
                                             (:id u :cells ("ui-test") :bar 9.0)))))
      ;; The key set is exactly the documented allowlist.
      (should (equal (sort (cl-loop for (k _) on result by #'cddr collect k)
                           #'string<)
                     (sort (copy-sequence claude-lib--view-return-keys) #'string<)))
      (should (stringp (plist-get result :buffer)))
      (should (buffer-live-p (get-buffer (plist-get result :buffer))))
      ;; The real assertion: no cell text escapes through the channel.
      (should-not (string-match-p "SENTINEL-ROW-TEXT" (prin1-to-string result)))
      (should-not (plist-member result :contents))
      ;; The summary restates the numbers, so the model retains what it
      ;; rendered even when nobody rasterises it.
      (should (string-match-p "3 rows" (plist-get result :summary)))
      (should (string-match-p "max windows-test 282\\.0" (plist-get result :summary)))
      (should (string-match-p "min ui-test 9\\.0" (plist-get result :summary)))
      (should (< (string-bytes (plist-get result :summary))
                 (claude-lib--view-max-output-bytes))))))

;; ============================================================================
;; AC7 -- displacement is a recorded decision, not a surprise
;; ============================================================================

(ert-deftest claude-lib-view-test-reports-displaced-panes ()
  "A pane closed to make room comes back as `:displaced', and survives as a buffer."
  (claude-lib-view-test--require-windows)
  (claude-lib-view-test--with-views
    (let ((one (get-buffer-create "*claude-lib-view-test-one*"))
          (two (get-buffer-create "*claude-lib-view-test-two*")))
      (unwind-protect
          (let ((edmacs-stack-max-windows 1))
            (delete-other-windows)
            (display-buffer one)
            (display-buffer two)
            ;; Precondition, asserted rather than assumed: a frame shape
            ;; difference must fail loudly, not pass vacuously.
            (should (get-buffer-window one))
            (should (get-buffer-window two))
            (let ((result (claude-lib-render "evicting" :name "evicting"
                                             :columns [("A" 10 t)]
                                             :rows '((:id a :cells ("x"))))))
              (should (eq (plist-get result :displayed) t))
              (should (member (buffer-name one) (plist-get result :displaced)))
              ;; Only the WINDOW closed: the buffer, and an agent's
              ;; process with it, survives.
              (should (buffer-live-p one))))
        (kill-buffer one)
        (kill-buffer two)
        (delete-other-windows)))))

(ert-deftest claude-lib-view-test-display-nil-shows-nothing ()
  "`:display nil' builds and populates the buffer and displays nothing."
  (claude-lib-view-test--with-views
    (let* ((result (claude-lib-render "quiet" :name "quiet" :display nil
                                      :columns [("A" 10 t)]
                                      :rows '((:id a :cells ("alpha")))))
           (buffer (get-buffer (plist-get result :buffer))))
      (should (buffer-live-p buffer))
      (should-not (get-buffer-window buffer t))
      (should-not (plist-get result :displayed))
      (should-not (plist-get result :displaced))
      (with-current-buffer buffer
        (should (string-match-p "alpha" (buffer-string)))))))

(ert-deftest claude-lib-view-test-decision-is-recorded ()
  "The stack-eviction accept-decision is written down, not folklore."
  (let ((source (claude-lib-view-test--source)))
    (should (string-match-p "edmacs-stack-max-windows" source))
    (should (string-match-p "DECISION: ACCEPT" source))
    (should (string-match-p "only the WINDOW closes" source))))

;; ============================================================================
;; AC9 -- the caps error, and leave nothing on screen
;; ============================================================================

(ert-deftest claude-lib-view-test-caps-error-and-leave-no-buffer ()
  "Both caps ERROR rather than truncate, and no half-rendered view survives."
  (claude-lib-view-test--with-views
    (let ((claude-lib-render-max-rows 10))
      (let ((err (should-error (claude-lib-render "big" :name "big"
                                                  :columns [("A" 10 t)]
                                                  :rows (claude-lib-view-test--rows 11))
                               :type 'user-error)))
        (should (string-match-p "11" (error-message-string err)))
        (should (string-match-p "10" (error-message-string err))))
      (should-not (get-buffer (claude-lib--view-buffer-name "big"))))
    (let ((claude-lib-render-max-svg-elements 5))
      ;; A standalone image the row cap can never catch.
      (should-error (claude-lib-render
                     "wide" :name "wide"
                     :image (list :width 100 :height 10
                                  :elements (make-list 6 '(rect :x 0 :y 0 :width 1 :height 1))))
                    :type 'user-error)
      (should-not (get-buffer (claude-lib--view-buffer-name "wide")))
      ;; And the per-row bars summed: 3 rows x 2 rects is over 5.
      (should-error (claude-lib-render "bars" :name "bars"
                                       :columns [("A" 10 t)]
                                       :rows '((:id a :cells ("x") :bar 1)
                                               (:id b :cells ("y") :bar 2)
                                               (:id c :cells ("z") :bar 3)))
                    :type 'user-error)
      (should-not (get-buffer (claude-lib--view-buffer-name "bars"))))))

(ert-deftest claude-lib-view-test-summary-is-bounded-by-the-shared-ceiling ()
  "An oversized SUMMARY is rejected against the ONE existing output ceiling."
  (claude-lib-view-test--with-views
    (let ((huge (make-string (1+ (claude-lib--view-max-output-bytes)) ?x)))
      (should-error (claude-lib-render huge :name "huge"
                                       :columns [("A" 10 t)]
                                       :rows '((:id a :cells ("x"))))
                    :type 'user-error)
      (should-not (get-buffer (claude-lib--view-buffer-name "huge")))))
  ;; No second 1 MiB constant of this module's own: the ceiling is read
  ;; from claude-lib.el by name.
  (let ((source (claude-lib-view-test--source)))
    (should (string-match-p "edmacs-claude-lib-max-output-bytes" source))
    (should-not (string-match-p
                 "(def\\(var\\|const\\|custom\\) +claude-lib[a-z-]*max-\\(output\\|bytes\\)"
                 source))))

;; ============================================================================
;; AC10 -- library conventions, no new dependency, one covered major mode
;; ============================================================================

(ert-deftest claude-lib-view-test-library-conventions ()
  "Entry points are documented, first-line-returns-shaped, and unpolluted."
  (let ((first-line (car (split-string (documentation 'claude-lib-render) "\n"))))
    (should (string-suffix-p "." first-line))
    (should (string-match-p "return" first-line)))
  (should (string-match-p "return"
                          (car (split-string
                                (documentation 'claude-lib-render-rasterize) "\n"))))
  (let ((entries (apropos-internal "\\`claude-lib-[^-]" #'fboundp)))
    (should (memq 'claude-lib-render entries))
    (should (memq 'claude-lib-render-rasterize entries))
    ;; Internals are `claude-lib--view-*'; the `claude-lib-view--*' shape
    ;; would match the entry-point regexp and fill a listing of what is
    ;; callable with this file's helpers.
    (should-not (seq-find (lambda (s) (string-prefix-p "claude-lib-view--"
                                                       (symbol-name s)))
                          entries))
    (dolist (entry '(claude-lib-render claude-lib-render-rasterize
                     claude-lib-view-visit))
      (should (documentation entry))))
  ;; A dated provenance comment naming the destination, per phase 3.
  (let ((source (claude-lib-view-test--source)))
    (should (string-match-p ";; Added 2026-09-07:" source))
    (should (string-match-p "Destination: edmacs\\." source))))

(ert-deftest claude-lib-view-test-no-new-dependency ()
  "The module requires core features only, and installs nothing."
  (let ((source (claude-lib-view-test--source))
        (allowed '(svg tabulated-list subr-x seq cl-lib image))
        required)
    (with-temp-buffer
      (insert source)
      (goto-char (point-min))
      (while (re-search-forward "^(require '\\([a-z-]+\\))" nil t)
        (push (intern (match-string 1)) required)))
    (should required)
    (should (seq-every-p (lambda (feature) (memq feature allowed)) required))
    (should-not (string-match-p "use-package\\|straight-use-package" source))))

(ert-deftest claude-lib-view-test-mode-parent-is-evil-collection-covered ()
  "One major mode, derived from the one evil-collection ships a module for."
  (should (eq (get 'claude-lib-view-mode 'derived-mode-parent) 'tabulated-list-mode))
  (let ((source (claude-lib-view-test--source))
        (count 0))
    (with-temp-buffer
      (insert source)
      (goto-char (point-min))
      (while (re-search-forward "^(define-derived-mode " nil t)
        (setq count (1+ count))))
    (should (= count 1))))

(ert-deftest claude-lib-view-test-buffer-is-reused-not-accumulated ()
  "A re-render erases in place; a foreign buffer of that name is never clobbered."
  (claude-lib-view-test--with-views
    (let* ((first (claude-lib-render "once" :name "reuse" :display nil
                                     :columns [("A" 10 t)]
                                     :rows '((:id a :cells ("alpha")))))
           (second (claude-lib-render "twice" :name "reuse" :display nil
                                      :columns [("A" 10 t)]
                                      :rows '((:id b :cells ("beta"))))))
      (should (equal (plist-get first :buffer) (plist-get second :buffer)))
      (with-current-buffer (plist-get second :buffer)
        (should (string-match-p "beta" (buffer-string)))
        (should-not (string-match-p "alpha" (buffer-string)))))
    ;; A user's own buffer under that name keeps its contents.
    (let ((foreign (get-buffer-create (claude-lib--view-buffer-name "foreign"))))
      (unwind-protect
          (progn
            (with-current-buffer foreign (insert "not a view"))
            (let ((result (claude-lib-render "x" :name "foreign" :display nil
                                             :columns [("A" 10 t)]
                                             :rows '((:id a :cells ("x"))))))
              (should-not (equal (plist-get result :buffer) (buffer-name foreign)))
              (with-current-buffer foreign
                (should (string-match-p "not a view" (buffer-string))))))
        (kill-buffer foreign)))))

(provide 'claude-lib-view-test)
;;; claude-lib-view-test.el ends here
