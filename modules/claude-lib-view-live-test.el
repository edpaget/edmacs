;;; claude-lib-view-live-test.el --- Round-trip and pixel tests for claude-lib-view.el -*- lexical-binding: t -*-

;;; Commentary:
;; The half of the rendering substrate that a pure batch suite cannot
;; reach.  Two tiers, modelled on `modules/window-geometry-live-test.el'.
;;
;;   Tier 1 -- runs under plain `-Q --batch'.  The AC8 round trip as a
;;   GENUINE subprocess: `svg-print' to a temp file, the real
;;   `rsvg-convert' binary, a real PNG on disk checked for its magic
;;   bytes.  Gated on `executable-find', with the graceful-degradation
;;   path exercised by pointing the program at a name that cannot be
;;   found.
;;
;;   Tier 2 -- needs a real graphical frame.  `image-size' signals
;;   "Window system frame should be used" under `--batch', so every
;;   pixel assertion FAILS rather than skips if it runs there; these are
;;   gated on `display-graphic-p' and skip in batch by design.  A batch
;;   frame accepts an image spec and renders nothing, so "the bar has
;;   real pixels" and "the bar does not run under the next column" are
;;   unfalsifiable there.
;;
;; Tier 1 invocation:
;;
;;   scripts/run-ert-suite.sh 60 \
;;     emacs -Q --batch -l ert \
;;           -l modules/claude-lib-view.el \
;;           -l modules/claude-lib-view-live-test.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; Tier 2 invocation (the 3 GUI tests; tier 1 runs there too):
;;
;;   scripts/gui-ert.sh modules/claude-lib-view-live-test.el \
;;     "" -l modules/claude-lib-view.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'claude-lib-view)

;; See this repo's CLAUDE.md: `cl-letf' on a C subr forces a synchronous
;; ~28s native-comp trampoline build on a cold eln-cache, with the suite
;; still reporting a clean pass.  Nothing here `cl-letf's a subr -- the
;; rasteriser's seams are `let'-bound Lisp variables -- but the guard
;; travels with the file so a later addition cannot reintroduce it
;; silently.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

;; ============================================================================
;; Helpers
;; ============================================================================

(defmacro claude-lib-view-live-test--with-views (&rest body)
  "Run BODY, killing every `*claude-view: ...*' buffer afterwards."
  (declare (indent 0) (debug t))
  `(unwind-protect (progn ,@body)
     (dolist (buf (buffer-list))
       (when (string-prefix-p "*claude-view: " (buffer-name buf))
         (kill-buffer buf)))))

(defun claude-lib-view-live-test--require-rsvg ()
  "Skip unless the real `rsvg-convert' is on `exec-path'."
  (unless (executable-find claude-lib-render-rsvg-program)
    (ert-skip (format "needs %s on exec-path; it is a convenience for CHECKING \
output, never a requirement for producing it"
                      claude-lib-render-rsvg-program))))

(defun claude-lib-view-live-test--require-gui ()
  "Skip unless running on a real graphical frame."
  (unless (display-graphic-p)
    (ert-skip "needs a graphical frame; run via scripts/gui-ert.sh")))

(defun claude-lib-view-live-test--magic (path n)
  "Return PATH's first N bytes as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 n)
    (buffer-string)))

(defun claude-lib-view-live-test--chart ()
  "Render the sample unified view every test here works from."
  (claude-lib-render "suite wall clock" :name "live" :display nil
                     :columns [("Suite" 20 t)]
                     :rows '((:id windows :cells ("windows-test") :bar 282.0)
                             (:id ui :cells ("ui-test") :bar 12.4)
                             (:id sidebar :cells ("sidebar-test") :bar 9.0))))

;; ============================================================================
;; Tier 1 -- the round trip, as a real subprocess
;; ============================================================================

(ert-deftest claude-lib-view-live-test-round-trip-produces-png ()
  "The documented loop really rasterises: a PNG on disk, with PNG magic bytes.
This is the whole reason the loop is wired rather than assumed.  The
first chart produced this way had a defect visible only by looking --
a duration label colliding with the budget line drawn across it, which
nothing in the markup says and the model that emitted it could not have
known."
  (claude-lib-view-live-test--require-rsvg)
  (claude-lib-view-live-test--with-views
    (let* ((result (claude-lib-render "gc pauses, 40 samples"
                                      :name "live-image" :display nil
                                      :image '(:width 200 :height 60
                                               :elements ((rect :x 0 :y 0 :width 200
                                                           :height 60 :fill "#222")
                                                          (rect :x 10 :y 20 :width 40
                                                           :height 30 :fill "tomato")))))
           (path (claude-lib-render-rasterize (plist-get result :buffer))))
      (unwind-protect
          (progn
            ;; A PATH comes back, never image data: `emacsclient -e'
            ;; returns a printed value, so the read happens outside the
            ;; channel.
            (should (stringp path))
            (should (file-name-absolute-p path))
            (should (string-suffix-p ".png" path))
            (should (file-exists-p path))
            (should (> (file-attribute-size (file-attributes path)) 0))
            (should (equal (claude-lib-view-live-test--magic path 4)
                           (unibyte-string 137 ?P ?N ?G))))
        (when (file-exists-p path) (delete-file path))))))

(ert-deftest claude-lib-view-live-test-round-trip-rasterizes-one-row ()
  "A unified view rasterises a named row's bar, and names the ids without one."
  (claude-lib-view-live-test--require-rsvg)
  (claude-lib-view-live-test--with-views
    (let* ((result (claude-lib-view-live-test--chart))
           (buffer (plist-get result :buffer)))
      ;; No ROW-ID against a rows/unified view: the error lists what is
      ;; available rather than dumping the whole id set.
      (let ((err (should-error (claude-lib-render-rasterize buffer) :type 'user-error)))
        (should (string-match-p "windows" (error-message-string err))))
      (should-error (claude-lib-render-rasterize buffer 'nosuchrow) :type 'user-error)
      (let ((path (claude-lib-render-rasterize buffer 'windows)))
        (unwind-protect
            (progn
              (should (string-suffix-p ".png" path))
              (should (equal (claude-lib-view-live-test--magic path 4)
                             (unibyte-string 137 ?P ?N ?G))))
          (when (file-exists-p path) (delete-file path)))))))

(ert-deftest claude-lib-view-live-test-round-trip-degrades-without-rsvg ()
  "An absent converter returns the SVG path and messages -- it never signals."
  (claude-lib-view-live-test--with-views
    (let* ((result (claude-lib-render "no converter" :name "live-degrade" :display nil
                                      :image '(:width 20 :height 10
                                               :elements ((rect :x 0 :y 0 :width 20
                                                           :height 10)))))
           (claude-lib-render-rsvg-program "claude-lib-view-no-such-program")
           (messages nil)
           path)
      (unwind-protect
          (progn
            (setq path
                  (cl-letf (((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (push (apply #'format fmt args) messages))))
                    (claude-lib-render-rasterize (plist-get result :buffer))))
            (should (string-suffix-p ".svg" path))
            (should (file-exists-p path))
            (should (> (file-attribute-size (file-attributes path)) 0))
            (should (seq-find (lambda (m) (string-match-p "skipped" m)) messages)))
        (when (and path (file-exists-p path)) (delete-file path))))))

(ert-deftest claude-lib-view-live-test-round-trip-survives-a-failing-converter ()
  "A converter that exits non-zero degrades to the SVG, not a zero-byte PNG."
  (claude-lib-view-live-test--with-views
    (let* ((result (claude-lib-render "failing converter" :name "live-fail" :display nil
                                      :image '(:width 20 :height 10
                                               :elements ((rect :x 0 :y 0 :width 20
                                                           :height 10)))))
           ;; `false' exits 1 and writes nothing -- the "present but
           ;; failing" case, indistinguishable from absent to the caller.
           (claude-lib-render-rsvg-program "false")
           path)
      (unless (executable-find "false")
        (ert-skip "needs /usr/bin/false to stand in for a failing converter"))
      (unwind-protect
          (progn
            (setq path (claude-lib-render-rasterize (plist-get result :buffer)))
            (should (string-suffix-p ".svg" path))
            (should (file-exists-p path)))
        (when (and path (file-exists-p path)) (delete-file path))))))

(ert-deftest claude-lib-view-live-test-round-trip-rasterizes-a-thresholded-bar ()
  "A bar drawn with `:bar-threshold' still rasterises cleanly, extra element and all."
  (claude-lib-view-live-test--require-rsvg)
  (claude-lib-view-live-test--with-views
    (let* ((result (claude-lib-render "suite wall clock, budgeted" :name "live-threshold"
                                      :display nil
                                      :columns [("Suite" 20 t)]
                                      :bar-threshold 100.0
                                      :rows '((:id windows :cells ("windows-test") :bar 282.0)
                                              (:id ui :cells ("ui-test") :bar 12.4))))
           (path (claude-lib-render-rasterize (plist-get result :buffer) 'windows)))
      (unwind-protect
          (progn
            (should (string-suffix-p ".png" path))
            (should (file-exists-p path))
            (should (> (file-attribute-size (file-attributes path)) 0))
            (should (equal (claude-lib-view-live-test--magic path 4)
                           (unibyte-string 137 ?P ?N ?G))))
        (when (file-exists-p path) (delete-file path))))))

(ert-deftest claude-lib-view-live-test-rasterized-svg-is-well-formed ()
  "The markup written for the round trip parses -- no duplicate `xmlns'.
The duplicate is what a caller copying the common `svg-create :xmlns'
idiom produces: Emacs displays it happily and `rsvg-convert' rejects it
with \"Attribute xmlns redefined\", so the loop would fail on a picture
that looks fine on screen."
  (claude-lib-view-live-test--require-rsvg)
  (claude-lib-view-live-test--with-views
    (let* ((dom (svg-create 40 20 :xmlns "http://www.w3.org/2000/svg"))
           (_ (svg-rectangle dom 0 0 40 20 :fill "blue"))
           (result (claude-lib-render "duplicated xmlns" :name "live-xmlns"
                                      :display nil :image dom))
           (path (claude-lib-render-rasterize (plist-get result :buffer))))
      (unwind-protect
          (should (string-suffix-p ".png" path))
        (when (file-exists-p path) (delete-file path))))))

;; ============================================================================
;; Tier 2 -- pixels, which only a real graphical frame can falsify
;; ============================================================================

(ert-deftest claude-lib-view-live-test-image-has-real-pixels ()
  "A bar cell's image reports positive pixel dimensions on a real frame."
  (claude-lib-view-live-test--require-gui)
  (claude-lib-view-live-test--with-views
    (let ((result (claude-lib-view-live-test--chart)))
      (with-current-buffer (plist-get result :buffer)
        (goto-char (point-min))
        (let* ((end (line-end-position))
               (image (get-text-property (1- end) 'display)))
          (should (and (consp image) (eq (car image) 'image)))
          (should (eq (plist-get (cdr image) :type) 'svg))
          (let ((size (image-size image t)))
            (should (> (car size) 0))
            (should (> (cdr size) 0))))))))

(ert-deftest claude-lib-view-live-test-bar-ascent-is-centered ()
  "The bar sits centered against the row's text baseline, not hung off it."
  (claude-lib-view-live-test--require-gui)
  (claude-lib-view-live-test--with-views
    (let ((result (claude-lib-view-live-test--chart)))
      (with-current-buffer (plist-get result :buffer)
        (goto-char (point-min))
        (let ((image (get-text-property (1- (line-end-position)) 'display)))
          (should (eq (plist-get (cdr image) :ascent) 'center)))))))

(defun claude-lib-view-live-test--image-at-line ()
  "Return (POSITION . IMAGE) for the first image on the current line."
  (let ((pos (line-beginning-position))
        (end (line-end-position))
        found)
    (while (and (not found) (< pos end))
      (let ((value (get-text-property pos 'display)))
        (if (and (consp value) (eq (car value) 'image))
            (setq found (cons pos value))
          (setq pos (1+ pos)))))
    found))

(ert-deftest claude-lib-view-live-test-bar-column-fits-its-image ()
  "The bar column is declared wide enough to hold the image it carries.
A column narrower than the image does not overlap anything -- Emacs
pushes the following text right instead -- it breaks ALIGNMENT: every
column after the bar starts somewhere other than where the header says
it does.  Unfalsifiable in batch, where a frame accepts the image spec
and renders nothing, so the declared width and the pixel width can
disagree indefinitely without a single test noticing.  Caught exactly
that: a 16-character column and a 120-pixel bar on a 7-pixel frame."
  (claude-lib-view-live-test--require-gui)
  (claude-lib-view-live-test--with-views
    (let ((result (claude-lib-render "trailing column" :name "live-overlap"
                                     :columns [("Suite" 20 t) ("Where" 20 t)]
                                     :bar-column 1
                                     :rows '((:id w :cells ("windows-test" "modules/")
                                              :bar 282.0)))))
      (with-current-buffer (plist-get result :buffer)
        (let ((window (get-buffer-window (current-buffer) t)))
          (should window)
          (goto-char (point-min))
          (let* ((char-width (frame-char-width (window-frame window)))
                 (hit (claude-lib-view-live-test--image-at-line))
                 (image-pixels (car (image-size (cdr hit) t)))
                 (column-pixels (* claude-lib--view-bar-column-width char-width)))
            (should hit)
            (should (> image-pixels 0))
            (should (<= image-pixels column-pixels))))))))

(provide 'claude-lib-view-live-test)
;;; claude-lib-view-live-test.el ends here
