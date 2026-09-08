;;; claude-lib-view.el --- Render structured data into a navigable Emacs buffer -*- lexical-binding: t -*-

;;; Commentary:
;; The rendering substrate the `claude-lib-' library is built on: one
;; entry point, `claude-lib-render', that turns structured data into an
;; Emacs BUFFER and returns a summary naming that buffer -- never the
;; buffer's contents.  "Rendered 34 callers in *claude-view: callers*"
;; is the whole return value; the artifact itself is on screen, for a
;; human, which is the thing a terminal-only agent cannot reach.
;;
;; TWO RENDER MODES, ONE SUBSTRATE, three shapes chosen from the data:
;;
;;   rows     -- `:rows' with no `:bar' anywhere.  A plain table.
;;   unified  -- `:rows' with at least one `:bar' (or an explicit
;;               `:bar-column').  THE DEFAULT SHAPE: an SVG bar lives
;;               inside a `tabulated-list' cell, so one buffer is both
;;               navigable and illustrated -- point moves by row, RET
;;               jumps to the source behind the row, and the bar sits
;;               beside the label it measures.
;;   image    -- no rows, `:image' only.  The FALLBACK, not the default.
;;               It honestly gives up navigation (point cannot move
;;               through a scatter plot), so it is for data with no
;;               useful row decomposition, and its summary has to
;;               restate the numbers the picture shows.
;;
;; ONE MAJOR MODE, `claude-lib-view-mode', derived from
;; `tabulated-list-mode' -- including for the standalone image, which is
;; rendered as a one-row table rather than through `image-mode'.  Two
;; reasons: evil-collection ships a `tabulated-list' module (see
;; straight/repos/evil-collection/modes/tabulated-list/), so normal
;; state, `q', `S' and `gl'/`gh' are inherited rather than hand-written;
;; and `image-mode' errors "Display does not support images" under
;; `emacs -Q --batch' and drops the buffer to `fundamental-mode', which
;; would make the whole fallback untestable in a batch tier.
;;
;; NO NEW DEPENDENCY.  Emacs 31.1 reports `svg: t' and `png: t' and
;; `svg.el' loads from core; nothing here is installed.  The requires
;; below are core-only and deliberately short.
;;
;; ------------------------------------------------------------------
;; DISPLAYING A VIEW IS A VISIBLE, DISRUPTIVE ACT -- DECISION: ACCEPT.
;;
;; This module declares NO placement.  `modules/windows.el' has one rule
;; with no exceptions (windows.el:10-24): every buffer takes MAIN, and
;; whatever MAIN was showing is pushed onto the top of the right-hand
;; stack.  A push past `edmacs-stack-max-windows' (default 3,
;; windows.el:493) closes the BOTTOM pane -- the least recently
;; displaced -- which can be the agent pane the user is talking to.
;;
;; The decision is ACCEPT: only the WINDOW closes.  The buffer and its
;; process survive untouched and the session is one buffer switch away.
;; The alternative -- declaring a placement through `edmacs-windows-place'
;; -- would reintroduce exactly the per-class unpredictability the
;; uniform rule removed (windows.el:22-24), so this module does not
;; become that mechanism's first caller.
;;
;; The consequence is made observable instead of silent: `claude-lib-render'
;; diffs the frame's window buffers around its own `display-buffer' call
;; and reports whatever lost its last window as `:displaced' in the
;; returned plist, so it is a fact that comes back through the channel
;; rather than a surprise discovered mid-conversation.  `:display nil'
;; is the opt-out -- it builds and populates the buffer and displays
;; nothing.
;;
;; ------------------------------------------------------------------
;; `q' IS ALREADY OWNED.  This module binds no `q' of its own, on
;; purpose.  `special-mode' -> `tabulated-list-mode' ->
;; `claude-lib-view-mode' inherits `q' -> `quit-window', which routes
;; through the `edmacs-stack--quit-restore-window' advice at
;; windows.el:733 (installed at :765): `q' in a stack pane deletes the
;; pane and returns to `edmacs-main-window' rather than risk restoring a
;; stale prior popup.  evil-collection's tabulated-list module binds the
;; same `quit-window'.  Deliberately NOT copied here:
;; `modules/claude-usage.el:614' overrides `q' to `bury-buffer' -- that
;; module wants a bury, this substrate wants the inherited pane-closing
;; behaviour, so the inconsistency is intended and should not be "fixed".
;;
;; ------------------------------------------------------------------
;; THE ROUND TRIP: putting a rendered chart back in front of the model
;; that drew it.  The return value is a summary by design, so nothing
;; about what landed on screen comes back through the tool.  That does
;; not make the rendering unverifiable -- `claude-lib-render-rasterize'
;; closes the loop for CONTENT, and the image never travels through the
;; channel because what comes back is a PATH:
;;
;;   ;; 1. build (over the eval channel)
;;   (claude-lib-render "suite wall clock"
;;                      :name "suites"
;;                      :columns [("Suite" 24 t)]
;;                      :rows '((:id windows :cells ("windows-test") :bar 282.0)
;;                              (:id ui      :cells ("ui-test")      :bar 12.4)))
;;   ;; 2. rasterise one row's bar to a real PNG; returns an absolute path
;;   (claude-lib-render-rasterize "*claude-view: suites*" 'windows)
;;   ;; 3. read that path with an ordinary file-reading tool, which
;;   ;;    renders images.  The picture is now in front of the model.
;;
;; This is worth running rather than assuming.  The first chart produced
;; this way -- four ERT suites as bars against a dashed budget line --
;; had a defect visible only by looking: the "12.4" duration label
;; collided with the budget line drawn across it.  Nothing in the SVG
;; markup says so, and the model that emitted the markup could not have
;; known.
;;
;; WHAT THE LOOP DOES NOT COVER, so it is never over-claimed: it
;; verifies CONTENT, not COMPOSITION.  Rasterising the SVG alone never
;; involves the buffer, so it cannot answer whether the bar column
;; aligns inside a live `tabulated-list', whether `:ascent' sits right
;; against the row's text baseline, or whether the active theme renders
;; a fill invisible against its own background.  Those need a picture of
;; a real frame, which is a dead end on this build -- recorded here so
;; it is not re-derived:
;;
;;   - `x-export-frames' is the clean Emacs-native answer and is
;;     X11/PGTK only; this is an NS build, so it is unbound.
;;   - `screencapture' needs a Screen Recording grant the agent's shell
;;     does not have.
;;   - NEVER call `osascript' from an agent context: it blocks for two
;;     minutes on the Automation prompt rather than failing.
;;   - `scripts/gui-ert.sh' cannot substitute -- its frame is off-screen,
;;     so there is nothing to capture.
;;
;; If Screen Recording is ever granted, the mechanism needs no window
;; id: Emacs reports its own `frame-position' and pixel size.
;;
;; ------------------------------------------------------------------
;; NAMESPACE.  Entry points are `claude-lib-render' and
;; `claude-lib-render-rasterize'; every internal helper is
;; `claude-lib--view-NAME' (double dash) rather than
;; `claude-lib-view--NAME', because the library's entry-point-only
;; regexp is "\\`claude-lib-[^-]" -- the latter shape would match it and
;; fill a listing of what is callable with this file's internals.

;;; Code:

(require 'svg)
(require 'image)
(require 'tabulated-list)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)

;; `evil' loads only in a real init.el session; declared so a bare
;; `batch-byte-compile' of this file is clean, matching
;; `modules/claude-usage.el:69-70'.
(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")

;; Defined by `modules/claude-lib.el', which a `-Q' batch tier need not
;; have loaded; see `claude-lib--view-max-output-bytes'.
(defvar edmacs-claude-lib-max-output-bytes)

;; ============================================================================
;; Customization
;; ============================================================================

(defcustom claude-lib-render-max-rows 1000
  "Most rows `claude-lib-render' will accept in a single view.
Exceeding this ERRORS rather than truncating, matching the convention
`edmacs-claude-lib-max-output-bytes' states in `modules/claude-lib.el':
a silently shortened result looks complete and is worse than a loud
failure.  A 40k-row `tabulated-list' is a hang, not a view."
  :type 'integer
  :group 'claude-lib)

(defcustom claude-lib-render-max-svg-elements 2000
  "Most SVG elements `claude-lib-render' will draw in a single view.
Counted across the standalone image, or summed across every per-row bar
in a unified render.  Exceeding this ERRORS rather than truncating.
The row cap alone does not catch this: 40k rects inside ONE standalone
image is just as unusable, and the output-byte ceiling never catches
either, because the return value is a short summary in both cases."
  :type 'integer
  :group 'claude-lib)

(defcustom claude-lib-render-rsvg-program "rsvg-convert"
  "Program `claude-lib-render-rasterize' shells out to for PNG conversion.
A CONVENIENCE for checking output, never a requirement for producing
it: when this is not on `exec-path', or exits non-zero, the rasteriser
returns the `.svg' path instead and signals nothing."
  :type 'string
  :group 'claude-lib)

(defconst claude-lib--view-bar-pixel-width 120
  "Pixel width of a per-row bar SVG.")

(defconst claude-lib--view-bar-pixel-height 12
  "Pixel height of a per-row bar SVG.")

(defconst claude-lib--view-bar-column-width 22
  "Character width declared for the bar column in `tabulated-list-format'.
Deliberately generous relative to `claude-lib--view-bar-pixel-width': the
cell's pad string sets the column's PADDING in CHARACTERS, while the
image occupies PIXELS, so a column narrower than the image pushes every
following column rightward off the position the header declares for it.
At 120 image pixels this holds down to a 6-pixel character cell.

Unfalsifiable in batch, where a frame accepts the image spec and renders
nothing -- 16 looked fine there and was already too narrow on a real
7-pixel NS frame.  The relationship is pinned in the GUI tier of
`modules/claude-lib-view-live-test.el' instead.")

(defconst claude-lib--view-return-keys
  '(:buffer :mode :rows :summary :displayed :displaced :truncated)
  "The complete, fixed key set of `claude-lib-render's return value.
There is no `:contents', no `:text' and no `:entries' key, ever: the
return value names the buffer and never carries what is in it.  This
list is the allowlist `claude-lib--view-summary-plist' builds from, so
adding a payload key has to be a deliberate edit here rather than a
line that slips into one call site.")

;; ============================================================================
;; Buffer-local state
;; ============================================================================

(defvar-local claude-lib--view-render-mode nil
  "Which of `rows', `unified' or `image' produced this buffer.")

(defvar-local claude-lib--view-locations nil
  "Hash of row id -> normalised source location, for `claude-lib-view-visit'.")

(defvar-local claude-lib--view-image-svg nil
  "The standalone image's SVG dom, in a buffer rendered in `image' mode.")

(defvar-local claude-lib--view-row-svgs nil
  "Alist of (ROW-ID . DOM) for a unified render's per-row bars.
Kept so `claude-lib-render-rasterize' can reach the source markup after
the fact without re-running the caller's data pipeline.")

;; ============================================================================
;; Major mode
;; ============================================================================

;; Added 2026-09-07: the one major mode the substrate renders through, in
;; all three shapes. Derived from `tabulated-list-mode' because
;; evil-collection ships a module for it -- inventing a major mode means
;; hand-writing evil bindings for it. Destination: edmacs.
(define-derived-mode claude-lib-view-mode tabulated-list-mode "Claude-View"
  "Major mode for a `claude-lib-render' view buffer.
Rows, a unified rows-plus-bars table and a standalone image all render
through this one mode.  Binds no `q': `special-mode's `quit-window' is
inherited and routes through `modules/windows.el's
`edmacs-stack--quit-restore-window' advice, which owns pane closing."
  (setq tabulated-list-padding 1)
  ;; Set explicitly even though evil-collection already puts
  ;; `tabulated-list-mode' in normal state: whether a derived mode
  ;; inherits its parent's initial state is an evil implementation
  ;; detail, and normal state on entry is a substrate responsibility.
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'claude-lib-view-mode 'normal)))

;; Bound twice, on purpose. The plain binding serves the evil-absent
;; case (a `-Q' batch tier); it is DEAD in normal state on a real
;; daemon, because evil installs its state keymaps through
;; `emulation-mode-map-alists', which is consulted BEFORE the buffer's
;; local map and whose `evil-normal-state-map' binds RET. Same shadowing
;; hazard documented at `modules/claude-usage.el:613-622'.
(define-key claude-lib-view-mode-map (kbd "RET") #'claude-lib-view-visit)

(with-eval-after-load 'evil
  (evil-define-key 'normal claude-lib-view-mode-map
    (kbd "RET") #'claude-lib-view-visit
    (kbd "gd") #'claude-lib-view-visit))

;; ============================================================================
;; Source locations
;; ============================================================================

(defun claude-lib--view-normalise-location (location index)
  "Normalise LOCATION for the row at INDEX, or signal a `user-error'.
Accepts (FILE . LINE), (FILE LINE COLUMN), (BUFFER . POSITION) with a
real buffer object, or a marker.  Returns a plist:
 (:kind file :file F :line L :column C) or
 (:kind buffer :buffer B :position P).
Normalising at render time means a malformed location fails inside
`claude-lib-render' rather than later, under the user's finger."
  (cond
   ((null location) nil)
   ((markerp location)
    (list :kind 'buffer :buffer (marker-buffer location)
          :position (marker-position location)))
   ((and (consp location) (bufferp (car location)) (integerp (cdr location)))
    (list :kind 'buffer :buffer (car location) :position (cdr location)))
   ((and (consp location) (stringp (car location)) (integerp (cdr location)))
    (list :kind 'file :file (car location) :line (cdr location) :column 0))
   ((and (consp location) (stringp (car location)) (consp (cdr location))
         (integerp (nth 1 location))
         (or (null (nthcdr 2 location)) (integerp (nth 2 location))))
    (list :kind 'file :file (car location) :line (nth 1 location)
          :column (or (nth 2 location) 0)))
   (t
    (user-error "claude-lib-render: row %d has an unrecognised :location %S"
                index location))))

;; Added 2026-09-07: RET-to-source is the substrate's deliverable, not a
;; follow-up -- a view whose rows do not lead anywhere is a screenshot.
;; Destination: edmacs.
(defun claude-lib-view-visit ()
  "Visit the source location behind the row at point and return its buffer.
Reaches the source through `pop-to-buffer' and nothing else, so
`modules/windows.el's uniform rule applies: the source takes MAIN and
this view is pushed onto the stack, still reachable rather than
replaced.  Signals a `user-error' naming the row when the row carries no
location, and a distinct one when its file no longer exists.  A line
number past end of buffer stops at point-max rather than erroring, so a
stale line looks like a jump to the end of the file."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (loc (and claude-lib--view-locations
                   (gethash id claude-lib--view-locations))))
    (unless id
      (user-error "claude-lib-view-visit: point is not on a row"))
    (unless loc
      (user-error "claude-lib-view-visit: row %S carries no :location" id))
    (pcase (plist-get loc :kind)
      ('file
       (let ((file (plist-get loc :file)))
         (unless (file-exists-p file)
           (user-error "claude-lib-view-visit: row %S names a file that no longer exists: %s"
                       id file))
         (let ((buf (find-file-noselect file)))
           (pop-to-buffer buf)
           (goto-char (point-min))
           (forward-line (1- (plist-get loc :line)))
           (move-to-column (or (plist-get loc :column) 0))
           buf)))
      ('buffer
       (let ((buf (plist-get loc :buffer)))
         (unless (buffer-live-p buf)
           (user-error "claude-lib-view-visit: row %S names a buffer that is no longer live" id))
         (pop-to-buffer buf)
         (goto-char (plist-get loc :position))
         buf)))))

;; ============================================================================
;; SVG construction
;; ============================================================================

(defun claude-lib--view-dedupe-root-attributes (dom)
  "Return DOM with duplicate root attribute keys collapsed to the first.
`svg-create' already emits `xmlns' and `xmlns:xlink'.  A caller passing
`:xmlns' -- the idiom a model reaches for -- appends a SECOND one, and
the result is markup Emacs happily displays while `rsvg-convert' rejects
it outright with \"Attribute xmlns redefined\", breaking the round trip
on a picture that looks fine on screen."
  (let ((attrs (dom-attributes dom))
        seen kept)
    (dolist (pair attrs)
      (unless (assq (car pair) seen)
        (push pair seen)
        (push pair kept)))
    (setcar (cdr dom) (nreverse kept))
    dom))

(defun claude-lib--view-attribute-alist (plist)
  "Convert keyword PLIST to an SVG attribute alist of (SYMBOL . STRING)."
  (let (alist)
    (while plist
      (let ((key (car plist))
            (value (cadr plist)))
        (unless (keywordp key)
          (user-error "claude-lib-render: SVG element attribute %S is not a keyword" key))
        (push (cons (intern (substring (symbol-name key) 1))
                    (if (stringp value) value (format "%s" value)))
              alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun claude-lib--view-build-svg (image)
  "Return an SVG dom for IMAGE, which is a dom or a declarative plist.
A declarative IMAGE is (:width W :height H :elements ((TAG ATTRS...) ...)),
where each element's ATTRS are keyword/value pairs -- (rect :x 0 :y 0
:width 100 :height 20 :fill \"steelblue\").  A dom is used as given, with
duplicate root attributes collapsed."
  (cond
   ((and (consp image) (symbolp (car image)) (eq (car image) 'svg))
    (claude-lib--view-dedupe-root-attributes image))
   ((and (consp image) (keywordp (car image)))
    (let ((width (plist-get image :width))
          (height (plist-get image :height))
          (elements (plist-get image :elements)))
      (unless (and (integerp width) (> width 0) (integerp height) (> height 0))
        (user-error "claude-lib-render: :image needs positive integer :width and :height"))
      (let ((dom (svg-create width height)))
        (dolist (element elements)
          (unless (and (consp element) (symbolp (car element)))
            (user-error "claude-lib-render: :image element %S is not (TAG ATTRS...)" element))
          (dom-append-child
           dom (dom-node (car element)
                         (claude-lib--view-attribute-alist (cdr element)))))
        (claude-lib--view-dedupe-root-attributes dom))))
   (t
    (user-error "claude-lib-render: :image is neither an SVG dom nor a (:width :height :elements) plist"))))

(defun claude-lib--view-bar-svg (value max width height)
  "Return a two-rect SVG dom: a track WIDTH wide and a fill sized by VALUE/MAX.
A nil, zero or negative VALUE, or a MAX that is nil or not positive,
renders the bare track rather than dividing by zero or emitting a
negative-width rect -- which some renderers accept and `rsvg-convert'
may not.  A VALUE above MAX clamps to the full width."
  (let* ((ratio (if (and (numberp value) (numberp max) (> max 0))
                    (/ (float value) max)
                  0))
         (fill (min width (max 0 (round (* width ratio)))))
         (dom (svg-create width height)))
    (svg-rectangle dom 0 0 width height :fill "#3a3a3a" :rx 2)
    (when (> fill 0)
      (svg-rectangle dom 0 0 fill height :fill "#4682b4" :rx 2))
    dom))

(defun claude-lib--view-element-count (dom)
  "Count the element nodes DOM contains, not counting DOM's own root node.
Equals the number of shapes a caller supplied, which is what the
`claude-lib-render-max-svg-elements' cap is stated in."
  (let ((n 0))
    (dolist (child (dom-children dom))
      (when (and (consp child) (symbolp (car child)))
        (setq n (+ n 1 (claude-lib--view-element-count child)))))
    n))

(defun claude-lib--view-bar-cell (dom)
  "Return a short pad string carrying DOM as a `display' property.
The property is on this pad string ALONE -- never `insert-image' into a
label and never a whole-row overlay -- so the row's label stays real
buffer text that isearch and evil motions traverse.  The image replaces
only its own cell."
  (propertize "  " 'display (svg-image dom :ascent 'center :scale 1)))

;; ============================================================================
;; Spec validation and mode dispatch
;; ============================================================================

(defun claude-lib--view-max-output-bytes ()
  "Return the shared output ceiling, in bytes.
Reads `edmacs-claude-lib-max-output-bytes' from `modules/claude-lib.el'
rather than defining a second ceiling; falls back only so this module
still loads standalone in a `-Q' test that never loaded that file."
  (or (bound-and-true-p edmacs-claude-lib-max-output-bytes) (* 1024 1024)))

(defun claude-lib--view-mode-for-spec (rows image bar-column)
  "Return the render mode symbol implied by ROWS, IMAGE and BAR-COLUMN.
The single dispatch point: rows with no bar anywhere are `rows', rows
with any `:bar' (or an explicit BAR-COLUMN) are `unified', and IMAGE
without rows is `image'."
  (cond
   ((and rows (or bar-column
                  (seq-some (lambda (row) (numberp (plist-get row :bar))) rows)))
    'unified)
   (rows 'rows)
   (image 'image)
   (t (user-error "claude-lib-render: needs :rows or :image; neither was supplied"))))

(defun claude-lib--view-validate-spec (summary columns rows image)
  "Check SUMMARY, COLUMNS, ROWS and IMAGE, signalling `user-error' on any fault.
Called FIRST, before any buffer is created and before any SVG is built,
so an oversized or malformed spec costs nothing and can leave no
half-rendered buffer behind.  Every failure names the offending row
index rather than surfacing as a signal from inside
`tabulated-list-print'."
  (unless (and (stringp summary) (not (string-empty-p summary)))
    (user-error "claude-lib-render: SUMMARY must be a non-empty string"))
  (when (> (string-bytes summary) (claude-lib--view-max-output-bytes))
    (user-error "claude-lib-render: SUMMARY is %d bytes, over the %d-byte ceiling (edmacs-claude-lib-max-output-bytes)"
                (string-bytes summary) (claude-lib--view-max-output-bytes)))
  (when rows
    (when (> (length rows) claude-lib-render-max-rows)
      (user-error "claude-lib-render: %d rows exceeds claude-lib-render-max-rows (%d)"
                  (length rows) claude-lib-render-max-rows))
    (unless (and (vectorp columns) (> (length columns) 0))
      (user-error "claude-lib-render: :rows needs a non-empty :columns vector"))
    (seq-do-indexed
     (lambda (column index)
       (unless (and (consp column) (stringp (car column)) (integerp (nth 1 column)))
         (user-error "claude-lib-render: column %d is not (NAME WIDTH SORTABLE): %S"
                     index column)))
     columns)
    (let ((seen (make-hash-table :test #'equal))
          (index 0))
      (dolist (row rows)
        (unless (plistp row)
          (user-error "claude-lib-render: row %d is not a plist: %S" index row))
        (let ((id (plist-get row :id))
              (cells (plist-get row :cells)))
          (unless id
            (user-error "claude-lib-render: row %d has no :id" index))
          (when (gethash id seen)
            ;; A duplicate id silently breaks `tabulated-list-get-id'
            ;; navigation: two rows resolve to one source location with
            ;; no error anywhere.
            (user-error "claude-lib-render: row %d repeats the :id %S already used by row %d"
                        index id (gethash id seen)))
          (puthash id index seen)
          (unless (and (listp cells) (= (length cells) (length columns)))
            (user-error "claude-lib-render: row %d has %d cells but :columns declares %d"
                        index (length cells) (length columns)))
          (unless (seq-every-p #'stringp cells)
            (user-error "claude-lib-render: row %d has a non-string cell: %S" index cells))
          (let ((bar (plist-get row :bar)))
            (unless (or (null bar) (numberp bar))
              (user-error "claude-lib-render: row %d has a non-numeric :bar: %S" index bar)))
          (claude-lib--view-normalise-location (plist-get row :location) index))
        (setq index (1+ index)))))
  (when (and (null rows) (null image))
    (user-error "claude-lib-render: needs :rows or :image; neither was supplied")))

;; ============================================================================
;; Buffers
;; ============================================================================

(defun claude-lib--view-slug (string)
  "Return STRING as a short lowercase slug fit for a buffer name.
Falls back to \"view\" when STRING carries no alphanumerics at all, so a
punctuation-only SUMMARY cannot produce the nameless `*claude-view: *'."
  (let ((slug (string-trim
               (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" string))
               "-+" "-+")))
    (if (string-empty-p slug) "view" (substring slug 0 (min 32 (length slug))))))

(defun claude-lib--view-buffer-name (name)
  "Return the view buffer name for NAME."
  (format "*claude-view: %s*" name))

(defun claude-lib--view-get-buffer (name)
  "Return the buffer to render NAME into, reusing an existing view in place.
A live `claude-lib-view-mode' buffer of that name is reused so a
re-render does not accumulate `<2>', `<3>' copies; a buffer of that name
in any other mode -- a file the user happened to open -- is never
clobbered, and a fresh one is generated instead."
  (let* ((bufname (claude-lib--view-buffer-name name))
         (existing (get-buffer bufname)))
    (if (and existing
             (buffer-live-p existing)
             (eq (buffer-local-value 'major-mode existing) 'claude-lib-view-mode))
        existing
      (if existing (generate-new-buffer bufname) (get-buffer-create bufname)))))

(defun claude-lib--view-window-buffers (frame)
  "Return the buffers showing in a live FRAME's non-minibuffer windows."
  (when (frame-live-p frame)
    (mapcar #'window-buffer (window-list frame 'no-minibuf))))

(defun claude-lib--view-displaced-buffers (before after view-buffer)
  "Return names of buffers in BEFORE that hold no window in AFTER.
VIEW-BUFFER is excluded: it is the thing that was just displayed, not
something the display pushed out."
  (delq nil
        (mapcar (lambda (buf)
                  (and (buffer-live-p buf)
                       (not (eq buf view-buffer))
                       (not (memq buf after))
                       (buffer-name buf)))
                (seq-uniq before))))

;; ============================================================================
;; Summary
;; ============================================================================

(defun claude-lib--view-format-number (value)
  "Format VALUE for the numeric column beside a bar."
  (cond ((integerp value) (number-to-string value))
        ((numberp value) (format "%.1f" value))
        (t "")))

(defun claude-lib--view-summary-text (summary mode bufname rows image-dom)
  "Return SUMMARY with a derived tail restating what MODE put in BUFNAME.
The numbers are restated in TEXT because the buffer is a surface the
model cannot read back: without this the model would not retain what it
rendered unless somebody rasterised it.  ROWS and IMAGE-DOM supply the
figures."
  (pcase mode
    ('image
     (format "%s (%s: image %sx%s, %d elements)"
             summary bufname
             (dom-attr image-dom 'width) (dom-attr image-dom 'height)
             (claude-lib--view-element-count image-dom)))
    ('unified
     (let* ((bars (seq-filter (lambda (row) (numberp (plist-get row :bar))) rows))
            (sorted (sort (copy-sequence bars)
                          (lambda (a b) (> (plist-get a :bar) (plist-get b :bar)))))
            (label (lambda (row)
                     (format "%s %s"
                             (or (car (plist-get row :cells)) (plist-get row :id))
                             (claude-lib--view-format-number (plist-get row :bar))))))
       (if sorted
           (format "%s (%s: %d rows; max %s, min %s)"
                   summary bufname (length rows)
                   (funcall label (car sorted))
                   (funcall label (car (last sorted))))
         (format "%s (%s: %d rows)" summary bufname (length rows)))))
    (_ (format "%s (%s: %d rows)" summary bufname (length rows)))))

(defun claude-lib--view-summary-plist (bufname mode row-count summary displayed displaced)
  "Return the fixed-shape value `claude-lib-render' hands back.
Built from the `claude-lib--view-return-keys' allowlist rather than
assembled inline, so a later edit that wants to add a payload has to
change that list deliberately.  BUFNAME, MODE, ROW-COUNT, SUMMARY,
DISPLAYED and DISPLACED are the only facts that travel."
  (let ((values (list :buffer bufname
                      :mode mode
                      :rows row-count
                      :summary summary
                      :displayed displayed
                      :displaced displaced
                      ;; Reserved and always nil: every cap here ERRORS
                      ;; rather than truncating. Present so the key set
                      ;; is frozen for this substrate's consumers.
                      :truncated nil))
        result)
    (dolist (key (reverse claude-lib--view-return-keys))
      (setq result (cons key (cons (plist-get values key) result))))
    result))

;; ============================================================================
;; The entry points
;; ============================================================================

;; Added 2026-09-07: "custom ways to look at a project" needs a function
;; that does not return a string but BUILDS A BUFFER -- the artifact goes
;; on screen for a human and only a summary comes back through the tool
;; channel. Destination: edmacs.
(defun claude-lib-render (summary &rest keys)
  "Render SUMMARY's data into a buffer and return a summary plist naming it.
The return value NEVER contains the buffer's contents: it is
 (:buffer NAME :mode MODE :rows N :summary TEXT :displayed BOOL
  :displaced (NAME...) :truncated nil).
The unified rows-plus-bars shape is the DEFAULT and the one to prefer --
a malformed table degrades far more gracefully than a malformed picture.
The standalone image is a fallback for data with no useful row
decomposition; it gives up navigation, so its SUMMARY must restate the
numbers the picture shows.

KEYS is a plist:
  :columns     a `tabulated-list-format'-shaped vector of (NAME WIDTH SORT).
  :rows        a list of plists (:id ID :cells (\"a\" \"b\") :bar NUMBER
               :location LOC).  ID must be unique.  :cells must have one
               string per :columns entry -- the bar columns are the
               substrate's to fill.  LOC is (FILE . LINE), (FILE LINE
               COLUMN), (BUFFER . POSITION) or a marker.
  :image       an SVG dom, or a declarative
               (:width W :height H :elements ((TAG ATTRS...) ...)).
  :bar-max     the value a full bar represents; derived from the rows when
               omitted.
  :bar-column  index at which to insert the value/bar column pair;
               appended when omitted.
  :name        buffer base name, slugged from SUMMARY when omitted.
  :display     display the buffer (default t).  nil builds and populates
               it and displays nothing.

The mode is chosen from the data: rows with no :bar render `rows', rows
with any :bar (or an explicit :bar-column) render `unified' with an SVG
bar inside the `tabulated-list' cell, and :image with no rows renders
`image' as a single-row table.  RET and `gd' visit the :location behind
the row in both row modes.

Two caps ERROR rather than truncate: `claude-lib-render-max-rows' and
`claude-lib-render-max-svg-elements'.  Both are checked before any
buffer exists, so a rejected spec leaves nothing on screen.

Displaying a view is a visible act.  The view takes MAIN and pushes what
MAIN held onto the stack, and a push past `edmacs-stack-max-windows' can
close the bottom pane -- possibly an agent pane, whose buffer and process
survive even though its window does not.  Whatever lost its last window
is reported in :displaced; a name there may well be an agent's."
  (let* ((columns (plist-get keys :columns))
         (rows (plist-get keys :rows))
         (image (plist-get keys :image))
         (bar-column (plist-get keys :bar-column))
         (bar-max (plist-get keys :bar-max))
         (name (or (plist-get keys :name) (claude-lib--view-slug summary)))
         (display (if (plist-member keys :display) (plist-get keys :display) t)))
    (claude-lib--view-validate-spec summary columns rows image)
    (let ((mode (claude-lib--view-mode-for-spec rows image bar-column))
          image-dom row-svgs format entries)
      ;; Everything that can fail is done before a buffer exists.
      (pcase mode
        ('image
         (setq image-dom (claude-lib--view-build-svg image))
         (let ((count (claude-lib--view-element-count image-dom)))
           (when (> count claude-lib-render-max-svg-elements)
             (user-error "claude-lib-render: :image has %d elements, over claude-lib-render-max-svg-elements (%d)"
                         count claude-lib-render-max-svg-elements)))
         (setq format (vector (list "Image" 60 nil))
               entries (list (list 'image
                                   (vector (claude-lib--view-bar-cell image-dom))))))
        ('unified
         (let* ((values (seq-filter #'numberp
                                    (mapcar (lambda (row) (plist-get row :bar)) rows)))
                ;; Not named `max': shadowing the function of that name
                ;; inside this `let*' is exactly the sort of thing that
                ;; reads fine and breaks on the next edit.
                (ceiling (or bar-max (and values (apply #'max values))))
                (total 0))
           (dolist (row rows)
             (let ((dom (claude-lib--view-bar-svg (plist-get row :bar) ceiling
                                                  claude-lib--view-bar-pixel-width
                                                  claude-lib--view-bar-pixel-height)))
               (setq total (+ total (claude-lib--view-element-count dom)))
               (push (cons (plist-get row :id) dom) row-svgs)))
           (setq row-svgs (nreverse row-svgs))
           (when (> total claude-lib-render-max-svg-elements)
             (user-error "claude-lib-render: per-row bars total %d elements, over claude-lib-render-max-svg-elements (%d)"
                         total claude-lib-render-max-svg-elements))
           (let* ((at (or bar-column (length columns)))
                  (value-index at)
                  ;; The bar cell is a pad string, so sorting it would
                  ;; sort nothing. The numeric value gets its own
                  ;; sortable column beside it, with a numeric -- not
                  ;; lexicographic -- predicate.
                  (pred (lambda (a b)
                          (< (string-to-number (aref (cadr a) value-index))
                             (string-to-number (aref (cadr b) value-index)))))
                  (head (seq-take (append columns nil) at))
                  (tail (seq-drop (append columns nil) at)))
             (setq format (vconcat head
                                   (list (list "Value" 9 pred)
                                         (list "Bar" claude-lib--view-bar-column-width nil))
                                   tail))
             (setq entries
                   (mapcar
                    (lambda (row)
                      (let ((cells (append (plist-get row :cells) nil)))
                        (list (plist-get row :id)
                              (vconcat (seq-take cells at)
                                       (list (claude-lib--view-format-number
                                              (plist-get row :bar))
                                             (claude-lib--view-bar-cell
                                              (alist-get (plist-get row :id) row-svgs
                                                         nil nil #'equal)))
                                       (seq-drop cells at)))))
                    rows)))))
        ('rows
         (setq format (vconcat columns)
               entries (mapcar (lambda (row)
                                 (list (plist-get row :id)
                                       (vconcat (plist-get row :cells))))
                               rows))))
      (let* ((buffer (claude-lib--view-get-buffer name))
             (bufname (buffer-name buffer))
             (frame (selected-frame))
             (before (claude-lib--view-window-buffers frame))
             displayed displaced)
        ;; Anything signalling from here on has already produced a
        ;; buffer, so kill it rather than leave a half-rendered view.
        (condition-case err
            (with-current-buffer buffer
              (let ((inhibit-read-only t)) (erase-buffer))
              (claude-lib-view-mode)
              (setq claude-lib--view-render-mode mode
                    claude-lib--view-image-svg image-dom
                    claude-lib--view-row-svgs row-svgs
                    claude-lib--view-locations (make-hash-table :test #'equal))
              (let ((index 0))
                (dolist (row rows)
                  (let ((loc (claude-lib--view-normalise-location
                              (plist-get row :location) index)))
                    (when loc
                      (puthash (plist-get row :id) loc claude-lib--view-locations)))
                  (setq index (1+ index))))
              (setq tabulated-list-format format)
              (tabulated-list-init-header)
              (setq tabulated-list-entries entries)
              (tabulated-list-print)
              (goto-char (point-min)))
          (error (kill-buffer buffer) (signal (car err) (cdr err))))
        (when display
          ;; No placement declared, deliberately: windows.el's one rule
          ;; takes MAIN and pushes what was there onto the stack.
          (display-buffer buffer)
          (setq displayed (and (get-buffer-window buffer t) t))
          (setq displaced (claude-lib--view-displaced-buffers
                           before (claude-lib--view-window-buffers frame) buffer)))
        (claude-lib--view-summary-plist
         bufname mode (length rows)
         (claude-lib--view-summary-text summary mode bufname rows image-dom)
         displayed displaced)))))

;; Added 2026-09-07: the agent writes to a surface it cannot read back,
;; which does not make the drawing unverifiable -- an image cannot travel
;; through `emacsclient -e', but a PATH can, and the read then happens
;; outside the channel. Destination: edmacs.
(defun claude-lib-render-rasterize (buffer-or-name &optional row-id dir)
  "Write BUFFER-OR-NAME's SVG to a file and return that file's absolute path.
Returns a PATH string, never image data: `emacsclient -e' returns the
printed representation of a value, so an image cannot come back through
the channel at all -- read the returned path with an ordinary
file-reading tool, which renders images.

With no ROW-ID the buffer's standalone image is used; with a ROW-ID the
per-row bar of that row.  DIR defaults to `temporary-file-directory'.

The path is a `.png' when `claude-lib-render-rsvg-program' is on
`exec-path' and converts cleanly, and the `.svg' otherwise -- an absent
or failing converter is reported with `message' and never signals.  That
program is a convenience for CHECKING output, never a requirement for
producing it, and the SVG path is useful on its own."
  (let* ((buffer (get-buffer buffer-or-name)))
    (unless (buffer-live-p buffer)
      (user-error "claude-lib-render-rasterize: no live buffer %S" buffer-or-name))
    (let* ((image-svg (buffer-local-value 'claude-lib--view-image-svg buffer))
           (row-svgs (buffer-local-value 'claude-lib--view-row-svgs buffer))
           (dom (if row-id
                    (alist-get row-id row-svgs nil nil #'equal)
                  image-svg)))
      (unless dom
        (let ((ids (mapcar #'car row-svgs)))
          (if (null ids)
              (user-error "claude-lib-render-rasterize: %s has no SVG to rasterise"
                          (buffer-name buffer))
            ;; Capped: a 1000-row view would otherwise put 1000 ids back
            ;; through the channel, defeating the point of the caps.
            (user-error "claude-lib-render-rasterize: no such row %S in %s; ids include %s (%d total)"
                        row-id (buffer-name buffer)
                        (mapconcat (lambda (id) (format "%S" id)) (seq-take ids 20) ", ")
                        (length ids)))))
      (let* ((dir (expand-file-name (or dir temporary-file-directory)))
             (stem (expand-file-name (make-temp-name "claude-view-") dir))
             (svg-path (concat stem ".svg"))
             (png-path (concat stem ".png")))
        (condition-case err
            (with-temp-file svg-path (svg-print dom))
          (error (user-error "claude-lib-render-rasterize: cannot write into %s: %s"
                             dir (error-message-string err))))
        (let ((program (executable-find claude-lib-render-rsvg-program)))
          (cond
           ((null program)
            (message "claude-lib-render-rasterize: %s not found; PNG conversion skipped, returning SVG"
                     claude-lib-render-rsvg-program)
            svg-path)
           ((and (zerop (call-process program nil nil nil "-o" png-path svg-path))
                 (file-exists-p png-path)
                 (> (file-attribute-size (file-attributes png-path)) 0))
            png-path)
           (t
            (message "claude-lib-render-rasterize: %s failed; PNG conversion skipped, returning SVG"
                     claude-lib-render-rsvg-program)
            svg-path)))))))

(provide 'claude-lib-view)
;;; claude-lib-view.el ends here
