;;; sidebar-buffers.el --- Per-tab buffer tree in the sidebar -*- lexical-binding: t -*-

;;; Commentary:
;; Phase 7 of the edmacs-sidebar roadmap: renders each OPEN worktree's own
;; tab buffer list (via `bufferlo-buffer-list') as a `buffers' child
;; section underneath that worktree's row -- the same
;; `edmacs-sidebar-worktree-section-functions' extension point
;; `sidebar-agents.el' (phase 6) uses for its own `agents' child section.
;;
;; File/dired buffers render as a directory tree (grouped by path
;; relative to the worktree root, single-child chains flattened, nodes
;; beyond a rendered depth of two folded by default); everything else
;; (`*Messages*', magit, compilation, ...) trails in a flat, dimmed
;; group. Ordering throughout comes from the tab's own MAIN window's
;; `window-prev-buffers' stack (live windows for the current tab, the
;; tab's own serialized window-state tree for a background tab) --
;; independent of `bufferlo-buffer-list's own (unordered) enumeration,
;; which supplies membership only.
;;
;; `RET' shows the buffer via `windows.el's `edmacs-window-pop-buffer-to-main',
;; switching tab first if needed; `d' kills it (`kill-buffer' already
;; prompts on a modified buffer); `[' / `]' drive the tab's main window
;; through the exact `previous-buffer'/`next-buffer' commands `SPC b p'/
;; `SPC b n' already run, and follow point to the resulting row. A
;; per-frame `s' toggles between the directory tree and a flat
;; `window-prev-buffers'-ordered stack.
;;
;; Two independent redraw paths: a 0.2s debounced FULL rebuild off
;; `buffer-list-update-hook' (a burst of buffer opens redraws once), and
;; an immediate, cheap, overlay-only marker refresh off
;; `window-selection-change-functions'/`window-buffer-change-functions'
;; that only touches the visible/modified/selected glyphs on existing
;; rows -- no `erase-buffer', so fold state and scroll position survive.
;;
;; Run pure-function tests:
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/sidebar-buffers-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

;; See sidebar-agents.el's own Commentary for why this require is needed
;; even though sidebar.el (loaded first, per init.el's `load-module'
;; order) has already required it -- this file's own standalone test
;; harness loads it directly.
(require 'magit-section)

;; ============================================================================
;; Forward declarations
;; ============================================================================
;; Every one of these resolves at real init.el runtime (windows.el loads
;; well before this module; frames.el loads right after it -- see
;; init.el's own comment on this module's load line) -- declared for
;; byte-compile hygiene only, mirroring sidebar.el's and
;; sidebar-agents.el's own forward-ref blocks.

(declare-function bufferlo-buffer-list "bufferlo")
(declare-function edmacs-frames--tab-for-root "frames")
(declare-function edmacs-main-window "windows")
(declare-function edmacs-window-pop-buffer-to-main "windows")
(declare-function edmacs-sidebar--buffer "sidebar")
(declare-function edmacs-sidebar--redraw "sidebar")
(declare-function edmacs-sidebar--goto-identity "sidebar")
(defvar edmacs-sidebar-worktree-section-functions)
(defvar edmacs-sidebar-extra-section-functions)

(defgroup edmacs-sidebar-buffers nil
  "Per-tab buffer tree in the sidebar."
  :group 'edmacs-sidebar)

;; ============================================================================
;; Faces
;; ============================================================================

(defface edmacs-sidebar-buffers-special-face
  '((t :inherit shadow))
  "Face for a non-file-like buffer's row (`*Messages*', magit, compilation, ...)."
  :group 'edmacs-sidebar-buffers)

(defface edmacs-sidebar-buffers-selected-face
  '((t :inherit highlight))
  "Face for the row of the buffer shown in the frame's selected window."
  :group 'edmacs-sidebar-buffers)

;; ============================================================================
;; Buffer classification
;; ============================================================================

(defun edmacs-sidebar-buffers--file-like-p (buf)
  "Non-nil when BUF is file-visiting or a `dired-mode' buffer.
Everything else -- `*Messages*', magit-status, compilation, Help, ... --
is \"special\": no mode-specific special-casing needed beyond this one
predicate. Reads buffer-local values directly rather than switching
into BUF, so classifying a whole buffer list never touches the current
buffer."
  (and (buffer-live-p buf)
       (or (buffer-local-value 'buffer-file-name buf)
           (provided-mode-derived-p (buffer-local-value 'major-mode buf) 'dired-mode))))

(defun edmacs-sidebar-buffers--raw-path (buf)
  "Return BUF's own file or directory path, or nil.
`dired-directory' is occasionally a (DIR . FILES) cons (a dired buffer
listing explicit files/wildcards) rather than a bare string; only DIR
matters for placement in the tree."
  (or (buffer-local-value 'buffer-file-name buf)
      (when (provided-mode-derived-p (buffer-local-value 'major-mode buf) 'dired-mode)
        (let ((dd (buffer-local-value 'dired-directory buf)))
          (cond ((stringp dd) dd)
                ((consp dd) (car dd))
                (t (buffer-local-value 'default-directory buf)))))))

(defun edmacs-sidebar-buffers--path (buf root)
  "Return BUF's path relative to ROOT, or nil if BUF has no path at all.
A result starting with \"../\" means BUF's file lives outside the
worktree -- rendered as a single top-level entry under that literal
relative path rather than folded into the tree, see `--build-tree'."
  (let ((raw (edmacs-sidebar-buffers--raw-path buf)))
    (and raw (directory-file-name (file-relative-name raw root)))))

(defun edmacs-sidebar-buffers--outside-root-p (rel)
  "Non-nil when REL (from `--path') names something outside the worktree."
  (or (equal rel "..") (string-prefix-p "../" rel)))

;; ============================================================================
;; Ordering: window-prev-buffers, live window or serialized ws-tree
;; ============================================================================

(defun edmacs-sidebar-buffers--current-tab-main-window (frame)
  "Return FRAME's main window (the one carrying the `edmacs-main'
parameter), or nil. Frame-parameterized rather than reusing
`windows.el's own `edmacs-main-window' (which always targets the
selected frame) -- a redraw is not always on the selected frame."
  (seq-find (lambda (w) (window-parameter w 'edmacs-main))
            (window-list frame 'no-minibuf)))

(defun edmacs-sidebar-buffers--ws-main-prev-buffers (ws)
  "Return (MARKED . PREV-BUFFERS) for the leaf in WS marked `edmacs-main'.
WS is a tab's own serialized `window-state-get' tree (its `ws' field).
Recurses through `vc'/`hc' combination nodes exactly like frames.el's
own `edmacs-frames--ws-selected-buffer-name', falling back to the first
leaf found when nothing is marked `edmacs-main' (a tab created before
windows.el's convention existed, or one with no main window for any
other reason) -- PREV-BUFFERS is a list of (NAME START POINT), the
writable form `window-state-get's WRITABLE argument produces."
  (pcase ws
    (`(leaf . ,params)
     (let* ((leaf-params (alist-get 'parameters params))
            (marked (and (consp leaf-params) (alist-get 'edmacs-main leaf-params))))
       (cons (and marked t) (alist-get 'prev-buffers params))))
    (`(,(or 'vc 'hc) . ,rest)
     (let (found first)
       (dolist (child rest)
         (when (and (consp child) (memq (car child) '(leaf vc hc)))
           (let* ((result (edmacs-sidebar-buffers--ws-main-prev-buffers child))
                  (marked (car result)) (pbs (cdr result)))
             (unless first (setq first (cons t pbs)))
             (when marked (setq found (cons t pbs))))))
       (or found first (cons nil nil))))
    (_ (cons nil nil))))

(defun edmacs-sidebar-buffers--main-window-prev-names (frame tab)
  "Return TAB's main window's `window-prev-buffers' names, most-recent-first.
For the current tab (TAB's car is the symbol `current-tab'), reads the
live window carrying `edmacs-main' in FRAME -- `window-prev-buffers' already
conses new entries onto the front, so no reversal is needed. For any
other tab, walks its own serialized `ws' tree instead, since it was
never switched to and has no live windows at all."
  (when tab
    (if (eq (car tab) 'current-tab)
        (let ((win (edmacs-sidebar-buffers--current-tab-main-window frame)))
          (when win
            (delq nil (mapcar (lambda (e) (and (buffer-live-p (car e)) (buffer-name (car e))))
                               (window-prev-buffers win)))))
      ;; `tab's own `ws' field is `window-state-get's raw return, a cons
      ;; of (CONSTRAINTS-ALIST . STATE-TREE) regardless of the WRITABLE
      ;; argument tab-bar.el passes -- only the `cdr' is the `(leaf ...)'/
      ;; `(vc|hc ...)' tree `--ws-main-prev-buffers' pattern-matches on.
      (let ((ws (alist-get 'ws tab)))
        (when ws
          (mapcar #'car (cdr (edmacs-sidebar-buffers--ws-main-prev-buffers (cdr ws)))))))))

(defun edmacs-sidebar-buffers--rank (buf prev-names)
  "Return BUF's position in PREV-NAMES, or `most-positive-fixnum' if absent.
Realizes the \"not in the list go last, alphabetically\" rule: sorting
on (RANK . NAME) pairs everywhere puts every ranked buffer ahead of
every unranked one, then falls back to name."
  (or (cl-position (buffer-name buf) prev-names :test #'equal)
      most-positive-fixnum))

;; ============================================================================
;; Directory tree: build, flatten single-child chains, sort
;; ============================================================================
;; A tree node is a plist: file nodes are (:kind file :name NAME :buf BUF);
;; dir nodes are (:kind dir :seg SEGMENT :children NODES). SEGMENT may
;; itself be a "/"-joined chain once `--flatten-chains' has merged
;; consecutive single-child directories.

(defun edmacs-sidebar-buffers--tree-insert (nodes segments buf)
  "Return NODES with BUF inserted at SEGMENTS (a list of path components)."
  (if (null (cdr segments))
      (cons (list :kind 'file :name (car segments) :buf buf) nodes)
    (let* ((seg (car segments))
           (existing (seq-find (lambda (n) (and (eq (plist-get n :kind) 'dir)
                                                  (equal (plist-get n :seg) seg)))
                                nodes)))
      (if existing
          (cons (list :kind 'dir :seg seg
                       :children (edmacs-sidebar-buffers--tree-insert
                                  (plist-get existing :children) (cdr segments) buf))
                (remq existing nodes))
        (cons (list :kind 'dir :seg seg
                     :children (edmacs-sidebar-buffers--tree-insert nil (cdr segments) buf))
              nodes)))))

(defun edmacs-sidebar-buffers--build-tree (file-bufs root)
  "Build a raw (unsorted, unflattened) tree from FILE-BUFS relative to ROOT.
A buffer whose relative path escapes ROOT (`--outside-root-p') becomes
one top-level leaf keyed by its whole literal relative path, rather
than being split into segments -- see `--path'."
  (let (nodes)
    (dolist (buf file-bufs)
      (let* ((rel (edmacs-sidebar-buffers--path buf root))
             (segments (cond
                        ((null rel) (list (buffer-name buf)))
                        ((edmacs-sidebar-buffers--outside-root-p rel) (list rel))
                        (t (split-string rel "/" t)))))
        (setq nodes (edmacs-sidebar-buffers--tree-insert nodes segments buf))))
    nodes))

(defun edmacs-sidebar-buffers--merge-chains (node)
  "Merge NODE's consecutive single-dir-child chains into one dir node
whose SEGMENT is the \"/\"-joined chain, recursing until a real branch
\(more than one child) or a lone file child is reached. File nodes pass
through unchanged."
  (if (eq (plist-get node :kind) 'file)
      node
    (let ((children (mapcar #'edmacs-sidebar-buffers--merge-chains (plist-get node :children))))
      (if (and (= (length children) 1) (eq (plist-get (car children) :kind) 'dir))
          (edmacs-sidebar-buffers--merge-chains
           (list :kind 'dir
                 :seg (concat (plist-get node :seg) "/" (plist-get (car children) :seg))
                 :children (plist-get (car children) :children)))
        (list :kind 'dir :seg (plist-get node :seg) :children children)))))

(defun edmacs-sidebar-buffers--collapse-lone-files (node)
  "After chain-merging, collapse a dir node whose only (chain-merged)
child is a lone FILE into one file section, heading \"DIR-CHAIN/
FILENAME\" -- e.g. the given \"claude-repl/ claude-repl-buffer.el\"
example. A dir node with any other shape (a real branch, or no
children) is left as a dir node."
  (if (eq (plist-get node :kind) 'file)
      node
    (let ((children (mapcar #'edmacs-sidebar-buffers--collapse-lone-files
                             (plist-get node :children))))
      (if (and (= (length children) 1) (eq (plist-get (car children) :kind) 'file))
          (list :kind 'file
                :buf (plist-get (car children) :buf)
                :name (concat (plist-get node :seg) "/ " (plist-get (car children) :name)))
        (list :kind 'dir :seg (plist-get node :seg) :children children)))))

(defun edmacs-sidebar-buffers--flatten-chains (nodes)
  "Apply chain-merging then lone-file collapse to every node in NODES."
  (mapcar (lambda (n) (edmacs-sidebar-buffers--collapse-lone-files
                        (edmacs-sidebar-buffers--merge-chains n)))
          nodes))

(defun edmacs-sidebar-buffers--node-rank (node prev-names)
  "Return NODE's sort rank: its own rank for a file, the min over every
descendant file's rank for a dir (so a directory sorts as recently as
its most-recently-visited member)."
  (pcase (plist-get node :kind)
    ('file (edmacs-sidebar-buffers--rank (plist-get node :buf) prev-names))
    ('dir (let ((ranks (mapcar (lambda (c) (edmacs-sidebar-buffers--node-rank c prev-names))
                                (plist-get node :children))))
            (if ranks (apply #'min ranks) most-positive-fixnum)))))

(defun edmacs-sidebar-buffers--node-name (node)
  "Return NODE's own display name, for sorting ties."
  (pcase (plist-get node :kind)
    ('file (plist-get node :name))
    ('dir (plist-get node :seg))))

(defun edmacs-sidebar-buffers--sort-siblings (nodes prev-names)
  "Sort NODES (one tree level): directories before files -- a file-tree
convention not stated explicitly by the phase body, but needed to
satisfy AC1's own worked example (a directory sorts ahead of a
same-level file even when the file was visited more recently) -- each
group internally by (rank . name) ascending, most-recently-visited first."
  (cl-flet ((cmp (a b)
              (let ((ra (edmacs-sidebar-buffers--node-rank a prev-names))
                    (rb (edmacs-sidebar-buffers--node-rank b prev-names)))
                (if (/= ra rb) (< ra rb)
                  (string< (edmacs-sidebar-buffers--node-name a)
                           (edmacs-sidebar-buffers--node-name b))))))
    (let ((dirs (seq-filter (lambda (n) (eq (plist-get n :kind) 'dir)) nodes))
          (files (seq-filter (lambda (n) (eq (plist-get n :kind) 'file)) nodes)))
      (append (sort (copy-sequence dirs) #'cmp)
              (sort (copy-sequence files) #'cmp)))))

(defun edmacs-sidebar-buffers--sort-bufs (bufs prev-names)
  "Sort BUFS (a flat buffer list) by (rank . name) ascending."
  (sort (copy-sequence bufs)
        (lambda (a b)
          (let ((ra (edmacs-sidebar-buffers--rank a prev-names))
                (rb (edmacs-sidebar-buffers--rank b prev-names)))
            (if (/= ra rb) (< ra rb) (string< (buffer-name a) (buffer-name b)))))))

;; ============================================================================
;; Point-identity buffer case (extends sidebar.el's own mechanism)
;; ============================================================================
;; sidebar.el's `edmacs-sidebar--point-identity'/`--goto-identity' are
;; extended (in sidebar.el itself) with a `(buffer . NAME)' case that
;; needs no function from this file -- only the type symbols
;; `edmacs-sidebar-buffers-file'/`-special' below, which sidebar.el
;; checks structurally via `oref', not by calling into this module.

;; ============================================================================
;; Row overlays: marker glyph / modified asterisk / selected highlight
;; ============================================================================
;; Rendered as overlay before/after-strings and an overlay face, not text
;; properties on the row's own text, so the cheap refresh path
;; (`--refresh-markers') can update them in place without touching buffer
;; text -- no `erase-buffer', so fold state and scroll position survive.

(defvar-local edmacs-sidebar-buffers--row-overlays nil
  "Hash table BUFFER -> its row's marker overlay, in this sidebar buffer.
Repopulated once per redraw pass -- see `--ensure-cleared-this-pass'.")

(defvar-local edmacs-sidebar-buffers--cleared-this-pass nil
  "Non-nil once this redraw pass has already cleared stale overlays.
Reset to nil by `--reset-cleared-flag', registered on
`edmacs-sidebar-extra-section-functions' (fires once at the end of every
`edmacs-sidebar--redraw' pass, whether or not this module rendered
anything), so the NEXT pass clears exactly once too.")

(defun edmacs-sidebar-buffers--ensure-cleared-this-pass ()
  "Clear every stale row overlay exactly once per redraw pass.
Safe to call from every worktree's `--on-worktree-section' invocation:
the first call within a pass finds nothing of this module's own in the
buffer yet (whatever was there is left over from the LAST pass), and
every later call in the same pass is a no-op."
  (unless edmacs-sidebar-buffers--cleared-this-pass
    (remove-overlays (point-min) (point-max))
    (setq edmacs-sidebar-buffers--row-overlays (make-hash-table :test 'eq)
          edmacs-sidebar-buffers--cleared-this-pass t)))

(defun edmacs-sidebar-buffers--reset-cleared-flag (frame)
  "Arm the NEXT redraw pass of FRAME's sidebar to clear overlays again."
  (let ((buf (and (fboundp 'edmacs-sidebar--buffer) (edmacs-sidebar--buffer frame))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq edmacs-sidebar-buffers--cleared-this-pass nil)))))

(add-hook 'edmacs-sidebar-extra-section-functions #'edmacs-sidebar-buffers--reset-cleared-flag)

(defun edmacs-sidebar-buffers--decorate-overlay (ov buf frame)
  "Set OV's marker/asterisk/highlight to BUF's current state in FRAME.
`get-buffer-window'/`window-buffer' answer \"is this buffer showing in a
live window of FRAME\" correctly by construction: a non-current tab has
no live windows at all (tab-bar tears down the whole window tree on
select), so a row under a background tab's subsection is never marked
visible/selected without any extra current-tab check needed here."
  (let* ((visible (and (buffer-live-p buf) (get-buffer-window buf frame)))
         (sel-win (frame-selected-window frame))
         (selected (and (buffer-live-p buf) (window-live-p sel-win)
                        (eq buf (window-buffer sel-win))))
         (modified (and (buffer-live-p buf) (buffer-modified-p buf))))
    (overlay-put ov 'before-string (if visible "● " "  "))
    (overlay-put ov 'after-string (if modified "*" ""))
    (overlay-put ov 'face (and selected 'edmacs-sidebar-buffers-selected-face))))

(defun edmacs-sidebar-buffers--register-row (buf beg end frame)
  "Create and decorate BUF's row overlay spanning [BEG END)."
  (let ((ov (make-overlay beg end)))
    (unless (hash-table-p edmacs-sidebar-buffers--row-overlays)
      (setq edmacs-sidebar-buffers--row-overlays (make-hash-table :test 'eq)))
    (puthash buf ov edmacs-sidebar-buffers--row-overlays)
    (edmacs-sidebar-buffers--decorate-overlay ov buf frame)))

(defun edmacs-sidebar-buffers--refresh-markers (frame)
  "Update every row overlay in FRAME's sidebar in place -- no redraw."
  (let ((buf (and (fboundp 'edmacs-sidebar--buffer) (edmacs-sidebar--buffer frame))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (hash-table-p edmacs-sidebar-buffers--row-overlays)
          (maphash (lambda (b ov)
                     (when (overlay-buffer ov)
                       (edmacs-sidebar-buffers--decorate-overlay ov b frame)))
                   edmacs-sidebar-buffers--row-overlays))))))

(defun edmacs-sidebar-buffers--on-window-change (frame)
  "Registered on the two high-frequency window hooks (AC4): cheap,
immediate, marker-only -- never a full rebuild."
  (edmacs-sidebar-buffers--refresh-markers frame))

(add-hook 'window-selection-change-functions #'edmacs-sidebar-buffers--on-window-change)
(add-hook 'window-buffer-change-functions #'edmacs-sidebar-buffers--on-window-change)

;; ============================================================================
;; Rendering
;; ============================================================================
;; FRAME is threaded through as a dynamic special (not a lexical
;; argument) across the render/insert helpers below -- every call in one
;; worktree's render happens for exactly one FRAME, so a `let'-bound
;; dynamic variable is simpler than a FRAME parameter on every function.

(defvar edmacs-sidebar-buffers--render-frame nil
  "The frame currently being rendered -- dynamically bound around one
worktree's `--render-tree'/`--render-flat' call.")

(defun edmacs-sidebar-buffers--insert-file-row (buf label depth)
  "Insert one `edmacs-sidebar-buffers-file' section for BUF, heading LABEL
indented two spaces per DEPTH."
  (let ((beg (point)))
    (magit-insert-section (edmacs-sidebar-buffers-file buf)
      (magit-insert-heading (concat (make-string (* 2 depth) ?\s) label)))
    (edmacs-sidebar-buffers--register-row buf beg (1- (point)) edmacs-sidebar-buffers--render-frame)))

(defun edmacs-sidebar-buffers--insert-special-row (buf depth)
  "Insert one `edmacs-sidebar-buffers-special' section for BUF, dimmed,
indented two spaces per DEPTH."
  (let ((beg (point)))
    (magit-insert-section (edmacs-sidebar-buffers-special buf)
      (magit-insert-heading (propertize (concat (make-string (* 2 depth) ?\s) (buffer-name buf))
                                         'face 'edmacs-sidebar-buffers-special-face)))
    (edmacs-sidebar-buffers--register-row buf beg (1- (point)) edmacs-sidebar-buffers--render-frame)))

(defun edmacs-sidebar-buffers--apply-initial-fold (section)
  "Fold SECTION now if its `hidden' slot says to.
`magit-insert-section's own HIDE argument only sets that slot (whether
from the fresh argument, or inherited from a same-identity predecessor
on a redraw -- see its docstring); nothing then actually collapses the
body unless the insertion also uses `magit-insert-section-body's
deferred-washer mechanism, which the rest of this codebase does not.
So every foldable section this module inserts calls this once, right
after its body is fully (eagerly) inserted, to apply whatever the slot
ended up holding."
  (when (oref section hidden)
    (magit-section-hide section)))

(defun edmacs-sidebar-buffers--render-node (node prefix depth prev-names)
  "Render NODE (a tree node from `--flatten-chains') at DEPTH (1-based).
PREFIX is the full joined path of NODE's parent, or nil at the top
level -- used to build a dir node's own full path, its section VALUE,
so manual fold state survives redraw by matching predecessor (see
`magit-insert-section's own docstring), and to hide dir bodies beyond a
RENDERED depth of two, counted after chain-flattening. The heading text
itself shows only NODE's own (possibly chain-merged) segment, not the
full path -- the full path would otherwise repeat at every nested
level."
  (pcase (plist-get node :kind)
    ('file (edmacs-sidebar-buffers--insert-file-row (plist-get node :buf) (plist-get node :name) depth))
    ('dir
     (let* ((full (if prefix (concat prefix "/" (plist-get node :seg)) (plist-get node :seg)))
            (hide (> depth 2))
            (sec (magit-insert-section (edmacs-sidebar-buffers-dir full hide)
                   (magit-insert-heading (concat (make-string (* 2 (1- depth)) ?\s) (plist-get node :seg) "/"))
                   (dolist (child (edmacs-sidebar-buffers--sort-siblings (plist-get node :children) prev-names))
                     (edmacs-sidebar-buffers--render-node child full (1+ depth) prev-names)))))
       (edmacs-sidebar-buffers--apply-initial-fold sec)))))

(defun edmacs-sidebar-buffers--render-tree (bufs root prev-names)
  "Render BUFS as a directory tree, special buffers trailing, dimmed."
  (let* ((file-bufs (seq-filter #'edmacs-sidebar-buffers--file-like-p bufs))
         (special-bufs (seq-remove #'edmacs-sidebar-buffers--file-like-p bufs))
         (tree (edmacs-sidebar-buffers--flatten-chains
                (edmacs-sidebar-buffers--build-tree file-bufs root))))
    (dolist (node (edmacs-sidebar-buffers--sort-siblings tree prev-names))
      (edmacs-sidebar-buffers--render-node node nil 1 prev-names))
    (dolist (buf (edmacs-sidebar-buffers--sort-bufs special-bufs prev-names))
      (edmacs-sidebar-buffers--insert-special-row buf 1))))

(defun edmacs-sidebar-buffers--render-flat (bufs prev-names)
  "Render BUFS as one row per buffer, in exactly PREV-NAMES order.
No directory grouping and no special-trailing split -- flat mode's
whole point is a literal `window-prev-buffers'-order stack (Steps item
6), so file-like and special buffers interleave in pure recency order."
  (dolist (buf (edmacs-sidebar-buffers--sort-bufs bufs prev-names))
    (if (edmacs-sidebar-buffers--file-like-p buf)
        (edmacs-sidebar-buffers--insert-file-row buf (buffer-name buf) 1)
      (edmacs-sidebar-buffers--insert-special-row buf 1))))

(defun edmacs-sidebar-buffers--on-worktree-section (root has-tab &optional frame tab-number)
  "Append ROOT's `buffers' child section, for its open tab TAB-NUMBER.
Registered on `edmacs-sidebar-worktree-section-functions'. A no-op when
HAS-TAB is nil -- a tab-less worktree has no `bufferlo' buffer list at
all. `bufferlo-buffer-list' takes a 0-based TABNUM while this feature's
own TAB-NUMBER convention (like every other consumer of this hook) is
1-based, so the single `(1- tab-number)' boundary lives here."
  (when has-tab
    (edmacs-sidebar-buffers--ensure-cleared-this-pass)
    (let* ((tab (edmacs-frames--tab-for-root root frame))
           (bufs (bufferlo-buffer-list frame (1- tab-number)))
           (prev-names (edmacs-sidebar-buffers--main-window-prev-names frame tab))
           (is-current (and tab (eq (car tab) 'current-tab)))
           (flat (frame-parameter frame 'edmacs-sidebar-buffers-flat)))
      (let ((root-sec
             (magit-insert-section sec (edmacs-sidebar-buffers-root (cons root tab-number) (not is-current))
               ;; Headless (no `magit-insert-heading' call): stamp `content'
               ;; ourselves so `--apply-initial-fold' below has a body span
               ;; to hide -- otherwise `content' stays nil (`magit-section's
               ;; own :initform) and folding this section would no-op.
               (oset sec content (point-marker))
               (let ((edmacs-sidebar-buffers--render-frame frame))
                 (if flat
                     (edmacs-sidebar-buffers--render-flat bufs prev-names)
                   (edmacs-sidebar-buffers--render-tree bufs root prev-names))))))
        (edmacs-sidebar-buffers--apply-initial-fold root-sec)))))

(add-hook 'edmacs-sidebar-worktree-section-functions #'edmacs-sidebar-buffers--on-worktree-section)

;; ============================================================================
;; RET -- show the buffer in the tab's main window
;; ============================================================================

(defun edmacs-sidebar-buffers--enclosing-root (section)
  "Return the `edmacs-sidebar-buffers-root' section enclosing SECTION."
  (while (and section (not (eq (oref section type) 'edmacs-sidebar-buffers-root)))
    (setq section (oref section parent)))
  section)

(defun edmacs-sidebar-buffers--row-buffer-at-point ()
  "Return the buffer at point's row, or nil when point is on neither a
`edmacs-sidebar-buffers-file' nor `-special' section."
  (let ((section (magit-current-section)))
    (and section
         (memq (oref section type) '(edmacs-sidebar-buffers-file edmacs-sidebar-buffers-special))
         (slot-boundp section 'value)
         (oref section value))))

(defun edmacs-sidebar-buffers--select-tab-if-needed (root tab-number)
  "Select ROOT's tab (TAB-NUMBER) on the selected frame, unless it is
already the current one. The sidebar's own frame is always the selected
frame here: a row is only ever visited with point already inside its
frame's sidebar window."
  (let ((tab (edmacs-frames--tab-for-root root)))
    (unless (and tab (eq (car tab) 'current-tab))
      (tab-bar-select-tab tab-number))))

;;;###autoload
(defun edmacs-sidebar-buffers-visit ()
  "Show the buffer at point in its tab's main window, switching tab first
if the row belongs to a different (background) tab."
  (interactive)
  (when-let* ((buf (edmacs-sidebar-buffers--row-buffer-at-point))
              (root-section (edmacs-sidebar-buffers--enclosing-root (magit-current-section)))
              (root-tab (and (slot-boundp root-section 'value) (oref root-section value))))
    (edmacs-sidebar-buffers--select-tab-if-needed (car root-tab) (cdr root-tab))
    (when (buffer-live-p buf)
      (edmacs-window-pop-buffer-to-main buf))))

;; ============================================================================
;; d -- kill the buffer at point
;; ============================================================================

;;;###autoload
(defun edmacs-sidebar-buffers-kill ()
  "Kill the buffer at point (`kill-buffer' prompts on a modified one) and
force an immediate redraw of the frame's sidebar."
  (interactive)
  (when-let* ((buf (edmacs-sidebar-buffers--row-buffer-at-point)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (edmacs-sidebar--redraw (selected-frame))))

;; ============================================================================
;; [ / ] -- previous-buffer / next-buffer on the tab's main window
;; ============================================================================

(defun edmacs-sidebar-buffers--enclosing-root-tab ()
  "Return the `(ROOT . TAB-NUMBER)' of the buffers-root enclosing point,
or nil when point is not inside any buffers subsection."
  (let ((root-section (edmacs-sidebar-buffers--enclosing-root (magit-current-section))))
    (and root-section (slot-boundp root-section 'value) (oref root-section value))))

(defun edmacs-sidebar-buffers--step (forward-p)
  "Drive TAB-NUMBER's main window through `next-buffer'/`previous-buffer'
\(FORWARD-P non-nil for `next-buffer') -- the exact commands `SPC b n'/
`SPC b p' already run -- then redraw and follow point to the resulting
buffer's row."
  (let ((root-tab (edmacs-sidebar-buffers--enclosing-root-tab)))
    (if (null root-tab)
        (message "edmacs-sidebar-buffers: point is not in a buffers subsection")
      (edmacs-sidebar-buffers--select-tab-if-needed (car root-tab) (cdr root-tab))
      (let ((main (edmacs-main-window)))
        (when main
          (with-selected-window main
            (funcall (if forward-p #'next-buffer #'previous-buffer)))
          (edmacs-sidebar--redraw (selected-frame))
          (let ((buf (window-buffer main)))
            (edmacs-sidebar--goto-identity (cons 'buffer (buffer-name buf)))))))))

;;;###autoload
(defun edmacs-sidebar-buffers-next ()
  "`next-buffer' on this row's tab's main window; point follows."
  (interactive)
  (edmacs-sidebar-buffers--step t))

;;;###autoload
(defun edmacs-sidebar-buffers-prev ()
  "`previous-buffer' on this row's tab's main window; point follows."
  (interactive)
  (edmacs-sidebar-buffers--step nil))

;; ============================================================================
;; s -- toggle directory tree vs. flat window-prev-buffers stack
;; ============================================================================
;; Frame-local (Steps item 6), unlike sidebar-agents.el's own
;; `edmacs-sidebar-agents-toggle-all', which is deliberately global.

;;;###autoload
(defun edmacs-sidebar-buffers-toggle-flat ()
  "Toggle the selected frame's sidebar between the directory tree and a
flat `window-prev-buffers'-ordered stack."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-parameter frame 'edmacs-sidebar-buffers-flat
                          (not (frame-parameter frame 'edmacs-sidebar-buffers-flat)))
    (edmacs-sidebar--redraw frame)))

;; ============================================================================
;; Redraw triggers: debounced full rebuild, immediate marker refresh
;; ============================================================================
;; Markers (visible/selected/modified) already have their own immediate,
;; cheap path above (`--on-window-change'); this section is the OTHER
;; half -- a full section-tree rebuild, needed whenever the buffer LIST
;; itself might have changed (a buffer opened, killed, or renamed).

(defvar edmacs-sidebar-buffers-debounce-seconds 0.2
  "Seconds `buffer-list-update-hook' firings coalesce into one redraw.
A plain `defvar', not `defcustom', so a test can shrink it -- mirrors
`edmacs-sidebar-agents-coalesce-seconds's own convention.")

(defvar edmacs-sidebar-buffers--redraw-timer nil
  "The pending debounced full-redraw timer, or nil.
Canceled and rescheduled on every new hook firing -- a rolling
debounce, never a stack of timers -- so a burst of buffer-list changes
produces exactly one redraw once the burst settles.")

(defun edmacs-sidebar-buffers--redraw-all ()
  "Redraw every live frame's sidebar buffer.
The hook driving this (`buffer-list-update-hook') carries no frame, so
every frame is redrawn even when only one frame's tab actually changed
-- a deliberate simplicity-over-precision choice; AC4 only constrains
tab-switch latency and keystroke-quiet debounce, not this hook's total
redraw count."
  (dolist (frame (frame-list))
    (when (frame-live-p frame)
      (edmacs-sidebar--redraw frame))))

(defun edmacs-sidebar-buffers--debounced-redraw ()
  (setq edmacs-sidebar-buffers--redraw-timer nil)
  (edmacs-sidebar-buffers--redraw-all))

(defun edmacs-sidebar-buffers--schedule-redraw ()
  "Registered on `buffer-list-update-hook': cancel-and-reschedule a
single rolling timer rather than redrawing on every firing -- so
opening 30 files in a burst produces exactly one redraw, and ordinary
typing (which does not touch this hook at all unless it creates or
kills a buffer) never triggers one."
  (when (timerp edmacs-sidebar-buffers--redraw-timer)
    (cancel-timer edmacs-sidebar-buffers--redraw-timer))
  (setq edmacs-sidebar-buffers--redraw-timer
        (run-at-time edmacs-sidebar-buffers-debounce-seconds nil
                      #'edmacs-sidebar-buffers--debounced-redraw)))

(add-hook 'buffer-list-update-hook #'edmacs-sidebar-buffers--schedule-redraw)

(provide 'sidebar-buffers)
;;; sidebar-buffers.el ends here
