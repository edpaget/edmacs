;;; ui-test.el --- Tests for ui.el -*- lexical-binding: t -*-

;;; Commentary:
;; ui.el's window-management code (the windmove `:around' advice, the main
;; window, the stack, and rotate.el's parameter-preserving advice) moved to
;; modules/windows.el in edmacs-window-management/phase-1-layout-model,
;; along with the three tests that covered it (now
;; edmacs-windows-test-windmove-* in modules/windows-test.el).
;;
;; What this file covers is the "Modeline content" section: the buffer-name
;; filtering and the diagnostics segment. Those are pure string functions,
;; so they run under plain `-Q --batch' with no display, no theme and no
;; nano-modeline. The `edmacs-modeline-*-mode' line CONSTRUCTORS are not
;; unit-tested -- they are nano's own lists with elements swapped, and
;; asserting the list back would test the literal, not behavior; what
;; matters about them is which element functions they name, which is
;; checked here directly.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/ui.el -l modules/ui-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Loading ui.el under `-Q' prints benign "Unrecognized keyword: :straight"
;; and "Cannot load" notices from its `use-package' forms; the definitions
;; under test are all at top level, ahead of and outside those forms.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; See modules/sessions-test.el for the same guard and why: `cl-letf' on a C
;; primitive builds a native trampoline, and that compile fails under `-Q'
;; when `user-emacs-directory''s eln-cache is not writable.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

;; ============================================================================
;; edmacs-modeline-filter-name
;; ============================================================================

(ert-deftest edmacs-ui-test-filter-name-strips-the-claude-term-wrapper ()
  "The case this section was written for: a pane's modeline should carry
the session label, not the `*claude-term:...*' plumbing around it."
  (should (equal (edmacs-modeline-filter-name "*claude-term:edmacs*") "edmacs"))
  (should (equal (edmacs-modeline-filter-name "*claude-term:edmacs:review*")
                 "edmacs:review")))

(ert-deftest edmacs-ui-test-filter-name-leaves-unmatched-names-alone ()
  (dolist (name '("init.el" "*scratch*" "*Messages*" "windows.el"))
    (should (equal (edmacs-modeline-filter-name name) name))))

(ert-deftest edmacs-ui-test-filter-name-handles-the-other-stock-filters ()
  (should (equal (edmacs-modeline-filter-name "*magit-diff: edmacs*") "edmacs diff"))
  (should (equal (edmacs-modeline-filter-name "*magit-log: edmacs*") "edmacs log"))
  (should (equal (edmacs-modeline-filter-name "*helpful variable: foo*") "foo"))
  (should (equal (edmacs-modeline-filter-name "*helpful function: bar*") "bar"))
  (should (equal (edmacs-modeline-filter-name "*cider-repl edmacs*") "edmacs repl")))

(ert-deftest edmacs-ui-test-filter-name-uses-the-first-matching-filter ()
  "Documented precedence: first match wins, not last, not longest."
  (let ((edmacs-modeline-name-filters '(("\\`a\\(.*\\)\\'" . "first-\\1")
                                        ("\\`a\\(.*\\)\\'" . "second-\\1"))))
    (should (equal (edmacs-modeline-filter-name "abc") "first-bc"))))

(ert-deftest edmacs-ui-test-filter-name-never-returns-empty ()
  "A filter that would erase the name is ignored: a nameless modeline is
worse than a noisy one."
  (let ((edmacs-modeline-name-filters '(("\\`.*\\'" . ""))))
    (should (equal (edmacs-modeline-filter-name "*claude-term:edmacs*")
                   "*claude-term:edmacs*"))))

(ert-deftest edmacs-ui-test-filter-name-falls-through-to-a-later-filter ()
  "An erasing filter is skipped rather than ending the search, so a
following filter still gets its chance."
  (let ((edmacs-modeline-name-filters '(("\\`a\\(.*\\)\\'" . "")
                                        ("\\`abc\\'" . "kept"))))
    (should (equal (edmacs-modeline-filter-name "abc") "kept"))))

(ert-deftest edmacs-ui-test-filter-name-does-not-rename-the-buffer ()
  "Only the displayed string changes -- `claude-term--parse-buffer-name'
and every other consumer still sees the real name."
  (let ((buf (generate-new-buffer "*claude-term:edmacs:probe*")))
    (unwind-protect
        (with-current-buffer buf
          (should (equal (edmacs-modeline-filter-name (buffer-name))
                         "edmacs:probe"))
          (should (equal (buffer-name) "*claude-term:edmacs:probe*")))
      (kill-buffer buf))))

;; ============================================================================
;; edmacs-modeline-buffer-name
;; ============================================================================

(ert-deftest edmacs-ui-test-buffer-name-passes-the-filtered-name-through ()
  (let (passed)
    (cl-letf (((symbol-function 'nano-modeline-buffer-name)
               (lambda (&optional name) (setq passed name) name)))
      (with-temp-buffer
        (rename-buffer "*claude-term:edmacs:review*" t)
        (edmacs-modeline-buffer-name)
        (should (equal passed "edmacs:review"))))))

(ert-deftest edmacs-ui-test-buffer-name-keeps-the-narrowing-suffix ()
  "Passing `nano-modeline-buffer-name' an explicit NAME takes away its own
narrowed-buffer branch, so the suffix has to be re-applied here."
  (let (passed)
    (cl-letf (((symbol-function 'nano-modeline-buffer-name)
               (lambda (&optional name) (setq passed name) name)))
      (with-temp-buffer
        (rename-buffer "*claude-term:edmacs*" t)
        (insert "one\ntwo\nthree\n")
        (narrow-to-region (point-min) (+ (point-min) 3))
        (edmacs-modeline-buffer-name)
        (should (equal passed "edmacs [narrow]"))))))

;; ============================================================================
;; edmacs-modeline-diagnostics
;; ============================================================================
;; The segment reads flycheck directly rather than through
;; `global-mode-string', which nano-modeline's single `:eval' form never
;; consults -- the reason `lsp-modeline-diagnostics-enable' rendered nothing.

;; Declared WITH values, unlike ui.el's own bare `(defvar flycheck-mode)':
;; a valueless `defvar' marks a symbol special only inside the file that
;; carries it, so a plain `let' here would create a LEXICAL binding that
;; `edmacs-modeline-diagnostics' -- reading the dynamic value -- never sees,
;; and every count assertion below would silently test the flycheck-is-off
;; path instead. Real flycheck is not loadable under `-Q', so nothing else
;; owns these names here.
(defvar flycheck-mode nil)
(defvar flycheck-current-errors nil)

(defmacro edmacs-ui-test--with-flycheck (counts &rest body)
  "Run BODY with flycheck on and `flycheck-count-errors' returning COUNTS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'flycheck-count-errors) (lambda (&rest _) ,counts)))
     (let ((flycheck-mode t)
           (flycheck-current-errors nil))
       ,@body)))

(ert-deftest edmacs-ui-test-diagnostics-is-silent-when-flycheck-is-off ()
  "Silent, not zero: the segment must cost no width in the common case."
  (let ((flycheck-mode nil))
    (should (equal (edmacs-modeline-diagnostics) ""))))

(ert-deftest edmacs-ui-test-diagnostics-is-silent-when-clean ()
  (edmacs-ui-test--with-flycheck nil
    (should (equal (edmacs-modeline-diagnostics) ""))))

(ert-deftest edmacs-ui-test-diagnostics-is-silent-at-zero-counts ()
  "An explicit zero count reads as clean, not as `E0'."
  (edmacs-ui-test--with-flycheck '((error . 0) (warning . 0))
    (should (equal (edmacs-modeline-diagnostics) ""))))

(ert-deftest edmacs-ui-test-diagnostics-shows-errors-only ()
  (edmacs-ui-test--with-flycheck '((error . 3))
    (should (equal (substring-no-properties (edmacs-modeline-diagnostics)) "E3 "))))

(ert-deftest edmacs-ui-test-diagnostics-shows-warnings-only ()
  (edmacs-ui-test--with-flycheck '((warning . 2))
    (should (equal (substring-no-properties (edmacs-modeline-diagnostics)) "W2 "))))

(ert-deftest edmacs-ui-test-diagnostics-shows-both-errors-first ()
  (edmacs-ui-test--with-flycheck '((warning . 2) (error . 3))
    (should (equal (substring-no-properties (edmacs-modeline-diagnostics)) "E3 W2 "))))

(ert-deftest edmacs-ui-test-diagnostics-ignores-other-levels ()
  "`info'-level results are noise in a modeline; only errors and warnings
earn the width."
  (edmacs-ui-test--with-flycheck '((info . 9))
    (should (equal (edmacs-modeline-diagnostics) ""))))

(ert-deftest edmacs-ui-test-diagnostics-carries-severity-faces ()
  "The counts are distinguishable by color, not only by letter -- the
stock `error'/`warning' faces, which every theme defines."
  (edmacs-ui-test--with-flycheck '((error . 1) (warning . 1))
    (let ((s (edmacs-modeline-diagnostics)))
      (should (eq (get-text-property (string-search "E" s) 'face s) 'error))
      (should (eq (get-text-property (string-search "W" s) 'face s) 'warning)))))

(ert-deftest edmacs-ui-test-diagnostics-honors-the-format-variables ()
  (let ((edmacs-modeline-diagnostics-format "%d err")
        (edmacs-modeline-diagnostics-warning-format "%d warn"))
    (edmacs-ui-test--with-flycheck '((error . 1) (warning . 2))
      (should (equal (substring-no-properties (edmacs-modeline-diagnostics))
                     "1 err 2 warn ")))))

;; nano-modeline is not loadable under `-Q', and a bare `defvar' in ui.el
;; marks the symbol special only within ui.el itself -- so without this the
;; `let' below would bind lexically and the dynamic read inside
;; `edmacs--apply-modeline-inactive-status-faces' would see it as void.
(defvar nano-modeline-faces)

(ert-deftest edmacs-ui-test-inactive-status-faces-mirror-the-active-ones ()
  "Each `status-*-active' entry gains an `-inactive' twin, so an unfocused
window keeps its buffer-status badge instead of rendering two raised
padding spaces against no background."
  (let ((nano-modeline-faces '((status-RW-active . (nano-modeline-status))
                               (status-RO-active . (nano-modeline-status))
                               (status-**-active . (nano-modeline-status)))))
    (edmacs--apply-modeline-inactive-status-faces)
    (dolist (key '(status-RW-inactive status-RO-inactive status-**-inactive))
      (should (assq key nano-modeline-faces)))
    (should (memq 'nano-modeline-status
                  (cdr (assq 'status-RW-inactive nano-modeline-faces))))))

(ert-deftest edmacs-ui-test-inactive-status-faces-never-clobber-a-user-entry ()
  "An existing `-inactive' entry is left alone -- the function only fills a
gap, so a user customisation of `nano-modeline-faces' survives, and
re-running it never stacks a duplicate."
  (let ((nano-modeline-faces '((status-RW-inactive . (my-own-face)))))
    (edmacs--apply-modeline-inactive-status-faces)
    (edmacs--apply-modeline-inactive-status-faces)
    (should (equal '(my-own-face)
                    (cdr (assq 'status-RW-inactive nano-modeline-faces))))
    (should (= 1 (cl-count 'status-RW-inactive nano-modeline-faces :key #'car)))))

(ert-deftest edmacs-ui-test-modeline-contrast-draws-no-line ()
  "No mode-line face draws a visible edge. solarized boxes the mode line in
the OLD background colour and adds an overline/underline; once the
background moved, each of those became a stray line across the bar. The
box is kept (it supplies the bar's height) but recoloured to the
background, and the overline/underline are dropped."
  ;; nano-modeline is not loadable under `-Q', so its faces may not exist.
  (dolist (f '(nano-modeline-active nano-modeline-inactive))
    (unless (facep f) (make-face f)))
  (cl-letf (((symbol-function 'frame-parameter) (lambda (&rest _) 'dark)))
    (edmacs--apply-modeline-contrast))
  (dolist (face '(nano-modeline-active nano-modeline-inactive
                  mode-line mode-line-inactive))
    (should-not (face-attribute face :overline nil t))
    (should-not (face-attribute face :underline nil t))
    ;; A box whose colour equals the background cannot draw a visible line.
    (should (equal (face-attribute face :background nil t)
                    (plist-get (face-attribute face :box nil t) :color)))))

;; ============================================================================
;; edmacs-modeline--mode-icon / edmacs-modeline-buffer-status
;; ============================================================================
;; `nano-modeline-buffer-status' is not loadable under `-Q' (a straight
;; package), so every test here stubs it via `cl-letf' and asserts on the
;; STATUS string it was called with -- the same indirection
;; sidebar-agents-test.el uses for nerd-icons and its own line
;; constructors.

(ert-deftest edmacs-ui-test-buffer-status-uses-mode-icon-when-available ()
  "When `nerd-icons' is (simulated) present, its glyph is passed through as
STATUS instead of being left to `nano-modeline-buffer-status's default."
  (with-temp-buffer
    (let ((captured 'unset)
          (edmacs-modeline-force-text-glyphs nil))
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'nerd-icons-icon-for-mode)
                 (lambda (_mode) "MODE-ICON"))
                ((symbol-function 'nano-modeline-buffer-status)
                 (lambda (&optional status) (setq captured status))))
        (edmacs-modeline-buffer-status))
      (should (equal captured "MODE-ICON")))))

(ert-deftest edmacs-ui-test-buffer-status-marks-modified-without-losing-readonly-precedence ()
  "The modified marker is appended only when the buffer is writable and
modified; a read-only+modified buffer keeps the plain icon, matching
`nano-modeline-buffer-status's own read-only-wins precedence."
  (with-temp-buffer
    (let ((captured 'unset))
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'nerd-icons-icon-for-mode) (lambda (_mode) "ICON"))
                ((symbol-function 'nano-modeline-buffer-status)
                 (lambda (&optional status) (setq captured status))))
        (setq buffer-read-only nil)
        (set-buffer-modified-p t)
        (edmacs-modeline-buffer-status)
        (should (equal captured (concat "ICON" edmacs-modeline-modified-marker)))
        (setq captured 'unset)
        (setq buffer-read-only t)
        (set-buffer-modified-p t)
        (edmacs-modeline-buffer-status)
        (should (equal captured "ICON"))))))

(ert-deftest edmacs-ui-test-mode-icon-falls-back-to-nil-when-forced-off-or-absent ()
  "Every path that yields no icon leaves `nano-modeline-buffer-status's
STATUS argument nil, i.e. its own default RO/**/RW text renders
unchanged."
  (with-temp-buffer
    ;; (a) forced off, nerd-icons simulated present.
    (let ((captured 'unset)
          (edmacs-modeline-force-text-glyphs t))
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'nerd-icons-icon-for-mode) (lambda (_mode) "ICON"))
                ((symbol-function 'nano-modeline-buffer-status)
                 (lambda (&optional status) (setq captured status))))
        (should-not (edmacs-modeline--mode-icon))
        (edmacs-modeline-buffer-status)
        (should-not captured)))
    ;; (b) nerd-icons not loaded.
    (let ((captured 'unset)
          (edmacs-modeline-force-text-glyphs nil))
      (cl-letf (((symbol-function 'featurep) (lambda (_f) nil))
                ((symbol-function 'nano-modeline-buffer-status)
                 (lambda (&optional status) (setq captured status))))
        (should-not (edmacs-modeline--mode-icon))
        (edmacs-modeline-buffer-status)
        (should-not captured)))
    ;; (c) the lookup itself signals.
    (let ((captured 'unset)
          (edmacs-modeline-force-text-glyphs nil))
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'nerd-icons-icon-for-mode)
                 (lambda (_mode) (error "boom")))
                ((symbol-function 'nano-modeline-buffer-status)
                 (lambda (&optional status) (setq captured status))))
        (should-not (edmacs-modeline--mode-icon))
        (edmacs-modeline-buffer-status)
        (should-not captured)))))

;; ============================================================================
;; edmacs-modeline--agent-icon / edmacs-modeline-ghostel-status
;; ============================================================================

(ert-deftest edmacs-ui-test-ghostel-status-distinguishes-claude-term-from-plain-terminal ()
  "A claude-term pane gets a distinct robot glyph (or its \"AI\" text
fallback); a plain ghostel/vterm terminal keeps the unchanged \">_\"."
  ;; A claude-term buffer with nerd-icons available: the robot glyph wins.
  (with-temp-buffer
    (setq-local claude-term-mode t)
    (let ((captured 'unset)
          (edmacs-modeline-force-text-glyphs nil))
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'nerd-icons)))
                ((symbol-function 'nerd-icons-mdicon) (lambda (_name) "ROBOT-ICON"))
                ((symbol-function 'edmacs-modeline-fixed-status)
                 (lambda (status &optional _face) (setq captured status))))
        (edmacs-modeline-ghostel-status))
      (should (equal captured "ROBOT-ICON"))))
  ;; A plain terminal buffer, `claude-term-mode' never bound: unchanged ">_".
  (with-temp-buffer
    (let ((captured 'unset))
      (cl-letf (((symbol-function 'edmacs-modeline-fixed-status)
                 (lambda (status &optional _face) (setq captured status))))
        (edmacs-modeline-ghostel-status))
      (should (equal captured ">_"))))
  ;; A claude-term buffer with no nerd-icons: the "AI" text fallback.
  (with-temp-buffer
    (setq-local claude-term-mode t)
    (let ((captured 'unset)
          (edmacs-modeline-force-text-glyphs nil))
      (cl-letf (((symbol-function 'featurep) (lambda (_f) nil))
                ((symbol-function 'edmacs-modeline-fixed-status)
                 (lambda (status &optional _face) (setq captured status))))
        (edmacs-modeline-ghostel-status))
      (should (equal captured "AI")))))

;;; ui-test.el ends here
