;;; claude-repl-buffer.el --- Response buffer UI for Claude Code -*- lexical-binding: t -*-

;;; Commentary:
;; Provides a beautiful, structured response buffer for Claude Code interactions.
;; Features markdown rendering, syntax highlighting, and organized sections.

;;; Code:

(require 'markdown-mode)
(require 'claude-repl-process)
(require 'claude-repl-tool-output)

;; ============================================================================
;; Forward Declarations
;; ============================================================================

;; Suppress byte-compiler warnings for buffer-local variables defined later
(defvar claude-repl-buffer-interactions)

;; Forward declare functions from other modules
(declare-function claude-repl-ask "claude-repl-core")
(declare-function claude-repl-interrupt-and-ask "claude-repl-core")

;; ============================================================================
;; Custom Faces
;; ============================================================================

(defgroup claude-repl-buffer nil
  "Response buffer for Claude Code."
  :group 'tools)

;; ============================================================================
;; Faces - Enhanced with light/dark mode support
;; ============================================================================

;; Header faces
(defface claude-repl-prompt-header
  '((t :inherit font-lock-keyword-face
       :weight bold
       :height 1.1
       :underline t))
  "Face for prompt section headers (## Prompt)."
  :group 'claude-repl-buffer)

(defface claude-repl-response-header
  '((t :inherit font-lock-type-face
       :weight bold
       :height 1.1
       :underline t))
  "Face for response section headers (## Response)."
  :group 'claude-repl-buffer)

;; Content faces
(defface claude-repl-user-prompt
  '((((background light)) :inherit default :slant italic :foreground "#5c6370")
    (((background dark)) :inherit default :slant italic :foreground "#abb2bf"))
  "Face for user prompt text."
  :group 'claude-repl-buffer)

(defface claude-repl-assistant-text
  '((t :inherit default))
  "Face for Claude's response text (inherits theme default)."
  :group 'claude-repl-buffer)

;; Tool use faces
(defface claude-repl-tool-header
  '((t :inherit warning :weight bold))
  "Face for tool use headers (e.g., [Tool: Read])."
  :group 'claude-repl-buffer)

(defface claude-repl-tool-name
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for tool names in tool headers."
  :group 'claude-repl-buffer)

(defface claude-repl-tool-section
  '((((background light)) :background "#f5f5f5" :extend t)
    (((background dark)) :background "#282c34" :extend t))
  "Face for entire tool use sections (background)."
  :group 'claude-repl-buffer)

;; Status and metadata faces
(defface claude-repl-timestamp
  '((t :inherit font-lock-comment-face :slant italic :height 0.9))
  "Face for timestamps."
  :group 'claude-repl-buffer)

(defface claude-repl-metadata-label
  '((t :inherit font-lock-constant-face :weight bold :height 0.85))
  "Face for metadata labels (Tokens:, Duration:)."
  :group 'claude-repl-buffer)

(defface claude-repl-metadata-value
  '((t :inherit font-lock-number-face :height 0.85))
  "Face for metadata values."
  :group 'claude-repl-buffer)

;; Separator faces
(defface claude-repl-separator
  '((((background light)) :foreground "#d0d0d0")
    (((background dark)) :foreground "#3e4451"))
  "Face for section separators (horizontal rules)."
  :group 'claude-repl-buffer)

(defface claude-repl-separator-heavy
  '((((background light)) :foreground "#a0a0a0" :weight bold)
    (((background dark)) :foreground "#5c6370" :weight bold))
  "Face for heavy separators between interactions."
  :group 'claude-repl-buffer)

;; Input area faces
(defface claude-repl-input-prompt
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for the input prompt indicator."
  :group 'claude-repl-buffer)

(defface claude-repl-input-area
  '((((background light)) :background "#fafafa" :extend t)
    (((background dark)) :background "#2c313a" :extend t))
  "Face for the input area background."
  :group 'claude-repl-buffer)

;; Status indicator faces
(defface claude-repl-status-streaming
  '((t :inherit success :weight bold))
  "Face for streaming status indicator."
  :group 'claude-repl-buffer)

(defface claude-repl-status-complete
  '((t :inherit font-lock-comment-face))
  "Face for complete status indicator."
  :group 'claude-repl-buffer)

(defface claude-repl-status-error
  '((t :inherit error :weight bold))
  "Face for error status indicator."
  :group 'claude-repl-buffer)

;; Session info faces
(defface claude-repl-session-id
  '((t :inherit font-lock-string-face :height 0.8 :family "monospace"))
  "Face for session IDs."
  :group 'claude-repl-buffer)

(defface claude-repl-model-name
  '((t :inherit font-lock-keyword-face :height 0.85 :weight bold))
  "Face for model name display."
  :group 'claude-repl-buffer)

;; Legacy face aliases for backward compatibility
(defface claude-repl-prompt-face
  '((t :inherit claude-repl-prompt-header))
  "Deprecated: Use `claude-repl-prompt-header' instead."
  :group 'claude-repl-buffer)

(defface claude-repl-response-face
  '((t :inherit claude-repl-assistant-text))
  "Deprecated: Use `claude-repl-assistant-text' instead."
  :group 'claude-repl-buffer)

(defface claude-repl-tool-face
  '((t :inherit claude-repl-tool-header))
  "Deprecated: Use `claude-repl-tool-header' instead."
  :group 'claude-repl-buffer)

(defface claude-repl-timestamp-face
  '((t :inherit claude-repl-timestamp))
  "Deprecated: Use `claude-repl-timestamp' instead."
  :group 'claude-repl-buffer)

(defface claude-repl-separator-face
  '((t :inherit claude-repl-separator))
  "Deprecated: Use `claude-repl-separator' instead."
  :group 'claude-repl-buffer)

(defface claude-repl-metadata-face
  '((t :inherit claude-repl-metadata-label))
  "Deprecated: Use `claude-repl-metadata-label' instead."
  :group 'claude-repl-buffer)

;; ============================================================================
;; Customization Variables
;; ============================================================================

(defcustom claude-repl-buffer-header-style 'simple
  "Style for section headers in Claude Code buffers.
Choices:
  - \\='simple: Plain markdown headers (##)
  - \\='box: ASCII box drawing
  - \\='unicode-box: Unicode box drawing characters
  - \\='unicode-fancy: Unicode with icons (requires nerd-fonts)"
  :type '(choice (const :tag "Simple markdown" simple)
                 (const :tag "ASCII box" box)
                 (const :tag "Unicode box" unicode-box)
                 (const :tag "Unicode + icons" unicode-fancy))
  :group 'claude-repl-buffer)

(defcustom claude-repl-separator-style 'line
  "Style for separators between interactions.
Choices:
  - \\='line: Simple line (──────)
  - \\='double: Double line (══════)
  - \\='labeled: Line with interaction number
  - \\='minimal: Short line
  - \\='none: No separator"
  :type '(choice (const :tag "Single line" line)
                 (const :tag "Double line" double)
                 (const :tag "Labeled with number" labeled)
                 (const :tag "Minimal" minimal)
                 (const :tag "None" none))
  :group 'claude-repl-buffer)

(defcustom claude-repl-use-icons t
  "Whether to use icons in the buffer.
When enabled, uses nerd-icons if available, falls back to Unicode emoji.
When disabled, uses plain text markers."
  :type 'boolean
  :group 'claude-repl-buffer)

(defcustom claude-repl-input-prompt-string "claude> "
  "String to display as the input prompt.
This appears at the start of the input area where you type your message."
  :type 'string
  :group 'claude-repl-buffer)

(defcustom claude-repl-section-spacing 1
  "Number of blank lines between sections.
Controls vertical spacing between prompt/response/tool sections."
  :type 'integer
  :group 'claude-repl-buffer)

(defcustom claude-repl-buffer-max-width nil
  "Maximum width for content in characters.
If nil, use full window width.  If set, content will be limited to this width."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max width"))
  :group 'claude-repl-buffer)

(defcustom claude-repl-separator-character ?─
  "Character to use for simple line separators.
Default is the Unicode box-drawing light horizontal character (U+2500)."
  :type 'character
  :group 'claude-repl-buffer)

(defcustom claude-repl-separator-length 80
  "Default length for separators in characters."
  :type 'integer
  :group 'claude-repl-buffer)

(defcustom claude-repl-auto-scroll t
  "Whether to automatically scroll to bottom during streaming output.
When enabled, the buffer will automatically scroll to show new output as it
arrives.  Auto-scrolling is disabled if the user moves the cursor away from
the end of the buffer."
  :type 'boolean
  :group 'claude-repl-buffer)

;; ============================================================================
;; Styling Helper Functions
;; ============================================================================

(defun claude-repl-buffer--icon (name &optional fallback)
  "Get icon NAME using nerd-icons, or use FALLBACK.
If `claude-repl-use-icons' is nil, returns empty string.
If nerd-icons is not available, returns FALLBACK (or empty string)."
  (cond
   ((not claude-repl-use-icons) "")
   ((and (fboundp 'nerd-icons-mdicon)
         (require 'nerd-icons nil t))
    (nerd-icons-mdicon name :face 'nerd-icons-blue))
   (t (or fallback ""))))

(defun claude-repl-buffer--get-adaptive-width ()
  "Get the adaptive width for content based on settings and window size."
  (if claude-repl-buffer-max-width
      (min claude-repl-buffer-max-width (window-body-width))
    (or claude-repl-separator-length 80)))

(defun claude-repl-buffer--format-status-indicator (status)
  "Format a status indicator for STATUS.
STATUS should be \\='streaming, \\='complete, or \\='error.
Returns a propertized string with icon and text."
  (let* ((face (pcase status
                 ('streaming 'claude-repl-status-streaming)
                 ('complete 'claude-repl-status-complete)
                 ('error 'claude-repl-status-error)
                 (_ 'claude-repl-status-streaming)))
         (icon (pcase status
                 ('streaming (claude-repl-buffer--icon "nf-md-loading" "⟳"))
                 ('complete (claude-repl-buffer--icon "nf-md-check" "✓"))
                 ('error (claude-repl-buffer--icon "nf-md-close" "✗"))
                 (_ "")))
         (text (pcase status
                 ('streaming "Streaming...")
                 ('complete "Complete")
                 ('error "Error")
                 (_ ""))))
    (if (not (string-empty-p text))
        (propertize (format " %s %s" icon text)
                    'face face
                    'claude-repl-status t
                    'claude-repl-status-value status)
      ;; Return empty string for unknown status
      "")))

(defun claude-repl-buffer--format-header (type text &optional timestamp status)
  "Format a header of TYPE with TEXT, optional TIMESTAMP and STATUS.
TYPE should be \\='prompt or \\='response.
STATUS should be \\='streaming, \\='complete, or \\='error.
Returns a propertized string ready for insertion."
  (let* ((width (claude-repl-buffer--get-adaptive-width))
         (face (if (eq type 'prompt)
                   'claude-repl-prompt-header
                 'claude-repl-response-header))
         (icon (if (eq type 'prompt)
                   (claude-repl-buffer--icon "nf-md-account" "📝")
                 (claude-repl-buffer--icon "nf-md-robot" "🤖")))
         (timestamp-str (if timestamp
                            (format "[%s]" timestamp)
                          ""))
         (icon-prefix (if (string-empty-p icon) "" (concat icon " ")))
         (status-str (if status
                         (claude-repl-buffer--format-status-indicator status)
                       "")))

    (pcase claude-repl-buffer-header-style
      ('simple
       ;; Simple markdown style: ## Prompt [timestamp] ✓ Complete
       (concat (propertize (format "## %s" text) 'face face)
               (when timestamp
                 (concat " "
                         (propertize timestamp-str
                                     'face 'claude-repl-timestamp)))
               status-str))

      ('box
       ;; ASCII box style: +--- Prompt ---[timestamp] Status ---+
       (let* ((status-text (if status (format " %s " (pcase status
                                                       ('streaming "Streaming...")
                                                       ('complete "Complete")
                                                       ('error "Error")
                                                       (_ ""))) ""))
              (header-text (concat text " " timestamp-str status-text))
              (dashes (max 3 (- width (length header-text) 6))))
         (concat (propertize "+--- " 'face face)
                 (propertize text 'face face)
                 " "
                 (when timestamp
                   (concat (propertize timestamp-str 'face 'claude-repl-timestamp)
                           " "))
                 status-str
                 " "
                 (propertize (make-string (max 0 (- dashes (length status-text) (if timestamp (length timestamp-str) 0) 1)) ?-)
                             'face face)
                 (propertize "+" 'face face))))

      ('unicode-box
       ;; Unicode box style: ╭─ Prompt ─[timestamp] Status ─╮
       (let* ((status-text (if status (format " %s " (pcase status
                                                       ('streaming "Streaming...")
                                                       ('complete "Complete")
                                                       ('error "Error")
                                                       (_ ""))) ""))
              (header-text (concat text " " timestamp-str status-text))
              (dashes (max 2 (- width (length header-text) 6))))
         (concat (propertize "╭─ " 'face face)
                 (propertize text 'face face)
                 " "
                 (when timestamp
                   (concat (propertize timestamp-str 'face 'claude-repl-timestamp)
                           " "))
                 status-str
                 " "
                 (propertize (make-string (max 0 (- dashes (length status-text) (if timestamp (length timestamp-str) 0) 1)) ?─)
                             'face face)
                 (propertize "╮" 'face face))))

      ('unicode-fancy
       ;; Unicode with icons: ┏━ 📝 Prompt ━[timestamp] Status ━┓
       (let* ((status-text (if status (format " %s " (pcase status
                                                       ('streaming "Streaming...")
                                                       ('complete "Complete")
                                                       ('error "Error")
                                                       (_ ""))) ""))
              (header-text (concat icon-prefix text " " timestamp-str status-text))
              (dashes (max 2 (- width (length header-text) 6))))
         (concat (propertize "┏━ " 'face face)
                 icon-prefix
                 (propertize text 'face face)
                 " "
                 (when timestamp
                   (concat (propertize timestamp-str 'face 'claude-repl-timestamp)
                           " "))
                 status-str
                 " "
                 (propertize (make-string (max 0 (- dashes (length icon-prefix) (length status-text) (if timestamp (length timestamp-str) 0) 1)) ?━)
                             'face face)
                 (propertize "┓" 'face face))))

      (_ ;; Default to simple
       (concat (propertize (format "## %s" text) 'face face)
               (when timestamp
                 (concat " "
                         (propertize timestamp-str
                                     'face 'claude-repl-timestamp)))
               status-str)))))

(defun claude-repl-buffer--make-separator (&optional label)
  "Create a separator line with optional LABEL.
Respects `claude-repl-separator-style' customization."
  (let ((width (claude-repl-buffer--get-adaptive-width)))
    (pcase claude-repl-separator-style
      ('line
       ;; Simple line: ────────────────
       (propertize (make-string width claude-repl-separator-character)
                   'face 'claude-repl-separator))

      ('double
       ;; Double line: ════════════════
       (propertize (make-string width ?═)
                   'face 'claude-repl-separator-heavy))

      ('labeled
       ;; Labeled: ─── Label ───
       (let* ((text (or label (format "Interaction #%d"
                                      (1+ (length claude-repl-buffer-interactions)))))
              (text-len (length text))
              (total-dashes (- width text-len 2)) ; 2 for spaces around text
              (left-dashes (/ total-dashes 2))
              (right-dashes (- total-dashes left-dashes)))
         (concat (propertize (make-string left-dashes claude-repl-separator-character)
                             'face 'claude-repl-separator)
                 " "
                 (propertize text 'face 'claude-repl-separator)
                 " "
                 (propertize (make-string right-dashes claude-repl-separator-character)
                             'face 'claude-repl-separator))))

      ('minimal
       ;; Minimal: just three dashes
       (propertize "───" 'face 'claude-repl-separator))

      ('none
       ;; No separator
       "")

      (_ ;; Default to line
       (propertize (make-string width claude-repl-separator-character)
                   'face 'claude-repl-separator)))))

(defun claude-repl-buffer--insert-spacing (n)
  "Insert N blank lines for section spacing."
  (dotimes (_ (or n claude-repl-section-spacing))
    (insert "\n")))

(defun claude-repl-buffer--update-status (interaction new-status)
  "Update the status indicator for INTERACTION to NEW-STATUS.
NEW-STATUS should be \\='streaming, \\='complete, or \\='error.
This dynamically updates the response header in the buffer."
  (when (and interaction
             (markerp (claude-repl-interaction-start-marker interaction))
             (marker-position (claude-repl-interaction-start-marker interaction)))
    (let ((inhibit-read-only t))
      (save-excursion
        ;; Find the response header line
        (goto-char (claude-repl-interaction-start-marker interaction))
        (when (re-search-forward "^\\(##\\|[+╭┏]\\).*Response"
                                 (claude-repl-interaction-end-marker interaction) t)
          (let ((line-start (line-beginning-position)))
            ;; Remove old status indicator if present
            (goto-char line-start)
            (let ((search-end (line-end-position)))
              (while (and (< (point) search-end)
                          (not (get-text-property (point) 'claude-repl-status)))
                (goto-char (next-single-property-change (point) 'claude-repl-status nil search-end)))
              (when (< (point) search-end)
                ;; Found old status, delete it
                (let ((status-start (point))
                      (status-end (next-single-property-change (point) 'claude-repl-status nil search-end)))
                  (delete-region status-start status-end))))

            ;; Insert new status at end of line (recalculate position after deletion)
            (goto-char (line-end-position))
            (let ((new-status-str (claude-repl-buffer--format-status-indicator new-status)))
              (when new-status-str
                (insert new-status-str)))))))))

;; ============================================================================
;; Buffer Management
;; ============================================================================

(defun claude-repl-buffer--get-buffer-name (project-root)
  "Get the response buffer name for PROJECT-ROOT."
  (format "*claude-repl: %s*"
          (file-name-nondirectory (directory-file-name project-root))))

(defun claude-repl-buffer-get-or-create (project-root)
  "Get or create the response buffer for PROJECT-ROOT."
  (let ((buffer-name (claude-repl-buffer--get-buffer-name project-root)))
    (or (get-buffer buffer-name)
        (with-current-buffer (generate-new-buffer buffer-name)
          (claude-repl-buffer-mode)
          (setq-local claude-repl-buffer-project-root project-root)
          ;; Set up initial input area
          (claude-repl-buffer-setup-input-area)
          (current-buffer)))))

(defvar-local claude-repl-buffer-project-root nil
  "Project root for this Claude Code buffer.")

(defvar-local claude-repl-buffer-current-interaction nil
  "Marker for the current interaction being built.")

(defvar-local claude-repl-buffer-interactions nil
  "List of all completed interactions in this buffer.")

(defvar-local claude-repl-buffer-prompt-start-marker nil
  "Marker for the start of the current prompt.")

(defvar-local claude-repl-buffer-input-start-marker nil
  "Marker for the start of the input area.")

(defvar-local claude-repl-buffer-input-ring nil
  "Ring of previous inputs for history navigation.")

(defvar-local claude-repl-buffer-input-ring-index nil
  "Current position in the input ring.")

(defvar-local claude-repl-buffer-input-history-pattern nil
  "Pattern for searching history.  Set by history search commands.")

(defconst claude-repl-buffer-input-ring-size 100
  "Maximum number of inputs to store in history.")

(defvar-local claude-repl-buffer-session-id nil
  "Session ID for conversation continuity across process restarts.")

;; ============================================================================
;; Buffer Mode
;; ============================================================================

;; ============================================================================
;; Fontification Helper
;; ============================================================================

(defun claude-repl-buffer--fontify-history-only (start end)
  "Fontify region from START to END, but only in the history area.
This prevents markdown formatting from being applied to the editable input area."
  (when claude-repl-buffer-input-start-marker
    (let ((input-start (marker-position claude-repl-buffer-input-start-marker)))
      (when input-start
        (cond
         ;; Region is entirely before input area - fontify it
         ((< end input-start)
          (font-lock-fontify-region start end))
         ;; Region starts before input area - fontify only the history part
         ((< start input-start)
          (font-lock-fontify-region start input-start))
         ;; Region is entirely in input area - don't fontify at all
         (t
          nil))))))

(defun claude-repl-buffer--clean-input-area ()
  "Remove any text properties that might interfere with editing in the input area.
This is called on `post-command-hook' to ensure the input area stays editable."
  (when claude-repl-buffer-input-start-marker
    (let ((input-start (marker-position claude-repl-buffer-input-start-marker))
          (inhibit-read-only t))
      (when (and input-start (<= input-start (point-max)))
        ;; Remove problematic properties from input area
        ;; This includes markdown formatting properties that make text invisible
        (remove-text-properties input-start (point-max)
                               '(read-only nil
                                 keymap nil
                                 syntax-table nil
                                 face nil
                                 font-lock-face nil
                                 fontified nil
                                 font-lock-multiline nil
                                 invisible nil
                                 display nil
                                 composition nil
                                 rear-nonsticky nil
                                 front-sticky nil))))))

;; Keymap for read-only history area with single-letter commands
(defvar claude-repl-buffer-readonly-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'claude-repl-buffer-refresh)
    (define-key map (kbd "c") #'claude-repl-buffer-copy-last-response)
    map)
  "Keymap for read-only (history) area in Claude Code buffers.")

(defvar claude-repl-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Always-available commands
    (define-key map (kbd "C-c C-c") #'claude-repl-buffer-copy-code-block-at-point)
    (define-key map (kbd "C-c C-k") #'claude-repl-buffer-interrupt-and-ask)
    (define-key map (kbd "C-c q") #'quit-window)
    (define-key map (kbd "C-c g") #'claude-repl-buffer-refresh)

    ;; Phase 3: Navigation commands
    (define-key map (kbd "C-c C-n") #'claude-repl-buffer-next-interaction)
    (define-key map (kbd "C-c C-p") #'claude-repl-buffer-previous-interaction)
    (define-key map (kbd "C-M-n") #'claude-repl-buffer-next-code-block)
    (define-key map (kbd "C-M-p") #'claude-repl-buffer-previous-code-block)

    ;; Phase 3: Action commands
    (define-key map (kbd "C-c C-r") #'claude-repl-buffer-resend-prompt)
    (define-key map (kbd "C-c C-s") #'claude-repl-buffer-search-interactions)

    ;; Phase 4: Interactive REPL input
    (define-key map (kbd "RET") #'claude-repl-buffer-return)
    (define-key map (kbd "C-c RET") #'claude-repl-buffer-send-input)
    (define-key map (kbd "C-j") #'electric-newline-and-maybe-indent)
    (define-key map (kbd "M-p") #'claude-repl-buffer-previous-input)
    (define-key map (kbd "M-n") #'claude-repl-buffer-next-input)
    (define-key map (kbd "M-r") #'claude-repl-buffer-history-search-backward)
    (define-key map (kbd "M-s") #'claude-repl-buffer-history-search-forward)

    map)
  "Keymap for Claude Code buffer mode.")

(define-derived-mode claude-repl-buffer-mode text-mode "Claude-Code"
  "Major mode for Claude Code response buffers.
Displays structured conversations with Claude including prompts,
responses, tool usage, and metadata.

\\{claude-repl-buffer-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (visual-line-mode 1)
  ;; Ensure buffer is not globally read-only
  (setq buffer-read-only nil)
  ;; Initialize input history ring
  (setq-local claude-repl-buffer-input-ring (make-ring claude-repl-buffer-input-ring-size))
  (setq-local claude-repl-buffer-input-ring-index nil)
  ;; Initialize auto-scroll tracking
  (setq-local claude-repl-buffer-auto-scroll-enabled claude-repl-auto-scroll)
  ;; Enable markdown syntax highlighting if available
  ;; Only apply to history area, not input area
  (when (fboundp 'markdown-mode)
    (font-lock-mode -1)
    (setq-local font-lock-defaults
                (list markdown-mode-font-lock-keywords t))
    ;; Add fontification function that respects input area boundaries
    (add-hook 'jit-lock-functions
              #'claude-repl-buffer--fontify-history-only
              nil t)
    (font-lock-mode 1))
  ;; Add post-command hook to keep input area clean
  (add-hook 'post-command-hook
            #'claude-repl-buffer--clean-input-area
            nil t)
  ;; Add post-command hook to detect user cursor movement
  (add-hook 'post-command-hook
            #'claude-repl-buffer--handle-cursor-movement
            nil t)
  ;; Ensure mode-line is updated
  (force-mode-line-update))

(add-hook 'claude-repl-buffer-mode-hook
          (lambda ()
            (setq mode-name "Claude-Code")
            (force-mode-line-update)))

;; ============================================================================
;; Interaction Structure
;; ============================================================================

(cl-defstruct claude-repl-interaction
  "Structure representing a single prompt/response interaction."
  prompt              ; User's prompt text
  prompt-time         ; Timestamp when prompt was sent
  response-text       ; Accumulated response text
  tool-uses           ; List of tools used
  tool-outputs        ; List of tool outputs (tool-name tool-input tool-output)
  start-marker        ; Buffer position where interaction starts
  end-marker          ; Buffer position where interaction ends
  status              ; Status: streaming, complete, error
  metadata)           ; Additional metadata (tokens, duration, etc.)

;; ============================================================================
;; Buffer Manipulation Utilities
;; ============================================================================

(defmacro claude-repl-buffer-save-markers (&rest body)
  "Execute BODY while preserving buffer markers.
Saves and restores positions of input-start and prompt-start markers."
  (declare (indent 0) (debug t))
  `(let ((input-pos (when (and claude-repl-buffer-input-start-marker
                               (marker-position claude-repl-buffer-input-start-marker))
                      (marker-position claude-repl-buffer-input-start-marker)))
         (prompt-pos (when (and claude-repl-buffer-prompt-start-marker
                                (marker-position claude-repl-buffer-prompt-start-marker))
                       (marker-position claude-repl-buffer-prompt-start-marker))))
     (prog1 (progn ,@body)
       (when (and claude-repl-buffer-input-start-marker input-pos)
         (set-marker claude-repl-buffer-input-start-marker input-pos))
       (when (and claude-repl-buffer-prompt-start-marker prompt-pos)
         (set-marker claude-repl-buffer-prompt-start-marker prompt-pos)))))

(defun claude-repl-buffer-propertize-region (start end properties)
  "Apply PROPERTIES (a plist) to region from START to END.
More convenient than multiple `put-text-property' calls.

Example:
  (claude-repl-buffer-propertize-region
    start end
    \\='(face claude-repl-prompt-face
      read-only t
      field claude-repl-prompt))"
  (let ((props properties))
    (while props
      (let ((prop (car props))
            (value (cadr props)))
        (put-text-property start end prop value)
        (setq props (cddr props))))))

(defun claude-repl-buffer-get-field-bounds (&optional pos)
  "Get the bounds of the field at POS (default: point).
Returns (start . end) or nil if not in a field."
  (let ((pos (or pos (point))))
    (let ((field (get-text-property pos 'field)))
      (when field
        (let ((start pos)
              (end pos))
          ;; Find field start
          (while (and (> start (point-min))
                      (eq (get-text-property (1- start) 'field) field))
            (setq start (1- start)))
          ;; Find field end
          (while (and (< end (point-max))
                      (eq (get-text-property end 'field) field))
            (setq end (1+ end)))
          (cons start end))))))

;; ============================================================================
;; Buffer Insertion Functions
;; ============================================================================

(defun claude-repl-buffer--insert-with-face (text face)
  "Insert TEXT with FACE at point."
  (let ((start (point)))
    (insert text)
    (put-text-property start (point) 'face face)))

(defun claude-repl-buffer--insert-separator (&optional label)
  "Insert a visual separator with optional LABEL.
Uses the configured separator style from `claude-repl-separator-style'."
  (let ((sep (claude-repl-buffer--make-separator label)))
    (unless (string-empty-p sep)
      (insert sep)
      (insert "\n"))))

(defun claude-repl-buffer--insert-timestamp (time)
  "Insert formatted timestamp for TIME."
  (claude-repl-buffer--insert-with-face
   (format-time-string "[%Y-%m-%d %H:%M:%S]" time)
   'claude-repl-timestamp-face))

(defun claude-repl-buffer--format-todo-list (todos)
  "Format TODOS list for TodoWrite tool in a readable way.
TODOS should be a list or vector of todo items with content, status,
and activeForm."
  (let ((lines nil)
        (status-icons '((completed . "✓")
                       (in_progress . "▶")
                       (pending . "○"))))
    ;; Convert vector to list if needed
    (let ((todos-list (if (vectorp todos) (append todos nil) todos)))
      (dolist (todo todos-list)
        (let* ((content (alist-get 'content todo))
               (status-raw (alist-get 'status todo))
               ;; Normalize status: convert string to symbol if needed
               (status (if (stringp status-raw) (intern status-raw) status-raw))
               (icon (alist-get status status-icons "•"))
               (face (pcase status
                       ('completed 'success)
                       ('in_progress 'warning)
                       ('pending 'shadow)
                       (_ 'default))))
          (push (propertize (format "%s %s" icon content)
                           'face face)
                lines)))
      (string-join (nreverse lines) "\n"))))

(defun claude-repl-buffer--format-tool-params (tool-input)
  "Format TOOL-INPUT parameters in a readable markdown list.
Returns a formatted string with markdown-style bullet points."
  (if (null tool-input)
      "_No parameters_"
    (let ((params nil))
      (dolist (pair tool-input)
        (let* ((key (symbol-name (car pair)))
               (value (cdr pair))
               (value-str (cond
                          ((stringp value)
                           ;; For strings, use code formatting
                           (format "`%s`" (if (> (length value) 80)
                                             (concat (substring value 0 80) "...")
                                           value)))
                          ((null value) "`nil`")
                          ((numberp value) (format "`%d`" value))
                          (t (format "`%S`" value)))))
          (push (format "- **%s**: %s" key value-str) params)))
      (string-join (nreverse params) "\n"))))

(defun claude-repl-buffer-start-interaction (buffer prompt)
  "Start a new interaction in BUFFER with PROMPT."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (start-pos (point-max))
          (timestamp (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))
      (goto-char (point-max))

      ;; Add separator if not first interaction
      (unless (= (point-min) (point-max))
        (claude-repl-buffer--insert-spacing 1)
        (claude-repl-buffer--insert-separator)
        (claude-repl-buffer--insert-spacing 1))

      ;; Insert prompt header with new formatting
      (insert (claude-repl-buffer--format-header 'prompt "Prompt" timestamp))
      (insert "\n")
      (claude-repl-buffer--insert-spacing claude-repl-section-spacing)

      ;; Insert prompt text with user-prompt face
      (let ((prompt-start (point)))
        (insert prompt)
        (put-text-property prompt-start (point) 'face 'claude-repl-user-prompt))
      (insert "\n")
      (claude-repl-buffer--insert-spacing claude-repl-section-spacing)

      ;; Insert response header with streaming status
      (insert (claude-repl-buffer--format-header 'response "Response" nil 'streaming))
      (insert "\n")
      (claude-repl-buffer--insert-spacing claude-repl-section-spacing)

      ;; Create interaction object
      (let ((interaction (make-claude-repl-interaction
                          :prompt prompt
                          :prompt-time (current-time)
                          :response-text ""
                          :tool-uses nil
                          :tool-outputs nil
                          :start-marker (copy-marker start-pos)
                          :end-marker (point-marker)
                          :status 'streaming
                          :metadata nil)))
        (setq-local claude-repl-buffer-current-interaction interaction)

        ;; Re-enable auto-scroll when starting a new interaction
        (setq-local claude-repl-buffer-auto-scroll-enabled claude-repl-auto-scroll)

        ;; Auto-scroll to show the new interaction
        (claude-repl-buffer--maybe-auto-scroll)

        interaction))))

(defun claude-repl-buffer-append-text (buffer text)
  "Append TEXT to the current interaction in BUFFER."
  (with-current-buffer buffer
    (when claude-repl-buffer-current-interaction
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (claude-repl-interaction-end-marker
                      claude-repl-buffer-current-interaction))
          ;; Use insert-before-markers to automatically preserve marker positions
          (insert-before-markers text))

        ;; Update interaction
        (setf (claude-repl-interaction-response-text
               claude-repl-buffer-current-interaction)
              (concat (claude-repl-interaction-response-text
                       claude-repl-buffer-current-interaction)
                      text))

        ;; Auto-scroll if enabled
        (claude-repl-buffer--maybe-auto-scroll)))))

(defun claude-repl-buffer-add-tool-use (buffer tool-id tool-name tool-input)
  "Add a tool use entry to BUFFER for TOOL-NAME with TOOL-INPUT.
TOOL-ID is the unique identifier for this tool use."
  (with-current-buffer buffer
    (when claude-repl-buffer-current-interaction
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (claude-repl-interaction-end-marker
                      claude-repl-buffer-current-interaction))
          (insert "\n")
          (claude-repl-buffer--insert-spacing claude-repl-section-spacing)

          ;; Special handling for TodoWrite tool
          (if (string= tool-name "TodoWrite")
              (let* ((todos (alist-get 'todos tool-input))
                     (header-start (point)))
                ;; Use a checklist icon for TodoWrite
                (insert (claude-repl-buffer--icon "nf-md-format_list_checks" "☑") " ")
                (insert "**Task Progress**")
                (put-text-property header-start (point) 'face 'claude-repl-tool-header)
                (insert "\n\n")
                ;; Format todos with special formatting
                (let ((section-start (point)))
                  (insert (claude-repl-buffer--format-todo-list todos))
                  (insert "\n")
                  ;; Apply tool section background
                  (put-text-property section-start (point) 'face 'claude-repl-tool-section)))

            ;; Standard tool formatting for other tools
            (let ((tool-icon (claude-repl-buffer--icon "nf-md-tools" "🔧"))
                  (header-start (point)))
              (insert (if (string-empty-p tool-icon) "" (concat tool-icon " ")))
              (insert (format "**Tool: %s**" tool-name))
              (put-text-property header-start (point) 'face 'claude-repl-tool-header)
              (insert "\n\n")
              ;; Format tool parameters as markdown list
              (let ((section-start (point)))
                (insert (claude-repl-buffer--format-tool-params tool-input))
                (insert "\n")
                ;; Apply tool section background
                (put-text-property section-start (point) 'face 'claude-repl-tool-section))))

          (set-marker (claude-repl-interaction-end-marker
                       claude-repl-buffer-current-interaction)
                      (point)))

        ;; Update interaction - store tool-id for matching with results
        (push (list :id tool-id :tool tool-name :input tool-input)
              (claude-repl-interaction-tool-uses
               claude-repl-buffer-current-interaction))))))

(defun claude-repl-buffer-add-tool-output (buffer tool-use-id tool-name tool-input tool-output)
  "Add tool output to BUFFER for tool with TOOL-USE-ID.
TOOL-NAME and TOOL-INPUT may be provided directly, or looked up via TOOL-USE-ID.
TOOL-OUTPUT is the tool result (string or alist).
Formats and displays the output using `claude-repl-tool-output-format'."
  (with-current-buffer buffer
    (when claude-repl-buffer-current-interaction
      ;; Look up the matching tool use by ID to get accurate name/input
      (let* ((tool-uses (claude-repl-interaction-tool-uses
                         claude-repl-buffer-current-interaction))
             (matching-use (cl-find-if (lambda (use)
                                         (equal (plist-get use :id) tool-use-id))
                                       tool-uses))
             ;; Use matched tool data if available, otherwise fall back to provided values
             (actual-name (if matching-use
                              (plist-get matching-use :tool)
                            tool-name))
             (actual-input (if matching-use
                               (plist-get matching-use :input)
                             tool-input))
             (inhibit-read-only t))
        (save-excursion
          (goto-char (claude-repl-interaction-end-marker
                      claude-repl-buffer-current-interaction))
          (insert "\n")
          (claude-repl-buffer--insert-spacing claude-repl-section-spacing)

          ;; Format and insert tool output using matched data
          (let ((formatted-output (claude-repl-tool-output-format
                                   actual-name actual-input tool-output)))
            (insert formatted-output)
            (insert "\n"))

          (set-marker (claude-repl-interaction-end-marker
                       claude-repl-buffer-current-interaction)
                      (point)))

        ;; Update interaction with tool output
        (push (list :tool-use-id tool-use-id
                    :tool actual-name
                    :input actual-input
                    :output tool-output)
              (claude-repl-interaction-tool-outputs
               claude-repl-buffer-current-interaction))

        ;; Auto-scroll if enabled
        (claude-repl-buffer--maybe-auto-scroll)))))

(defun claude-repl-buffer-complete-interaction (buffer &optional metadata)
  "Mark the current interaction in BUFFER as complete with optional METADATA."
  (with-current-buffer buffer
    (when claude-repl-buffer-current-interaction
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (claude-repl-interaction-end-marker
                      claude-repl-buffer-current-interaction))
          (insert "\n\n")

          ;; Add metadata if provided
          (when metadata
            (let ((tokens-in (alist-get 'input_tokens metadata))
                  (tokens-out (alist-get 'output_tokens metadata))
                  (duration (alist-get 'duration_ms metadata)))
              (when (or tokens-in tokens-out duration)
                ;; Format with enhanced styling
                ;; Tokens label
                (insert (propertize "Tokens: " 'face 'claude-repl-metadata-label))
                ;; Tokens values
                (insert (propertize (format "%d" (or tokens-in 0))
                                    'face 'claude-repl-metadata-value))
                (insert " → ")
                (insert (propertize (format "%d" (or tokens-out 0))
                                    'face 'claude-repl-metadata-value))
                ;; Duration if available
                (when duration
                  (insert " • ")
                  (insert (propertize "Duration: " 'face 'claude-repl-metadata-label))
                  (insert (propertize (format "%.2fs" (/ duration 1000.0))
                                      'face 'claude-repl-metadata-value)))
                (insert "\n"))))

          ;; Update end marker to current position (after metadata)
          (set-marker (claude-repl-interaction-end-marker
                       claude-repl-buffer-current-interaction)
                      (point))))

      ;; Update interaction status
      (setf (claude-repl-interaction-status
             claude-repl-buffer-current-interaction) 'complete)
      (setf (claude-repl-interaction-metadata
             claude-repl-buffer-current-interaction) metadata)

      ;; Update status indicator in buffer to show complete
      (claude-repl-buffer--update-status claude-repl-buffer-current-interaction 'complete)

      ;; Mark entire interaction as a field for better navigation
      (let ((start (marker-position (claude-repl-interaction-start-marker
                                     claude-repl-buffer-current-interaction)))
            (end (marker-position (claude-repl-interaction-end-marker
                                   claude-repl-buffer-current-interaction))))
        (put-text-property start end 'field 'claude-repl-interaction))

      ;; Save to history
      (push claude-repl-buffer-current-interaction
            claude-repl-buffer-interactions)

      (setq-local claude-repl-buffer-current-interaction nil))

    ;; Set up input area for next interaction
    (claude-repl-buffer-setup-input-area)))

(defun claude-repl-buffer-error-interaction (buffer error-message)
  "Mark the current interaction in BUFFER as failed with ERROR-MESSAGE."
  (with-current-buffer buffer
    (when claude-repl-buffer-current-interaction
      (let ((inhibit-read-only t))
        ;; Update interaction status FIRST
        (setf (claude-repl-interaction-status
               claude-repl-buffer-current-interaction) 'error)

        ;; Update status indicator in buffer BEFORE inserting error message
        (claude-repl-buffer--update-status claude-repl-buffer-current-interaction 'error)

        ;;Now insert the error message
        (save-excursion
          (goto-char (claude-repl-interaction-end-marker
                      claude-repl-buffer-current-interaction))
          (insert "\n\n")

          ;; Display error message
          (let ((error-start (point)))
            (insert "❌ Error: ")
            (insert error-message)
            (insert "\n")
            (put-text-property error-start (point) 'face 'claude-repl-status-error))

          (set-marker (claude-repl-interaction-end-marker
                       claude-repl-buffer-current-interaction)
                      (point)))

        ;; Mark entire interaction as a field for better navigation
        (let ((start (marker-position (claude-repl-interaction-start-marker
                                       claude-repl-buffer-current-interaction)))
              (end (marker-position (claude-repl-interaction-end-marker
                                     claude-repl-buffer-current-interaction))))
          (put-text-property start end 'field 'claude-repl-interaction))

        ;; Save to history
        (push claude-repl-buffer-current-interaction
              claude-repl-buffer-interactions)

        (setq-local claude-repl-buffer-current-interaction nil))

      ;; Set up input area for next interaction
      (claude-repl-buffer-setup-input-area))))

;; ============================================================================
;; Buffer Commands
;; ============================================================================

(defun claude-repl-buffer-refresh ()
  "Refresh the current buffer (currently just a message)."
  (interactive)
  (message "Claude Code buffer refreshed"))

(defun claude-repl-buffer-interrupt-and-ask ()
  "Interrupt current Claude execution and ask a new question.
This is a convenience wrapper for `claude-repl-interrupt-and-ask' that
can be called from the buffer."
  (interactive)
  (require 'claude-repl-core)
  (call-interactively #'claude-repl-interrupt-and-ask))

(defun claude-repl-buffer-copy-last-response ()
  "Copy the last response to the kill ring."
  (interactive)
  (let ((interaction claude-repl-buffer-current-interaction))
    (when interaction
      (let ((text (claude-repl-interaction-response-text interaction)))
        (when text
          (kill-new text)
          (message "Copied response to kill ring"))))))

(defun claude-repl-buffer-copy-code-block-at-point ()
  "Copy the code block at point to the kill ring."
  (interactive)
  (save-excursion
    (let* ((start (re-search-backward "^```" nil t))
           (end (when start (forward-line 1)
                      (re-search-forward "^```" nil t))))
      (when (and start end)
        (let ((code (buffer-substring-no-properties
                     (save-excursion (goto-char start) (forward-line 1) (point))
                     (save-excursion (goto-char end) (forward-line -1) (point)))))
          (kill-new code)
          (message "Copied code block to kill ring"))))))

;; ============================================================================
;; Phase 3: Navigation Commands
;; ============================================================================

(defun claude-repl-buffer-next-interaction ()
  "Move point to the next interaction in the buffer."
  (interactive)
  (let ((start-pos (point)))
    (end-of-line)
    (if (re-search-forward "^## Prompt" nil t)
        (progn
          (beginning-of-line)
          (message "Moved to next interaction"))
      (goto-char start-pos)
      (message "No next interaction"))))

(defun claude-repl-buffer-previous-interaction ()
  "Move point to the previous interaction in the buffer."
  (interactive)
  (let ((start-pos (point)))
    (beginning-of-line)
    (if (re-search-backward "^## Prompt" nil t)
        (message "Moved to previous interaction")
      (goto-char start-pos)
      (message "No previous interaction"))))

(defun claude-repl-buffer-get-interaction-at-point ()
  "Get the interaction structure at point."
  (save-excursion
    ;; Find the start of the current interaction
    (unless (looking-at "^## Prompt")
      (re-search-backward "^## Prompt" nil t))
    (let ((prompt-start (point)))
      ;; Find all interactions
      (cl-block nil
        (dolist (interaction (reverse claude-repl-buffer-interactions))
          (when (and (markerp (claude-repl-interaction-start-marker interaction))
                     (marker-position (claude-repl-interaction-start-marker interaction))
                     (= (marker-position (claude-repl-interaction-start-marker interaction))
                        prompt-start))
            (cl-return interaction)))))))

(defun claude-repl-buffer-resend-prompt ()
  "Re-send the prompt from the interaction at point."
  (interactive)
  (let ((interaction (claude-repl-buffer-get-interaction-at-point)))
    (if interaction
        (let ((prompt (claude-repl-interaction-prompt interaction)))
          (if prompt
              (progn
                (require 'claude-repl-core)
                (claude-repl-ask prompt))
            (message "No prompt found in current interaction")))
      (message "Not in an interaction"))))

(defun claude-repl-buffer-next-code-block ()
  "Move point to the next code block in the buffer."
  (interactive)
  (let ((start-pos (point)))
    (end-of-line)
    (if (re-search-forward "^```" nil t)
        (progn
          (beginning-of-line)
          (message "Moved to next code block"))
      (goto-char start-pos)
      (message "No next code block"))))

(defun claude-repl-buffer-previous-code-block ()
  "Move point to the previous code block in the buffer."
  (interactive)
  (let ((start-pos (point)))
    (beginning-of-line)
    (if (re-search-backward "^```" nil t)
        (message "Moved to previous code block")
      (goto-char start-pos)
      (message "No previous code block"))))

(defun claude-repl-buffer-search-interactions (query)
  "Search for QUERY in all interactions and jump to the first match."
  (interactive "sSearch in interactions: ")
  (let ((start-pos (point))
        (case-fold-search t))
    (goto-char (point-min))
    (if (search-forward query nil t)
        (progn
          (beginning-of-line)
          (message "Found: %s" query))
      (goto-char start-pos)
      (message "Not found: %s" query))))

;; ============================================================================
;; Phase 4: Interactive REPL Input
;; ============================================================================

(defun claude-repl-buffer-setup-input-area ()
  "Set up the input area at the end of the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      ;; Add spacing before input prompt
      (unless (bobp)
        (insert "\n\n"))
      ;; Insert prompt with enhanced styling
      (let ((prompt-start (point)))
        ;; Mark the start of the prompt
        (setq-local claude-repl-buffer-prompt-start-marker (copy-marker prompt-start))
        (set-marker-insertion-type claude-repl-buffer-prompt-start-marker nil)

        ;; Insert customizable prompt string
        (insert (or claude-repl-input-prompt-string "> "))
        ;; Apply properties with specific rear-nonsticky list
        (put-text-property prompt-start (point) 'face 'claude-repl-input-prompt)
        (put-text-property prompt-start (point) 'read-only t)
        (put-text-property prompt-start (point) 'field 'claude-repl-prompt)
        (put-text-property prompt-start (point) 'intangible t)
        (put-text-property prompt-start (point) 'rear-nonsticky '(read-only face field intangible)))
      ;; Mark the start of the input area
      (setq-local claude-repl-buffer-input-start-marker (point-marker))
      (set-marker-insertion-type claude-repl-buffer-input-start-marker nil)
      ;; Note: We don't apply face property here to avoid any formatting interference
      ;; The input area should remain completely unformatted for easy editing
      ))
  ;; Set up text properties for read-only history area
  (when claude-repl-buffer-input-start-marker
    (let ((inhibit-read-only t)
          (marker-pos (marker-position claude-repl-buffer-input-start-marker)))
      (when (> marker-pos (point-min))
        ;; Calculate history-end to exclude the "> " prompt (2 chars before marker)
        (let ((history-end (max (point-min) (- marker-pos 2))))
          ;; Make history read-only and add keymap for single-letter commands
          (put-text-property (point-min) history-end 'read-only t)
          (put-text-property (point-min) history-end 'keymap claude-repl-buffer-readonly-map)))
      ;; Explicitly ensure the input area is NOT read-only and has NO special keymap
      ;; Also remove any markdown or font-lock properties that might interfere with editing
      ;; This is critical to prevent markdown formatting from making input text invisible
      (when (<= marker-pos (point-max))
        (remove-text-properties marker-pos (point-max)
                               '(read-only nil
                                 keymap nil
                                 syntax-table nil
                                 face nil
                                 font-lock-face nil
                                 fontified nil
                                 font-lock-multiline nil
                                 invisible nil
                                 display nil
                                 composition nil)))))
  ;; Move point to input area
  (goto-char (point-max))
  (message "Input area ready. Type your message and press C-c RET to send."))

(defun claude-repl-buffer-in-input-area-p ()
  "Return t if point is in the input area."
  (and claude-repl-buffer-input-start-marker
       (>= (point) claude-repl-buffer-input-start-marker)))

(defun claude-repl-buffer-input-complete-p ()
  "Check if the current input is complete and ready to send.
Returns t if input is non-empty after trimming."
  (when claude-repl-buffer-input-start-marker
    (let ((input (claude-repl-buffer-get-input)))
      (and input
           (not (string-empty-p (string-trim input)))))))

(defun claude-repl-buffer-get-input ()
  "Get the current input text."
  (when claude-repl-buffer-input-start-marker
    (buffer-substring-no-properties
     (marker-position claude-repl-buffer-input-start-marker)
     (point-max))))

(defun claude-repl-buffer-clear-input ()
  "Clear the input area."
  (when claude-repl-buffer-input-start-marker
    (let ((inhibit-read-only t))
      (delete-region claude-repl-buffer-input-start-marker (point-max)))))

(defun claude-repl-buffer-clear-input-area ()
  "Clear and properly deallocate the input area.
This function clears the input text and deallocates both the
prompt-start and input-start markers to prevent memory leaks."
  (when claude-repl-buffer-input-start-marker
    (let ((inhibit-read-only t))
      ;; Clear content from prompt-start (if available) or input-start to end
      (let ((clear-start (if (and claude-repl-buffer-prompt-start-marker
                                  (marker-position claude-repl-buffer-prompt-start-marker))
                             claude-repl-buffer-prompt-start-marker
                           claude-repl-buffer-input-start-marker)))
        (delete-region clear-start (point-max)))

      ;; Properly deallocate markers
      (when claude-repl-buffer-prompt-start-marker
        (set-marker claude-repl-buffer-prompt-start-marker nil))
      (set-marker claude-repl-buffer-input-start-marker nil)

      ;; Clear variables
      (setq-local claude-repl-buffer-prompt-start-marker nil)
      (setq-local claude-repl-buffer-input-start-marker nil))))

(defun claude-repl-buffer-send-input ()
  "Send the current input to Claude."
  (interactive)
  (cond
   ;; No input marker set up
   ((not claude-repl-buffer-input-start-marker)
    (user-error "No input area available.  Try reopening the buffer"))

   ;; Input not complete or empty
   ((not (claude-repl-buffer-input-complete-p))
    (user-error "Please enter some text before sending"))

   ;; Everything OK, send it
   (t
    (let ((input (string-trim (claude-repl-buffer-get-input))))
      ;; Add to history
      (ring-insert claude-repl-buffer-input-ring input)
      (setq-local claude-repl-buffer-input-ring-index nil)

      ;; Clear input area and properly deallocate markers
      (claude-repl-buffer-clear-input-area)

      ;; Send to Claude
      (require 'claude-repl-core)
      (claude-repl-ask input)

      (message "Sent: %s" (truncate-string-to-width input 50 nil nil "..."))))))

(defun claude-repl-buffer-return ()
  "Smart RET: send input if in input area, otherwise normal behavior."
  (interactive)
  (if (claude-repl-buffer-in-input-area-p)
      (claude-repl-buffer-send-input)
    ;; In read-only area, just move to next line or do nothing
    (when (not (eobp))
      (forward-line 1))))

(defun claude-repl-buffer-previous-input ()
  "Insert the previous input from history."
  (interactive)
  (when (and claude-repl-buffer-input-ring
             (not (ring-empty-p claude-repl-buffer-input-ring)))
    (let ((inhibit-read-only t))
      ;; Initialize index if needed
      (unless claude-repl-buffer-input-ring-index
        (setq-local claude-repl-buffer-input-ring-index 0))

      ;; Get previous input
      (let ((input (ring-ref claude-repl-buffer-input-ring
                             claude-repl-buffer-input-ring-index)))
        (claude-repl-buffer-clear-input)
        (insert input)

        ;; Move to next older input
        (when (< (1+ claude-repl-buffer-input-ring-index)
                 (ring-length claude-repl-buffer-input-ring))
          (setq-local claude-repl-buffer-input-ring-index
                      (1+ claude-repl-buffer-input-ring-index)))))))

(defun claude-repl-buffer-next-input ()
  "Insert the next input from history."
  (interactive)
  (when (and claude-repl-buffer-input-ring
             claude-repl-buffer-input-ring-index
             (> claude-repl-buffer-input-ring-index 0))
    (let ((inhibit-read-only t))
      ;; Move to next newer input
      (setq-local claude-repl-buffer-input-ring-index
                  (1- claude-repl-buffer-input-ring-index))

      ;; Get and insert input
      (let ((input (ring-ref claude-repl-buffer-input-ring
                             claude-repl-buffer-input-ring-index)))
        (claude-repl-buffer-clear-input)
        (insert input)))))

(defun claude-repl-buffer-history-search-backward (pattern)
  "Search backward in history for entries matching PATTERN (regex).
Updates input area with first match found.  When called repeatedly,
continues searching through older matches."
  (interactive "sSearch history backward (regex): ")
  (setq-local claude-repl-buffer-input-history-pattern pattern)
  (when (and claude-repl-buffer-input-ring
             (not (ring-empty-p claude-repl-buffer-input-ring)))
    (let ((inhibit-read-only t)
          (start-index (if (and claude-repl-buffer-input-ring-index
                                (eq last-command 'claude-repl-buffer-history-search-backward))
                           (1+ claude-repl-buffer-input-ring-index)
                         0))
          (found nil)
          (found-index nil))
      ;; Search through history from start-index
      (let ((index start-index))
        (while (and (< index (ring-length claude-repl-buffer-input-ring))
                    (not found))
          (let ((input (ring-ref claude-repl-buffer-input-ring index)))
            (when (string-match-p pattern input)
              (setq found input)
              (setq found-index index)))
          (setq index (1+ index))))

      ;; Insert if found
      (if found
          (progn
            (claude-repl-buffer-clear-input)
            (insert found)
            (setq-local claude-repl-buffer-input-ring-index found-index)
            (message "Found [%d/%d]: %s"
                     (1+ found-index)
                     (ring-length claude-repl-buffer-input-ring)
                     (truncate-string-to-width found 50 nil nil "...")))
        (message "No match for: %s" pattern)))))

(defun claude-repl-buffer-history-search-forward (pattern)
  "Search forward in history for entries matching PATTERN (regex).
Updates input area with first match found.  When called repeatedly,
continues searching through newer matches."
  (interactive "sSearch history forward (regex): ")
  (setq-local claude-repl-buffer-input-history-pattern pattern)
  (when (and claude-repl-buffer-input-ring
             claude-repl-buffer-input-ring-index
             (> claude-repl-buffer-input-ring-index 0))
    (let ((inhibit-read-only t)
          (start-index (if (eq last-command 'claude-repl-buffer-history-search-forward)
                           (1- claude-repl-buffer-input-ring-index)
                         (1- claude-repl-buffer-input-ring-index)))
          (found nil)
          (found-index nil))
      ;; Search backward through history
      (let ((index start-index))
        (while (and (>= index 0) (not found))
          (let ((input (ring-ref claude-repl-buffer-input-ring index)))
            (when (string-match-p pattern input)
              (setq found input)
              (setq found-index index)))
          (setq index (1- index))))

      ;; Insert if found
      (if found
          (progn
            (claude-repl-buffer-clear-input)
            (insert found)
            (setq-local claude-repl-buffer-input-ring-index found-index)
            (message "Found [%d/%d]: %s"
                     (1+ found-index)
                     (ring-length claude-repl-buffer-input-ring)
                     (truncate-string-to-width found 50 nil nil "...")))
        (message "No match for: %s" pattern)))))

;; ============================================================================
;; Auto-scroll Support
;; ============================================================================

(defun claude-repl-buffer--at-end-of-buffer-p ()
  "Return t if point is at or near the end of the buffer.
Considers point to be at the end if it's within the input area or at the
last position in the history area."
  (or
   ;; Point is in the input area
   (and claude-repl-buffer-input-start-marker
        (>= (point) claude-repl-buffer-input-start-marker))
   ;; Point is at the very end of buffer
   (>= (point) (point-max))
   ;; Point is at the end of the current interaction (if streaming)
   (and claude-repl-buffer-current-interaction
        (>= (point) (claude-repl-interaction-end-marker
                     claude-repl-buffer-current-interaction)))))

(defun claude-repl-buffer--handle-cursor-movement ()
  "Detect when user moves cursor and disable auto-scroll if not at end.
This is called via post-command-hook."
  (when (and (boundp 'claude-repl-buffer-auto-scroll-enabled)
             claude-repl-buffer-auto-scroll-enabled
             ;; Only check for commands that move the cursor
             (memq this-command '(previous-line next-line
                                 forward-char backward-char
                                 beginning-of-line end-of-line
                                 beginning-of-buffer end-of-buffer
                                 scroll-up-command scroll-down-command
                                 mouse-set-point
                                 evil-previous-line evil-next-line
                                 evil-forward-char evil-backward-char
                                 evil-beginning-of-line evil-end-of-line
                                 evil-goto-first-line evil-goto-line
                                 evil-scroll-up evil-scroll-down)))
    ;; If cursor moved away from the end, disable auto-scroll
    (unless (claude-repl-buffer--at-end-of-buffer-p)
      (setq-local claude-repl-buffer-auto-scroll-enabled nil))))

(defun claude-repl-buffer--maybe-auto-scroll ()
  "Scroll to bottom if auto-scroll is enabled and we're in a visible window."
  (when (and (boundp 'claude-repl-buffer-auto-scroll-enabled)
             claude-repl-buffer-auto-scroll-enabled)
    ;; For each window showing this buffer, scroll to the end
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (with-selected-window window
        (goto-char (point-max))
        ;; Recenter to show some context above the new text
        (recenter -1)))))

;; ============================================================================
;; Event Handlers (for use with process callbacks)
;; ============================================================================

(defun claude-repl-buffer-handle-user-event (buffer event)
  "Handle a \\='user\\=' event in BUFFER from EVENT.
User events may contain tool_result content blocks."
  (when-let* ((message (alist-get 'message event))
              (content (alist-get 'content message)))
    ;; Convert vector to list if needed
    (let ((content-list (if (vectorp content) (append content nil) content)))
      ;; Process each content block
      (dolist (block content-list)
        (let ((block-type (alist-get 'type block)))
          ;; Only handle tool_result blocks from user events
          (when (equal block-type "tool_result")
            (let* ((tool-use-id (alist-get 'tool_use_id block))
                   (raw-output (alist-get 'content block))
                   ;; Filter system reminders from tool output
                   (tool-output (if (stringp raw-output)
                                    (claude-repl-tool-output--filter-system-reminders raw-output)
                                  raw-output))
                   (tool-name (or (alist-get 'tool_name block) "Unknown"))
                   (tool-input (or (alist-get 'input block) nil)))
              (claude-repl-buffer-add-tool-output buffer tool-use-id tool-name tool-input tool-output))))))))

(defun claude-repl-buffer-handle-assistant-event (buffer event)
  "Handle an \\='assistant\\=' event in BUFFER from EVENT."
  (when-let* ((message (alist-get 'message event))
              (content (alist-get 'content message)))
    ;; Convert vector to list if needed
    (let ((content-list (if (vectorp content) (append content nil) content)))
      ;; Process each content block
      (dolist (block content-list)
        (let ((block-type (alist-get 'type block)))
          (cond
           ;; Text content
           ((equal block-type "text")
            (when-let* ((text (alist-get 'text block)))
              (claude-repl-buffer-append-text buffer text)))

           ;; Tool use
           ((equal block-type "tool_use")
            (let ((tool-id (alist-get 'id block))
                  (tool-name (alist-get 'name block))
                  (tool-input (alist-get 'input block)))
              (claude-repl-buffer-add-tool-use buffer tool-id tool-name tool-input)))))))))

(defun claude-repl-buffer-handle-result-event (buffer event)
  "Handle a \\='result\\=' event in BUFFER from EVENT."
  (let ((usage (alist-get 'usage event))
        (duration (alist-get 'duration_ms event)))
    (claude-repl-buffer-complete-interaction
     buffer
     (list (cons 'input_tokens (alist-get 'input_tokens usage))
           (cons 'output_tokens (alist-get 'output_tokens usage))
           (cons 'duration_ms duration)))))

;; ============================================================================
;; Convenience Functions
;; ============================================================================

(defun claude-repl-buffer-clear (buffer)
  "Clear all content from BUFFER and start fresh conversation."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      ;; Properly deallocate markers before erasing
      (when claude-repl-buffer-prompt-start-marker
        (set-marker claude-repl-buffer-prompt-start-marker nil))
      (when claude-repl-buffer-input-start-marker
        (set-marker claude-repl-buffer-input-start-marker nil))

      (erase-buffer)
      (setq-local claude-repl-buffer-current-interaction nil)
      (setq-local claude-repl-buffer-interactions nil)
      (setq-local claude-repl-buffer-prompt-start-marker nil)
      (setq-local claude-repl-buffer-input-start-marker nil)
      (setq-local claude-repl-buffer-session-id nil))  ; Reset session for fresh start
    ;; Set up fresh input area
    (claude-repl-buffer-setup-input-area)))

(provide 'claude-repl-buffer)
;;; claude-repl-buffer.el ends here
