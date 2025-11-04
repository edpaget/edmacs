;;; claude-code-buffer.el --- Response buffer UI for Claude Code -*- lexical-binding: t -*-

;;; Commentary:
;; Provides a beautiful, structured response buffer for Claude Code interactions.
;; Features markdown rendering, syntax highlighting, and organized sections.

;;; Code:

(require 'markdown-mode)
(require 'claude-code-process)

;; ============================================================================
;; Custom Faces
;; ============================================================================

(defgroup claude-code-buffer nil
  "Response buffer for Claude Code."
  :group 'tools)

(defface claude-code-prompt-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for prompt headers."
  :group 'claude-code-buffer)

(defface claude-code-response-face
  '((t :inherit default))
  "Face for response text."
  :group 'claude-code-buffer)

(defface claude-code-tool-face
  '((t :inherit font-lock-function-name-face))
  "Face for tool usage headers."
  :group 'claude-code-buffer)

(defface claude-code-timestamp-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for timestamps."
  :group 'claude-code-buffer)

(defface claude-code-separator-face
  '((t :inherit font-lock-comment-face))
  "Face for section separators."
  :group 'claude-code-buffer)

(defface claude-code-metadata-face
  '((t :inherit font-lock-doc-face :height 0.9))
  "Face for metadata (token counts, etc.)."
  :group 'claude-code-buffer)

;; ============================================================================
;; Buffer Management
;; ============================================================================

(defun claude-code-buffer--get-buffer-name (project-root)
  "Get the response buffer name for PROJECT-ROOT."
  (format "*claude-code: %s*"
          (file-name-nondirectory (directory-file-name project-root))))

(defun claude-code-buffer-get-or-create (project-root)
  "Get or create the response buffer for PROJECT-ROOT."
  (let ((buffer-name (claude-code-buffer--get-buffer-name project-root)))
    (or (get-buffer buffer-name)
        (with-current-buffer (generate-new-buffer buffer-name)
          (claude-code-buffer-mode)
          (setq-local claude-code-buffer-project-root project-root)
          ;; Set up initial input area
          (claude-code-buffer-setup-input-area)
          (current-buffer)))))

(defvar-local claude-code-buffer-project-root nil
  "Project root for this Claude Code buffer.")

(defvar-local claude-code-buffer-current-interaction nil
  "Marker for the current interaction being built.")

(defvar-local claude-code-buffer-interactions nil
  "List of all completed interactions in this buffer.")

(defvar-local claude-code-buffer-prompt-start-marker nil
  "Marker for the start of the current prompt.")

(defvar-local claude-code-buffer-input-start-marker nil
  "Marker for the start of the input area.")

(defvar-local claude-code-buffer-input-ring nil
  "Ring of previous inputs for history navigation.")

(defvar-local claude-code-buffer-input-ring-index nil
  "Current position in the input ring.")

(defvar-local claude-code-buffer-input-history-pattern nil
  "Pattern for searching history. Set by history search commands.")

(defconst claude-code-buffer-input-ring-size 100
  "Maximum number of inputs to store in history.")

(defvar-local claude-code-buffer-session-id nil
  "Session ID for conversation continuity across process restarts.")

;; ============================================================================
;; Buffer Mode
;; ============================================================================

;; Keymap for read-only history area with single-letter commands
(defvar claude-code-buffer-readonly-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'claude-code-buffer-refresh)
    (define-key map (kbd "c") #'claude-code-buffer-copy-last-response)
    map)
  "Keymap for read-only (history) area in Claude Code buffers.")

(defvar claude-code-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Always-available commands
    (define-key map (kbd "C-c C-c") #'claude-code-buffer-copy-code-block-at-point)
    (define-key map (kbd "C-c q") #'quit-window)
    (define-key map (kbd "C-c g") #'claude-code-buffer-refresh)

    ;; Phase 3: Navigation commands
    (define-key map (kbd "C-c C-n") #'claude-code-buffer-next-interaction)
    (define-key map (kbd "C-c C-p") #'claude-code-buffer-previous-interaction)
    (define-key map (kbd "C-M-n") #'claude-code-buffer-next-code-block)
    (define-key map (kbd "C-M-p") #'claude-code-buffer-previous-code-block)

    ;; Phase 3: Action commands
    (define-key map (kbd "C-c C-r") #'claude-code-buffer-resend-prompt)
    (define-key map (kbd "C-c C-s") #'claude-code-buffer-search-interactions)

    ;; Phase 4: Interactive REPL input
    (define-key map (kbd "RET") #'claude-code-buffer-return)
    (define-key map (kbd "C-c RET") #'claude-code-buffer-send-input)
    (define-key map (kbd "C-j") #'electric-newline-and-maybe-indent)
    (define-key map (kbd "M-p") #'claude-code-buffer-previous-input)
    (define-key map (kbd "M-n") #'claude-code-buffer-next-input)
    (define-key map (kbd "M-r") #'claude-code-buffer-history-search-backward)
    (define-key map (kbd "M-s") #'claude-code-buffer-history-search-forward)

    map)
  "Keymap for Claude Code buffer mode.")

(define-derived-mode claude-code-buffer-mode text-mode "Claude-Code"
  "Major mode for Claude Code response buffers.
Displays structured conversations with Claude including prompts,
responses, tool usage, and metadata.

\\{claude-code-buffer-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (visual-line-mode 1)
  ;; Ensure buffer is not globally read-only
  (setq buffer-read-only nil)
  ;; Initialize input history ring
  (setq-local claude-code-buffer-input-ring (make-ring claude-code-buffer-input-ring-size))
  (setq-local claude-code-buffer-input-ring-index nil)
  ;; Enable markdown syntax highlighting if available
  (when (fboundp 'markdown-mode)
    (font-lock-mode -1)
    (setq-local font-lock-defaults
                (list markdown-mode-font-lock-keywords t))
    (font-lock-mode 1))
  ;; Ensure mode-line is updated
  (force-mode-line-update))

(add-hook 'claude-code-buffer-mode-hook
          (lambda ()
            (setq mode-name "Claude-Code")
            (force-mode-line-update)))

;; ============================================================================
;; Interaction Structure
;; ============================================================================

(cl-defstruct claude-code-interaction
  "Structure representing a single prompt/response interaction."
  prompt              ; User's prompt text
  prompt-time         ; Timestamp when prompt was sent
  response-text       ; Accumulated response text
  tool-uses           ; List of tools used
  start-marker        ; Buffer position where interaction starts
  end-marker          ; Buffer position where interaction ends
  status              ; Status: streaming, complete, error
  metadata)           ; Additional metadata (tokens, duration, etc.)

;; ============================================================================
;; Buffer Manipulation Utilities
;; ============================================================================

(defmacro claude-code-buffer-save-markers (&rest body)
  "Execute BODY while preserving buffer markers.
Saves and restores positions of input-start and prompt-start markers."
  (declare (indent 0) (debug t))
  `(let ((input-pos (when (and claude-code-buffer-input-start-marker
                               (marker-position claude-code-buffer-input-start-marker))
                      (marker-position claude-code-buffer-input-start-marker)))
         (prompt-pos (when (and claude-code-buffer-prompt-start-marker
                                (marker-position claude-code-buffer-prompt-start-marker))
                       (marker-position claude-code-buffer-prompt-start-marker))))
     (prog1 (progn ,@body)
       (when (and claude-code-buffer-input-start-marker input-pos)
         (set-marker claude-code-buffer-input-start-marker input-pos))
       (when (and claude-code-buffer-prompt-start-marker prompt-pos)
         (set-marker claude-code-buffer-prompt-start-marker prompt-pos)))))

(defun claude-code-buffer-propertize-region (start end properties)
  "Apply PROPERTIES (a plist) to region from START to END.
More convenient than multiple put-text-property calls.

Example:
  (claude-code-buffer-propertize-region
    start end
    \\='(face claude-code-prompt-face
      read-only t
      field claude-code-prompt))"
  (let ((props properties))
    (while props
      (let ((prop (car props))
            (value (cadr props)))
        (put-text-property start end prop value)
        (setq props (cddr props))))))

(defun claude-code-buffer-get-field-bounds (&optional pos)
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

(defun claude-code-buffer--insert-with-face (text face)
  "Insert TEXT with FACE at point."
  (let ((start (point)))
    (insert text)
    (put-text-property start (point) 'face face)))

(defun claude-code-buffer--insert-separator ()
  "Insert a visual separator."
  (claude-code-buffer--insert-with-face
   "────────────────────────────────────────────────────────────────────────────────\n"
   'claude-code-separator-face))

(defun claude-code-buffer--insert-timestamp (time)
  "Insert formatted timestamp for TIME."
  (claude-code-buffer--insert-with-face
   (format-time-string "[%Y-%m-%d %H:%M:%S]" time)
   'claude-code-timestamp-face))

(defun claude-code-buffer-start-interaction (buffer prompt)
  "Start a new interaction in BUFFER with PROMPT."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (start-pos (point-max)))
      (goto-char (point-max))

      ;; Add separator if not first interaction
      (unless (= (point-min) (point-max))
        (insert "\n")
        (claude-code-buffer--insert-separator)
        (insert "\n"))

      ;; Insert prompt header
      (claude-code-buffer--insert-with-face "## Prompt " 'claude-code-prompt-face)
      (claude-code-buffer--insert-timestamp (current-time))
      (insert "\n\n")

      ;; Insert prompt text
      (insert prompt)
      (insert "\n\n")

      ;; Insert response header
      (claude-code-buffer--insert-with-face "## Response\n\n" 'claude-code-response-face)

      ;; Create interaction object
      (let ((interaction (make-claude-code-interaction
                          :prompt prompt
                          :prompt-time (current-time)
                          :response-text ""
                          :tool-uses nil
                          :start-marker (copy-marker start-pos)
                          :end-marker (point-marker)
                          :status 'streaming
                          :metadata nil)))
        (setq-local claude-code-buffer-current-interaction interaction)
        interaction))))

(defun claude-code-buffer-append-text (buffer text)
  "Append TEXT to the current interaction in BUFFER."
  (with-current-buffer buffer
    (when claude-code-buffer-current-interaction
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (claude-code-interaction-end-marker
                      claude-code-buffer-current-interaction))
          ;; Use insert-before-markers to automatically preserve marker positions
          (insert-before-markers text))

        ;; Update interaction
        (setf (claude-code-interaction-response-text
               claude-code-buffer-current-interaction)
              (concat (claude-code-interaction-response-text
                       claude-code-buffer-current-interaction)
                      text))))))

(defun claude-code-buffer-add-tool-use (buffer tool-name tool-input)
  "Add a tool use entry to BUFFER for TOOL-NAME with TOOL-INPUT."
  (with-current-buffer buffer
    (when claude-code-buffer-current-interaction
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (claude-code-interaction-end-marker
                      claude-code-buffer-current-interaction))
          (insert "\n\n")
          (claude-code-buffer--insert-with-face
           (format "**[Tool: %s]**\n" tool-name)
           'claude-code-tool-face)

          ;; Format tool input nicely
          (insert "```elisp\n")
          (insert (format "%S" tool-input))
          (insert "\n```\n")

          (set-marker (claude-code-interaction-end-marker
                       claude-code-buffer-current-interaction)
                      (point)))

        ;; Update interaction
        (push (list :tool tool-name :input tool-input)
              (claude-code-interaction-tool-uses
               claude-code-buffer-current-interaction))))))

(defun claude-code-buffer-complete-interaction (buffer &optional metadata)
  "Mark the current interaction in BUFFER as complete with optional METADATA."
  (with-current-buffer buffer
    (when claude-code-buffer-current-interaction
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (claude-code-interaction-end-marker
                      claude-code-buffer-current-interaction))
          (insert "\n\n")

          ;; Add metadata if provided
          (when metadata
            (let ((tokens-in (alist-get 'input_tokens metadata))
                  (tokens-out (alist-get 'output_tokens metadata))
                  (duration (alist-get 'duration_ms metadata)))
              (when (or tokens-in tokens-out duration)
                (claude-code-buffer--insert-with-face
                 (format "Tokens: %d in, %d out"
                         (or tokens-in 0)
                         (or tokens-out 0))
                 'claude-code-metadata-face)
                (when duration
                  (insert (format " • Duration: %.2fs" (/ duration 1000.0))))
                (insert "\n"))))

          (set-marker (claude-code-interaction-end-marker
                       claude-code-buffer-current-interaction)
                      (point)))

        ;; Update interaction
        (setf (claude-code-interaction-status
               claude-code-buffer-current-interaction) 'complete)
        (setf (claude-code-interaction-metadata
               claude-code-buffer-current-interaction) metadata)

        ;; Mark entire interaction as a field for better navigation
        (let ((start (marker-position (claude-code-interaction-start-marker
                                       claude-code-buffer-current-interaction)))
              (end (marker-position (claude-code-interaction-end-marker
                                     claude-code-buffer-current-interaction))))
          (put-text-property start end 'field 'claude-code-interaction))

        ;; Save to history
        (push claude-code-buffer-current-interaction
              claude-code-buffer-interactions)

        (setq-local claude-code-buffer-current-interaction nil))

      ;; Set up input area for next interaction
      (claude-code-buffer-setup-input-area))))

;; ============================================================================
;; Buffer Commands
;; ============================================================================

(defun claude-code-buffer-refresh ()
  "Refresh the current buffer (currently just a message)."
  (interactive)
  (message "Claude Code buffer refreshed"))

(defun claude-code-buffer-copy-last-response ()
  "Copy the last response to the kill ring."
  (interactive)
  (let ((interaction claude-code-buffer-current-interaction))
    (when interaction
      (let ((text (claude-code-interaction-response-text interaction)))
        (when text
          (kill-new text)
          (message "Copied response to kill ring"))))))

(defun claude-code-buffer-copy-code-block-at-point ()
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

(defun claude-code-buffer-next-interaction ()
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

(defun claude-code-buffer-previous-interaction ()
  "Move point to the previous interaction in the buffer."
  (interactive)
  (let ((start-pos (point)))
    (beginning-of-line)
    (if (re-search-backward "^## Prompt" nil t)
        (message "Moved to previous interaction")
      (goto-char start-pos)
      (message "No previous interaction"))))

(defun claude-code-buffer-get-interaction-at-point ()
  "Get the interaction structure at point."
  (save-excursion
    ;; Find the start of the current interaction
    (unless (looking-at "^## Prompt")
      (re-search-backward "^## Prompt" nil t))
    (let ((prompt-start (point)))
      ;; Find all interactions
      (dolist (interaction (reverse claude-code-buffer-interactions))
        (when (and (markerp (claude-code-interaction-start-marker interaction))
                   (marker-position (claude-code-interaction-start-marker interaction))
                   (= (marker-position (claude-code-interaction-start-marker interaction))
                      prompt-start))
          (cl-return interaction))))))

(defun claude-code-buffer-resend-prompt ()
  "Re-send the prompt from the interaction at point."
  (interactive)
  (let ((interaction (claude-code-buffer-get-interaction-at-point)))
    (if interaction
        (let ((prompt (claude-code-interaction-prompt interaction)))
          (if prompt
              (progn
                (require 'claude-code-core)
                (claude-code-ask prompt))
            (message "No prompt found in current interaction")))
      (message "Not in an interaction"))))

(defun claude-code-buffer-next-code-block ()
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

(defun claude-code-buffer-previous-code-block ()
  "Move point to the previous code block in the buffer."
  (interactive)
  (let ((start-pos (point)))
    (beginning-of-line)
    (if (re-search-backward "^```" nil t)
        (message "Moved to previous code block")
      (goto-char start-pos)
      (message "No previous code block"))))

(defun claude-code-buffer-search-interactions (query)
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

(defun claude-code-buffer-setup-input-area ()
  "Set up the input area at the end of the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      ;; Add input prompt
      (unless (bobp)
        (insert "\n\n"))
      ;; Insert prompt with better text property handling
      (let ((prompt-start (point)))
        ;; Mark the start of the prompt
        (setq-local claude-code-buffer-prompt-start-marker (copy-marker prompt-start))
        (set-marker-insertion-type claude-code-buffer-prompt-start-marker nil)

        (insert "> ")
        ;; Apply properties with specific rear-nonsticky list
        (put-text-property prompt-start (point) 'face 'claude-code-prompt-face)
        (put-text-property prompt-start (point) 'read-only t)
        (put-text-property prompt-start (point) 'field 'claude-code-prompt)
        (put-text-property prompt-start (point) 'intangible t)
        (put-text-property prompt-start (point) 'rear-nonsticky '(read-only face field intangible)))
      ;; Mark the start of the input area
      (setq-local claude-code-buffer-input-start-marker (point-marker))
      (set-marker-insertion-type claude-code-buffer-input-start-marker nil)))
  ;; Set up text properties for read-only history area
  (when claude-code-buffer-input-start-marker
    (let ((inhibit-read-only t)
          (marker-pos (marker-position claude-code-buffer-input-start-marker)))
      (when (> marker-pos (point-min))
        ;; Calculate history-end to exclude the "> " prompt (2 chars before marker)
        (let ((history-end (max (point-min) (- marker-pos 2))))
          ;; Make history read-only and add keymap for single-letter commands
          (put-text-property (point-min) history-end 'read-only t)
          (put-text-property (point-min) history-end 'keymap claude-code-buffer-readonly-map)))
      ;; Explicitly ensure the input area is NOT read-only and has NO special keymap
      (when (<= marker-pos (point-max))
        (remove-text-properties marker-pos (point-max) '(read-only nil keymap nil)))))
  ;; Move point to input area
  (goto-char (point-max))
  (message "Input area ready. Type your message and press C-c RET to send."))

(defun claude-code-buffer-in-input-area-p ()
  "Return t if point is in the input area."
  (and claude-code-buffer-input-start-marker
       (>= (point) claude-code-buffer-input-start-marker)))

(defun claude-code-buffer-input-complete-p ()
  "Check if the current input is complete and ready to send.
Returns t if input is non-empty after trimming."
  (when claude-code-buffer-input-start-marker
    (let ((input (claude-code-buffer-get-input)))
      (and input
           (not (string-empty-p (string-trim input)))))))

(defun claude-code-buffer-get-input ()
  "Get the current input text."
  (when claude-code-buffer-input-start-marker
    (buffer-substring-no-properties
     (marker-position claude-code-buffer-input-start-marker)
     (point-max))))

(defun claude-code-buffer-clear-input ()
  "Clear the input area."
  (when claude-code-buffer-input-start-marker
    (let ((inhibit-read-only t))
      (delete-region claude-code-buffer-input-start-marker (point-max)))))

(defun claude-code-buffer-clear-input-area ()
  "Clear and properly deallocate the input area.
This function clears the input text and deallocates both the
prompt-start and input-start markers to prevent memory leaks."
  (when claude-code-buffer-input-start-marker
    (let ((inhibit-read-only t))
      ;; Clear content from prompt-start (if available) or input-start to end
      (let ((clear-start (if (and claude-code-buffer-prompt-start-marker
                                  (marker-position claude-code-buffer-prompt-start-marker))
                             claude-code-buffer-prompt-start-marker
                           claude-code-buffer-input-start-marker)))
        (delete-region clear-start (point-max)))

      ;; Properly deallocate markers
      (when claude-code-buffer-prompt-start-marker
        (set-marker claude-code-buffer-prompt-start-marker nil))
      (set-marker claude-code-buffer-input-start-marker nil)

      ;; Clear variables
      (setq-local claude-code-buffer-prompt-start-marker nil)
      (setq-local claude-code-buffer-input-start-marker nil))))

(defun claude-code-buffer-send-input ()
  "Send the current input to Claude."
  (interactive)
  (cond
   ;; No input marker set up
   ((not claude-code-buffer-input-start-marker)
    (user-error "No input area available. Try reopening the buffer"))

   ;; Input not complete or empty
   ((not (claude-code-buffer-input-complete-p))
    (user-error "Please enter some text before sending"))

   ;; Everything OK, send it
   (t
    (let ((input (string-trim (claude-code-buffer-get-input))))
      ;; Add to history
      (ring-insert claude-code-buffer-input-ring input)
      (setq-local claude-code-buffer-input-ring-index nil)

      ;; Clear input area and properly deallocate markers
      (claude-code-buffer-clear-input-area)

      ;; Send to Claude
      (require 'claude-code-core)
      (claude-code-ask input)

      (message "Sent: %s" (truncate-string-to-width input 50 nil nil "..."))))))

(defun claude-code-buffer-return ()
  "Smart RET: send input if in input area, otherwise normal behavior."
  (interactive)
  (if (claude-code-buffer-in-input-area-p)
      (claude-code-buffer-send-input)
    ;; In read-only area, just move to next line or do nothing
    (when (not (eobp))
      (forward-line 1))))

(defun claude-code-buffer-previous-input ()
  "Insert the previous input from history."
  (interactive)
  (when (and claude-code-buffer-input-ring
             (not (ring-empty-p claude-code-buffer-input-ring)))
    (let ((inhibit-read-only t))
      ;; Initialize index if needed
      (unless claude-code-buffer-input-ring-index
        (setq-local claude-code-buffer-input-ring-index 0))

      ;; Get previous input
      (let ((input (ring-ref claude-code-buffer-input-ring
                             claude-code-buffer-input-ring-index)))
        (claude-code-buffer-clear-input)
        (insert input)

        ;; Move to next older input
        (when (< (1+ claude-code-buffer-input-ring-index)
                 (ring-length claude-code-buffer-input-ring))
          (setq-local claude-code-buffer-input-ring-index
                      (1+ claude-code-buffer-input-ring-index)))))))

(defun claude-code-buffer-next-input ()
  "Insert the next input from history."
  (interactive)
  (when (and claude-code-buffer-input-ring
             claude-code-buffer-input-ring-index
             (> claude-code-buffer-input-ring-index 0))
    (let ((inhibit-read-only t))
      ;; Move to next newer input
      (setq-local claude-code-buffer-input-ring-index
                  (1- claude-code-buffer-input-ring-index))

      ;; Get and insert input
      (let ((input (ring-ref claude-code-buffer-input-ring
                             claude-code-buffer-input-ring-index)))
        (claude-code-buffer-clear-input)
        (insert input)))))

(defun claude-code-buffer-history-search-backward (pattern)
  "Search backward in history for entries matching PATTERN (regex).
Updates input area with first match found. When called repeatedly,
continues searching through older matches."
  (interactive "sSearch history backward (regex): ")
  (setq-local claude-code-buffer-input-history-pattern pattern)
  (when (and claude-code-buffer-input-ring
             (not (ring-empty-p claude-code-buffer-input-ring)))
    (let ((inhibit-read-only t)
          (start-index (if (and claude-code-buffer-input-ring-index
                                (eq last-command 'claude-code-buffer-history-search-backward))
                           (1+ claude-code-buffer-input-ring-index)
                         0))
          (found nil)
          (found-index nil))
      ;; Search through history from start-index
      (let ((index start-index))
        (while (and (< index (ring-length claude-code-buffer-input-ring))
                    (not found))
          (let ((input (ring-ref claude-code-buffer-input-ring index)))
            (when (string-match-p pattern input)
              (setq found input)
              (setq found-index index)))
          (setq index (1+ index))))

      ;; Insert if found
      (if found
          (progn
            (claude-code-buffer-clear-input)
            (insert found)
            (setq-local claude-code-buffer-input-ring-index found-index)
            (message "Found [%d/%d]: %s"
                     (1+ found-index)
                     (ring-length claude-code-buffer-input-ring)
                     (truncate-string-to-width found 50 nil nil "...")))
        (message "No match for: %s" pattern)))))

(defun claude-code-buffer-history-search-forward (pattern)
  "Search forward in history for entries matching PATTERN (regex).
Updates input area with first match found. When called repeatedly,
continues searching through newer matches."
  (interactive "sSearch history forward (regex): ")
  (setq-local claude-code-buffer-input-history-pattern pattern)
  (when (and claude-code-buffer-input-ring
             claude-code-buffer-input-ring-index
             (> claude-code-buffer-input-ring-index 0))
    (let ((inhibit-read-only t)
          (start-index (if (eq last-command 'claude-code-buffer-history-search-forward)
                           (1- claude-code-buffer-input-ring-index)
                         (1- claude-code-buffer-input-ring-index)))
          (found nil)
          (found-index nil))
      ;; Search backward through history
      (let ((index start-index))
        (while (and (>= index 0) (not found))
          (let ((input (ring-ref claude-code-buffer-input-ring index)))
            (when (string-match-p pattern input)
              (setq found input)
              (setq found-index index)))
          (setq index (1- index))))

      ;; Insert if found
      (if found
          (progn
            (claude-code-buffer-clear-input)
            (insert found)
            (setq-local claude-code-buffer-input-ring-index found-index)
            (message "Found [%d/%d]: %s"
                     (1+ found-index)
                     (ring-length claude-code-buffer-input-ring)
                     (truncate-string-to-width found 50 nil nil "...")))
        (message "No match for: %s" pattern)))))

;; ============================================================================
;; Event Handlers (for use with process callbacks)
;; ============================================================================

(defun claude-code-buffer-handle-assistant-event (buffer event)
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
              (claude-code-buffer-append-text buffer text)))

           ;; Tool use
           ((equal block-type "tool_use")
            (let ((tool-name (alist-get 'name block))
                  (tool-input (alist-get 'input block)))
              (claude-code-buffer-add-tool-use buffer tool-name tool-input)))))))))

(defun claude-code-buffer-handle-result-event (buffer event)
  "Handle a \\='result\\=' event in BUFFER from EVENT."
  (let ((usage (alist-get 'usage event))
        (duration (alist-get 'duration_ms event)))
    (claude-code-buffer-complete-interaction
     buffer
     (list (cons 'input_tokens (alist-get 'input_tokens usage))
           (cons 'output_tokens (alist-get 'output_tokens usage))
           (cons 'duration_ms duration)))))

;; ============================================================================
;; Convenience Functions
;; ============================================================================

(defun claude-code-buffer-clear (buffer)
  "Clear all content from BUFFER and start fresh conversation."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      ;; Properly deallocate markers before erasing
      (when claude-code-buffer-prompt-start-marker
        (set-marker claude-code-buffer-prompt-start-marker nil))
      (when claude-code-buffer-input-start-marker
        (set-marker claude-code-buffer-input-start-marker nil))

      (erase-buffer)
      (setq-local claude-code-buffer-current-interaction nil)
      (setq-local claude-code-buffer-interactions nil)
      (setq-local claude-code-buffer-prompt-start-marker nil)
      (setq-local claude-code-buffer-input-start-marker nil)
      (setq-local claude-code-buffer-session-id nil))  ; Reset session for fresh start
    ;; Set up fresh input area
    (claude-code-buffer-setup-input-area)))

(provide 'claude-code-buffer)
;;; claude-code-buffer.el ends here
