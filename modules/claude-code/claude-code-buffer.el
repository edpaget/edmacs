;;; claude-code-buffer.el --- Response buffer UI for Claude Code -*- lexical-binding: t -*-

;;; Commentary:
;; Provides a beautiful, structured response buffer for Claude Code interactions.
;; Features markdown rendering, syntax highlighting, and organized sections.

;;; Code:

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
          (current-buffer)))))

(defvar-local claude-code-buffer-project-root nil
  "Project root for this Claude Code buffer.")

(defvar-local claude-code-buffer-current-interaction nil
  "Marker for the current interaction being built.")

;; ============================================================================
;; Buffer Mode
;; ============================================================================

(defvar claude-code-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'claude-code-buffer-refresh)
    (define-key map (kbd "c") #'claude-code-buffer-copy-last-response)
    (define-key map (kbd "C-c C-c") #'claude-code-buffer-copy-code-block-at-point)
    map)
  "Keymap for Claude Code buffer mode.")

(define-derived-mode claude-code-buffer-mode markdown-mode "Claude-Code"
  "Major mode for Claude Code response buffers.
Displays structured conversations with Claude including prompts,
responses, tool usage, and metadata.

\\{claude-code-buffer-mode-map}"
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (visual-line-mode 1))

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
          (insert text)
          (set-marker (claude-code-interaction-end-marker
                       claude-code-buffer-current-interaction)
                      (point)))

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
        (setq-local claude-code-buffer-current-interaction nil)))))

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
    (let ((start (re-search-backward "^```" nil t))
          (end (when start (forward-line 1)
                     (re-search-forward "^```" nil t))))
      (when (and start end)
        (let ((code (buffer-substring-no-properties
                     (save-excursion (goto-char start) (forward-line 1) (point))
                     (save-excursion (goto-char end) (forward-line -1) (point)))))
          (kill-new code)
          (message "Copied code block to kill ring"))))))

;; ============================================================================
;; Event Handlers (for use with process callbacks)
;; ============================================================================

(defun claude-code-buffer-handle-assistant-event (buffer event)
  "Handle an 'assistant' event in BUFFER from EVENT."
  (when-let* ((message (alist-get 'message event))
              (content (alist-get 'content message)))
    ;; Process each content block
    (seq-do
     (lambda (block)
       (let ((block-type (alist-get 'type block)))
         (cond
          ;; Text content
          ((equal block-type "text")
           (when-let ((text (alist-get 'text block)))
             (claude-code-buffer-append-text buffer text)))

          ;; Tool use
          ((equal block-type "tool_use")
           (let ((tool-name (alist-get 'name block))
                 (tool-input (alist-get 'input block)))
             (claude-code-buffer-add-tool-use buffer tool-name tool-input))))))
     content)))

(defun claude-code-buffer-handle-result-event (buffer event)
  "Handle a 'result' event in BUFFER from EVENT."
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
  "Clear all content from BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq-local claude-code-buffer-current-interaction nil))))

(provide 'claude-code-buffer)
;;; claude-code-buffer.el ends here
