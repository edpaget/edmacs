;;; claude-code-process.el --- Process management for Claude Code -*- lexical-binding: t -*-

;;; Commentary:
;; Manages non-interactive Claude Code processes per project.
;; Handles streaming JSON responses and conversation history.

;;; Code:

(require 'json)
(require 'projectile)

;; ============================================================================
;; Variables
;; ============================================================================

(defvar claude-code-processes (make-hash-table :test 'equal)
  "Hash table mapping project roots to Claude Code process objects.")

(defvar claude-code-executable "claude"
  "Path to the Claude Code executable.")

(defvar claude-code-default-model "sonnet"
  "Default model to use for Claude Code sessions.")

;; ============================================================================
;; Process Object Structure
;; ============================================================================

(cl-defstruct (claude-code-process
               (:constructor claude-code-process--create)
               (:copier nil))
  "Structure representing a Claude Code process."
  project-root          ; Project root directory
  process               ; Emacs process object
  session-id            ; Claude session ID (optional)
  buffer                ; Process output buffer
  partial-json          ; Partial JSON string being accumulated
  response-callbacks    ; List of callbacks for response events
  error-callbacks       ; List of callbacks for error events
  status                ; Process status: running, stopped, error
  last-prompt           ; Last prompt sent
  last-response         ; Last complete response received
  metadata)             ; Additional metadata

;; ============================================================================
;; Core Process Functions
;; ============================================================================

(defun claude-code-process--get-project-root ()
  "Get the current project root directory."
  (or (projectile-project-root)
      default-directory))

(defun claude-code-process--make-buffer-name (project-root)
  "Create a buffer name for the Claude Code process for PROJECT-ROOT."
  (format "*claude-code-process: %s*"
          (file-name-nondirectory (directory-file-name project-root))))

(defun claude-code-process--parse-json-event (json-string)
  "Parse a JSON event string from Claude Code output.
Returns the parsed object or nil if parsing fails."
  (condition-case err
      (json-read-from-string json-string)
    (error
     (message "Claude Code: Failed to parse JSON: %s" (error-message-string err))
     nil)))

(defun claude-code-process--handle-json-line (proc-obj line)
  "Handle a complete JSON line from the Claude process.
PROC-OBJ is the claude-code-process structure.
LINE is the complete JSON string to process."
  (when-let* ((event (claude-code-process--parse-json-event line)))
    ;; Store the last response for debugging
    (setf (claude-code-process-last-response proc-obj) event)

    ;; Call registered callbacks
    (dolist (callback (claude-code-process-response-callbacks proc-obj))
      (condition-case err
          (funcall callback event)
        (error
         (message "Claude Code: Callback error: %s" (error-message-string err)))))))

(defun claude-code-process--filter (process output)
  "Process filter for Claude Code process.
PROCESS is the Emacs process object.
OUTPUT is the new output string."
  (when-let* ((proc-obj (gethash (process-get process 'project-root)
                                 claude-code-processes)))
    (with-current-buffer (claude-code-process-buffer proc-obj)
      ;; Append output to buffer
      (goto-char (point-max))
      (insert output)

      ;; Accumulate partial JSON
      (let ((partial (or (claude-code-process-partial-json proc-obj) "")))
        (setq partial (concat partial output))

        ;; Process complete JSON lines
        (let ((lines (split-string partial "\n" t)))
          (if (string-suffix-p "\n" partial)
              ;; All lines are complete
              (progn
                (dolist (line lines)
                  (claude-code-process--handle-json-line proc-obj line))
                (setf (claude-code-process-partial-json proc-obj) ""))
            ;; Last line is incomplete
            (let ((complete-lines (butlast lines))
                  (incomplete-line (car (last lines))))
              (dolist (line complete-lines)
                (claude-code-process--handle-json-line proc-obj line))
              (setf (claude-code-process-partial-json proc-obj) incomplete-line))))))))

(defun claude-code-process--sentinel (process event)
  "Process sentinel for Claude Code process.
PROCESS is the Emacs process object.
EVENT is the process event string."
  (when-let* ((project-root (process-get process 'project-root))
              (proc-obj (gethash project-root claude-code-processes)))
    (cond
     ((string-match-p "^finished" event)
      (setf (claude-code-process-status proc-obj) 'stopped)
      (message "Claude Code process finished for %s"
               (file-name-nondirectory (directory-file-name project-root))))

     ((string-match-p "^exited abnormally\\|^failed\\|^killed" event)
      (setf (claude-code-process-status proc-obj) 'error)
      (message "Claude Code process error for %s: %s"
               (file-name-nondirectory (directory-file-name project-root))
               (string-trim event))

      ;; Call error callbacks
      (dolist (callback (claude-code-process-error-callbacks proc-obj))
        (condition-case err
            (funcall callback event)
          (error
           (message "Claude Code: Error callback failed: %s"
                    (error-message-string err)))))))))

(defun claude-code-process-start (project-root &optional session-id model)
  "Start a new Claude Code process for PROJECT-ROOT.
Optional SESSION-ID to resume a specific session.
Optional MODEL to use instead of default."
  (let* ((buffer-name (claude-code-process--make-buffer-name project-root))
         (buffer (generate-new-buffer buffer-name))
         (model (or model claude-code-default-model))
         (args (list "--print"
                     "--verbose"
                     "--output-format" "stream-json"
                     "--model" model))
         process proc-obj)

    ;; Add session-related arguments
    (when session-id
      (setq args (append args (list "--resume" session-id))))

    ;; Start the process
    (setq process
          (make-process
           :name (format "claude-code-%s"
                         (file-name-nondirectory (directory-file-name project-root)))
           :buffer buffer
           :command (cons claude-code-executable args)
           :connection-type 'pipe
           :filter #'claude-code-process--filter
           :sentinel #'claude-code-process--sentinel
           :noquery t))

    ;; Store project root in process for lookup
    (process-put process 'project-root project-root)

    ;; Create process object
    (setq proc-obj
          (claude-code-process--create
           :project-root project-root
           :process process
           :session-id session-id
           :buffer buffer
           :partial-json ""
           :response-callbacks nil
           :error-callbacks nil
           :status 'running
           :last-prompt nil
           :last-response nil
           :metadata (list :model model :started-at (current-time))))

    ;; Store in hash table
    (puthash project-root proc-obj claude-code-processes)

    (message "Started Claude Code process for %s (model: %s)"
             (file-name-nondirectory (directory-file-name project-root))
             model)

    proc-obj))

(defun claude-code-process-get (project-root)
  "Get the Claude Code process for PROJECT-ROOT, or nil if none exists."
  (gethash project-root claude-code-processes))

(defun claude-code-process-get-or-create (project-root)
  "Get or create a Claude Code process for PROJECT-ROOT."
  (or (claude-code-process-get project-root)
      (claude-code-process-start project-root)))

(defun claude-code-process-send-prompt (proc-obj prompt)
  "Send PROMPT to the Claude Code process PROC-OBJ.
Returns t if successful, nil otherwise."
  (unless (claude-code-process-p proc-obj)
    (error "Invalid Claude Code process object"))

  (let ((process (claude-code-process-process proc-obj)))
    (unless (and process (process-live-p process))
      (error "Claude Code process is not running"))

    ;; Store the prompt
    (setf (claude-code-process-last-prompt proc-obj) prompt)

    ;; Send the prompt followed by EOF
    (condition-case err
        (progn
          (process-send-string process prompt)
          (process-send-string process "\n")
          (process-send-eof process)
          t)
      (error
       (message "Claude Code: Failed to send prompt: %s" (error-message-string err))
       nil))))

(defun claude-code-process-kill (proc-obj)
  "Kill the Claude Code process PROC-OBJ and clean up resources."
  (unless (claude-code-process-p proc-obj)
    (error "Invalid Claude Code process object"))

  (let ((process (claude-code-process-process proc-obj))
        (buffer (claude-code-process-buffer proc-obj))
        (project-root (claude-code-process-project-root proc-obj)))

    ;; Kill the process
    (when (and process (process-live-p process))
      (kill-process process))

    ;; Kill the buffer
    (when (buffer-live-p buffer)
      (kill-buffer buffer))

    ;; Remove from hash table
    (remhash project-root claude-code-processes)

    (message "Killed Claude Code process for %s"
             (file-name-nondirectory (directory-file-name project-root)))))

(defun claude-code-process-kill-all ()
  "Kill all Claude Code processes."
  (interactive)
  (maphash (lambda (_key proc-obj)
             (claude-code-process-kill proc-obj))
           claude-code-processes)
  (message "Killed all Claude Code processes"))

(defun claude-code-process-add-response-callback (proc-obj callback)
  "Add a response CALLBACK to PROC-OBJ.
CALLBACK should be a function that takes a single argument (the JSON event)."
  (push callback (claude-code-process-response-callbacks proc-obj)))

(defun claude-code-process-add-error-callback (proc-obj callback)
  "Add an error CALLBACK to PROC-OBJ.
CALLBACK should be a function that takes a single argument (the error event)."
  (push callback (claude-code-process-error-callbacks proc-obj)))

(defun claude-code-process-alive-p (proc-obj)
  "Return t if PROC-OBJ's process is alive."
  (when-let* ((process (claude-code-process-process proc-obj)))
    (process-live-p process)))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defun claude-code-process-list-all ()
  "Return a list of all active Claude Code process objects."
  (let (processes)
    (maphash (lambda (_key proc-obj)
               (push proc-obj processes))
             claude-code-processes)
    processes))

(defun claude-code-process-current ()
  "Get the Claude Code process for the current project."
  (claude-code-process-get (claude-code-process--get-project-root)))

(defun claude-code-process-current-or-create ()
  "Get or create the Claude Code process for the current project."
  (claude-code-process-get-or-create (claude-code-process--get-project-root)))

;; ============================================================================
;; Interactive Commands
;; ============================================================================

(defun claude-code-process-start-current-project ()
  "Start a Claude Code process for the current project."
  (interactive)
  (let ((project-root (claude-code-process--get-project-root)))
    (if (claude-code-process-get project-root)
        (message "Claude Code process already running for this project")
      (claude-code-process-start project-root))))

(defun claude-code-process-kill-current-project ()
  "Kill the Claude Code process for the current project."
  (interactive)
  (if-let* ((proc-obj (claude-code-process-current)))
      (claude-code-process-kill proc-obj)
    (message "No Claude Code process running for this project")))

(defun claude-code-process-status-current-project ()
  "Show the status of the Claude Code process for the current project."
  (interactive)
  (if-let* ((proc-obj (claude-code-process-current)))
      (let* ((status (claude-code-process-status proc-obj))
             (metadata (claude-code-process-metadata proc-obj))
             (model (plist-get metadata :model))
             (alive (claude-code-process-alive-p proc-obj)))
        (message "Claude Code: status=%s, alive=%s, model=%s"
                 status alive model))
    (message "No Claude Code process running for this project")))

(provide 'claude-code-process)
;;; claude-code-process.el ends here
