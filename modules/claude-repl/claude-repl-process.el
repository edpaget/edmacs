;;; claude-repl-process.el --- Process management for Claude Code -*- lexical-binding: t -*-

;;; Commentary:
;; Manages non-interactive Claude Code processes per project.
;; Handles streaming JSON responses and conversation history.

;;; Code:

(require 'json)
(require 'projectile)
(require 'claude-repl-approval)

;; ============================================================================
;; Variables
;; ============================================================================

(defvar claude-repl-processes (make-hash-table :test 'equal)
  "Hash table mapping project roots to Claude Code process objects.")

(defvar claude-repl-executable "claude"
  "Path to the Claude Code executable.")

(defvar claude-repl-default-model "sonnet"
  "Default model to use for Claude Code sessions.")

(defvar claude-repl-process-directory
  (file-name-directory (or load-file-name
                           buffer-file-name
                           (error "Cannot determine claude-repl directory")))
  "Directory where claude-repl-process.el is installed.
This is captured at load-time to reliably locate the approval-hook.py script.")

(defun claude-repl-process--get-hook-script-path ()
  "Get the absolute path to the approval-hook.py script.
The script is located in the same directory as this file."
  (expand-file-name "approval-hook.py" claude-repl-process-directory))

;; ============================================================================
;; Process Object Structure
;; ============================================================================

(cl-defstruct (claude-repl-process
               (:constructor claude-repl-process--create)
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
  metadata              ; Additional metadata
  socket-path           ; Path to approval socket
  settings-file)        ; Path to temp settings file

;; ============================================================================
;; Core Process Functions
;; ============================================================================

(defun claude-repl-process--get-project-root ()
  "Get the current project root directory."
  (or (projectile-project-root)
      default-directory))

(defun claude-repl-process--make-buffer-name (project-root)
  "Create a buffer name for the Claude Code process for PROJECT-ROOT."
  (format "*claude-repl-process: %s*"
          (file-name-nondirectory (directory-file-name project-root))))

(defun claude-repl-process--parse-json-event (json-string)
  "Parse JSON-STRING event from Claude Code output.
Returns the parsed object or nil if parsing fails."
  (condition-case err
      (json-read-from-string json-string)
    (error
     (message "Claude Code: Failed to parse JSON: %s" (error-message-string err))
     nil)))

(defun claude-repl-process--handle-json-line (proc-obj line)
  "Handle a complete JSON line from the Claude process.
PROC-OBJ is the claude-repl-process structure.
LINE is the complete JSON string to process."
  (when-let* ((event (claude-repl-process--parse-json-event line)))
    ;; Store the last response for debugging
    (setf (claude-repl-process-last-response proc-obj) event)

    ;; Call registered callbacks
    (dolist (callback (claude-repl-process-response-callbacks proc-obj))
      (condition-case err
          (funcall callback event)
        (error
         (message "Claude Code: Callback error: %s" (error-message-string err)))))))

(defun claude-repl-process--filter (process output)
  "Process filter for Claude Code process.
PROCESS is the Emacs process object.
OUTPUT is the new output string."
  (when-let* ((proc-obj (gethash (process-get process 'project-root)
                                 claude-repl-processes)))
    (with-current-buffer (claude-repl-process-buffer proc-obj)
      ;; Append output to buffer
      (goto-char (point-max))
      (insert output)

      ;; Accumulate partial JSON
      (let ((partial (or (claude-repl-process-partial-json proc-obj) "")))
        (setq partial (concat partial output))

        ;; Process complete JSON lines
        (let ((lines (split-string partial "\n" t)))
          (if (string-suffix-p "\n" partial)
              ;; All lines are complete
              (progn
                (dolist (line lines)
                  (claude-repl-process--handle-json-line proc-obj line))
                (setf (claude-repl-process-partial-json proc-obj) ""))
            ;; Last line is incomplete
            (let ((complete-lines (butlast lines))
                  (incomplete-line (car (last lines))))
              (dolist (line complete-lines)
                (claude-repl-process--handle-json-line proc-obj line))
              (setf (claude-repl-process-partial-json proc-obj) incomplete-line))))))))

(defun claude-repl-process--sentinel (process event)
  "Process sentinel for Claude Code process.
PROCESS is the Emacs process object.
EVENT is the process event string."
  (when-let* ((project-root (process-get process 'project-root))
              (proc-obj (gethash project-root claude-repl-processes)))
    (cond
     ((string-match-p "^finished" event)
      (setf (claude-repl-process-status proc-obj) 'stopped)
      (message "Claude Code process finished for %s"
               (file-name-nondirectory (directory-file-name project-root))))

     ((string-match-p "^exited abnormally\\|^failed\\|^killed" event)
      (setf (claude-repl-process-status proc-obj) 'error)
      (message "Claude Code process error for %s: %s"
               (file-name-nondirectory (directory-file-name project-root))
               (string-trim event))

      ;; Call error callbacks
      (dolist (callback (claude-repl-process-error-callbacks proc-obj))
        (condition-case err
            (funcall callback event)
          (error
           (message "Claude Code: Error callback failed: %s"
                    (error-message-string err)))))))))

(defun claude-repl-process-start (project-root &optional session-id model)
  "Start a new Claude Code process for PROJECT-ROOT.
Optional SESSION-ID to resume a specific session.
Optional MODEL to use instead of default."
  (let* ((buffer-name (claude-repl-process--make-buffer-name project-root))
         (buffer (generate-new-buffer buffer-name))
         (model (or model claude-repl-default-model))
         (socket-path (claude-repl-approval-start-server project-root))
         (hook-script (claude-repl-process--get-hook-script-path))
         ;; Generate settings JSON with approval hook
         ;; Uses standalone Python script for reliable stdin/stdout handling over Unix socket
         ;; The script properly reads the full response by looping until complete JSON is received
         (settings-json (json-encode
                         `((hooks . ((PreToolUse . [((matcher . "*")
                                                     (hooks . [((type . "command")
                                                                (command . ,(format "python3 %s %s" hook-script socket-path))
                                                                (timeout . ,claude-repl-approval-timeout))]))])))
                           (defaultMode . "default"))))
         
         ;; Write settings to temp file
         (settings-file (make-temp-file "claude-settings" nil ".json" settings-json))

         (args (list "--print"
                     "--verbose"
                     "--output-format" "stream-json"
                     "--model" model
                     "--settings" settings-file))
         process proc-obj)

    (message "settings %s" settings-json)
    ;; Add session-related arguments
    (when session-id
      (setq args (append args (list "--resume" session-id))))

    (message "Starting Claude Code with approval hook enabled")
    (message "Socket path: %s" socket-path)
    (message "Settings file: %s" settings-file)

    ;; Start the process with default-directory set to project-root
    (setq process
          (let ((default-directory project-root))
            (make-process
             :name (format "claude-repl-%s"
                           (file-name-nondirectory (directory-file-name project-root)))
             :buffer buffer
             :command (cons claude-repl-executable args)
             :connection-type 'pipe
             :filter #'claude-repl-process--filter
             :sentinel #'claude-repl-process--sentinel
             :noquery t)))

    ;; Store project root in process for lookup
    (process-put process 'project-root project-root)

    ;; Create process object
    (setq proc-obj
          (claude-repl-process--create
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
           :metadata (list :model model :started-at (current-time))
           :socket-path socket-path
           :settings-file settings-file))

    ;; Store in hash table
    (puthash project-root proc-obj claude-repl-processes)

    (message "Started Claude Code process for %s (model: %s)"
             (file-name-nondirectory (directory-file-name project-root))
             model)

    proc-obj))

(defun claude-repl-process-get (project-root)
  "Get the Claude Code process for PROJECT-ROOT, or nil if none exists."
  (gethash project-root claude-repl-processes))

(defun claude-repl-process-get-or-create (project-root)
  "Get or create a Claude Code process for PROJECT-ROOT."
  (or (claude-repl-process-get project-root)
      (claude-repl-process-start project-root)))

(defun claude-repl-process-send-prompt (proc-obj prompt)
  "Send PROMPT to the Claude Code process PROC-OBJ.
Returns t if successful, nil otherwise."
  (unless (claude-repl-process-p proc-obj)
    (error "Invalid Claude Code process object"))

  (let ((process (claude-repl-process-process proc-obj)))
    (unless (and process (process-live-p process))
      (error "Claude Code process is not running"))

    ;; Store the prompt
    (setf (claude-repl-process-last-prompt proc-obj) prompt)

    ;; Send the prompt followed by EOF
    ;; Note: EOF ends this process session, so each prompt is independent
    ;; For conversation continuity, we rely on --resume with captured session-id
    (condition-case err
        (progn
          (process-send-string process prompt)
          (process-send-string process "\n")
          (process-send-eof process)
          t)
      (error
       (message "Claude Code: Failed to send prompt: %s" (error-message-string err))
       nil))))

(defun claude-repl-process-kill (proc-obj)
  "Kill the Claude Code process PROC-OBJ and clean up resources."
  (unless (claude-repl-process-p proc-obj)
    (error "Invalid Claude Code process object"))

  (let ((process (claude-repl-process-process proc-obj))
        (buffer (claude-repl-process-buffer proc-obj))
        (project-root (claude-repl-process-project-root proc-obj))
        (settings-file (claude-repl-process-settings-file proc-obj)))

    ;; Kill the process
    (when (and process (process-live-p process))
      (kill-process process))

    ;; Kill the buffer
    (when (buffer-live-p buffer)
      (kill-buffer buffer))

    ;; Stop approval server
    (claude-repl-approval-stop-server project-root)

    ;; Delete temp settings file
    (when (and settings-file (file-exists-p settings-file))
      (ignore-errors (delete-file settings-file)))

    ;; Remove from hash table
    (remhash project-root claude-repl-processes)

    (message "Killed Claude Code process for %s"
             (file-name-nondirectory (directory-file-name project-root)))))

(defun claude-repl-process-kill-all ()
  "Kill all Claude Code processes."
  (interactive)
  (maphash (lambda (_key proc-obj)
             (claude-repl-process-kill proc-obj))
           claude-repl-processes)
  (message "Killed all Claude Code processes"))

(defun claude-repl-process-add-response-callback (proc-obj callback)
  "Add a response CALLBACK to PROC-OBJ.
CALLBACK should be a function that takes a single argument (the JSON event)."
  (push callback (claude-repl-process-response-callbacks proc-obj)))

(defun claude-repl-process-add-error-callback (proc-obj callback)
  "Add an error CALLBACK to PROC-OBJ.
CALLBACK should be a function that takes a single argument (the error event)."
  (push callback (claude-repl-process-error-callbacks proc-obj)))

(defun claude-repl-process-alive-p (proc-obj)
  "Return t if PROC-OBJ's process is alive."
  (when-let* ((process (claude-repl-process-process proc-obj)))
    (process-live-p process)))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defun claude-repl-process-list-all ()
  "Return a list of all active Claude Code process objects."
  (let (processes)
    (maphash (lambda (_key proc-obj)
               (push proc-obj processes))
             claude-repl-processes)
    processes))

(defun claude-repl-process-current ()
  "Get the Claude Code process for the current project."
  (claude-repl-process-get (claude-repl-process--get-project-root)))

(defun claude-repl-process-current-or-create ()
  "Get or create the Claude Code process for the current project."
  (claude-repl-process-get-or-create (claude-repl-process--get-project-root)))

;; ============================================================================
;; Interactive Commands
;; ============================================================================

(defun claude-repl-process-start-current-project ()
  "Start a Claude Code process for the current project."
  (interactive)
  (let ((project-root (claude-repl-process--get-project-root)))
    (if (claude-repl-process-get project-root)
        (message "Claude Code process already running for this project")
      (claude-repl-process-start project-root))))

(defun claude-repl-process-kill-current-project ()
  "Kill the Claude Code process for the current project."
  (interactive)
  (if-let* ((proc-obj (claude-repl-process-current)))
      (claude-repl-process-kill proc-obj)
    (message "No Claude Code process running for this project")))

(defun claude-repl-process-status-current-project ()
  "Show the status of the Claude Code process for the current project."
  (interactive)
  (if-let* ((proc-obj (claude-repl-process-current)))
      (let* ((status (claude-repl-process-status proc-obj))
             (metadata (claude-repl-process-metadata proc-obj))
             (model (plist-get metadata :model))
             (alive (claude-repl-process-alive-p proc-obj)))
        (message "Claude Code: status=%s, alive=%s, model=%s"
                 status alive model))
    (message "No Claude Code process running for this project")))

(provide 'claude-repl-process)
;;; claude-repl-process.el ends here
