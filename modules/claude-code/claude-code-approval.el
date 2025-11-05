;;; claude-code-approval.el --- Tool approval system for Claude Code -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: tools, processes
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This module implements a hook-based tool authorization system for Claude Code.
;; It provides:
;; - Unix socket server for receiving approval requests from Claude Code hooks
;; - Policy-based auto-approval rules
;; - Interactive approval UI for user decisions
;; - Session-based "always allow/deny" rules

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Custom Variables

(defgroup claude-code-approval nil
  "Tool approval settings for Claude Code."
  :group 'claude-code
  :prefix "claude-code-approval-")

(defcustom claude-code-approval-mode 'hybrid
  "Approval mode for Claude Code tool usage.
- \\='interactive: Always show UI, wait for user decision
- \\='auto-approve: Use policy rules, no UI interaction
- \\='deny-all: Block all tools (read-only mode)
- \\='hybrid: Auto-approve safe tools, ask for risky ones (default)"
  :type '(choice (const :tag "Interactive (always ask)" interactive)
                 (const :tag "Auto-approve (use rules)" auto-approve)
                 (const :tag "Deny all (read-only)" deny-all)
                 (const :tag "Hybrid (safe tools auto, risky ask)" hybrid))
  :group 'claude-code-approval)

(defcustom claude-code-approval-rules
  '((tool "Read" action allow)
    (tool "Grep" action allow)
    (tool "Glob" action allow)
    (tool "WebFetch" pattern "domain:github.com" action allow))
  "List of approval rules for auto-approval mode.
Each rule is a plist with the following keys:
- tool: Tool name to match (string)
- action: \\='allow or \\='deny
- pattern: Optional pattern to match against tool input (string)

Rules are evaluated in order. First matching rule wins.
Deny rules take precedence over allow rules."
  :type '(repeat (plist :key-type symbol))
  :group 'claude-code-approval)

(defcustom claude-code-approval-default-action 'ask
  "Default action when no rule matches in auto-approve mode.
- \\='ask: Show interactive UI
- \\='allow: Allow by default
- \\='deny: Deny by default"
  :type '(choice (const :tag "Ask user" ask)
                 (const :tag "Allow" allow)
                 (const :tag "Deny" deny))
  :group 'claude-code-approval)

(defcustom claude-code-approval-timeout 600
  "Timeout in seconds for interactive approval requests.
After this timeout, the request is automatically denied.
This also sets the timeout for the Claude Code hook execution.
Default is 600 seconds (10 minutes)."
  :type 'integer
  :group 'claude-code-approval)

(defcustom claude-code-approval-silent t
  "When non-nil, suppress informational messages during approval.
This prevents the minibuffer from growing with verbose output.
Errors and important notifications will still be shown.
Default is t (silent mode enabled)."
  :type 'boolean
  :group 'claude-code-approval)

;;; Internal Variables

(defvar claude-code-approval-servers (make-hash-table :test 'equal)
  "Hash table mapping project roots to socket server info.
Each entry is a plist with :server (process) and :socket-path (string).")

(defvar claude-code-approval-session-rules (make-hash-table :test 'equal)
  "Session-wide \"always allow/deny\" rules created during interactive approval.
Each entry maps a rule key to \\='allow or \\='deny.")

(defvar claude-code-approval--pending-requests (make-hash-table :test 'equal)
  "Hash table of pending approval requests.
Maps request ID to plist with :process, :tool, :input, :timer.
This keeps strong references to client processes to prevent GC.")

(defvar claude-code-approval--active-processes (make-hash-table :test 'eq)
  "Hash table keeping strong references to active client processes.
Maps process object to t to prevent garbage collection.")

(defvar claude-code-approval--responded-processes (make-hash-table :test 'eq)
  "Hash table tracking processes we've already responded to.
Prevents duplicate responses if filter is called multiple times.")

;; Declare buffer-local variables to suppress byte-compiler warnings
;; These are set via setq-local in the approval UI buffer
(defvar claude-code-approval--current-tool nil
  "Current tool name being approved in this buffer.")
(defvar claude-code-approval--current-input nil
  "Current tool input being approved in this buffer.")
(defvar claude-code-approval--current-id nil
  "Current request ID being approved in this buffer.")
(defvar claude-code-approval--current-proc nil
  "Current client process for this approval request.")
(defvar claude-code-approval--previous-buffer nil
  "Buffer that was active before the approval buffer was displayed.")

;;; Helper Functions

(defun claude-code-approval--message (format-string &rest args)
  "Display a message unless `claude-code-approval-silent' is non-nil.
FORMAT-STRING and ARGS are passed to `message'."
  (unless claude-code-approval-silent
    (apply #'message format-string args)))

(defun claude-code-approval--format-tool-input (input)
  "Format tool INPUT in a readable way.
Returns a string with nicely formatted key-value pairs."
  (if (null input)
      "(no parameters)"
    (let ((lines nil))
      (dolist (pair input)
        (let* ((key (symbol-name (car pair)))
               (value (cdr pair))
               (value-str (cond
                          ((stringp value)
                           (if (> (length value) 100)
                               (concat (substring value 0 100) "...")
                             value))
                          ((null value) "nil")
                          (t (format "%S" value)))))
          (push (format "  %s: %s"
                       (propertize key 'face '(:foreground "#8be9fd" :weight bold))
                       (propertize value-str 'face 'font-lock-string-face))
                lines)))
      (string-join (nreverse lines) "\n"))))

;;; Socket Server Lifecycle

(defun claude-code-approval-start-server (project-root)
  "Start Unix socket server for approval requests in PROJECT-ROOT.
Returns the socket path."
  (let* ((socket-name (format "cc-approval-%s.sock"
                              (substring (secure-hash 'md5 project-root) 0 8)))
         (socket-path (expand-file-name socket-name temporary-file-directory)))

    ;; Clean up existing socket file if it exists
    (when (file-exists-p socket-path)
      (delete-file socket-path))

    ;; Stop any existing server for this project
    (claude-code-approval-stop-server project-root)

    (claude-code-approval--message "starting server %s" (format "cc-approval-%s"
                                          (secure-hash 'md5 project-root)))
    (condition-case err
        (let ((server (make-network-process
                       :name (format "cc-approval-%s"
                                     (substring (secure-hash 'md5 project-root) 0 8))
                       :server t
                       :family 'local
                       :service socket-path
                       :filter #'claude-code-approval--socket-filter
                       :sentinel #'claude-code-approval--socket-sentinel
                       :noquery t
                       :coding 'utf-8-unix)))

          (puthash project-root
                   (list :server server :socket-path socket-path)
                   claude-code-approval-servers)

          (claude-code-approval--message "Claude Code approval server listening on %s" socket-path)
          socket-path)

      (error
       (message "Failed to start approval server: %s" (error-message-string err))
       nil))))

(defun claude-code-approval-stop-server (project-root)
  "Stop the approval server for PROJECT-ROOT and clean up resources."
  (when-let* ((server-info (gethash project-root claude-code-approval-servers))
              (server (plist-get server-info :server))
              (socket-path (plist-get server-info :socket-path)))

    ;; Delete the process
    (when (process-live-p server)
      (delete-process server))

    ;; Clean up socket file
    (when (file-exists-p socket-path)
      (ignore-errors (delete-file socket-path)))

    ;; Remove from hash table
    (remhash project-root claude-code-approval-servers)

    (claude-code-approval--message "Claude Code approval server stopped for %s" project-root)))

(defun claude-code-approval--socket-sentinel (proc event)
  "Sentinel function for approval socket server.
PROC is the process, EVENT is the event string."
  (claude-code-approval--message "Socket sentinel for %s: %s (live: %s)" proc (string-trim event) (process-live-p proc))
  (unless (process-live-p proc)
    (claude-code-approval--message "Claude Code approval socket closed: %s" (string-trim event))
    ;; Clean up all references
    (remhash proc claude-code-approval--active-processes)
    (remhash proc claude-code-approval--responded-processes)))

(defun claude-code-approval--socket-filter (proc string)
  "Handle incoming approval request from hook via nc.
PROC is the client connection process.
STRING is the JSON request data."
  (claude-code-approval--message "Socket filter called for %s: %S (length: %d)" proc string (length string))

  (cond
   ;; Ignore if we've already processed/responded to this connection
   ((gethash proc claude-code-approval--responded-processes)
    (claude-code-approval--message "Already responded to process %s, ignoring" proc))

   ;; Ignore empty or whitespace-only data
   ((string-match-p "\\`[[:space:]]*\\'" string)
    (claude-code-approval--message "Received empty/whitespace data, ignoring"))

   ;; Only process if it looks like JSON
   ((not (string-match-p "\\`[[:space:]]*{" string))
    (claude-code-approval--message "Data doesn't look like JSON, ignoring: %S" string))

   ;; Process the JSON request
   (t
    (condition-case err
        (let* ((request (json-read-from-string string))
               (tool (alist-get 'tool_name request))
               (input (alist-get 'tool_input request))
               (id (or (alist-get 'id request)
                       (format "%s-%d" tool (random 100000)))))

          ;; Mark as being processed to prevent duplicate processing
          (puthash proc t claude-code-approval--responded-processes)

          ;; Keep a strong reference to the client process
          (puthash proc t claude-code-approval--active-processes)
          (claude-code-approval--message "Processing approval request for tool: %s" tool)

          ;; Get approval decision asynchronously
          ;; The callback will send the response through the process
          (claude-code-approval--get-decision-async
           tool input id proc))

      (error
       ;; On error, deny by default
       (let ((error-msg (format "Error processing request: %s"
                                (error-message-string err))))
         (message "Claude Code approval error: %s" error-msg)
         (when (and (process-live-p proc)
                    (not (gethash proc claude-code-approval--responded-processes)))
           (puthash proc t claude-code-approval--responded-processes)
           (let ((response (json-encode (claude-code-approval--make-hook-response "deny" error-msg))))
             (process-send-string proc (concat response "\n"))))
         ;; Remove reference
         (remhash proc claude-code-approval--active-processes)))))))

;;; Decision Logic

(defun claude-code-approval--send-response (proc decision)
  "Send DECISION response through PROC and close connection."
  (claude-code-approval--message "Sending response: %s (proc live: %s)" decision (process-live-p proc))
  (when (process-live-p proc)
    (let ((response (json-encode decision)))
      (process-send-string proc (concat response "\n"))
      (claude-code-approval--message "Response sent, closing connection")
      ;; Close the connection after sending response
      (process-send-eof proc)))
  ;; Remove the strong reference to allow GC
  (remhash proc claude-code-approval--active-processes))

(defun claude-code-approval--make-hook-response (decision reason)
  "Create a properly formatted hook response for PreToolUse.
DECISION should be \"allow\", \"deny\", or \"ask\".
REASON is the explanation string."
  `((hookSpecificOutput . ((hookEventName . "PreToolUse")
                           (permissionDecision . ,decision)
                           (permissionDecisionReason . ,reason)))))

(defun claude-code-approval--get-decision-async (tool input id proc)
  "Get approval decision for TOOL with INPUT asynchronously.
ID is the request identifier.
PROC is the client process to send the response to.
Calls callback with decision alist containing \\='behavior and \\='message."
  (pcase claude-code-approval-mode
    ('deny-all
     (claude-code-approval--send-response
      proc
      (claude-code-approval--make-hook-response "deny" "All tools denied by policy")))

    ('auto-approve
     (claude-code-approval--send-response
      proc
      (claude-code-approval--check-policy tool input)))

    ('interactive
     (claude-code-approval--request-user-approval-async tool input id proc))

    ('hybrid
     ;; Check policy first, fall back to interactive if no match
     (let* ((policy-decision (claude-code-approval--check-policy tool input))
            (hook-output (alist-get 'hookSpecificOutput policy-decision))
            (permission (alist-get 'permissionDecision hook-output)))
       (if (string= permission "ask")
           (claude-code-approval--request-user-approval-async tool input id proc)
         (claude-code-approval--send-response proc policy-decision))))

    (_
     (claude-code-approval--send-response
      proc
      (claude-code-approval--make-hook-response "deny" "Unknown approval mode")))))

(defun claude-code-approval--check-policy (tool input)
  "Check policy rules for TOOL with INPUT and return decision.
Returns properly formatted hook response."
  ;; First check session rules (from \"always allow/deny\" during session)
  (let ((session-key (claude-code-approval--make-rule-key tool input))
        session-action)
    (setq session-action (gethash session-key claude-code-approval-session-rules))
    (if session-action
        ;; Found session rule, return it
        (claude-code-approval--make-hook-response
         (symbol-name session-action)
         (format "Session rule: %s %s" session-action tool))

      ;; Check configured rules
      (let ((deny-rules nil)
            (allow-rules nil))

        ;; Separate deny and allow rules
        (dolist (rule claude-code-approval-rules)
          (when (claude-code-approval--rule-matches-p rule tool input)
            (let ((action (plist-get rule 'action)))
              (if (eq action 'deny)
                  (push rule deny-rules)
                (push rule allow-rules)))))

        ;; Deny rules take precedence
        (cond
         (deny-rules
          (claude-code-approval--make-hook-response
           "deny"
           (format "Denied by policy: %s" tool)))

         (allow-rules
          (claude-code-approval--make-hook-response
           "allow"
           (format "Allowed by policy: %s" tool)))

         ;; No matching rule - use default action
         (t
          (pcase claude-code-approval-default-action
            ('allow
             (claude-code-approval--make-hook-response
              "allow"
              (format "Allowed by default: %s" tool)))
            ('deny
             (claude-code-approval--make-hook-response
              "deny"
              (format "Denied by default: %s" tool)))
            ('ask
             (claude-code-approval--make-hook-response
              "ask"
              (format "No policy match for %s, interactive approval needed" tool)))
            (_
             (claude-code-approval--make-hook-response
              "deny"
              "Invalid default action")))))))))

(defun claude-code-approval--rule-matches-p (rule tool input)
  "Check if RULE matches TOOL with INPUT.
RULE is a plist with :tool, :action, and optional :pattern."
  (let ((rule-tool (plist-get rule 'tool))
        (pattern (plist-get rule 'pattern)))

    ;; Tool name must match
    (when (string= rule-tool tool)
      ;; If no pattern, it's a match
      (if (not pattern)
          t
        ;; Otherwise check pattern against input
        (claude-code-approval--pattern-matches-input-p pattern input)))))

(defun claude-code-approval--pattern-matches-input-p (pattern input)
  "Check if PATTERN matches INPUT.
PATTERN can be a glob pattern or regex.
INPUT is the tool input parameters (alist from JSON)."
  ;; Convert input to a searchable string representation
  (let ((input-str (json-encode input)))
    (cond
     ;; Check for glob patterns (e.g., "*.el", "test:*")
     ((string-match-p "[*?]" pattern)
      (let ((regex (wildcard-to-regexp pattern)))
        (string-match-p regex input-str)))

     ;; Otherwise treat as substring match
     (t
      (string-match-p (regexp-quote pattern) input-str)))))

(defun claude-code-approval--make-rule-key (tool input)
  "Create a hash key for TOOL and INPUT for session rules."
  (format "%s:%s" tool (json-encode input)))

;;; Interactive Approval UI

(defvar claude-code-approval--decision-timer nil
  "Timer for approval timeout countdown.")

(defvar claude-code-approval--timeout-remaining 0
  "Remaining seconds before timeout.")

(defun claude-code-approval--request-user-approval-async (tool input id proc)
  "Show interactive approval UI asynchronously.
TOOL is the tool name, INPUT is the tool parameters, ID is the request ID.
PROC is the client process to send the response to.
This function returns immediately after showing the UI."
  (let ((buffer (get-buffer-create "*Claude Code Approval*"))
        (previous-buffer (current-buffer)))

    ;; Reset timeout
    (setq claude-code-approval--timeout-remaining claude-code-approval-timeout)

    ;; Set up the approval buffer
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (claude-code-approval-mode)

        ;; Store request info for callbacks
        (setq-local claude-code-approval--current-tool tool
                    claude-code-approval--current-input input
                    claude-code-approval--current-id id
                    claude-code-approval--current-proc proc
                    claude-code-approval--previous-buffer previous-buffer)

        ;; Render the approval UI
        (claude-code-approval--render-ui tool input)))

    ;; Display the buffer and ensure it has focus
    (pop-to-buffer buffer)
    (select-window (get-buffer-window buffer))
    (raise-frame)

    ;; Force evil-mode to motion state if it's active
    ;; This allows vim motions while keeping our keybindings active
    (with-current-buffer buffer
      (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-motion-state))
        (evil-motion-state)))

    ;; Start timeout timer
    (setq claude-code-approval--decision-timer
          (run-with-timer 1 1 #'claude-code-approval--update-timeout-async buffer))))

(defun claude-code-approval--render-ui (tool input)
  "Render the approval UI for TOOL with INPUT in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Header
    (insert (propertize "Claude Code Tool Approval Request\n"
                        'face '(:weight bold :height 1.2))
            (propertize (make-string 60 ?─) 'face 'shadow)
            "\n\n")

    ;; Tool name with icon
    (insert (propertize "🔧 Tool: " 'face 'bold)
            (propertize tool 'face '(:foreground "#50fa7b" :weight bold))
            "\n\n")

    ;; Tool parameters - formatted nicely
    (insert (propertize "Parameters:\n" 'face 'bold))
    (insert (claude-code-approval--format-tool-input input))
    (insert "\n\n")

    ;; Timeout warning
    (insert (propertize "⏱ " 'face 'warning)
            (propertize (format "Auto-deny in %d seconds"
                                claude-code-approval--timeout-remaining)
                        'face 'warning
                        'claude-timeout-marker t)
            "\n\n")

    ;; Action buttons
    (insert (propertize "Actions:\n" 'face 'bold))
    (insert "  ")
    (insert-text-button "[ a ] Allow"
                        'action #'claude-code-approval--action-allow
                        'follow-link t
                        'face '(:background "green" :foreground "black" :weight bold))
    (insert "  ")
    (insert-text-button "[ d ] Deny"
                        'action #'claude-code-approval--action-deny
                        'follow-link t
                        'face '(:background "red" :foreground "white" :weight bold))
    (insert "\n  ")
    (insert-text-button "[ A ] Allow Always (this session)"
                        'action #'claude-code-approval--action-allow-always
                        'follow-link t
                        'face '(:background "dark green" :foreground "white"))
    (insert "  ")
    (insert-text-button "[ D ] Deny Always (this session)"
                        'action #'claude-code-approval--action-deny-always
                        'follow-link t
                        'face '(:background "dark red" :foreground "white"))
    (insert "\n\n")

    ;; Help text
    (insert (propertize "Press the button or use keyboard shortcuts (a/d/A/D)\n"
                        'face 'shadow))

    (goto-char (point-min))))

(defun claude-code-approval--update-timeout-async (buffer)
  "Update the timeout countdown in BUFFER asynchronously."
  (when (buffer-live-p buffer)
    (setq claude-code-approval--timeout-remaining
          (1- claude-code-approval--timeout-remaining))

    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        ;; Find and update the timeout marker
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "Auto-deny in [0-9]+ seconds" nil t)
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              (delete-region start end)
              (goto-char start)
              (insert (propertize (format "Auto-deny in %d seconds"
                                          claude-code-approval--timeout-remaining)
                                  'face 'warning
                                  'claude-timeout-marker t))))))

      ;; Auto-deny on timeout
      (when (<= claude-code-approval--timeout-remaining 0)
        (let ((previous-buf claude-code-approval--previous-buffer))
          (when (timerp claude-code-approval--decision-timer)
            (cancel-timer claude-code-approval--decision-timer)
            (setq claude-code-approval--decision-timer nil))
          ;; Send deny response
          (claude-code-approval--send-response
           claude-code-approval--current-proc
           (claude-code-approval--make-hook-response "deny" "Approval request timed out"))
          ;; Restore previous buffer and kill approval buffer
          (when (and previous-buf (buffer-live-p previous-buf))
            (switch-to-buffer previous-buf))
          (kill-buffer buffer))))))

(defun claude-code-approval--show-feedback (message-text)
  "Show feedback MESSAGE-TEXT in the approval buffer before closing."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert "\n\n")
    (insert (propertize "━" 'face 'shadow)
            (propertize (make-string 59 ?━) 'face 'shadow))
    (insert "\n")
    (insert (propertize message-text 'face '(:foreground "green" :weight bold)))
    (insert "\n"))
  ;; Show the message briefly before closing
  (sit-for 0.5))

(defun claude-code-approval--cleanup-request ()
  "Clean up approval request state and restore previous buffer."
  (when (timerp claude-code-approval--decision-timer)
    (cancel-timer claude-code-approval--decision-timer)
    (setq claude-code-approval--decision-timer nil))
  (let ((buffer (current-buffer))
        (previous-buffer claude-code-approval--previous-buffer))
    ;; Switch back to previous buffer if it's still alive
    (when (and previous-buffer (buffer-live-p previous-buffer))
      (switch-to-buffer previous-buffer))
    ;; Kill the approval buffer
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun claude-code-approval--action-allow (&optional _button)
  "Action for Allow button."
  (interactive)
  (let ((decision (claude-code-approval--make-hook-response
                   "allow"
                   (format "User approved: %s" claude-code-approval--current-tool))))
    (claude-code-approval--show-feedback "✓ ALLOWED - Sending response...")
    (claude-code-approval--send-response claude-code-approval--current-proc decision)
    (claude-code-approval--cleanup-request)))

(defun claude-code-approval--action-deny (&optional _button)
  "Action for Deny button."
  (interactive)
  (let ((decision (claude-code-approval--make-hook-response
                   "deny"
                   (format "User denied: %s" claude-code-approval--current-tool))))
    (claude-code-approval--show-feedback "✗ DENIED - Sending response...")
    (claude-code-approval--send-response claude-code-approval--current-proc decision)
    (claude-code-approval--cleanup-request)))

(defun claude-code-approval--action-allow-always (&optional _button)
  "Action for Allow Always button."
  (interactive)
  (let* ((rule-key (claude-code-approval--make-rule-key
                    claude-code-approval--current-tool
                    claude-code-approval--current-input))
         (decision (claude-code-approval--make-hook-response
                    "allow"
                    (format "User approved always (session): %s"
                            claude-code-approval--current-tool))))
    (puthash rule-key 'allow claude-code-approval-session-rules)
    (claude-code-approval--show-feedback "✓ ALLOWED ALWAYS (this session) - Sending response...")
    (claude-code-approval--send-response claude-code-approval--current-proc decision)
    (claude-code-approval--cleanup-request)))

(defun claude-code-approval--action-deny-always (&optional _button)
  "Action for Deny Always button."
  (interactive)
  (let* ((rule-key (claude-code-approval--make-rule-key
                    claude-code-approval--current-tool
                    claude-code-approval--current-input))
         (decision (claude-code-approval--make-hook-response
                    "deny"
                    (format "User denied always (session): %s"
                            claude-code-approval--current-tool))))
    (puthash rule-key 'deny claude-code-approval-session-rules)
    (claude-code-approval--show-feedback "✗ DENIED ALWAYS (this session) - Sending response...")
    (claude-code-approval--send-response claude-code-approval--current-proc decision)
    (claude-code-approval--cleanup-request)))

;;; Major Mode for Approval Buffer

(defvar claude-code-approval-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'claude-code-approval--action-allow)
    (define-key map (kbd "d") #'claude-code-approval--action-deny)
    (define-key map (kbd "A") #'claude-code-approval--action-allow-always)
    (define-key map (kbd "D") #'claude-code-approval--action-deny-always)
    (define-key map (kbd "q") #'claude-code-approval--action-deny)
    (define-key map (kbd "RET") #'claude-code-approval--action-allow)
    map)
  "Keymap for Claude Code approval buffer.")

(define-derived-mode claude-code-approval-mode special-mode "Claude-Approval"
  "Major mode for Claude Code tool approval requests.
\\{claude-code-approval-mode-map}"
  (setq buffer-read-only t
        truncate-lines nil)
  ;; Ensure our keymap takes precedence over special-mode-map
  (use-local-map claude-code-approval-mode-map)
  ;; Configure evil-mode to use motion state if available
  ;; This allows vim motions (h/j/k/l) while keeping our keybindings active
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'claude-code-approval-mode 'motion)))

;;; Buffer-local variables for approval requests
;; These are already declared as defvar above to suppress byte-compiler warnings
;; Here we make them explicitly buffer-local

(make-variable-buffer-local 'claude-code-approval--current-tool)
(make-variable-buffer-local 'claude-code-approval--current-input)
(make-variable-buffer-local 'claude-code-approval--current-id)
(make-variable-buffer-local 'claude-code-approval--current-proc)
(make-variable-buffer-local 'claude-code-approval--previous-buffer)

;;; Utility Functions

(defun claude-code-approval-clear-session-rules ()
  "Clear all session-based approval rules."
  (interactive)
  (clrhash claude-code-approval-session-rules)
  (message "Cleared all session approval rules"))

(defun claude-code-approval-show-session-rules ()
  "Display current session approval rules."
  (interactive)
  (if (hash-table-empty-p claude-code-approval-session-rules)
      (message "No session approval rules")
    (let ((rules nil))
      (maphash (lambda (key value)
                 (push (format "%s -> %s" key value) rules))
               claude-code-approval-session-rules)
      (message "Session approval rules:\n%s" (string-join rules "\n")))))

(provide 'claude-code-approval)
;;; claude-code-approval.el ends here
