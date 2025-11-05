;;; claude-code-core.el --- Core Claude Code REPL integration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Claude Code Contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.7") (projectile "2.9.1"))
;; Keywords: tools, ai, claude
;; URL: https://github.com/edpaget/edmacs/modules/claude-code

;;; Commentary:
;; Main entry point for Claude Code REPL-style integration.
;; Provides high-level commands for interacting with Claude Code.
;;
;; This package provides a beautiful REPL-style interface for Claude Code
;; with markdown rendering, syntax highlighting, and structured conversation history.

;;; Code:

(require 'claude-code-process)
(require 'claude-code-buffer)
(require 'claude-code-approval)

;; ============================================================================
;; Interactive Commands
;; ============================================================================

(defun claude-code-test-prompt (prompt)
  "Send a test PROMPT to Claude Code and display raw JSON responses.
This is a debugging command to test the process management layer."
  (interactive "sPrompt: ")
  (let* ((proc-obj (claude-code-process-current-or-create))
         (response-buffer (get-buffer-create "*claude-code-test-output*")))

    ;; Add a callback to display events
    (claude-code-process-add-response-callback
     proc-obj
     (lambda (event)
       (with-current-buffer response-buffer
         (goto-char (point-max))
         (insert (format "\n=== Event: %s ===\n" (alist-get 'type event)))
         (insert (json-encode event))
         (insert "\n"))))

    ;; Send the prompt
    (if (claude-code-process-send-prompt proc-obj prompt)
        (progn
          (pop-to-buffer response-buffer)
          (with-current-buffer response-buffer
            (goto-char (point-max))
            (insert (format "\n\n>>> SENT PROMPT: %s\n" prompt)))
          (message "Prompt sent. Watch *claude-code-test-output* for responses."))
      (message "Failed to send prompt"))))

(defun claude-code-ask (prompt)
  "Ask Claude a question with PROMPT using the response buffer.
Each prompt in the same buffer continues the conversation using
session continuity."
  (interactive "sAsk Claude: ")
  (let* ((project-root (claude-code-process--get-project-root))
         (response-buffer (claude-code-buffer-get-or-create project-root))
         ;; Get session ID from buffer for conversation continuity
         (session-id (with-current-buffer response-buffer
                       claude-code-buffer-session-id))
         ;; Start new process with session-id to continue conversation
         (proc-obj (claude-code-process-start project-root session-id)))

    ;; Start a new interaction in the buffer
    (claude-code-buffer-start-interaction response-buffer prompt)

    ;; Display the buffer
    (pop-to-buffer response-buffer)

    ;; Add callback to handle events
    (claude-code-process-add-response-callback
     proc-obj
     (lambda (event)
       (let ((event-type (alist-get 'type event)))
         (cond
          ;; System event - may contain session ID for conversation continuity
          ((equal event-type "system")
           (when-let* ((session-id (alist-get 'session_id event)))
             ;; Store in buffer for conversation continuity
             (with-current-buffer response-buffer
               (setq-local claude-code-buffer-session-id session-id))
             (message "Claude Code: Session ID captured (%s)" session-id)))

          ;; Assistant message - contains the actual response
          ((equal event-type "assistant")
           (claude-code-buffer-handle-assistant-event response-buffer event))

          ;; Result - final summary
          ((equal event-type "result")
           (claude-code-buffer-handle-result-event response-buffer event)
           ;; Kill process after response - will restart with --resume for next prompt
           (run-with-timer
            0.5 nil
            (lambda ()
              (when (claude-code-process-p proc-obj)
                (claude-code-process-kill proc-obj))))
           (message "Claude Code: Response complete"))))))

    ;; Add error callback
    (claude-code-process-add-error-callback
     proc-obj
     (lambda (event)
       (with-current-buffer response-buffer
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert (format "\n\n**ERROR:** %s\n" event))))))

    ;; Send the prompt
    (if (claude-code-process-send-prompt proc-obj prompt)
        (message "Asking Claude...")
      (message "Failed to send prompt"))))

;; Alias for backward compatibility
(defalias 'claude-code-quick-ask 'claude-code-ask)

(defun claude-code-open-buffer ()
  "Open the Claude Code response buffer for the current project."
  (interactive)
  (let* ((project-root (claude-code-process--get-project-root))
         (buffer (claude-code-buffer-get-or-create project-root)))
    (pop-to-buffer buffer)))

(defun claude-code-clear-buffer ()
  "Clear the Claude Code response buffer for the current project."
  (interactive)
  (let* ((project-root (claude-code-process--get-project-root))
         (buffer (get-buffer (claude-code-buffer--get-buffer-name project-root))))
    (if buffer
        (progn
          (claude-code-buffer-clear buffer)
          (message "Cleared Claude Code buffer"))
      (message "No Claude Code buffer for this project"))))

(defun claude-code-show-processes ()
  "Show all active Claude Code processes."
  (interactive)
  (let ((processes (claude-code-process-list-all))
        (buffer (get-buffer-create "*claude-code-processes*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Active Claude Code Processes\n")
      (insert "============================\n\n")
      (if (null processes)
          (insert "No active processes.\n")
        (dolist (proc-obj processes)
          (let* ((project-root (claude-code-process-project-root proc-obj))
                 (status (claude-code-process-status proc-obj))
                 (alive (claude-code-process-alive-p proc-obj))
                 (metadata (claude-code-process-metadata proc-obj))
                 (model (plist-get metadata :model)))
            (insert (format "Project: %s\n" project-root))
            (insert (format "  Status: %s\n" status))
            (insert (format "  Alive: %s\n" alive))
            (insert (format "  Model: %s\n" model))
            (insert "\n"))))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

;; ============================================================================
;; Approval Management Commands
;; ============================================================================

(defun claude-code-approval-set-mode (mode)
  "Set the approval MODE for Claude Code tool usage.
MODE can be: interactive, auto-approve, deny-all, or hybrid."
  (interactive
   (list (intern (completing-read "Approval mode: "
                                  '("interactive" "auto-approve" "deny-all" "hybrid")
                                  nil t))))
  (setq claude-code-approval-mode mode)
  (message "Claude Code approval mode set to: %s" mode))

(defun claude-code-approval-show-policy ()
  "Display current approval policy rules and settings."
  (interactive)
  (let ((buffer (get-buffer-create "*claude-code-approval-policy*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (propertize "Claude Code Approval Policy\n"
                          'face '(:weight bold :height 1.2))
              (propertize (make-string 60 ?=) 'face 'shadow)
              "\n\n")

      ;; Current mode
      (insert (propertize "Current Mode: " 'face 'bold)
              (propertize (symbol-name claude-code-approval-mode)
                          'face '(:foreground "cyan" :weight bold))
              "\n\n")

      ;; Default action
      (insert (propertize "Default Action: " 'face 'bold)
              (format "%s\n\n" claude-code-approval-default-action))

      ;; Policy rules
      (insert (propertize "Policy Rules:\n" 'face 'bold))
      (if (null claude-code-approval-rules)
          (insert "  No rules configured.\n")
        (dolist (rule claude-code-approval-rules)
          (let ((tool (plist-get rule 'tool))
                (action (plist-get rule 'action))
                (pattern (plist-get rule 'pattern)))
            (insert (format "  • %s: %s"
                            (propertize (symbol-name action)
                                        'face (if (eq action 'allow)
                                                  'success
                                                'error))
                            tool))
            (when pattern
              (insert (format " (pattern: %s)" pattern)))
            (insert "\n"))))
      (insert "\n")

      ;; Session rules
      (insert (propertize "Session Rules:\n" 'face 'bold))
      (if (hash-table-empty-p claude-code-approval-session-rules)
          (insert "  No session rules.\n")
        (maphash (lambda (key value)
                   (insert (format "  • %s: %s\n"
                                   (propertize (symbol-name value)
                                               'face (if (eq value 'allow)
                                                         'success
                                                       'error))
                                   key)))
                 claude-code-approval-session-rules))

      (goto-char (point-min))
      (help-mode))
    (pop-to-buffer buffer)))

(defun claude-code-approval-add-allow-rule (tool &optional pattern)
  "Add a rule to always allow TOOL, optionally matching PATTERN."
  (interactive "sTool name: \nsPattern (optional): ")
  (let ((rule (if (and pattern (not (string-empty-p pattern)))
                  (list 'tool tool 'action 'allow 'pattern pattern)
                (list 'tool tool 'action 'allow))))
    (add-to-list 'claude-code-approval-rules rule)
    (message "Added allow rule for %s%s"
             tool
             (if (and pattern (not (string-empty-p pattern)))
                 (format " with pattern: %s" pattern)
               ""))))

(defun claude-code-approval-add-deny-rule (tool &optional pattern)
  "Add a rule to always deny TOOL, optionally matching PATTERN."
  (interactive "sTool name: \nsPattern (optional): ")
  (let ((rule (if (and pattern (not (string-empty-p pattern)))
                  (list 'tool tool 'action 'deny 'pattern pattern)
                (list 'tool tool 'action 'deny))))
    (add-to-list 'claude-code-approval-rules rule)
    (message "Added deny rule for %s%s"
             tool
             (if (and pattern (not (string-empty-p pattern)))
                 (format " with pattern: %s" pattern)
               ""))))

(defun claude-code-approval-reset-rules ()
  "Reset approval rules to defaults."
  (interactive)
  (when (yes-or-no-p "Reset all approval rules to defaults? ")
    (setq claude-code-approval-rules
          '((tool "Read" action allow)
            (tool "Grep" action allow)
            (tool "Glob" action allow)
            (tool "WebFetch" pattern "domain:github.com" action allow)))
    (message "Approval rules reset to defaults")))

;; ============================================================================
;; Keybindings
;; ============================================================================

(defun claude-code-core-setup-keybindings ()
  "Set up keybindings for Claude Code core functions."
  (when (fboundp 'general-define-key)
    (general-define-key
     :states 'normal
     :prefix "SPC a c"
     "" '(:ignore t :which-key "claude code (repl)")
     "a" '(claude-code-ask :which-key "ask claude")
     "b" '(claude-code-open-buffer :which-key "open buffer")
     "c" '(claude-code-clear-buffer :which-key "clear buffer")
     "s" '(claude-code-process-start-current-project :which-key "start process")
     "k" '(claude-code-process-kill-current-project :which-key "kill process")
     "K" '(claude-code-process-kill-all :which-key "kill all processes")
     "l" '(claude-code-show-processes :which-key "list processes")
     "i" '(claude-code-process-status-current-project :which-key "status")
     "t" '(claude-code-test-prompt :which-key "test prompt (debug)"))

    ;; Approval management keybindings
    (general-define-key
     :states 'normal
     :prefix "SPC a c p"
     "" '(:ignore t :which-key "approval policy")
     "m" '(claude-code-approval-set-mode :which-key "set mode")
     "s" '(claude-code-approval-show-policy :which-key "show policy")
     "a" '(claude-code-approval-add-allow-rule :which-key "add allow rule")
     "d" '(claude-code-approval-add-deny-rule :which-key "add deny rule")
     "r" '(claude-code-approval-reset-rules :which-key "reset rules")
     "c" '(claude-code-approval-clear-session-rules :which-key "clear session rules")
     "l" '(claude-code-approval-show-session-rules :which-key "show session rules"))))

(provide 'claude-code-core)
;;; claude-code-core.el ends here
