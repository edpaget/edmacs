;;; claude-code-core.el --- Core Claude Code REPL integration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Claude Code Contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.0") (markdown-mode "2.7") (projectile "2.9.1"))
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
  "Ask Claude a question with PROMPT using the beautiful response buffer."
  (interactive "sAsk Claude: ")
  (let* ((project-root (claude-code-process--get-project-root))
         (proc-obj (claude-code-process-start project-root))
         (response-buffer (claude-code-buffer-get-or-create project-root)))

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
          ;; Assistant message - contains the actual response
          ((equal event-type "assistant")
           (claude-code-buffer-handle-assistant-event response-buffer event))

          ;; Result - final summary
          ((equal event-type "result")
           (claude-code-buffer-handle-result-event response-buffer event)
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
     "t" '(claude-code-test-prompt :which-key "test prompt (debug)"))))

(provide 'claude-code-core)
;;; claude-code-core.el ends here
