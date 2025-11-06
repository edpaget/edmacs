;;; test-helper.el --- Test helpers for Claude Code -*- lexical-binding: t -*-

;;; Commentary:
;; Shared test utilities, fixtures, and setup for Claude Code test suite.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Add source directory to load path
(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (project-dir (file-name-directory (directory-file-name (file-name-directory current-file)))))
  (add-to-list 'load-path project-dir))

;; Load the modules
(require 'claude-repl-approval)
(require 'claude-repl-process)
(require 'claude-repl-buffer)
(require 'claude-repl-core)

;; ============================================================================
;; Test Configuration
;; ============================================================================

(defvar claude-repl-test-project-root "/tmp/claude-repl-test/"
  "Temporary project root for testing.")

(defvar claude-repl-test-debug nil
  "Enable debug output during tests.")

;; ============================================================================
;; Cleanup Utilities
;; ============================================================================

(defun claude-repl-test-cleanup ()
  "Clean up all test resources."
  ;; Kill all processes (this also stops approval servers)
  (claude-repl-process-kill-all)

  ;; Kill all claude-repl buffers
  (dolist (buf (buffer-list))
    (when (string-match-p "\\*claude-repl\\|\\*Claude Code Approval\\*" (buffer-name buf))
      (kill-buffer buf)))

  ;; Clear the process hash table
  (clrhash claude-repl-processes)

  ;; Clear approval-related hash tables
  (clrhash claude-repl-approval-servers)
  (clrhash claude-repl-approval-session-rules)
  (clrhash claude-repl-approval--active-processes)
  (clrhash claude-repl-approval--responded-processes))

;; ============================================================================
;; Mock Process Creation
;; ============================================================================

(defvar claude-repl-test-mock-process nil
  "Mock process object for testing.")

(defvar claude-repl-test-mock-process-buffer nil
  "Buffer for mock process.")

(defun claude-repl-test-create-mock-process ()
  "Create a mock process for testing."
  (setq claude-repl-test-mock-process-buffer
        (generate-new-buffer " *mock-claude-process*"))
  (setq claude-repl-test-mock-process
        (make-process
         :name "mock-claude"
         :buffer claude-repl-test-mock-process-buffer
         :command '("cat")  ; Dummy command that does nothing
         :noquery t))
  claude-repl-test-mock-process)

(defun claude-repl-test-kill-mock-process ()
  "Kill the mock process and its buffer."
  (when (and claude-repl-test-mock-process
             (process-live-p claude-repl-test-mock-process))
    (kill-process claude-repl-test-mock-process))
  (when (and claude-repl-test-mock-process-buffer
             (buffer-live-p claude-repl-test-mock-process-buffer))
    (kill-buffer claude-repl-test-mock-process-buffer))
  (setq claude-repl-test-mock-process nil
        claude-repl-test-mock-process-buffer nil))

;; ============================================================================
;; Mock JSON Events
;; ============================================================================

(defvar claude-repl-test-system-event
  '((type . "system")
    (subtype . "init")
    (session_id . "test-session-123")
    (model . "claude-sonnet-4-5-20250929"))
  "Mock system initialization event.")

(defvar claude-repl-test-assistant-event-text
  '((type . "assistant")
    (message . ((model . "claude-sonnet-4-5-20250929")
                (id . "msg_test123")
                (content . [((type . "text")
                             (text . "This is a test response from Claude."))]))))
  "Mock assistant event with text content.")

(defvar claude-repl-test-assistant-event-tool
  '((type . "assistant")
    (message . ((model . "claude-sonnet-4-5-20250929")
                (id . "msg_test456")
                (content . [((type . "tool_use")
                             (id . "toolu_test")
                             (name . "Read")
                             (input . ((file_path . "/test/file.el"))))]))))
  "Mock assistant event with tool use.")

(defvar claude-repl-test-result-event
  '((type . "result")
    (subtype . "success")
    (is_error . :json-false)
    (duration_ms . 1234)
    (usage . ((input_tokens . 10)
              (output_tokens . 50)))
    (session_id . "test-session-123"))
  "Mock result event.")

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defun claude-repl-test-json-string (event)
  "Convert EVENT to a JSON string."
  (json-encode event))

(defun claude-repl-test-simulate-process-output (proc-obj output)
  "Simulate PROCESS receiving OUTPUT.
PROC-OBJ is the claude-repl-process object."
  (let ((process (claude-repl-process-process proc-obj)))
    (when (process-live-p process)
      ;; Call the filter function directly
      (funcall #'claude-repl-process--filter process output))))

(defun claude-repl-test-wait-for (predicate &optional timeout)
  "Wait for PREDICATE to return non-nil, with optional TIMEOUT in seconds."
  (let ((start (current-time))
        (timeout (or timeout 5)))
    (while (and (not (funcall predicate))
                (< (float-time (time-subtract (current-time) start)) timeout))
      (sleep-for 0.01))
    (funcall predicate)))

;; ============================================================================
;; Spy Helpers
;; ============================================================================

(defun claude-repl-test-spy-on-make-process ()
  "Spy on make-process and return a mock process."
  (spy-on 'make-process :and-return-value (claude-repl-test-create-mock-process)))

;; ============================================================================
;; Buffer Helpers
;; ============================================================================

(defun claude-repl-test-buffer-contents (buffer)
  "Get the contents of BUFFER as a string."
  (with-current-buffer buffer
    (buffer-string)))

(defun claude-repl-test-buffer-contains-p (buffer regexp)
  "Return t if BUFFER contains text matching REGEXP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regexp nil t))))

;; ============================================================================
;; Fixtures
;; ============================================================================

(defvar claude-repl-test-fixtures-dir
  (expand-file-name "fixtures" (file-name-directory load-file-name))
  "Directory containing test fixtures.")

(defun claude-repl-test-load-fixture (filename)
  "Load a test fixture file named FILENAME."
  (let ((fixture-path (expand-file-name filename claude-repl-test-fixtures-dir)))
    (when (file-exists-p fixture-path)
      (with-temp-buffer
        (insert-file-contents fixture-path)
        (buffer-string)))))

;; ============================================================================
;; Matchers
;; ============================================================================

(buttercup-define-matcher :to-have-function (symbol)
                          (if (fboundp (funcall symbol))
                              t
                            (cons nil (format "Expected %s to be a function, but it was not defined"
                                              (funcall symbol)))))

(buttercup-define-matcher :to-be-live-process (process)
                          (let ((proc (funcall process)))
                            (if (and proc (process-live-p proc))
                                t
                              (cons nil (format "Expected process to be alive, but it was %s"
                                                (if proc "dead" "nil"))))))

(buttercup-define-matcher :to-match-buffer (buffer regexp)
                          (let ((buf (funcall buffer))
                                (rx (funcall regexp)))
                            (if (claude-repl-test-buffer-contains-p buf rx)
                                t
                              (cons nil (format "Expected buffer to contain text matching '%s', but it did not.\nBuffer contents:\n%s"
                                                rx
                                                (claude-repl-test-buffer-contents buf))))))

;; ============================================================================
;; Setup and Teardown
;; ============================================================================

(defun claude-repl-test-setup ()
  "Set up test environment."
  (when claude-repl-test-debug
    (message "Setting up test environment...")))

(defun claude-repl-test-teardown ()
  "Tear down test environment."
  (when claude-repl-test-debug
    (message "Tearing down test environment..."))
  (claude-repl-test-cleanup)
  (claude-repl-test-kill-mock-process))

(provide 'test-helper)
;;; test-helper.el ends here
