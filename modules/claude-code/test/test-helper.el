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
(require 'claude-code-process)
(require 'claude-code-buffer)
(require 'claude-code-core)

;; ============================================================================
;; Test Configuration
;; ============================================================================

(defvar claude-code-test-project-root "/tmp/claude-code-test/"
  "Temporary project root for testing.")

(defvar claude-code-test-debug nil
  "Enable debug output during tests.")

;; ============================================================================
;; Cleanup Utilities
;; ============================================================================

(defun claude-code-test-cleanup ()
  "Clean up all test resources."
  ;; Kill all processes
  (claude-code-process-kill-all)

  ;; Kill all claude-code buffers
  (dolist (buf (buffer-list))
    (when (string-match-p "\\*claude-code" (buffer-name buf))
      (kill-buffer buf)))

  ;; Clear the process hash table
  (clrhash claude-code-processes))

;; ============================================================================
;; Mock Process Creation
;; ============================================================================

(defvar claude-code-test-mock-process nil
  "Mock process object for testing.")

(defvar claude-code-test-mock-process-buffer nil
  "Buffer for mock process.")

(defun claude-code-test-create-mock-process ()
  "Create a mock process for testing."
  (setq claude-code-test-mock-process-buffer
        (generate-new-buffer " *mock-claude-process*"))
  (setq claude-code-test-mock-process
        (make-process
         :name "mock-claude"
         :buffer claude-code-test-mock-process-buffer
         :command '("cat")  ; Dummy command that does nothing
         :noquery t))
  claude-code-test-mock-process)

(defun claude-code-test-kill-mock-process ()
  "Kill the mock process and its buffer."
  (when (and claude-code-test-mock-process
             (process-live-p claude-code-test-mock-process))
    (kill-process claude-code-test-mock-process))
  (when (and claude-code-test-mock-process-buffer
             (buffer-live-p claude-code-test-mock-process-buffer))
    (kill-buffer claude-code-test-mock-process-buffer))
  (setq claude-code-test-mock-process nil
        claude-code-test-mock-process-buffer nil))

;; ============================================================================
;; Mock JSON Events
;; ============================================================================

(defvar claude-code-test-system-event
  '((type . "system")
    (subtype . "init")
    (session_id . "test-session-123")
    (model . "claude-sonnet-4-5-20250929"))
  "Mock system initialization event.")

(defvar claude-code-test-assistant-event-text
  '((type . "assistant")
    (message
     ((model . "claude-sonnet-4-5-20250929")
      (id . "msg_test123")
      (content . [((type . "text")
                   (text . "This is a test response from Claude."))]))))
  "Mock assistant event with text content.")

(defvar claude-code-test-assistant-event-tool
  '((type . "assistant")
    (message
     ((model . "claude-sonnet-4-5-20250929")
      (id . "msg_test456")
      (content . [((type . "tool_use")
                   (id . "toolu_test")
                   (name . "Read")
                   (input . ((file_path . "/test/file.el"))))]))))
  "Mock assistant event with tool use.")

(defvar claude-code-test-result-event
  '((type . "result")
    (subtype . "success")
    (is_error . :json-false)
    (duration_ms . 1234)
    (usage ((input_tokens . 10)
            (output_tokens . 50)))
    (session_id . "test-session-123"))
  "Mock result event.")

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defun claude-code-test-json-string (event)
  "Convert EVENT to a JSON string."
  (json-encode event))

(defun claude-code-test-simulate-process-output (proc-obj output)
  "Simulate PROCESS receiving OUTPUT.
PROC-OBJ is the claude-code-process object."
  (let ((process (claude-code-process-process proc-obj)))
    (when (process-live-p process)
      ;; Call the filter function directly
      (funcall #'claude-code-process--filter process output))))

(defun claude-code-test-wait-for (predicate &optional timeout)
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

(defun claude-code-test-spy-on-make-process ()
  "Spy on make-process and return a mock process."
  (spy-on 'make-process :and-return-value (claude-code-test-create-mock-process)))

;; ============================================================================
;; Buffer Helpers
;; ============================================================================

(defun claude-code-test-buffer-contents (buffer)
  "Get the contents of BUFFER as a string."
  (with-current-buffer buffer
    (buffer-string)))

(defun claude-code-test-buffer-contains-p (buffer regexp)
  "Return t if BUFFER contains text matching REGEXP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regexp nil t))))

;; ============================================================================
;; Fixtures
;; ============================================================================

(defvar claude-code-test-fixtures-dir
  (expand-file-name "fixtures" (file-name-directory load-file-name))
  "Directory containing test fixtures.")

(defun claude-code-test-load-fixture (filename)
  "Load a test fixture file named FILENAME."
  (let ((fixture-path (expand-file-name filename claude-code-test-fixtures-dir)))
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
                            (if (claude-code-test-buffer-contains-p buf rx)
                                t
                              (cons nil (format "Expected buffer to contain text matching '%s', but it did not.\nBuffer contents:\n%s"
                                                rx
                                                (claude-code-test-buffer-contents buf))))))

;; ============================================================================
;; Setup and Teardown
;; ============================================================================

(defun claude-code-test-setup ()
  "Set up test environment."
  (when claude-code-test-debug
    (message "Setting up test environment...")))

(defun claude-code-test-teardown ()
  "Tear down test environment."
  (when claude-code-test-debug
    (message "Tearing down test environment..."))
  (claude-code-test-cleanup)
  (claude-code-test-kill-mock-process))

(provide 'test-helper)
;;; test-helper.el ends here
