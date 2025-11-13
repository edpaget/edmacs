;;; claude-repl-tool-output-test.el --- Tests for tool output formatting -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for claude-repl-tool-output.el

;;; Code:

(require 'claude-repl-tool-output)
(require 'buttercup)

(describe "Tool output formatting"
  (describe "claude-repl-tool-output-format-read"
    (it "formats Read tool output with file path and content"
      (let* ((tool-input '((file_path . "test.el")))
             (tool-output ";;; test.el\n(defun foo ())\n")
             (formatted (claude-repl-tool-output-format-read tool-input tool-output)))
        (expect formatted :to-match "Read: test.el")
        (expect formatted :to-match "defun foo")))

    (it "handles Read output as alist with content key"
      (let* ((tool-input '((file_path . "test.el")))
             (tool-output '((content . ";;; test content")))
             (formatted (claude-repl-tool-output-format-read tool-input tool-output)))
        (expect formatted :to-match "Read: test.el")
        (expect formatted :to-match "test content")))

    (it "shows metadata when enabled"
      (let* ((claude-repl-tool-output-show-metadata t)
             (tool-input '((file_path . "test.el")))
             (tool-output "line 1\nline 2\n")
             (formatted (claude-repl-tool-output-format-read tool-input tool-output)))
        (expect formatted :to-match "Lines:")
        (expect formatted :to-match "Size:")))

    (it "truncates long outputs"
      (let* ((claude-repl-tool-output-max-lines 5)
             (long-content (string-join (make-list 20 "line") "\n"))
             (tool-input '((file_path . "test.txt")))
             (formatted (claude-repl-tool-output-format-read tool-input long-content)))
        (expect formatted :to-match "truncated")))

    (it "filters out system reminders"
      (let* ((tool-input '((file_path . "test.el")))
             (tool-output "before\n<system-reminder>This is a reminder</system-reminder>\nafter")
             (formatted (claude-repl-tool-output-format-read tool-input tool-output)))
        (expect formatted :not :to-match "system-reminder")
        (expect formatted :not :to-match "This is a reminder")
        (expect formatted :to-match "before")
        (expect formatted :to-match "after")))

    (it "filters multiple system reminders"
      (let* ((tool-input '((file_path . "test.el")))
             (tool-output "line1\n<system-reminder>rem1</system-reminder>\nline2\n<system-reminder>rem2</system-reminder>\nline3")
             (formatted (claude-repl-tool-output-format-read tool-input tool-output)))
        (expect formatted :not :to-match "rem1")
        (expect formatted :not :to-match "rem2")
        (expect formatted :to-match "line1")
        (expect formatted :to-match "line2")
        (expect formatted :to-match "line3"))))

  (describe "claude-repl-tool-output-format"
    (it "uses registered formatter for Read tool"
      (let* ((tool-input '((file_path . "test.el")))
             (tool-output "content")
             (formatted (claude-repl-tool-output-format "Read" tool-input tool-output)))
        (expect formatted :to-match "Read: test.el")))

    (it "falls back to generic formatter for unknown tools"
      (let* ((tool-input '((param . "value")))
             (tool-output "output")
             (formatted (claude-repl-tool-output-format "UnknownTool" tool-input tool-output)))
        (expect formatted :to-match "UnknownTool")))

    (it "handles errors during formatting gracefully"
      (let* ((formatted (claude-repl-tool-output-format "Read" nil nil)))
        (expect formatted :to-match "Error formatting"))))

  (describe "Tool output helpers"
    (describe "claude-repl-tool-output--filter-system-reminders"
      (it "removes system reminder blocks"
        (let* ((content "before\n<system-reminder>reminder text</system-reminder>\nafter")
               (filtered (claude-repl-tool-output--filter-system-reminders content)))
          (expect filtered :not :to-match "system-reminder")
          (expect filtered :not :to-match "reminder text")
          (expect filtered :to-match "before")
          (expect filtered :to-match "after")))

      (it "removes multiple system reminder blocks"
        (let* ((content "<system-reminder>a</system-reminder>text<system-reminder>b</system-reminder>")
               (filtered (claude-repl-tool-output--filter-system-reminders content)))
          (expect filtered :not :to-match "system-reminder")
          (expect filtered :to-match "text")))

      (it "handles content with no system reminders"
        (let* ((content "just normal content")
               (filtered (claude-repl-tool-output--filter-system-reminders content)))
          (expect filtered :to-equal content)))

      (it "handles nil content"
        (expect (claude-repl-tool-output--filter-system-reminders nil) :to-be nil))

      (it "removes multiline system reminder blocks"
        (let* ((content "before\n<system-reminder>\nThis is a multiline\nreminder text\n</system-reminder>\nafter")
               (filtered (claude-repl-tool-output--filter-system-reminders content)))
          (expect filtered :not :to-match "system-reminder")
          (expect filtered :not :to-match "reminder text")
          (expect filtered :to-match "before")
          (expect filtered :to-match "after"))))

    (describe "claude-repl-tool-output--truncate-content"
      (it "returns content unchanged when under limit"
        (let* ((content "line 1\nline 2")
               (result (claude-repl-tool-output--truncate-content content 10)))
          (expect (car result) :to-equal content)
          (expect (cdr result) :to-be nil)))

      (it "truncates content when over limit"
        (let* ((content "line 1\nline 2\nline 3\nline 4")
               (result (claude-repl-tool-output--truncate-content content 2)))
          (expect (car result) :to-match "line 1")
          (expect (car result) :to-match "line 2")
          (expect (car result) :to-match "truncated")
          (expect (cdr result) :to-be t)))

      (it "returns content unchanged when max-lines is nil"
        (let* ((content "line 1\nline 2\nline 3")
               (result (claude-repl-tool-output--truncate-content content nil)))
          (expect (car result) :to-equal content)
          (expect (cdr result) :to-be nil))))

    (describe "claude-repl-tool-output--icon"
      (it "returns correct icon for known tools"
        (expect (claude-repl-tool-output--icon "Read") :to-equal "🔍")
        (expect (claude-repl-tool-output--icon "Edit") :to-equal "✏️")
        (expect (claude-repl-tool-output--icon "Bash") :to-equal "⚡"))

      (it "returns default icon for unknown tools"
        (expect (claude-repl-tool-output--icon "Unknown") :to-equal "🔧")))))

(provide 'claude-repl-tool-output-test)
;;; claude-repl-tool-output-test.el ends here
