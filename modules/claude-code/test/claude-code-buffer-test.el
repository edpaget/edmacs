;;; claude-code-buffer-test.el --- Tests for buffer UI -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for claude-code-buffer.el

;;; Code:

(require 'buttercup)
(require 'test-helper)

(describe "claude-code-buffer"

  (describe "Module loading"
    (it "defines all expected functions"
      (expect 'claude-code-buffer-get-or-create :to-have-function)
      (expect 'claude-code-buffer-start-interaction :to-have-function)
      (expect 'claude-code-buffer-append-text :to-have-function)
      (expect 'claude-code-buffer-add-tool-use :to-have-function)
      (expect 'claude-code-buffer-complete-interaction :to-have-function)
      (expect 'claude-code-buffer-handle-assistant-event :to-have-function)
      (expect 'claude-code-buffer-handle-result-event :to-have-function)
      (expect 'claude-code-buffer-clear :to-have-function)))

  (describe "Buffer creation"

    (after-each
      (claude-code-test-teardown))

    (it "creates buffer with correct name"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test-project/")))
        (expect (buffer-name buffer) :to-equal "*claude-code: test-project*")))

    (it "reuses existing buffer"
      (let ((buf1 (claude-code-buffer-get-or-create "/tmp/test/"))
            (buf2 (claude-code-buffer-get-or-create "/tmp/test/")))
        (expect buf1 :to-be buf2)))

    (it "creates unique buffers for different projects"
      (let ((buf1 (claude-code-buffer-get-or-create "/tmp/project1/"))
            (buf2 (claude-code-buffer-get-or-create "/tmp/project2/")))
        (expect buf1 :not :to-be buf2)))

    (it "sets correct major mode"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (expect major-mode :to-be 'claude-code-buffer-mode))))

    (it "sets buffer as read-only"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (expect buffer-read-only :to-be t))))

    (it "stores project root in buffer-local variable"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (expect claude-code-buffer-project-root :to-equal "/tmp/test/")))))

  (describe "Interaction management"

    (after-each
      (claude-code-test-teardown))

    (it "starts interaction with prompt"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test prompt")

        (expect buffer :to-match-buffer "Test prompt")))

    (it "adds prompt header"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test prompt")

        (expect buffer :to-match-buffer "## Prompt")))

    (it "adds response header"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test prompt")

        (expect buffer :to-match-buffer "## Response")))

    (it "appends text to current interaction"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-append-text buffer "Response text")

        (expect buffer :to-match-buffer "Response text")))

    (it "appends multiple text chunks"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-append-text buffer "First ")
        (claude-code-buffer-append-text buffer "second ")
        (claude-code-buffer-append-text buffer "third")

        (expect buffer :to-match-buffer "First second third")))

    (it "creates interaction structure"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test")

        (with-current-buffer buffer
          (expect claude-code-buffer-current-interaction :to-be-truthy)
          (expect (claude-code-interaction-p claude-code-buffer-current-interaction)
                  :to-be t))))

    (it "stores prompt in interaction"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "My prompt")

        (with-current-buffer buffer
          (expect (claude-code-interaction-prompt claude-code-buffer-current-interaction)
                  :to-equal "My prompt")))))

  (describe "Tool usage"

    (after-each
      (claude-code-test-teardown))

    (it "adds tool use to buffer"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-add-tool-use buffer "Read" '((file_path . "/test.el")))

        (expect buffer :to-match-buffer "Tool: Read")))

    (it "formats tool input as code block"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-add-tool-use buffer "Bash" '((command . "ls -la")))

        (expect buffer :to-match-buffer "```elisp")
        (expect buffer :to-match-buffer "```")))

    (it "stores tool use in interaction"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-add-tool-use buffer "Grep" '((pattern . "test")))

        (with-current-buffer buffer
          (let ((tools (claude-code-interaction-tool-uses
                        claude-code-buffer-current-interaction)))
            (expect tools :to-be-truthy)
            (expect (plist-get (car tools) :tool) :to-equal "Grep"))))))

  (describe "Interaction completion"

    (after-each
      (claude-code-test-teardown))

    (it "completes interaction"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-complete-interaction buffer)

        (with-current-buffer buffer
          (expect claude-code-buffer-current-interaction :to-be nil))))

    (it "adds metadata to buffer"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-complete-interaction
         buffer
         '((input_tokens . 10) (output_tokens . 20)))

        (expect buffer :to-match-buffer "Tokens: 10 in, 20 out")))

    (it "stores completion status"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (with-current-buffer buffer
          (let ((interaction claude-code-buffer-current-interaction))
            (claude-code-buffer-complete-interaction buffer)
            (expect (claude-code-interaction-status interaction)
                    :to-equal 'complete))))))

  (describe "Event handling"

    (after-each
      (claude-code-test-teardown))

    (it "handles assistant event with text"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test")
        (claude-code-buffer-handle-assistant-event
         buffer
         claude-code-test-assistant-event-text)

        (expect buffer :to-match-buffer "This is a test response from Claude")))

    (it "handles assistant event with tool use"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test")
        (claude-code-buffer-handle-assistant-event
         buffer
         claude-code-test-assistant-event-tool)

        (expect buffer :to-match-buffer "Tool: Read")))

    (it "handles result event"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test")
        (claude-code-buffer-handle-result-event
         buffer
         claude-code-test-result-event)

        (expect buffer :to-match-buffer "Tokens: 10 in, 50 out"))))

  (describe "Multiple interactions"

    (after-each
      (claude-code-test-teardown))

    (it "adds separator between interactions"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "First")
        (claude-code-buffer-complete-interaction buffer)

        (claude-code-buffer-start-interaction buffer "Second")

        (expect buffer :to-match-buffer "────")))

    (it "keeps both interactions in buffer"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "First prompt")
        (claude-code-buffer-append-text buffer "First response")
        (claude-code-buffer-complete-interaction buffer)

        (claude-code-buffer-start-interaction buffer "Second prompt")
        (claude-code-buffer-append-text buffer "Second response")

        (expect buffer :to-match-buffer "First prompt")
        (expect buffer :to-match-buffer "First response")
        (expect buffer :to-match-buffer "Second prompt")
        (expect buffer :to-match-buffer "Second response"))))

  (describe "Buffer clearing"

    (after-each
      (claude-code-test-teardown))

    (it "clears buffer contents"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test")
        (claude-code-buffer-append-text buffer "Some text")

        (claude-code-buffer-clear buffer)

        (with-current-buffer buffer
          (expect (buffer-string) :to-equal ""))))

    (it "resets current interaction"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test")

        (claude-code-buffer-clear buffer)

        (with-current-buffer buffer
          (expect claude-code-buffer-current-interaction :to-be nil)))))

  (describe "Buffer mode"

    (after-each
      (claude-code-test-teardown))

    (it "has correct mode name"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (expect (format-mode-line mode-name) :to-equal "Claude-Code"))))

    (it "derives from markdown-mode"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (expect (derived-mode-p 'markdown-mode) :to-be-truthy))))))

(provide 'claude-code-buffer-test)
;;; claude-code-buffer-test.el ends here
