;;; claude-repl-buffer-autoscroll-test.el --- Tests for auto-scroll -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for auto-scroll functionality in claude-repl-buffer.el

;;; Code:

(require 'buttercup)
(require 'test-helper)

(describe "Auto-scroll feature"

  (after-each
    (claude-repl-test-teardown))

  (describe "Auto-scroll configuration"

    (it "initializes auto-scroll enabled from customization"
      (let ((claude-repl-auto-scroll t))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (with-current-buffer buffer
            (expect claude-repl-buffer-auto-scroll-enabled :to-be t)))))

    (it "initializes auto-scroll disabled from customization"
      (let ((claude-repl-auto-scroll nil))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (with-current-buffer buffer
            (expect claude-repl-buffer-auto-scroll-enabled :to-be nil))))))

  (describe "At-end-of-buffer detection"

    (it "detects point at max position as end"
      (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (goto-char (point-max))
          (expect (claude-repl-buffer--at-end-of-buffer-p) :to-be t))))

    (it "detects point in input area as end"
      (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (when claude-repl-buffer-input-start-marker
            (goto-char claude-repl-buffer-input-start-marker)
            (expect (claude-repl-buffer--at-end-of-buffer-p) :to-be t)))))

    (it "detects point at current interaction end as end"
      (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
        (claude-repl-buffer-start-interaction buffer "Test")
        (claude-repl-buffer-append-text buffer "Some text")
        (with-current-buffer buffer
          (when claude-repl-buffer-current-interaction
            (goto-char (claude-repl-interaction-end-marker
                       claude-repl-buffer-current-interaction))
            (expect (claude-repl-buffer--at-end-of-buffer-p) :to-be t)))))

    (it "detects point in middle of buffer as not at end"
      (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
        (claude-repl-buffer-start-interaction buffer "Test prompt")
        (claude-repl-buffer-append-text buffer "Response text")
        (with-current-buffer buffer
          (goto-char (point-min))
          (expect (claude-repl-buffer--at-end-of-buffer-p) :to-be nil)))))

  (describe "Auto-scroll re-enabling"

    (it "re-enables auto-scroll when starting new interaction"
      (let ((claude-repl-auto-scroll t))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (with-current-buffer buffer
            (setq-local claude-repl-buffer-auto-scroll-enabled nil))
          (claude-repl-buffer-start-interaction buffer "Test")
          (with-current-buffer buffer
            (expect claude-repl-buffer-auto-scroll-enabled :to-be t))))))

  (describe "Cursor movement detection"

    (it "disables auto-scroll when moving cursor away from end"
      (let ((claude-repl-auto-scroll t))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (claude-repl-buffer-start-interaction buffer "Test prompt")
          (claude-repl-buffer-append-text buffer "Some response text")
          (with-current-buffer buffer
            (setq-local claude-repl-buffer-auto-scroll-enabled t)
            (goto-char (point-min))
            (setq this-command 'previous-line)
            (claude-repl-buffer--handle-cursor-movement)
            (expect claude-repl-buffer-auto-scroll-enabled :to-be nil)))))

    (it "keeps auto-scroll enabled when cursor stays at end"
      (let ((claude-repl-auto-scroll t))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (claude-repl-buffer-start-interaction buffer "Test")
          (with-current-buffer buffer
            (goto-char (point-max))
            (setq this-command 'end-of-buffer)
            (claude-repl-buffer--handle-cursor-movement)
            (expect claude-repl-buffer-auto-scroll-enabled :to-be t)))))

    (it "ignores non-movement commands"
      (let ((claude-repl-auto-scroll t))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (with-current-buffer buffer
            (setq-local claude-repl-buffer-auto-scroll-enabled t)
            (goto-char (point-min))
            (setq this-command 'self-insert-command)
            (claude-repl-buffer--handle-cursor-movement)
            (expect claude-repl-buffer-auto-scroll-enabled :to-be t)))))

    (it "detects evil-mode movement commands"
      (let ((claude-repl-auto-scroll t))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (claude-repl-buffer-start-interaction buffer "Test prompt")
          (with-current-buffer buffer
            (setq-local claude-repl-buffer-auto-scroll-enabled t)
            (goto-char (point-min))
            (setq this-command 'evil-previous-line)
            (claude-repl-buffer--handle-cursor-movement)
            (expect claude-repl-buffer-auto-scroll-enabled :to-be nil))))))

  (describe "Auto-scroll during streaming"

    (it "calls auto-scroll when appending text"
      (let ((claude-repl-auto-scroll t))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (claude-repl-buffer-start-interaction buffer "Test")
          (with-current-buffer buffer
            (setq-local claude-repl-buffer-auto-scroll-enabled t)
            (spy-on 'claude-repl-buffer--maybe-auto-scroll :and-call-through)
            (claude-repl-buffer-append-text buffer "New text")
            (expect 'claude-repl-buffer--maybe-auto-scroll :to-have-been-called)))))

    (it "does not scroll when auto-scroll disabled"
      (let ((claude-repl-auto-scroll nil))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (claude-repl-buffer-start-interaction buffer "Test prompt with content")
          (with-current-buffer buffer
            (setq-local claude-repl-buffer-auto-scroll-enabled nil)
            (let ((orig-point (goto-char (point-min))))
              (claude-repl-buffer-append-text buffer "New text")
              (expect (point) :to-equal orig-point)))))))

  (describe "Auto-scroll interaction lifecycle"

    (it "enables auto-scroll when new interaction starts"
      (let ((claude-repl-auto-scroll t))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (claude-repl-buffer-start-interaction buffer "First")
          (with-current-buffer buffer
            (setq-local claude-repl-buffer-auto-scroll-enabled nil))
          (claude-repl-buffer-complete-interaction buffer)
          (claude-repl-buffer-start-interaction buffer "Second")
          (with-current-buffer buffer
            (expect claude-repl-buffer-auto-scroll-enabled :to-be t)))))

    (it "preserves auto-scroll state across text appends"
      (let ((claude-repl-auto-scroll t))
        (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
          (claude-repl-buffer-start-interaction buffer "Test")
          (with-current-buffer buffer
            (setq-local claude-repl-buffer-auto-scroll-enabled t))
          (claude-repl-buffer-append-text buffer "First ")
          (claude-repl-buffer-append-text buffer "second ")
          (claude-repl-buffer-append-text buffer "third")
          (with-current-buffer buffer
            (expect claude-repl-buffer-auto-scroll-enabled :to-be t))))))

  (describe "Edge cases"

    (it "handles auto-scroll when buffer is empty"
      (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (setq-local claude-repl-buffer-auto-scroll-enabled t)
          (expect (claude-repl-buffer--at-end-of-buffer-p) :to-be-truthy))))

    (it "handles auto-scroll when no current interaction"
      (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (setq-local claude-repl-buffer-current-interaction nil)
          (setq-local claude-repl-buffer-auto-scroll-enabled t)
          (expect (claude-repl-buffer--at-end-of-buffer-p) :to-be-truthy))))

    (it "handles auto-scroll when input marker is nil"
      (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (setq-local claude-repl-buffer-input-start-marker nil)
          (setq-local claude-repl-buffer-auto-scroll-enabled t)
          (goto-char (point-max))
          (expect (claude-repl-buffer--at-end-of-buffer-p) :to-be-truthy))))

    (it "handles cursor movement hook when auto-scroll already disabled"
      (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (setq-local claude-repl-buffer-auto-scroll-enabled nil)
          (goto-char (point-min))
          (setq this-command 'previous-line)
          (claude-repl-buffer--handle-cursor-movement)
          (expect claude-repl-buffer-auto-scroll-enabled :to-be nil))))))

(provide 'claude-repl-buffer-autoscroll-test)
;;; claude-repl-buffer-autoscroll-test.el ends here
