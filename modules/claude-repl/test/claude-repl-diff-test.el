;;; claude-repl-diff-test.el --- Tests for claude-repl-diff -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:
;; Unit tests for the diff generation and preview functionality

;;; Code:

(require 'claude-repl-diff)
(require 'buttercup)

(describe "claude-repl-diff"

  (describe "claude-repl-diff--find-line-number"
    (it "finds line number for single-line string at beginning of file"
      (let ((file-content "line 1\nline 2\nline 3\n")
            (search-string "line 1"))
        (expect (claude-repl-diff--find-line-number file-content search-string)
                :to-equal 1)))

    (it "finds line number for single-line string in middle of file"
      (let ((file-content "line 1\nline 2\nline 3\n")
            (search-string "line 2"))
        (expect (claude-repl-diff--find-line-number file-content search-string)
                :to-equal 2)))

    (it "finds line number for multi-line string"
      (let ((file-content "line 1\nline 2\nline 3\nline 4\n")
            (search-string "line 2\nline 3"))
        (expect (claude-repl-diff--find-line-number file-content search-string)
                :to-equal 2)))

    (it "returns nil when string not found"
      (let ((file-content "line 1\nline 2\nline 3\n")
            (search-string "line 99"))
        (expect (claude-repl-diff--find-line-number file-content search-string)
                :to-be nil)))

    (it "returns nil for empty file content"
      (expect (claude-repl-diff--find-line-number "" "test")
              :to-be nil))

    (it "returns nil for nil file content"
      (expect (claude-repl-diff--find-line-number nil "test")
              :to-be nil))

    (it "handles exact match with trailing newlines"
      (let ((file-content "Q: Why?\nA: Because!\n\nQ: When?\n")
            (search-string "Q: Why?\nA: Because!"))
        (expect (claude-repl-diff--find-line-number file-content search-string)
                :to-equal 1))))

  (describe "claude-repl-diff--derive-mode-from-file"
    (it "derives emacs-lisp-mode for .el files"
      (expect (claude-repl-diff--derive-mode-from-file "test.el")
              :to-equal 'emacs-lisp-mode))

    (it "derives python-mode for .py files"
      (expect (claude-repl-diff--derive-mode-from-file "test.py")
              :to-equal 'python-mode))

    (it "derives text-mode for .txt files"
      (expect (claude-repl-diff--derive-mode-from-file "test.txt")
              :to-equal 'text-mode))

    (it "returns nil for unknown extensions"
      (expect (claude-repl-diff--derive-mode-from-file "test.unknown123456")
              :to-be nil))

    (it "returns nil for nil path"
      (expect (claude-repl-diff--derive-mode-from-file nil)
              :to-be nil)))

  (describe "claude-repl-diff-for-edit"
    (it "returns nil when old_string equals new_string"
      (let ((input '((file_path . "/tmp/test.txt")
                     (old_string . "unchanged")
                     (new_string . "unchanged"))))
        (expect (claude-repl-diff-for-edit input)
                :to-be nil)))

    (it "generates diff buffer for different strings"
      (let ((input '((file_path . "/tmp/test.txt")
                     (old_string . "old text")
                     (new_string . "new text"))))
        (let ((diff-buf (claude-repl-diff-for-edit input)))
          (expect diff-buf :not :to-be nil)
          (expect (buffer-live-p diff-buf) :to-be t)
          (with-current-buffer diff-buf
            (expect (buffer-string) :to-match "\\-old text")
            (expect (buffer-string) :to-match "\\+new text"))
          (kill-buffer diff-buf))))

    (it "handles nil old_string as empty string"
      (let ((input '((file_path . "/tmp/test.txt")
                     (old_string . nil)
                     (new_string . "new text"))))
        (let ((diff-buf (claude-repl-diff-for-edit input)))
          (expect diff-buf :not :to-be nil)
          (expect (buffer-live-p diff-buf) :to-be t)
          (kill-buffer diff-buf))))

    (it "replaces temp buffer names with actual file path in diff headers"
      (let ((input '((file_path . "/tmp/myfile.txt")
                     (old_string . "old")
                     (new_string . "new"))))
        (let ((diff-buf (claude-repl-diff-for-edit input)))
          (expect diff-buf :not :to-be nil)
          (with-current-buffer diff-buf
            (expect (buffer-string) :to-match "--- /tmp/myfile.txt")
            (expect (buffer-string) :to-match "\\+\\+\\+ /tmp/myfile.txt"))
          (kill-buffer diff-buf))))

    (it "includes line number information when file exists"
      (let* ((temp-file (make-temp-file "claude-test-" nil ".txt"))
             (content "line 1\nline 2\nline 3\n"))
        (with-temp-file temp-file
          (insert content))
        (let ((input `((file_path . ,temp-file)
                       (old_string . "line 2")
                       (new_string . "modified line 2"))))
          (let ((diff-buf (claude-repl-diff-for-edit input)))
            (expect diff-buf :not :to-be nil)
            (with-current-buffer diff-buf
              (expect (buffer-string) :to-match ";; Lines 2-2"))
            (kill-buffer diff-buf)))
        (delete-file temp-file))))

  (describe "claude-repl-diff-for-write"
    (it "generates preview buffer for Write tool"
      (let ((input '((file_path . "/tmp/newfile.txt")
                     (content . "Hello, World!\nThis is a test.\n"))))
        (let ((preview-buf (claude-repl-diff-for-write input)))
          (expect preview-buf :not :to-be nil)
          (expect (buffer-live-p preview-buf) :to-be t)
          (with-current-buffer preview-buf
            (expect (buffer-string) :to-match "File: /tmp/newfile.txt")
            (expect (buffer-string) :to-match "Total lines:")
            (expect (buffer-string) :to-match "Hello, World!")
            (expect (buffer-string) :to-match "This is a test."))
          (kill-buffer preview-buf))))

    (it "handles nil content as empty string"
      (let ((input '((file_path . "/tmp/empty.txt")
                     (content . nil))))
        (let ((preview-buf (claude-repl-diff-for-write input)))
          (expect preview-buf :not :to-be nil)
          (expect (buffer-live-p preview-buf) :to-be t)
          (kill-buffer preview-buf))))

    (it "truncates long content when max-preview-lines is set"
      (let ((claude-repl-diff-max-preview-lines 5)
            (long-content (mapconcat #'number-to-string (number-sequence 1 20) "\n")))
        (let ((input `((file_path . "/tmp/long.txt")
                       (content . ,long-content))))
          (let ((preview-buf (claude-repl-diff-for-write input)))
            (expect preview-buf :not :to-be nil)
            (with-current-buffer preview-buf
              (expect (buffer-string) :to-match "truncated"))
            (kill-buffer preview-buf)))))

    (it "does not truncate when max-preview-lines is nil"
      (let ((claude-repl-diff-max-preview-lines nil)
            (content (mapconcat #'number-to-string (number-sequence 1 20) "\n")))
        (let ((input `((file_path . "/tmp/notlong.txt")
                       (content . ,content))))
          (let ((preview-buf (claude-repl-diff-for-write input)))
            (expect preview-buf :not :to-be nil)
            (with-current-buffer preview-buf
              (expect (buffer-string) :not :to-match "truncated"))
            (kill-buffer preview-buf))))))

  (describe "claude-repl-diff-get-statistics"
    (it "returns correct added and removed counts"
      (let ((input '((file_path . "/tmp/test.txt")
                     (old_string . "line 1\nline 2\nline 3")
                     (new_string . "line 1\nmodified line 2\nline 3\nline 4"))))
        (let ((diff-buf (claude-repl-diff-for-edit input)))
          (when diff-buf
            (let ((stats (claude-repl-diff-get-statistics diff-buf)))
              (expect (plist-get stats :added) :to-be-greater-than 0)
              (expect (plist-get stats :removed) :to-be-greater-than 0))
            (kill-buffer diff-buf)))))

    (it "returns zero counts for nil buffer"
      (let ((stats (claude-repl-diff-get-statistics nil)))
        (expect (plist-get stats :added) :to-equal 0)
        (expect (plist-get stats :removed) :to-equal 0)))

    (it "returns zero counts for killed buffer"
      (let ((buf (generate-new-buffer "*test*")))
        (kill-buffer buf)
        (let ((stats (claude-repl-diff-get-statistics buf)))
          (expect (plist-get stats :added) :to-equal 0)
          (expect (plist-get stats :removed) :to-equal 0))))))

(provide 'claude-repl-diff-test)
;;; claude-repl-diff-test.el ends here
