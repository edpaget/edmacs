;;; claude-repl-diff.el --- Diff generation for Claude Code approval -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: tools, diff
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This module provides diff and preview generation for Claude Code tool approvals.
;; It supports:
;; - Unified diffs for Edit tool (old_string vs new_string)
;; - File previews for Write tool
;; - Syntax highlighting via diff-mode
;; - Fine-grained refinement
;; - Extensibility for custom tools

;;; Code:

(require 'diff-mode)
(require 'diff)

(defgroup claude-repl-diff nil
  "Diff display for Claude Code tool approvals."
  :group 'claude-repl
  :prefix "claude-repl-diff-")

(defcustom claude-repl-diff-enable-refinement t
  "Enable fine-grained character-level diff refinement."
  :type 'boolean
  :group 'claude-repl-diff)

(defcustom claude-repl-diff-enable-syntax-highlighting t
  "Enable source code syntax highlighting in diffs."
  :type 'boolean
  :group 'claude-repl-diff)

(defcustom claude-repl-diff-enable-prettify t
  "Enable diff prettification features."
  :type 'boolean
  :group 'claude-repl-diff)

(defcustom claude-repl-diff-context-lines 3
  "Number of context lines to show in diffs."
  :type 'integer
  :group 'claude-repl-diff)

(defcustom claude-repl-diff-max-preview-lines 100
  "Maximum lines to show in Write tool previews.
If nil, show entire file."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max lines"))
  :group 'claude-repl-diff)

;;; Helper Functions

(defun claude-repl-diff--find-line-number (file-content search-string)
  "Find the line number where SEARCH-STRING appears in FILE-CONTENT.
Returns the line number (1-indexed) or nil if not found."
  (when (and file-content search-string)
    (let ((lines (split-string file-content "\n"))
          (search-lines (split-string search-string "\n"))
          (line-num 1)
          (found nil))
      ;; Look for the first line of search-string in the file
      (while (and lines (not found))
        (when (string= (car lines) (car search-lines))
          ;; Found potential match, verify all lines match
          (let ((match t)
                (remaining-file lines)
                (remaining-search search-lines))
            (while (and remaining-search match)
              (unless (and remaining-file
                           (string= (car remaining-file) (car remaining-search)))
                (setq match nil))
              (setq remaining-file (cdr remaining-file)
                    remaining-search (cdr remaining-search)))
            (when match
              (setq found line-num))))
        (setq lines (cdr lines)
              line-num (1+ line-num)))
      found)))

(defun claude-repl-diff--derive-mode-from-file (file-path)
  "Derive major mode from FILE-PATH.
Returns a major mode function symbol, or nil if cannot determine."
  (when file-path
    (let ((mode (assoc-default file-path auto-mode-alist #'string-match-p)))
      (if (and mode (functionp mode))
          mode
        nil))))

(defun claude-repl-diff--create-temp-buffer (content file-path &optional buffer-name)
  "Create temporary buffer with CONTENT and appropriate major mode.
FILE-PATH is used to derive the major mode.
BUFFER-NAME is optional name for the buffer.
Returns the created buffer."
  (let* ((buf (generate-new-buffer (or buffer-name " *claude-diff-temp*")))
         (mode (claude-repl-diff--derive-mode-from-file file-path)))
    (with-current-buffer buf
      (insert content)
      (when mode
        (condition-case err
            (funcall mode)
          (error
           (message "Failed to enable mode %s: %s" mode (error-message-string err))
           (text-mode))))
      (setq-local buffer-read-only nil))
    buf))

(defun claude-repl-diff--generate-diff (old-buf new-buf)
  "Generate unified diff between OLD-BUF and NEW-BUF.
Returns diff buffer with diff-mode enabled, or nil if buffers are identical."
  (let* ((diff-switches (list "-u" (format "-U%d" claude-repl-diff-context-lines)))
         (diff-buf (condition-case err
                       (diff-no-select old-buf new-buf diff-switches t)
                     (error
                      (message "Error generating diff: %s" (error-message-string err))
                      nil))))
    (when diff-buf
      (with-current-buffer diff-buf
        (if (= (point-min) (point-max))
            (progn
              (kill-buffer diff-buf)
              nil)
          diff-buf)))))

(defun claude-repl-diff--configure-diff-buffer (diff-buffer)
  "Configure DIFF-BUFFER for optimal approval display."
  (when (buffer-live-p diff-buffer)
    (with-current-buffer diff-buffer
      (unless (derived-mode-p 'diff-mode)
        (diff-mode))
      (when claude-repl-diff-enable-refinement
        (setq-local diff-refine 'font-lock))
      (when claude-repl-diff-enable-syntax-highlighting
        (setq-local diff-font-lock-syntax t))
      (when claude-repl-diff-enable-prettify
        (setq-local diff-font-lock-prettify t))
      (setq buffer-read-only t))))

(defun claude-repl-diff--extract-line-counts (diff-buffer)
  "Count added and removed lines in DIFF-BUFFER.
Returns cons cell (ADDED . REMOVED)."
  (if (not (buffer-live-p diff-buffer))
      (cons 0 0)
    (with-current-buffer diff-buffer
      (save-excursion
        (goto-char (point-min))
        (let ((added 0)
              (removed 0))
          (while (not (eobp))
            (cond
             ((looking-at "^\\+[^+]")
              (setq added (1+ added)))
             ((looking-at "^-[^-]")
              (setq removed (1+ removed))))
            (forward-line 1))
          (cons added removed))))))

;;; Public API

(defun claude-repl-diff-for-edit (tool-input)
  "Generate unified diff for Edit tool.
TOOL-INPUT is an alist with keys: file_path, old_string, new_string.
Returns diff buffer with diff-mode enabled, or nil if no changes."
  (let* ((file-path (alist-get 'file_path tool-input))
         (old-string (or (alist-get 'old_string tool-input) ""))
         (new-string (or (alist-get 'new_string tool-input) "")))
    (if (string= old-string new-string)
        nil
      ;; Read the actual file to find line numbers
      (let* ((file-content (when (and file-path (file-exists-p file-path))
                             (with-temp-buffer
                               (insert-file-contents file-path)
                               (buffer-string))))
             (start-line (when file-content
                           (claude-repl-diff--find-line-number file-content old-string)))
             (old-buf (claude-repl-diff--create-temp-buffer
                       old-string file-path " *claude-diff-old*"))
             (new-buf (claude-repl-diff--create-temp-buffer
                       new-string file-path " *claude-diff-new*"))
             (diff-buf nil))
        (unwind-protect
            (progn
              (setq diff-buf (claude-repl-diff--generate-diff old-buf new-buf))
              (when diff-buf
                ;; Clean up the diff headers to show actual file path and line numbers
                (with-current-buffer diff-buf
                  (let ((inhibit-read-only t))
                    (goto-char (point-min))
                    ;; Replace temp buffer names with actual file path
                    (when file-path
                      (while (re-search-forward "^--- .*" nil t)
                        (replace-match (format "--- %s" file-path)))
                      (goto-char (point-min))
                      (while (re-search-forward "^\\+\\+\\+ .*" nil t)
                        (replace-match (format "+++ %s" file-path))))
                    ;; Add line number information if we found it
                    (when start-line
                      (goto-char (point-min))
                      (when (re-search-forward "^@@ " nil t)
                        (beginning-of-line)
                        (insert (format ";; Lines %d-%d in %s\n"
                                        start-line
                                        (+ start-line (length (split-string old-string "\n")) -1)
                                        (file-name-nondirectory file-path)))))))
                (claude-repl-diff--configure-diff-buffer diff-buf))
              diff-buf)
          (when (buffer-live-p old-buf)
            (kill-buffer old-buf))
          (when (buffer-live-p new-buf)
            (kill-buffer new-buf)))))))

(defun claude-repl-diff-for-write (tool-input)
  "Generate preview for Write tool.
TOOL-INPUT is an alist with keys: file_path, content.
Returns preview buffer with appropriate major mode enabled."
  (let* ((file-path (alist-get 'file_path tool-input))
         (content (or (alist-get 'content tool-input) ""))
         (preview-buf (generate-new-buffer "*claude-write-preview*")))
    (with-current-buffer preview-buf
      (let* ((lines (split-string content "\n"))
             (line-count (length lines))
             (should-truncate (and claude-repl-diff-max-preview-lines
                                   (> line-count claude-repl-diff-max-preview-lines)))
             (display-content (if should-truncate
                                  (string-join
                                   (seq-take lines claude-repl-diff-max-preview-lines)
                                   "\n")
                                content)))
        (insert display-content)
        (when should-truncate
          (insert (format "\n\n... (%d lines truncated)\n"
                          (- line-count claude-repl-diff-max-preview-lines))))
        (goto-char (point-min))
        (insert (format "File: %s\n" (or file-path "(no path)")))
        (insert (format "Total lines: %d\n\n" line-count))
        (insert "────────────────────────────────────────────────────────────\n\n")
        (let ((mode (claude-repl-diff--derive-mode-from-file file-path)))
          (when mode
            (condition-case err
                (funcall mode)
              (error
               (message "Failed to enable mode %s: %s" mode (error-message-string err))
               (text-mode)))))
        (setq buffer-read-only t)
        (when (fboundp 'font-lock-ensure)
          (font-lock-ensure))))
    preview-buf))

(defun claude-repl-diff-get-statistics (diff-buffer)
  "Extract statistics from DIFF-BUFFER.
Returns plist with :added, :removed counts."
  (let ((counts (claude-repl-diff--extract-line-counts diff-buffer)))
    (list :added (car counts)
          :removed (cdr counts))))

(provide 'claude-repl-diff)
;;; claude-repl-diff.el ends here
