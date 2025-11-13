;;; claude-repl-tool-output.el --- Tool output formatting for Claude Code -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Claude Code Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, claude

;;; Commentary:
;; This module provides formatting and display for tool outputs in claude-repl.
;; It handles displaying the results of Read, Grep, Edit, Write, Bash, and other
;; tools that Claude uses during interactions.

;;; Code:

(require 'claude-repl-diff)

;; ============================================================================
;; Customization
;; ============================================================================

(defgroup claude-repl-tool-output nil
  "Tool output display for claude-repl."
  :group 'claude-repl-buffer
  :prefix "claude-repl-tool-output-")

(defcustom claude-repl-tool-output-show-inline t
  "Show tool outputs inline in conversation buffer.
If nil, tool outputs are not displayed (only tool use requests)."
  :type 'boolean
  :group 'claude-repl-tool-output)

(defcustom claude-repl-tool-output-max-lines 50
  "Maximum lines to show in tool output before truncating.
If nil, show full output regardless of length."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max lines"))
  :group 'claude-repl-tool-output)

(defcustom claude-repl-tool-output-syntax-highlight t
  "Apply syntax highlighting to tool outputs."
  :type 'boolean
  :group 'claude-repl-tool-output)

(defcustom claude-repl-tool-output-style 'boxed
  "Visual style for tool output display.
- \\='boxed: Draw boxes around outputs
- \\='indented: Indent with background color
- \\='minimal: Plain text with separator"
  :type '(choice (const :tag "Boxed" boxed)
                 (const :tag "Indented" indented)
                 (const :tag "Minimal" minimal))
  :group 'claude-repl-tool-output)

(defcustom claude-repl-tool-output-show-metadata t
  "Show metadata (file size, duration, exit codes) for tool outputs."
  :type 'boolean
  :group 'claude-repl-tool-output)

;; ============================================================================
;; Faces
;; ============================================================================

(defface claude-repl-tool-output-header
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for tool output headers."
  :group 'claude-repl-tool-output)

(defface claude-repl-tool-output-border
  '((((background light)) :foreground "#d0d0d0")
    (((background dark)) :foreground "#3e4451"))
  "Face for tool output borders."
  :group 'claude-repl-tool-output)

(defface claude-repl-tool-output-metadata
  '((t :inherit font-lock-comment-face :height 0.9))
  "Face for tool output metadata."
  :group 'claude-repl-tool-output)

(defface claude-repl-tool-output-content
  '((((background light)) :background "#f9f9f9" :extend t)
    (((background dark)) :background "#2a2a2a" :extend t))
  "Face for tool output content background."
  :group 'claude-repl-tool-output)

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defun claude-repl-tool-output--icon (tool-name)
  "Get icon for TOOL-NAME.
Returns emoji or empty string."
  (pcase tool-name
    ("Read" "🔍")
    ("Grep" "🔎")
    ("Edit" "✏️")
    ("Write" "📝")
    ("Bash" "⚡")
    ("Glob" "📁")
    ("Task" "🤖")
    ("TodoWrite" "☑")
    (_ "🔧")))

(defun claude-repl-tool-output--truncate-content (content max-lines)
  "Truncate CONTENT to MAX-LINES if necessary.
Returns (truncated-content . was-truncated-p)."
  (if (or (null max-lines)
          (<= max-lines 0))
      (cons content nil)
    (let* ((lines (split-string content "\n"))
           (line-count (length lines)))
      (if (<= line-count max-lines)
          (cons content nil)
        (cons (concat (string-join (seq-take lines max-lines) "\n")
                      (format "\n\n... (%d lines truncated)" (- line-count max-lines)))
              t)))))

(defun claude-repl-tool-output--apply-syntax-highlighting (content file-path)
  "Apply syntax highlighting to CONTENT based on FILE-PATH.
Returns string with text properties for highlighting."
  (if (not claude-repl-tool-output-syntax-highlight)
      content
    (let* ((mode (claude-repl-diff--derive-mode-from-file file-path))
           (temp-buf (generate-new-buffer " *tool-output-highlight*")))
      (unwind-protect
          (with-current-buffer temp-buf
            (insert content)
            (when mode
              (condition-case err
                  (progn
                    (funcall mode)
                    (font-lock-ensure))
                (error
                 (message "Failed to apply syntax highlighting: %s"
                          (error-message-string err)))))
            (buffer-string))
        (kill-buffer temp-buf)))))

(defun claude-repl-tool-output--make-box (content &optional header)
  "Wrap CONTENT in a box with optional HEADER.
Uses the configured style from `claude-repl-tool-output-style'."
  (let ((width 60))
    (pcase claude-repl-tool-output-style
      ('boxed
       (let ((top-line (propertize (concat "╭─"
                                          (if header (concat " " header " ") "")
                                          (make-string (max 2 (- width (length (or header "")) 4)) ?─)
                                          "╮")
                                  'face 'claude-repl-tool-output-border))
             (bottom-line (propertize (concat "╰" (make-string (- width 2) ?─) "╯")
                                     'face 'claude-repl-tool-output-border)))
         (concat top-line "\n"
                 (propertize content 'face 'claude-repl-tool-output-content)
                 "\n" bottom-line)))

      ('indented
       (let ((indented (mapconcat (lambda (line)
                                   (concat "  " line))
                                 (split-string content "\n")
                                 "\n")))
         (propertize indented 'face 'claude-repl-tool-output-content)))

      ('minimal
       content)

      (_ content))))

;; ============================================================================
;; Formatter Registry
;; ============================================================================

(defvar claude-repl-tool-output-formatters nil
  "Alist mapping tool names to formatter functions.
Each formatter function takes (tool-input tool-output) and returns
a formatted string for display.")

(defun claude-repl-tool-output-register-formatter (tool-name formatter-fn)
  "Register FORMATTER-FN for TOOL-NAME.
FORMATTER-FN should be a function taking (tool-input tool-output)."
  (setf (alist-get tool-name claude-repl-tool-output-formatters
                   nil nil #'string=)
        formatter-fn))

;; ============================================================================
;; Read Tool Formatter
;; ============================================================================

(defun claude-repl-tool-output-format-read (tool-input tool-output)
  "Format Read tool output.
TOOL-INPUT is an alist with file_path, etc.
TOOL-OUTPUT is the file content (string or alist)."
  (let* ((file-path (alist-get 'file_path tool-input))
         (content (if (stringp tool-output)
                      tool-output
                    (alist-get 'content tool-output)))
         (lines (when content (length (split-string content "\n"))))
         (size (when content (length content)))
         ;; Truncate if needed
         (truncated (claude-repl-tool-output--truncate-content
                     content
                     claude-repl-tool-output-max-lines))
         (display-content (car truncated))
         (was-truncated (cdr truncated))
         ;; Apply syntax highlighting
         (highlighted (if file-path
                          (claude-repl-tool-output--apply-syntax-highlighting
                           display-content file-path)
                        display-content)))

    (concat
     ;; Header
     (propertize (format "%s Read: %s"
                        (claude-repl-tool-output--icon "Read")
                        (or file-path "(no path)"))
                'face 'claude-repl-tool-output-header)
     "\n"

     ;; Metadata
     (when claude-repl-tool-output-show-metadata
       (concat
        (propertize (format "Lines: %d | Size: %s"
                           (or lines 0)
                           (if size
                               (file-size-human-readable size)
                             "unknown"))
                   'face 'claude-repl-tool-output-metadata)
        "\n"))
     "\n"

     ;; Content box
     (claude-repl-tool-output--make-box highlighted "Content")
     "\n"

     ;; Truncation notice
     (when was-truncated
       (propertize "  (output truncated)\n"
                  'face 'claude-repl-tool-output-metadata)))))

;; Register the Read formatter
(claude-repl-tool-output-register-formatter "Read"
                                           #'claude-repl-tool-output-format-read)

;; ============================================================================
;; Generic Formatter (fallback)
;; ============================================================================

(defun claude-repl-tool-output-format-generic (tool-name tool-input tool-output)
  "Generic formatter for TOOL-NAME when no specific formatter exists.
TOOL-INPUT and TOOL-OUTPUT are passed as-is."
  (concat
   ;; Header
   (propertize (format "%s %s"
                      (claude-repl-tool-output--icon tool-name)
                      tool-name)
              'face 'claude-repl-tool-output-header)
   "\n\n"

   ;; Input parameters
   (when tool-input
     (concat
      (propertize "Parameters:\n" 'face 'claude-repl-tool-output-metadata)
      (format "%S\n\n" tool-input)))

   ;; Output
   (propertize "Output:\n" 'face 'claude-repl-tool-output-metadata)
   (claude-repl-tool-output--make-box
    (format "%S" tool-output))
   "\n"))

;; ============================================================================
;; Main Formatting Function
;; ============================================================================

(defun claude-repl-tool-output-format (tool-name tool-input tool-output)
  "Format TOOL-OUTPUT for display based on TOOL-NAME.
TOOL-INPUT is the tool parameters alist.
TOOL-OUTPUT is the tool result (string or alist).
Returns formatted string ready for insertion into buffer."
  (condition-case err
      (let ((formatter (alist-get tool-name
                                 claude-repl-tool-output-formatters
                                 nil nil #'string=)))
        (if formatter
            (funcall formatter tool-input tool-output)
          ;; Fallback to generic formatter
          (claude-repl-tool-output-format-generic tool-name tool-input tool-output)))
    (error
     ;; Error during formatting - show raw output
     (format "⚠️  Error formatting %s output: %s\n\nRaw output:\n%S\n"
             tool-name
             (error-message-string err)
             tool-output))))

(provide 'claude-repl-tool-output)
;;; claude-repl-tool-output.el ends here
