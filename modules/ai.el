;;; ai.el --- AI assistant integration -*- lexical-binding: t -*-

;;; Commentary:
;; Claude Code integration via vterm

;;; Code:

;; ============================================================================
;; Markdown Mode
;; ============================================================================

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; ============================================================================
;; Claude Code VTerm Integration
;; ============================================================================

(defvar-local claude-code-project-root nil
  "The project root for this Claude Code vterm buffer.")

(defun claude-code--find-buffer-for-project (project-root)
  "Find the Claude Code vterm buffer associated with PROJECT-ROOT."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (and (eq major-mode 'vterm-mode)
                       (boundp 'claude-code-project-root)
                       claude-code-project-root
                       (string= claude-code-project-root project-root))))
              (buffer-list)))

(defun claude-code-vterm ()
  "Start Claude Code in a vterm session in the current project root.
Uses `pop-to-buffer' to display the buffer in a sensible location."
  (interactive)
  (require 'vterm)
  (require 'projectile)
  (let* ((project-root (or (projectile-project-root)
                           default-directory))
         (existing-buffer (claude-code--find-buffer-for-project project-root)))
    (if existing-buffer
        ;; Switch to existing buffer
        (progn
          (pop-to-buffer existing-buffer)
          (when (bound-and-true-p evil-mode)
            (evil-insert-state)))
      ;; Create new vterm buffer
      (let* ((vterm-shell "claude")
             (buffer-name (format "claude-code: %s" (file-name-nondirectory (directory-file-name project-root)))))
        (pop-to-buffer
         (with-current-buffer (vterm buffer-name)
           (setq default-directory project-root)
           (setq-local claude-code-project-root project-root)
           (vterm-send-string (concat "cd " (shell-quote-argument project-root)))
           (vterm-send-return)
           (current-buffer)))
        (when (bound-and-true-p evil-mode)
          (evil-insert-state))))))

(defun claude-code-vterm-other-window ()
  "Start Claude Code in a vterm session in other window."
  (interactive)
  (let ((buffer (save-window-excursion
                  (call-interactively #'claude-code-vterm)
                  (current-buffer))))
    (switch-to-buffer-other-window buffer)))

(defun claude-code-switch ()
  "Switch to the Claude Code vterm buffer for the current project.
Uses `pop-to-buffer' to display the buffer."
  (interactive)
  (require 'projectile)
  (let* ((project-root (or (projectile-project-root)
                           default-directory))
         (existing-buffer (claude-code--find-buffer-for-project project-root)))
    (if existing-buffer
        (progn
          (pop-to-buffer existing-buffer)
          (when (bound-and-true-p evil-mode)
            (evil-insert-state)))
      (message "No Claude Code session found for this project. Use `claude-code-vterm` to start one."))))

(defun claude-code-close ()
  "Close the Claude Code vterm session for the current project."
  (interactive)
  (require 'projectile)
  (let* ((project-root (or (projectile-project-root)
                           default-directory))
         (existing-buffer (claude-code--find-buffer-for-project project-root)))
    (if existing-buffer
        (progn
          (kill-buffer existing-buffer)
          (message "Closed Claude Code session for %s" (file-name-nondirectory (directory-file-name project-root))))
      (message "No Claude Code session found for this project."))))

(defvar claude-code-prompt-buffer-name "*claude-code-prompt*"
  "Name of the temporary buffer for Claude Code prompts.")

(defvar-local claude-code-prompt-target-buffer nil
  "The Claude Code vterm buffer to send the prompt to.")

(defun claude-code-send-prompt-and-close ()
  "Send the prompt buffer contents to Claude Code and close the prompt buffer."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (target-buffer claude-code-prompt-target-buffer))
    (if (and target-buffer (buffer-live-p target-buffer))
        (progn
          ;; Close the prompt buffer and window
          (quit-window t)
          ;; Send the prompt to Claude Code vterm
          (with-current-buffer target-buffer
            (vterm-send-string prompt)
            (vterm-send-return))
          (message "Prompt sent to Claude Code"))
      (message "Claude Code session no longer exists")
      (quit-window t))))

(defun claude-code-prompt-abort ()
  "Abort the prompt and close the buffer."
  (interactive)
  (quit-window t)
  (message "Prompt cancelled"))

(defvar claude-code-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-code-send-prompt-and-close)
    (define-key map (kbd "C-c C-k") #'claude-code-prompt-abort)
    map)
  "Keymap for Claude Code prompt mode.")

(define-derived-mode claude-code-prompt-mode markdown-mode "Claude-Prompt"
  "Major mode for composing prompts to send to Claude Code."
  (setq-local header-line-format
              (substitute-command-keys
               "Compose prompt for Claude Code. \\[claude-code-send-prompt-and-close] to send, \\[claude-code-prompt-abort] to cancel.")))

(defun claude-code-prompt (&optional initial-text)
  "Open a temporary buffer to compose and send a prompt to Claude Code.
If INITIAL-TEXT is provided, insert it into the prompt buffer."
  (interactive)
  (require 'projectile)
  (let* ((project-root (or (projectile-project-root)
                           default-directory))
         (existing-buffer (claude-code--find-buffer-for-project project-root)))
    (if existing-buffer
        (let ((prompt-buffer (get-buffer-create claude-code-prompt-buffer-name)))
          (pop-to-buffer prompt-buffer)
          (claude-code-prompt-mode)
          (erase-buffer)
          (when initial-text
            (insert initial-text))
          (setq-local claude-code-prompt-target-buffer existing-buffer)
          ;; Start in insert mode if using evil
          (when (bound-and-true-p evil-mode)
            (evil-insert-state)))
      (message "No Claude Code session found for this project. Use `claude-code-vterm` to start one."))))

(defun claude-code-send-region (start end)
  "Send the selected region directly to Claude Code."
  (interactive "r")
  (require 'projectile)
  (let* ((project-root (or (projectile-project-root)
                           default-directory))
         (existing-buffer (claude-code--find-buffer-for-project project-root))
         (text (buffer-substring-no-properties start end)))
    (if existing-buffer
        (progn
          (with-current-buffer existing-buffer
            (vterm-send-string text)
            (vterm-send-return))
          (message "Region sent to Claude Code"))
      (message "No Claude Code session found for this project. Use `claude-code-vterm` to start one."))))

(defun claude-code-prompt-region (start end)
  "Open prompt buffer with the selected region as initial text."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (claude-code-prompt text)))

;; AI keybindings with general.el
(general-define-key
 :states 'normal
 :prefix "SPC a"
 "" '(:ignore t :which-key "ai/claude")
 "a" '(claude-code-vterm :which-key "claude code")
 "o" '(claude-code-vterm-other-window :which-key "claude code (other window)")
 "b" '(claude-code-switch :which-key "switch to claude code")
 "p" '(claude-code-prompt :which-key "prompt claude code")
 "k" '(claude-code-close :which-key "close claude code"))

(general-define-key
 :states 'visual
 :prefix "SPC a"
 "" '(:ignore t :which-key "ai/claude")
 "s" '(claude-code-send-region :which-key "send region to claude")
 "p" '(claude-code-prompt-region :which-key "prompt with region"))

;; Local leader bindings for AI in programming buffers
(general-define-key
 :states 'normal
 :keymaps 'prog-mode-map
 :prefix ","
 "a" '(:ignore t :which-key "ai")
 "aa" '(claude-code-vterm :which-key "claude code")
 "ao" '(claude-code-vterm-other-window :which-key "claude code (other window)")
 "ab" '(claude-code-switch :which-key "switch to claude code")
 "ap" '(claude-code-prompt :which-key "prompt claude code")
 "ak" '(claude-code-close :which-key "close claude code"))

(general-define-key
 :states 'visual
 :keymaps 'prog-mode-map
 :prefix ","
 "a" '(:ignore t :which-key "ai")
 "as" '(claude-code-send-region :which-key "send region to claude")
 "ap" '(claude-code-prompt-region :which-key "prompt with region"))

;; ============================================================================
;; Optional: gptel (alternative/additional AI interface)
;; ============================================================================

;; Uncomment if you want to use gptel as an alternative or additional AI tool
;; (use-package gptel
;;   :config
;;   (setq gptel-api-key (getenv "ANTHROPIC_API_KEY")
;;         gptel-model "claude-sonnet-4-20250514"
;;         gptel-backend (gptel-make-anthropic "Claude"
;;                         :stream t
;;                         :key gptel-api-key))
;;
;;   ;; Additional gptel keybindings
;;   (general-define-key
;;    :states '(normal visual)
;;    :prefix "SPC a"
;;    "g" '(gptel :which-key "gptel chat")
;;    "G" '(gptel-send :which-key "gptel send")))

;;; ai.el ends here
