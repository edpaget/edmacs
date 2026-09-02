;;; ai.el --- AI assistant integration -*- lexical-binding: t -*-

;;; Commentary:
;; Claude REPL integration with REPL-style interface

;;; Code:

;; ============================================================================
;; Markdown Mode
;; ============================================================================

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :custom
  (markdown-hide-markup t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  ;; Hooked on `markdown-mode' (gfm-mode derives from it) rather than
  ;; `text-mode', so the claude-repl transcript buffer is not affected.
  :hook ((markdown-mode . variable-pitch-mode)
         (markdown-mode . olivetti-mode)
         (markdown-mode . (lambda () (setq-local line-spacing 0.15)))))

(use-package olivetti
  :straight t
  :custom (olivetti-body-width 84))

;; ============================================================================
;; Claude REPL Integration
;; ============================================================================

(add-to-list 'load-path
             (expand-file-name "claude-repl"
                               (file-name-directory
                                (or load-file-name
                                    buffer-file-name
                                    (expand-file-name "ai.el" default-directory)))))

(require 'claude-repl-core)

(with-eval-after-load 'claude-repl-buffer
  (setq claude-repl-buffer-header-style 'unicode-fancy)
  (setq claude-repl-separator-style 'labeled)
  (setq claude-repl-use-icons t)
  (setq claude-repl-input-prompt-string "claude> ")
  (setq claude-repl-section-spacing 1)
  ;; (setq claude-repl-buffer-max-width 120)
  )

;; ============================================================================
;; Claude REPL Keybindings
;; ============================================================================

(with-eval-after-load 'general
  (general-define-key
   :states 'normal
   :prefix "SPC a"
   "" '(:ignore t :which-key "ai/claude")
   "a" '(claude-repl-ask :which-key "ask claude")
   "I" '(claude-repl-interrupt-and-ask :which-key "interrupt and ask")
   "b" '(claude-repl-open-buffer :which-key "open buffer")
   "c" '(claude-repl-clear-buffer :which-key "clear buffer")
   "s" '(claude-repl-process-start-current-project :which-key "start process")
   "k" '(claude-repl-process-kill-current-project :which-key "kill process")
   "K" '(claude-repl-process-kill-all :which-key "kill all processes")
   "l" '(claude-repl-show-processes :which-key "list processes")
   "i" '(claude-repl-process-status-current-project :which-key "status")
   "t" '(claude-repl-test-prompt :which-key "test prompt (debug)"))

  ;; Approval management keybindings
  (general-define-key
   :states 'normal
   :prefix "SPC a p"
   "" '(:ignore t :which-key "approval policy")
   "m" '(claude-repl-approval-set-mode :which-key "set mode")
   "s" '(claude-repl-approval-show-policy :which-key "show policy")
   "a" '(claude-repl-approval-add-allow-rule :which-key "add allow rule")
   "d" '(claude-repl-approval-add-deny-rule :which-key "add deny rule")
   "r" '(claude-repl-approval-reset-rules :which-key "reset rules")
   "c" '(claude-repl-approval-clear-session-rules :which-key "clear session rules")
   "l" '(claude-repl-approval-show-session-rules :which-key "show session rules"))

  ;; Local leader bindings for AI in programming buffers
  (general-define-key
   :states 'normal
   :keymaps 'prog-mode-map
   :prefix ","
   "a" '(:ignore t :which-key "ai")
   "aa" '(claude-repl-ask :which-key "ask claude")
   "aI" '(claude-repl-interrupt-and-ask :which-key "interrupt and ask")
   "ab" '(claude-repl-open-buffer :which-key "open buffer")
   "ac" '(claude-repl-clear-buffer :which-key "clear buffer")
   "as" '(claude-repl-process-start-current-project :which-key "start process")
   "ak" '(claude-repl-process-kill-current-project :which-key "kill process")))

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
