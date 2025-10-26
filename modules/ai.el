;;; ai.el --- AI assistant integration -*- lexical-binding: t -*-

;;; Commentary:
;; Claude AI assistant integration using claude-code-ide.el

;;; Code:

;; ============================================================================
;; Claude Code IDE
;; ============================================================================

(use-package claude-code-ide
  :config
  ;; API key configuration
  ;; Set your API key in one of these ways:
  ;; 1. Set the ANTHROPIC_API_KEY environment variable
  ;; 2. Set it directly (not recommended for shared configs):
  ;;    (setq claude-code-ide-api-key "your-api-key-here")

  ;; Model configuration
  (setq claude-code-ide-model "claude-sonnet-4-20250514")  ; or your preferred model

  ;; AI keybindings with general.el
  (general-define-key
   :states '(normal visual)
   :prefix "SPC a"
   "" '(:ignore t :which-key "ai")
   "a" '(claude-code-ide-chat :which-key "chat")
   "c" '(claude-code-ide-code-action :which-key "code action")
   "e" '(claude-code-ide-explain :which-key "explain")
   "r" '(claude-code-ide-refactor :which-key "refactor")
   "d" '(claude-code-ide-document :which-key "document")
   "f" '(claude-code-ide-fix :which-key "fix")
   "t" '(claude-code-ide-test :which-key "generate test")
   "o" '(claude-code-ide-optimize :which-key "optimize")
   "s" '(claude-code-ide-suggest :which-key "suggest"))

  ;; Local leader bindings for AI in programming buffers
  (general-define-key
   :states '(normal visual)
   :keymaps 'prog-mode-map
   :prefix ","
   "a" '(:ignore t :which-key "ai")
   "aa" '(claude-code-ide-chat :which-key "chat")
   "ac" '(claude-code-ide-code-action :which-key "code action")
   "ae" '(claude-code-ide-explain :which-key "explain")
   "ar" '(claude-code-ide-refactor :which-key "refactor")))

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
