;;; ai.el --- AI assistant integration -*- lexical-binding: t -*-

;;; Commentary:
;; Claude AI assistant integration using claude-code-ide.el

;;; Code:

;; ============================================================================
;; Claude Code IDE
;; ============================================================================

(use-package claude-code-ide
  :config
  ;; Claude Code IDE uses the Claude Code CLI which manages its own API key
  ;; You can configure additional CLI flags with:
  ;; (setq claude-code-ide-cli-extra-flags "--model opus")

  ;; AI keybindings with general.el
  (general-define-key
   :states '(normal visual)
   :prefix "SPC a"
   "" '(:ignore t :which-key "ai")
   "a" '(claude-code-ide :which-key "start session")
   "m" '(claude-code-ide-menu :which-key "menu")
   "c" '(claude-code-ide-continue :which-key "continue")
   "r" '(claude-code-ide-resume :which-key "resume")
   "q" '(claude-code-ide-stop :which-key "stop")
   "b" '(claude-code-ide-switch-to-buffer :which-key "switch to buffer")
   "t" '(claude-code-ide-toggle :which-key "toggle window")
   "l" '(claude-code-ide-list-sessions :which-key "list sessions")
   "i" '(claude-code-ide-insert-at-mentioned :which-key "insert selection")
   "p" '(claude-code-ide-send-prompt :which-key "send prompt")
   "s" '(claude-code-ide-check-status :which-key "check status"))

  ;; Local leader bindings for AI in programming buffers
  (general-define-key
   :states '(normal visual)
   :keymaps 'prog-mode-map
   :prefix ","
   "a" '(:ignore t :which-key "ai")
   "aa" '(claude-code-ide :which-key "start session")
   "am" '(claude-code-ide-menu :which-key "menu")
   "ai" '(claude-code-ide-insert-at-mentioned :which-key "insert selection")
   "ap" '(claude-code-ide-send-prompt :which-key "send prompt")))

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
