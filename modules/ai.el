;;; ai.el --- AI assistant integration -*- lexical-binding: t -*-

;;; Commentary:
;; Markdown editing polish and the AI-adjacent editor packages.
;; The Claude CLI integration itself lives in modules/claude-term.el
;; and modules/claude-term-registry.el, which own the SPC a prefix.

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
  ;; On `markdown-mode' (gfm-mode derives from it), not `text-mode',
  ;; so plain text buffers are untouched.
  :hook ((markdown-mode . variable-pitch-mode)
         (markdown-mode . olivetti-mode)
         (markdown-mode . (lambda () (setq-local line-spacing 0.15)))))

(use-package olivetti
  :straight t
  :custom (olivetti-body-width 84))

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
