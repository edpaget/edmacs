;;; completion.el --- Completion framework configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Modern completion setup with Vertico, Corfu, Consult, and friends.

;;; Code:

;; ============================================================================
;; Vertico - Vertical completion UI
;; ============================================================================

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)  ; Cycle from last to first candidate
  )

;; Vertico extensions
(use-package vertico-directory
  :straight nil
  :after vertico
  :load-path "straight/repos/vertico/extensions/"
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; ============================================================================
;; Orderless - Flexible completion style
;; ============================================================================

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; ============================================================================
;; Marginalia - Annotations for completions
;; ============================================================================

(use-package marginalia
  :init
  (marginalia-mode)
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; ============================================================================
;; Consult - Enhanced commands
;; ============================================================================

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)

         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)

         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)

         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :config
  ;; Optionally configure preview
  (setq consult-preview-key 'any)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure register preview
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Use consult for register actions
  (advice-add #'register-preview :override #'consult-register-window))

;; ============================================================================
;; Embark - Contextual actions
;; ============================================================================

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-Consult integration
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
;; Corfu - In-buffer completion
;; ============================================================================

(use-package corfu
  :init
  (global-corfu-mode)

  :config
  (setq corfu-cycle t                ; Cycle through candidates
        corfu-auto t                 ; Enable auto completion
        corfu-auto-delay 0.2         ; Delay before showing completions
        corfu-auto-prefix 2          ; Minimum prefix length for auto completion
        corfu-quit-no-match 'separator ; Quit if no match
        corfu-preview-current nil    ; Don't preview current candidate
        corfu-preselect 'prompt      ; Preselect the prompt
        corfu-on-exact-match nil)    ; Don't auto-select single match

  ;; Enable Corfu in minibuffer
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; Corfu extensions
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :load-path "straight/repos/corfu/extensions/"
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; ============================================================================
;; Cape - Completion At Point Extensions
;; ============================================================================

(use-package cape
  :init
  ;; Add useful completion-at-point functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; ============================================================================
;; Kind Icon - Icons for completions
;; ============================================================================

(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; completion.el ends here
