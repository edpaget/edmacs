;;; completion.el --- Completion framework configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Modern completion setup with Vertico, Corfu, Consult, and friends.

;;; Code:

;; ============================================================================
;; Minibuffer Behavior
;; ============================================================================

;; Lets Embark's self-prompting actions (`embark-become', etc.) and
;; `vertico-suspend' open a minibuffer from inside another one.
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; M-x stops offering commands that cannot run in the current mode/buffer.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Keeps point out of the prompt text itself.
(setq minibuffer-prompt-properties
      (append minibuffer-prompt-properties '(cursor-intangible t)))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; ============================================================================
;; Vertico - Vertical completion UI
;; ============================================================================

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ;; Vim-style navigation
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)
              ;; Vim-style scrolling
              ("C-d" . vertico-scroll-down)
              ("C-u" . vertico-scroll-up)
              ;; Vim-style word deletion
              ("C-w" . backward-kill-word))
  :config
  ;; `vertico-preselect' stays at its shipped default ('directory);
  ;; nothing here needs it changed.
  (setq vertico-cycle t
        vertico-count 13
        vertico-resize t
        vertico-scroll-margin 2))

;; Vertico extensions
(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-h" . vertico-directory-up)
              ("C-l" . vertico-directory-enter))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; buffer/file candidates render in a dedicated buffer instead of the
;; minibuffer for the listed commands; file candidates render as a grid.
(use-package vertico-multiform
  :straight nil
  :after vertico
  :init
  (vertico-multiform-mode)
  :config
  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          (consult-ripgrep buffer)
          (consult-line buffer))
        vertico-multiform-categories
        '((file grid))))

;; `vertico-repeat-save' snapshots the session on every minibuffer entry so
;; `vertico-repeat'/`vertico-repeat-select' can restore it later; the
;; snapshot rides `savehist' across Emacs restarts via
;; `vertico-repeat-history'.
(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :init
  ;; Registering here (not :config) keeps this unconditional on the
  ;; :after vertico wrapper firing -- it must not wait on vertico-repeat.el
  ;; itself being loaded, which :hook only triggers on first minibuffer use.
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

;; `vertico-map''s own bindings (C-j/C-k/C-n/C-p/C-d/C-u/C-w) and
;; `vertico-directory''s (RET/DEL/M-DEL/C-h/C-l) leave C-q and M-q free;
;; `evil-collection' and the rest of this repo bind neither key anywhere
;; (checked: `grep -rn "\"C-q\"\|\"M-q\""' over both straight/build/evil-collection
;; and modules/*.el turned up zero hits).
(use-package vertico-quick
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("C-q" . vertico-quick-exit)))

;; ============================================================================
;; Orderless - Flexible completion style
;; ============================================================================

;; `orderless-style-dispatchers' defaults to `orderless-affix-dispatch',
;; so `=' `^' `~' `,' `!' `&' `%' component affixes already work with no
;; setup here. `&' (match against the annotation) only pays off once
;; marginalia is producing annotations, which the block below does.
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))
        ;; Plain-quoted, not sharp-quoted: orderless's own functions aren't
        ;; visible to the byte-compiler when this file compiles in isolation,
        ;; and sharp-quoting them adds spurious "might not be defined"
        ;; warnings for a value that is otherwise identical at runtime.
        orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))

;; ============================================================================
;; Marginalia - Annotations for completions
;; ============================================================================

(use-package marginalia
  :init
  (marginalia-mode)
  :config
  (setq marginalia-align 'right))

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
  ;; Debounced: with `any', arrowing through results previewed and
  ;; fontified every candidate.
  (setq consult-preview-key '(:debounce 0.3 any))

  ;; Narrow `consult-buffer' (and friends) to one source: `< b' buffers,
  ;; `< f' files, `< p' project, `< ?' lists the available sources.
  (setq consult-narrow-key "<")

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window))

;; ============================================================================
;; Embark - Contextual actions
;; ============================================================================

;; Embark Collect's display-buffer-alist routing (right stack, slot -1,
;; mode-line hidden) lives in modules/windows.el, consolidated with every
;; other popup buffer.
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   ;; Alternative vim-friendly binding
   ("C-o" . embark-act)))

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

  :bind (:map corfu-map
              ;; Vim-style navigation
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ;; Vim completion keys
              ("C-y" . corfu-insert)      ; Accept/yank (vim-style)
              ("C-e" . corfu-quit)        ; Abort (vim-style)
              ;; Tab completion
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ;; Enter to insert
              ("RET" . corfu-insert)
              ([return] . corfu-insert))

  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 3
        corfu-quit-no-match 'separator
        corfu-preview-current nil
        corfu-preselect 'prompt
        corfu-on-exact-match nil)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; straight builds corfu's extensions/ into straight/build/corfu/, already on
;; `load-path'. A :load-path to straight/repos shadows the .elc with source
;; and recompiles it every boot.
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :bind (:map corfu-popupinfo-map
              ;; Vim-style scrolling in documentation popup
              ("C-d" . corfu-popupinfo-scroll-up)
              ("C-u" . corfu-popupinfo-scroll-down)
              ("C-j" . corfu-popupinfo-scroll-up)
              ("C-k" . corfu-popupinfo-scroll-down))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; ============================================================================
;; Cape - Completion At Point Extensions
;; ============================================================================

(use-package cape
  :init
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
