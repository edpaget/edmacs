;;; javascript.el --- JavaScript and TypeScript configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Configuration for JavaScript, TypeScript, JSX, and TSX development.
;; Uses tree-sitter modes for enhanced syntax understanding.

;;; Code:

;; ============================================================================
;; JavaScript Mode (Tree-sitter)
;; ============================================================================

(use-package js
  :straight nil
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :config
  ;; JavaScript settings
  (setq js-indent-level 2
        js-switch-indent-offset 2)

  ;; Enable LSP for JavaScript
  (add-hook 'js-ts-mode-hook #'lsp-deferred)

  ;; Enable smartparens for structural editing
  (add-hook 'js-ts-mode-hook #'smartparens-mode))

;; ============================================================================
;; TypeScript Mode (Tree-sitter)
;; ============================================================================

(use-package typescript-ts-mode
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  ;; TypeScript settings
  (setq typescript-ts-mode-indent-offset 2)

  ;; Enable LSP for TypeScript
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
  (add-hook 'tsx-ts-mode-hook #'lsp-deferred)

  ;; Enable smartparens for structural editing
  (add-hook 'typescript-ts-mode-hook #'smartparens-mode)
  (add-hook 'tsx-ts-mode-hook #'smartparens-mode))

;; ============================================================================
;; JSON Mode (Tree-sitter)
;; ============================================================================

(use-package json-ts-mode
  :straight nil
  :mode ("\\.json\\'" . json-ts-mode)
  :config
  ;; Enable LSP for JSON
  (add-hook 'json-ts-mode-hook #'lsp-deferred))

;; ============================================================================
;; Language-Specific Keybindings
;; ============================================================================

;; JavaScript keybindings with local leader
(general-define-key
 :states 'normal
 :keymaps 'js-ts-mode-map
 :prefix ","
 "" '(:ignore t :which-key "javascript")

 ;; Running/Building
 "r" '(:ignore t :which-key "run")
 "rr" '(nodejs-repl :which-key "node repl")
 "re" '(nodejs-repl-send-last-expression :which-key "send expression")
 "rb" '(nodejs-repl-send-buffer :which-key "send buffer")

 ;; Testing (assuming jest or similar)
 "t" '(:ignore t :which-key "test")
 "tt" '(projectile-test-project :which-key "test project")
 "tf" '(projectile-find-test-file :which-key "find test file")

 ;; Refactoring
 "=" '(:ignore t :which-key "refactor")
 "=i" '(lsp-organize-imports :which-key "organize imports")
 "=r" '(lsp-rename :which-key "rename")

 ;; Documentation
 "d" '(:ignore t :which-key "doc")
 "dd" '(lsp-describe-thing-at-point :which-key "describe"))

;; TypeScript keybindings with local leader
(general-define-key
 :states 'normal
 :keymaps '(typescript-ts-mode-map tsx-ts-mode-map)
 :prefix ","
 "" '(:ignore t :which-key "typescript")

 ;; Compilation/Building
 "c" '(:ignore t :which-key "compile")
 "cc" '(projectile-compile-project :which-key "compile project")
 "cr" '(projectile-run-project :which-key "run project")

 ;; Testing
 "t" '(:ignore t :which-key "test")
 "tt" '(projectile-test-project :which-key "test project")
 "tf" '(projectile-find-test-file :which-key "find test file")

 ;; Refactoring
 "=" '(:ignore t :which-key "refactor")
 "=i" '(lsp-organize-imports :which-key "organize imports")
 "=r" '(lsp-rename :which-key "rename")
 "=a" '(lsp-execute-code-action :which-key "code action")

 ;; Documentation
 "d" '(:ignore t :which-key "doc")
 "dd" '(lsp-describe-thing-at-point :which-key "describe"))

;; ============================================================================
;; LSP Configuration
;; ============================================================================

(with-eval-after-load 'lsp-mode
  ;; TypeScript/JavaScript LSP settings
  (setq lsp-typescript-suggest-auto-imports t
        lsp-typescript-preferences-import-module-specifier "relative"
        lsp-typescript-preferences-quote-style "single"
        lsp-javascript-suggest-auto-imports t)

  ;; ESLint auto-fix on save
  (setq lsp-eslint-auto-fix-on-save t
        lsp-eslint-enable t))

;; ============================================================================
;; Consult-LSP Integration
;; ============================================================================

(with-eval-after-load 'consult-lsp
  (general-define-key
   :states 'normal
   :keymaps '(js-ts-mode-map typescript-ts-mode-map tsx-ts-mode-map)
   :prefix "SPC c"
   "s" '(consult-lsp-symbols :which-key "workspace symbols")
   "S" '(consult-lsp-file-symbols :which-key "file symbols")))

;; ============================================================================
;; Apheleia - Format on save with Prettier
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; Use Prettier for JS/TS files
  (dolist (mode '(js-ts-mode typescript-ts-mode tsx-ts-mode json-ts-mode))
    (add-to-list 'apheleia-mode-alist (cons mode 'prettier))))

;; ============================================================================
;; Node.js REPL Integration
;; ============================================================================

(use-package nodejs-repl
  :commands (nodejs-repl nodejs-repl-send-last-expression nodejs-repl-send-buffer)
  :config
  (setq nodejs-repl-command "node"))

;; ============================================================================
;; NPM Integration
;; ============================================================================

;; Add npm script runner keybindings
(general-define-key
 :states 'normal
 :keymaps '(js-ts-mode-map typescript-ts-mode-map tsx-ts-mode-map)
 :prefix ", n"
 "" '(:ignore t :which-key "npm")
 "i" '((lambda () (interactive)
         (projectile-run-shell-command-in-root "npm install"))
       :which-key "npm install")
 "r" '((lambda () (interactive)
         (projectile-run-shell-command-in-root "npm run"))
       :which-key "npm run")
 "t" '((lambda () (interactive)
         (projectile-run-shell-command-in-root "npm test"))
       :which-key "npm test")
 "s" '((lambda () (interactive)
         (projectile-run-shell-command-in-root "npm start"))
       :which-key "npm start")
 "b" '((lambda () (interactive)
         (projectile-run-shell-command-in-root "npm run build"))
       :which-key "npm build")
 "l" '((lambda () (interactive)
         (projectile-run-shell-command-in-root "npm run lint"))
       :which-key "npm lint"))

(provide 'javascript)
;;; javascript.el ends here
