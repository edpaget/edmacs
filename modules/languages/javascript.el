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
  (setq js-indent-level 2
        js-switch-indent-offset 2)

  (add-hook 'js-ts-mode-hook #'eglot-ensure)

  (add-hook 'js-ts-mode-hook #'smartparens-mode))

;; ============================================================================
;; TypeScript Mode (Tree-sitter)
;; ============================================================================

(use-package typescript-ts-mode
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2)

  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook #'eglot-ensure)

  (add-hook 'typescript-ts-mode-hook #'smartparens-mode)
  (add-hook 'tsx-ts-mode-hook #'smartparens-mode))

;; ============================================================================
;; JSON Mode (Tree-sitter)
;; ============================================================================

(use-package json-ts-mode
  :straight nil
  :mode ("\\.json\\'" . json-ts-mode)
  :config
  (add-hook 'json-ts-mode-hook #'eglot-ensure))

;; ============================================================================
;; Project.el helpers (projectile replacements)
;; ============================================================================

(defun edmacs-js--project-root ()
  "Return the current project root via project.el, or `default-directory'."
  (if-let* ((proj (project-current)))
      (project-root proj)
    default-directory))

(defun edmacs-js--run-in-project-root (command)
  "Run shell COMMAND with `default-directory' bound to the project root."
  (let ((default-directory (edmacs-js--project-root)))
    (compile command)))

;; ============================================================================
;; Language-Specific Keybindings
;; ============================================================================

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
 "tt" '((lambda () (interactive) (edmacs-js--run-in-project-root "npm test"))
        :which-key "test project")
 ;; No project.el equivalent of projectile-find-test-file; use SPC p f.

 ;; Refactoring
 "=" '(:ignore t :which-key "refactor")
 "=i" '(eglot-code-action-organize-imports :which-key "organize imports")
 "=r" '(eglot-rename :which-key "rename")

 ;; Documentation
 "d" '(:ignore t :which-key "doc")
 "dd" '(eldoc-doc-buffer :which-key "describe"))

(general-define-key
 :states 'normal
 :keymaps '(typescript-ts-mode-map tsx-ts-mode-map)
 :prefix ","
 "" '(:ignore t :which-key "typescript")

 ;; Compilation/Building
 "c" '(:ignore t :which-key "compile")
 "cc" '(project-compile :which-key "compile project")
 "cr" '((lambda () (interactive)
          (edmacs-js--run-in-project-root (read-shell-command "Run: ")))
        :which-key "run project")

 ;; Testing
 "t" '(:ignore t :which-key "test")
 "tt" '((lambda () (interactive) (edmacs-js--run-in-project-root "npm test"))
        :which-key "test project")

 ;; Refactoring
 "=" '(:ignore t :which-key "refactor")
 "=i" '(eglot-code-action-organize-imports :which-key "organize imports")
 "=r" '(eglot-rename :which-key "rename")
 "=a" '(eglot-code-actions :which-key "code action")

 ;; Documentation
 "d" '(:ignore t :which-key "doc")
 "dd" '(eldoc-doc-buffer :which-key "describe"))

;; ============================================================================
;; eglot Configuration
;; ============================================================================

(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)

(defvar edmacs-js--typescript-lsp-cache (make-hash-table :test #'equal)
  "Cache of resolved tsc path -> non-nil when that tsc supports `--lsp'.")

(defun edmacs-js--tsc-supports-lsp-p (tsc)
  "Return non-nil when TSC is a TypeScript 7+ compiler, which serves `--lsp'.
TypeScript 7's Go compiler is itself the language server; 5 and 6 ship a
separate tsserver and answer `--lsp' with error TS5023."
  (let ((cached (gethash tsc edmacs-js--typescript-lsp-cache 'missing)))
    (if (not (eq cached 'missing))
        cached
      (puthash tsc
               (with-temp-buffer
                 (and (eq 0 (ignore-errors
                              (call-process tsc nil t nil "--version")))
                      (progn (goto-char (point-min))
                             (re-search-forward "\\([0-9]+\\)\\." nil t))
                      (>= (string-to-number (match-string 1)) 7)))
               edmacs-js--typescript-lsp-cache))))

(defun edmacs-js--typescript-server (&optional _interactive _project)
  "Return the language server command for a JavaScript or TypeScript buffer.
eglot funcalls a contact function with one or two arguments -- never zero --
so the two ignored parameters are load-bearing.

`tsc' is resolved against the buffer-local `exec-path' mise has already set
from the project's own config chain, so a project pinning TypeScript 5 or 6
gets `typescript-language-server' while everything else gets TS 7's native
server."
  (let ((tsc (executable-find "tsc")))
    (if (and tsc (edmacs-js--tsc-supports-lsp-p tsc))
        (list tsc "--lsp" "--stdio")
      (list "typescript-language-server" "--stdio"))))

;; Ahead of eglot 31.1's own entry, whose first alternative ("rass ts") is an
;; upstream typo that resolves to nothing and falls through to a server we do
;; not install. `add-to-list' prepends and `eglot--lookup-mode' takes the
;; first match, so this wins.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript"))
                 . edmacs-js--typescript-server))

  ;; The servers' own settings keys, not the `lsp-typescript-*' /
  ;; `lsp-javascript-*' wrappers this replaced. Merged into the one shared
  ;; plist; see go.el for why it is a global default rather than buffer-local.
  (setq-default eglot-workspace-configuration
                (plist-put
                 (plist-put (default-value 'eglot-workspace-configuration)
                            :typescript
                            '(:suggest (:autoImports t)
                              :preferences (:importModuleSpecifier "relative"
                                            :quoteStyle "single")))
                 :javascript
                 '(:suggest (:autoImports t)))))

;; The eslint integration is dropped with no replacement: eglot runs one
;; server per project and mode, and the TypeScript server owns that slot, so
;; eslint cannot ride along as a second client the way it did here.
;; Tracked as task `eslint-diagnostics-under-flymake'.

;; Symbol search lives in the client-agnostic `eglot-mode-map' set in
;; programming.el (`SPC c s' / `SPC c S'), so no per-major-mode duplicate here.

;; ============================================================================
;; Apheleia - Format on save with Prettier
;; ============================================================================

(with-eval-after-load 'apheleia
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

(general-define-key
 :states 'normal
 :keymaps '(js-ts-mode-map typescript-ts-mode-map tsx-ts-mode-map)
 :prefix ", n"
 "" '(:ignore t :which-key "npm")
 "i" '((lambda () (interactive)
         (edmacs-js--run-in-project-root "npm install"))
       :which-key "npm install")
 "r" '((lambda () (interactive)
         (edmacs-js--run-in-project-root "npm run"))
       :which-key "npm run")
 "t" '((lambda () (interactive)
         (edmacs-js--run-in-project-root "npm test"))
       :which-key "npm test")
 "s" '((lambda () (interactive)
         (edmacs-js--run-in-project-root "npm start"))
       :which-key "npm start")
 "b" '((lambda () (interactive)
         (edmacs-js--run-in-project-root "npm run build"))
       :which-key "npm build")
 "l" '((lambda () (interactive)
         (edmacs-js--run-in-project-root "npm run lint"))
       :which-key "npm lint"))

(provide 'javascript)
;;; javascript.el ends here
