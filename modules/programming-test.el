;;; programming-test.el --- Tests for programming.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pins the `SPC c' keymap `modules/programming.el' owns, against REAL
;; `evil' and `general' loaded out of the straight repos tree (this
;; checkout's, falling back to the sibling main checkout's) -- the same
;; convention `keybindings-test.el' uses, so this runs from a worktree
;; too.  `programming.el' had no suite at all until now, so an edit
;; reintroducing a retired `flycheck-'/`lsp-' command under `SPC c', or
;; re-adding the `eglot-mode-map' entries that shadow the global flymake
;; keys, would have shipped with nothing to catch it.
;;
;; The bindings are asserted end to end through `key-binding' in a real
;; evil normal-state buffer, not by reading whichever keymap general
;; happened to write into: where the binding lives is an implementation
;; detail, whether the key reaches the command is the contract.  The
;; buffer is `emacs-lisp-mode' -- deliberately not a language server's
;; major mode -- because the point of moving these keys off
;; `eglot-mode-map' was that they resolve in an unmanaged buffer.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/programming-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; Declared, not required: every one of these arrives only once
;; `edmacs-programming-test--ensure' has loaded evil, general and
;; programming.el, which a byte-compile of this file alone never does.
(defvar use-package-keywords)
(defvar eglot-mode-map)
(defvar eglot-managed-mode-hook)
(defvar flymake-show-diagnostics-at-end-of-line)
(defvar flymake-no-changes-timeout)
(declare-function evil-local-mode "evil-core")
(declare-function evil-normal-state "evil-states")
(declare-function evil-normalize-keymaps "evil-core")
(declare-function evil-get-auxiliary-keymap "evil-core")
(declare-function general-define-key "general")
(declare-function use-package-process-keywords "use-package-core")

(defvar edmacs-programming-test--repo-root
  (file-name-as-directory
   (file-name-directory
    (directory-file-name
     (file-name-directory
      (or load-file-name buffer-file-name default-directory)))))
  "This checkout's root, resolved from this file's own location.
`default-directory' is never consulted, so the suite runs from any cwd.")

(defun edmacs-programming-test--locate-straight-repos-root ()
  "Return a populated `straight/repos' directory, or nil.
This checkout's own first, then the sibling main `edmacs' checkout's --
a roadmap worktree lives at `<parent>/edmacs__worktrees/<name>', sibling
to `<parent>/edmacs', and only the latter has a bootstrapped tree."
  (or
   (let ((here (expand-file-name "straight/repos" edmacs-programming-test--repo-root)))
     (and (file-directory-p here) here))
   (let* ((root (directory-file-name edmacs-programming-test--repo-root))
          (worktrees-dir (directory-file-name (file-name-directory root))))
     (when (string-suffix-p "__worktrees" worktrees-dir)
       (let* ((projects-dir (file-name-directory worktrees-dir))
              (repo-name (string-remove-suffix
                          "__worktrees" (file-name-nondirectory worktrees-dir)))
              (main-repos (expand-file-name
                           (concat repo-name "/straight/repos") projects-dir)))
         (and (file-directory-p main-repos) main-repos))))))

(defvar edmacs-programming-test--state nil
  "nil before the first load attempt, then t or `unavailable'.")

(defun edmacs-programming-test--ensure ()
  "Load real evil/general and `modules/programming.el', once.
Return non-nil on success, nil when straight has never been bootstrapped
here, so every caller can `ert-skip' rather than error on file load."
  (unless edmacs-programming-test--state
    (setq edmacs-programming-test--state
          (let* ((repos (edmacs-programming-test--locate-straight-repos-root))
                 (evil-source (and repos (expand-file-name "evil/evil.el" repos)))
                 (general-source (and repos (expand-file-name "general.el/general.el" repos))))
            (if (not (and evil-source
                          (file-exists-p evil-source)
                          (file-exists-p general-source)))
                'unavailable
              (add-to-list 'load-path (file-name-directory evil-source))
              (add-to-list 'load-path (file-name-directory general-source))
              (require 'evil)
              (require 'general)
              (require 'use-package)
              ;; `:straight' is unrecognized with no straight.el loaded, and an
              ;; unrecognized keyword aborts the whole block's expansion -- which
              ;; would silently skip the `:straight nil' flymake block this file
              ;; exists to test. Same no-op handler `completion-test.el' registers.
              (add-to-list 'use-package-keywords :straight t)
              (defun use-package-normalize/:straight (_name _keyword args) args)
              (defun use-package-handler/:straight (name _keyword _arg rest state)
                (use-package-process-keywords name rest state))
              (load (expand-file-name "modules/programming.el"
                                      edmacs-programming-test--repo-root)
                    nil t)
              ;; eglot is built in and its block defers `:config'; requiring it
              ;; runs that body, which is what defines the eglot-side bindings.
              (require 'eglot)
              t))))
  (eq edmacs-programming-test--state t))

(defmacro edmacs-programming-test--with-normal-state (&rest body)
  "Run BODY in an `emacs-lisp-mode' buffer in evil normal state.
`emacs-lisp-mode' has no language server, which is the point: the
diagnostics keys must resolve without eglot managing the buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     ;; The mode hooks are suppressed: `programming.el' hooks packages onto
     ;; `prog-mode-hook' that a bare `-Q' harness has no load-path for, and
     ;; none of them participates in key resolution.
     (let ((prog-mode-hook nil)
           (lisp-mode-hook nil)
           (emacs-lisp-mode-hook nil))
       (emacs-lisp-mode))
     (evil-local-mode 1)
     (evil-normal-state)
     (evil-normalize-keymaps)
     ,@body))

(defun edmacs-programming-test--commands-under (keys)
  "Return every command symbol reachable under the key sequence KEYS."
  (let ((map (key-binding (kbd keys)))
        (found '()))
    (when (keymapp map)
      (map-keymap
       (lambda (_event def)
         (cond
          ((keymapp def)
           (map-keymap (lambda (_e d) (when (symbolp d) (push d found))) def))
          ((symbolp def) (push def found))))
       map))
    found))

(defconst edmacs-programming-test--skip-message
  "real evil.el/general.el not found in this checkout or its sibling main \
checkout; bootstrap straight once locally to enable this test")

;; ============================================================================
;; Flymake settings the flycheck block used to own
;; ============================================================================

(ert-deftest edmacs-programming-test-inline-diagnostics-are-enabled ()
  "`flymake-show-diagnostics-at-end-of-line' replaces flycheck's inline
annotations, which defaulted off.  A nil here is a silent feature loss."
  (unless (edmacs-programming-test--ensure)
    (ert-skip edmacs-programming-test--skip-message))
  (should (eq flymake-show-diagnostics-at-end-of-line 'short)))

(ert-deftest edmacs-programming-test-flymake-runs-in-every-prog-buffer ()
  "`prog-mode-hook' -> `flymake-mode' is what replaced `global-flycheck-mode'.
Without it only eglot-managed buffers get diagnostics at all."
  (unless (edmacs-programming-test--ensure)
    (ert-skip edmacs-programming-test--skip-message))
  (should (memq #'flymake-mode prog-mode-hook)))

(ert-deftest edmacs-programming-test-flymake-check-stays-on-an-idle-timer ()
  "`flymake-no-changes-timeout' must stay non-nil: eglot folds a pushed
diagnostic into the current report only while flymake is idle-timed."
  (unless (edmacs-programming-test--ensure)
    (ert-skip edmacs-programming-test--skip-message))
  (should (numberp flymake-no-changes-timeout)))

;; ============================================================================
;; The diagnostics keys resolve without a language server
;; ============================================================================

(ert-deftest edmacs-programming-test-diagnostics-keys-resolve-outside-eglot ()
  "`SPC c x n/p/l', `SPC c w' and `SPC c W' reach flymake in a buffer no
language server manages -- the reason they are global, not on
`eglot-mode-map'."
  (unless (edmacs-programming-test--ensure)
    (ert-skip edmacs-programming-test--skip-message))
  (edmacs-programming-test--with-normal-state
    (should (eq (key-binding (kbd "SPC c x n")) 'flymake-goto-next-error))
    (should (eq (key-binding (kbd "SPC c x p")) 'flymake-goto-prev-error))
    (should (eq (key-binding (kbd "SPC c x l")) 'flymake-show-buffer-diagnostics))
    (should (eq (key-binding (kbd "SPC c w")) 'consult-flymake))
    (should (eq (key-binding (kbd "SPC c W")) 'flymake-show-project-diagnostics))))

(ert-deftest edmacs-programming-test-eglot-map-does-not-shadow-the-flymake-keys ()
  "A minor-mode map wins over the global one, so `eglot-mode-map' must not
carry its own w/W/xl/xn/xp -- the duplicates this phase removed."
  (unless (edmacs-programming-test--ensure)
    (ert-skip edmacs-programming-test--skip-message))
  (let ((aux (evil-get-auxiliary-keymap eglot-mode-map 'normal)))
    (should (keymapp aux))
    ;; The verbs eglot itself owns are still there ...
    (should (eq (lookup-key aux (kbd "SPC c a")) 'eglot-code-actions))
    (should (eq (lookup-key aux (kbd "SPC c h")) 'eldoc-doc-buffer))
    ;; ... and the client-agnostic ones are not. `commandp' is the wrong
    ;; test here: `consult-flymake' is an autoload this bare harness never
    ;; resolves, so a re-added duplicate would read as "not a command".
    (dolist (key '("SPC c w" "SPC c W" "SPC c x l" "SPC c x n" "SPC c x p"))
      (let ((def (lookup-key aux (kbd key))))
        (should (or (null def) (numberp def)))))))

;; ============================================================================
;; The retired clients stay retired
;; ============================================================================

(ert-deftest edmacs-programming-test-no-retired-checker-command-under-spc-c ()
  "No `flycheck-'/`lsp-'/`dap-' command is reachable under `SPC c'.
Retiring those packages leaves their commands void, so a rebound key
would be a runtime error, not a fallback."
  (unless (edmacs-programming-test--ensure)
    (ert-skip edmacs-programming-test--skip-message))
  (edmacs-programming-test--with-normal-state
    (let ((commands (append (edmacs-programming-test--commands-under "SPC c")
                            (edmacs-programming-test--commands-under "SPC c x"))))
      (should commands)
      (dolist (cmd commands)
        (should-not (string-match-p "\\`\\(flycheck\\|lsp\\|dap\\)-"
                                    (symbol-name cmd)))))))

(ert-deftest edmacs-programming-test-no-flycheck-hook-survives ()
  "Nothing re-enables flycheck from a hook: the eglot/flycheck coexistence
bridge and the yaml `flycheck-mode' hook are both gone."
  (unless (edmacs-programming-test--ensure)
    (ert-skip edmacs-programming-test--skip-message))
  (should-not (fboundp 'edmacs--eglot-disable-flycheck))
  (dolist (hook '(prog-mode-hook yaml-mode-hook eglot-managed-mode-hook))
    (dolist (entry (and (boundp hook) (symbol-value hook)))
      (when (symbolp entry)
        (should-not (string-prefix-p "flycheck" (symbol-name entry)))))))

;; ============================================================================
;; Semantic tokens on every managed buffer
;; ============================================================================

(ert-deftest edmacs-programming-test-semantic-tokens-hooked ()
  "`eglot-semantic-tokens-mode' is a per-buffer minor mode, so the only way
to have it everywhere is a hook on every eglot-managed buffer.  It goes
through a guarded wrapper, not the raw mode function, because
`eglot-managed-mode-hook' also fires on the disable transition and a
no-arg call always turns the mode back on."
  (unless (edmacs-programming-test--ensure)
    (ert-skip edmacs-programming-test--skip-message))
  (should (memq 'edmacs--eglot-enable-semantic-tokens eglot-managed-mode-hook))
  (should-not (memq 'eglot-semantic-tokens-mode eglot-managed-mode-hook)))

(ert-deftest edmacs-programming-test-semantic-tokens-wrapper-guards-on-disable ()
  "The wrapper enables semantic-tokens-mode only while eglot still manages
the buffer, so it does not resurrect the mode on the disable transition."
  (unless (edmacs-programming-test--ensure)
    (ert-skip edmacs-programming-test--skip-message))
  (let (enabled)
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil))
              ((symbol-function 'eglot-semantic-tokens-mode)
               (lambda (&rest _) (setq enabled t))))
      (edmacs--eglot-enable-semantic-tokens)
      (should-not enabled))
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglot-semantic-tokens-mode)
               (lambda (&rest _) (setq enabled t))))
      (edmacs--eglot-enable-semantic-tokens)
      (should enabled))))

(provide 'programming-test)
;;; programming-test.el ends here
