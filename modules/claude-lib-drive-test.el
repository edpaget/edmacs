;;; claude-lib-drive-test.el --- Tests for claude-lib-drive.el -*- lexical-binding: t -*-

;;; Commentary:
;; In-process coverage of `modules/claude-lib-drive.el': the window pin,
;; the frame-correct restore, the `:choice' stub, output capture, the
;; timeout backstop, the reload report, the `-Q' gate, and the skill
;; document phase 7 carries.
;;
;;   scripts/run-ert-suite.sh 120 \
;;     emacs -Q --batch -l ert \
;;           -l modules/claude-lib.el \
;;           -l modules/claude-lib-drive.el \
;;           -l modules/claude-lib-drive-test.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; The budget is generous because `claude-lib-check-q' forks two real
;; `emacs -Q --batch' children per check.
;;
;; NOT HERE, and deliberately: everything about minibuffer PROMPTS.
;; Under `--batch', `noninteractive' is t and `read-from-minibuffer'
;; reads from STDIN -- a pre-fed `unread-command-events' yields
;; `(end-of-file "Error reading from stdin")' and no
;; `minibuffer-setup-hook' ever runs.  The pre-fed real `completing-read'
;; path, the unanswerable-prompt abort and the daemon-wide wedge they
;; exist to prevent are all in `modules/claude-lib-drive-live-test.el',
;; driven against a real daemon.  `execute-kbd-macro' and the window pin
;; DO work in batch and are tested here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'claude-lib-drive)

;; `cl-letf' on a C subr forces a synchronous native-comp trampoline
;; build -- ~28s of wall clock per target on a cold eln-cache, with the
;; suite still reporting a clean pass. See this repo's CLAUDE.md; the
;; canonical comment is in `modules/sessions-test.el'.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defvar claude-lib-drive-test--repo-root
  (file-name-as-directory (expand-file-name default-directory))
  "Repository root, captured at load time, as every sibling suite does.")

(defmacro claude-lib-drive-test--with-fixture (var contents &rest body)
  "Bind VAR to a temp .el file written with CONTENTS, run BODY, then delete it."
  (declare (indent 2) (debug t))
  `(let ((,var (make-temp-file "claude-lib-drive-test-" nil ".el")))
     (unwind-protect
         (progn (with-temp-file ,var (insert ,contents)) ,@body)
       (ignore-errors (delete-file ,var))
       (ignore-errors (delete-file (concat ,var "c"))))))

(defun claude-lib-drive-test--fresh-buffer (name)
  "Return an empty buffer called NAME, creating or clearing it."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer (erase-buffer))
    buffer))

;; ============================================================================
;; AC1 -- drive without blocking; nothing leaks
;; ============================================================================

(ert-deftest claude-lib-drive-test-choice-stubs-completing-read ()
  "`:choice' answers every `completing-read' without reaching a minibuffer."
  (let ((result (claude-lib-drive
                 (lambda () (completing-read "Pick: " '("alpha" "beta")))
                 :choice "beta")))
    (should (equal (plist-get result :value) "beta"))
    (should-not (plist-get result :error))))

(ert-deftest claude-lib-drive-test-choice-accepts-a-function ()
  "`:choice' also takes a whole `completing-read-function' replacement."
  (let ((result (claude-lib-drive
                 (lambda () (completing-read "Pick: " '("a")))
                 :choice (lambda (prompt &rest _) (concat "saw:" prompt)))))
    (should (equal (plist-get result :value) "saw:Pick: "))))

(ert-deftest claude-lib-drive-test-captures-messages-and-printed-output ()
  "`:messages' carries both `message' and `standard-output' text."
  (let ((result (claude-lib-drive (lambda () (message "hi %d" 7) (princ "printed") 'done))))
    (should (eq (plist-get result :value) 'done))
    (should (string-match-p "hi 7" (plist-get result :messages)))
    (should (string-match-p "printed" (plist-get result :messages)))))

(defun claude-lib-drive-test--message-advice-count ()
  "Return how many pieces of advice are currently on `message'."
  (let ((n 0))
    (advice-mapc (lambda (&rest _) (setq n (1+ n))) 'message)
    n))

(ert-deftest claude-lib-drive-test-message-advice-is-removed-afterwards ()
  "The `message' advice must not survive the call, on success or on error.
It is added on every drive, so a leak would stack silently and every
later `message' in the daemon would be mirrored into a dead buffer."
  (let ((before (claude-lib-drive-test--message-advice-count)))
    (claude-lib-drive (lambda () (message "one")))
    (should (= (claude-lib-drive-test--message-advice-count) before))
    (claude-lib-drive (lambda () (error "boom")))
    (should (= (claude-lib-drive-test--message-advice-count) before))
    (claude-lib-drive (lambda () (sit-for 30)) :timeout 1)
    (should (= (claude-lib-drive-test--message-advice-count) before))))

(ert-deftest claude-lib-drive-test-error-comes-back-as-data ()
  "A signalled error is reported, not propagated -- the caller still gets a plist."
  (let ((result (claude-lib-drive (lambda () (error "boom")))))
    (should (equal (plist-get result :error) '(error "boom")))
    (should-not (plist-get result :value))))

(ert-deftest claude-lib-drive-test-unconsumed-keys-do-not-leak ()
  "Unconsumed `:keys' are counted and discarded, never left in the command loop."
  (let ((result (claude-lib-drive #'ignore :keys "a b c")))
    (should (= (plist-get result :unconsumed-keys) 3))
    (should (null unread-command-events))
    (should (null (default-value 'unread-command-events)))))

(ert-deftest claude-lib-drive-test-command-runs-through-call-interactively ()
  "`claude-lib-drive-command' exercises the interactive spec, not a bare call."
  (let ((seen nil))
    (defalias 'claude-lib-drive-test--cmd
      (lambda (arg)
        (interactive (list 'from-interactive-spec))
        (setq seen arg)
        (switch-to-buffer (claude-lib-drive-test--fresh-buffer "*cldt-cmd*"))))
    (let ((result (claude-lib-drive-command 'claude-lib-drive-test--cmd)))
      (should (eq seen 'from-interactive-spec))
      (should (equal (plist-get result :window-buffer) "*cldt-cmd*")))))

(ert-deftest claude-lib-drive-test-dead-window-is-rejected ()
  (let ((window (split-window (selected-window))))
    (delete-window window)
    (should-error (claude-lib-drive #'ignore :window window) :type 'user-error)))

;; ============================================================================
;; AC1b -- the selected-window trap
;; ============================================================================

(ert-deftest claude-lib-drive-test-input-lands-in-the-pinned-window-not-the-selected-one ()
  "Simulated keys must follow `:window', not whatever window is selected.
The direct regression for the verified `with-temp-buffer' +
`execute-kbd-macro' failure: the typed characters landed in the SELECTED
window's buffer and `current-buffer' was left pointing there."
  (let* ((decoy (claude-lib-drive-test--fresh-buffer "*cldt-decoy*"))
         (target (claude-lib-drive-test--fresh-buffer "*cldt-target*"))
         (decoy-window (selected-window))
         (target-window (split-window decoy-window)))
    (unwind-protect
        (progn
          (set-window-buffer decoy-window decoy)
          (set-window-buffer target-window target)
          (select-window decoy-window)
          (let ((result (claude-lib-drive
                         (lambda () (execute-kbd-macro (kbd "h i")))
                         :window target-window)))
            (should (equal (with-current-buffer target (buffer-string)) "hi"))
            (should (equal (with-current-buffer decoy (buffer-string)) ""))
            (should (equal (plist-get result :window-buffer) "*cldt-target*"))
            (should (equal (plist-get result :current-buffer) "*cldt-target*"))))
      (when (window-live-p target-window) (delete-window target-window)))))

(ert-deftest claude-lib-drive-test-restores-the-layout-and-the-selected-window ()
  "The caller's selected window and the frame's layout come back unchanged."
  (let* ((resident (claude-lib-drive-test--fresh-buffer "*cldt-resident*"))
         (window (selected-window)))
    (set-window-buffer window resident)
    (claude-lib-drive-command
     (lambda () (interactive) (switch-to-buffer (claude-lib-drive-test--fresh-buffer "*cldt-moved*")))
     :window window)
    (should (eq (selected-window) window))
    (should (equal (buffer-name (window-buffer window)) "*cldt-resident*"))))

(ert-deftest claude-lib-drive-test-reports-a-deleted-pinned-window ()
  "A command that deletes the pinned window yields nil, not a dereference error."
  (let* ((window (split-window (selected-window)))
         (result (claude-lib-drive (lambda () (delete-window window)) :window window)))
    (should-not (plist-get result :window-buffer))
    (should-not (plist-get result :error))))

;; ============================================================================
;; AC1c -- the timeout backstop (the minibuffer layer is in the live suite)
;; ============================================================================

(ert-deftest claude-lib-drive-test-timeout-backstop-returns ()
  "A form blocking OUTSIDE the minibuffer is cut off and reported.
Independently falsifiable from the `minibuffer-setup-hook' layer, so
neither can silently mask the other."
  (let* ((start (float-time))
         (result (claude-lib-drive (lambda () (sit-for 30)) :timeout 1)))
    (should (eq (plist-get result :error) 'timeout))
    (should (< (- (float-time) start) 15))))

;; ============================================================================
;; AC3 -- reload, and what it does not undo
;; ============================================================================

(ert-deftest claude-lib-drive-test-reload-redefines-defuns ()
  (claude-lib-drive-test--with-fixture file
      ";;; -*- lexical-binding: t -*-\n(defun claude-lib-drive-test--f () 1)\n"
    (load-file file)
    (should (= (claude-lib-drive-test--f) 1))
    (with-temp-file file
      (insert ";;; -*- lexical-binding: t -*-\n(defun claude-lib-drive-test--f () 2)\n"))
    (let ((report (claude-lib-reload file)))
      (should (plist-get report :loaded))
      (should (= (claude-lib-drive-test--f) 2)))))

(ert-deftest claude-lib-drive-test-reload-reports-what-it-does-not-reset ()
  (claude-lib-drive-test--with-fixture file
      (concat ";;; -*- lexical-binding: t -*-\n"
              "(defvar claude-lib-drive-test--v 1 \"doc\")\n"
              "(defcustom claude-lib-drive-test--c 2 \"doc\" :type 'integer)\n"
              "(defun claude-lib-drive-test--g () 3)\n"
              "(add-hook 'claude-lib-drive-test--hook #'claude-lib-drive-test--g)\n"
              "(advice-add 'claude-lib-drive-test--g :around #'ignore)\n"
              "(define-key global-map (kbd \"C-c C-x C-z\") #'claude-lib-drive-test--g)\n")
    (let ((report (claude-lib-reload file)))
      (unwind-protect
          (progn
            (should (memq 'claude-lib-drive-test--v (plist-get report :not-reset)))
            (should (memq 'claude-lib-drive-test--c (plist-get report :not-reset)))
            (should (= (plist-get report :add-hook) 1))
            (should (= (plist-get report :advice) 1))
            (should (= (plist-get report :keymap) 1))
            (should (plist-get report :restart-recommended)))
        (advice-remove 'claude-lib-drive-test--g #'ignore)
        (define-key global-map (kbd "C-c C-x C-z") nil)))))

(ert-deftest claude-lib-drive-test-reload-clean-file-recommends-nothing ()
  (claude-lib-drive-test--with-fixture file
      ";;; -*- lexical-binding: t -*-\n(defun claude-lib-drive-test--h () 1)\n(defun claude-lib-drive-test--i () 2)\n"
    (let ((report (claude-lib-reload file)))
      (should-not (plist-get report :restart-recommended))
      (should-not (plist-get report :not-reset))
      (should (= (plist-get report :add-hook) 0)))))

(ert-deftest claude-lib-drive-test-reload-counts-nested-stacking-forms ()
  "A hook added inside `with-eval-after-load' stacks just the same, so it counts."
  (claude-lib-drive-test--with-fixture file
      ";;; -*- lexical-binding: t -*-\n(with-eval-after-load 'seq (add-hook 'claude-lib-drive-test--hook2 #'ignore))\n"
    (let ((report (claude-lib-reload file)))
      (should (= (plist-get report :add-hook) 1))
      (should (plist-get report :restart-recommended)))))

(ert-deftest claude-lib-drive-test-reload-limits-are-documented ()
  "The module must state, in prose, what `load-file' does not undo."
  (let ((text (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "modules/claude-lib-drive.el" claude-lib-drive-test--repo-root))
                (buffer-string))))
    (dolist (needle '("defvar" "defcustom" "defface" "add-hook" "advice-add"
                      "claude-scratch.sh restart"))
      (should (string-match-p (regexp-quote needle) text)))))

;; ============================================================================
;; AC4 -- the `-Q' gate
;; ============================================================================

(ert-deftest claude-lib-drive-test-check-q-passes-a-clean-file ()
  (claude-lib-drive-test--with-fixture file
      ";;; -*- lexical-binding: t -*-\n(defun claude-lib-drive-test--clean () \"Return 1.\" 1)\n(provide 'cldt-clean)\n"
    (let ((report (claude-lib-check-q file)))
      (should (plist-get report :ok))
      (should (plist-get report :compiled))
      (should (plist-get report :loaded))
      (should-not (plist-get report :missing-features)))))

(ert-deftest claude-lib-drive-test-check-q-catches-a-hard-require ()
  "A top-level hard `require' of an absent feature fails, and is NAMED."
  (claude-lib-drive-test--with-fixture file
      ";;; -*- lexical-binding: t -*-\n(require 'cldt-absent-feature)\n(defun claude-lib-drive-test--hard () 1)\n"
    (let ((report (claude-lib-check-q file)))
      (should-not (plist-get report :ok))
      (should-not (plist-get report :loaded))
      (should (memq 'cldt-absent-feature (plist-get report :missing-features))))))

(ert-deftest claude-lib-drive-test-check-q-passes-a-soft-dependency-file ()
  "The same dependency declared SOFT passes -- the posture the gate protects."
  (claude-lib-drive-test--with-fixture file
      (concat ";;; -*- lexical-binding: t -*-\n"
              "(require 'cldt-absent-feature nil t)\n"
              "(declare-function cldt-absent-fn \"cldt-absent-feature\")\n"
              "(defun claude-lib-drive-test--soft () \"Return 1.\" 1)\n")
    (let ((report (claude-lib-check-q file)))
      (should (plist-get report :ok))
      (should (plist-get report :loaded))
      (should-not (plist-get report :missing-features)))))

(ert-deftest claude-lib-drive-test-check-q-scrubs-emacsloadpath ()
  "The child must not inherit EMACSLOADPATH, or the whole gate is decorative.
With it set, the child gets the parent's fully-loaded path and a hard
`require' passes -- exactly the miss the gate exists to prevent."
  (let ((dep-dir (make-temp-file "claude-lib-drive-test-dep" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "cldt-planted.el" dep-dir)
            (insert ";;; -*- lexical-binding: t -*-\n(provide 'cldt-planted)\n"))
          (claude-lib-drive-test--with-fixture file
              ";;; -*- lexical-binding: t -*-\n(require 'cldt-planted)\n(defun claude-lib-drive-test--planted () 1)\n"
            ;; Sanity: with the directory handed to the child explicitly it
            ;; DOES load, so the failure below is the scrub and not a typo.
            (should (plist-get (claude-lib-check-q file :load-path (list dep-dir)) :ok))
            (let* ((process-environment
                    (cons (concat "EMACSLOADPATH=" dep-dir ":") process-environment))
                   (report (claude-lib-check-q file)))
              (should-not (plist-get report :ok))
              (should (memq 'cldt-planted (plist-get report :missing-features))))))
      (ignore-errors (delete-directory dep-dir t)))))

(ert-deftest claude-lib-drive-test-check-q-timeout-is-a-failure ()
  "A child that outlives the deadline is a FAILED check, never a silent pass."
  (claude-lib-drive-test--with-fixture file
      ;; `eval-when-compile' hangs the byte-compile child and the bare
      ;; `sleep-for' hangs the load child, so BOTH deadlines are exercised.
      (concat ";;; -*- lexical-binding: t -*-\n"
              "(eval-when-compile (sleep-for 30))\n"
              "(sleep-for 30)\n"
              "(defun claude-lib-drive-test--slow () 1)\n")
    (let ((report (claude-lib-check-q file :timeout 2)))
      (should-not (plist-get report :ok))
      (should-not (plist-get report :compiled))
      (should-not (plist-get report :loaded)))))

(ert-deftest claude-lib-drive-test-check-q-leaves-no-elc-beside-the-source ()
  (claude-lib-drive-test--with-fixture file
      ";;; -*- lexical-binding: t -*-\n(defun claude-lib-drive-test--noelc () \"Return 1.\" 1)\n"
    (claude-lib-check-q file)
    (should-not (file-exists-p (concat file "c")))))

(ert-deftest claude-lib-drive-test-check-q-strict-fails-on-a-warning ()
  (claude-lib-drive-test--with-fixture file
      ";;; -*- lexical-binding: t -*-\n(defun claude-lib-drive-test--warned () \"Return nil.\" (let ((unused 1)) nil))\n"
    (let ((lax (claude-lib-check-q file))
          (strict (claude-lib-check-q file :strict t)))
      (should (plist-get lax :ok))
      (should (plist-get lax :warnings))
      (should-not (plist-get strict :ok)))))

;; ============================================================================
;; AC5 -- the conventions land where phase 7 carries them
;; ============================================================================

(defvar claude-lib-drive-test--skill
  (expand-file-name "claude-plugin/skills/emacs-usage/SKILL.md"
                    claude-lib-drive-test--repo-root)
  "The phase-7 skill document this phase seeds with phase-6 conventions.")

(defun claude-lib-drive-test--skill-text ()
  "Return the skill document's text."
  (with-temp-buffer
    (insert-file-contents claude-lib-drive-test--skill)
    (buffer-string)))

(ert-deftest claude-lib-drive-test-skill-documents-the-conventions ()
  (let ((text (claude-lib-drive-test--skill-text)))
    (should (string-match-p "\\`---\n" text))
    (should (string-match-p "^name: " text))
    (should (string-match-p "^description: " text))
    (dolist (needle '("unread-command-events" "completing-read-function"
                      "select-window" "claude-scratch.sh" "-Q"))
      (should (string-match-p (regexp-quote needle) text)))))

(ert-deftest claude-lib-drive-test-skill-names-only-live-symbols ()
  "Every `claude-lib-' symbol the skill names must actually exist.
Keeps the static entry point honest without letting it restate
docstrings, which would go stale invisibly."
  (let ((text (claude-lib-drive-test--skill-text))
        (start 0) (named nil))
    (while (string-match "\\_<\\(claude-lib-[a-z-]*[a-z]\\)\\_>" text start)
      (push (intern (match-string 1 text)) named)
      (setq start (match-end 0)))
    (setq named (delete-dups named))
    (should named)
    (dolist (symbol named)
      (should (or (fboundp symbol) (boundp symbol)
                  ;; File basenames (`claude-lib-drive.el') read as symbols too.
                  (file-exists-p (expand-file-name
                                  (format "modules/%s.el" symbol)
                                  claude-lib-drive-test--repo-root)))))))

(ert-deftest claude-lib-drive-test-skill-carries-no-plugin-manifest ()
  "Phase 7 owns the manifest and the injection; this phase seeds content only."
  (should-not (file-exists-p (expand-file-name "claude-plugin/.claude-plugin/plugin.json"
                                               claude-lib-drive-test--repo-root))))

;; ============================================================================
;; The scratch script's guards, as plain text -- lifecycle is in the live suite
;; ============================================================================

(ert-deftest claude-lib-drive-test-scratch-script-refuses-the-live-server-name ()
  (let ((text (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "scripts/claude-scratch.sh" claude-lib-drive-test--repo-root))
                (buffer-string))))
    (should (string-match-p "SERVER\\\" == \\\"server\\\"" text))
    ;; Never `--init-directory': that is how a second straight tree gets
    ;; bootstrapped and the main checkout's build directory poisoned.
    (should-not (string-match-p "--init-directory=" text))))

(provide 'claude-lib-drive-test)
;;; claude-lib-drive-test.el ends here
