;;; claude-agent-test.el --- Tests for claude-agent.el -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage of modules/claude-agent.el: the `.mcp.json'
;; reader and its translation into agent-shell's server shape, login-shell
;; executable resolution, the missing-agent message, the truename session
;; key, and the picker-free start path driven against a stubbed
;; `agent-shell--start'.
;;
;; Run with:
;;   scripts/run-ert-suite.sh 30 emacs -Q --batch -l ert \
;;         -l modules/test-support.el -l modules/claude-agent.el \
;;         -l modules/claude-agent-test.el -f ert-run-tests-batch-and-exit
;;
;; (Loading claude-agent.el under `-Q' prints a benign "Unrecognized
;; keyword: :straight" notice from each of its three `use-package' forms,
;; since straight.el is not bootstrapped in this bare batch harness.  The
;; notice is caught internally by use-package and aborts nothing -- but it
;; does mean each of those forms expands to NOTHING here, every clause on
;; it included.  That is exactly why the two transcript/dot-subdir
;; settings live outside them, and it is what makes this harness able to
;; assert their live values rather than only grep the source for them: if
;; either one ever moves back into a `:custom' block, the assertion below
;; fails here long before a real session writes into a checkout.)
;;
;; NO live row.  There is deliberately no claude-agent-live-test.el in
;; scripts/test-manifest.sh: a real ACP session spawns a Node agent and
;; talks to the Anthropic API, and neither network nor subscription cost
;; belongs in the pre-landing gate.
;;
;; The `SPC a c' keybinding assertion is NOT here -- the binding lives in
;; modules/claude-term-registry.el (SPC a's sole owner), so it is asserted
;; in claude-term-registry-test.el, whose manifest row already loads
;; claude-term.el, claude-term-registry.el and the real evil/general.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'warnings)

;; `executable-find' and `file-readable-p' are C subrs this file `cl-letf's;
;; without this guard each redirected subr makes Emacs build a native
;; trampoline via a synchronous compiler subprocess (~28s, almost entirely
;; wall clock). See .claude/CLAUDE.md's Testing section and windows-test.el's
;; precedent.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defconst claude-agent-test--module
  (expand-file-name "claude-agent.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the module under test, sibling to this file.")

(defconst claude-agent-test--repo-root
  (file-name-directory (directory-file-name
                        (file-name-directory claude-agent-test--module)))
  "The checkout this suite is running out of -- modules/'s parent.")

;; agent-shell is absent under `-Q'.  Standing in for it as a FEATURE (so
;; `claude-agent--start-at's `(require 'agent-shell nil t)' succeeds) plus
;; a real keymap is enough for every assertion below; the functions
;; themselves are stubbed per-test.
(provide 'agent-shell)
(defvar agent-shell-mode-map (make-sparse-keymap))
(defvar agent-shell-anthropic-claude-acp-command '("claude-agent-acp"))

;; Declared, not defined: claude-agent.el owns both, and is loaded before
;; this file.  Naming them here keeps the `let' bindings below dynamic
;; even when this file is byte-compiled on its own.
(defvar claude-agent--path-reimported)
(defvar claude-agent-acp-command)

;; agent-shell's own, set by claude-agent.el at load time and asserted
;; below.  Declared without a value so this assertion reads what the
;; module actually did rather than a value this file supplied.
(defvar agent-shell-transcript-file-path-function)
(defvar agent-shell-dot-subdir-function)

(defun claude-agent-test--write (dir name text)
  "Write TEXT to NAME under DIR and return the directory."
  (with-temp-file (expand-file-name name dir)
    (insert text))
  dir)

(defmacro claude-agent-test--with-root (var &rest body)
  "Bind VAR to a fresh temporary project root and run BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "claude-agent-root" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

;; ============================================================================
;; .mcp.json
;; ============================================================================

(ert-deftest claude-agent-test-mcp-absent-file-returns-nil ()
  "An absent `.mcp.json' is the DEFAULT case -- this repo has none."
  (claude-agent-test--with-root root
    (should-not (claude-agent--read-mcp-config root))
    (should-not (claude-agent--mcp-servers-for-root root))))

(ert-deftest claude-agent-test-mcp-malformed-json-returns-nil ()
  "Malformed JSON degrades to nil, never a backtrace at session start."
  (claude-agent-test--with-root root
    (claude-agent-test--write root ".mcp.json" "{ not json at all ")
    (let ((warning-minimum-level :emergency))
      (should-not (claude-agent--read-mcp-config root))
      (should-not (claude-agent--mcp-servers-for-root root)))))

(ert-deftest claude-agent-test-mcp-stdio-server-translates ()
  "A stdio entry becomes agent-shell's (name command args env) alist.
`env' becomes the name/value pair list `agent-shell-mcp-servers'
documents, not the raw JSON object."
  (claude-agent-test--with-root root
    (claude-agent-test--write
     root ".mcp.json"
     "{\"mcpServers\":{\"fs\":{\"command\":\"npx\",\"args\":[\"-y\",\"srv\"],\"env\":{\"A\":\"1\"}}}}")
    (let ((servers (claude-agent--mcp-servers-for-root root)))
      (should (= 1 (length servers)))
      (let ((server (car servers)))
        (should (equal "fs" (alist-get 'name server)))
        (should (equal "npx" (alist-get 'command server)))
        (should (equal '("-y" "srv") (alist-get 'args server)))
        (should (equal '(((name . "A") (value . "1"))) (alist-get 'env server)))
        (should-not (alist-get 'url server))))))

(ert-deftest claude-agent-test-mcp-http-server-translates ()
  "An http entry takes the url/headers schema, not the stdio one."
  (claude-agent-test--with-root root
    (claude-agent-test--write
     root ".mcp.json"
     "{\"mcpServers\":{\"n\":{\"type\":\"http\",\"url\":\"https://e/mcp\",\"headers\":{\"H\":\"v\"}}}}")
    (let ((server (car (claude-agent--mcp-servers-for-root root))))
      (should (equal "n" (alist-get 'name server)))
      (should (equal "http" (alist-get 'type server)))
      (should (equal "https://e/mcp" (alist-get 'url server)))
      (should (equal '(((name . "H") (value . "v"))) (alist-get 'headers server)))
      (should-not (alist-get 'command server)))))

(ert-deftest claude-agent-test-mcp-sse-server-translates ()
  "An sse entry takes the same url/headers schema as http."
  (claude-agent-test--with-root root
    (claude-agent-test--write
     root ".mcp.json"
     "{\"mcpServers\":{\"s\":{\"type\":\"sse\",\"url\":\"https://e/sse\"}}}")
    (let ((server (car (claude-agent--mcp-servers-for-root root))))
      (should (equal "sse" (alist-get 'type server)))
      (should (equal "https://e/sse" (alist-get 'url server)))
      (should (equal '() (alist-get 'headers server))))))

(ert-deftest claude-agent-test-mcp-unsupported-entry-is-skipped-and-named ()
  "An entry naming neither a command nor an http/sse url is dropped.
It is dropped by NAME, not silently: `claude-agent--mcp-skipped-names'
is what the caller's single warning reports."
  (claude-agent-test--with-root root
    (claude-agent-test--write
     root ".mcp.json"
     "{\"mcpServers\":{\"ok\":{\"command\":\"c\"},\"weird\":{\"type\":\"carrier-pigeon\"}}}")
    (let* ((config (claude-agent--read-mcp-config root))
           (warning-minimum-level :emergency))
      (should (equal '("weird") (claude-agent--mcp-skipped-names config)))
      (should (equal '("ok") (mapcar (lambda (s) (alist-get 'name s))
                                     (claude-agent--mcp-servers-from-config config)))))))

(ert-deftest claude-agent-test-mcp-non-object-entry-is-skipped-not-signalled ()
  "A server entry that is a JSON scalar is valid JSON and not a server.
`{\"weird\": \"just-a-string\"}' used to reach `alist-get' on a string and
take `SPC a c' down with a raw `wrong-type-argument'."
  (claude-agent-test--with-root root
    (claude-agent-test--write
     root ".mcp.json"
     "{\"mcpServers\":{\"ok\":{\"command\":\"c\"},\"weird\":\"just-a-string\"}}")
    (let ((warning-minimum-level :emergency))
      (should (equal '("ok") (mapcar (lambda (s) (alist-get 'name s))
                                     (claude-agent--mcp-servers-for-root root))))
      (should (equal '("weird")
                     (claude-agent--mcp-skipped-names
                      (claude-agent--read-mcp-config root)))))))

(ert-deftest claude-agent-test-mcp-wrong-shaped-config-is-ignored-whole ()
  "Valid JSON in a shape that is not a config degrades to nil, with a reason.
A top-level array, a scalar, and an `mcpServers' that is an array all
parse; none of them is an object of server objects."
  (claude-agent-test--with-root root
    (let ((warning-minimum-level :emergency))
      (dolist (text '("{\"mcpServers\": [1,2,3]}" "[1,2,3]" "\"hello\"" "42"))
        (claude-agent-test--write root ".mcp.json" text)
        (should (claude-agent--mcp-config-problem
                 (claude-agent--read-mcp-config root)))
        (should-not (claude-agent--mcp-servers-for-root root))))))

(ert-deftest claude-agent-test-mcp-servers-as-an-array-of-objects-is-ignored-whole ()
  "An `mcpServers' ARRAY of server objects is rejected as a shape, not parsed.
The array-of-scalars case above is caught by any \"every element is a
cons\" check; this one is not.  Parsed with arrays as lists, an array of
objects IS an alist of alists, so it reaches the translators, which then
read each server object as a NAME -- the user gets `Skipping unsupported
MCP server(s): (command . c)', which names neither the real problem nor a
fix.  Both halves of `claude-agent--json-object-p' exist for this file."
  (claude-agent-test--with-root root
    (let ((warning-minimum-level :emergency)
          (text "{\"mcpServers\":[{\"command\":\"c\"},{\"command\":\"d\"}]}"))
      (claude-agent-test--write root ".mcp.json" text)
      (let ((config (claude-agent--read-mcp-config root)))
        (should config)
        (should (equal "its `mcpServers' is not a JSON object"
                       (claude-agent--mcp-config-problem config)))
        ;; No garbled name ever reaches a warning: the file is rejected
        ;; before the per-entry skip path runs at all.
        (should-not (claude-agent--mcp-entries config))
        (should-not (claude-agent--mcp-skipped-names config)))
      (should-not (claude-agent--mcp-servers-for-root root)))))

(ert-deftest claude-agent-test-mcp-json-arrays-parse-as-vectors ()
  "The parser setting the shape checks rest on, asserted directly.
`claude-agent--json-object-p' is only sound because an array is a vector
here; flipping `:array-type' back to `list' would make every array of
objects read as an object again."
  (claude-agent-test--with-root root
    (claude-agent-test--write
     root ".mcp.json" "{\"mcpServers\":{\"x\":{\"command\":\"c\",\"args\":[\"a\"]}}}")
    (let* ((config (claude-agent--read-mcp-config root))
           (entry (cdr (car (claude-agent--mcp-entries config)))))
      (should (vectorp (alist-get 'args entry)))
      (should-not (claude-agent--json-object-p (alist-get 'args entry)))
      (should (claude-agent--json-object-p entry))
      ;; ...and the vector still reaches agent-shell as the list its own
      ;; normalizer expects.
      (should (equal '("a")
                     (alist-get 'args (car (claude-agent--mcp-servers-from-config config))))))))

(ert-deftest claude-agent-test-mcp-scalar-env-and-args-degrade ()
  "Per-field scalars where an object or array belongs are dropped, not fatal.
`\"env\": true' and `\"args\": 7' are the same hand-edit one level down."
  (claude-agent-test--with-root root
    (claude-agent-test--write
     root ".mcp.json"
     "{\"mcpServers\":{\"x\":{\"command\":\"c\",\"env\":true,\"args\":7}}}")
    (let ((server (car (claude-agent--mcp-servers-for-root root))))
      (should (equal "c" (alist-get 'command server)))
      (should-not (alist-get 'env server))
      (should-not (alist-get 'args server)))))

(ert-deftest claude-agent-test-mcp-empty-server-object-is-not-a-problem ()
  "An explicitly empty `mcpServers' is a project with no servers, not an error."
  (claude-agent-test--with-root root
    (claude-agent-test--write root ".mcp.json" "{\"mcpServers\":{}}")
    (should-not (claude-agent--mcp-config-problem
                 (claude-agent--read-mcp-config root)))
    (should-not (claude-agent--mcp-servers-for-root root))))

(ert-deftest claude-agent-test-mcp-servers-for-root-never-signals ()
  "The contract the start path depends on, asserted against a thrown error.
`claude-agent--start-at' calls this unguarded; if a translator this module
does not own can ever signal through it, `SPC a c' reports a
`wrong-type-argument' naming neither the file nor the fault."
  (claude-agent-test--with-root root
    (claude-agent-test--write root ".mcp.json" "{\"mcpServers\":{\"a\":{\"command\":\"c\"}}}")
    (let ((warning-minimum-level :emergency))
      (cl-letf (((symbol-function 'claude-agent--mcp-servers-from-config)
                 (lambda (_config) (signal 'wrong-type-argument '(listp 1)))))
        (should-not (claude-agent--mcp-servers-for-root root))))))

(ert-deftest claude-agent-test-mcp-is-read-per-root ()
  "Two roots get two answers -- a worktree and its main checkout differ."
  (claude-agent-test--with-root with-config
    (claude-agent-test--with-root without-config
      (claude-agent-test--write
       with-config ".mcp.json" "{\"mcpServers\":{\"a\":{\"command\":\"c\"}}}")
      (should (claude-agent--mcp-servers-for-root with-config))
      (should-not (claude-agent--mcp-servers-for-root without-config)))))

(ert-deftest claude-agent-test-header-records-mcp-scope ()
  "The Commentary records the residual gap rather than claiming parity.
Phase 1 rated MCP the one `degraded' row in its checklist; a header that
stopped naming the user-scope and settings-gating limitations would
overstate what this module does."
  (with-temp-buffer
    (insert-file-contents claude-agent-test--module)
    (goto-char (point-min))
    (should (search-forward "~/.claude.json" nil t))
    (goto-char (point-min))
    (should (search-forward "enabledMcpjsonServers" nil t))
    (goto-char (point-min))
    (should (search-forward "IMAGE PASTE" nil t))))

;; ============================================================================
;; Session key and project root
;; ============================================================================

(ert-deftest claude-agent-test-key-collapses-a-symlinked-root ()
  "Two paths to one root collapse to one key, as in the claude-term registry."
  (claude-agent-test--with-root root
    (let ((link (expand-file-name "link" (temporary-file-directory))))
      (unwind-protect
          (progn
            (when (file-symlink-p link) (delete-file link))
            (make-symbolic-link (directory-file-name root) link t)
            (should (equal (claude-agent--key root nil)
                           (claude-agent--key link nil)))
            (should-not (equal (claude-agent--key root nil)
                               (claude-agent--key root "second"))))
        (when (file-symlink-p link) (delete-file link))))))

(ert-deftest claude-agent-test-project-root-signals-outside-a-project ()
  "No project is a `user-error', not a backtrace out of `SPC a c'."
  (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
    (should-error (claude-agent--project-root) :type 'user-error)))

;; ============================================================================
;; Executable resolution
;; ============================================================================

(ert-deftest claude-agent-test-resolve-executable-hit-needs-no-shell ()
  "A PATH hit returns immediately without shelling out to the login shell."
  (let ((claude-agent--path-reimported nil)
        (shelled 0))
    (cl-letf (((symbol-function 'executable-find) (lambda (_n &optional _r) "/bin/node"))
              ((symbol-function 'exec-path-from-shell-copy-env)
               (lambda (_v) (cl-incf shelled))))
      (should (equal "/bin/node" (claude-agent--resolve-executable "node")))
      (should (= 0 shelled)))))

(ert-deftest claude-agent-test-resolve-executable-retries-login-shell-once ()
  "A first miss re-imports PATH once; a second miss must not re-shell.
The memo is what keeps a missing toolchain from costing a login shell on
every single call."
  (let ((claude-agent--path-reimported nil)
        (shelled 0)
        (found nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_n &optional _r) found))
              ((symbol-function 'exec-path-from-shell-copy-env)
               (lambda (_v) (cl-incf shelled) (setq found "/mise/bin/node"))))
      (should (equal "/mise/bin/node" (claude-agent--resolve-executable "node")))
      (should (= 1 shelled))
      (setq found nil)
      (should-not (claude-agent--resolve-executable "node"))
      (should (= 1 shelled)))))

(ert-deftest claude-agent-test-resolve-executable-final-miss-returns-nil ()
  "The resolver never signals -- callers decide what a miss means."
  (let ((claude-agent--path-reimported nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_n &optional _r) nil))
              ((symbol-function 'exec-path-from-shell-copy-env) #'ignore))
      (should-not (claude-agent--resolve-executable "definitely-not-installed")))))

(ert-deftest claude-agent-test-script-interpreter-reads-the-shebang ()
  "`#!/usr/bin/env node' names node, not env -- env is not what must resolve."
  (claude-agent-test--with-root root
    (claude-agent-test--write root "envscript" "#!/usr/bin/env node\nconsole.log(1)\n")
    (claude-agent-test--write root "direct" "#!/bin/sh\necho hi\n")
    (claude-agent-test--write root "plain" "not a script at all\n")
    (should (equal "node" (claude-agent--script-interpreter
                           (expand-file-name "envscript" root))))
    (should (equal "/bin/sh" (claude-agent--script-interpreter
                              (expand-file-name "direct" root))))
    (should-not (claude-agent--script-interpreter (expand-file-name "plain" root)))
    (should-not (claude-agent--script-interpreter (expand-file-name "absent" root)))))

;; ============================================================================
;; A missing agent is a readable line
;; ============================================================================

(ert-deftest claude-agent-test-missing-agent-signals-user-error ()
  "A missing ACP agent names the install command, and signals no backtrace."
  (let ((claude-agent--path-reimported t))
    (cl-letf (((symbol-function 'executable-find) (lambda (_n &optional _r) nil)))
      (let ((err (should-error (claude-agent--ensure-agent) :type 'user-error)))
        (should (string-match-p "claude-agent-acp" (error-message-string err)))
        (should (string-match-p "@agentclientprotocol/claude-agent-acp"
                                (error-message-string err))))
      ;; Reached through the command, not only the helper: the whole point
      ;; is that `SPC a c' fails in this module's message.
      (cl-letf (((symbol-function 'claude-agent--project-root)
                 (lambda () (expand-file-name "~/"))))
        (should-error (claude-agent-start) :type 'user-error)))))

(ert-deftest claude-agent-test-missing-interpreter-signals-user-error ()
  "An agent whose node interpreter is gone is the other readable failure."
  (claude-agent-test--with-root root
    (let ((agent (expand-file-name "claude-agent-acp" root))
          (claude-agent--path-reimported t))
      (claude-agent-test--write root "claude-agent-acp" "#!/usr/bin/env node\n")
      (cl-letf (((symbol-function 'claude-agent--resolve-executable)
                 (lambda (name) (and (equal name claude-agent-acp-command) agent))))
        (let ((err (should-error (claude-agent--ensure-agent) :type 'user-error)))
          (should (string-match-p "node" (error-message-string err))))))))

;; ============================================================================
;; Filesystem cleanliness
;; ============================================================================

(ert-deftest claude-agent-test-dot-subdir-resolves-outside-every-checkout ()
  "agent-shell's on-demand writers land outside any project root.
This is what suppresses `agent-shell--ensure-gitignore': it appends
`/.agent-shell/' to `.git/info/exclude' only when the directory it just
created is under the project's own `.agent-shell/'."
  (let ((repo claude-agent-test--repo-root))
    (dolist (subdir '("transcripts" "screenshots" "worktrees"))
      (let ((path (claude-agent--dot-subdir subdir)))
        (should (file-name-absolute-p path))
        (should (string-suffix-p subdir path))
        (should-not (file-in-directory-p path repo))
        (should-not (string-match-p "\\.agent-shell" path))))))

(ert-deftest claude-agent-test-module-disables-the-transcript-writer ()
  "Loading the module is what disables the writer, in THIS bare harness.
No straight, so no `:straight' keyword, so every `use-package' form in the
module expanded to nothing -- and the two settings still hold, which is
the whole point of applying them at top level.  Were they in a `:custom'
block, both variables would read as unbound here and a session started
from any straight-less Emacs would write `.agent-shell/transcripts/' into
the checkout it ran in."
  (should (boundp 'agent-shell-transcript-file-path-function))
  (should-not agent-shell-transcript-file-path-function)
  (should (eq #'claude-agent--dot-subdir agent-shell-dot-subdir-function)))

(ert-deftest claude-agent-test-settings-are-not-inside-a-use-package-form ()
  "Structural guard on the test above: no `:custom' clause anywhere.
The live assertion alone would start passing again the moment a future
edit moved the settings into a `use-package' form in an Emacs that DOES
parse `:straight' -- so the shape is asserted too, at the source level."
  (with-temp-buffer
    (insert-file-contents claude-agent-test--module)
    (goto-char (point-min))
    ;; Code, not prose: the Commentary discusses `:custom' at length.
    (should-not (re-search-forward "^[ \t]*:custom\\b" nil t))
    (goto-char (point-min))
    (should (search-forward "(setq agent-shell-transcript-file-path-function nil)" nil t))
    (goto-char (point-min))
    (should (search-forward
             "(setq agent-shell-dot-subdir-function #'claude-agent--dot-subdir)" nil t))))

(ert-deftest claude-agent-test-headless-recipe-loads-the-module ()
  "The documented automation recipe names the load step it needs.
scripts/claude-scratch.sh boots the claude-lib family and nothing else, so
a recipe that goes straight from `start' to `(claude-agent-start)' fails
with a void function -- which is how the recipe shipped once already."
  (with-temp-buffer
    (insert-file-contents claude-agent-test--module)
    (goto-char (point-min))
    (should (search-forward "claude-scratch.sh start" nil t))
    (should (search-forward "modules/claude-agent.el" nil t))
    (should (search-forward "claude-agent-start" nil t))))

(ert-deftest claude-agent-test-headless-recipe-is-demonstrated-not-just-written ()
  "The recipe has a runnable demonstration, and the module names it.
A recipe asserted only in prose (or only in a commit message) is a claim;
scripts/claude-agent-headless-check.sh runs it against a REAL
`claude-agent-acp' and fails on a wedge.  It cannot be a manifest row --
it spawns a live Claude Code process -- so this is the link that keeps
the script from being deleted as unreferenced and the Commentary from
outliving it."
  (let ((script (expand-file-name "scripts/claude-agent-headless-check.sh"
                                  claude-agent-test--repo-root)))
    (should (file-exists-p script))
    (should (file-executable-p script))
    (with-temp-buffer
      (insert-file-contents claude-agent-test--module)
      (goto-char (point-min))
      (should (search-forward "scripts/claude-agent-headless-check.sh" nil t)))))

(ert-deftest claude-agent-test-headless-check-is-not-a-manifest-row ()
  "The live demonstration stays out of the pre-landing gate, deliberately.
scripts/test-all.sh runs every manifest row; a row here would spawn a real
Claude Code process on every landing."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "scripts/test-manifest.sh"
                                            claude-agent-test--repo-root))
    (goto-char (point-min))
    (should-not (search-forward "claude-agent-headless-check" nil t))))

(ert-deftest claude-agent-test-module-opens-no-general-block ()
  "`SPC a' is claude-term-registry.el's alone; this module adds no second
`general' block to fight it for the prefix label."
  (with-temp-buffer
    (insert-file-contents claude-agent-test--module)
    (goto-char (point-min))
    (should-not (re-search-forward "with-eval-after-load '?general" nil t))))

;; ============================================================================
;; The picker-free start path
;; ============================================================================

(defvar claude-agent-test--start-args nil
  "Keyword arguments the stubbed `agent-shell--start' last received.")

(defvar claude-agent-test--start-cwd nil
  "`default-directory' the stubbed `agent-shell--start' last saw.")

(defmacro claude-agent-test--with-stubbed-start (&rest body)
  "Run BODY with a stubbed agent-shell start path and a minibuffer counter.
Binds `claude-agent-test--entered-minibuffer' to the number of minibuffer
entries observed while BODY ran."
  (declare (indent 0))
  `(let ((claude-agent-test--start-args nil)
         (claude-agent-test--start-cwd nil)
         (claude-agent-test--entered-minibuffer 0)
         (buffer (get-buffer-create "*claude-agent-test-shell*")))
     (unwind-protect
         (cl-letf* ((minibuffer-setup-hook
                     (list (lambda () (cl-incf claude-agent-test--entered-minibuffer))))
                    ((symbol-function 'claude-agent--resolve-executable)
                     (lambda (_name) "/opt/bin/claude-agent-acp"))
                    ((symbol-function 'claude-agent--script-interpreter)
                     (lambda (_path) nil))
                    ((symbol-function 'agent-shell-anthropic-make-claude-code-config)
                     (lambda () (list (cons :identifier 'claude-code)
                                      (cons :client-maker #'ignore)
                                      (cons :mcp-servers nil))))
                    ((symbol-function 'agent-shell-anthropic-make-claude-client)
                     (lambda (&rest _) '((:command . "stub"))))
                    ((symbol-function 'display-buffer) (lambda (b &rest _) b))
                    ((symbol-function 'agent-shell--start)
                     (lambda (&rest args)
                       (setq claude-agent-test--start-args args)
                       (setq claude-agent-test--start-cwd default-directory)
                       buffer)))
           ,@body)
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defvar claude-agent-test--entered-minibuffer 0
  "Minibuffer entries counted inside `claude-agent-test--with-stubbed-start'.")

(ert-deftest claude-agent-test-resume-opens-the-picker ()
  "`claude-agent-resume' asks for the `prompt' strategy.
That is the only path in this module that opens agent-shell's session
picker.  The picker is populated from the agent's `session/list', which
reads the same `~/.claude/projects' store the `claude' CLI resumes from,
so a claude-term-started conversation is offered here too."
  (claude-agent-test--with-root root
    (claude-agent-test--with-stubbed-start
      (should (claude-agent-resume root))
      (should (eq 'prompt (plist-get claude-agent-test--start-args :session-strategy))))))

(ert-deftest claude-agent-test-resume-does-not-inhibit-interaction ()
  "The resume path leaves `inhibit-interaction' alone.
`claude-agent--start-at' binds it to t on every other strategy so a
prompt upstream adds fails loudly instead of wedging the daemon.  The
`prompt' strategy IS a prompt, so inhibiting it there would break the
picker it exists to open."
  (claude-agent-test--with-root root
    (claude-agent-test--with-stubbed-start
      (let ((inhibit-interaction nil)
            (seen 'unset))
        (cl-letf (((symbol-function 'agent-shell--start)
                   (lambda (&rest args)
                     (setq claude-agent-test--start-args args)
                     (setq seen inhibit-interaction)
                     (current-buffer))))
          (claude-agent-resume root)
          (should-not seen))))))

(ert-deftest claude-agent-test-start-still-inhibits-interaction ()
  "The default strategy keeps the headless guarantee.
Sibling of the resume test above: `claude-agent-start' must still bind
`inhibit-interaction' to t, so adding the resume path did not loosen the
property phase 2 was accepted on."
  (claude-agent-test--with-root root
    (claude-agent-test--with-stubbed-start
      (let ((inhibit-interaction nil)
            (seen 'unset))
        (cl-letf (((symbol-function 'agent-shell--start)
                   (lambda (&rest args)
                     (setq claude-agent-test--start-args args)
                     (setq seen inhibit-interaction)
                     (current-buffer))))
          (claude-agent-start root)
          (should (eq t seen)))))))

(defvar corfu--index nil
  "Globally special so the `let' forms below bind DYNAMICALLY.
claude-agent.el declares it with a one-argument `defvar', which is special
only within that file -- enough for the byte-compiler, not enough for a
`let' here to be visible to `bound-and-true-p'.  corfu.el defines it
properly in a real session.")

(ert-deftest claude-agent-test-ret-accepts-a-selected-candidate ()
  "With a candidate selected, RET inserts it and does not fall through."
  (let ((corfu--index 2) (inserted 0) (fell-through 0))
    (cl-letf (((symbol-function 'corfu-insert)
               (lambda () (interactive) (setq inserted (1+ inserted))))
              ((symbol-function 'claude-agent-submit)
               (lambda () (interactive) (setq fell-through (1+ fell-through)))))
      (claude-agent-ret)
      (should (= 1 inserted))
      (should (= 0 fell-through)))))

(ert-deftest claude-agent-test-ret-falls-through-to-the-state-binding ()
  "With nothing selected, RET runs what RET would have run without corfu.
The point of the command: it must not hardcode submit.  Here the
non-corfu binding is `newline', so `newline' is what must run -- the same
code path yields `claude-agent-submit' in normal state, because that is
what the keymaps say there."
  (let ((corfu--index -1) (ran nil))
    (with-temp-buffer
      (use-local-map (let ((m (make-sparse-keymap)))
                       (define-key m (kbd "RET") #'newline) m))
      (cl-letf (((symbol-function 'newline)
                 (lambda (&rest _) (interactive) (setq ran 'newline)))
                ((symbol-function 'claude-agent-submit)
                 (lambda () (interactive) (setq ran 'submit))))
        (claude-agent-ret)
        (should (eq 'newline ran))))))

(ert-deftest claude-agent-test-ret-does-not-consume-the-key ()
  "RET always does something: it never silently swallows the keystroke.
That swallowing is the live bug -- `corfu-insert' at index -1 calls
`corfu-quit' and stops."
  (let ((corfu--index -1) (ran nil))
    (with-temp-buffer
      (use-local-map (let ((m (make-sparse-keymap)))
                       (define-key m (kbd "RET") #'ignore) m))
      (cl-letf (((symbol-function 'ignore)
                 (lambda (&rest _) (interactive) (setq ran t))))
        (claude-agent-ret)
        (should ran)))))

(ert-deftest claude-agent-test-kill-does-not-prompt-to-save ()
  "The buffer opts out of shell-maker's save-on-kill query.
agent-shell only suppresses it when IT writes a transcript; this module
disables transcripts, which re-enables the query unless we turn it off."
  (let ((buffer (generate-new-buffer " *claude-agent-kill-test*")))
    (unwind-protect
        (progn
          (claude-agent--configure-buffer buffer)
          (with-current-buffer buffer
            (should (local-variable-p 'shell-maker-prompt-before-killing-buffer))
            (should-not shell-maker-prompt-before-killing-buffer)))
      (kill-buffer buffer))))

(ert-deftest claude-agent-test-startup-chrome-is-off ()
  "The welcome banner and header are disabled by this module.
Both are one-shot or redundant decoration.  The busy indicator is left
alone deliberately -- it reports live state rather than decorating the
start -- and this asserts that by its absence: loading this module under
`-Q' must leave `agent-shell-show-busy-indicator' unbound, which a
`setq' or `defvar' added here would silently change."
  (should-not agent-shell-show-welcome-message)
  (should-not agent-shell-header-style)
  (should-not (boundp 'agent-shell-show-busy-indicator)))

(ert-deftest claude-agent-test-start-never-opens-the-picker ()
  "The start call asks for a NEW session outright.
agent-shell's `Start shell (default: New shell)' picker is not on the
start call: `agent-shell-session-strategy' defaults to `prompt' and the
`completing-read' fires from the async `session/list' callback, long
after the start form returned.  Overriding the strategy is what keeps it
from opening; phase 1 proved that answering it once open wedges the
driving connection."
  (claude-agent-test--with-root root
    (claude-agent-test--with-stubbed-start
      (should (claude-agent-start root))
      (should (eq 'new (plist-get claude-agent-test--start-args :session-strategy)))
      (should (plist-get claude-agent-test--start-args :new-session))
      (should (plist-get claude-agent-test--start-args :no-focus))
      (should (= 0 claude-agent-test--entered-minibuffer))
      (should (= 0 (minibuffer-depth))))))

(ert-deftest claude-agent-test-start-runs-at-the-truename-project-root ()
  "With no ROOT, the session starts at the current project's root."
  (claude-agent-test--with-root root
    (claude-agent-test--with-stubbed-start
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) (cons 'transient root))))
        (should (claude-agent-start))
        (should (equal (file-name-as-directory (file-truename root))
                       claude-agent-test--start-cwd))))))

(ert-deftest claude-agent-test-start-passes-the-projects-mcp-servers ()
  "`.mcp.json' reaches the agent config, not the global defcustom.
Going through the config's `:mcp-servers' (which
`agent-shell--mcp-servers' prefers) is what keeps two projects' sessions
from cross-contaminating through one global list."
  (claude-agent-test--with-root root
    (claude-agent-test--write
     root ".mcp.json" "{\"mcpServers\":{\"fs\":{\"command\":\"npx\"}}}")
    (claude-agent-test--with-stubbed-start
      (claude-agent-start root)
      (let ((config (plist-get claude-agent-test--start-args :config)))
        (should (equal '("fs") (mapcar (lambda (s) (alist-get 'name s))
                                       (alist-get :mcp-servers config))))))))

(ert-deftest claude-agent-test-start-bakes-the-resolved-agent-path-in ()
  "The client is built from the ABSOLUTE agent path, never a bare name.
modules/claude-repl/ shipped exactly this bug: it resolved `python3' at
spawn time, against whatever `exec-path' the spawning buffer had."
  (claude-agent-test--with-root root
    (claude-agent-test--with-stubbed-start
      (claude-agent-start root)
      (let* ((config (plist-get claude-agent-test--start-args :config))
             (maker (alist-get :client-maker config))
             (seen nil))
        (should (functionp maker))
        (cl-letf (((symbol-function 'agent-shell-anthropic-make-claude-client)
                   (lambda (&rest _)
                     (setq seen (car agent-shell-anthropic-claude-acp-command))
                     nil)))
          (funcall maker (current-buffer)))
        (should (equal "/opt/bin/claude-agent-acp" seen))
        ;; ... and the global defcustom was left alone.
        (should (equal '("claude-agent-acp") agent-shell-anthropic-claude-acp-command))))))

(ert-deftest claude-agent-test-start-fires-the-create-hook ()
  "`claude-agent-session-create-functions' fires exactly once, with the
truename'd slash-terminated ROOT and the live session BUFFER.
The seam modules/claude-agent-agents.el registers a row on: an adapter
must never have to advise the start path to learn a session exists."
  (claude-agent-test--with-root root
    (claude-agent-test--with-stubbed-start
      (let ((calls nil))
        (let ((claude-agent-session-create-functions
               (list (lambda (r b) (push (cons r b) calls)))))
          (let ((buffer (claude-agent-start root)))
            (should (= 1 (length calls)))
            (should (equal (file-name-as-directory (file-truename root))
                           (car (car calls))))
            (should (eq buffer (cdr (car calls))))))))))

(ert-deftest claude-agent-test-pop-to-buffer-rejects-a-dead-buffer ()
  "A row can outlive its session by a redraw, so the jump target must
report that plainly rather than erroring inside `pop-to-buffer'."
  (let ((buffer (generate-new-buffer " *claude-agent-test-dead*")))
    (kill-buffer buffer)
    (should-error (claude-agent-pop-to-buffer buffer) :type 'user-error))
  (let ((buffer (generate-new-buffer " *claude-agent-test-live*")))
    (unwind-protect
        (let ((popped nil))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (b &rest _) (setq popped b))))
            (claude-agent-pop-to-buffer buffer)
            (should (eq buffer popped))))
      (kill-buffer buffer))))

(ert-deftest claude-agent-test-buffer-name-is-not-claude-term-shaped ()
  "An ACP shell must not be mistaken for a claude-term session.
claude-term's registry, session picker and the sidebar's agent rows all
match on `*claude-term:<leaf>[:<instance>]*'; agent-shell's own default
name shares no part of it."
  (let ((regexp "\\`\\*claude-term:\\([^:*]+\\)\\(?::\\([^*]+\\)\\)?\\*\\'"))
    (dolist (name '("Claude Agent @ edmacs" "*Claude Agent @ edmacs*" "*agent*"))
      (should-not (string-match-p regexp name)))
    ;; Control: the shape it must not collide with really does match.
    (should (string-match-p regexp "*claude-term:edmacs*"))))

;; ============================================================================
;; evil
;; ============================================================================
;; Drives the REAL evil, loaded from the straight repos tree (this
;; checkout's, falling back to the sibling main checkout's) -- the same
;; technique claude-term-registry-test.el uses -- rather than asserting
;; against a hand-rolled stand-in for evil's state machinery.

(defconst claude-agent-test--evil-source
  ;; `fboundp'-guarded so this file still LOADS under a bare `-Q' without
  ;; modules/test-support.el (`claude-lib-check-q' runs it that way); the
  ;; manifest row supplies it for real, so the evil tests below do not skip.
  (if-let* (((fboundp 'edmacs-test-support-straight-repos-root))
            (repos (edmacs-test-support-straight-repos-root)))
      (expand-file-name "evil/evil.el" repos)
    "")
  "Path to the real evil.el, when this checkout or its sibling has it.")

(defun claude-agent-test--load-real-evil ()
  "Load the real evil, or return nil so the caller can `ert-skip'."
  (when (file-exists-p claude-agent-test--evil-source)
    (add-to-list 'load-path (file-name-directory claude-agent-test--evil-source))
    (require 'evil)
    t))

(ert-deftest claude-agent-test-evil-initial-state-is-insert-explicitly ()
  "Insert state is declared, not inherited from evil's comint default.
Phase 1 found the inherited behaviour accidental; an upstream change to
`evil-insert-state-modes', or a shell-maker mode reparent, would silently
take it away."
  (unless (claude-agent-test--load-real-evil)
    (ert-skip "real evil.el not found in this checkout or its sibling main checkout"))
  ;; Modern evil records an initial state as membership in the state's own
  ;; `-modes' list, which is what `evil-set-initial-state' writes.
  (should (memq 'agent-shell-mode evil-insert-state-modes)))

(ert-deftest claude-agent-test-normal-state-ret-is-not-evil-ret ()
  "Normal-state RET resolves to this module's wrapper.
Evil's own `evil-ret' errors with \"End of buffer\" on the input line --
the last line of the buffer -- which is phase 1's reproduction."
  (unless (claude-agent-test--load-real-evil)
    (ert-skip "real evil.el not found in this checkout or its sibling main checkout"))
  (let ((aux (evil-get-auxiliary-keymap agent-shell-mode-map 'normal)))
    (should (keymapp aux))
    (should (eq 'claude-agent-submit (lookup-key aux (kbd "RET"))))))

(ert-deftest claude-agent-test-submit-outside-an-agent-shell-does-nothing ()
  "RET elsewhere is a no-op, not a signal."
  (with-temp-buffer
    (should-not (claude-agent-submit))))

(ert-deftest claude-agent-test-submit-outside-the-input-region-does-nothing ()
  "Above the prompt, RET neither submits nor errors."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (cl-letf (((symbol-function 'agent-shell-submit)
               (lambda () (error "must not be reached")))
              ((symbol-function 'claude-agent--in-input-region-p) (lambda () nil)))
      (should-not (claude-agent-submit)))))

(ert-deftest claude-agent-test-submit-reports-rather-than-signals ()
  "agent-shell's own \"Busy, please wait\", and an end-of-buffer, become
messages -- a key pressed this often must not raise."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (cl-letf (((symbol-function 'claude-agent--in-input-region-p) (lambda () t)))
      (cl-letf (((symbol-function 'agent-shell-submit)
                 (lambda () (user-error "Busy, please wait"))))
        (should-not (claude-agent-submit)))
      (cl-letf (((symbol-function 'agent-shell-submit)
                 (lambda () (signal 'end-of-buffer nil))))
        (should-not (claude-agent-submit)))
      (let ((submitted 0))
        (cl-letf (((symbol-function 'agent-shell-submit)
                   (lambda () (cl-incf submitted))))
          (claude-agent-submit)
          (should (= 1 submitted)))))))

;;; claude-agent-test.el ends here
