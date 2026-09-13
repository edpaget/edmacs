;;; claude-agent.el --- ACP-backed Claude sessions via agent-shell -*- lexical-binding: t -*-

;; Copyright (C) 2026 Edward Paget
;; Author: Edward Paget <ed.paget@gmail.com>
;; Keywords: claude acp agent

;; This file is part of edmacs.

;;; Commentary:

;; Starts a Claude Code session over the Agent Client Protocol, rendered by
;; `agent-shell' in an ordinary Emacs buffer, as a second agent surface
;; alongside `modules/claude-term.el's terminal-hosted CLI.  Strictly
;; additive: claude-term is untouched and remains the default.
;;
;; Do NOT hand-roll here: ACP framing and JSON-RPC transport (acp.el),
;; the comint substrate and markdown rendering (shell-maker), or the
;; transcript, diff and permission widgets (agent-shell).  This module is
;; configuration, one project-rooted entry point, and the four gaps phase 1
;; of the edmacs-claude-acp roadmap found between agent-shell's defaults and
;; this config's expectations.  modules/claude-repl/ is the cautionary case:
;; 72 KB of hand-rolled buffer rendering that never handled a streaming
;; delta, since retired to the `archive/claude-repl' tag.
;;
;; ------------------------------------------------------------------
;; KEYBINDING.  This module deliberately opens no `with-eval-after-load
;; 'general' block.  modules/claude-term-registry.el is the sole owner of
;; the `SPC a' prefix and hosts every module's leaf binding under it, so
;; `SPC a c' -> `claude-agent-start' lives in that file's single
;; `general-define-key' form -- the same treatment sidebar-agents.el's
;; `SPC a TAB' gets.  `general-define-key' stores the symbol and resolves
;; it at keypress, so the forward reference is safe even though init.el
;; loads this module after the registry.
;;
;; ------------------------------------------------------------------
;; MCP SERVERS.  agent-shell does not read a project's `.mcp.json'; it
;; sends whatever `agent-shell-mcp-servers' (or an agent config's
;; `:mcp-servers') holds.  `claude-agent--read-mcp-config' closes that gap
;; for the project-scoped file only.  Residual, and deliberately NOT
;; claimed as parity with the CLI:
;;
;;   - only `<root>/.mcp.json' is read.  User-scope servers in
;;     `~/.claude.json' and any `.claude/settings.json'
;;     `enabledMcpjsonServers' gating are ignored outright.
;;   - the CLI's approval prompt for a newly-seen project server has no
;;     equivalent here: every stdio/http/sse entry in the file is sent.
;;   - an entry naming neither a `command' nor an http/sse `url' is
;;     skipped with one warning naming it, as is one that is not a JSON
;;     object at all; a file whose shape is not an object of objects is
;;     ignored whole, with one warning.  `.mcp.json' is not authored here,
;;     so valid-JSON-wrong-shape is a case this module owns rather than a
;;     `wrong-type-argument' out of `SPC a c'.
;;
;; IMAGE PASTE / SCREENSHOTS are unevaluated -- phase 1 time-boxed them out
;; entirely.  Treat as unknown, not working.
;;
;; ------------------------------------------------------------------
;; NO TRANSCRIPTS IN THE REPO.  Claude Code already persists every session
;; to ~/.claude/projects/**/*.jsonl and the ACP adapter resumes from that
;; same store, so agent-shell's own `.agent-shell/transcripts/*.md' would
;; be a second copy written inside the checkout -- and creating any
;; `.agent-shell/' subdirectory makes `agent-shell--ensure-gitignore'
;; append `/.agent-shell/' to `.git/info/exclude', which in a worktree
;; means synthesizing `<main>/.git/worktrees/<name>/info/' from nothing.
;; Two settings close that off: `agent-shell-transcript-file-path-function'
;; is nil (its `:type' carries an explicit "Disabled" nil), and
;; `agent-shell-dot-subdir-function' points the two remaining on-demand
;; writers (screenshots, worktrees) at `claude-agent-data-directory',
;; outside every repo -- which is what actually suppresses the exclude
;; write, since `agent-shell--dot-subdir' only calls the gitignore path
;; when the directory it just created is under the project's own
;; `.agent-shell/'.
;;
;; ------------------------------------------------------------------
;; AGENT-SHELL SETTINGS are applied by plain `setq', NOT by a `:custom'
;; block on the `use-package' form.  use-package parses a declaration as a
;; whole, so wherever `:straight' is unregistered -- a bare `-Q',
;; scripts/claude-scratch.sh's daemon, this module's ERT harness -- the
;; form expands to nothing and every clause on it goes with it, reported
;; only as a warning.  agent-shell then comes up on its defaults and the
;; first session writes into whatever checkout it ran in.
;;
;; ------------------------------------------------------------------
;; HEADLESS START.  agent-shell's session picker is not on the start call
;; at all: `agent-shell-session-strategy' defaults to `prompt', and the
;; `completing-read' fires asynchronously from the `session/list' response
;; callback, long after the start form has returned.  Phase 1 reproduced
;; this repo's minibuffer-wedge hazard by answering that already-open
;; prompt from a separate `emacsclient -e' call.  So the bypass is to keep
;; it from opening: `claude-agent-start' passes `:session-strategy 'new'
;; to `agent-shell--start', the non-interactive constructor, rather than
;; going through `agent-shell-anthropic-start-claude-code'.  Automation
;; recipe, from a THROWAWAY daemon (never the server named `server'):
;;
;;   scripts/claude-scratch.sh start
;;   scripts/claude-scratch.sh eval \
;;     '(load (expand-file-name "modules/claude-agent.el" default-directory) nil t)'
;;   scripts/claude-scratch.sh eval '(claude-agent-start "/abs/path/to/root")'
;;
;; The middle line is not optional: claude-scratch.sh boots the claude-lib
;; family and nothing else.  It runs `emacs -Q' with no straight
;; bootstrap, which is exactly where a `:custom' block would have been
;; dropped -- see "AGENT-SHELL SETTINGS".  `default-directory' there is
;; the CALLING checkout, so the `expand-file-name' picks up the module
;; under test rather than main's copy.
;;
;; and for the interactive variant, `claude-lib-drive-command' from
;; modules/claude-lib-drive.el, which cannot block.
;;
;; That recipe is DEMONSTRATED, not merely written down:
;; scripts/claude-agent-headless-check.sh runs exactly it against a real
;; `claude-agent-acp' and asserts the start call returned, `(minibuffer-depth)'
;; was 0 and no minibuffer opened at all, a real `session/new' answered with a
;; session id, and neither `.git/info/exclude' nor the worktree's absent
;; `info/' directory changed.  It is deliberately not a row in
;; scripts/test-manifest.sh: it spawns a real Claude Code process, which is
;; not a cost `scripts/test-all.sh' should carry.  Run it by hand whenever
;; this start path or the pinned agent-shell/acp.el revisions move -- a
;; regression here does not fail an assertion, it hangs, which is the one
;; failure mode a stubbed ERT test cannot reproduce.
;;
;; ------------------------------------------------------------------
;; A DEAD AGENT IS NOT A BACKTRACE.  `claude-agent--ensure-agent' resolves
;; the ACP agent, and the interpreter its shebang names, before any
;; agent-shell entry point is touched, and signals a `user-error' naming
;; the install command for either miss -- so a missing agent
;; fails in this module's own message rather than deep inside acp.el's
;; process filter -- which matters more than usual because the failure
;; would otherwise read as `SPC a c', a key in claude-term's own prefix
;; block, breaking.  An agent that spawns and then dies is already handled
;; upstream on the pinned revisions: acp.el's process sentinel turns it
;; into a synthetic JSON-RPC internal error (`acp--fail-pending-requests'),
;; which agent-shell renders as an in-buffer "Notices" fragment.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'seq)
(require 'json)

;; ============================================================================
;; Packages
;; ============================================================================
;; Pinned in straight/versions/default.el at phase 1's evaluated revisions
;; (agent-shell 7377ba8, acp.el 0f2cac4, shell-maker f448a74).  Six of that
;; phase's ten parity rows are source-derived rather than live, so moving
;; the pins invalidates them rather than merely ageing them.  None of the
;; three is on MELPA under these names, hence the explicit git recipes.

(use-package acp
  :straight (acp :type git :host github :repo "xenodium/acp.el")
  :defer t)

(use-package shell-maker
  :straight (shell-maker :type git :host github :repo "xenodium/shell-maker")
  :defer t)

(use-package agent-shell
  :straight (agent-shell :type git :host github :repo "xenodium/agent-shell")
  :defer t)

;; The two settings that keep a session out of the checkout are applied
;; below, at top level, NOT through a `:custom' block on the form above --
;; see "AGENT-SHELL SETTINGS" in the Commentary for why that distinction
;; is load-bearing rather than stylistic.

;; Referenced from function bodies and from the `:custom' block above;
;; declared so this file byte-compiles and loads under a bare `emacs -Q',
;; where none of the three packages exists.  Same posture as
;; modules/claude-term.el's ghostel-family declarations.
(declare-function evil-define-key* "evil-core")
(defvar agent-shell-transcript-file-path-function)
(defvar agent-shell-show-welcome-message)
(defvar shell-maker-prompt-before-killing-buffer)
(defvar agent-shell-header-style)
(defvar agent-shell-dot-subdir-function)
(defvar agent-shell-cwd-function)
(defvar agent-shell-anthropic-claude-acp-command)
(defvar agent-shell-mode-map)
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-submit "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function agent-shell-anthropic-make-claude-client "agent-shell-anthropic")

;; `evil' and `exec-path-from-shell' are likewise absent under `-Q'.
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-insert-state "evil-states")
(declare-function exec-path-from-shell-copy-env "exec-path-from-shell")

;; ============================================================================
;; Customization
;; ============================================================================

(defgroup claude-agent nil
  "ACP-backed Claude sessions."
  :group 'tools
  :prefix "claude-agent-")

(defcustom claude-agent-data-directory
  (expand-file-name "emacs/agent-shell/"
                    (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache")))
  "Directory agent-shell's on-demand writers are steered into.
Must sit outside every project checkout: a path inside one makes
`agent-shell--dot-subdir' append `/.agent-shell/' to that repo's
`.git/info/exclude' the first time it creates a subdirectory there."
  :type 'directory
  :group 'claude-agent)

(defconst claude-agent-acp-command "claude-agent-acp"
  "Executable name of the Claude Code ACP agent.")

(defconst claude-agent-install-command
  "npm install -g @agentclientprotocol/claude-agent-acp"
  "Install command named in this module's missing-agent message.
Phase 1 of the edmacs-claude-acp roadmap evaluated version 0.76.0.")

;; ============================================================================
;; Executable resolution
;; ============================================================================
;; `node' here is mise-shimmed (~/.local/share/mise/installs/node/24/bin),
;; which is on the login shell's PATH and on no default one -- the same
;; route modules/languages/java.el documents for JAVA_HOME and
;; scripts/go-eglot-check.sh for gopls.  core.el's `exec-path-from-shell'
;; already imports that PATH at startup, so the fallback below is only for
;; a session where it did not run.  Note `global-mise-mode' is wired to
;; `after-init' and is therefore OFF in every batch check: resolution here
;; deliberately depends on the login-shell PATH alone, never on
;; buffer-local mise environment.

(defvar claude-agent--path-reimported nil
  "Non-nil once this session has re-imported PATH from the login shell.
Memoizes `claude-agent--resolve-executable's fallback so a miss costs at
most one shell-out for the whole session.")

(defun claude-agent--resolve-executable (name)
  "Return the absolute path to executable NAME, or nil.
Retries once through `exec-path-from-shell' on a first miss, so a daemon
started by launchd without the login shell's PATH still finds a
mise-shimmed toolchain.  Never signals: callers decide what a nil means."
  (or (executable-find name)
      (progn
        (unless claude-agent--path-reimported
          (setq claude-agent--path-reimported t)
          (when (fboundp 'exec-path-from-shell-copy-env)
            (ignore-errors (exec-path-from-shell-copy-env "PATH"))))
        (executable-find name))))

(defun claude-agent--script-interpreter (path)
  "Return the interpreter named in PATH's `#!' line, or nil.
Nil for a binary, an unreadable file, or a file with no shebang.  A
`#!/usr/bin/env node' line yields \"node\" rather than \"env\", since the
interpreter that has to resolve is the one env goes looking for."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path nil 0 256)
      (goto-char (point-min))
      (when (looking-at "#![ \t]*\\(.*\\)$")
        (let* ((words (split-string (match-string 1) "[ \t]+" t))
               (head (car words)))
          (when head
            (if (equal (file-name-nondirectory head) "env")
                (cadr words)
              head)))))))

(defun claude-agent--ensure-agent ()
  "Return the absolute path to the Claude ACP agent, or signal a `user-error'.
Checked before any agent-shell entry point is touched so a missing
toolchain surfaces as one readable echo-area line rather than a failure
deep inside acp.el's process filter."
  (let ((agent (claude-agent--resolve-executable claude-agent-acp-command)))
    (unless agent
      (user-error "Claude-agent: `%s' not found on PATH.  Install it with: %s"
                  claude-agent-acp-command claude-agent-install-command))
    (let ((interpreter (claude-agent--script-interpreter agent)))
      (when (and interpreter
                 (not (claude-agent--resolve-executable interpreter)))
        (user-error "Claude-agent: `%s' needs `%s', which is not on PATH"
                    claude-agent-acp-command interpreter)))
    agent))

;; ============================================================================
;; Data directory
;; ============================================================================

(defun claude-agent--dot-subdir (subdir)
  "Return the path agent-shell should write SUBDIR under.
Always inside `claude-agent-data-directory', never inside a checkout --
see this file's Commentary on `.git/info/exclude'.  Directory creation is
`agent-shell--dot-subdir's job, not this function's."
  (expand-file-name subdir claude-agent-data-directory))

;; Applied unconditionally at load time, not through `use-package' -- see
;; the Commentary.  This works in both load orders: `defcustom' leaves an
;; already-bound variable alone, so setting them before agent-shell loads
;; survives its declaration, and setting them after it has loaded
;; overwrites the default.  Neither upstream defcustom carries a `:set',
;; so `setq' and `:custom' are equivalent wherever both actually run.
(setq agent-shell-transcript-file-path-function nil)

;; Startup chrome off: the welcome banner is a one-shot splash that costs a
;; screenful on every new session, and the header duplicates what the mode
;; line and the buffer name already say.  The busy indicator stays on -- it
;; reports live state rather than decorating the start.
(setq agent-shell-show-welcome-message nil)
(setq agent-shell-header-style nil)
(setq agent-shell-dot-subdir-function #'claude-agent--dot-subdir)

;; ============================================================================
;; Sessions
;; ============================================================================

(defun claude-agent--project-root ()
  "Return the current project's root directory.
Signals a `user-error' when not inside a project, mirroring
`claude-term--project-root'."
  (let ((proj (project-current)))         ; ambient-reads: ok
    (unless proj
      (user-error "Claude-agent: not inside a project"))
    (project-root proj)))

(defun claude-agent--key (root instance)
  "Return the session key for project ROOT and INSTANCE.
The same `(file-truename ROOT . INSTANCE)' shape
`claude-term-registry--key' uses, so a symlinked path to one worktree
never registers as two sessions and so roadmap phase 3 can correlate a
claude-term row with an ACP one.  ROOT is also slash-terminated, which
`claude-term-registry--key' gets for free from always being handed a
`project-root': without it, the same directory named with and without a
trailing slash keys two ways."
  (cons (file-name-as-directory (file-truename root)) instance))

;; ============================================================================
;; MCP servers
;; ============================================================================

(defun claude-agent--mcp-config-file (root)
  "Return ROOT's `.mcp.json' path."
  (expand-file-name ".mcp.json" root))

(defun claude-agent--read-mcp-config (root)
  "Return ROOT's parsed `.mcp.json', or nil.
Nil for an absent, unreadable or malformed file -- a malformed one also
emits one warning, since a silently ignored MCP config is worse than a
noisy one.  Never signals."
  (let ((file (claude-agent--mcp-config-file root)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            ;; Arrays come back as VECTORS, deliberately.  Parsed as lists
            ;; they are indistinguishable from objects -- an array of
            ;; objects is a list whose every element is a cons, which is
            ;; exactly what an alist is -- and `mcpServers' written as an
            ;; array is a realistic hand-edit.  See
            ;; `claude-agent--json-object-p'.
            (if (fboundp 'json-parse-buffer)
                (json-parse-buffer :object-type 'alist :array-type 'array
                                   :null-object nil :false-object nil)
              (let ((json-object-type 'alist)
                    (json-array-type 'vector))
                (json-read))))
        (error
         (display-warning 'claude-agent
                          (format "Ignoring unparseable %s: %s"
                                  file (error-message-string err))
                          :warning)
         nil)))))

(defun claude-agent--json-object-p (value)
  "Return non-nil when VALUE is a parsed JSON object.
`claude-agent--read-mcp-config' parses objects as alists, `{}' as nil,
and arrays as vectors -- so a vector is never an object here, and \"is it
a list\" is sound for the rest.  The per-element key check is the second
half: it is what rejects an array of objects (`{\"mcpServers\":
[{\"command\":\"c\"}]}', a realistic hand-edit) should a parser ever
hand arrays back as lists, since such a list's elements are whole alists
whose `car' is a cons rather than a key.  Without both halves that file
reaches the translators, which read each server ENTRY as a name, and the
warning names `(command . c)' instead of saying the file is misshapen.
This is the only shape check between a hand-edited `.mcp.json' and an
`alist-get' on an integer."
  (and (listp value)
       (seq-every-p (lambda (element)
                      (and (consp element)
                           (let ((key (car element)))
                             (or (symbolp key) (stringp key)))))
                    value)))

(defun claude-agent--json-array-to-list (value)
  "Return JSON array VALUE as a list, or nil when it is not an array.
Arrays parse as vectors; a list here is an object and not an array."
  (and (vectorp value) (append value nil)))

(defun claude-agent--mcp-key-name (key)
  "Return JSON object KEY as a string, whatever the parser produced."
  (cond ((symbolp key) (symbol-name key))
        ((stringp key) key)
        (t (format "%s" key))))

(defun claude-agent--mcp-name-value-pairs (object)
  "Translate a JSON OBJECT of string values into ACP name/value alists.
`((A . \"1\"))' becomes `(((name . \"A\") (value . \"1\")))', the shape
`agent-shell-mcp-servers' documents for both `env' and `headers'.  A
non-object OBJECT -- `\"env\": true' is the realistic hand-edit -- yields
nil rather than signalling."
  (when (claude-agent--json-object-p object)
    (delq nil
          (mapcar (lambda (pair)
                    (let ((key (car pair))
                          (value (cdr pair)))
                      (when (and key (stringp value))
                        `((name . ,(claude-agent--mcp-key-name key))
                          (value . ,value)))))
                  object))))

(defun claude-agent--mcp-server-from-entry (name entry)
  "Translate `.mcp.json' ENTRY named NAME into one ACP server alist.
Returns nil for an entry this client cannot express: one naming neither a
stdio `command' nor an http/sse `url', and equally one that is not a JSON
object at all (`\"weird\": \"a-string\"' parses fine and is not a server).
Both are skipped by name, not signalled.  The result is the shape
`agent-shell--make-mcp-server' normalizes -- see `agent-shell-mcp-servers'."
  (when (claude-agent--json-object-p entry)
    (let ((type (alist-get 'type entry))
          (command (alist-get 'command entry))
          (url (alist-get 'url entry))
          (args (alist-get 'args entry)))
      (cond
       ((and (member type '("http" "sse")) (stringp url))
        `((name . ,name)
          (type . ,type)
          (url . ,url)
          (headers . ,(claude-agent--mcp-name-value-pairs (alist-get 'headers entry)))))
       ((and (stringp command) (or (null type) (equal type "stdio")))
        `((name . ,name)
          (command . ,command)
          (args . ,(seq-filter #'stringp (claude-agent--json-array-to-list args)))
          (env . ,(claude-agent--mcp-name-value-pairs (alist-get 'env entry)))))))))

(defun claude-agent--mcp-servers-object (config)
  "Return CONFIG's `mcpServers' value when it is a JSON object, else nil."
  (when (claude-agent--json-object-p config)
    (let ((servers (alist-get 'mcpServers config)))
      (and (claude-agent--json-object-p servers) servers))))

(defun claude-agent--mcp-config-problem (config)
  "Return why CONFIG is not a usable `.mcp.json', or nil when it is one.
An absent or empty `mcpServers' is not a problem -- that is a project with
no servers, which is this repo's own case.  A top-level array or scalar,
or an `mcpServers' that is not itself an object, is: each parses as valid
JSON and none of them is a config."
  (cond
   ((not (claude-agent--json-object-p config))
    "its top level is not a JSON object")
   ((let ((servers (alist-get 'mcpServers config)))
      (and servers (not (claude-agent--json-object-p servers))))
    "its `mcpServers' is not a JSON object")))

(defun claude-agent--mcp-entries (config)
  "Return CONFIG's `mcpServers' object as a list of (NAME . ENTRY) conses.
NAME is a string; ENTRY is whatever the file held there, validated by
`claude-agent--mcp-server-from-entry' rather than assumed to be an alist.
Nil whenever CONFIG is not the object-of-objects shape."
  (mapcar (lambda (pair)
            (cons (claude-agent--mcp-key-name (car pair)) (cdr pair)))
          (claude-agent--mcp-servers-object config)))

(defun claude-agent--mcp-servers-from-config (config)
  "Translate CONFIG into the list `agent-shell' takes as `:mcp-servers'.
Entries this client cannot express are dropped; see
`claude-agent--mcp-skipped-names' for their names."
  (delq nil
        (mapcar (lambda (pair)
                  (claude-agent--mcp-server-from-entry (car pair) (cdr pair)))
                (claude-agent--mcp-entries config))))

(defun claude-agent--mcp-skipped-names (config)
  "Return the names of CONFIG's MCP entries this client cannot express."
  (delq nil
        (mapcar (lambda (pair)
                  (unless (claude-agent--mcp-server-from-entry (car pair) (cdr pair))
                    (car pair)))
                (claude-agent--mcp-entries config))))

(defun claude-agent--mcp-servers-for-root (root)
  "Return the ACP MCP server list for project ROOT, warning about skips.
Reads only ROOT's own `.mcp.json' -- a worktree and its main checkout are
separate roots and get separate answers.

NEVER SIGNALS.  `.mcp.json' is an external file this module does not
author, and a hand-edited one can be valid JSON in a shape none of the
translators above expect.  Every such shape degrades to an empty server
list plus one warning naming the file, because the alternative is `SPC a
c' dying with a bare `wrong-type-argument' that names neither the file
nor what is wrong with it.  The `condition-case' is the backstop under
the shape checks, not a substitute for them."
  (let ((file (claude-agent--mcp-config-file root)))
    (condition-case err
        (let ((config (claude-agent--read-mcp-config root)))
          (cond
           ((null config) nil)
           ((claude-agent--mcp-config-problem config)
            (display-warning 'claude-agent
                             (format "Ignoring %s: %s" file
                                     (claude-agent--mcp-config-problem config))
                             :warning)
            nil)
           (t
            (let ((skipped (claude-agent--mcp-skipped-names config)))
              (when skipped
                (display-warning 'claude-agent
                                 (format "Skipping unsupported MCP server(s) in %s: %s"
                                         file (string-join skipped ", "))
                                 :warning)))
            (claude-agent--mcp-servers-from-config config))))
      (error
       (display-warning 'claude-agent
                        (format "Ignoring %s: %s" file (error-message-string err))
                        :warning)
       nil))))

;; ============================================================================
;; Start
;; ============================================================================

(defun claude-agent--make-config (agent mcp-servers)
  "Return agent-shell's Claude config, bound to AGENT and MCP-SERVERS.
AGENT is the absolute path resolved by `claude-agent--ensure-agent'; it
is baked into the config's `:client-maker' closure rather than left to
`agent-shell-anthropic-claude-acp-command', so the spawned process is
never re-resolved against a different `exec-path' (modules/claude-repl/
shipped exactly that bug against bare `python3')."
  (let ((config (agent-shell-anthropic-make-claude-code-config))
        (params (cdr agent-shell-anthropic-claude-acp-command)))
    (setf (alist-get :mcp-servers config) mcp-servers)
    (setf (alist-get :client-maker config)
          (lambda (buffer)
            (let ((agent-shell-anthropic-claude-acp-command (cons agent params)))
              (agent-shell-anthropic-make-claude-client :buffer buffer))))
    config))

(defun claude-agent--configure-buffer (buffer)
  "Apply this module's buffer-local settings to BUFFER.

`shell-maker-prompt-before-killing-buffer' is turned off.  agent-shell
only suppresses that prompt when IT is writing a transcript
\(`agent-shell--transcript-file' non-nil); disabling transcripts -- which
this module does deliberately, since Claude Code already stores every
session under `~/.claude/projects' -- drops into the else branch and
re-enables shell-maker's own save-on-kill query."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local shell-maker-prompt-before-killing-buffer nil))))

(defun claude-agent--configure-evil (buffer)
  "Put BUFFER into evil insert state when evil is on.
`evil-set-initial-state' below is what makes insert state contractual for
every later agent shell; this covers the buffer just created, which
agent-shell brought up before that setting could apply to it."
  (when (and (buffer-live-p buffer)
             (bound-and-true-p evil-mode)
             (fboundp 'evil-insert-state))
    (with-current-buffer buffer
      (evil-insert-state))))

(defvar claude-agent-session-create-functions nil
  "Hook run with (ROOT BUFFER) once an ACP session's buffer exists.
ROOT is the slash-terminated truename the session was started at;
BUFFER is its live `agent-shell-mode' buffer.  A swappable seam in the
same spirit as `claude-term-registry-create-functions', so an adapter
\(modules/claude-agent-agents.el) can mirror a session into another
table without this module knowing that table exists, and without
advising the start path.

Deliberately UNPAIRED -- there is no matching remove hook.  A session
ends in two ways this module cannot observe from here: its buffer is
killed (agent-shell emits its own `clean-up' event from
`kill-buffer-hook') or its agent process dies (only the process
sentinel sees that).  A remove hook fired from this file would cover
neither, so removal is left entirely to whoever registered.

Fired at the very END of `claude-agent--start-at', after
`display-buffer', mirroring where `claude-term-registry-put' fires its
own create hook.")

(defun claude-agent-pop-to-buffer (buffer)
  "Display and select BUFFER, an ACP session's agent shell.
Not `claude-term--pop-to-window': that is claude-term's own pane logic
for a ghostel buffer, and an `agent-shell-mode' buffer is an ordinary
buffer with no pane discipline of its own.  Signals `user-error' rather
than erroring deep inside `pop-to-buffer' when BUFFER is already dead --
a sidebar row can outlive its session by the width of one redraw."
  (unless (buffer-live-p buffer)
    (user-error "Claude-agent: this session's buffer is gone"))
  (pop-to-buffer buffer))

(defun claude-agent--start-at (root &optional strategy)
  "Start an ACP session rooted at ROOT and return its buffer.
STRATEGY is an `agent-shell-session-strategy' value -- `new' (the
default), `latest', or `prompt'.

`new' and `latest' never enter the minibuffer: passing the strategy
explicitly is what keeps agent-shell's \"Start shell (default: New
shell)\" picker from opening at all, rather than racing to answer it
once it has.  `prompt' deliberately does open it, so it is reachable
only from `claude-agent-resume' -- never from a headless caller, where
phase 1 proved answering that picker wedges the driving connection."
  (unless (require 'agent-shell nil t)
    (user-error "Claude-agent: agent-shell is not installed"))
  (let* ((strategy (or strategy 'new))
         (interactive-strategy (eq strategy 'prompt))
         (agent (claude-agent--ensure-agent))
         (root (file-name-as-directory (file-truename root)))
         (config (claude-agent--make-config agent (claude-agent--mcp-servers-for-root root)))
         (buffer (let ((default-directory root)
                       (agent-shell-cwd-function (lambda () root))
                       ;; Belt and braces on the non-prompt strategies: any
                       ;; prompt upstream adds there must fail loudly rather
                       ;; than wedge the daemon for every later client.  The
                       ;; `prompt' strategy IS the picker, so it must not be
                       ;; inhibited -- a human is at the keyboard for it.
                       (inhibit-interaction (if interactive-strategy
                                                inhibit-interaction
                                              t))
                       (enable-recursive-minibuffers nil))
                   (agent-shell--start :config config
                                       :no-focus t
                                       :new-session t
                                       :session-strategy strategy))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; The `let' above is long gone by the time agent-shell asks for a
        ;; cwd again -- `session/new' is sent from an async callback.
        (setq-local agent-shell-cwd-function (lambda () root)))
      (claude-agent--configure-evil buffer)
      (claude-agent--configure-buffer buffer)
      (display-buffer buffer)
      (run-hook-with-args 'claude-agent-session-create-functions root buffer))
    buffer))

;;;###autoload
(defun claude-agent-start (&optional root)
  "Start an ACP-backed Claude session at ROOT, or the current project root.
Returns the session buffer.  Independent of `claude-term': neither
module's registry, buffer names or keys are touched by the other."
  (interactive)
  (claude-agent--start-at (or root (claude-agent--project-root))))

(defun claude-agent-resume (&optional root)
  "Resume an ACP-backed Claude session at ROOT, choosing from past ones.
Opens agent-shell's session picker, which it populates from the agent's
`session/list' -- the same `~/.claude/projects' store the `claude' CLI
resumes from, so a conversation started in `claude-term' (or a bare
terminal) is offered here too, and one started here is offered to
`claude --resume'.

`claude-agent-start' is the always-fresh entry point and never prompts;
this is the only path that opens the picker, and it is interactive-only
for that reason."
  (interactive)
  (claude-agent--start-at (or root (claude-agent--project-root)) 'prompt))

(defun claude-agent--in-input-region-p ()
  "Return non-nil when point is in the agent shell's editable input region."
  (let ((process (get-buffer-process (current-buffer))))
    (and process
         (>= (point) (marker-position (process-mark process))))))

(defun claude-agent-submit ()
  "Submit the agent shell's current input, or do nothing.
Bound to normal-state RET below.  Evil's own `evil-ret' errors with \"End
of buffer\" there -- the input line is the last line -- which phase 1
reproduced; doing nothing outside the input region, and reporting
agent-shell's own \"Busy, please wait\" as a message, are both quieter
than a signal for a key pressed this often."
  (interactive)
  (when (and (derived-mode-p 'agent-shell-mode)
             (fboundp 'agent-shell-submit)
             (claude-agent--in-input-region-p))
    (condition-case err
        (agent-shell-submit)
      ((user-error end-of-buffer beginning-of-buffer)
       (message "%s" (error-message-string err))
       nil))))

;; ============================================================================
;; evil
;; ============================================================================
;; Phase 1 found insert-state-on-entry was accidental -- inherited from
;; evil's `comint-mode' default via shell-maker's derived mode -- and came
;; with a concrete rough edge.  Both halves are made explicit here.  This
;; is a MODE map, not the `SPC a' leader prefix, so it is outside the
;; sole-owner convention that sends the leader binding to
;; modules/claude-term-registry.el.

(with-eval-after-load 'evil
  (evil-set-initial-state 'agent-shell-mode 'insert)
  (with-eval-after-load 'agent-shell
    ;; Normal-state RET goes through `agent-shell-submit', not
    ;; evil-collection's `shell-maker-submit'.  The difference is a gate:
    ;; `agent-shell-submit' refuses with "Busy, please wait" until the ACP
    ;; session is ready, so text typed during startup stays editable
    ;; instead of being committed and rejected.  Insert-state RET is left
    ;; to evil-collection's `repl-newline' -- both agent-shell's README and
    ;; `evil-collection-repl-submit-state' intend newline there, with
    ;; S-RET as the always-newline key.
    (evil-define-key* 'normal agent-shell-mode-map (kbd "RET") #'claude-agent-submit)))

(provide 'claude-agent)
;;; claude-agent.el ends here
