---
name: emacs-usage
description: Drive a live Emacs from the eval channel without wedging the daemon.
---

# Driving Emacs interaction

## Reach Emacs through the eval channel

The channel is `modules/claude-lib.el`, file-in/file-out, no MCP server and no
allowlist beyond Bash's own `permissions.defaultMode`:

1. Write the elisp form(s) to evaluate, verbatim, to a form file.
2. Run:
   ```
   emacsclient -s <daemon-name> -e \
     '(edmacs-claude-lib-eval-file "<form-file>" "<output-file>" "<root>")'
   ```
   Three plain string literals — no shell-quoting hazard even for a form
   containing a quote, backslash, or newline; that hazard lives inside the
   form file's own contents, written by an ordinary file write, never
   assembled on a command line.
3. Read the output file. A non-zero `emacsclient` exit plus `*ERROR*: ...` on
   stderr means a form signaled — there is no separate success/failure
   protocol beyond `emacsclient`'s own.

`ROOT` is required and never inferred from ambient `default-directory` or
`(project-current)` — the daemon's last-touched buffer says nothing reliable
about which project a given call means.

## One sexp per call, not one call per sexp

Every form in the form file is read and evaluated **in order**, not just the
first — write several related forms into one file rather than round-tripping
through `emacsclient` once per sexp. The **last** form's return value is what
the output file's value section reports. Output from `print`/`princ` and from
`message` is captured too, in call order, into the output file's printed
section — so a multi-line result, incidental `message` chatter, and the final
value all come back from one call. For an image, `claude-lib-render-rasterize`
returns a file **path** (never image data — `emacsclient -e` only returns a
value's printed representation), and that path is what to read with an
ordinary file-reading tool.

## Never call an interactive surface directly

A form that reads the minibuffer does not merely hang its own request. Verified:

```
emacsclient -e '(completing-read "pick: " (list "alpha" "beta"))'   => rc=124
emacsclient -e '(list :alive t)'      # a second, independent client => rc=124
emacsclient -e '(kill-emacs)'         # recovery through the channel => never returns
```

The daemon is down for every later client and needs a real `kill -9`. Interactive
surfaces are the ordinary shape of an Emacs package, not a corner case, so this
is the default hazard rather than a rare one.

Route them through `claude-lib-drive` / `claude-lib-drive-command`
(`modules/claude-lib-drive.el`), which cannot block: an unanswerable prompt is
aborted and reported, and anything else that hangs is cut off by a timeout. Read
their docstrings for the argument list — do not restate it here.

## Feed input two ways, and prefer the first

```elisp
;; pre-fed -- drives the REAL completing-read. PREFER THIS.
(let ((unread-command-events (listify-key-sequence (kbd "b e t a RET"))))
  (completing-read "pick: " (list "alpha" "beta")))        ; => "beta"

;; stub -- fast, but bypasses the code path under test
(let ((completing-read-function (lambda (p c &rest _) "beta")))
  (completing-read "pick: " (list "alpha" "beta")))        ; => "beta"
```

The stub proves only that a caller handles a return value. The pre-fed form
exercises the completion machinery itself, which for a consult/embark package
*is* the code under test. Reach for the stub only when the caller, not the
picker, is the point. Always `let`-bind, never `setq`: leftover events otherwise
prefix the operator's next real keystroke.

## Pin the window before feeding input

Simulated keys follow the **selected window**, not the current buffer. Verified:
`execute-kbd-macro` inside `with-temp-buffer` typed into the selected window's
buffer — the returned string was `*scratch*`'s contents with the typed characters
appended, and `current-buffer` was left pointing there afterwards. In a live
daemon that means keys land in whatever the operator was editing.

So `select-window` the target first, and restore the window configuration of
**that window's own frame** — `save-window-excursion` covers only the selected
frame, and this config is multi-frame by design.

## Scratch daemon, never the operator's

Prototyping installs hooks, advice, keymaps and initial evil states into
whatever daemon you reach. Do not reach the live one (server name `server`).

```bash
scripts/claude-scratch.sh start        # per-checkout throwaway daemon
scripts/claude-scratch.sh eval '<FORM>'
scripts/claude-scratch.sh restart
scripts/claude-scratch.sh stop
```

It runs `emacs -Q` and never `--init-directory` (that bootstraps a second
straight package tree and can poison the main checkout's `straight/build`),
takes module sources from the current checkout and packages from the main one,
and refuses the name `server` outright.

## Discover the library, don't restate it

Emacs already indexes itself; do not build or consult a second index:

```elisp
(apropos-internal "^claude-lib-" #'fboundp)      ; list the library
(documentation 'claude-lib-demo)                 ; full docstring
(help-function-arglist 'claude-lib-demo)         ; signature
```

The cheap combined index — name plus first docstring line, for the whole
library — is one form:

```elisp
(mapcar (lambda (s) (cons s (car (split-string (or (documentation s) "") "\n"))))
        (apropos-internal "^claude-lib-" #'fboundp))
```

**Never `describe-function`.** `(with-output-to-string (describe-function 'foo))`
returns the empty string — `describe-function` renders into a `*Help*` buffer,
not `standard-output`, so it captures nothing back through the channel.
`documentation` plus `help-function-arglist` are the read path; this skill
carries no per-function documentation of its own precisely because the library
answers that question live, and a copy here would go stale the moment a
function changes without the staleness being visible.

## Prefer a subprocess for anything long-running

Elisp evaluates on the same thread the operator is using — a form that runs
long blocks the daemon for every other client, the operator's own keystrokes
included, the same way an unanswered minibuffer prompt does. Shell a
long-running step out via `call-process`/`make-process` and poll or read its
result, rather than doing the work in the evaluated form itself.

## Reload, and what reload does not do

`claude-lib-reload` re-evaluates a file's `defun`s, which covers most iteration.
It does **not** reset `defvar`/`defcustom`/`defface` (they only initialise when
unbound), it double-adds anything the file `add-hook`s, it stacks `advice-add`,
and an already-installed keymap keeps the bindings it was given. It reports what
it could not undo and sets `:restart-recommended`.

The remedy is `scripts/claude-scratch.sh restart`, which is cheap because the
daemon is throwaway. There is no teardown framework and there should not be one:
a long reload-only session accumulates invisible state and eventually a "fix"
that works only because a stale hook is still installed.

## The `-Q` gate is a step in the loop

The daemon has evil, evil-collection, consult, embark, magit and transient all
loaded, so a hard `require` leaking into code whose declared dependency posture
is *soft* is invisible there — it fails later, in the consumer repo's own
`emacs -Q --batch` harness. Run `claude-lib-check-q` on the file before calling a
prototype done; it byte-compiles **and** loads under bare `-Q` (neither
substitutes for the other) and names the offending feature.
