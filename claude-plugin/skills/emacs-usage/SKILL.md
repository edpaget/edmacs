---
name: emacs-usage
description: Conventions for driving a live Emacs from the eval channel — how to exercise interactive surfaces (pickers, transients, compose buffers, keymaps) without wedging the daemon, which daemon to target, how to reload, and the bare `-Q` gate that catches a hard `require` the daemon hides.
---

<!-- Content only. Phase 7 owns the plugin manifest (`.claude-plugin/plugin.json`)
     and the `--plugin-dir` injection; this file is not half-wired, it is the
     conventions phase 6 settled, parked where phase 7 will assemble them. -->

# Driving Emacs interaction

The eval channel itself is `modules/claude-lib.el` (write forms to a file, run
`emacsclient -s <daemon> -e '(edmacs-claude-lib-eval-file "<form>" "<out>" "<root>")'`,
read the output file). Everything below is about the half of Emacs that channel
cannot touch naively.

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
