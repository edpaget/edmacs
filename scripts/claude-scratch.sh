#!/usr/bin/env bash
#
# claude-scratch.sh -- a per-checkout THROWAWAY Emacs daemon for prototyping
# interactive code, so the operator's live daemon is never the target.
#
#   scripts/claude-scratch.sh start|stop|restart|status|name|eval <FORM>
#
# WHY THIS EXISTS.  A form that reads the minibuffer does not merely hang its
# own `emacsclient' request -- it wedges the daemon for EVERY later client, and
# recovery is not available through the eval channel (even `(kill-emacs)' never
# returns; the process needs a real kill).  Interactive surfaces -- pickers,
# transients, compose buffers -- are the ordinary shape of an Emacs package, so
# prototyping them against the daemon the operator works in all day is how that
# daemon dies.  Drive them through `claude-lib-drive' (modules/claude-lib-drive.el),
# and drive them HERE.
#
# It never touches the user's own daemon: the server name `server' is refused
# outright, on every subcommand including the destructive ones.
#
# It never passes `--init-directory'.  Per .claude/CLAUDE.md's Worktrees
# section, that flag makes the given directory a full `user-emacs-directory',
# and `init.el' then bootstraps a SECOND straight package tree there -- which
# can leave the main checkout's `straight/build' full of symlinks into a
# worktree that no longer exists.  This runs `emacs -Q' and adds the main
# checkout's already-built packages to `load-path' from inside the daemon
# instead; nothing is ever written to a package tree, and a missing one is a
# refusal to start, never a bootstrap.
#
# TWO ROOTS, resolved separately, exactly as scripts/startup-check.sh does:
# module sources come from THIS checkout (so a worktree prototypes its own
# code), the package tree from the main checkout (the only populated one).
#
# ENVIRONMENT
#   EDMACS_SCRATCH_NAME      server name; default edmacs-scratch-<checkout>
#   EDMACS_SCRATCH_PACKAGES  `none' for a pure `-Q' daemon with no load-path
#                            additions -- the honest environment for
#                            reproducing a soft-dependency failure by hand.
#
# Restarting is the answer to every limit of `claude-lib-reload' (stale
# `defvar's, double-added hooks, stacked advice).  It is cheap precisely
# because this daemon is throwaway; there is deliberately no teardown
# framework anywhere in this loop.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if [[ "$MODULE_ROOT" == */edmacs__worktrees/* ]]; then
  PACKAGE_ROOT="${MODULE_ROOT%%/edmacs__worktrees/*}/edmacs"
else
  PACKAGE_ROOT="$MODULE_ROOT"
fi

SERVER="${EDMACS_SCRATCH_NAME:-edmacs-scratch-$(basename "$MODULE_ROOT")}"

# The single rule this script exists to enforce, checked before anything is
# spawned OR killed -- `restart' is the subcommand through which a mistyped
# EDMACS_SCRATCH_NAME would actually kill the operator's daemon.
if [[ -z "$SERVER" ]]; then
  echo "claude-scratch: refusing an empty server name." >&2
  exit 2
fi
if [[ "$SERVER" == "server" ]]; then
  echo "claude-scratch: refusing the server name 'server' -- that is the" >&2
  echo "                operator's live daemon. Set EDMACS_SCRATCH_NAME to" >&2
  echo "                something else, or unset it for the default" >&2
  echo "                'edmacs-scratch-$(basename "$MODULE_ROOT")'." >&2
  exit 2
fi
if [[ "$SERVER" == */* ]]; then
  echo "claude-scratch: refusing a server name containing '/': $SERVER" >&2
  exit 2
fi

# `emacsclient' fails with "Unknown terminal type" in an agent environment
# unless TERM is set (live-daemon-diagnosis project memory), so it is set
# explicitly on every child rather than trusting the ambient shell's.
export TERM=dumb

socket_dir() {
  if [[ -n "${XDG_RUNTIME_DIR:-}" && -d "$XDG_RUNTIME_DIR/emacs" ]]; then
    echo "$XDG_RUNTIME_DIR/emacs"
  else
    echo "${TMPDIR:-/tmp}/emacs$(id -u)"
  fi
}
SOCKET="$(socket_dir)/$SERVER"

# `emacsclient' cannot be interrupted on a deadline by plain invocation, and a
# wedged daemon is exactly the case that has to stay recoverable -- so every
# call runs in the background under a poll loop. Returns 124 on timeout.
ev_bounded() {
  local form="$1" secs="${2:-15}" out rc pid i=0
  out="$(mktemp -t edmacs-scratch-ev)"
  emacsclient -s "$SERVER" -e "$form" >"$out" 2>&1 &
  pid=$!
  while kill -0 "$pid" 2>/dev/null && (( i < secs * 10 )); do
    sleep 0.1
    i=$(( i + 1 ))
  done
  if kill -0 "$pid" 2>/dev/null; then
    kill -9 "$pid" 2>/dev/null
    wait "$pid" 2>/dev/null
    rc=124
  else
    wait "$pid"
    rc=$?
  fi
  cat "$out"
  rm -f "$out"
  return $rc
}

alive() { ev_bounded "(quote alive)" 10 >/dev/null 2>&1; }

daemon_pids() { pgrep -f -- "--daemon=${SERVER}\$" 2>/dev/null; }

cmd_name() { echo "$SERVER"; }

cmd_status() {
  if alive; then
    echo "claude-scratch: '$SERVER' is alive (modules: $MODULE_ROOT)"
    return 0
  fi
  echo "claude-scratch: '$SERVER' is not answering"
  return 1
}

cmd_stop() {
  local pids
  pids="$(daemon_pids)"
  ev_bounded "(kill-emacs)" 5 >/dev/null 2>&1
  sleep 0.2
  # A wedged daemon never answers `(kill-emacs)' -- the whole reason this
  # script exists -- so a real signal is the fallback, not the exception.
  for pid in $pids; do
    if kill -0 "$pid" 2>/dev/null; then
      kill -TERM "$pid" 2>/dev/null
      sleep 0.5
      kill -0 "$pid" 2>/dev/null && kill -KILL "$pid" 2>/dev/null
    fi
  done
  [[ -S "$SOCKET" ]] && rm -f "$SOCKET"
  echo "claude-scratch: stopped '$SERVER'"
  return 0
}

cmd_eval() {
  local form="${1:-}"
  if [[ -z "$form" ]]; then
    echo "usage: claude-scratch.sh eval '<FORM>'" >&2
    exit 2
  fi
  if ! alive; then
    echo "claude-scratch: '$SERVER' is not running; start it first" >&2
    exit 1
  fi
  ev_bounded "$form" 30
}

cmd_start() {
  if alive; then
    echo "claude-scratch: '$SERVER' is already running (modules: $MODULE_ROOT)"
    return 0
  fi
  # Nothing answers, so any socket left here is stale from a killed daemon.
  [[ -S "$SOCKET" ]] && rm -f "$SOCKET"

  local build=""
  if [[ "${EDMACS_SCRATCH_PACKAGES:-}" != "none" ]]; then
    build="$PACKAGE_ROOT/straight/build"
    if [[ ! -d "$build" ]]; then
      echo "error: $PACKAGE_ROOT has no built package tree ($build)." >&2
      echo "       Only the main checkout has one; a worktree has a lockfile" >&2
      echo "       and nothing else. This refuses to start rather than" >&2
      echo "       bootstrap a second tree -- run from the main checkout, or" >&2
      echo "       set EDMACS_SCRATCH_PACKAGES=none for a pure -Q daemon." >&2
      exit 2
    fi
  fi

  echo "claude-scratch: starting '$SERVER'"
  echo "  module sources: $MODULE_ROOT"
  echo "  packages:       ${build:-none (pure -Q)}"
  emacs -Q --daemon="$SERVER" >/dev/null 2>&1 || {
    echo "claude-scratch: could not start daemon '$SERVER'" >&2
    exit 1
  }

  boot() {
    if ! ev_bounded "$1" 30 >/dev/null 2>&1; then
      echo "claude-scratch: bootstrap form failed: $1" >&2
      cmd_stop >/dev/null 2>&1
      exit 1
    fi
  }

  boot "(setq default-directory \"$MODULE_ROOT/\")"
  # `-Q' still writes backups, auto-saves and lock files wherever a
  # prototyping session saves; without this the checkout accumulates litter
  # that looks like uncommitted work.
  boot "(setq make-backup-files nil auto-save-default nil create-lockfiles nil)"

  if [[ -n "$build" ]]; then
    boot "(dolist (d (directory-files \"$build\" t nil t)) (when (and (file-directory-p d) (not (member (file-name-nondirectory d) '(\".\" \"..\")))) (add-to-list 'load-path d)))"
  fi

  for module in claude-lib claude-lib-view claude-lib-ert claude-lib-drive; do
    boot "(load \"$MODULE_ROOT/modules/$module.el\" nil t)"
  done

  # Assert what the daemon actually IS before reporting success, rather than
  # assuming it. `claude-lib-file' resolves from `load-file-name', so loading
  # THIS checkout's copy already scopes `claude-lib-promote' to the checkout
  # under test instead of the main checkout's live library -- invisible unless
  # it is printed.
  local report
  report="$(ev_bounded "(format \"daemonp=%S claude-lib-file=%S drive=%S\" (daemonp) (and (boundp 'claude-lib-file) claude-lib-file) (featurep 'claude-lib-drive))" 15)"
  echo "  $report"
  if [[ "$report" != *"daemonp=\\\"$SERVER\\\""* ]]; then
    echo "claude-scratch: daemon did not report the expected server name" >&2
    cmd_stop >/dev/null 2>&1
    exit 1
  fi
  if [[ "$report" != *"drive=t"* ]]; then
    echo "claude-scratch: claude-lib-drive did not load into '$SERVER'" >&2
    cmd_stop >/dev/null 2>&1
    exit 1
  fi
  echo "claude-scratch: '$SERVER' ready -- reach it with"
  echo "  emacsclient -s $SERVER -e '<FORM>'    (or: $0 eval '<FORM>')"
  return 0
}

case "${1:-}" in
  start)   cmd_start ;;
  stop)    cmd_stop ;;
  restart) cmd_stop >/dev/null 2>&1; cmd_start ;;
  status)  cmd_status ;;
  name)    cmd_name ;;
  eval)    shift; cmd_eval "${1:-}" ;;
  *)
    echo "usage: $0 start|stop|restart|status|name|eval '<FORM>'" >&2
    exit 2
    ;;
esac
