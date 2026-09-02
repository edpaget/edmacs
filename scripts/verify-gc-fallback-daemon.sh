#!/usr/bin/env bash
#
# verify-gc-fallback-daemon.sh -- real-daemon reproduction of AC2's
# deliberate-error fallback path, for
# edmacs-performance/phase-6-gc-and-unbounded-state.
#
# WHY THIS EXISTS
#   AC2 requires that gc-cons-threshold "remains sane if init is made to
#   error deliberately before emacs-startup-hook registers." A code-review
#   pass on the prior attempt at this phase reproduced an injected
#   pre-bootstrap error against a real `emacs --daemon` and reported the
#   daemon process exiting within ~0.1s ("Error: server did not start
#   correctly"), concluding the fallback timer's whole premise (a daemon
#   that keeps running for weeks with GC pinned at most-positive-fixnum)
#   cannot occur -- a boot-time error supposedly kills the daemon outright.
#
#   That reproduction used `emacs --daemon=<sock> -l early-init.el -l
#   init.el`, i.e. explicit `-l` flags on the command line. That is NOT how
#   a real daemon starts, and it behaves differently: an error escaping a
#   `-l`-loaded file is fatal to the whole process, because `-l` runs
#   `load` directly during command-line switch processing with no
#   protective wrapper. A real daemon instead discovers early-init.el and
#   init.el itself via `load-user-init-file`, which wraps each file's load
#   in its own top-level `condition-case`: an error prints a `Warning
#   (initialization): ...` message and the daemon proceeds to start the
#   server anyway, running in a "degraded" state -- forms in the erroring
#   file after the error point never ran, but the process itself survives
#   indefinitely. That is exactly the scenario this phase's Context section
#   describes and the early-init.el fallback timer protects against.
#
#   This script reproduces the correct (non-`-l`) startup path end to end
#   against a real, throwaway `emacs --daemon`, so AC2's failure-path claim
#   rests on a scripted, rerunnable repro instead of a manual narrative
#   that can be redone differently by a future run and disagree (as
#   happened here). It is intentionally NOT part of
#   verify-gc-and-unbounded-state.sh's default run: it boots a real Emacs
#   daemon and sleeps out the actual idle delay (~20s+), which that
#   script's header already explains is deliberately excluded from the
#   fast structural pass.
#
# HAZARDS THIS SCRIPT AVOIDS (both caused false negatives previously)
#   - Emacs's daemon-socket safety check refuses to start if the socket's
#     directory is a symlink, world/group-writable, or not owned by the
#     caller -- `/tmp` on macOS is a symlink to `/private/tmp`, and
#     `/private/tmp` itself is root-owned, so a naive `--daemon=/tmp/...`
#     invocation fails for reasons unrelated to the injected error. This
#     script creates its own chmod-700 directory outside of `/tmp` and
#     points `--daemon=` at a socket file directly inside it.
#   - This config's mise integration (modules/core.el) prompts
#     interactively ("Mise: trust dir ...?") the first time
#     `global-mise-mode` sees a new directory, on `after-init-hook`. With
#     stdin closed (as under a backgrounded/redirected daemon launch) that
#     prompt hits EOF and can wedge startup. The error is injected right
#     after `(straight-use-package 'use-package)` in init.el -- before
#     any module (including core.el) loads -- so module loading, and
#     therefore the mise prompt, never happens in this repro. This mirrors
#     the phase's own named exposure window (anything before init.el's
#     post-startup GC reset, including the straight bootstrap machinery
#     immediately above that line).
#   - Instead of copying the whole checkout (straight/repos and
#     straight/build are large and irrelevant to this test), this script
#     symlinks every top-level entry of the real checkout into a fake
#     $HOME/.emacs.d except early-init.el/init.el, which get real
#     (modified) copies carrying the injected error. Nothing under the
#     real checkout is touched.
#
# USAGE
#   scripts/verify-gc-fallback-daemon.sh [path-to-edmacs-checkout]
#   Exits 0 if the daemon survives the injected error and the fallback
#   value is observed after the idle delay; 1 otherwise. Always tears down
#   the throwaway daemon and its temp directory on exit.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="${1:-$(cd "$SCRIPT_DIR/.." && pwd)}"

if [[ ! -f "$REPO_ROOT/init.el" || ! -f "$REPO_ROOT/early-init.el" ]]; then
  echo "error: $REPO_ROOT does not look like an edmacs checkout (no init.el/early-init.el)" >&2
  exit 2
fi

# The fallback timer's idle delay, read out of early-init.el itself so this
# script stays correct if that value ever changes, rather than hardcoding
# a second copy of it that can silently drift.
IDLE_DELAY="$(grep -A1 'run-with-idle-timer' "$REPO_ROOT/early-init.el" \
  | tail -1 | grep -oE '[0-9]+' | head -1)"
if [[ -z "$IDLE_DELAY" ]]; then
  echo "error: could not extract the fallback idle-timer delay from early-init.el" >&2
  exit 2
fi

# Deliberately NOT under $TMPDIR: macOS's per-process $TMPDIR
# (/var/folders/.../T/) is long enough that the resulting socket path
# overflows the platform's UNIX-socket path-length limit ("daemon: child
# name too long"), and /tmp itself is a symlink to /private/tmp, which
# Emacs's daemon-socket safety check refuses as unsafe regardless of
# permissions. $HOME is short, real, and (per the chmod below) safe.
TESTDIR="$(mktemp -d "$HOME/.edmacs-gc-fallback-test.XXXXXX")"
chmod 700 "$TESTDIR"
HOMEDIR="$TESTDIR/home"
EMACSD="$HOMEDIR/.emacs.d"
SOCK="$TESTDIR/sock"

FAILED=0
pass() { echo "PASS: $1"; }
fail() { echo "FAIL: $1"; FAILED=1; }

cleanup() {
  if [[ -S "$SOCK" ]]; then
    emacsclient --socket-name="$SOCK" --eval '(kill-emacs)' >/dev/null 2>&1
  fi
  rm -rf "$TESTDIR"
}
trap cleanup EXIT

mkdir -p "$EMACSD"
for entry in "$REPO_ROOT"/*; do
  base="$(basename "$entry")"
  case "$base" in
    init.el|early-init.el) continue ;;
  esac
  ln -s "$entry" "$EMACSD/$base"
done
# Dotfiles (e.g. .gitignore) are skipped by the glob above; harmless to
# omit since nothing this test loads references them.

cp "$REPO_ROOT/early-init.el" "$EMACSD/early-init.el"
cp "$REPO_ROOT/init.el" "$EMACSD/init.el"

# Inject the error at the exact point deed461's own verification and the
# phase's Context section both name: right after straight bootstraps
# use-package, before anything else in init.el (all module loads, the
# gcmh use-package block, and emacs-startup-hook registration) runs.
if ! grep -q "^(straight-use-package 'use-package)$" "$EMACSD/init.el"; then
  echo "error: could not find the expected straight-use-package anchor line in init.el to inject after" >&2
  exit 2
fi
INJECT_ELISP='(error "verify-gc-fallback-daemon: injected test error")'
awk -v inject="$INJECT_ELISP" '
  { print }
  /^\(straight-use-package .use-package\)$/ && !done { print inject; done=1 }
' "$EMACSD/init.el" > "$EMACSD/init.el.tmp" && mv "$EMACSD/init.el.tmp" "$EMACSD/init.el"

echo "booting throwaway daemon (HOME=$HOMEDIR, socket=$SOCK)..."
BOOT_LOG="$TESTDIR/boot.log"
HOME="$HOMEDIR" emacs --daemon="$SOCK" >"$BOOT_LOG" 2>&1
BOOT_STATUS=$?

# A real daemon that hits our injected error still prints
# "Error: server did not start correctly" on some Emacs builds even when
# the background process survives (the parent CLI invocation reports the
# *foreground* handshake, not the backgrounded server's fate) -- so check
# the socket and the process, not $BOOT_STATUS, to decide liveness.
sleep 1
if [[ ! -S "$SOCK" ]] || ! emacsclient --socket-name="$SOCK" --eval 't' >/dev/null 2>&1; then
  fail "daemon did not survive the injected pre-startup-hook error (exit=$BOOT_STATUS) -- boot log follows:"
  cat "$BOOT_LOG" >&2
  echo
  [[ $FAILED -eq 0 ]] && exit 0 || exit 1
fi
pass "daemon survived an init.el error injected before emacs-startup-hook registers"

GCT_IMMEDIATE="$(emacsclient --socket-name="$SOCK" --eval 'gc-cons-threshold' 2>/dev/null)"
if [[ "$GCT_IMMEDIATE" == "2305843009213693951" ]]; then
  pass "gc-cons-threshold is most-positive-fixnum immediately after the injected error (fallback armed, not yet fired)"
else
  fail "gc-cons-threshold immediately after the injected error is not most-positive-fixnum -- got $GCT_IMMEDIATE"
fi

WAIT=$((IDLE_DELAY + 8))
echo "waiting ${WAIT}s (idle delay ${IDLE_DELAY}s + margin) for the fallback timer to fire..."
sleep "$WAIT"

GCT_AFTER="$(emacsclient --socket-name="$SOCK" --eval 'gc-cons-threshold' 2>/dev/null)"
if [[ "$GCT_AFTER" =~ ^[0-9]+$ && "$GCT_AFTER" -ne 2305843009213693951 && "$GCT_AFTER" -ge $((32 * 1024 * 1024)) ]]; then
  pass "gc-cons-threshold reads the early-init.el fallback value after the idle delay ($GCT_AFTER)"
else
  fail "gc-cons-threshold did not fall back to a sane value after the idle delay -- got $GCT_AFTER"
fi

echo
if [[ $FAILED -eq 0 ]]; then
  echo "ALL CHECKS PASSED"
  exit 0
else
  echo "ONE OR MORE CHECKS FAILED"
  exit 1
fi
