#!/usr/bin/env bash
#
# verify-gc-fallback-daemon.sh -- boot a throwaway `emacs --daemon' with an
# error injected into init.el before emacs-startup-hook registers, and
# confirm early-init.el's idle-timer fallback lowers gc-cons-threshold.
#
# A real daemon loads init.el via `load-user-init-file', which wraps the load
# in condition-case: the daemon survives the error in a degraded state.
# Passing the files with `-l' instead makes the error fatal and does not
# reproduce this. Kept out of verify-gc-and-unbounded-state.sh because it
# sleeps out the real idle delay.
#
# USAGE
#   scripts/verify-gc-fallback-daemon.sh [path-to-edmacs-checkout]
#   Exits 0 if the daemon survives and the fallback value is observed.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="${1:-$(cd "$SCRIPT_DIR/.." && pwd)}"

if [[ ! -f "$REPO_ROOT/init.el" || ! -f "$REPO_ROOT/early-init.el" ]]; then
  echo "error: $REPO_ROOT does not look like an edmacs checkout (no init.el/early-init.el)" >&2
  exit 2
fi

# Read the idle delay from early-init.el so this can't drift.
IDLE_DELAY="$(grep -A1 'run-with-idle-timer' "$REPO_ROOT/early-init.el" \
  | tail -1 | grep -oE '[0-9]+' | head -1)"
if [[ -z "$IDLE_DELAY" ]]; then
  echo "error: could not extract the fallback idle-timer delay from early-init.el" >&2
  exit 2
fi

# Not under $TMPDIR: its path is too long for a UNIX socket, and /tmp is a
# symlink, which Emacs's daemon-socket safety check refuses.
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

cp "$REPO_ROOT/early-init.el" "$EMACSD/early-init.el"
cp "$REPO_ROOT/init.el" "$EMACSD/init.el"

# Inject right after straight bootstraps use-package: before any module
# loads (so the mise trust prompt never fires) and before gcmh.
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

# The CLI's exit status reports the foreground handshake, not whether the
# background server survived; check the socket instead.
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
