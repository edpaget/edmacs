#!/usr/bin/env bash
#
# wedge-watchdog.sh -- notice a wedged Emacs daemon within seconds and
# capture the evidence while it is still wedged.
#
# The macOS NS event-loop wedge this chases leaves the daemon alive,
# burning ~2% CPU, with its socket present but refusing connections. By
# the time a human notices the frame is dead, the interesting question --
# what was running when the loop stopped returning -- has to be answered
# from whatever was already written down. This script does the writing
# down: it polls, and on the first failed poll captures a sample(1) stack
# plus the tail of modules/wedge-trace.el's trace file.
#
# USAGE
#   scripts/wedge-watchdog.sh [--interval SECONDS] [--once]
#
# Captures land in <capture-dir>/wedge-<timestamp>/ as stack.txt,
# trace-tail.log, heartbeat, and summary.txt. One capture per wedge
# episode: it arms again only after the daemon answers a poll, so a wedge
# that lasts an hour produces one capture, not hundreds.
#
# Environment:
#   EDMACS_SERVER        server name to probe          (default: server)
#   EDMACS_CACHE_DIR     where the tracer writes       (default: ~/.config/emacs/.cache)
#   EDMACS_CAPTURE_DIR   where captures land           (default: $EDMACS_CACHE_DIR/wedge-captures)
#
# Depends on emacsclient, sample(1) and perl. It never loads the real
# config and never sets --init-directory, so it is safe from any checkout
# (see CLAUDE.md's Worktrees section).

set -uo pipefail

INTERVAL=5
ONCE=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --interval) INTERVAL="${2:?--interval needs a value}"; shift 2 ;;
    --once)     ONCE=1; shift ;;
    -h|--help)  sed -n '2,30p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

if ! [[ "$INTERVAL" =~ ^[0-9]+$ ]] || [[ "$INTERVAL" -lt 1 ]]; then
  echo "wedge-watchdog: --interval must be a positive integer, got '$INTERVAL'" >&2
  exit 2
fi

SERVER="${EDMACS_SERVER:-server}"
CACHE_DIR="${EDMACS_CACHE_DIR:-$HOME/.config/emacs/.cache}"
CAPTURE_DIR="${EDMACS_CAPTURE_DIR:-$CACHE_DIR/wedge-captures}"
TRACE_FILE="$CACHE_DIR/wedge-trace.log"
HEARTBEAT_FILE="$CACHE_DIR/wedge-heartbeat"

# Probing `server' is the whole point here -- unlike the scratch-daemon
# scripts, which refuse it -- but a wedge capture is read-only: this
# script never evaluates anything that could mutate the daemon.
probe() {
  perl -e 'alarm shift; exec @ARGV' "$INTERVAL" \
    emacsclient -s "$SERVER" --eval '(emacs-pid)' >/dev/null 2>&1
}

daemon_pid() {
  pgrep -f 'Emacs --fg-daemon' | head -1
}

capture() {
  local pid="$1" stamp dir
  stamp="$(date +%Y%m%d-%H%M%S)"
  dir="$CAPTURE_DIR/wedge-$stamp"
  mkdir -p "$dir" || return 1

  {
    echo "wedge detected: $(date '+%F %T %z')"
    echo "daemon pid:     $pid"
    echo "server:         $SERVER"
    echo "probe timeout:  ${INTERVAL}s"
    echo
    echo "--- heartbeat (last write before the wedge) ---"
    cat "$HEARTBEAT_FILE" 2>/dev/null || echo "(no heartbeat file)"
    echo
    echo "--- last trace record ---"
    tail -1 "$TRACE_FILE" 2>/dev/null || echo "(no trace file)"
  } > "$dir/summary.txt"

  cp "$HEARTBEAT_FILE" "$dir/heartbeat" 2>/dev/null
  tail -500 "$TRACE_FILE" > "$dir/trace-tail.log" 2>/dev/null

  # sample(1) is the only thing here that must run while the daemon is
  # still wedged; everything else could be reconstructed afterwards.
  sample "$pid" 5 -file "$dir/stack.txt" >/dev/null 2>&1 \
    || echo "(sample failed)" > "$dir/stack.txt"

  echo "$dir"
}

echo "wedge-watchdog: polling '$SERVER' every ${INTERVAL}s; captures -> $CAPTURE_DIR"
armed=1
while true; do
  if probe; then
    if [[ "$armed" -eq 0 ]]; then
      echo "wedge-watchdog: $(date '+%F %T') daemon answering again; re-armed"
    fi
    armed=1
  elif [[ "$armed" -eq 1 ]]; then
    pid="$(daemon_pid)"
    if [[ -z "$pid" ]]; then
      echo "wedge-watchdog: $(date '+%F %T') no --fg-daemon process; not a wedge"
    else
      dir="$(capture "$pid")"
      echo "wedge-watchdog: $(date '+%F %T') WEDGE captured -> $dir"
      grep -E '^(heartbeat|--- last trace)' -A1 "$dir/summary.txt" 2>/dev/null | tail -4
      [[ "$ONCE" -eq 1 ]] && exit 0
    fi
    armed=0
  fi
  sleep "$INTERVAL"
done
