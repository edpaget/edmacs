#!/usr/bin/env bash
#
# claude-agent-headless-check.sh -- start a REAL ACP session with no human at
# the keyboard, and prove the two things that claim rests on: the driving
# connection never hangs, and the session leaves nothing behind in the repo.
#
#   scripts/claude-agent-headless-check.sh
#
# WHY A SCRIPT AND NOT AN ERT ROW.  modules/claude-agent-test.el already
# asserts the picker-free start path against a STUBBED agent-shell: that
# proves this module passes `:session-strategy 'new', not that a real
# `claude-agent-acp' completes `initialize' and `session/new' without ever
# reading the minibuffer.  Only a live agent can falsify that, and a live
# agent spawns a real Claude Code process against the user's subscription --
# which is why this is deliberately NOT a row in scripts/test-manifest.sh and
# not reached by scripts/test-all.sh.  Run it by hand when claude-agent.el's
# start path, or the pinned agent-shell/acp.el revisions, change.
#
# WHAT IT WOULD CATCH.  Phase 1 of the edmacs-claude-acp roadmap reproduced
# this repo's minibuffer-wedge hazard here: agent-shell's "Start shell
# (default: New shell)" picker fires from the `session/list' response
# callback, long after the start form returned, and answering that
# already-open prompt from a separate `emacsclient -e' wedges the daemon for
# every later client.  A regression would not fail an assertion -- it would
# hang.  So every eval below runs under claude-scratch.sh's bounded channel
# (124 on timeout), and a timeout is reported as the wedge it is.
#
# IT NEVER TOUCHES THE OPERATOR'S DAEMON.  Everything runs in a throwaway
# `emacs -Q' daemon under its own server name; claude-scratch.sh refuses the
# name `server' outright, and the daemon is killed on every exit path.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SCRATCH="$SCRIPT_DIR/claude-scratch.sh"

export EDMACS_SCRATCH_NAME="${EDMACS_SCRATCH_NAME:-edmacs-acp-headless-check}"

# Node here is mise-shimmed, so `claude-agent-acp' resolves through the login
# shell exactly as `gopls' does for scripts/go-eglot-check.sh -- and exactly
# as claude-agent.el's own resolver does at runtime. Re-exec once rather than
# reporting a missing agent that is in fact installed.
if ! command -v claude-agent-acp >/dev/null 2>&1; then
  if [[ -z "${EDMACS_ACP_CHECK_RELOGIN:-}" ]]; then
    export EDMACS_ACP_CHECK_RELOGIN=1
    exec "${SHELL:-/bin/bash}" -lc "$(printf '%q ' "$0" "$@")"
  fi
  echo "claude-agent-headless-check: claude-agent-acp is not installed." >&2
  echo "  npm install -g @agentclientprotocol/claude-agent-acp" >&2
  exit 2
fi

fail() { echo "FAIL: $*" >&2; FAILED=1; }
pass() { echo "ok:   $*"; }
FAILED=0

cleanup() { "$SCRATCH" stop >/dev/null 2>&1 || true; }
trap cleanup EXIT

# ---------------------------------------------------------------------------
# Baseline. `.git/info/exclude' is what agent-shell--ensure-gitignore appends
# `/.agent-shell/' to, and in a WORKTREE that file lives under
# <main>/.git/worktrees/<name>/info/ -- a directory that does not otherwise
# exist, so its mere appearance is the regression. Main's own exclude already
# has a line in it, so this diffs rather than asserting empty.
# ---------------------------------------------------------------------------
GIT_DIR_PATH="$(git -C "$MODULE_ROOT" rev-parse --absolute-git-dir 2>/dev/null || true)"
COMMON_DIR_PATH="$(git -C "$MODULE_ROOT" rev-parse --path-format=absolute --git-common-dir 2>/dev/null || true)"

hash_or_absent() { [[ -f "$1" ]] && git hash-object "$1" || echo "ABSENT"; }

EXCLUDE_HERE="$GIT_DIR_PATH/info/exclude"
EXCLUDE_COMMON="$COMMON_DIR_PATH/info/exclude"
BEFORE_HERE="$(hash_or_absent "$EXCLUDE_HERE")"
BEFORE_COMMON="$(hash_or_absent "$EXCLUDE_COMMON")"
BEFORE_INFO_DIR="$([[ -d "$GIT_DIR_PATH/info" ]] && echo present || echo absent)"

echo "claude-agent-headless-check: module root $MODULE_ROOT"
echo "  git dir:        $GIT_DIR_PATH  (info/ $BEFORE_INFO_DIR)"
echo "  common git dir: $COMMON_DIR_PATH"

# ---------------------------------------------------------------------------
# Start the session. claude-scratch.sh boots the claude-lib family only, so
# the module is loaded explicitly -- the same two lines claude-agent.el's
# Commentary publishes as the automation recipe.
# ---------------------------------------------------------------------------
"$SCRATCH" start >/dev/null 2>&1 || { echo "could not start scratch daemon" >&2; exit 1; }

# `emacsclient' pretty-prints its result across lines, so every match below
# reads the whitespace-collapsed form rather than the raw one -- otherwise
# `:entries 0' arrives as `:entries\n\t0' and a passing assertion reads as a
# failure.
ev() { "$SCRATCH" eval "$1" | tr '\n\t' '  ' | tr -s ' '; }

if ! ev "(load (expand-file-name \"modules/claude-agent.el\" default-directory) nil t)" >/dev/null; then
  echo "could not load modules/claude-agent.el into the scratch daemon" >&2
  exit 1
fi

START_FORM='(let ((entries 0))
  (add-hook (quote minibuffer-setup-hook) (lambda () (setq entries (1+ entries))))
  (let ((buffer (claude-agent-start default-directory)))
    (list :buffer (buffer-name buffer)
          :depth (minibuffer-depth)
          :entries entries)))'

START_OUT="$(ev "$START_FORM")"
START_RC=$?
echo "  start returned: $START_OUT"

if [[ $START_RC -eq 124 ]]; then
  fail "the start call never returned -- this is the minibuffer wedge, not a slow agent"
  exit 1
elif [[ $START_RC -ne 0 || "$START_OUT" == *"*ERROR*"* ]]; then
  fail "claude-agent-start signalled: $START_OUT"
  exit 1
else
  pass "claude-agent-start returned without hanging the driving connection"
fi

[[ "$START_OUT" == *":depth 0"* ]] && pass "minibuffer-depth 0 at return" \
  || fail "the start path entered the minibuffer: $START_OUT"
[[ "$START_OUT" == *":entries 0"* ]] && pass "no minibuffer was opened at all" \
  || fail "minibuffer-setup-hook fired: $START_OUT"

BUFFER="$(sed -n 's/.*:buffer "\([^"]*\)".*/\1/p' <<<"$START_OUT")"
[[ -n "$BUFFER" ]] || { fail "no session buffer name in: $START_OUT"; exit 1; }

# ---------------------------------------------------------------------------
# The agent is real, not a stub: poll for the session id `session/new'
# returned. A buffer alone would come up even if the process died instantly.
# ---------------------------------------------------------------------------
SESSION_FORM="(with-current-buffer \"$BUFFER\"
  (list :session (map-nested-elt agent-shell--state (list :session :id))
        :process (car (mapcar (lambda (p) (list (process-name p) (process-status p)))
                              (seq-filter (lambda (p) (string-match-p \"acp-client\" (process-name p)))
                                          (process-list))))))"

SESSION_OUT=""
for _ in $(seq 1 30); do
  SESSION_OUT="$(ev "$SESSION_FORM")"
  [[ "$SESSION_OUT" == *':session "'* ]] && break
  sleep 1
done
echo "  session state:  $SESSION_OUT"

[[ "$SESSION_OUT" == *':session "'* ]] \
  && pass "a real claude-agent-acp answered session/new with a session id" \
  || fail "no ACP session id after 30s -- the agent never completed the handshake"
[[ "$SESSION_OUT" == *" run)"* ]] \
  && pass "the acp-client process is running" \
  || fail "no running acp-client process: $SESSION_OUT"

cleanup
trap - EXIT

# ---------------------------------------------------------------------------
# Nothing written into the repo.
# ---------------------------------------------------------------------------
AFTER_HERE="$(hash_or_absent "$EXCLUDE_HERE")"
AFTER_COMMON="$(hash_or_absent "$EXCLUDE_COMMON")"
AFTER_INFO_DIR="$([[ -d "$GIT_DIR_PATH/info" ]] && echo present || echo absent)"

[[ "$BEFORE_HERE" == "$AFTER_HERE" ]] \
  && pass "$EXCLUDE_HERE unchanged ($AFTER_HERE)" \
  || fail "$EXCLUDE_HERE changed: $BEFORE_HERE -> $AFTER_HERE"
[[ "$BEFORE_COMMON" == "$AFTER_COMMON" ]] \
  && pass "$EXCLUDE_COMMON unchanged ($AFTER_COMMON)" \
  || fail "$EXCLUDE_COMMON changed: $BEFORE_COMMON -> $AFTER_COMMON"
[[ "$BEFORE_INFO_DIR" == "$AFTER_INFO_DIR" ]] \
  && pass "the git dir's info/ is still $AFTER_INFO_DIR" \
  || fail "the session synthesized $GIT_DIR_PATH/info/"

STRAY="$(find "$MODULE_ROOT" -name '.agent-shell*' -not -path "$MODULE_ROOT/straight/*" 2>/dev/null)"
[[ -z "$STRAY" ]] && pass "no .agent-shell/ anywhere under the checkout" \
  || fail "the session wrote into the checkout: $STRAY"

DIRTY="$(git -C "$MODULE_ROOT" status --porcelain --ignored -- ':!straight' 2>/dev/null | grep -F '.agent-shell' || true)"
[[ -z "$DIRTY" ]] && pass "git sees no .agent-shell path, ignored or otherwise" \
  || fail "git reports: $DIRTY"

if [[ $FAILED -eq 0 ]]; then
  echo "claude-agent-headless-check: PASS"
  exit 0
fi
echo "claude-agent-headless-check: FAIL" >&2
exit 1
