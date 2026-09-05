#!/usr/bin/env bash
# Run an ERT suite against a REAL graphical frame.
#
#   scripts/gui-ert.sh modules/window-geometry-live-test.el [SELECTOR] [-l EXTRA.el ...]
#
# `emacs --batch' has no window system, so fringes are always 0, scroll
# bars never exist, and `window-body-width' is always exactly one column
# less than `window-total-width'.  Any assertion about a collapsed strip's
# real usable width is therefore unfalsifiable in batch.  This script
# starts a THROWAWAY daemon under its own server name, makes one
# off-screen graphical frame in it, runs the suite inside that frame, and
# kills the daemon again.
#
# It never touches the user's own daemon (server name "server") and it
# never sets --init-directory, so it cannot bootstrap a second straight
# package tree: the suite files put the main checkout's straight/build on
# `load-path' themselves.  Safe to run from a worktree.
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_FILE="${1:?usage: gui-ert.sh <test-file.el> [selector] [-l extra.el ...]}"
shift
SELECTOR="t"
if [ $# -gt 0 ] && [ "$1" != "-l" ]; then SELECTOR="$1"; shift; fi

EXTRA_LOADS=()
while [ $# -gt 0 ]; do
  case "$1" in
    -l) EXTRA_LOADS+=("$2"); shift 2 ;;
    *) echo "gui-ert.sh: unexpected argument '$1'" >&2; exit 2 ;;
  esac
done

case "$TEST_FILE" in /*) ;; *) TEST_FILE="$REPO_ROOT/$TEST_FILE" ;; esac
[ -f "$TEST_FILE" ] || { echo "gui-ert.sh: no such test file: $TEST_FILE" >&2; exit 2; }

SERVER="edmacs-gui-ert-$$"
cleanup() { emacsclient -s "$SERVER" -e '(kill-emacs)' >/dev/null 2>&1 || true; }
trap cleanup EXIT

echo "gui-ert: starting throwaway daemon '$SERVER' (repo: $REPO_ROOT)"
emacs -Q --daemon="$SERVER" >/dev/null 2>&1 || {
  echo "gui-ert: could not start daemon" >&2; exit 1; }

ev() { emacsclient -s "$SERVER" -e "$1"; }

# `default-directory' is how the suite files locate modules/ and the main
# checkout's straight/build -- set it before anything is loaded.
ev "(setq default-directory \"$REPO_ROOT/\")" >/dev/null || exit 1
ev "(load \"$REPO_ROOT/scripts/gui-ert.el\" nil t)" >/dev/null || exit 1
ev "(load \"$REPO_ROOT/modules/git-common-dir.el\" nil t)" >/dev/null || exit 1
for extra in ${EXTRA_LOADS[@]+"${EXTRA_LOADS[@]}"}; do
  case "$extra" in /*) ;; *) extra="$REPO_ROOT/$extra" ;; esac
  ev "(load \"$extra\" nil t)" >/dev/null || exit 1
done

if ! ev "(edmacs-gui-ert-make-frame)" >/dev/null 2>&1; then
  echo "gui-ert: could not create a graphical frame -- is there a window server?" >&2
  exit 1
fi

ev "(load \"$TEST_FILE\" nil t)" >/dev/null || exit 1

# `ert-select-tests' treats a symbol selector as "exactly this test name" and
# a string selector as a regexp/prefix match.  The default "t" means "every
# test" and must stay the symbol `t'; anything else is a regexp and must be
# passed through as a quoted Lisp string, or partial/prefix selectors (the
# normal ERT idiom) silently fail with `ert-test-unbound' instead of matching.
if [ "$SELECTOR" = "t" ]; then
  SELECTOR_FORM="t"
else
  ESCAPED_SELECTOR=$(printf '%s' "$SELECTOR" | sed 's/\\/\\\\/g; s/"/\\"/g')
  SELECTOR_FORM="\"$ESCAPED_SELECTOR\""
fi

RESULT=$(ev "(let ((r (edmacs-gui-ert-run $SELECTOR_FORM))) (format \"%s\n__FAILURES__ %d\" (car r) (cdr r)))")
# emacsclient prints the string escaped and quoted; unwrap it.
printf '%s\n' "$RESULT" | python3 -c '
import sys
s = sys.stdin.read().strip()
if s.startswith("\"") and s.endswith("\""):
    s = s[1:-1]
sys.stdout.write(s.encode().decode("unicode_escape"))
'
echo
FAILURES=$(printf '%s' "$RESULT" | grep -o '__FAILURES__ [0-9]*' | awk '{print $2}')
[ "${FAILURES:-1}" = "0" ]
