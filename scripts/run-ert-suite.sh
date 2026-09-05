#!/usr/bin/env bash
#
# run-ert-suite.sh -- wraps an ERT batch invocation with a wall-clock budget
# so a duration regression fails the run instead of passing silently. The
# windows-test.el incident (4.2s -> 282s from an un-guarded native-comp
# trampoline compile, see .claude/CLAUDE.md's Testing section) reported
# 108/108 passed the whole time nothing in the pipeline measured duration.
#
# USAGE
#   scripts/run-ert-suite.sh <budget-seconds> <ert-batch-command...>
#
# Example:
#   scripts/run-ert-suite.sh 30 emacs -Q --batch -l ert \
#     -l modules/git-common-dir.el -l modules/claude-term.el \
#     -l modules/windows.el -l modules/windows-test.el \
#     -f ert-run-tests-batch-and-exit
#
# Exits non-zero -- even if the wrapped command itself exited 0 -- when
# elapsed wall time exceeds the budget. Otherwise propagates the wrapped
# command's own exit status. Depends on nothing but perl and the wrapped
# command: no `user-emacs-directory', no straight, no eln-cache, so it is
# safe to run from any worktree (see CLAUDE.md's Worktrees section).

set -uo pipefail

if [[ $# -lt 2 ]]; then
  echo "usage: $0 <budget-seconds> <ert-batch-command...>" >&2
  exit 2
fi

BUDGET="$1"
shift

# A non-numeric budget would otherwise numify to 0 in the perl comparisons
# below and silently defeat the whole point of this script (elapsed > 0
# looks true, but so does the wrapped command's own success) -- fail fast
# with an actionable message instead.
if ! [[ "$BUDGET" =~ ^[0-9]+([.][0-9]+)?$ ]]; then
  echo "budget must be a positive number of seconds, got: ${BUDGET}" >&2
  exit 2
fi

# Sub-second timestamp portable across BSD date (no %N) and bash < 5 (no
# EPOCHREALTIME); perl ships with macOS. Matches scripts/startup-bench.sh's
# own now()/elapsed_since() helpers.
now() {
  perl -MTime::HiRes=time -e 'printf "%.6f", time'
}

START="$(now)"
# set -e is not in play here (only -uo pipefail), so "$@"'s exit status is
# captured directly rather than swallowed before the budget check runs.
if "$@"; then
  STATUS=0
else
  STATUS=$?
fi
ELAPSED="$(perl -e "printf('%.2f', $(now) - $START)")"

echo "Suite took ${ELAPSED}s (budget ${BUDGET}s)"

if perl -e "exit(($ELAPSED > $BUDGET) ? 0 : 1)"; then
  echo "FAIL: exceeded ${BUDGET}s duration budget" >&2
  exit 1
fi

exit "$STATUS"
