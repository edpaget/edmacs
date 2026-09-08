#!/usr/bin/env bash
#
# pty-ert.sh -- runs its arguments under a real pty and exits with the
# wrapped command's actual exit code.
#
# Several suites (sidebar-test.el, sidebar-buffers-live-test.el,
# sidebar-agents-live-test.el, workspaces-live-test.el) need a second real
# frame, which needs a controlling terminal -- absent under plain
# `emacs -Q --batch'. `script -q /dev/null <cmd>' is the traditional way to
# get one, but it fails outright wherever stdin is not itself a terminal
# (`tcgetattr/ioctl: Operation not supported on socket'), which includes
# most non-interactive contexts (CI, an agent's tool call). Allocating a
# pty directly via Python's `pty' module works in both.
#
# `pty.spawn' returns a wait status, not an exit code -- a caller checking
# its return value directly never sees a wrapped command's failure
# (verified: wrapping `false' returns 0 from a bare `pty.spawn'). This
# script translates that wait status into a real exit code via
# `os.waitstatus_to_exitcode' (Python >= 3.9) before exiting with it.
#
# USAGE
#   scripts/pty-ert.sh <command...>
#
# Example:
#   scripts/pty-ert.sh emacs -Q --batch -l ert \
#     -l modules/git-common-dir.el -l modules/sidebar-test.el \
#     -f ert-run-tests-batch-and-exit

set -u

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <command...>" >&2
  exit 2
fi

exec python3 -c '
import os, pty, sys
status = pty.spawn(sys.argv[1:])
sys.exit(os.waitstatus_to_exitcode(status))
' "$@"
