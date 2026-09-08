#!/usr/bin/env bash
# test-manifest.sh -- the suite table scripts/test-all.sh drives.
#
# Sourced, not executed: it only sets the MANIFEST array. Each entry is one
# (suite, tier) row, pipe-delimited:
#
#   name|tier|budget|expected_skips_on_main|loads|target
#
# - name: the test file's basename with ".el" removed, e.g. "sidebar-test".
# - tier: "batch", "pty", or "gui".
# - budget: wall-clock seconds passed to run-ert-suite.sh (batch/pty only;
#   "-" for gui, which has no budget wrapper -- see CLAUDE.md's Testing
#   section on why the gui daemon-startup overhead makes a tight budget
#   the wrong tool there).
# - expected_skips_on_main: the skip count accepted on the MAIN checkout
#   ONLY (see is_main_checkout in test-all.sh); on a worktree, any skip
#   count is accepted (straight/build is legitimately absent there). A
#   suite that appears in more than one tier gets its own row and its own
#   expected_skips_on_main per tier -- the four pty suites' batch-tier row
#   carries their documented GUI-only skip count, but their pty-tier row
#   must show 0: attaching a pty is the whole point of that tier, so any
#   skip surviving it is a regression, not an acceptable baseline.
# - loads: comma-separated -l arguments, in the exact order the suite's own
#   Commentary documents, NOT including "-l ert" (always first) or (for
#   gui) modules/git-common-dir.el (scripts/gui-ert.sh always loads it
#   itself). "-" means no extra loads.
# - target: for batch/pty, the -f function ERT's batch runner calls --
#   almost always ert-run-tests-batch-and-exit, except the four leaking
#   suites, which point at modules/test-support.el's shared
#   edmacs-test-support-run-and-exit (wraps ert-run-tests-batch in
#   with-hermetic-state, then exits with its status) instead of pasting a
#   local copy. For gui, this field is instead the selector
#   scripts/gui-ert.sh takes ("t" for every test).
#
# workspaces-test and sessions-test both load modules/windows.el before
# modules/workspaces.el: workspaces.el's post-open handler calls
# `edmacs-windows-designate-main' directly (it is the sole owner of
# `tab-bar-tab-post-open-functions'), so a suite that drives a real
# `tab-bar-new-tab' without windows.el dies with a void-function.
#
# windows-test's batch row expects 1: its cross-frame main-window lookup
# asks for a tty frame on /dev/tty, which no batch run has. The pty tier
# would clear it, but a single test does not earn a whole extra row --
# accepting the documented skip is the cheaper honest answer.
#
# sessions-live-test's batch row expects 3, all three of them its GUI-only
# tests: the two that count graphical frames after the real restore bridge,
# and the sidebar-selected-new-tab check, whose side window dedication a
# batch frame cannot answer for. Its gui row runs all three and must show 0.
#
# Keep this the single source of truth for "what suites exist and how to
# run them": a 25th modules/*-test.el file, or a new tier for an existing
# suite, is added here, not hand-rolled elsewhere.

MANIFEST=(
  "agents-test|batch|15|0|modules/test-support.el,modules/agents.el,modules/agents-test.el|ert-run-tests-batch-and-exit"
  "claude-term-agents-test|batch|15|0|modules/test-support.el,modules/git-common-dir.el,modules/claude-term.el,modules/claude-term-registry.el,modules/agents.el,modules/claude-term-agents.el,modules/claude-term-agents-test.el|ert-run-tests-batch-and-exit"
  "claude-term-approval-parity-live-test|batch|30|2|modules/git-common-dir.el,modules/claude-term.el,modules/claude-term-registry.el,modules/claude-term-approval-parity-live-test.el|ert-run-tests-batch-and-exit"
  "claude-term-live-test|batch|30|5|modules/claude-term.el,modules/claude-term-registry.el,modules/claude-term-live-test.el|ert-run-tests-batch-and-exit"
  "claude-term-registry-live-test|batch|15|0|modules/git-common-dir.el,modules/claude-term.el,modules/claude-term-registry.el,modules/claude-term-registry-live-test.el|ert-run-tests-batch-and-exit"
  "claude-term-registry-test|batch|15|0|modules/test-support.el,modules/git-common-dir.el,modules/claude-term.el,modules/claude-term-registry.el,modules/claude-term-registry-test.el|ert-run-tests-batch-and-exit"
  "claude-term-test|batch|15|0|modules/claude-term.el,modules/claude-term-test.el|ert-run-tests-batch-and-exit"
  "claude-usage-test|batch|15|0|modules/test-support.el,modules/git-common-dir.el,modules/claude-usage-test.el|ert-run-tests-batch-and-exit"
  "core-live-test|batch|15|0|modules/test-support.el,modules/core-live-test.el|ert-run-tests-batch-and-exit"
  "git-common-dir-test|batch|15|0|modules/git-common-dir.el,modules/git-common-dir-test.el|ert-run-tests-batch-and-exit"
  "keybindings-test|batch|15|0|modules/test-support.el,modules/keybindings-test.el|ert-run-tests-batch-and-exit"
  "sessions-live-test|batch|15|3|modules/test-support.el,modules/git-common-dir.el,modules/sessions-live-test.el|ert-run-tests-batch-and-exit"
  "sessions-test|batch|15|0|modules/test-support.el,modules/git-common-dir.el,modules/windows.el,modules/workspaces.el,modules/sessions-test.el|ert-run-tests-batch-and-exit"
  "sidebar-agents-live-test|batch|15|1|modules/test-support.el,modules/git-common-dir.el,modules/sidebar-agents-live-test.el|ert-run-tests-batch-and-exit"
  "sidebar-agents-test|batch|15|0|modules/test-support.el,modules/git-common-dir.el,modules/agents.el,modules/sidebar-agents-test.el|ert-run-tests-batch-and-exit"
  "sidebar-buffers-live-test|batch|15|2|modules/test-support.el,modules/git-common-dir.el,modules/sidebar-buffers-live-test.el|edmacs-test-support-run-and-exit"
  "sidebar-buffers-test|batch|15|0|modules/test-support.el,modules/git-common-dir.el,modules/sidebar-buffers-test.el|ert-run-tests-batch-and-exit"
  "sidebar-test|batch|15|2|modules/test-support.el,modules/git-common-dir.el,modules/sidebar-test.el|edmacs-test-support-run-and-exit"
  "ui-live-test|batch|15|1|modules/test-support.el,modules/ui-live-test.el|ert-run-tests-batch-and-exit"
  "ui-test|batch|15|0|modules/ui.el,modules/ui-test.el|ert-run-tests-batch-and-exit"
  "window-geometry-live-test|batch|15|4|modules/test-support.el,modules/git-common-dir.el,modules/window-geometry-live-test.el|ert-run-tests-batch-and-exit"
  "windows-test|batch|30|1|modules/test-support.el,modules/git-common-dir.el,modules/claude-term.el,modules/workspaces.el,modules/windows.el,modules/windows-test.el|edmacs-test-support-run-and-exit"
  "workspaces-live-test|batch|15|3|modules/git-common-dir.el,modules/windows.el,modules/workspaces.el,modules/workspaces-live-test.el|ert-run-tests-batch-and-exit"
  "workspaces-test|batch|15|0|modules/test-support.el,modules/git-common-dir.el,modules/windows.el,modules/workspaces.el,modules/workspaces-test.el|edmacs-test-support-run-and-exit"

  "sidebar-test|pty|30|0|modules/test-support.el,modules/git-common-dir.el,modules/sidebar-test.el|edmacs-test-support-run-and-exit"
  "sidebar-buffers-live-test|pty|30|0|modules/test-support.el,modules/git-common-dir.el,modules/sidebar-buffers-live-test.el|edmacs-test-support-run-and-exit"
  "sidebar-agents-live-test|pty|30|0|modules/test-support.el,modules/git-common-dir.el,modules/sidebar-agents-live-test.el|ert-run-tests-batch-and-exit"
  "workspaces-live-test|pty|30|0|modules/git-common-dir.el,modules/windows.el,modules/workspaces.el,modules/workspaces-live-test.el|ert-run-tests-batch-and-exit"

  "ui-live-test|gui|-|0|modules/test-support.el|t"
  "sessions-live-test|gui|-|0|modules/test-support.el|t"
  "window-geometry-live-test|gui|-|0|modules/test-support.el|t"
)
