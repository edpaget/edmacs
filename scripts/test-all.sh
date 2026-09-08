#!/usr/bin/env bash
# test-all.sh -- the single pre-landing gate: every modules/*-test.el suite,
# every tier it needs, in one command.
#
#   scripts/test-all.sh [batch|pty|gui|all]
#
# Default (no argument) is "all". Reads its suite table from
# scripts/test-manifest.sh (see that file for the row format and for why
# a suite that runs in more than one tier gets one row per tier, each with
# its own expected-skip allowance).
#
# For each (suite, tier) row this prints one line:
#
#   SUITE [TIER]  tests=N unexpected=N skipped=N wall=Xs
#
# followed by a final TOTAL line, then exits non-zero if any row has
# unexpected>0, or -- ONLY when run from the main checkout (see
# is_main_checkout below) -- any row's skipped exceeds that row's own
# expected_skips_on_main. A worktree has no populated straight/build, so
# every suite that needs it legitimately reports more skips there; the
# strict skip bar only makes sense where straight/build is actually
# populated. Run from a worktree, this still catches every unexpected
# failure -- it just does not additionally enforce the skip bar.
#
# gui-tier rows fail the whole run if a real failure happens INSIDE a
# frame that was created; if no graphical frame could be created at all
# (no window server -- e.g. a headless CI runner or a sandboxed agent
# environment), that row is reported SKIPPED-GUI and does not fail the
# run, matching the phase's own baseline of only 2-3 gui-ert commands
# being routinely exercised today.
#
# A row whose own output does not match the expected ERT-summary shape at
# all (a parser failure, not a test failure) is treated as unexpected>=1:
# a broken parser must fail loud, never silently report a false green.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

# shellcheck source=test-manifest.sh
if ! source "$REPO_ROOT/scripts/test-manifest.sh"; then
  echo "FATAL: could not source scripts/test-manifest.sh -- refusing to report a false green" >&2
  exit 1
fi

if [ "${#MANIFEST[@]}" -eq 0 ]; then
  echo "FATAL: scripts/test-manifest.sh loaded but MANIFEST is empty -- refusing to report a false green" >&2
  exit 1
fi

# Sanity floor: the manifest must cover at least the 24 modules/*-test.el
# files known at the time this gate was built (several appear under more
# than one tier, so the array itself is longer than 24). A manifest that
# quietly lost rows -- a bad merge, a stray edit -- must fail loud here
# rather than silently running fewer suites and still exiting 0.
declare -A MANIFEST_SUITE_NAMES=()
for row in "${MANIFEST[@]}"; do
  IFS='|' read -r manifest_name _ _ _ _ _ <<<"$row"
  MANIFEST_SUITE_NAMES["$manifest_name"]=1
done
if [ "${#MANIFEST_SUITE_NAMES[@]}" -lt 24 ]; then
  echo "FATAL: manifest names only ${#MANIFEST_SUITE_NAMES[@]} distinct suites, expected at least 24 -- refusing to report a false green" >&2
  exit 1
fi

TIER_ARG="${1:-all}"
case "$TIER_ARG" in
  batch|pty|gui|all) ;;
  *) echo "usage: $0 [batch|pty|gui|all]" >&2; exit 2 ;;
esac

# A worktree's .gitignore keeps only straight/versions/ (see CLAUDE.md's
# "Worktrees" section) -- straight/build only exists, and only has
# content, once this checkout has itself been opened as a real Emacs
# config and straight has bootstrapped it. That is the signal for "am I
# the main checkout", not the directory's mere presence (a worktree could
# have an empty straight/build/ left by a stray tool).
is_main_checkout() {
  [ -d "$REPO_ROOT/straight/build" ] && [ -n "$(ls -A "$REPO_ROOT/straight/build" 2>/dev/null)" ]
}

MAIN_CHECKOUT=0
if is_main_checkout; then
  MAIN_CHECKOUT=1
fi

TOTAL_TESTS=0
TOTAL_UNEXPECTED=0
TOTAL_SKIPPED=0
OVERALL_EXIT=0
declare -a RESULT_LINES=()

# Parses one ERT batch summary line out of TEXT into the global
# PARSED_TESTS/PARSED_UNEXPECTED/PARSED_SKIPPED. Returns non-zero (and
# leaves those unset) if no such line is found at all -- the caller then
# treats the row as a parse failure rather than a false green.
parse_ert_summary() {
  local text="$1" line
  line="$(printf '%s\n' "$text" | grep -Eo 'Ran [0-9]+ tests, [0-9]+ results as expected, [0-9]+ unexpected(, [0-9]+ skipped)?' | tail -1)"
  [ -n "$line" ] || return 1
  PARSED_TESTS="$(printf '%s\n' "$line" | perl -ne '/Ran (\d+) tests/ and print $1')"
  PARSED_UNEXPECTED="$(printf '%s\n' "$line" | perl -ne '/(\d+) unexpected/ and print $1')"
  if printf '%s' "$line" | grep -q 'skipped'; then
    PARSED_SKIPPED="$(printf '%s\n' "$line" | perl -ne '/(\d+) skipped/ and print $1')"
  else
    PARSED_SKIPPED=0
  fi
  [ -n "${PARSED_TESTS:-}" ] && [ -n "${PARSED_UNEXPECTED:-}" ] || return 1
  return 0
}

parse_wall_seconds() {
  local text="$1"
  printf '%s\n' "$text" | grep -Eo 'Suite took [0-9.]+s' | tail -1 | grep -Eo '[0-9.]+' || true
}

record_row() {
  local name="$1" tier="$2" tests="$3" unexpected="$4" skipped="$5" wall="$6" skip_ok="$7"
  RESULT_LINES+=("$(printf '%-42s [%-5s] tests=%-4s unexpected=%-3s skipped=%-3s wall=%ss' \
    "$name" "$tier" "$tests" "$unexpected" "$skipped" "$wall")")
  local numeric_tests="$tests" numeric_skipped="$skipped"
  [[ "$numeric_tests" =~ ^[0-9]+$ ]] || numeric_tests=0
  [[ "$numeric_skipped" =~ ^[0-9]+$ ]] || numeric_skipped=0
  TOTAL_TESTS=$((TOTAL_TESTS + numeric_tests))
  TOTAL_UNEXPECTED=$((TOTAL_UNEXPECTED + unexpected))
  TOTAL_SKIPPED=$((TOTAL_SKIPPED + numeric_skipped))
  if [ "$unexpected" -gt 0 ]; then
    OVERALL_EXIT=1
  elif [ "$MAIN_CHECKOUT" -eq 1 ] && [ "$skip_ok" -eq 0 ]; then
    OVERALL_EXIT=1
  fi
}

run_batch_or_pty_row() {
  local name="$1" tier="$2" budget="$3" expected_skips="$4" loads_csv="$5" target="$6"
  local -a load_args=()
  if [ "$loads_csv" != "-" ]; then
    local IFS=','
    local -a loads=($loads_csv)
    for l in "${loads[@]}"; do
      load_args+=("-l" "$l")
    done
  fi
  local -a cmd=(scripts/run-ert-suite.sh "$budget")
  if [ "$tier" = "pty" ]; then
    cmd+=(scripts/pty-ert.sh)
  fi
  # loads_csv's own last entry is always the suite's test file itself (see
  # test-manifest.sh), so it is NOT appended again here.
  cmd+=(emacs -Q --batch -l ert "${load_args[@]}" -f "$target")

  local output tmp_out rc
  tmp_out="$(mktemp)"
  "${cmd[@]}" >"$tmp_out" 2>&1
  rc=$?
  # A pty-tier run's captured output carries real terminal escape codes
  # (the second frame is a live tty frame) which can include NUL bytes --
  # stripped here to avoid bash's own "ignored null byte" warning from a
  # plain $(...) capture, and because none of the parsing below needs them.
  output="$(tr -d '\0' <"$tmp_out")"
  rm -f "$tmp_out"

  local wall
  wall="$(parse_wall_seconds "$output")"
  [ -n "$wall" ] || wall="?"

  if ! parse_ert_summary "$output"; then
    echo "----- $name [$tier]: could not parse ERT summary from output -----" >&2
    printf '%s\n' "$output" | tail -20 >&2
    record_row "$name" "$tier" "?" "1" "?" "$wall" 0
    return
  fi

  local tests="$PARSED_TESTS" unexpected="$PARSED_UNEXPECTED" skipped="$PARSED_SKIPPED"
  # A duration-budget failure from run-ert-suite.sh (rc!=0 despite the
  # wrapped ERT run itself reporting 0 unexpected) must still fail the row.
  if [ "$rc" -ne 0 ] && [ "$unexpected" -eq 0 ]; then
    unexpected=1
    echo "----- $name [$tier]: run-ert-suite.sh reported failure (budget or wrapper) -----" >&2
    printf '%s\n' "$output" | tail -10 >&2
  fi

  local skip_ok=1
  if [ "$skipped" -gt "$expected_skips" ]; then
    skip_ok=0
  fi
  record_row "$name" "$tier" "$tests" "$unexpected" "$skipped" "$wall" "$skip_ok"
}

run_gui_row() {
  local name="$1" expected_skips="$2" loads_csv="$3" selector="$4"
  local test_file="modules/${name}.el"
  local -a extra=()
  if [ "$loads_csv" != "-" ]; then
    local IFS=','
    local -a loads=($loads_csv)
    for l in "${loads[@]}"; do
      extra+=("-l" "$l")
    done
  fi

  local start end wall output rc
  start="$(perl -MTime::HiRes=time -e 'printf "%.6f", time')"
  output="$(timeout 90 scripts/gui-ert.sh "$test_file" "$selector" "${extra[@]}" 2>&1)"
  rc=$?
  end="$(perl -MTime::HiRes=time -e 'printf "%.6f", time')"
  wall="$(perl -e "printf('%.2f', $end - $start)")"

  if printf '%s' "$output" | grep -q 'could not create a graphical frame'; then
    RESULT_LINES+=("$(printf '%-42s [%-5s] SKIPPED-GUI (no window server available)' "$name" "gui")")
    return
  fi

  local line
  line="$(printf '%s\n' "$output" | grep -Eo 'Ran [0-9]+ tests, [0-9]+ passed, [0-9]+ expected-failed, [0-9]+ failed, [0-9]+ skipped' | tail -1)"
  if [ -z "$line" ]; then
    echo "----- $name [gui]: could not parse gui-ert.sh summary -----" >&2
    printf '%s\n' "$output" | tail -20 >&2
    record_row "$name" "gui" "?" "1" "?" "$wall" 0
    return
  fi

  local tests failed skipped
  tests="$(printf '%s\n' "$line" | perl -ne '/Ran (\d+) tests/ and print $1')"
  failed="$(printf '%s\n' "$line" | perl -ne '/(\d+) failed/ and print $1')"
  skipped="$(printf '%s\n' "$line" | perl -ne '/(\d+) skipped/ and print $1')"

  if [ "$rc" -ne 0 ] && [ "$failed" -eq 0 ]; then
    failed=1
    echo "----- $name [gui]: gui-ert.sh exited non-zero with no failed test recorded -----" >&2
    printf '%s\n' "$output" | tail -10 >&2
  fi

  local skip_ok=1
  if [ "$skipped" -gt "$expected_skips" ]; then
    skip_ok=0
  fi
  record_row "$name" "gui" "$tests" "$failed" "$skipped" "$wall" "$skip_ok"
}

for row in "${MANIFEST[@]}"; do
  IFS='|' read -r name tier budget expected_skips loads target <<<"$row"
  case "$tier" in
    batch) [ "$TIER_ARG" = "batch" ] || [ "$TIER_ARG" = "all" ] || continue ;;
    pty)   [ "$TIER_ARG" = "pty" ]   || [ "$TIER_ARG" = "all" ] || continue ;;
    gui)   [ "$TIER_ARG" = "gui" ]   || [ "$TIER_ARG" = "all" ] || continue ;;
    *)
      echo "FATAL: manifest row for '$name' has unrecognized tier '$tier' -- refusing to silently drop it" >&2
      OVERALL_EXIT=1
      continue
      ;;
  esac

  case "$tier" in
    batch|pty)
      run_batch_or_pty_row "$name" "$tier" "$budget" "$expected_skips" "$loads" "$target"
      ;;
    gui)
      run_gui_row "$name" "$expected_skips" "$loads" "$target"
      ;;
    *)
      echo "FATAL: manifest row for '$name' has unrecognized tier '$tier' -- refusing to silently drop it" >&2
      OVERALL_EXIT=1
      ;;
  esac
done

echo
for line in "${RESULT_LINES[@]}"; do
  echo "$line"
done
echo
printf 'TOTAL tests=%s unexpected=%s skipped=%s\n' "$TOTAL_TESTS" "$TOTAL_UNEXPECTED" "$TOTAL_SKIPPED"

if [ "$MAIN_CHECKOUT" -eq 1 ]; then
  echo "(running from the main checkout: skip counts above the manifest's expected_skips_on_main fail the run)"
else
  echo "(running from a worktree: skip counts are not enforced, only unexpected failures are)"
fi

exit "$OVERALL_EXIT"
