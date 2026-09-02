#!/usr/bin/env bash
#
# startup-bench.sh -- reproducible interactive-startup benchmark for edmacs.
#
# `emacs --batch' implies -q and never loads the init files, so startup can
# only be timed with a real `-nw' Emacs run through a pty (`script -q
# /dev/null', BSD syntax; on Linux use `script -qc "..." /dev/null'). Each run
# quits itself from `emacs-startup-hook'. A -Q control run bounds the
# pty/process overhead; if it is not on the order of milliseconds, the kill
# strategy is stalling and the numbers are a harness bug.
#
# A fresh checkout looks pathologically slow: straight saves its build cache
# from `post-command-hook', which never fires under this kill strategy, so
# packages rebuild on every run. Open `emacs -nw --init-directory=<dir>' by
# hand once and press a key before trusting numbers for that directory.
#
# --eval and --stats need their form evaluated before init.el loads (the
# use-package macro reads use-package-compute-statistics at expansion, and
# straight-check-for-modifications is consulted during bootstrap), so they go
# through a shim --init-directory; see write_shim_init_dir.
#
# Expect run-to-run variance of a few tenths of a second; rerun before
# trusting a small delta.
#
# USAGE
#   scripts/startup-bench.sh                    # 5 real-config runs + -Q control
#   scripts/startup-bench.sh -n 10
#   scripts/startup-bench.sh --no-control
#   scripts/startup-bench.sh --eval '(setq straight-check-for-modifications (quote (check-on-save)))'
#   scripts/startup-bench.sh --stats             # per-package use-package timing
#
#   Runs fine over ssh or in CI: no GUI, and `script' provides the tty.
#
set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

RUNS=5
EXTRA_EVAL=""
STATS_MODE=0
RUN_CONTROL=1

usage() {
  cat <<'USAGE'
Usage: startup-bench.sh [options]

  -n, --runs N     Number of timed runs per configuration (default: 5)
      --eval LISP  Extra Lisp form evaluated before the real config's own
                   early-init.el/init.el run (see header for why it must be
                   this early, not a plain trailing --eval). Lets you A/B
                   one knob without editing the config, e.g.:
                     --eval "(setq straight-check-for-modifications '(check-on-save))"
                   Combine with --stats to A/B the per-package report too.
      --stats      Instead of timing, run one startup with
                   use-package-compute-statistics enabled and print a
                   per-package elapsed-time report, sorted descending.
      --no-control Skip the -Q control run (faster iteration; the control
                   run is what proves the harness measures config load and
                   not process-spawn overhead, so don't skip it by default).
  -h, --help       Show this help and exit.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--runs)
      if [[ $# -lt 2 ]]; then
        echo "error: -n/--runs requires a value, e.g. --runs 5" >&2
        exit 1
      fi
      RUNS="$2"
      shift 2
      ;;
    --eval)
      if [[ $# -lt 2 ]]; then
        echo "error: --eval requires a value, e.g. --eval '(setq foo t)'" >&2
        exit 1
      fi
      EXTRA_EVAL="$2"
      shift 2
      ;;
    --stats)
      STATS_MODE=1
      shift
      ;;
    --no-control)
      RUN_CONTROL=0
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

# Validate before use as a loop bound: a bad value otherwise surfaces as an
# opaque arithmetic error or an empty-results misreport.
if ! [[ "$RUNS" =~ ^[1-9][0-9]*$ ]]; then
  echo "error: --runs must be a positive integer, got '$RUNS'" >&2
  exit 1
fi

# Sub-second timestamp portable across BSD date (no %N) and bash < 5 (no
# EPOCHREALTIME); perl ships with macOS.
now() {
  perl -MTime::HiRes=time -e 'printf "%.6f", time'
}

elapsed_since() {
  # $1 = start timestamp (from now()); prints elapsed seconds to stdout.
  local start="$1" end
  end=$(now)
  perl -e "printf('%.6f', $end - $start)"
}

# Appended so it runs after the config's own startup-hook work.
KILL_EVAL='(add-hook (quote emacs-startup-hook) (function kill-emacs) t)'

cleanup_tmp() {
  # if/fi, not `cond && cmd': a false test would become the EXIT trap's
  # (and so the script's) exit status under set -e.
  if [[ -n "${REAL_CONFIG_SHIM_DIR:-}" && -d "$REAL_CONFIG_SHIM_DIR" ]]; then
    rm -rf "$REAL_CONFIG_SHIM_DIR"
  fi
}
trap cleanup_tmp EXIT

# Run one interactive Emacs through a pty; print elapsed seconds. Args go
# straight to Emacs argv. `script' occasionally fails to attach its pty in
# sandboxed shells; retry rather than record a bogus near-zero sample.
run_timed() {
  local max_attempts=5 attempt=1 start
  while (( attempt <= max_attempts )); do
    start=$(now)
    if script -q /dev/null emacs -nw "$@" >/dev/null 2>&1; then
      elapsed_since "$start"
      return 0
    fi
    echo "warning: script/emacs invocation failed (attempt $attempt/$max_attempts) -- retrying" >&2
    attempt=$((attempt + 1))
  done
  echo "warning: giving up after $max_attempts failed attempts -- skipping this sample" >&2
  return 1
}

# Reads one float per line on stdin; prints "min mean max" (3 decimals).
summarize() {
  awk '
    { sum += $1; n++; if (n == 1 || $1 < min) min = $1; if (n == 1 || $1 > max) max = $1 }
    END {
      if (n == 0) { print "0.000 0.000 0.000"; exit }
      printf "%.3f %.3f %.3f\n", min, sum / n, max
    }
  '
}

print_table_row() {
  # $1 = label, $2 = "min mean max"
  local label="$1" mmm="$2"
  local min mean max
  read -r min mean max <<<"$mmm"
  printf "  %-16s min %8ss   mean %8ss   max %8ss\n" "$label" "$min" "$mean" "$max"
}

# --init-directory for the real-config run: the repo root, or a shim that
# applies EXTRA_EVAL first.
REAL_CONFIG_SHIM_DIR=""

build_real_config_init_dir() {
  if [[ -z "$EXTRA_EVAL" ]]; then
    printf '%s' "$REPO_ROOT"
    return
  fi
  REAL_CONFIG_SHIM_DIR=$(mktemp -d "${TMPDIR:-/tmp}/startup-bench-eval-initdir.XXXXXX")
  write_shim_init_dir "$REAL_CONFIG_SHIM_DIR" "$EXTRA_EVAL"
  printf '%s' "$REAL_CONFIG_SHIM_DIR"
}

# Write a shim early-init.el AND init.el into DIR that apply EXTRA_FORM and
# then load the real config. Both are needed: Emacs fixes the directory it
# looks in for init.el before early-init.el runs and never re-derives it, so
# resetting user-emacs-directory in a shim early-init.el alone leaves Emacs
# finding no init.el and silently skipping the whole config.
write_shim_init_dir() {
  local dir="$1" extra_form="$2"
  cat >"$dir/early-init.el" <<EOF
;; Generated by scripts/startup-bench.sh -- not part of the config.
$extra_form
(setq user-emacs-directory (file-name-as-directory "$REPO_ROOT"))
(load (expand-file-name "early-init.el" user-emacs-directory) nil 'nomessage)
EOF
  cat >"$dir/init.el" <<EOF
;; Generated by scripts/startup-bench.sh -- not part of the config.
(load (expand-file-name "init.el" user-emacs-directory) nil 'nomessage)
EOF
}

# Warn when straight's build cache looks unpopulated; see the header. This
# script can never warm it itself.
check_build_cache_freshness() {
  local cache_file="$REPO_ROOT/straight/build-cache.el" entries
  if [[ ! -f "$cache_file" ]]; then
    cat >&2 <<WARN
warning: $cache_file does not exist yet.
  straight has never bootstrapped in this checkout, so the very first
  startup below will build every package from scratch. Timings from this
  invocation will NOT reflect steady-state startup cost. Fix: run
  \`emacs -nw --init-directory=$REPO_ROOT\` by hand ONCE (not through this
  script), let it sit past its first real keystroke, then exit and re-run
  See the script header.

WARN
    return
  fi
  # The cache is a single-line hash-table literal, so count occurrences with
  # grep -o | wc -l. `|| true' on the grep keeps pipefail from killing the
  # script when there are zero matches.
  entries=$( (grep -o ':local-repo' "$cache_file" 2>/dev/null || true) | wc -l | tr -d ' ')
  entries=${entries:-0}
  if (( entries < 10 )); then
    cat >&2 <<WARN
warning: $cache_file has only $entries package entries recorded.
  This checkout's build cache looks freshly created (this repo's config
  installs 100+ packages), which means straight cannot yet trust most
  packages are already built and will rebuild them on every run below --
  expect a mean an order of magnitude above the documented baseline; that
  is a cold cache, not a startup regression. Fix: run
  \`emacs -nw --init-directory=$REPO_ROOT\` by hand ONCE (not through this
  script), let it sit past its first real keystroke so straight's
  post-command-hook can persist the cache, then exit and re-run this
  script. See the script header.

WARN
  fi
}

run_real_config_bench() {
  check_build_cache_freshness
  echo "Extra --eval: ${EXTRA_EVAL:-none}"
  echo "Runs: $RUNS"
  echo

  local init_dir
  init_dir=$(build_real_config_init_dir)
  local args=(--init-directory="$init_dir" --eval "$KILL_EVAL")

  local times=() i t
  for ((i = 1; i <= RUNS; i++)); do
    if t=$(run_timed "${args[@]}"); then
      times+=("$t")
    fi
  done
  if [[ "${#times[@]}" -eq 0 ]]; then
    echo "error: every real-config run failed -- see warnings above" >&2
    exit 1
  fi

  local real_summary
  real_summary=$(printf '%s\n' "${times[@]}" | summarize)
  echo "Results (${#times[@]}/$RUNS runs succeeded):"
  print_table_row "Real config:" "$real_summary"

  if [[ "$RUN_CONTROL" -eq 1 ]]; then
    local control_times=() ct
    for ((i = 1; i <= RUNS; i++)); do
      if ct=$(run_timed -Q --eval "$KILL_EVAL"); then
        control_times+=("$ct")
      fi
    done
    if [[ "${#control_times[@]}" -eq 0 ]]; then
      echo "error: every -Q control run failed -- see warnings above" >&2
      exit 1
    fi
    local control_summary
    control_summary=$(printf '%s\n' "${control_times[@]}" | summarize)
    print_table_row "Control (-Q) (${#control_times[@]}/$RUNS):" "$control_summary"
    echo
    echo "If Control (-Q) is not on the order of milliseconds, the kill"
    echo "strategy above is stalling -- treat that as a harness bug, not"
    echo "a real result (see header comment)."
  fi
}

run_stats_mode() {
  check_build_cache_freshness
  echo "Extra --eval: ${EXTRA_EVAL:-none}"

  local tmp_initdir tmp_report
  tmp_initdir=$(mktemp -d "${TMPDIR:-/tmp}/startup-bench-initdir.XXXXXX")
  tmp_report=$(mktemp "${TMPDIR:-/tmp}/startup-bench-stats.XXXXXX")
  trap 'rm -rf "$tmp_initdir" "$tmp_report"' RETURN

  # Any --eval form goes ahead of the statistics flag so `--stats --eval'
  # A/Bs the report under that knob.
  local stats_form="(setq use-package-compute-statistics t)"
  local combined_form="$stats_form"
  if [[ -n "$EXTRA_EVAL" ]]; then
    combined_form="$EXTRA_EVAL
$stats_form"
  fi
  write_shim_init_dir "$tmp_initdir" "$combined_form"

  local report_eval
  report_eval=$(cat <<EOF
(let ((rows nil))
  (maphash (lambda (pkg stats)
             (push (cons pkg (use-package-statistics-time stats)) rows))
           use-package-statistics)
  (setq rows (sort rows (lambda (a b) (> (cdr a) (cdr b)))))
  (with-temp-file "$tmp_report"
    (dolist (row rows)
      (insert (format "%-30s %.3f\n" (car row) (cdr row))))))
EOF
)

  echo "Running one instrumented startup (use-package-compute-statistics)..."
  local attempt=1 max_attempts=5 succeeded=0
  while (( attempt <= max_attempts )); do
    if script -q /dev/null emacs -nw --init-directory="$tmp_initdir" \
         --eval "$report_eval" --eval "$KILL_EVAL" >/dev/null 2>&1; then
      succeeded=1
      break
    fi
    echo "warning: script/emacs invocation failed (attempt $attempt/$max_attempts) -- retrying" >&2
    attempt=$((attempt + 1))
  done
  if [[ "$succeeded" -ne 1 ]]; then
    echo "error: giving up after $max_attempts failed attempts" >&2
    # `return', not `exit': the RETURN trap is this function's only cleanup
    # and does not fire on exit.
    return 1
  fi

  echo
  echo "=== use-package statistics, sorted by elapsed seconds (descending) ==="
  if [[ -s "$tmp_report" ]]; then
    tr -d '\r' <"$tmp_report"
  else
    echo "(no statistics captured -- see script header for how --stats works" \
         "and what to check if this is empty)"
  fi
}

if [[ "$STATS_MODE" -eq 1 ]]; then
  run_stats_mode
else
  run_real_config_bench
fi
