#!/usr/bin/env bash
#
# startup-bench.sh -- reproducible Emacs startup benchmark for edmacs
#
# WHY THIS EXISTS
#   Every roadmap phase in this repo's performance work claims a number
#   (e.g. "saves 0.4s"). Without a committed, rerunnable way to reproduce
#   those numbers, the claims decay into folklore -- see
#   edmacs-straight-hygiene/phase-1, whose commit message claimed a result
#   "verified by a batch script" that could not have run against the
#   checkout it claimed to verify. This script is that rerunnable harness.
#
# THE --batch TRAP (read this before "fixing" this script to use --batch)
#   `emacs --batch` implies `-q`: it NEVER loads early-init.el or init.el,
#   and never sets `after-init-time`. A batch run of this config reports
#   something like ~0.006s and looks like a spectacular optimization result
#   -- it is actually measuring nothing. Interactive startup cost can only
#   be measured by actually running an interactive (`-nw`) Emacs through a
#   pty, which is what this script does via `script -q /dev/null`.
#
# WHY A PTY (`script`) AND NOT A PLAIN SUBPROCESS
#   `emacs -nw` needs a controlling terminal to create its frame; a bare
#   subprocess with redirected stdio has no tty and Emacs will refuse to
#   start (or behave differently). `script -q /dev/null <cmd...>` allocates
#   a pty and runs <cmd...> attached to it, with no session log kept
#   (BSD/macOS: the log path argument is mandatory, so we point it at
#   /dev/null). This is Darwin/BSD `script` syntax, matching this
#   environment. GNU/Linux's `script` takes the command differently:
#     script -qc "emacs -nw ..." /dev/null
#   If this script is ever ported to Linux, swap the invocation accordingly.
#   `script` itself adds a small amount of overhead on top of Emacs's own
#   startup; the -Q control run below exists specifically to bound that
#   overhead so it doesn't get misread as "config load time".
#
# HOW EACH TIMED RUN IS MADE TO EXIT ON ITS OWN
#   `script` obscures Emacs's own reported timing (and there is no timing
#   to report anyway under -nw), so this script times full wall-clock
#   process duration from outside. For that duration to reflect only
#   startup, each timed Emacs process must quit itself right after startup
#   finishes. This is done by adding a function to `emacs-startup-hook`
#   (appended, so it runs after this config's own startup-hook work) that
#   calls `(kill-emacs)`. `emacs-startup-hook` runs once, after the init
#   file has fully loaded and the initial frame exists, which is exactly
#   the point at which "startup" is over. This was checked empirically
#   against the installed Emacs (31.1): plain command-line --eval switches
#   run during `command-line-1`, which happens strictly AFTER the init
#   file has been loaded but BEFORE `emacs-startup-hook` is run -- so an
#   --eval that does `(add-hook 'emacs-startup-hook #'kill-emacs t)` reaches
#   the hook in time, and does not race native-comp's async compilation
#   queue or straight's own modification checks -- those run synchronously
#   as part of module loading (see the --eval section below: they can, in
#   fact, make module loading itself dramatically slower depending on
#   `straight-check-for-modifications`), so by the time emacs-startup-hook
#   fires they have already finished one way or the other; nothing is
#   left running in the background for the kill to race against, and any
#   package rebuild a run actually triggers has already completed on disk
#   (a normal, idempotent straight build) before the process exits.
#   A `-Q` control run whose min/mean/max come back far above tens of
#   milliseconds means this kill strategy is stalling, not that the
#   harness is somehow measuring something real -- treat that as a bug in
#   this script, not a finding about Emacs.
#
# A CAVEAT THIS KILL STRATEGY IMPLIES: A FRESH CHECKOUT/WORKTREE CAN LOOK
# PATHOLOGICALLY SLOW, AND THIS SCRIPT CANNOT SELF-HEAL THAT
#   straight defers saving its own build cache (`straight/build-cache.el`,
#   git-ignored, per-checkout local state) to `post-command-hook` in an
#   interactive session -- it is NOT written synchronously as each package
#   finishes building (confirmed by reading straight.el's
#   `straight--transaction-finalize-at-top-level`: it hooks
#   `kill-emacs-hook` only when `noninteractive`, i.e. under `--batch`;
#   an `-nw` session like every one this script runs instead hooks
#   `post-command-hook`, which fires only once the command loop has
#   processed its first command). This script's `emacs-startup-hook`-based
#   kill fires BEFORE that command loop ever starts, so straight's build
#   cache is never persisted by ANY run this script makes -- confirmed
#   empirically: in a freshly created worktree whose build-cache.el had
#   only ever seen one bootstrap entry, repeated runs of this script kept
#   rebuilding the same ~25 packages every single time (mean >20s, not
#   ~1.5s), because the cache never had a chance to record that they were
#   already built. Pointed at this repo's main checkout instead -- whose
#   build-cache.el was already fully populated from ordinary interactive
#   use, i.e. a session that was allowed to run a real command and let
#   `post-command-hook` fire -- the exact same script immediately measured
#   the documented ~1.47s baseline. The fix is not in this script: open
#   `emacs -nw --init-directory=<dir>` by hand ONCE (not through this
#   script) and let it sit past its first real keystroke so straight can
#   save its cache, before trusting this script's numbers for that
#   directory. A "Real config" mean stuck an order of magnitude above the
#   documented baseline, with "Building ..." lines visible if you drop
#   the `>/dev/null 2>&1` redirect for a manual look, is this condition --
#   not a startup regression.
#
# WHY A PLAIN --eval IS TOO LATE, AND WHAT THIS SCRIPT DOES INSTEAD
#   The naive design threads --eval straight into the timed Emacs command
#   line. That is provably too late for the two things this script needs
#   --eval for, and this was confirmed empirically, not assumed:
#
#     - use-package-compute-statistics: `use-package` is a macro. With
#       this repo's modules loaded as plain .el source (checked: only the
#       vendored claude-repl module ships .elc files, the top-level
#       modules/*.el do not), the macro re-expands on every startup by
#       reading `use-package-compute-statistics` at the moment each
#       module file is loaded -- so the variable must be t before
#       init.el's `load-module` calls run. A plain --eval runs during
#       `command-line-1`, which happens strictly AFTER the init file has
#       already fully loaded (this is the same ordering the kill-hook
#       trick above relies on). A stats run using a plain --eval to set
#       the flag reports an empty `use-package-statistics` table --
#       confirmed by actually running it that way.
#
#     - straight-check-for-modifications: the whole point of --eval'ing
#       this knob is to change how straight checks each package during
#       init.el's own `straight-use-package`/`use-package` calls. Setting
#       it after init.el has already loaded is a no-op on that startup --
#       confirmed empirically: a real-config run with a plain trailing
#       --eval "(setq straight-check-for-modifications '(check-on-save))"
#       measured within noise of a run with no --eval at all.
#
#   The fix used for BOTH --eval and --stats, via write_shim_init_dir()
#   below: generate a throwaway directory containing a shim
#   `early-init.el` *and* a shim `init.el`, and hand the directory to
#   Emacs via `--init-directory=<shim>`. Both shim files are required --
#   this was the single hardest thing to get right in this script, found
#   by actually reading Emacs 31.1's startup.el, not by assuming:
#   `--init-directory` sets `user-emacs-directory` early, and Emacs loads
#   `<user-emacs-directory>/early-init.el` from that location, so the
#   shim early-init.el runs and can apply the extra form and then reset
#   `user-emacs-directory` back to this repo's real root. BUT Emacs
#   decides which directory it will look in for *init.el* itself
#   (internally, `startup-init-directory`) from `user-emacs-directory`'s
#   value ONCE, before early-init.el runs, and does not re-derive it
#   afterward -- resetting `user-emacs-directory` inside early-init.el
#   changes where module files, custom-file, and straight's bootstrap
#   path resolve to (those all run later, after the reset, and use the
#   variable directly), but it does NOT change where Emacs itself goes
#   looking for init.el. An earlier version of this script only shipped
#   the shim early-init.el and assumed the reset would redirect Emacs to
#   this repo's real init.el; instead Emacs found no init.el in the
#   throwaway shim directory at all, silently skipped the entire config
#   (no modules, no packages), and finished in ~0.06s -- a broken harness
#   that looked, at a glance, like a real (and wildly wrong) result. The
#   fix is the second shim file: a one-line `init.el` in the same
#   throwaway directory that just `load`s this repo's real init.el
#   explicitly, sidestepping Emacs's frozen directory lookup entirely.
#   No tracked file (init.el, early-init.el, or anything under modules/)
#   is ever edited; both shim files live entirely in a throwaway temp
#   directory created and removed by this script. When there is no
#   --eval, the real-config run skips the shim entirely and uses
#   --init-directory=$REPO_ROOT directly, since there is nothing that
#   needs to run early in that case.
#
# HOW THE STATS REPORT IS EXTRACTED
#   The interactive `use-package-report` command opens a tabulated-list
#   buffer -- useless with no display in -nw/pty. Instead, a trailing
#   --eval walks the `use-package-statistics` hash table directly, sorts
#   entries by `use-package-statistics-time` descending, and writes a
#   plain-text table via `with-temp-file` to a throwaway file path (NOT
#   captured from the pty's stdout stream -- writing straight to a file
#   sidesteps needing to strip pty-introduced control characters, and is
#   the more robust choice actually used here). The wrapper script then
#   cats that file (still passed through `tr -d '\r'` defensively, in
#   case a future change routes output through the pty after all) and
#   deletes the temp file/dir afterward.
#
# BASELINE TABLE (dated -- diff future runs against this)
#   Recorded 2026-09-01, on the installed Emacs 31.1, this repo's config,
#   with straight's packages already built (a cold straight bootstrap
#   inflates the first run enormously -- see the "COLD BUILD CACHE"
#   caveat below -- and is not part of steady-state startup cost). An
#   earlier draft of this table stated two numbers ("rustic is costliest
#   at ~0.8-0.9s" and "--eval check-on-save costs ~7.8s more") that a
#   plain re-run of this very script on this very machine directly
#   contradicts -- neither had actually been re-verified against the
#   script being committed. Both are corrected below from runs made
#   against this script as delivered. This correction is itself the
#   point of the exercise: don't let either of these numbers go stale
#   the same way.
#     Real config   (5-10 runs, --no-control): mean 1.59-1.69s, i.e.
#       within roughly 5-13% of the ~1.50s figure this script was
#       written to reproduce. Treat a mean outside that band as machine
#       load, not a regression, unless it repeats across a re-run with
#       the machine otherwise quiet.
#     Control (-Q)  (5 runs): ~0.04-0.05s -- two orders of magnitude
#       below the real-config row, confirming the harness measures
#       config load rather than process-spawn/pty overhead.
#     Costliest single use-package form (--stats): NOT reliably rustic.
#       Three separate --stats runs on this machine all named `envrc`
#       top (0.66-0.80s), with `rustic` a distant second (0.21-0.35s).
#       Root cause, confirmed by reading modules/core.el: this config
#       calls `(envrc-global-mode)` in envrc's :config, which turns the
#       mode on in every already-existing buffer immediately, and each
#       activation shells out to the `direnv` binary -- so its cost is
#       dominated by fork+exec latency and by however many directories
#       up the tree `direnv` has to walk looking for a `.envrc`, which
#       depends on the machine's own home directory contents, not on
#       this repo. `rustic`'s :config similarly shells out (rustc/cargo/
#       rust-analyzer discovery) and is comparably load-sensitive.
#       Practical implication: whichever of these two external-process
#       packages tops this report is a property of the machine running
#       it (what's installed, what's on $PATH, what dotfiles exist under
#       $HOME), not a fixed property of this config -- don't hardcode
#       an expected top entry or an expected value for this row; take
#       several back-to-back --stats runs with the machine otherwise
#       quiet and treat the result as "whichever external-process
#       package is currently costliest here", not a specific number.
#     --eval "(setq straight-check-for-modifications '(check-on-save))"
#       SAVES roughly 0.8s (measured mean ~0.80s, down from the ~1.59-
#       1.69s baseline above) -- the opposite of an earlier, unverified
#       claim that this knob cost several seconds more. Root cause,
#       confirmed by re-running with the pty's raw output left
#       unredirected and grepping for "Building": no rebuilds are
#       triggered by this knob on this checkout; the default modification
#       check (this config sets no explicit value, so straight's own
#       default applies) does comparatively expensive work on every
#       startup that `check-on-save` skips in favor of relying on
#       save-hooks. This makes it a genuinely good candidate for a
#       config-level default -- but that decision belongs to whichever
#       phase is chartered to change init.el, not to this benchmarking
#       phase, which only reports what it measures.
#   Run-to-run variance from OS scheduling, thermal state, disk cache
#   warmth, and background native-comp finishing across runs is real;
#   N=5 mitigates but does not eliminate it. A single outlier run is not
#   a regression -- rerun before trusting a delta smaller than a few
#   tenths of a second.
#
# USAGE
#   scripts/startup-bench.sh                    # 5 real-config runs + -Q control
#   scripts/startup-bench.sh -n 10               # more runs, tighter mean
#   scripts/startup-bench.sh --no-control        # skip the -Q run, faster iteration
#   scripts/startup-bench.sh --eval '(setq straight-check-for-modifications (quote (check-on-save)))'
#                                                 # A/B one Lisp knob without editing the config
#   scripts/startup-bench.sh --stats             # per-package use-package timing report
#
#   No GUI is ever created (every invocation passes -nw) and no controlling
#   terminal is required beyond what `script` itself provides, so this runs
#   fine over ssh, in tmux, or from a CI runner with no tty at all:
#     ssh headless-box 'cd edmacs && scripts/startup-bench.sh -n 3'
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

# Validate --runs/-n right after parsing, before it is ever used as an
# arithmetic loop bound. Two failure modes were observed with no
# validation: a non-numeric value (e.g. `--runs abc`) blew up deep inside
# a `for ((...))` arithmetic context with a raw `bash: abc: unbound
# variable` under `set -u`, naming neither the flag nor a valid value;
# and `--runs 0` (or a negative count) produced a loop that never ran,
# leaving the times array empty and misreporting "every real-config run
# failed -- see warnings above" for a case where no run was ever
# attempted and no warnings exist. Rejecting anything but a positive
# integer here converts both into one clear, actionable message.
if ! [[ "$RUNS" =~ ^[1-9][0-9]*$ ]]; then
  echo "error: --runs must be a positive integer, got '$RUNS'" >&2
  exit 1
fi

# Wall-clock timestamp with sub-second precision, portable across the
# GNU-vs-BSD `date` split (BSD date has no %N) and across bash versions
# (EPOCHREALTIME needs bash >= 5, not guaranteed to be /bin/bash on macOS).
# perl ships with macOS regardless of either of those, so it's the most
# reliable common denominator here.
now() {
  perl -MTime::HiRes=time -e 'printf "%.6f", time'
}

elapsed_since() {
  # $1 = start timestamp (from now()); prints elapsed seconds to stdout.
  local start="$1" end
  end=$(now)
  perl -e "printf('%.6f', $end - $start)"
}

# Appended (non-nil third arg) so this runs after this config's own
# emacs-startup-hook work (resetting gc-cons-threshold, the startup
# message) rather than pre-empting it -- see header comment for why this
# hook, and not a plain trailing --eval, is what reliably ends each run
# right after startup finishes.
KILL_EVAL='(add-hook (quote emacs-startup-hook) (function kill-emacs) t)'

cleanup_tmp() {
  # Written as an if/fi (not a bare `cond && cmd`) on purpose: under
  # `set -e`, a bare `[[ ... ]] && rm ...` whose test is false returns
  # exit status 1, and since this runs as an EXIT trap, THAT would become
  # the whole script's exit status even after a fully successful run.
  if [[ -n "${REAL_CONFIG_SHIM_DIR:-}" && -d "$REAL_CONFIG_SHIM_DIR" ]]; then
    rm -rf "$REAL_CONFIG_SHIM_DIR"
  fi
}
trap cleanup_tmp EXIT

# Run one interactive Emacs through a pty and print elapsed seconds on
# stdout. Extra args are passed straight through to Emacs argv (as a bash
# array, so nested quotes/parens in --eval strings survive intact -- macOS
# `script` execs the given command directly, it does not re-parse it
# through a shell).
#
# `script` itself can occasionally fail to attach its pty (seen in some
# sandboxed/nested-shell contexts as "tcgetattr/ioctl: ... on socket") --
# a transient failure of `script`, unrelated to Emacs or this config. A
# naive "|| true" around that call would silently record a bogus
# near-zero elapsed time for a run that never actually measured Emacs at
# all. Instead, retry a bounded number of times and only give up (return
# non-zero, print nothing) once every attempt has failed, so the caller
# can skip the sample instead of polluting the average with a fake zero.
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

# Emits (on stdout) the --init-directory value to use for the timed
# real-config run: the repo root directly when there is no extra --eval,
# or a generated shim directory that applies EXTRA_EVAL before delegating
# to the real config, when there is -- see header comment for why a plain
# --eval on the Emacs command line cannot do this in time.
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

# Writes a shim early-init.el AND init.el into DIR (both required -- see
# below), applying EXTRA_FORM before delegating to this repo's real
# config. Used by both --eval and --stats.
#
# CRITICAL, verified against Emacs 31.1's own startup.el source: Emacs
# decides which directory it will look in for init.el (internally,
# `startup-init-directory`) ONCE, from `user-emacs-directory`'s value
# BEFORE early-init.el runs -- and does NOT re-derive it afterward. Only
# a couple of specific things (native-comp-eln-load-path,
# package-user-dir) get explicitly "amended again" after early-init.el
# runs; init.el's own lookup path is not one of them. This was found the
# hard way: an earlier version of this script only reset
# `user-emacs-directory` inside a shim early-init.el and relied on Emacs
# to pick up REPO_ROOT/init.el on its own -- it never did, so every
# --eval/--stats run silently skipped the entire real config (no modules,
# no packages) and finished in ~0.06s, which looked like a real (and
# wildly wrong) result instead of a broken harness. The fix: place an
# init.el in the shim directory too, that explicitly `load`s the real
# one. `user-emacs-directory`-relative lookups *inside* the real init.el
# and its modules (module files, custom-file, straight's bootstrap path)
# still resolve correctly, because those run later, after early-init.el
# has already reset `user-emacs-directory` to REPO_ROOT.
write_shim_init_dir() {
  local dir="$1" extra_form="$2"
  cat >"$dir/early-init.el" <<EOF
;; Generated by scripts/startup-bench.sh -- not part of the config.
;; See write_shim_init_dir()'s comment in the script for why both this
;; file and the init.el next to it exist.
$extra_form
(setq user-emacs-directory (file-name-as-directory "$REPO_ROOT"))
(load (expand-file-name "early-init.el" user-emacs-directory) nil 'nomessage)
EOF
  cat >"$dir/init.el" <<EOF
;; Generated by scripts/startup-bench.sh -- not part of the config.
;; See write_shim_init_dir()'s comment in the script: Emacs's own
;; init-file lookup does not follow user-emacs-directory changes made in
;; early-init.el, so this file exists purely to \`load' the real one.
(load (expand-file-name "init.el" user-emacs-directory) nil 'nomessage)
EOF
}

# Warn (never fail) when straight's build cache looks unpopulated for this
# checkout -- see the "A CAVEAT THIS KILL STRATEGY IMPLIES" header section.
# This script's own kill-on-emacs-startup-hook trick means NO run it makes
# can ever be the one that warms straight's cache (that requires reaching
# `post-command-hook`, which only fires after a real command loop starts).
# So a fresh worktree/clone silently measures a ~15-20x-inflated mean with
# no signal that the harness -- not Emacs -- is in a degenerate state,
# unless something here says so. This probe cannot fix that (the fix has
# to happen outside this script, per the header); it only makes the
# degenerate state visible instead of silent.
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
  this script. See the header's "A CAVEAT THIS KILL STRATEGY IMPLIES"
  section for why this script cannot do that warm-up run itself.

WARN
    return
  fi
  # Each built package contributes one ":local-repo" key to the cache's
  # single-line hash-table literal, so `grep -c` (which counts matching
  # LINES) always reports 0 or 1 here regardless of package count -- this
  # must be `grep -o | wc -l` to count occurrences instead.
  # `grep -o` with no match exits 1, and `set -o pipefail` above would
  # otherwise propagate that through the pipe and kill the whole script
  # under `set -e` on a cache with zero matches -- `|| true` on the grep
  # itself (not on the pipeline as a whole) absorbs exactly that case.
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
  script. See the header's "A CAVEAT THIS KILL STRATEGY IMPLIES" section.

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

  # Shim early-init.el + init.el: set the statistics flag before
  # delegating to the real config -- see write_shim_init_dir()'s comment
  # for why both files (not just early-init.el) are required. Any --eval
  # form is prepended ahead of the statistics flag so `--stats --eval ...`
  # actually A/Bs the per-package report under that knob instead of
  # silently benching the plain config while claiming otherwise -- an
  # earlier version of this script accepted both flags together but only
  # ever wired EXTRA_EVAL into the timed real-config path, never into this
  # one, so `--stats --eval ...` looked accepted but the --eval had no
  # effect on the report at all.
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
    # `return`, not `exit`: the RETURN trap set above is this function's
    # only cleanup for tmp_initdir/tmp_report (the top-level cleanup_tmp
    # EXIT trap only ever touches REAL_CONFIG_SHIM_DIR, which this
    # function never sets). A bash RETURN trap fires when the function
    # actually returns -- it does NOT fire if the function instead calls
    # `exit` and takes down the whole shell from inside. `return 1` here
    # lets the trap run, then propagates the failure: under `set -e`, the
    # top-level `run_stats_mode` call (below) still terminates the script
    # with a non-zero status once this returns non-zero.
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
