#!/usr/bin/env bash
#
# verify-gc-and-unbounded-state.sh -- structural verification for
# edmacs-performance/phase-6-gc-and-unbounded-state
#
# WHAT THIS CHECKS AND WHY IT EXISTS
#   Phase 6 replaces a blocking, frame-focus-dependent GC hook with
#   gcmh-mode, raises gc-cons-threshold/undo-limit/undo-strong-limit for a
#   five-language lsp-mode workload, adds an early-init.el fallback timer
#   independent of emacs-startup-hook, and drops undo-tree for native
#   undo-redo. scripts/gc-session-bench.sh covers AC6 (the gcs-done
#   comparison) but nothing else in this phase had a rerunnable, PASS/FAIL,
#   exit-code-driven check -- following the same pattern
#   verify-redisplay-settings.sh and verify-lsp-and-completion-io.sh already
#   established for phases 4 and 5 in this roadmap, this script closes that
#   gap for AC1, AC2 (steady state), AC3, AC4, and AC5.
#
#   NOT (re-)checked here: AC2's deliberate-error fallback path (a
#   mid-init error before emacs-startup-hook registers) requires waiting
#   out the real 20s idle timer in a throwaway process and is exercised
#   manually per the phase record -- doing it here on every run would make
#   this script slow and flaky in CI-like contexts for a path that is
#   fundamentally about wall-clock idle time, not structural state. What IS
#   checked here is the fallback's *structure*: the guarded timer call is
#   present verbatim in early-init.el, and the guard/reset values match
#   what the phase specifies. AC6 (gcs-done improvement) is intentionally
#   left to gc-session-bench.sh, which already exists for exactly that.
#
# USAGE
#   scripts/verify-gc-and-unbounded-state.sh [path-to-edmacs-checkout]
#   Defaults to the checkout containing this script. Exits 0 if every
#   check passes, 1 otherwise, printing PASS/FAIL per line.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="${1:-$(cd "$SCRIPT_DIR/.." && pwd)}"

if [[ ! -f "$REPO_ROOT/init.el" || ! -f "$REPO_ROOT/early-init.el" ]]; then
  echo "error: $REPO_ROOT does not look like an edmacs checkout (no init.el/early-init.el)" >&2
  exit 2
fi

FAILED=0

pass() { echo "PASS: $1"; }
fail() { echo "FAIL: $1"; FAILED=1; }

# ---------------------------------------------------------------------------
# 1) Static/structural greps -- no Emacs process needed.
# ---------------------------------------------------------------------------

# AC1: the old blocking, frame-focus-dependent hook must be gone entirely
# as *code*, not merely as history in a comment explaining why it was
# removed (init.el's own header comment for the gcmh change names it for
# exactly that reason) -- so this greps for it actually being wired up
# (add-hook/add-function/add-variable-watcher onto it), excluding comment
# lines, rather than for the bare string anywhere in the file.
if grep -vE '^\s*;;' "$REPO_ROOT/init.el" \
   | grep -qE "(add-hook|add-function|add-variable-watcher)[^)]*after-focus-change-function"; then
  fail "after-focus-change-function is still wired up as a live hook in init.el"
else
  pass "after-focus-change-function is no longer wired up as a live hook in init.el"
fi

# AC1 (structural underpinning): gcmh's own mechanism must be command/timer
# driven, not frame-focus driven, for the emacsclient -t claim to hold.
GCMH_EL="$(find "$REPO_ROOT/straight/repos/gcmh" -iname 'gcmh.el' 2>/dev/null | head -1)"
if [[ -z "$GCMH_EL" ]]; then
  fail "could not locate gcmh.el under straight/repos to verify its GC-trigger mechanism"
elif grep -q "frame-focus-state\|after-focus-change-function" "$GCMH_EL"; then
  fail "gcmh.el references frame-focus machinery -- AC1's TTY-frame claim would not hold"
else
  pass "gcmh.el has no frame-focus dependency (uses pre/post-command-hook + a plain timer)"
fi

# AC2 fallback structure: a guarded one-shot idle timer, armed in
# early-init.el before straight's bootstrap or anything else in init.el
# runs, that only fires if gc-cons-threshold is still most-positive-fixnum.
if grep -q "run-with-idle-timer" "$REPO_ROOT/early-init.el" \
   && grep -q "most-positive-fixnum" "$REPO_ROOT/early-init.el" \
   && grep -qE "eq gc-cons-threshold most-positive-fixnum" "$REPO_ROOT/early-init.el"; then
  pass "early-init.el has a guarded fallback idle timer for gc-cons-threshold"
else
  fail "early-init.el is missing the guarded fallback idle timer (run-with-idle-timer + eq most-positive-fixnum guard)"
fi

# AC4: undo-tree must be fully removed from the lockfile, not just unused.
if grep -q "undo-tree" "$REPO_ROOT/straight/versions/default.el" 2>/dev/null; then
  fail "undo-tree is still pinned in straight/versions/default.el"
else
  pass "undo-tree is not pinned in straight/versions/default.el"
fi

if [[ -d "$REPO_ROOT/undo-tree-history" ]]; then
  fail "undo-tree-history/ still exists on disk"
else
  pass "undo-tree-history/ is absent"
fi

# ---------------------------------------------------------------------------
# 2) Boot the real config once (early-init.el + init.el, emacs-startup-hook
#    run explicitly) and probe live state for AC1 (mode enabled), AC2
#    (steady-state threshold), AC3 (no undo truncation on a large edit),
#    and AC4 (undo-tree gone, evil-undo-system correct).
# ---------------------------------------------------------------------------

# A payload comfortably larger than the OLD 160000-byte undo-limit, so a
# single edit's undo entry would have been discarded/truncated under the
# stock pre-phase-6 settings but must not be under the new ones.
LARGE_PAYLOAD_BYTES=250000

PROBE_ELISP="
(progn
  (message \"PROBE:gc-cons-threshold-post-startup=%S\" gc-cons-threshold)
  (message \"PROBE:gcmh-high-cons-threshold=%S\" (bound-and-true-p gcmh-high-cons-threshold))
  (message \"PROBE:gcmh-mode-enabled=%S\" (bound-and-true-p gcmh-mode))
  (message \"PROBE:undo-tree-featurep=%S\" (featurep 'undo-tree))
  (message \"PROBE:evil-undo-system=%S\" (bound-and-true-p evil-undo-system))
  (message \"PROBE:undo-limit=%S\" undo-limit)
  (message \"PROBE:undo-strong-limit=%S\" undo-strong-limit)

  ;; AC3: a single large edit, well past the OLD 160000-byte undo-limit,
  ;; must not trigger Emacs's own truncation message under the new limits.
  (let ((warned nil))
    (with-temp-buffer
      (buffer-enable-undo)
      (advice-add 'message :before
                  (lambda (fmt &rest args)
                    (when (and (stringp fmt) (string-match-p \"[Tt]runcat\" fmt))
                      (setq warned t))))
      (insert (make-string $LARGE_PAYLOAD_BYTES ?x))
      (undo-boundary)
      (goto-char (point-min))
      (insert (make-string $LARGE_PAYLOAD_BYTES ?y))
      (undo-boundary)
      (garbage-collect))
    (message \"PROBE:undo-truncation-warned=%S\" warned)))
"

BOOT_OUTPUT="$(cd "$REPO_ROOT" && emacs -Q --batch \
  --eval "(setq user-emacs-directory (expand-file-name \"$REPO_ROOT/\"))" \
  -l "$REPO_ROOT/early-init.el" \
  -l "$REPO_ROOT/init.el" \
  --eval "(run-hooks 'emacs-startup-hook)" \
  --eval "$PROBE_ELISP" 2>&1)"

get() {
  echo "$BOOT_OUTPUT" | grep "^PROBE:$1=" | tail -1 | sed "s/^PROBE:$1=//"
}

if [[ -z "$(get gc-cons-threshold-post-startup)" ]]; then
  fail "config failed to boot under -Q --batch -l early-init.el -l init.el -- raw output follows:"
  echo "$BOOT_OUTPUT" >&2
else
  # AC1: gcmh-mode is actually enabled after a full boot.
  [[ "$(get gcmh-mode-enabled)" == "t" ]] \
    && pass "gcmh-mode is enabled after a full boot" \
    || fail "gcmh-mode is enabled after a full boot -- got $(get gcmh-mode-enabled)"

  # AC2 (steady state): post-startup gc-cons-threshold must equal
  # gcmh-high-cons-threshold, and must be in the 64-100MB band, never left
  # at most-positive-fixnum.
  GHC="$(get gcmh-high-cons-threshold)"
  GCT="$(get gc-cons-threshold-post-startup)"
  if [[ "$GCT" == "$GHC" && "$GCT" =~ ^[0-9]+$ && "$GCT" -ge $((64 * 1024 * 1024)) && "$GCT" -le $((100 * 1024 * 1024)) ]]; then
    pass "gc-cons-threshold equals gcmh-high-cons-threshold post-startup and is in the 64-100MB band ($GCT)"
  else
    fail "gc-cons-threshold post-startup is not a sane gcmh-managed value -- gc-cons-threshold=$GCT gcmh-high-cons-threshold=$GHC"
  fi

  # AC3: no "truncat*" message fired during a >160000-byte undo-recorded edit.
  [[ "$(get undo-truncation-warned)" == "nil" ]] \
    && pass "a $LARGE_PAYLOAD_BYTES-byte undo-recorded edit produced no truncation warning" \
    || fail "a $LARGE_PAYLOAD_BYTES-byte undo-recorded edit produced a truncation warning"

  [[ "$(get undo-limit)" =~ ^[0-9]+$ && "$(get undo-limit)" -gt 160000 ]] \
    && pass "undo-limit is raised above the old 160000-byte stock value ($(get undo-limit))" \
    || fail "undo-limit is not raised above 160000 -- got $(get undo-limit)"
  [[ "$(get undo-strong-limit)" =~ ^[0-9]+$ && "$(get undo-strong-limit)" -gt 240000 ]] \
    && pass "undo-strong-limit is raised above the old 240000-byte stock value ($(get undo-strong-limit))" \
    || fail "undo-strong-limit is not raised above 240000 -- got $(get undo-strong-limit)"

  # AC4: undo-tree is gone and evil drives native undo-redo instead.
  [[ "$(get undo-tree-featurep)" == "nil" ]] \
    && pass "undo-tree is not loaded ((featurep 'undo-tree) is nil)" \
    || fail "undo-tree is loaded -- (featurep 'undo-tree) got $(get undo-tree-featurep)"
  [[ "$(get evil-undo-system)" == "undo-redo" ]] \
    && pass "evil-undo-system is 'undo-redo" \
    || fail "evil-undo-system is not 'undo-redo -- got $(get evil-undo-system)"
fi

# ---------------------------------------------------------------------------
# 3) AC5: .cache/lsp has a stated bound (documentation) and its current
#    size is reported for auditability. This is informational sizing, not
#    a hard pass/fail threshold -- the phase's own finding is that the
#    directory's size tracks the number of npm-installed servers, not
#    usage, so there is no single "correct" byte count to assert against.
# ---------------------------------------------------------------------------

if grep -q "\.cache/lsp" "$REPO_ROOT/modules/programming.el" 2>/dev/null; then
  pass ".cache/lsp's footprint model is documented in modules/programming.el"
else
  fail ".cache/lsp's footprint model is not documented in modules/programming.el"
fi

if [[ -d "$REPO_ROOT/.cache/lsp" ]]; then
  echo "INFO: current .cache/lsp size: $(du -sh "$REPO_ROOT/.cache/lsp" 2>/dev/null | cut -f1)"
else
  echo "INFO: .cache/lsp does not exist in this checkout (nothing installed yet)"
fi

echo
if [[ $FAILED -eq 0 ]]; then
  echo "ALL CHECKS PASSED"
  exit 0
else
  echo "ONE OR MORE CHECKS FAILED"
  exit 1
fi
