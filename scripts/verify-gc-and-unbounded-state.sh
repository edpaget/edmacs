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
#   AC3's check measures actual `buffer-undo-list' retention after a
#   delete-based edit workload and a GC, comparing this config's real
#   limits against the OLD stock 160000/240000 values in the same probe --
#   not a "truncat*" message/warning: code review found Emacs's
#   undo-limit/undo-strong-limit truncation is a silent C-level GC step
#   that never calls `message' or `display-warning' (only a *different*,
#   much larger `undo-outer-limit' overflow does that, via `yes-or-no-p'),
#   and that a pure-insertion workload never accumulates truncatable
#   "size" at all (insertion undo entries are cheap (BEG . END) pairs;
#   only deletion entries store the removed text, which is what actually
#   counts toward the byte limits) -- so the original message-advice
#   version of this check passed unconditionally regardless of whether the
#   limits were raised, reverted, or removed. The fix is verified
#   non-vacuous in both directions: it fails if the configured limits are
#   reverted to stock (reproduced manually), and its OLD-stock control arm
#   asserts truncation actually happens there in the same run.
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

# Total bytes of delete-based edits for the AC3 retention probe, and the
# chunk size each round deletes. Deletion undo entries (unlike insertion
# entries, which are cheap (BEG . END) position pairs) store the deleted
# text itself, so they are what actually accumulates counted "size" toward
# undo-limit/undo-strong-limit truncation -- a pure-insertion workload
# (the prior version of this check) never triggers truncation at any
# threshold and so cannot tell a raised limit from a stock one. 1,000,000
# total bytes sits comfortably under the new undo-limit (3,145,728) so it
# must survive intact, and comfortably over the OLD stock undo-limit
# (160,000)/undo-strong-limit (240,000) so it must NOT survive intact
# there -- giving the probe a real pass/fail distinction in both
# directions (empirically confirmed: new limits retain all 1,000,000
# bytes, old stock limits retain only ~200,000).
UNDO_TOTAL_BYTES=1000000
UNDO_CHUNK_BYTES=100000

PROBE_ELISP="
(progn
  (message \"PROBE:gc-cons-threshold-post-startup=%S\" gc-cons-threshold)
  (message \"PROBE:gcmh-high-cons-threshold=%S\" (bound-and-true-p gcmh-high-cons-threshold))
  (message \"PROBE:gcmh-mode-enabled=%S\" (bound-and-true-p gcmh-mode))
  (message \"PROBE:undo-tree-featurep=%S\" (featurep 'undo-tree))
  (message \"PROBE:evil-undo-system=%S\" (bound-and-true-p evil-undo-system))
  (message \"PROBE:undo-limit=%S\" undo-limit)
  (message \"PROBE:undo-strong-limit=%S\" undo-strong-limit)

  ;; AC3: how many bytes of delete-recorded undo history survive a GC,
  ;; under (a) this config's actual configured limits and (b) the OLD
  ;; stock 160000/240000 limits, for the identical workload. Emacs's
  ;; undo-limit/undo-strong-limit truncation is a silent C-level step
  ;; taken during GC (compact/truncate the undo list) -- it never calls
  ;; \`message' or \`display-warning', so the only way to observe it is to
  ;; measure what is actually left in \`buffer-undo-list' afterward, not
  ;; to listen for a warning that this mechanism never emits.
  (defun edmacs--undo-retention-probe (limit strong outer total-bytes chunk-bytes)
    (with-temp-buffer
      (buffer-enable-undo)
      (setq-local undo-limit limit)
      (setq-local undo-strong-limit strong)
      (setq-local undo-outer-limit outer)
      (insert (make-string total-bytes ?x))
      (undo-boundary)
      (let ((n (/ total-bytes chunk-bytes)))
        (dotimes (i n)
          (goto-char (point-min))
          (delete-region (point-min) (min (point-max) (+ (point-min) chunk-bytes)))
          (undo-boundary)))
      (garbage-collect)
      (apply #'+ (mapcar (lambda (e) (if (and (consp e) (stringp (car e))) (length (car e)) 0))
                          buffer-undo-list))))
  (message \"PROBE:undo-retained-configured=%d\"
           (edmacs--undo-retention-probe undo-limit undo-strong-limit undo-outer-limit
                                          $UNDO_TOTAL_BYTES $UNDO_CHUNK_BYTES))
  (message \"PROBE:undo-retained-old-stock=%d\"
           (edmacs--undo-retention-probe 160000 240000 nil
                                          $UNDO_TOTAL_BYTES $UNDO_CHUNK_BYTES)))
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

  # AC3: under this config's actual configured limits, the full
  # $UNDO_TOTAL_BYTES-byte delete-based edit history must survive a GC
  # intact (no truncation) -- and, to prove this probe actually
  # discriminates rather than passing unconditionally, the identical
  # workload run under the OLD stock 160000/240000 limits must retain
  # measurably less than the total, confirming truncation really is
  # occurring there and the configured limits are what prevent it here.
  RETAINED_CONFIGURED="$(get undo-retained-configured)"
  RETAINED_OLD_STOCK="$(get undo-retained-old-stock)"
  [[ "$RETAINED_CONFIGURED" == "$UNDO_TOTAL_BYTES" ]] \
    && pass "a $UNDO_TOTAL_BYTES-byte delete-based undo history survives GC intact under the configured limits ($RETAINED_CONFIGURED bytes retained)" \
    || fail "a $UNDO_TOTAL_BYTES-byte delete-based undo history was truncated under the configured limits -- retained $RETAINED_CONFIGURED of $UNDO_TOTAL_BYTES bytes"
  [[ "$RETAINED_OLD_STOCK" =~ ^[0-9]+$ && "$RETAINED_OLD_STOCK" -lt $((UNDO_TOTAL_BYTES / 2)) ]] \
    && pass "the identical workload IS truncated under the OLD stock 160000/240000 limits ($RETAINED_OLD_STOCK of $UNDO_TOTAL_BYTES bytes retained) -- confirming this probe discriminates raised limits from stock ones" \
    || fail "the OLD stock 160000/240000 limits did not truncate the probe workload as expected (retained $RETAINED_OLD_STOCK of $UNDO_TOTAL_BYTES bytes) -- this probe would not have caught a regression back to stock limits"

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
