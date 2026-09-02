#!/usr/bin/env bash
#
# verify-gc-and-unbounded-state.sh -- PASS/FAIL checks for the GC and undo
# configuration: gcmh replaces the focus-change GC hook, the early-init.el
# fallback timer exists, undo limits are raised, and undo-tree is gone.
#
# The AC3 undo check measures bytes retained in `buffer-undo-list' after a
# delete-heavy workload and a GC, under both the configured limits and the
# stock 160000/240000 ones. Truncation is a silent C-level GC step (no
# message), and insertion entries carry no size, so only a delete workload
# with a stock control arm can tell raised limits from stock.
#
# The deliberate-error fallback path needs a real daemon and a ~20s wait; see
# verify-gc-fallback-daemon.sh. gcs-done is covered by gc-session-bench.sh.
#
# USAGE
#   scripts/verify-gc-and-unbounded-state.sh [path-to-edmacs-checkout]
#   Exits 0 if every check passes, 1 otherwise.

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

# AC1: grep for the hook being wired up, excluding comment lines.
if grep -vE '^\s*;;' "$REPO_ROOT/init.el" \
   | grep -qE "(add-hook|add-function|add-variable-watcher)[^)]*after-focus-change-function"; then
  fail "after-focus-change-function is still wired up as a live hook in init.el"
else
  pass "after-focus-change-function is no longer wired up as a live hook in init.el"
fi

# AC1: gcmh must be command/timer driven, not frame-focus driven.
GCMH_EL="$(find "$REPO_ROOT/straight/repos/gcmh" -iname 'gcmh.el' 2>/dev/null | head -1)"
if [[ -z "$GCMH_EL" ]]; then
  fail "could not locate gcmh.el under straight/repos to verify its GC-trigger mechanism"
elif grep -q "frame-focus-state\|after-focus-change-function" "$GCMH_EL"; then
  fail "gcmh.el references frame-focus machinery -- AC1's TTY-frame claim would not hold"
else
  pass "gcmh.el has no frame-focus dependency (uses pre/post-command-hook + a plain timer)"
fi

# AC2 structure: a guarded idle timer in early-init.el.
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
# 2) Boot the real config once and probe live state.
# ---------------------------------------------------------------------------

# Delete-based workload for the AC3 probe: 1,000,000 bytes sits under the
# configured undo-limit (3MB) and well over the stock one (160000).
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

  ;; AC3: bytes of deleted text retained in buffer-undo-list after a GC,
  ;; under the configured limits and under the old stock ones.
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

  # AC2 steady state: gc-cons-threshold equals gcmh-high-cons-threshold.
  GHC="$(get gcmh-high-cons-threshold)"
  GCT="$(get gc-cons-threshold-post-startup)"
  if [[ "$GCT" == "$GHC" && "$GCT" =~ ^[0-9]+$ && "$GCT" -ge $((64 * 1024 * 1024)) && "$GCT" -le $((100 * 1024 * 1024)) ]]; then
    pass "gc-cons-threshold equals gcmh-high-cons-threshold post-startup and is in the 64-100MB band ($GCT)"
  else
    fail "gc-cons-threshold post-startup is not a sane gcmh-managed value -- gc-cons-threshold=$GCT gcmh-high-cons-threshold=$GHC"
  fi

  # AC3: the configured limits retain the whole history; the stock control
  # arm must truncate, proving the probe discriminates.
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
# 3) AC5: report .cache/lsp size. Informational: it tracks the number of
#    npm-installed servers, not usage, so there is no byte count to assert.
# ---------------------------------------------------------------------------

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
