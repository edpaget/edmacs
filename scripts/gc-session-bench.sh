#!/usr/bin/env bash
#
# gc-session-bench.sh -- compare gcs-done over a synthetic working session
# under the pre-gcmh GC settings (16MB threshold, stock undo limits) and the
# settings this config ships (gcmh + raised undo limits).
#
# --batch is fine here: unlike startup time, gcs-done is a plain counter that
# behaves the same headless. The config is loaded explicitly with -l.
#
# The workload reproduces the allocation shape of an LSP session rather than
# any real server: JSON parsing (JSON-RPC), large-buffer rewrites with undo
# (format-on-save, lsp-rename), and buffer switching. Post-change values are
# read from the tracked config so they cannot drift.
#
# USAGE
#   scripts/gc-session-bench.sh          # one run
#   scripts/gc-session-bench.sh -n 5     # mean of 5 runs
#
# Measures relative GC pressure only, not latency or real LSP traffic.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUNS=1

while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--runs)
      RUNS="$2"
      shift 2
      ;;
    *)
      echo "usage: $0 [-n RUNS]" >&2
      exit 1
      ;;
  esac
done

if ! [[ "$RUNS" =~ ^[0-9]+$ ]] || [[ "$RUNS" -lt 1 ]]; then
  echo "error: --runs must be a positive integer, got: $RUNS" >&2
  exit 1
fi

# Read post-change values from the tracked config so they cannot drift.
extract_setq_number() {
  # $1 = file, $2 = variable. Prints the evaluated integer of `(setq VAR ...)'.
  local file="$1" var="$2"
  emacs --batch --eval "
    (with-temp-buffer
      (insert-file-contents \"$file\")
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward (concat \"[( ]\" (regexp-quote \"$var\") \"[ \\n]+\") nil t)
          (let* ((form (ignore-errors (read (current-buffer))))
                 (val (ignore-errors (eval form t))))
            (when (numberp val)
              (princ (number-to-string val))
              (throw 'found nil))))
        (error \"could not find %s in %s\" \"$var\" \"$file\")))" 2>/dev/null
}

# Under `set -e', a failing command substitution aborts before any later
# emptiness check runs; check the exit status inline.
extract_or_die() {
  local file="$1" var="$2" out
  if ! out="$(extract_setq_number "$file" "$var")" || [[ -z "$out" ]]; then
    echo "error: failed to extract $var from $file -- has the variable name changed?" >&2
    exit 1
  fi
  printf '%s' "$out"
}

GCMH_HIGH="$(extract_or_die "$REPO_ROOT/init.el" "gcmh-high-cons-threshold")"
GCMH_IDLE_DELAY="$(extract_or_die "$REPO_ROOT/init.el" "gcmh-idle-delay")"
NEW_UNDO_LIMIT="$(extract_or_die "$REPO_ROOT/modules/core.el" "undo-limit")"
NEW_UNDO_STRONG_LIMIT="$(extract_or_die "$REPO_ROOT/modules/core.el" "undo-strong-limit")"

echo "Post-change values read from tracked config:"
echo "  gcmh-high-cons-threshold = $GCMH_HIGH"
echo "  gcmh-idle-delay          = $GCMH_IDLE_DELAY"
echo "  undo-limit               = $NEW_UNDO_LIMIT"
echo "  undo-strong-limit        = $NEW_UNDO_STRONG_LIMIT"
echo

# The workload; reused by both --batch invocations below.
SESSION_EL="$(mktemp -t edmacs-gc-bench-session-XXXXXX.el)"
trap 'rm -f "$SESSION_EL"' EXIT

cat > "$SESSION_EL" <<'ELISP'
(defun edmacs-gc-bench--json-payload (n)
  "Build an N-entry JSON array string, standing in for an LSP JSON-RPC body."
  (concat "["
          (mapconcat
           (lambda (i)
             (format "{\"id\":%d,\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\"file:///tmp/f%d.go\",\"diagnostics\":[{\"range\":{\"start\":{\"line\":%d,\"character\":0},\"end\":{\"line\":%d,\"character\":10}},\"message\":\"unused variable x%d\",\"severity\":2}]}}"
                     i i i i i))
           (number-sequence 0 (1- n))
           ",")
          "]"))

(defun edmacs-gc-bench--large-text (lines)
  "Build a LINES-line synthetic source buffer body."
  (mapconcat (lambda (i) (format "func handler%d(w http.ResponseWriter, r *http.Request) { fmt.Fprintf(w, \"ok %%d\", %d) }" i i))
             (number-sequence 0 (1- lines))
             "\n"))

(defun edmacs-gc-bench--run-session ()
  "Run one representative-session workload pass; return GCs it caused."
  (let ((before gcs-done)
        (json-payload (edmacs-gc-bench--json-payload 400))
        (big-text (edmacs-gc-bench--large-text 4000)))
    ;; JSON-RPC-shaped parsing churn: 5 synthetic "buffers" each getting
    ;; ~40 diagnostics batches parsed, like a multi-file lsp-mode session.
    (dotimes (_buf 5)
      (dotimes (_msg 40)
        (json-parse-string json-payload :object-type 'alist)))
    ;; Format-on-save-shaped rewrites with undo recording live.
    (dotimes (buf 3)
      (with-temp-buffer
        (buffer-enable-undo)
        (insert big-text)
        (dotimes (_rewrite 6)
          (goto-char (point-min))
          (while (re-search-forward "handler[0-9]+" nil t)
            (replace-match (format "handler_renamed_%d" buf)))
          (undo-boundary)
          (goto-char (point-max))
          (insert big-text))
        ;; Undo pressure: unwind most of what was just done.
        (dotimes (_u 8)
          (ignore-errors (primitive-undo 1 buffer-undo-list)))))
    ;; Buffer-switching churn: a session bouncing between several live
    ;; buffers, each already holding real content.
    (let (bufs)
      (dotimes (i 8)
        (push (generate-new-buffer (format "*gc-bench-%d*" i)) bufs))
      (dotimes (_cycle 20)
        (dolist (b bufs)
          (with-current-buffer b
            (goto-char (point-max))
            (insert (substring big-text 0 (min 500 (length big-text)))))))
      (mapc #'kill-buffer bufs))
    (- gcs-done before)))
ELISP

# Boot the real config, override GC variables for MODE (baseline or
# post-change), run the workload once, print "GCS-DELTA: N".
#
# $1 = "baseline" or "post-change"
run_session() {
  local mode="$1" override
  case "$mode" in
    baseline)
      # Pre-gcmh values: flat 16MB threshold, stock undo limits.
      override="
        (when (fboundp 'gcmh-mode) (gcmh-mode -1))
        (setq gc-cons-threshold (* 16 1024 1024)
              gc-cons-percentage 0.1
              undo-limit 160000
              undo-strong-limit 240000)"
      ;;
    post-change)
      # Re-asserted so both runs start the workload from a fresh threshold.
      override="
        (setq gc-cons-threshold $GCMH_HIGH
              gc-cons-percentage 0.1
              undo-limit $NEW_UNDO_LIMIT
              undo-strong-limit $NEW_UNDO_STRONG_LIMIT)"
      ;;
    *)
      echo "internal error: unknown mode $mode" >&2
      return 1
      ;;
  esac

  emacs --batch \
    --eval "(setq user-emacs-directory (expand-file-name \"$REPO_ROOT/\"))" \
    -l "$REPO_ROOT/early-init.el" \
    -l "$REPO_ROOT/init.el" \
    --eval "(run-hooks 'emacs-startup-hook)" \
    -l "$SESSION_EL" \
    --eval "(progn $override (garbage-collect) (princ (format \"GCS-DELTA: %d\n\" (edmacs-gc-bench--run-session))))" \
    2>/dev/null | grep '^GCS-DELTA:' | awk '{print $2}'
}

sum_baseline=0
sum_post=0

for ((i = 1; i <= RUNS; i++)); do
  # Check the assignment's exit status inline; under `set -e' a failed
  # pipeline in a bare assignment would abort the whole script.
  if ! b="$(run_session baseline)"; then
    echo "warning: run $i baseline produced no GCS-DELTA output -- skipping" >&2
    continue
  fi
  if ! p="$(run_session post-change)"; then
    echo "warning: run $i post-change produced no GCS-DELTA output -- skipping" >&2
    continue
  fi
  if [[ -z "$b" || -z "$p" ]]; then
    echo "warning: run $i produced no GCS-DELTA output -- skipping" >&2
    continue
  fi
  echo "run $i: baseline gcs-done delta = $b   post-change gcs-done delta = $p"
  sum_baseline=$((sum_baseline + b))
  sum_post=$((sum_post + p))
done

echo
echo "=== Summary over $RUNS run(s) ==="
# Non-zero exit on regression so this can gate CI.
awk -v b="$sum_baseline" -v p="$sum_post" -v n="$RUNS" '
  BEGIN {
    printf "  baseline    mean gcs-done delta: %.2f\n", b / n
    printf "  post-change mean gcs-done delta: %.2f\n", p / n
    if (p < b) {
      printf "  RESULT: post-change is lower (-%.2f, %.1f%% fewer GCs) -- AC6 satisfied (PASS)\n", (b - p) / n, 100 * (b - p) / b
      exit 0
    } else {
      printf "  RESULT: post-change is NOT lower than baseline -- AC6 NOT satisfied (FAIL)\n"
      exit 1
    }
  }
'
