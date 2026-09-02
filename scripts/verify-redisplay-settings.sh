#!/usr/bin/env bash
#
# verify-redisplay-settings.sh -- PASS/FAIL checks for the redisplay settings
# in modules/core.el: so-long (including evil neutralization), bidi, Git-only
# vc backends, skip-fontification-on-input, font-cache compaction, hl-line off
# in vterm, and removal of redisplay-dont-pause/scroll-step.
#
# Boots the real config under `emacs -Q --batch -l early-init.el -l init.el'.
# `user-emacs-directory' must be pinned to the checkout first, or straight and
# `load-module' resolve against ~/.emacs.d and silently test the wrong config.
#
# so-long is forced with an explicit `(so-long)' call, since it defers
# automatic detection when no window shows the buffer. The vterm hook is not
# run for real: vterm.el requires its native module at load, which would
# prompt to compile under --batch. Its mechanism is exercised synthetically
# and the hook line is grepped. Typing latency and rendering need a display.
#
# USAGE
#   scripts/verify-redisplay-settings.sh [path-to-edmacs-checkout]
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
# 1) Boot the real config once and dump PROBE:KEY=VALUE lines.
# ---------------------------------------------------------------------------

# The .txt extension matters: in fundamental-mode visual-line-mode is never
# enabled, which would make the so-long assertion below vacuous.
TMP_SINGLE_LINE_BASE="$(mktemp -t edmacs-so-long-test.XXXXXX)"
TMP_SINGLE_LINE="${TMP_SINGLE_LINE_BASE}.txt"
mv "$TMP_SINGLE_LINE_BASE" "$TMP_SINGLE_LINE"
trap 'rm -f "$TMP_SINGLE_LINE" "$TMP_SINGLE_LINE_BASE"' EXIT
# Well over so-long-threshold (250 chars).
perl -e 'print "x" x 3000000' > "$TMP_SINGLE_LINE"

PROBE_ELISP='
(progn
  (require (quote so-long))
  (require (quote magit))
  (require (quote diff-hl))

  (message "PROBE:vc-handled-backends=%S" vc-handled-backends)
  (message "PROBE:bidi-paragraph-direction=%S" bidi-paragraph-direction)
  (message "PROBE:bidi-inhibit-bpa=%S" bidi-inhibit-bpa)
  (message "PROBE:redisplay-skip-fontification-on-input=%S" redisplay-skip-fontification-on-input)
  (message "PROBE:inhibit-compacting-font-caches=%S" inhibit-compacting-font-caches)
  (message "PROBE:global-so-long-mode=%S" (bound-and-true-p global-so-long-mode))
  (message "PROBE:so-long-minor-modes-has-evil=%S" (and (memq (quote evil-local-mode) so-long-minor-modes) t))
  ;; font-lock-mode is inert under --batch; assert membership instead.
  (message "PROBE:so-long-minor-modes-has-font-lock=%S" (and (memq (quote font-lock-mode) so-long-minor-modes) t))
  (message "PROBE:so-long-minor-modes-has-visual-line=%S" (and (memq (quote visual-line-mode) so-long-minor-modes) t))
  (message "PROBE:redisplay-dont-pause-bound=%S" (boundp (quote redisplay-dont-pause)))

  ;; so-long defers detection when no window shows the buffer; force it.
  (find-file (getenv "EDMACS_VERIFY_SINGLE_LINE_FILE"))
  ;; Record the pre-so-long state so the nil-afterwards checks are not vacuous.
  (message "PROBE:pre-so-long-major-mode=%S" major-mode)
  (message "PROBE:pre-so-long-evil-local-mode=%S" (bound-and-true-p evil-local-mode))
  (message "PROBE:pre-so-long-visual-line-mode=%S" (bound-and-true-p visual-line-mode))
  (so-long)
  (message "PROBE:so-long-major-mode=%S" major-mode)
  (message "PROBE:so-long-evil-local-mode=%S" (bound-and-true-p evil-local-mode))
  (message "PROBE:so-long-visual-line-mode=%S" (bound-and-true-p visual-line-mode))
  (kill-buffer)

  ;; vc / magit / diff-hl interop against a real tracked file.
  (find-file (expand-file-name "init.el" user-emacs-directory))
  (message "PROBE:vc-backend=%S" (vc-backend (buffer-file-name)))
  (message "PROBE:magit-toplevel=%S" (and (magit-toplevel) t))
  (diff-hl-mode 1)
  (message "PROBE:diff-hl-mode=%S" (bound-and-true-p diff-hl-mode))
  (kill-buffer)

  ;; vterm.el cannot be loaded here (native module); reproduce the setq-local
  ;; mechanism the hook relies on. The grep below checks the real hook line.
  (let ((vterm-mode-hook nil))
    (add-hook (quote vterm-mode-hook) (lambda () (setq-local global-hl-line-mode nil)))
    (with-temp-buffer
      (run-hooks (quote vterm-mode-hook))
      (message "PROBE:vterm-hook-hl-line=%S"
               (if (local-variable-p (quote global-hl-line-mode) (current-buffer))
                   (buffer-local-value (quote global-hl-line-mode) (current-buffer))
                 (quote not-buffer-local)))))
  (with-temp-buffer
    (message "PROBE:plain-buffer-hl-line=%S"
             (if (local-variable-p (quote global-hl-line-mode) (current-buffer))
                 (buffer-local-value (quote global-hl-line-mode) (current-buffer))
               (quote not-buffer-local))))

  (message "PROBE:warnings-buffer=%S" (and (get-buffer "*Warnings*") t))
  (message "PROBE:probe-complete=t"))
'

OUTPUT="$(cd "$REPO_ROOT" && EDMACS_VERIFY_SINGLE_LINE_FILE="$TMP_SINGLE_LINE" \
  emacs -Q --batch \
  --eval "(setq user-emacs-directory (file-name-as-directory \"$REPO_ROOT\"))" \
  -l "$REPO_ROOT/early-init.el" -l "$REPO_ROOT/init.el" \
  --eval "$PROBE_ELISP" 2>&1)"
STATUS=$?

get() {
  # Extract the value for PROBE:<key>=..., last occurrence wins.
  echo "$OUTPUT" | sed -n "s/^PROBE:$1=//p" | tail -1
}

if [[ $STATUS -ne 0 ]] || [[ -z "$(get probe-complete)" ]]; then
  fail "boot completed without error"
  echo "$OUTPUT" | tail -40 >&2
  exit 1
else
  pass "boot completed without error"
fi

# ---------------------------------------------------------------------------
# 2) Per-AC checks against the probe output.
# ---------------------------------------------------------------------------

# AC: vc-handled-backends is (Git); magit and diff-hl still work.
[[ "$(get vc-handled-backends)" == "(Git)" ]] \
  && pass "vc-handled-backends is (Git)" \
  || fail "vc-handled-backends is (Git) -- got $(get vc-handled-backends)"
[[ "$(get vc-backend)" == "Git" ]] \
  && pass "vc-backend on a tracked file is Git" \
  || fail "vc-backend on a tracked file is Git -- got $(get vc-backend)"
[[ "$(get magit-toplevel)" == "t" ]] \
  && pass "magit-toplevel resolves" \
  || fail "magit-toplevel resolves -- got $(get magit-toplevel)"
[[ "$(get diff-hl-mode)" == "t" ]] \
  && pass "diff-hl-mode activates" \
  || fail "diff-hl-mode activates -- got $(get diff-hl-mode)"

# AC: hl-line is buffer-locally nil in vterm buffers. The mechanism check
# above is synthetic; this grep confirms the real hook line.
if grep -qE "add-hook 'vterm-mode-hook.*setq-local global-hl-line-mode nil" \
    "$REPO_ROOT/modules/vterm.el" 2>/dev/null; then
  pass "modules/vterm.el registers the global-hl-line-mode suppression on vterm-mode-hook"
else
  fail "modules/vterm.el does not register the expected vterm-mode-hook / global-hl-line-mode suppression"
fi
[[ "$(get vterm-hook-hl-line)" == "nil" ]] \
  && pass "mechanism check: setq-local on global-hl-line-mode in a hook is buffer-local" \
  || fail "mechanism check: setq-local on global-hl-line-mode in a hook is buffer-local -- got $(get vterm-hook-hl-line)"
[[ "$(get plain-buffer-hl-line)" == "not-buffer-local" ]] \
  && pass "global-hl-line-mode is untouched (not buffer-local) in a plain buffer" \
  || fail "global-hl-line-mode is untouched in a plain buffer -- got $(get plain-buffer-hl-line)"

# AC: redisplay-dont-pause and scroll-step are gone; scroll-conservatively
# appears exactly once.
[[ "$(get redisplay-dont-pause-bound)" == "nil" ]] \
  && pass "redisplay-dont-pause is not bound" \
  || fail "redisplay-dont-pause is not bound -- got $(get redisplay-dont-pause-bound)"
# Tracked *.el only; stray .elc files would double-count.
mapfile -t EL_FILES < <(cd "$REPO_ROOT" && git ls-files '*.el')
grep_el() { (cd "$REPO_ROOT" && grep -n "$1" "${EL_FILES[@]}"); }

if ! grep_el 'redisplay-dont-pause' >/dev/null 2>&1; then
  pass "redisplay-dont-pause absent from config source"
else
  fail "redisplay-dont-pause still referenced in config source"
fi
if ! grep_el 'scroll-step' >/dev/null 2>&1; then
  pass "scroll-step absent from config source"
else
  fail "scroll-step still referenced in config source"
fi
SC_HITS="$(grep_el 'scroll-conservatively' 2>/dev/null | wc -l | tr -d ' ')"
[[ "$SC_HITS" == "1" ]] \
  && pass "scroll-conservatively is set in exactly one place" \
  || fail "scroll-conservatively is set in exactly one place -- got $SC_HITS occurrence(s)"

# AC: bidi-paragraph-direction / bidi-inhibit-bpa /
# redisplay-skip-fontification-on-input / inhibit-compacting-font-caches.
[[ "$(get bidi-paragraph-direction)" == "left-to-right" ]] \
  && pass "bidi-paragraph-direction is left-to-right" \
  || fail "bidi-paragraph-direction is left-to-right -- got $(get bidi-paragraph-direction)"
[[ "$(get bidi-inhibit-bpa)" == "t" ]] \
  && pass "bidi-inhibit-bpa is non-nil" \
  || fail "bidi-inhibit-bpa is non-nil -- got $(get bidi-inhibit-bpa)"
[[ "$(get redisplay-skip-fontification-on-input)" == "t" ]] \
  && pass "redisplay-skip-fontification-on-input is non-nil" \
  || fail "redisplay-skip-fontification-on-input is non-nil -- got $(get redisplay-skip-fontification-on-input)"
[[ "$(get inhibit-compacting-font-caches)" == "t" ]] \
  && pass "inhibit-compacting-font-caches is non-nil" \
  || fail "inhibit-compacting-font-caches is non-nil -- got $(get inhibit-compacting-font-caches)"

# AC: global-so-long-mode on, and forcing so-long neutralizes evil,
# visual-line-mode, and font-lock.
[[ "$(get global-so-long-mode)" == "t" ]] \
  && pass "global-so-long-mode is enabled" \
  || fail "global-so-long-mode is enabled -- got $(get global-so-long-mode)"
[[ "$(get so-long-minor-modes-has-evil)" == "t" ]] \
  && pass "evil-local-mode is in so-long-minor-modes" \
  || fail "evil-local-mode is in so-long-minor-modes -- got $(get so-long-minor-modes-has-evil)"
[[ "$(get so-long-major-mode)" == "so-long-mode" ]] \
  && pass "forcing so-long on a huge single-line buffer engages so-long-mode" \
  || fail "forcing so-long on a huge single-line buffer engages so-long-mode -- got $(get so-long-major-mode)"
# Assert the modes were on before so-long ran, so the nil checks mean something.
[[ "$(get pre-so-long-major-mode)" == "text-mode" ]] \
  && pass "huge-line probe buffer starts in text-mode (so the checks below are not vacuous)" \
  || fail "huge-line probe buffer starts in text-mode -- got $(get pre-so-long-major-mode)"
[[ "$(get pre-so-long-evil-local-mode)" == "t" ]] \
  && pass "evil-local-mode is ON before so-long runs" \
  || fail "evil-local-mode is ON before so-long runs -- got $(get pre-so-long-evil-local-mode)"
[[ "$(get pre-so-long-visual-line-mode)" == "t" ]] \
  && pass "visual-line-mode is ON before so-long runs" \
  || fail "visual-line-mode is ON before so-long runs -- got $(get pre-so-long-visual-line-mode)"

[[ "$(get so-long-evil-local-mode)" == "nil" ]] \
  && pass "so-long neutralizes evil-local-mode" \
  || fail "so-long neutralizes evil-local-mode -- got $(get so-long-evil-local-mode)"
[[ "$(get so-long-visual-line-mode)" == "nil" ]] \
  && pass "so-long neutralizes visual-line-mode" \
  || fail "so-long neutralizes visual-line-mode -- got $(get so-long-visual-line-mode)"
# Structural, not dynamic -- font-lock-mode is inert under --batch (see probe).
[[ "$(get so-long-minor-modes-has-font-lock)" == "t" ]] \
  && pass "font-lock-mode is in so-long-minor-modes" \
  || fail "font-lock-mode is in so-long-minor-modes -- got $(get so-long-minor-modes-has-font-lock)"
[[ "$(get so-long-minor-modes-has-visual-line)" == "t" ]] \
  && pass "visual-line-mode is in so-long-minor-modes" \
  || fail "visual-line-mode is in so-long-minor-modes -- got $(get so-long-minor-modes-has-visual-line)"

# AC: booting produces an empty *Warnings* buffer.
[[ "$(get warnings-buffer)" == "nil" ]] \
  && pass "*Warnings* buffer is absent after a full boot" \
  || fail "*Warnings* buffer is absent after a full boot -- got $(get warnings-buffer)"

echo
if [[ $FAILED -eq 0 ]]; then
  echo "ALL CHECKS PASSED"
  exit 0
else
  echo "ONE OR MORE CHECKS FAILED"
  exit 1
fi
