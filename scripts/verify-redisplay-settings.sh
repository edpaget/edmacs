#!/usr/bin/env bash
#
# verify-redisplay-settings.sh -- structural verification for
# edmacs-performance/phase-4-redisplay-overhead
#
# WHAT THIS CHECKS AND WHY IT EXISTS
#   Phase 4 lands nine cheap redisplay/perf settings (global-so-long-mode,
#   forced LTR bidi + no bracket-pair-algorithm, Git-only vc-handled-backends,
#   skip-fontification-on-input, no font-cache compaction, hl-line disabled
#   in vterm) plus two dead-code removals (redisplay-dont-pause, scroll-step)
#   and a scroll-conservatively de-duplication. Its own acceptance criteria
#   were deliberately rewritten by plan review away from subjective language
#   ("feels smoother") to structural, rerunnable checks -- this script IS
#   that check, so the class of unverified claim that phase-4's first two
#   commits shipped (an incorrect claim that evil was already neutralized in
#   so-long buffers, silently wrong until the very next commit) can't
#   regress without this script catching it.
#
#   scripts/startup-bench.sh cannot do this job: it kills Emacs from
#   emacs-startup-hook and never runs an interactive event loop, so it
#   can't observe variable state, mode activation, or *Warnings* content.
#   This script instead boots the real config (early-init.el + init.el)
#   under `emacs -Q --batch`, evaluates small probes against the live
#   session, and reports PASS/FAIL per acceptance criterion.
#
# THE --batch + -Q --batch  -l early-init.el -l init.el TRAP
#   `-Q` prevents Emacs from loading this repo's early-init.el/init.el
#   *automatically* -- but they are then loaded explicitly and unconditionally
#   via `-l`, so the real config still runs; this is not the same thing as
#   plain `emacs --batch` with no -l flags (which never touches the init
#   files at all -- see the header comment in startup-bench.sh for that
#   trap). This script's -l invocation is what the phase's own approved
#   plan specified for its batch ACs.
#
#   `user-emacs-directory` must be pointed explicitly at the checkout being
#   tested (via --eval, before -l early-init.el): this repo's `load-module`
#   (init.el) and straight's bootstrap both resolve paths from
#   `user-emacs-directory`, not from the process's cwd or the -l path, so an
#   unset default (~/.config/emacs or ~/.emacs.d) silently tests a
#   *different* checkout with no error. This was hit empirically while
#   writing this script -- every setting read back as its stock Emacs
#   default until user-emacs-directory was pinned down.
#
# WHAT IS AND ISN'T CHECKED HERE
#   Checked: vc-handled-backends, the four always-on settings (bidi
#   direction/bpa, redisplay-skip-fontification-on-input,
#   inhibit-compacting-font-caches), so-long's evil neutralization (forced
#   via an explicit `(so-long)` call -- so-long defers its own automatic
#   detection when no window displays the buffer, which is always true in
#   --batch; forcing it exercises the same so-long-minor-modes teardown
#   that the interactive, window-triggered path uses), vc/magit/diff-hl
#   interop, dead-setting removal via grep, and an empty *Warnings* buffer.
#
#   Checked with a caveat -- the vterm/hl-line suppression: vterm.el
#   unconditionally requires its native module at load time, which is not
#   prebuilt in a clean checkout, so actually loading modules/vterm.el
#   here would mean either an interactive y-or-n-p compile prompt (which
#   --batch can't answer) or an unconditional slow/disk-writing cmake
#   build on every run. This script instead (a) greps modules/vterm.el to
#   confirm the real `(add-hook 'vterm-mode-hook ...)` line is present
#   verbatim, and (b) exercises the underlying setq-local-is-buffer-local
#   mechanism synthetically. Neither actually runs the shipped hook
#   function -- a real interactive `M-x vterm` check (see the phase-4
#   record) is still the authoritative verification for this one AC.
#
#   NOT checked (needs a real window/display; left to interactive manual
#   verification per the phase body, which explicitly says
#   scripts/startup-bench.sh -- and by extension any headless harness --
#   cannot observe typing latency or redisplay-under-load): that so-long
#   triggers *automatically* on file open with a window present, that
#   typing/scrolling stays responsive, and that diff-hl's fringe marks
#   actually render. See the phase-4 record for the manual steps.
#
# USAGE
#   scripts/verify-redisplay-settings.sh [path-to-edmacs-checkout]
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
# 1) Boot the real config once and dump every value this script needs, as
#    "KEY=VALUE" lines prefixed with PROBE:, so one Emacs process answers
#    every structural check below.
# ---------------------------------------------------------------------------

# The .txt extension is load-bearing, not cosmetic: without a recognized
# extension the buffer lands in fundamental-mode, where visual-line-mode is
# never enabled (it comes from text-mode-hook, modules/ui.el) -- which made
# the "so-long neutralizes visual-line-mode" assertion below vacuously true,
# passing even if so-long did nothing at all. Verified: with no extension,
# visual-line-mode reads nil BEFORE so-long runs.
TMP_SINGLE_LINE_BASE="$(mktemp -t edmacs-so-long-test.XXXXXX)"
TMP_SINGLE_LINE="${TMP_SINGLE_LINE_BASE}.txt"
mv "$TMP_SINGLE_LINE_BASE" "$TMP_SINGLE_LINE"
trap 'rm -f "$TMP_SINGLE_LINE" "$TMP_SINGLE_LINE_BASE"' EXIT
# 3,000,000 chars: comfortably over so-long-max-lines'-companion threshold
# (so-long triggers on line length, default so-long-threshold is 250 chars).
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
  ;; font-lock-mode cannot be exercised dynamically here: font-lock-mode is
  ;; inert when noninteractive is non-nil, so it reads nil under --batch even
  ;; after an explicit (font-lock-mode 1). Assert its membership in
  ;; so-long-minor-modes instead -- that is the structural fact that makes
  ;; so-long disable it in a real session, and it is the same pattern the
  ;; evil-local-mode check above uses.
  (message "PROBE:so-long-minor-modes-has-font-lock=%S" (and (memq (quote font-lock-mode) so-long-minor-modes) t))
  (message "PROBE:so-long-minor-modes-has-visual-line=%S" (and (memq (quote visual-line-mode) so-long-minor-modes) t))
  (message "PROBE:redisplay-dont-pause-bound=%S" (boundp (quote redisplay-dont-pause)))

  ;; Force so-long on the synthetic huge-line file: so-long defers its own
  ;; automatic detection when no window displays the buffer (always true
  ;; here), so an explicit call exercises the same teardown the
  ;; window-triggered path uses (see so-long-invisible-buffer-function).
  (find-file (getenv "EDMACS_VERIFY_SINGLE_LINE_FILE"))
  ;; Record the pre-so-long state. An "is nil afterwards" assertion only means
  ;; something if the mode was actually on beforehand.
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

  ;; hl-line-in-vterm mechanism check. NOTE: this does NOT exercise the
  ;; actual installed vterm-mode-hook from modules/vterm.el -- vterm.el
  ;; unconditionally requires its native module at load time (to define
  ;; several defcustoms), and that module is not prebuilt in a clean
  ;; checkout; compiling it here would mean either an interactive
  ;; y-or-n-p prompt (which --batch cannot answer) or an unconditional,
  ;; slow, disk-writing cmake build on every verification run. Instead
  ;; this reproduces the *mechanism* modules/vterm.el relies on --
  ;; buffer-local setq-local suppresses the globalized minor mode only in
  ;; the buffer it runs in -- via a synthetic hook variable, so a real
  ;; regression in that mechanism (e.g. a typo turning setq-local into
  ;; setq) would still be caught. The grep check below separately confirms
  ;; the real hook line is present, unchanged, in modules/vterm.el.
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

# AC: hl-line is buffer-locally nil wherever vterm-mode-hook runs, and
# unaffected (global t) elsewhere. The Elisp-level check above exercises
# the mechanism synthetically (see its comment for why); this grep
# confirms the real hook registration is actually present in vterm.el.
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
# Restrict to tracked *.el source (not .elc byte-compiled build artifacts,
# which are gitignored but can exist on disk from a local `eldev compile`/
# `byte-compile-file` and would otherwise double-count every match).
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

# AC (partial -- see header comment on what a headless run can't observe):
# global-so-long-mode is enabled, and forcing so-long on a huge single-line
# buffer neutralizes evil, visual-line-mode, and font-lock the same way the
# window-triggered automatic path does.
[[ "$(get global-so-long-mode)" == "t" ]] \
  && pass "global-so-long-mode is enabled" \
  || fail "global-so-long-mode is enabled -- got $(get global-so-long-mode)"
[[ "$(get so-long-minor-modes-has-evil)" == "t" ]] \
  && pass "evil-local-mode is in so-long-minor-modes" \
  || fail "evil-local-mode is in so-long-minor-modes -- got $(get so-long-minor-modes-has-evil)"
[[ "$(get so-long-major-mode)" == "so-long-mode" ]] \
  && pass "forcing so-long on a huge single-line buffer engages so-long-mode" \
  || fail "forcing so-long on a huge single-line buffer engages so-long-mode -- got $(get so-long-major-mode)"
# Guard the guards: if the mode was already off before so-long ran, the
# "is nil afterwards" checks below prove nothing. Assert the transition.
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
