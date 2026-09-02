#!/usr/bin/env bash
#
# verify-lsp-and-completion-io.sh -- PASS/FAIL checks for the Java wiring on
# java-ts-mode and the idle-path LSP/completion/magit settings.
#
# Boots the real config under `emacs -Q --batch -l early-init.el -l init.el'
# with `user-emacs-directory' pinned to the checkout (see
# verify-redisplay-settings.sh). No jdtls is installed, so `lsp-deferred' is
# advised to record that it was called rather than launch a server, and
# `lsp-mode' is forced on locally for the keybinding checks. Those guard the
# minor-mode-map-beats-major-mode-map collisions under SPC c: lsp-mode-map
# owns h/r/d/t/R/D, so java.el uses H/X/K/T.
#
# Three idle-path settings are checked at the mechanism level, not just the
# variable value: lsp-ui-doc's hover timer, consult's preview debounce, and
# magit's hunk refinement on refresh.
#
# Not checked: a real jdtls handshake, anything visual, and the "fewer idle
# requests" claim, which has never been measured.
#
# USAGE
#   scripts/verify-lsp-and-completion-io.sh [path-to-edmacs-checkout]
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
# 1) Boot the real config once, visit a .java file, dump PROBE lines.
# ---------------------------------------------------------------------------

TMP_JAVA="$(mktemp -t edmacs-java-verify.XXXXXX).java"
trap 'rm -f "$TMP_JAVA"' EXIT
cat > "$TMP_JAVA" <<'JAVA'
package com.example;

public class Sample {
    public static void main(String[] args) {}
}
JAVA

PROBE_ELISP='
(progn
  (require (quote lsp-mode))
  (require (quote lsp-ui))
  (require (quote consult))
  (require (quote magit))

  (message "PROBE:read-process-output-max=%S" read-process-output-max)
  ;; Built-in, nil by default in 31.1; the AC is that we never set it.
  (message "PROBE:process-adaptive-read-buffering-bound=%S" (boundp (quote process-adaptive-read-buffering)))
  (message "PROBE:process-adaptive-read-buffering-value=%S"
           (if (boundp (quote process-adaptive-read-buffering))
               (symbol-value (quote process-adaptive-read-buffering))
             (quote unbound)))
  (message "PROBE:lsp-modeline-code-actions-enable=%S" lsp-modeline-code-actions-enable)
  (message "PROBE:lsp-ui-doc-show-with-cursor=%S" lsp-ui-doc-show-with-cursor)
  (message "PROBE:lsp-enable-symbol-highlighting=%S" lsp-enable-symbol-highlighting)
  (message "PROBE:corfu-auto-prefix=%S" corfu-auto-prefix)
  (message "PROBE:consult-preview-key=%S" consult-preview-key)
  (message "PROBE:magit-diff-refine-hunk=%S" magit-diff-refine-hunk)

  ;; Record that lsp-deferred was called without launching a server.
  (defvar --verify-lsp-deferred-called nil)
  (advice-add (quote lsp-deferred) :override
              (lambda (&rest _) (setq --verify-lsp-deferred-called t)))

  (find-file (getenv "EDMACS_VERIFY_JAVA_FILE"))
  (message "PROBE:major-mode=%S" major-mode)
  ;; Checked after find-file: java.el adds these from with-eval-after-load.
  (message "PROBE:hook-has-lsp-deferred=%S" (and (memq (quote lsp-deferred) java-ts-mode-hook) t))
  (message "PROBE:hook-has-gradle-mode=%S" (and (memq (quote gradle-mode) java-ts-mode-hook) t))
  (message "PROBE:lsp-deferred-called-on-visit=%S" --verify-lsp-deferred-called)
  (message "PROBE:indent-offset=%S" (buffer-local-value (quote java-ts-mode-indent-offset) (current-buffer)))
  (message "PROBE:tab-width=%S" (buffer-local-value (quote tab-width) (current-buffer)))
  (message "PROBE:indent-tabs-mode=%S" (buffer-local-value (quote indent-tabs-mode) (current-buffer)))
  ;; buffer-local-value falls back to the global default, so also probe
  ;; that a LOCAL binding exists; that is the part java.el is responsible for.
  (message "PROBE:tab-width-local=%S" (local-variable-p (quote tab-width)))
  (message "PROBE:indent-tabs-mode-local=%S" (local-variable-p (quote indent-tabs-mode)))
  (message "PROBE:gradle-mode-active=%S" (bound-and-true-p gradle-mode))

  ;; These only :config when required; confirm a plain visit did that.
  (message "PROBE:mvn-loaded=%S" (featurep (quote mvn)))
  (message "PROBE:dap-mode-loaded=%S" (featurep (quote dap-mode)))
  (message "PROBE:lsp-java-loaded=%S" (featurep (quote lsp-java)))

  ;; Force lsp-mode on locally so the minor-mode keymap is active.
  (evil-local-mode 1)
  (evil-normal-state)
  (setq lsp-mode t)
  (evil-normalize-keymaps)
  (message "PROBE:key-spc-c-h=%S" (key-binding (kbd "SPC c h")))
  (message "PROBE:key-spc-c-H-t=%S" (key-binding (kbd "SPC c H t")))
  (message "PROBE:key-comma-m-c=%S" (key-binding (kbd ", m c")))
  (message "PROBE:key-spc-d-b=%S" (key-binding (kbd "SPC d b")))
  (message "PROBE:key-comma-g-b=%S" (key-binding (kbd ", g b")))
  ;; Lowercase keys must still reach lsp-mode-map; the java prefixes moved
  ;; to X/K/T (R and D are also taken by lsp-mode-map).
  (message "PROBE:key-spc-c-r=%S" (key-binding (kbd "SPC c r")))
  (message "PROBE:key-spc-c-d=%S" (key-binding (kbd "SPC c d")))
  (message "PROBE:key-spc-c-t=%S" (key-binding (kbd "SPC c t")))
  (message "PROBE:key-spc-c-R=%S" (key-binding (kbd "SPC c R")))
  (message "PROBE:key-spc-c-D=%S" (key-binding (kbd "SPC c D")))
  (message "PROBE:key-spc-c-X-r=%S" (key-binding (kbd "SPC c X r")))
  (message "PROBE:key-spc-c-K-d=%S" (key-binding (kbd "SPC c K d")))
  (message "PROBE:key-spc-c-T-t=%S" (key-binding (kbd "SPC c T t")))
  (setq lsp-mode nil)

  ;; Hover mechanism: lsp-ui-doc--make-request gates on
  ;; lsp-ui-doc-show-with-cursor before scheduling its timer. lsp-feature? is
  ;; mocked since no workspace exists.
  (require (quote lsp-ui-doc))
  (defun --verify-lsp-feature-t (&rest _) t)
  (advice-add (quote lsp-feature?) :override (function --verify-lsp-feature-t))
  (setq this-command (quote self-insert-command)
        lsp-ui-doc--bounds nil
        lsp-ui-doc--hide-on-next-command nil)
  (lsp-ui-util-safe-kill-timer lsp-ui-doc--timer)
  (setq lsp-ui-doc--timer nil)
  (let ((lsp-ui-doc-show-with-cursor nil))
    (lsp-ui-doc--make-request))
  (message "PROBE:hover-timer-scheduled-with-show-with-cursor-nil=%S" (and lsp-ui-doc--timer t))
  (lsp-ui-util-safe-kill-timer lsp-ui-doc--timer)
  (setq lsp-ui-doc--timer nil lsp-ui-doc--bounds nil)
  (let ((lsp-ui-doc-show-with-cursor t))
    (lsp-ui-doc--make-request))
  (message "PROBE:hover-timer-scheduled-with-show-with-cursor-t=%S" (and lsp-ui-doc--timer t))
  (lsp-ui-util-safe-kill-timer lsp-ui-doc--timer)
  (setq lsp-ui-doc--timer nil)
  (advice-remove (quote lsp-feature?) (function --verify-lsp-feature-t))

  (kill-buffer)

  ;; consult--preview-key-debounce is what consult calls per candidate; a
  ;; plain any returns 0, the configured value must return 0.3.
  (message "PROBE:consult-preview-debounce=%S"
           (consult--preview-key-debounce consult-preview-key "dummy-candidate"))

  ;; magit-diff-update-hunk-refinement runs on every refresh; with
  ;; magit-diff-refine-hunk nil it must never call diff-refine-hunk.
  (require (quote magit-diff))
  (defvar --verify-diff-refine-hunk-called nil)
  (defun --verify-diff-refine-hunk-override (&rest _)
    (setq --verify-diff-refine-hunk-called t))
  (advice-add (quote diff-refine-hunk) :override
              (function --verify-diff-refine-hunk-override))
  (with-temp-buffer
    (insert "@@ -1,2 +1,2 @@\n-foo\n+bar\n")
    ;; start/end/hidden/refined have no :initarg; oset after make-instance.
    (let ((sec (make-instance (quote magit-hunk-section))))
      (oset sec start (point-min))
      (oset sec end (point-max))
      (oset sec hidden nil)
      (oset sec refined nil)
      (magit-diff-update-hunk-refinement sec)))
  (message "PROBE:magit-refine-called-with-refine-hunk-nil=%S"
           (and --verify-diff-refine-hunk-called t))
  (advice-remove (quote diff-refine-hunk) (function --verify-diff-refine-hunk-override))

  (message "PROBE:warnings-buffer=%S" (and (get-buffer "*Warnings*") t))
  (message "PROBE:probe-complete=t"))
'

OUTPUT="$(cd "$REPO_ROOT" && EDMACS_VERIFY_JAVA_FILE="$TMP_JAVA" \
  emacs -Q --batch \
  --eval "(setq user-emacs-directory (file-name-as-directory \"$REPO_ROOT\"))" \
  -l "$REPO_ROOT/early-init.el" -l "$REPO_ROOT/init.el" \
  --eval "$PROBE_ELISP" 2>&1)"
STATUS=$?

get() {
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

# AC: .java resolves java-ts-mode (so the checks below test the right mode).
[[ "$(get major-mode)" == "java-ts-mode" ]] \
  && pass "a .java file resolves java-ts-mode" \
  || fail "a .java file resolves java-ts-mode -- got $(get major-mode)"

# AC: lsp-mode attaches on visit. Headless, assert the hook is wired and fires.
[[ "$(get hook-has-lsp-deferred)" == "t" ]] \
  && pass "java-ts-mode-hook contains lsp-deferred" \
  || fail "java-ts-mode-hook contains lsp-deferred -- got $(get hook-has-lsp-deferred)"
[[ "$(get hook-has-gradle-mode)" == "t" ]] \
  && pass "java-ts-mode-hook contains gradle-mode" \
  || fail "java-ts-mode-hook contains gradle-mode -- got $(get hook-has-gradle-mode)"
[[ "$(get lsp-deferred-called-on-visit)" == "t" ]] \
  && pass "visiting a real .java file actually invokes lsp-deferred (hook fires end-to-end)" \
  || fail "visiting a real .java file actually invokes lsp-deferred -- got $(get lsp-deferred-called-on-visit)"

# AC: java-ts-mode-indent-offset in effect; SPC c / SPC d / , m / , g
# bindings resolve on java-ts-mode-map.
[[ "$(get indent-offset)" == "4" ]] \
  && pass "java-ts-mode-indent-offset is 4 in a .java buffer" \
  || fail "java-ts-mode-indent-offset is 4 in a .java buffer -- got $(get indent-offset)"
[[ "$(get tab-width)" == "4" ]] \
  && pass "tab-width is 4 in a .java buffer" \
  || fail "tab-width is 4 in a .java buffer -- got $(get tab-width)"
[[ "$(get indent-tabs-mode)" == "nil" ]] \
  && pass "indent-tabs-mode is nil in a .java buffer" \
  || fail "indent-tabs-mode is nil in a .java buffer -- got $(get indent-tabs-mode)"
# Locality, not just value: without this, dropping java.el's settings still
# passes via core.el's identical global default.
[[ "$(get tab-width-local)" == "t" ]] \
  && pass "tab-width is buffer-locally bound in a .java buffer (not inherited from the global default)" \
  || fail "tab-width is buffer-locally bound in a .java buffer -- got $(get tab-width-local)"
[[ "$(get indent-tabs-mode-local)" == "t" ]] \
  && pass "indent-tabs-mode is buffer-locally bound in a .java buffer (not inherited from the global default)" \
  || fail "indent-tabs-mode is buffer-locally bound in a .java buffer -- got $(get indent-tabs-mode-local)"

[[ "$(get mvn-loaded)" == "t" ]] \
  && pass "mvn.el loads (and its , m keybindings install) from a plain .java visit alone" \
  || fail "mvn.el loads from a plain .java visit alone -- got $(get mvn-loaded)"
[[ "$(get dap-mode-loaded)" == "t" ]] \
  && pass "dap-mode loads from a plain .java visit alone" \
  || fail "dap-mode loads from a plain .java visit alone -- got $(get dap-mode-loaded)"
[[ "$(get lsp-java-loaded)" == "t" ]] \
  && pass "lsp-java loads from a plain .java visit alone" \
  || fail "lsp-java loads from a plain .java visit alone -- got $(get lsp-java-loaded)"

[[ "$(get key-comma-m-c)" == "mvn-clean" ]] \
  && pass ", m c resolves to mvn-clean" \
  || fail ", m c resolves to mvn-clean -- got $(get key-comma-m-c)"
[[ "$(get key-spc-d-b)" == "dap-breakpoint-toggle" ]] \
  && pass "SPC d b resolves to dap-breakpoint-toggle" \
  || fail "SPC d b resolves to dap-breakpoint-toggle -- got $(get key-spc-d-b)"
[[ "$(get key-comma-g-b)" == "gradle-build" ]] \
  && pass ", g b resolves to gradle-build" \
  || fail ", g b resolves to gradle-build -- got $(get key-comma-g-b)"

# SPC c h (lsp-mode-map) and SPC c H t (java-ts-mode-map) must both resolve
# with lsp-mode active.
[[ "$(get key-spc-c-h)" == "lsp-ui-doc-show" ]] \
  && pass "SPC c h resolves to lsp-ui-doc-show with lsp-mode active" \
  || fail "SPC c h resolves to lsp-ui-doc-show with lsp-mode active -- got $(get key-spc-c-h)"
[[ "$(get key-spc-c-H-t)" == "lsp-java-type-hierarchy" ]] \
  && pass "SPC c H t still resolves to lsp-java-type-hierarchy (no collision with SPC c h)" \
  || fail "SPC c H t still resolves to lsp-java-type-hierarchy -- got $(get key-spc-c-H-t)"

# Lowercase r/d/t and capital R/D stay with lsp-mode-map; java's Run/Debug/
# Test prefixes on X/K/T must be reachable with lsp-mode active.
[[ "$(get key-spc-c-r)" == "lsp-rename" ]] \
  && pass "SPC c r still resolves to lsp-rename with lsp-mode active" \
  || fail "SPC c r still resolves to lsp-rename with lsp-mode active -- got $(get key-spc-c-r)"
[[ "$(get key-spc-c-d)" == "lsp-find-definition" ]] \
  && pass "SPC c d still resolves to lsp-find-definition with lsp-mode active" \
  || fail "SPC c d still resolves to lsp-find-definition with lsp-mode active -- got $(get key-spc-c-d)"
[[ "$(get key-spc-c-t)" == "lsp-find-type-definition" ]] \
  && pass "SPC c t still resolves to lsp-find-type-definition with lsp-mode active" \
  || fail "SPC c t still resolves to lsp-find-type-definition with lsp-mode active -- got $(get key-spc-c-t)"
[[ "$(get key-spc-c-R)" == "lsp-find-references" ]] \
  && pass "SPC c R still resolves to lsp-find-references with lsp-mode active (confirms capital R was NOT a safe choice for java.el)" \
  || fail "SPC c R still resolves to lsp-find-references with lsp-mode active -- got $(get key-spc-c-R)"
[[ "$(get key-spc-c-D)" == "lsp-find-declaration" ]] \
  && pass "SPC c D still resolves to lsp-find-declaration with lsp-mode active (confirms capital D was NOT a safe choice for java.el)" \
  || fail "SPC c D still resolves to lsp-find-declaration with lsp-mode active -- got $(get key-spc-c-D)"
[[ "$(get key-spc-c-X-r)" == "dap-java-run-test-class" ]] \
  && pass "SPC c X r resolves to dap-java-run-test-class (X is genuinely unclaimed)" \
  || fail "SPC c X r resolves to dap-java-run-test-class -- got $(get key-spc-c-X-r)"
[[ "$(get key-spc-c-K-d)" == "dap-java-debug-test-class" ]] \
  && pass "SPC c K d resolves to dap-java-debug-test-class (K is genuinely unclaimed)" \
  || fail "SPC c K d resolves to dap-java-debug-test-class -- got $(get key-spc-c-K-d)"
[[ "$(get key-spc-c-T-t)" == "dap-java-run-test-method" ]] \
  && pass "SPC c T t resolves to dap-java-run-test-method (no collision with SPC c t)" \
  || fail "SPC c T t resolves to dap-java-run-test-method -- got $(get key-spc-c-T-t)"

# Hover: no timer scheduled when show-with-cursor is nil; one scheduled when
# forced t (control).
[[ "$(get hover-timer-scheduled-with-show-with-cursor-nil)" == "nil" ]] \
  && pass "lsp-ui-doc--make-request schedules no hover timer when lsp-ui-doc-show-with-cursor is nil" \
  || fail "lsp-ui-doc--make-request schedules no hover timer when nil -- got $(get hover-timer-scheduled-with-show-with-cursor-nil)"
[[ "$(get hover-timer-scheduled-with-show-with-cursor-t)" == "t" ]] \
  && pass "lsp-ui-doc--make-request does schedule a hover timer when forced t (control case)" \
  || fail "lsp-ui-doc--make-request does schedule a hover timer when forced t -- got $(get hover-timer-scheduled-with-show-with-cursor-t)"

# consult preview debounce is 0.3, not the instant 0 of plain `any'.
[[ "$(get consult-preview-debounce)" == "0.3" ]] \
  && pass "consult--preview-key-debounce returns 0.3 for consult-preview-key" \
  || fail "consult--preview-key-debounce returns 0.3 -- got $(get consult-preview-debounce)"

# magit never refines hunks on refresh when magit-diff-refine-hunk is nil.
[[ "$(get magit-refine-called-with-refine-hunk-nil)" == "nil" ]] \
  && pass "magit-diff-update-hunk-refinement never calls diff-refine-hunk when magit-diff-refine-hunk is nil" \
  || fail "magit-diff-update-hunk-refinement calls diff-refine-hunk when magit-diff-refine-hunk is nil -- got $(get magit-refine-called-with-refine-hunk-nil)"

# AC: gradle-mode is active in a .java buffer.
[[ "$(get gradle-mode-active)" == "t" ]] \
  && pass "gradle-mode is active in a .java buffer" \
  || fail "gradle-mode is active in a .java buffer -- got $(get gradle-mode-active)"

# AC: read-process-output-max=1MB; process-adaptive-read-buffering unset.
[[ "$(get read-process-output-max)" == "1048576" ]] \
  && pass "read-process-output-max is 1048576" \
  || fail "read-process-output-max is 1048576 -- got $(get read-process-output-max)"
[[ "$(get process-adaptive-read-buffering-bound)" == "t" ]] \
  && pass "process-adaptive-read-buffering is a genuine Emacs built-in (bound)" \
  || fail "process-adaptive-read-buffering is a genuine Emacs built-in (bound) -- got $(get process-adaptive-read-buffering-bound)"
[[ "$(get process-adaptive-read-buffering-value)" == "nil" ]] \
  && pass "process-adaptive-read-buffering is nil (its stock 31.1 default)" \
  || fail "process-adaptive-read-buffering is nil -- got $(get process-adaptive-read-buffering-value)"
# Confirm this config never sets it; a mention in a comment is fine.
mapfile -t EL_FILES < <(cd "$REPO_ROOT" && git ls-files '*.el')
grep_el() { (cd "$REPO_ROOT" && grep -n "$1" "${EL_FILES[@]}"); }
if ! grep_el 'set[qf]-\?\(default\)\?[^;]*process-adaptive-read-buffering' >/dev/null 2>&1; then
  pass "process-adaptive-read-buffering is never set by this config's source (its nil reading is purely Emacs's own default)"
else
  fail "process-adaptive-read-buffering is set somewhere in config source (the AC requires leaving it untouched)"
fi

# AC: idle-path settings.
[[ "$(get lsp-modeline-code-actions-enable)" == "nil" ]] \
  && pass "lsp-modeline-code-actions-enable is nil" \
  || fail "lsp-modeline-code-actions-enable is nil -- got $(get lsp-modeline-code-actions-enable)"
[[ "$(get lsp-ui-doc-show-with-cursor)" == "nil" ]] \
  && pass "lsp-ui-doc-show-with-cursor is nil" \
  || fail "lsp-ui-doc-show-with-cursor is nil -- got $(get lsp-ui-doc-show-with-cursor)"
[[ "$(get lsp-enable-symbol-highlighting)" == "t" ]] \
  && pass "lsp-enable-symbol-highlighting deliberately left t (documented decision)" \
  || fail "lsp-enable-symbol-highlighting deliberately left t -- got $(get lsp-enable-symbol-highlighting)"
[[ "$(get corfu-auto-prefix)" == "3" ]] \
  && pass "corfu-auto-prefix is 3" \
  || fail "corfu-auto-prefix is 3 -- got $(get corfu-auto-prefix)"
[[ "$(get consult-preview-key)" == "(:debounce 0.3 any)" ]] \
  && pass "consult-preview-key is (:debounce 0.3 any)" \
  || fail "consult-preview-key is (:debounce 0.3 any) -- got $(get consult-preview-key)"
[[ "$(get magit-diff-refine-hunk)" == "nil" ]] \
  && pass "magit-diff-refine-hunk is nil" \
  || fail "magit-diff-refine-hunk is nil -- got $(get magit-diff-refine-hunk)"

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
