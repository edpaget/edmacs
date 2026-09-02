#!/usr/bin/env bash
#
# verify-lsp-and-completion-io.sh -- structural verification for
# edmacs-performance/phase-5-lsp-and-completion-io
#
# WHAT THIS CHECKS AND WHY IT EXISTS
#   Phase 5 rewires Java's dead java-mode-hook/keymap wiring onto
#   java-ts-mode (the mode real .java buffers actually run) and tunes six
#   idle-path LSP/completion/magit settings. Its implementation commit
#   originally shipped with zero rerunnable verification -- code review
#   flagged this (finding phase5-no-verification-script) as the same gap
#   phase 4 was remediated for one phase earlier (see
#   verify-redisplay-settings.sh's header and commit f8c5051): a plausible
#   commit-message narrative of a one-off manual `emacs -Q --batch`
#   session, uncaptured as code, so nothing catches a regression. This
#   script is that missing capture.
#
#   It also regression-guards two real bugs code review found in the
#   Java wiring itself, past the wiring-target-symbol bug the phase body
#   already anticipated:
#     - mvn.el was declared `:after java-ts-mode :commands (...)` with no
#       `:demand`, which only wraps its autoloads in an eval-after-load --
#       it never actually `require's mvn, so its :config-defined ", m"
#       keybindings never installed on a fresh session. Fixed with
#       `:demand t`.
#     - The new generic "SPC c h" hover binding (lsp-ui-doc-show, on the
#       lsp-mode-map minor-mode keymap) shadows java.el's pre-existing
#       "SPC c h" type-hierarchy prefix (on the java-ts-mode-map
#       major-mode keymap) once lsp-mode is genuinely active in the
#       buffer, because a minor-mode keymap wins over a major-mode keymap.
#       Fixed by moving Java's hierarchy prefix to "SPC c H".
#     - The SAME collision class survived on three more keys (code-review
#       finding spc-c-drt-keymap-collision): lsp-mode-map's terminal
#       "SPC c r"/"SPC c d"/"SPC c t" (lsp-rename / lsp-find-definition /
#       lsp-find-type-definition) shadow java.el's Run/Debug/Test
#       which-key prefixes on the same keys. The obvious capital-letter
#       fix ("SPC c R"/"SPC c D"/"SPC c T") turned out to only be free for
#       "T" -- lsp-mode-map separately binds capital "SPC c R" to
#       lsp-find-references and "SPC c D" to lsp-find-declaration, a
#       THIRD instance of the same class this revision found while
#       verifying the second. Fixed by moving Run/Debug to the genuinely
#       free "SPC c X" (eXecute) and "SPC c K", keeping Test on "SPC c T".
#       This script now exercises all of "h"/"r"/"d"/"t"/"R"/"D" plus the
#       new "X"/"K"/"T" prefixes, not just the originally-fixed "h" case.
#
# THE --batch + -Q --batch -l early-init.el -l init.el TRAP
#   See verify-redisplay-settings.sh's header for the full explanation:
#   `-Q` skips *automatic* early-init.el/init.el loading, but they are
#   then loaded explicitly via `-l`, and `user-emacs-directory` must be
#   pinned to the checkout under test before that `-l`, or straight's
#   bootstrap and load-module resolve against the wrong directory.
#
# WHAT IS AND ISN'T CHECKED HERE
#   Checked structurally: java-ts-mode-hook membership (lsp-deferred,
#   gradle-mode), that visiting a real .java file actually *runs* the
#   hook and calls lsp-deferred (via advice, not by requiring a real
#   jdtls server -- see below), java-ts-mode-indent-offset/tab-width/
#   indent-tabs-mode, that mvn/dap-mode/lsp-java's :config blocks
#   actually ran (via featurep, not just grep) after nothing but a plain
#   .java visit, that gradle-mode activates, keybinding resolution for
#   all four leader-key sites plus the new/moved hover, hierarchy, and
#   run/debug/test keys (proving both rounds of the SPC-c-<letter>
#   collision are gone), read-process-output-max / process-adaptive-
#   read-buffering, lsp-modeline-code-actions-enable / lsp-ui-doc-show-
#   with-cursor / magit-diff-refine-hunk / corfu-auto-prefix / consult-
#   preview-key values, and the LSP_USE_PLISTS decision comment.
#
#   Checked at the MECHANISM level, past a bare defcustom-value check, for
#   three of the idle-path ACs that are otherwise only visually observable:
#     - hover popup: `lsp-ui-doc--make-request' (the post-command-hook
#       function that would schedule a hover-request idle timer) is called
#       directly and asserted to schedule nothing when
#       `lsp-ui-doc-show-with-cursor' is nil, with a control case (forced
#       t, `lsp-feature?' mocked since no live LS workspace exists here)
#       proving the absence is the gate working and not some unrelated
#       precondition failing.
#     - consult-ripgrep arrow-through: `consult--preview-key-debounce' --
#       the exact function consult calls per candidate -- is called
#       directly against the configured `consult-preview-key' and
#       asserted to return 0.3, not the instant 0 the old plain `any'
#       value produced.
#     - magit auto-revert refresh: `magit-diff-update-hunk-refinement' --
#       what `magit-section--refine' calls on every section refresh,
#       including the one the timer triggers -- is called directly on a
#       synthetic unrefined hunk section, with `diff-refine-hunk' advised
#       to record whether it was invoked; asserted never called while
#       `magit-diff-refine-hunk' is nil.
#   None of these three requires a live LS connection, a real git repo, or
#   a display, so they run in the same headless batch pass as everything
#   else, and they test the actual code path rather than inferring
#   behavior from a variable's value alone.
#
#   NOT checked -- needs a real jdtls install + network/display: that
#   `lsp-mode' becomes non-nil from a genuine JDT.LS handshake. No jdtls
#   is installed in this sandbox, and lsp-deferred silently waits forever
#   without one, so this script advises `lsp-deferred' to record that it
#   was *called* (proving the hook wiring itself fires end-to-end on a
#   real buffer visit) without letting it try to actually launch a
#   server. The keybinding checks below separately force `lsp-mode' to a
#   non-nil value locally (simulating a real attach) to prove the
#   minor-mode-keymap-wins-over-major-mode-keymap collision is fixed --
#   this mirrors exactly how code review itself reproduced the collision.
#   A real interactive visit with jdtls reachable (JAVA_HOME set, network
#   available) is still the authoritative end-to-end check for the
#   "lsp-mode is non-nil" acceptance criterion; see the phase record.
#   Also not checked, because it needs a real display/window and a real
#   language server actually returning completions: that the lsp-ui-doc
#   hover popup visually disappears, that a magit-status buffer's refresh
#   is visibly cheaper, and that corfu's popup still "feels responsive" at
#   the raised prefix length in a real Rust/Go buffer -- these three are
#   left to interactive manual verification per the phase body; everything
#   about them that IS mechanically checkable headless is covered above.
#
#   Also NOT checked, and called out explicitly so it is not mistaken for
#   covered: AC5's comparative clause, "lsp-log-io-free timing shows fewer
#   idle requests than before". This script verifies the MECHANISM changes
#   that should reduce idle traffic (lsp-ui-doc-show-with-cursor nil,
#   lsp-modeline-code-actions-enable nil, the raised corfu prefix), but it
#   never counts requests, and no before/after idle-request baseline was
#   ever recorded for this config. The request-reduction half of AC5 has no
#   evidence, headless or manual -- treat it as an untested claim, not a
#   measured result, until someone counts textDocument/* traffic over a
#   fixed idle window with lsp-log-io temporarily enabled.
#
# USAGE
#   scripts/verify-lsp-and-completion-io.sh [path-to-edmacs-checkout]
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
# 1) Boot the real config once, visit a synthetic .java file, and dump
#    every value this script needs as "PROBE:KEY=VALUE" lines.
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
  ;; process-adaptive-read-buffering is a genuine Emacs built-in (bound,
  ;; defaulting to nil in 31.1) -- NOT something this config introduces.
  ;; The AC is that we never touch it, so its nil value is purely the
  ;; stock default, not our doing; see the grep check below for that.
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

  ;; Advise lsp-deferred to record it was CALLED, without letting it try
  ;; to actually launch a (non-existent, in this sandbox) jdtls server --
  ;; see the header comment for why this is the honest thing to check
  ;; headless.
  (defvar --verify-lsp-deferred-called nil)
  (advice-add (quote lsp-deferred) :override
              (lambda (&rest _) (setq --verify-lsp-deferred-called t)))

  (find-file (getenv "EDMACS_VERIFY_JAVA_FILE"))
  (message "PROBE:major-mode=%S" major-mode)
  ;; Structural hook membership -- checked only now (not before find-file):
  ;; java.el installs this hook membership from inside a with-eval-after-load
  ;; block that only runs once java-ts-mode.el is actually loaded, which
  ;; happens as part of mode resolution above, not at init time.
  (message "PROBE:hook-has-lsp-deferred=%S" (and (memq (quote lsp-deferred) java-ts-mode-hook) t))
  (message "PROBE:hook-has-gradle-mode=%S" (and (memq (quote gradle-mode) java-ts-mode-hook) t))
  (message "PROBE:lsp-deferred-called-on-visit=%S" --verify-lsp-deferred-called)
  (message "PROBE:indent-offset=%S" (buffer-local-value (quote java-ts-mode-indent-offset) (current-buffer)))
  (message "PROBE:tab-width=%S" (buffer-local-value (quote tab-width) (current-buffer)))
  (message "PROBE:indent-tabs-mode=%S" (buffer-local-value (quote indent-tabs-mode) (current-buffer)))
  ;; buffer-local-value returns the effective value, which for an
  ;; auto-buffer-local variable is the global default when no local binding
  ;; exists -- so the two assertions above cannot tell "java.el applied it"
  ;; apart from "java.el did nothing and core.el happens to supply the same
  ;; number". Probe whether a LOCAL binding actually exists; that is the part
  ;; java.el is responsible for. (A load-time setq in with-eval-after-load
  ;; binds these in whatever buffer was current when java-ts-mode.el loaded,
  ;; never in a real .java buffer -- verified by mutation.)
  (message "PROBE:tab-width-local=%S" (local-variable-p (quote tab-width)))
  (message "PROBE:indent-tabs-mode-local=%S" (local-variable-p (quote indent-tabs-mode)))
  (message "PROBE:gradle-mode-active=%S" (bound-and-true-p gradle-mode))

  ;; These three packages :config only when actually `require-d; confirm
  ;; that happened from nothing but the plain .java visit above, with no
  ;; manual mvn-clean/dap-debug/lsp-java-* call anywhere in this probe.
  (message "PROBE:mvn-loaded=%S" (featurep (quote mvn)))
  (message "PROBE:dap-mode-loaded=%S" (featurep (quote dap-mode)))
  (message "PROBE:lsp-java-loaded=%S" (featurep (quote lsp-java)))

  ;; Keybinding resolution. lsp-mode itself never attaches for real here
  ;; (no jdtls) -- force it locally the same way code review reproduced
  ;; the SPC-c-h collision, so the minor-mode-map-vs-major-mode-map
  ;; interaction is exercised for real, not merely inferred from source.
  (evil-local-mode 1)
  (evil-normal-state)
  (setq lsp-mode t)
  (evil-normalize-keymaps)
  (message "PROBE:key-spc-c-h=%S" (key-binding (kbd "SPC c h")))
  (message "PROBE:key-spc-c-H-t=%S" (key-binding (kbd "SPC c H t")))
  (message "PROBE:key-comma-m-c=%S" (key-binding (kbd ", m c")))
  (message "PROBE:key-spc-d-b=%S" (key-binding (kbd "SPC d b")))
  (message "PROBE:key-comma-g-b=%S" (key-binding (kbd ", g b")))
  ;; Regression guard for code-review finding spc-c-drt-keymap-collision:
  ;; lsp-mode-map (a minor-mode keymap) defines terminal "SPC c r"/
  ;; "SPC c d"/"SPC c t" bindings (lsp-rename / lsp-find-definition /
  ;; lsp-find-type-definition), which win over the Run/Debug/Test
  ;; which-key prefixes on java-ts-mode-map (a major-mode keymap) once
  ;; lsp-mode is genuinely active -- exactly the SPC-c-h collision class
  ;; already fixed once, but left unfixed here on three more keys. The
  ;; Java prefixes were moved to capital R/D/T; assert both halves: the
  ;; lowercase keys still resolve to the lsp-mode-map commands (no
  ;; regression there), and the capitalized prefixes reach dap-java.
  (message "PROBE:key-spc-c-r=%S" (key-binding (kbd "SPC c r")))
  (message "PROBE:key-spc-c-d=%S" (key-binding (kbd "SPC c d")))
  (message "PROBE:key-spc-c-t=%S" (key-binding (kbd "SPC c t")))
  (message "PROBE:key-spc-c-R=%S" (key-binding (kbd "SPC c R")))
  (message "PROBE:key-spc-c-D=%S" (key-binding (kbd "SPC c D")))
  (message "PROBE:key-spc-c-X-r=%S" (key-binding (kbd "SPC c X r")))
  (message "PROBE:key-spc-c-K-d=%S" (key-binding (kbd "SPC c K d")))
  (message "PROBE:key-spc-c-T-t=%S" (key-binding (kbd "SPC c T t")))
  (setq lsp-mode nil)

  ;; Mechanism-level (not merely value-level) check for the cursor-rest
  ;; hover-popup AC. lsp-ui-doc installs the function
  ;; lsp-ui-doc--make-request on post-command-hook; that functions very
  ;; first gate is lsp-ui-doc-show-with-cursor, and only past that gate
  ;; does it ever schedule the idle timer that would later fire a real
  ;; textDocument/hover request. No live LS workspace exists in this
  ;; sandbox (see header), so lsp-feature? is mocked to t to isolate the
  ;; gate under test from that unrelated precondition. This proves the
  ;; popup-suppression mechanism itself fires, not just that a defcustom
  ;; holds the value nil.
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

  ;; Mechanism-level check for the consult-ripgrep arrow-through AC: the
  ;; function consult--preview-key-debounce is the exact function consult
  ;; calls per candidate to decide the preview delay. With plain (quote
  ;; any) (the pre-fix value) this always returns 0 (instant preview);
  ;; with the configured (:debounce 0.3 any) it must return 0.3. The
  ;; function this-single-command-keys is empty in batch mode, so lookup
  ;; falls through to the any entry, which is exactly the code path a
  ;; real arrow-key press also falls through to (arrow keys are not among
  ;; any specific keys listed).
  (message "PROBE:consult-preview-debounce=%S"
           (consult--preview-key-debounce consult-preview-key "dummy-candidate"))

  ;; Mechanism-level check for the magit auto-revert-timer AC: the
  ;; function magit-diff-update-hunk-refinement is what
  ;; magit-section--refine calls on every section refresh (including the
  ;; one global-auto-revert-non-file-buffers triggers every 5s on a
  ;; visible magit-status buffer). Its own pcase only ever refines when
  ;; magit-diff-refine-hunk is t or all; with it nil and a not-yet-refined
  ;; section, no clause matches, so diff-refine-hunk is never called --
  ;; verified here by advising it and asserting zero calls, not just
  ;; reading the defcustom value.
  (require (quote magit-diff))
  (defvar --verify-diff-refine-hunk-called nil)
  (defun --verify-diff-refine-hunk-override (&rest _)
    (setq --verify-diff-refine-hunk-called t))
  (advice-add (quote diff-refine-hunk) :override
              (function --verify-diff-refine-hunk-override))
  (with-temp-buffer
    (insert "@@ -1,2 +1,2 @@\n-foo\n+bar\n")
    ;; The start/end/hidden/refined slots of magit-section have no EIEIO
    ;; :initarg (only type/washer/selective-highlight/etc do), so they
    ;; cannot be set via the keyword constructor -- oset after a plain
    ;; make-instance is the correct construction here.
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

# AC: opening a .java file resolves java-ts-mode (not classic java-mode --
# this is the mode-resolution fact the phase says is ALREADY correct and
# out of scope to touch; asserted here only so the checks below aren't
# silently exercising the wrong mode).
[[ "$(get major-mode)" == "java-ts-mode" ]] \
  && pass "a .java file resolves java-ts-mode" \
  || fail "a .java file resolves java-ts-mode -- got $(get major-mode)"

# AC: lsp-mode attaches on .java visit. Full end-to-end (real jdtls) is
# out of reach headless (see header); assert the two things that ARE
# checkable: the hook is wired, and visiting a real buffer actually
# fires it into calling lsp-deferred.
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
# Locality, not just value: these prove java.el actually set them per buffer
# rather than the .java buffer silently inheriting core.el's identical global
# default. Without these, dropping java.el's settings entirely still passes.
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

# Regression guard for the SPC-c-h collision code review found: the
# generic hover binding (lsp-mode-map, a minor-mode keymap) must resolve
# on its own, AND java's type-hierarchy prefix (moved to capital H, on
# java-ts-mode-map, a major-mode keymap) must still be reachable, once
# lsp-mode is genuinely active in the buffer -- exactly the condition
# under which the two used to collide on lowercase "h".
[[ "$(get key-spc-c-h)" == "lsp-ui-doc-show" ]] \
  && pass "SPC c h resolves to lsp-ui-doc-show with lsp-mode active" \
  || fail "SPC c h resolves to lsp-ui-doc-show with lsp-mode active -- got $(get key-spc-c-h)"
[[ "$(get key-spc-c-H-t)" == "lsp-java-type-hierarchy" ]] \
  && pass "SPC c H t still resolves to lsp-java-type-hierarchy (no collision with SPC c h)" \
  || fail "SPC c H t still resolves to lsp-java-type-hierarchy -- got $(get key-spc-c-H-t)"

# Regression guard for code-review finding spc-c-drt-keymap-collision:
# lsp-mode-map's terminal "SPC c r"/"SPC c d"/"SPC c t" bindings must
# still resolve to their lsp-mode commands (no accidental removal). The
# obvious capital-letter fix ("R"/"D"/"T", mirroring how "h" was
# capitalized) turned out to only be free for "T" -- lsp-mode-map ALSO
# binds capital "SPC c R" (lsp-find-references) and "SPC c D"
# (lsp-find-declaration) -- so assert those still resolve to their
# lsp-mode commands too (confirming the second collision this revision
# found), and that java's Run/Debug/Test prefixes, moved to the genuinely
# free "X"/"K"/"T", are reachable with lsp-mode genuinely active.
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

# Mechanism-level guard for "resting the cursor no longer triggers an
# automatic hover popup": lsp-ui-doc--make-request (the post-command-hook
# function that would schedule the hover-request idle timer) must gate on
# lsp-ui-doc-show-with-cursor and schedule nothing when it is nil, while
# still being *capable* of scheduling when it is t (proving the absence
# above is the gate working, not an unrelated precondition failing).
[[ "$(get hover-timer-scheduled-with-show-with-cursor-nil)" == "nil" ]] \
  && pass "lsp-ui-doc--make-request schedules no hover timer when lsp-ui-doc-show-with-cursor is nil" \
  || fail "lsp-ui-doc--make-request schedules no hover timer when nil -- got $(get hover-timer-scheduled-with-show-with-cursor-nil)"
[[ "$(get hover-timer-scheduled-with-show-with-cursor-t)" == "t" ]] \
  && pass "lsp-ui-doc--make-request does schedule a hover timer when forced t (control case)" \
  || fail "lsp-ui-doc--make-request does schedule a hover timer when forced t -- got $(get hover-timer-scheduled-with-show-with-cursor-t)"

# Mechanism-level guard for "arrowing through consult-ripgrep results
# doesn't visibly open every hovered file": consult--preview-key-debounce
# is the exact function consult calls per-candidate; it must return the
# configured 0.3s delay, not the instant (0) preview the old plain 'any
# value produced.
[[ "$(get consult-preview-debounce)" == "0.3" ]] \
  && pass "consult--preview-key-debounce returns 0.3 for consult-preview-key" \
  || fail "consult--preview-key-debounce returns 0.3 -- got $(get consult-preview-debounce)"

# Mechanism-level guard for "a magit-status buffer no longer re-runs hunk
# refinement on the auto-revert timer": magit-diff-update-hunk-refinement
# (what magit-section--refine calls on every section refresh, including
# the one the 5s auto-revert timer triggers) must never call
# diff-refine-hunk for an unrefined section when magit-diff-refine-hunk
# is nil.
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
# Its nil value above is only meaningfully "the stock default, not our
# doing" if this config never sets it anywhere -- confirm that directly.
mapfile -t EL_FILES < <(cd "$REPO_ROOT" && git ls-files '*.el')
grep_el() { (cd "$REPO_ROOT" && grep -n "$1" "${EL_FILES[@]}"); }
# Mentioning the variable in an explanatory comment (as early-init.el
# does, documenting why it's deliberately left alone) is fine; only an
# actual setq/setq-default would violate the AC.
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

# AC: LSP_USE_PLISTS decision recorded, citing the real phase slug.
if grep -q 'LSP_USE_PLISTS decision (edmacs-performance/phase-5-lsp-and-completion-io)' \
    "$REPO_ROOT/modules/core.el" 2>/dev/null; then
  pass "LSP_USE_PLISTS decision comment present and cites the correct phase slug"
else
  fail "LSP_USE_PLISTS decision comment missing or cites the wrong phase slug"
fi

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
