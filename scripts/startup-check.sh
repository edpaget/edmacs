#!/usr/bin/env bash
#
# startup-check.sh -- run the real `init.el' over a given checkout's module
# sources while the package tree stays the main checkout's.
#
# The plain startup check in .claude/CLAUDE.md can only ever test the main
# checkout, because `--init-directory' sets `user-emacs-directory' and init.el
# resolves BOTH straight's bootstrap AND every `load-module' against it.
# Pointing it at a worktree bootstraps a second package tree and can leave the
# main `straight/build' full of dangling symlinks.
#
# init.el names its module directory in exactly three string literals
# (`load-module', `load-language-config', and the languages `load-path' entry),
# all of the form "modules/...". This rewrites those three to absolute paths
# under MODULE_ROOT and leaves `user-emacs-directory' -- and therefore
# straight's bootstrap, package tree, eln-cache and custom.el -- pointed at
# PACKAGE_ROOT. Nothing is written to MODULE_ROOT and nothing in PACKAGE_ROOT
# is modified.
#
# USAGE
#   scripts/startup-check.sh [module-root] [package-root]
#
#   module-root   checkout whose modules/ and init.el are under test.
#                 Default: this script's own checkout, so running a worktree's
#                 copy checks that worktree.
#   package-root  checkout holding the populated straight tree. Default: the
#                 sibling `edmacs' main checkout when module-root is a
#                 worktree, otherwise module-root itself.
#
#   Set STARTUP_CHECK_EVAL to an extra elisp form to evaluate after init, for
#   asserting module state the config is expected to leave behind.
#
#   Exits 0 if init loaded every module without error, 1 otherwise.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_ROOT="${1:-$(cd "$SCRIPT_DIR/.." && pwd)}"
MODULE_ROOT="$(cd "$MODULE_ROOT" && pwd)"

if [[ ! -f "$MODULE_ROOT/init.el" ]]; then
  echo "error: $MODULE_ROOT does not look like an edmacs checkout (no init.el)" >&2
  exit 2
fi

if [[ -n "${2:-}" ]]; then
  PACKAGE_ROOT="$(cd "$2" && pwd)"
elif [[ "$MODULE_ROOT" == */edmacs__worktrees/* ]]; then
  PACKAGE_ROOT="${MODULE_ROOT%%/edmacs__worktrees/*}/edmacs"
else
  PACKAGE_ROOT="$MODULE_ROOT"
fi

BOOTSTRAP="$PACKAGE_ROOT/straight/repos/straight.el/bootstrap.el"
if [[ ! -f "$BOOTSTRAP" ]]; then
  echo "error: $PACKAGE_ROOT has no bootstrapped straight tree ($BOOTSTRAP)." >&2
  echo "       Pass the main checkout as the second argument. Never let this" >&2
  echo "       resolve to a worktree -- init.el would bootstrap a second" >&2
  echo "       package tree there." >&2
  exit 2
fi

SHIM="$(mktemp -t edmacs-startup-check).el"
trap 'rm -f "$SHIM" "${SHIM%.el}"' EXIT

cat > "$SHIM" <<ELISP
;;; startup-check shim -*- lexical-binding: t -*-
(let* ((src (with-temp-buffer
              (insert-file-contents "$MODULE_ROOT/init.el")
              (buffer-string)))
       (prefix "\"$MODULE_ROOT/modules/")
       (redirected (string-replace "\"modules/" prefix src))
       (n (1- (length (split-string redirected prefix)))))
  (unless (= n 3)
    (error "startup-check: expected 3 module-path literals in init.el, found %d" n))
  (eval (car (read-from-string (concat "(progn\n" redirected "\n)"))) t))
ELISP

echo "module sources: $MODULE_ROOT"
echo "package tree:   $PACKAGE_ROOT"
echo

OUT="$(emacs --batch --init-directory="$PACKAGE_ROOT" -l "$SHIM" \
         --eval '(princ (format "\nstartup-check: themes=%S\n" custom-enabled-themes))' \
         ${STARTUP_CHECK_EVAL:+--eval "$STARTUP_CHECK_EVAL"} \
         -f kill-emacs 2>&1)"

echo "$OUT" | grep -E '^startup-check:|^assert:'

FAILED=0

# Drop this script's own marker lines before grepping: an assertion naming a
# buffer like *Flycheck errors* would otherwise read as an init failure.
DIAGNOSTICS="$(echo "$OUT" | grep -vE '^startup-check:|^assert:' \
                 | grep -Ei 'error|void-function|Cannot open load file')"

if [[ -n "$DIAGNOSTICS" ]]; then
  echo "FAIL: init.el reported errors:"
  echo "$DIAGNOSTICS"
  FAILED=1
else
  echo "PASS: init.el loaded every module without error"
fi

# `--batch' implies `-q', so a shim that silently failed to eval init.el would
# otherwise look like a clean run.
if echo "$OUT" | grep -q '^startup-check: themes=nil$'; then
  echo "FAIL: custom-enabled-themes is nil -- init.el never ran"
  FAILED=1
else
  echo "PASS: init.el really ran (custom-enabled-themes is set)"
fi

exit $FAILED
