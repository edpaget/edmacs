#!/usr/bin/env bash
#
# go-eglot-check.sh -- batch evidence that a Go buffer comes up under eglot
# and gopls, with no lsp-mode in sight.
#
# Everything here is reproducible in batch; nothing needs a frame or a
# screenshot. It builds a throwaway Go module OUTSIDE this repo, loads the
# real config over it through `startup-check.sh', and asserts:
#
#   - the buffer is eglot-managed and the server process is gopls
#   - `textDocument/definition' answers with the defining file
#   - `lsp-mode' never attached and `lsp-workspaces' is empty
#   - the `:gopls' workspace configuration reached the server
#   - gopls' `unusedparams' analyzer fires, so the `analyses' setting is
#     in effect rather than merely echoed back
#   - `edmacs-modeline-diagnostics' renders flymake's counts
#
# TWO TRAPS THIS SCRIPT EXISTS TO NAVIGATE
#
# 1. `eglot-ensure' never connects in batch on its own. It appends a
#    buffer-local `post-command-hook' entry and returns (eglot.el's
#    `eglot-ensure'); the noninteractive top level runs no command loop, so
#    that hook never fires. The obvious verification -- find-file, then poll
#    `eglot-managed-p' -- therefore spins to its deadline with zero processes
#    spawned and looks like a broken config. The fix is one `run-hooks' call,
#    below; the check prints the before/after pair so the trap stays visible.
#
# 2. The module must live outside the edmacs checkout. project.el roots a
#    module created inside a worktree at the edmacs git repo, so gopls sees no
#    go.mod and reports "no packages".
#
# gopls resolves through mise here, so the run needs a login shell PATH -- the
# script re-execs itself through one when `gopls' is missing, mirroring how
# `exec-path-from-shell' feeds PATH to the real daemon.
#
# USAGE
#   scripts/go-eglot-check.sh [module-root] [package-root]
#
#   Arguments are passed straight through to startup-check.sh; the defaults
#   (this checkout's modules, the main checkout's package tree) are what you
#   want from a worktree.
#
#   Exits 0 when every check passes, 1 otherwise.

set -uo pipefail

if ! command -v gopls >/dev/null 2>&1 && [[ -z "${GO_EGLOT_CHECK_RELOGIN:-}" ]]; then
  export GO_EGLOT_CHECK_RELOGIN=1
  exec bash -lc "$(printf '%q ' "$0" "$@")"
fi

for tool in go gopls emacs; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "error: $tool is not on PATH (login shell PATH is what the daemon sees)" >&2
    exit 2
  fi
done

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SCRATCH="$(mktemp -d -t edmacs-go-eglot)"
FORM="$SCRATCH/evidence.el"
trap 'rm -rf "$SCRATCH"' EXIT

MODULE="$SCRATCH/module"
mkdir -p "$MODULE/broken"

# `git init' so project.el roots the project at the module. Without it the
# nearest VC root is whatever encloses TMPDIR, and gopls gets the wrong view.
(cd "$MODULE" && git init -q . && go mod init example.com/scratch >/dev/null 2>&1) || {
  echo "error: could not create the scratch Go module in $MODULE" >&2
  exit 2
}

cat > "$MODULE/greet.go" <<'GO'
package main

// greet is defined here and called from main.go, so a definition request
// from the call site has somewhere to land.
func greet(name string) string {
	return "hello " + name
}
GO

cat > "$MODULE/main.go" <<'GO'
package main

import "fmt"

// shout carries an unused parameter: gopls' unusedparams analyzer reports it
// only when the `analyses' workspace configuration actually reached the
// server, which makes it a live check of the translated lsp-go settings.
func shout(msg string, unused int) string {
	return msg + "!"
}

func main() {
	fmt.Println(shout(greet("world"), 1))
}
GO

# Its own package, so the type error below cannot spoil the main package's
# definition and analyzer results.
cat > "$MODULE/broken/broken.go" <<'GO'
package broken

// Broken does not type-check. gopls reports an error diagnostic here, which
// is what the modeline segment is asked to render.
func Broken() int {
	return "not an int"
}
GO

cat > "$FORM" <<'ELISP'
;;; go-eglot-check evidence -*- lexical-binding: t -*-

(defvar edmacs-go-check-root (getenv "GO_EGLOT_CHECK_MODULE"))
(defvar edmacs-go-check-failures 0)
(defvar edmacs-go-check-total 0)

(defun edmacs-go-check--report (ok name fmt &rest args)
  (setq edmacs-go-check-total (1+ edmacs-go-check-total))
  (unless ok (setq edmacs-go-check-failures (1+ edmacs-go-check-failures)))
  (princ (format "assert: [%s] %-22s %s\n" (if ok "PASS" "FAIL") name
                 (apply #'format fmt args))))

(defun edmacs-go-check--note (fmt &rest args)
  (princ (concat "assert:        " (apply #'format fmt args) "\n")))

(defun edmacs-go-check--wait (predicate seconds)
  "Poll PREDICATE until it returns non-nil or SECONDS elapse.
`sit-for' as well as `accept-process-output': eglot finishes an
asynchronous connect and flymake reports diagnostics from timers."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (sit-for 0.05))
    (funcall predicate)))

(defun edmacs-go-check--connect ()
  "Open the buffer's server the way a command loop would.
`eglot-ensure' only appends to `post-command-hook'; batch runs no command
loop, so without this every later check fails against a config that is
perfectly fine interactively."
  (run-hooks 'post-command-hook)
  (edmacs-go-check--wait (lambda () (eglot-managed-p)) 60))

(defun edmacs-go-check--workspace-configuration (server)
  (save-window-excursion (eglot-show-workspace-configuration server))
  (let ((buffer (seq-find (lambda (b)
                            (string-match-p "workspace configuration" (buffer-name b)))
                          (buffer-list))))
    (and buffer
         (with-current-buffer buffer
           (replace-regexp-in-string
            "[ \n]+" " " (buffer-substring-no-properties (point-min) (point-max)))))))

(find-file (expand-file-name "main.go" edmacs-go-check-root))

(edmacs-go-check--report (eq major-mode 'go-ts-mode) "major-mode" "%S" major-mode)
(edmacs-go-check--report (and (memq 'eglot-ensure go-ts-mode-hook)
                              (not (memq 'lsp-deferred go-ts-mode-hook)))
                         "go-ts-mode-hook" "eglot-ensure only, no lsp-deferred")

;; The trap, shown rather than described: managed is nil until the deferred
;; `post-command-hook' entry gets a chance to run.
(edmacs-go-check--note "before post-command-hook: managed=%S server=%S"
                       (eglot-managed-p) (eglot-current-server))
(edmacs-go-check--connect)

(let* ((server (eglot-current-server))
       (command (and server (process-command (jsonrpc--process server)))))
  (edmacs-go-check--report (eglot-managed-p) "eglot-managed" "%S" (eglot-managed-p))
  (edmacs-go-check--report (and command (string-match-p "gopls" (car command)))
                           "server-is-gopls" "%S" command)
  (edmacs-go-check--report (not (or (bound-and-true-p lsp-mode)
                                    (and (fboundp 'lsp-workspaces) (lsp-workspaces))))
                           "no-lsp-mode" "lsp-mode=%S lsp-workspaces=%S"
                           (bound-and-true-p lsp-mode)
                           (and (fboundp 'lsp-workspaces) (lsp-workspaces)))

  (goto-char (point-min))
  (search-forward "greet(")
  (backward-char 3)
  (let ((locations (edmacs-go-check--wait
                    (lambda ()
                      (ignore-errors
                        (let ((r (eglot--request server :textDocument/definition
                                                 (eglot--TextDocumentPositionParams))))
                          (and (not (eq r :null)) (> (length r) 0) r))))
                    60)))
    (edmacs-go-check--report
     (and locations
          (string-match-p "greet\\.go" (plist-get (elt locations 0) :uri)))
     "definition-answers" "%S" locations))

  (let ((configuration (edmacs-go-check--workspace-configuration server)))
    (edmacs-go-check--report
     (and configuration
          (string-match-p "\"gopls\"" configuration)
          (string-match-p "\"unusedparams\": true" configuration)
          (string-match-p "\"usePlaceholders\": true" configuration))
     "workspace-config" "%s" configuration)
    ;; Phase decision: hints stay off, exact parity with the lsp-mode setup.
    (edmacs-go-check--report (not (string-match-p "hints" (or configuration "")))
                             "no-inlay-hints-section" "eglot-inlay-hints-mode=%S"
                             (bound-and-true-p eglot-inlay-hints-mode)))

  (let ((diagnostics (edmacs-go-check--wait
                      (lambda ()
                        (let ((d (flymake-diagnostics)))
                          (and d (mapcar #'flymake-diagnostic-text d))))
                      60)))
    (edmacs-go-check--report
     (seq-some (lambda (text) (string-match-p "unusedparams" text)) (or diagnostics '()))
     "analyses-in-effect" "%S" diagnostics)))

(find-file (expand-file-name "broken/broken.go" edmacs-go-check-root))
(edmacs-go-check--connect)
;; Trap 3, same family as the first: the second buffer of an already-managed
;; project has flymake switched on from `after-change-major-mode-hook', whose
;; first check the command loop would run and batch never does.
(flymake-start t t)
(let ((diagnostics (edmacs-go-check--wait
                    (lambda ()
                      (seq-find (lambda (d) (eq (flymake-diagnostic-type d) 'eglot-error))
                                (flymake-diagnostics)))
                    60)))
  (edmacs-go-check--report diagnostics "error-diagnostic" "%S"
                           (and diagnostics (flymake-diagnostic-text diagnostics))))
(edmacs-go-check--report (and (bound-and-true-p flymake-mode)
                              (not (bound-and-true-p flycheck-mode))
                              (string-match-p "\\`E[0-9]" (edmacs-modeline-diagnostics)))
                         "modeline-from-flymake" "%S flymake=%S flycheck=%S"
                         (substring-no-properties (edmacs-modeline-diagnostics))
                         (bound-and-true-p flymake-mode)
                         (bound-and-true-p flycheck-mode))

;; Every other language is still lsp-mode's, still reporting through flycheck.
;; Their `lsp-deferred' hooks are registered from `with-eval-after-load' and
;; `use-package' `:config' forms, so the mode libraries have to be loaded
;; before the hook lists say anything at all.
(dolist (library '(rust-ts-mode typescript-ts-mode js clojure-ts-mode java-ts-mode))
  (require library nil t))
(dolist (mode '(rust-ts-mode typescript-ts-mode tsx-ts-mode js-ts-mode
                clojure-ts-mode java-ts-mode))
  (let ((hook (intern (format "%s-hook" mode))))
    (edmacs-go-check--report (and (memq 'lsp-deferred (symbol-value hook))
                                  (not (memq 'eglot-ensure (symbol-value hook))))
                             (format "%s" mode) "still on lsp-deferred")))
(edmacs-go-check--report (bound-and-true-p global-flycheck-mode) "global-flycheck-mode" "on")

(princ (format "assert: go-eglot-check: %d checks, %d failed\n"
               edmacs-go-check-total edmacs-go-check-failures))
ELISP

export GO_EGLOT_CHECK_MODULE="$MODULE"
export STARTUP_CHECK_EVAL="(load \"$FORM\" nil t)"

OUT="$("$SCRIPT_DIR/startup-check.sh" "$@" 2>&1)"
STATUS=$?
echo "$OUT"

if [[ $STATUS -ne 0 ]]; then
  echo "FAIL: startup-check.sh did not come up clean"
  exit 1
fi

if ! grep -q '^assert: go-eglot-check: ' <<<"$OUT"; then
  echo "FAIL: the evidence form did not run to completion"
  exit 1
fi

if grep -q '^assert: \[FAIL\]' <<<"$OUT"; then
  echo "FAIL: one or more Go/eglot checks failed"
  exit 1
fi

echo "PASS: Go opens under eglot and gopls, with lsp-mode uninvolved"
