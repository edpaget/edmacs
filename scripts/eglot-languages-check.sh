#!/usr/bin/env bash
#
# eglot-languages-check.sh -- batch evidence that Rust, TypeScript, JSX,
# JavaScript, JSON and Clojure buffers come up under eglot, with no
# third-party LSP client in sight.
#
# The sibling of `go-eglot-check.sh', same shape and the same traps (read its
# header first). It builds throwaway projects OUTSIDE this repo, loads the
# real config over them through `startup-check.sh', and asserts per language:
#
#   - the buffer is eglot-managed and the server process is the intended one
#   - the retired third-party client never attached (the standing
#     `no-lsp-mode' regression guard, the one place this script still has
#     to name that symbol)
#   - the translated workspace configuration reached the server
#   - flymake owns diagnostics and `edmacs-modeline-diagnostics' renders
#     their counts
#
# Plus the ordering this config depends on but cannot assume: `mise-mode' has
# already set a buffer-local `exec-path' before eglot computes the TypeScript
# contact, so the per-project `tsc' is the one probed.
#
# TWO TRAPS, on top of go-eglot-check.sh's two
#
# 1. TypeScript 7 answers PULL diagnostics only -- its initialize result
#    advertises `diagnosticProvider' and it never sends publishDiagnostics.
#    Nothing pulls in batch, so an omitted `(flymake-start t t)' looks
#    exactly like "TypeScript reports no problems".
# 2. clojure-lsp builds a whole-project analysis cache on first run in a
#    fresh project, which is slow -- the deadlines here are 90s for Clojure.
#
# USAGE
#   scripts/eglot-languages-check.sh [module-root] [package-root]
#
#   Arguments are passed straight through to startup-check.sh; the defaults
#   (this checkout's modules, the main checkout's package tree) are what you
#   want from a worktree.
#
#   Exits 0 when every check passes, 1 otherwise.

set -uo pipefail

REQUIRED_TOOLS=(cargo rust-analyzer tsc vscode-json-language-server clojure-lsp emacs)

missing_tool() {
  local tool
  for tool in "${REQUIRED_TOOLS[@]}"; do
    command -v "$tool" >/dev/null 2>&1 || { echo "$tool"; return 0; }
  done
  return 1
}

if missing_tool >/dev/null && [[ -z "${EGLOT_LANGS_CHECK_RELOGIN:-}" ]]; then
  export EGLOT_LANGS_CHECK_RELOGIN=1
  exec bash -lc "$(printf '%q ' "$0" "$@")"
fi

if tool="$(missing_tool)"; then
  echo "error: $tool is not on PATH (login shell PATH is what the daemon sees)" >&2
  exit 2
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SCRATCH="$(mktemp -d -t edmacs-eglot-langs)"
FORM="$SCRATCH/evidence.el"
trap 'rm -rf "$SCRATCH"' EXIT

# ---------------------------------------------------------------- Rust
# A real cargo package: rust-analyzer reports no workspace, and so no
# diagnostics, for a loose .rs file.
# Two members, not two modules of one crate: a type error anywhere in a crate
# stops rustc before clippy's late lint passes run, so the crate carrying the
# clippy lint has to compile.
RUST="$SCRATCH/rustproj"
mkdir -p "$RUST/scratch/src" "$RUST/broken/src"
(cd "$RUST" && git init -q .) || { echo "error: git init failed in $RUST" >&2; exit 2; }
cat > "$RUST/Cargo.toml" <<'TOML'
[workspace]
resolver = "2"
members = ["scratch", "broken"]
TOML

cat > "$RUST/scratch/Cargo.toml" <<'TOML'
[package]
name = "scratch"
version = "0.1.0"
edition = "2021"
TOML

cat > "$RUST/scratch/src/main.rs" <<'RS'
// `needless_return' is a clippy lint and not a rustc one, so it appears only
// when check.command actually reached rust-analyzer as "clippy".
fn shout(msg: &str) -> String {
    return format!("{msg}!");
}

fn main() {
    println!("{}", shout("world"));
}
RS

cat > "$RUST/broken/Cargo.toml" <<'TOML'
[package]
name = "broken"
version = "0.1.0"
edition = "2021"
TOML

cat > "$RUST/broken/src/lib.rs" <<'RS'
pub fn broken() -> i32 {
    "not an int"
}
RS

# ---------------------------------------------------------------- TS / JS / JSON
TS="$SCRATCH/tsproj"
mkdir -p "$TS/src"
(cd "$TS" && git init -q .) || { echo "error: git init failed in $TS" >&2; exit 2; }
cat > "$TS/tsconfig.json" <<'JSON'
{
  "compilerOptions": {
    "target": "es2022",
    "module": "esnext",
    "strict": true
  },
  "include": ["src"]
}
JSON

cat > "$TS/src/main.ts" <<'TS'
export function greet(name: string): string {
  return `hello ${name}`;
}

const wrong: number = "nope";

console.log(greet("world"), wrong);
TS

cat > "$TS/src/widget.tsx" <<'TSX'
export const Widget = () => <div>hello</div>;
TSX

cat > "$TS/src/plain.js" <<'JS'
export function add(a, b) {
  return a + b;
}
JS

cat > "$TS/package.json" <<'JSON'
{
  "name": "scratch",
  "version": "1.0.0",
  "private": true
}
JSON

# ---------------------------------------------------------------- Clojure
CLJ="$SCRATCH/cljproj"
mkdir -p "$CLJ/src/scratch"
(cd "$CLJ" && git init -q .) || { echo "error: git init failed in $CLJ" >&2; exit 2; }
echo '{:paths ["src"]}' > "$CLJ/deps.edn"
cat > "$CLJ/src/scratch/core.clj" <<'CLJ'
(ns scratch.core)

(defn greet [name]
  (str "hello " name))

;; `no-such-fn' is unresolved: clj-kondo, which clojure-lsp embeds, reports
;; it as an error.
(defn shout [name]
  (no-such-fn (greet name)))
CLJ

cat > "$FORM" <<'ELISP'
;;; eglot-languages-check evidence -*- lexical-binding: t -*-

(defvar edmacs-langs-rust (getenv "EGLOT_LANGS_RUST"))
(defvar edmacs-langs-ts (getenv "EGLOT_LANGS_TS"))
(defvar edmacs-langs-clj (getenv "EGLOT_LANGS_CLJ"))
(defvar edmacs-langs-failures 0)
(defvar edmacs-langs-total 0)

(defun edmacs-langs--report (ok name fmt &rest args)
  (setq edmacs-langs-total (1+ edmacs-langs-total))
  (unless ok (setq edmacs-langs-failures (1+ edmacs-langs-failures)))
  (princ (format "assert: [%s] %-28s %s\n" (if ok "PASS" "FAIL") name
                 (apply #'format fmt args))))

(defun edmacs-langs--wait (predicate seconds)
  "Poll PREDICATE until it returns non-nil or SECONDS elapse."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (sit-for 0.05))
    (funcall predicate)))

(defun edmacs-langs--connect (seconds)
  "Open the buffer's server the way a command loop would.
`eglot-ensure' only appends to `post-command-hook'; batch runs no command
loop, so without this every later check fails against a config that is
perfectly fine interactively."
  (run-hooks 'post-command-hook)
  (edmacs-langs--wait (lambda () (eglot-managed-p)) seconds))

(defun edmacs-langs--workspace-configuration (server)
  (save-window-excursion (eglot-show-workspace-configuration server))
  (let ((buffer (seq-find (lambda (b)
                            (string-match-p "workspace configuration" (buffer-name b)))
                          (buffer-list))))
    (prog1 (and buffer
                (with-current-buffer buffer
                  (replace-regexp-in-string
                   "[ \n]+" " " (buffer-substring-no-properties
                                 (point-min) (point-max)))))
      (when buffer (kill-buffer buffer)))))

(defun edmacs-langs--attach (label file mode server-rx &optional seconds entry)
  "Open FILE, connect, and assert the basic attachment facts.
ENTRY is the hook function expected to start the server, defaulting to
`eglot-ensure' -- rustic routes through its own `rustic-setup-lsp', which
dispatches on `rustic-lsp-client'. Return the server, or nil."
  (find-file file)
  (edmacs-langs--report (eq major-mode mode) (format "%s/major-mode" label)
                        "%S" major-mode)
  (let ((hook (symbol-value (intern (format "%s-hook" mode)))))
    (edmacs-langs--report (memq (or entry 'eglot-ensure) hook)
                          (format "%s/hook" label)
                          "%S" (or entry 'eglot-ensure)))
  (edmacs-langs--connect (or seconds 60))
  (let* ((server (eglot-current-server))
         (command (and server (process-command (jsonrpc--process server)))))
    (edmacs-langs--report (eglot-managed-p) (format "%s/managed" label)
                          "%S" (eglot-managed-p))
    (edmacs-langs--report (and command (string-match-p server-rx
                                                       (mapconcat #'identity command " ")))
                          (format "%s/server" label) "%S" command)
    (edmacs-langs--report (not (or (bound-and-true-p lsp-mode)
                                   (and (fboundp 'lsp-workspaces) (lsp-workspaces))))
                          (format "%s/no-lsp-mode" label) "lsp-mode=%S workspaces=%S"
                          (bound-and-true-p lsp-mode)
                          (and (fboundp 'lsp-workspaces) (lsp-workspaces)))
    server))

(defun edmacs-langs--definition-uri (location)
  "Return LOCATION's target file URI.
rust-analyzer answers with a LocationLink (`:targetUri\='), the TypeScript
server with a plain Location (`:uri\=')."
  (or (plist-get location :uri) (plist-get location :targetUri)))

(defun edmacs-langs--definition (label server search rx seconds)
  "Ask for the definition of the symbol SEARCH names, and assert RX matches."
  (goto-char (point-max))
  (search-backward search)
  (forward-char 1)
  (let ((locations (edmacs-langs--wait
                    (lambda ()
                      (ignore-errors
                        (let ((r (eglot--request server :textDocument/definition
                                                 (eglot--TextDocumentPositionParams))))
                          (and (not (eq r :null)) (> (length r) 0) r))))
                    seconds)))
    (edmacs-langs--report
     (and locations
          (string-match-p rx (or (edmacs-langs--definition-uri (elt locations 0)) "")))
     (format "%s/definition" label) "%S" locations)))

(defun edmacs-langs--diagnostics (label seconds &optional severity text-rx)
  "Force a flymake check and assert eglot supplied a diagnostic.
SEVERITY defaults to `eglot-error'; TEXT-RX, when given, must match the
diagnostic's text. Also asserts the modeline segment renders flymake's
counts.

`flymake-start' is not optional: TypeScript 7 advertises
`diagnosticProvider' and answers pull requests only, so nothing arrives
until something pulls -- and batch runs no command loop to do it."
  (let ((severity (or severity 'eglot-error))
        (prefix (if (eq severity 'eglot-warning) "W" "E")))
    (flymake-start t t)
    (let ((diagnostic
           (edmacs-langs--wait
            (lambda ()
              (seq-find (lambda (d)
                          (and (eq (flymake-diagnostic-type d) severity)
                               (or (null text-rx)
                                   (string-match-p text-rx (flymake-diagnostic-text d)))))
                        (flymake-diagnostics)))
            seconds)))
      (edmacs-langs--report diagnostic (format "%s/%s-diagnostic" label severity)
                            "%S" (mapcar #'flymake-diagnostic-text (flymake-diagnostics))))
    (edmacs-langs--report (and (bound-and-true-p flymake-mode)
                               (string-match-p (format "\\`%s[0-9]" prefix)
                                               (or (edmacs-modeline-diagnostics) "")))
                          (format "%s/modeline" label) "%S flymake=%S"
                          (substring-no-properties (or (edmacs-modeline-diagnostics) ""))
                          (bound-and-true-p flymake-mode))))


;;; --------------------------------------------------------------- Preflight

;; A missing grammar is not a config failure but it looks exactly like one:
;; `treesit-auto-install' is `prompt', and a y-or-n-p in batch reads EOF from
;; stdin, which aborts the major-mode body before its hooks ever run.
(dolist (lang '(rust typescript tsx javascript jsdoc json clojure))
  (edmacs-langs--report (treesit-language-available-p lang)
                        (format "grammar/%s" lang) "installed"))

;; `global-mise-mode' is wired to `after-init', and `--batch -l init.el'
;; processes `-l' after `after-init-hook' has already run, so it is off here.
;; Turn it on explicitly: the ordering assertion below is about mise-mode
;; versus eglot's contact, not about when the daemon enables it.
;; `mise-trust' must not stay `ask' first -- its `yes-or-no-p' blocks a batch
;; run forever on the throwaway projects below.
(setq mise-trust nil)
(when (fboundp 'edmacs--enable-mise) (edmacs--enable-mise))
(edmacs-langs--report (bound-and-true-p global-mise-mode) "mise/global-mode" "on")


;;; ------------------------------------------------------------------ Rust

(let ((server (edmacs-langs--attach "rust" (expand-file-name "scratch/src/main.rs" edmacs-langs-rust)
                                    'rustic-mode "rust-analyzer" 90
                                    'rustic-setup-lsp)))
  (edmacs-langs--report (eq rustic-lsp-client 'eglot) "rust/rustic-lsp-client"
                        "%S" rustic-lsp-client)
  (edmacs-langs--report (not (memq 'flymake-mode-off rustic-mode-hook))
                        "rust/flymake-mode-off" "removed from rustic-mode-hook")
  (when server
    (let ((configuration (edmacs-langs--workspace-configuration server)))
      (edmacs-langs--report
       (and configuration
            (string-match-p "\"rust-analyzer\"" configuration)
            (string-match-p "\"clippy\"" configuration)
            (string-match-p "\"skip_trivial\"" configuration)
            (string-match-p "\"closureReturnTypeHints\"" configuration)
            (string-match-p "\"always\"" configuration)
            (string-match-p "\"parameterHints\": { \"enable\": false }" configuration))
       "rust/workspace-config" "%s" configuration))

    (edmacs-langs--definition "rust" server "shout(" "main\\.rs" 90))
  ;; A clippy-only lint, reported as a warning: proof the check.command
  ;; setting took effect rather than being echoed back.
  (edmacs-langs--diagnostics "rust" 180 'eglot-warning "needless_return"))

;; The second buffer of an already-managed project: it gets flymake from
;; `after-change-major-mode-hook', whose first check batch never runs.
(find-file (expand-file-name "broken/src/lib.rs" edmacs-langs-rust))
(edmacs-langs--connect 90)
(edmacs-langs--diagnostics "rust-2nd" 180)


;;; ------------------------------------------------------ TypeScript / JSX / JS

;; mise must have set the buffer-local `exec-path' before eglot computes the
;; contact, or the probe reads the global tsc rather than the project's.
(find-file (expand-file-name "src/main.ts" edmacs-langs-ts))
(edmacs-langs--report (and (bound-and-true-p mise-mode) (local-variable-p 'exec-path))
                      "ts/mise-before-eglot" "mise-mode=%S exec-path-local=%S"
                      (bound-and-true-p mise-mode) (local-variable-p 'exec-path))
(edmacs-langs--report (eq (cdr (eglot--lookup-mode 'typescript-ts-mode))
                          'edmacs-js--typescript-server)
                      "ts/contact-is-ours" "%S"
                      (cdr (eglot--lookup-mode 'typescript-ts-mode)))
(kill-buffer)

(let ((server (edmacs-langs--attach "ts" (expand-file-name "src/main.ts" edmacs-langs-ts)
                                    'typescript-ts-mode "tsc .*--lsp" 90)))
  (when server
    (let ((configuration (edmacs-langs--workspace-configuration server)))
      (edmacs-langs--report
       (and configuration
            (string-match-p "\"importModuleSpecifier\": \"relative\"" configuration)
            (string-match-p "\"quoteStyle\": \"single\"" configuration))
       "ts/workspace-config" "%s" configuration))

    (edmacs-langs--definition "ts" server "greet(" "main\\.ts" 90))
  (edmacs-langs--diagnostics "ts" 90))

(edmacs-langs--attach "tsx" (expand-file-name "src/widget.tsx" edmacs-langs-ts)
                      'tsx-ts-mode "tsc .*--lsp" 90)
(edmacs-langs--attach "js" (expand-file-name "src/plain.js" edmacs-langs-ts)
                      'js-ts-mode "tsc .*--lsp" 90)
(edmacs-langs--attach "json" (expand-file-name "tsconfig.json" edmacs-langs-ts)
                      'json-ts-mode "json-language-server" 90)


;;; --------------------------------------------------------------- Clojure

;; clojure-lsp indexes the whole project on first run in a fresh checkout.
(let ((server (edmacs-langs--attach
               "clojure" (expand-file-name "src/scratch/core.clj" edmacs-langs-clj)
               'clojure-ts-mode "clojure-lsp" 120)))
  (when server
    (let ((configuration (edmacs-langs--workspace-configuration server)))
      ;; Deliberately empty: clojure-lsp reads .lsp/config.edn, and the
      ;; client this replaced carried no clojure settings to translate.
      ;; Asserted so the emptiness is evidence rather than an omission.
      (edmacs-langs--report (not (string-match-p "\"clojure\"" (or configuration "")))
                            "clojure/no-config-section" "%s" configuration)))
  (edmacs-langs--diagnostics "clojure" 180))


;;; ----------------------------------------------------------------- Sweep

;; flycheck is gone from the config; flymake is the only checker left, and
;; `prog-mode-hook' turns it on in the buffers no server manages.
(edmacs-langs--report (not (featurep 'flycheck)) "flycheck-absent" "%S"
                      (featurep 'flycheck))

(princ (format "assert: eglot-languages-check: %d checks, %d failed\n"
               edmacs-langs-total edmacs-langs-failures))
ELISP

export EGLOT_LANGS_RUST="$RUST" EGLOT_LANGS_TS="$TS" EGLOT_LANGS_CLJ="$CLJ"
export STARTUP_CHECK_EVAL="(load \"$FORM\" nil t)"

# stdin from /dev/null: any prompt this run did not anticipate then reads EOF
# and errors out, instead of blocking the check forever.
OUT="$("$SCRIPT_DIR/startup-check.sh" "$@" 2>&1 </dev/null)"
STATUS=$?
echo "$OUT"

if [[ $STATUS -ne 0 ]]; then
  echo "FAIL: startup-check.sh did not come up clean"
  exit 1
fi

if ! grep -q '^assert: eglot-languages-check: ' <<<"$OUT"; then
  echo "FAIL: the evidence form did not run to completion"
  exit 1
fi

if grep -q '^assert: \[FAIL\]' <<<"$OUT"; then
  echo "FAIL: one or more eglot language checks failed"
  exit 1
fi

echo "PASS: Rust, TypeScript, JSX, JavaScript, JSON and Clojure open under eglot"
