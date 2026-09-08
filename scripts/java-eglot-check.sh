#!/usr/bin/env bash
#
# java-eglot-check.sh -- batch evidence that a Java buffer comes up under
# eglot and jdtls, with no third-party LSP client in sight.
#
# The sibling of `go-eglot-check.sh' (read its header first for the shared
# traps). It builds a throwaway Maven project OUTSIDE this repo, loads the
# real config over it through `startup-check.sh', and asserts:
#
#   - the buffer is eglot-managed and the server process is jdtls
#   - the retired third-party client never attached (the standing
#     `no-lsp-mode' regression guard, the one place this script still has
#     to name that symbol)
#   - the `:java' workspace configuration (gradle/maven import, download
#     sources, autobuild) reached the server
#   - `textDocument/definition', `textDocument/references',
#     `textDocument/typeDefinition', `textDocument/implementation' and
#     `workspace/symbol' all answer with real, expected locations
#   - `eglot-rename' actually renames a local variable on disk (via a real
#     `textDocument/rename' round trip through `eglot--apply-workspace-edit',
#     not just a check that jdtls advertises `renameProvider'). The target is
#     a variable local to the buffer already visited, so the resulting edit
#     is "peaceful" and `eglot--apply-workspace-edit' applies it without a
#     y-or-n-p prompt to answer in batch.
#   - `eglot-code-actions', called on the real flymake diagnostic range from
#     Broken.java, returns a real non-empty list of server-proposed actions
#     -- not just that jdtls advertises `codeActionProvider'. The fixture is
#     scratch (mktemp -d, deleted on exit), so mutating it here is free.
#   - diagnostics appear via flymake and `edmacs-modeline-diagnostics'
#     renders their count
#
# TWO TRAPS, same family as go-eglot-check.sh's
#
# 1. `eglot-ensure' never connects in batch on its own -- `run-hooks
#    'post-command-hook'' is required before polling `eglot-managed-p'.
# 2. jdtls needs a real Maven or Gradle project (a loose .java file gets no
#    workspace and so no diagnostics); the fixture below is a pom.xml-rooted
#    project. jdtls resolves Maven natively (an embedded m2e), so no system
#    `mvn' on PATH is required for a project with no external dependencies.
#
# jdtls resolves through mise here, so the run needs a login shell PATH --
# the script re-execs itself through one when `jdtls' is missing, mirroring
# how `exec-path-from-shell' feeds PATH to the real daemon. jdtls is a cold
# JVM plus a project import on every run, so the deadlines here are longer
# than the sibling scripts': 150s to first attach.
#
# USAGE
#   scripts/java-eglot-check.sh [module-root] [package-root]
#
#   Arguments are passed straight through to startup-check.sh; the defaults
#   (this checkout's modules, the main checkout's package tree) are what you
#   want from a worktree.
#
#   Exits 0 when every check passes, 1 otherwise.

set -uo pipefail

if ! command -v jdtls >/dev/null 2>&1 && [[ -z "${JAVA_EGLOT_CHECK_RELOGIN:-}" ]]; then
  export JAVA_EGLOT_CHECK_RELOGIN=1
  exec bash -lc "$(printf '%q ' "$0" "$@")"
fi

for tool in jdtls java emacs; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "error: $tool is not on PATH (login shell PATH is what the daemon sees)" >&2
    exit 2
  fi
done

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SCRATCH="$(mktemp -d -t edmacs-java-eglot)"
FORM="$SCRATCH/evidence.el"
trap 'rm -rf "$SCRATCH"' EXIT

PROJECT="$SCRATCH/mavenproj"
PKG="$PROJECT/src/main/java/com/example"
mkdir -p "$PKG"

# `git init' so project.el roots the project at PROJECT. Without it the
# nearest VC root is whatever encloses TMPDIR, and jdtls gets the wrong view.
(cd "$PROJECT" && git init -q .) || {
  echo "error: could not create the scratch Maven project in $PROJECT" >&2
  exit 2
}

cat > "$PROJECT/pom.xml" <<'XML'
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example</groupId>
  <artifactId>scratch</artifactId>
  <version>1.0</version>
  <properties>
    <maven.compiler.source>21</maven.compiler.source>
    <maven.compiler.target>21</maven.compiler.target>
  </properties>
</project>
XML

cat > "$PKG/Greeting.java" <<'JAVA'
package com.example;

// The implementation/type-definition checks need an interface with exactly
// one implementer to land on.
public interface Greeting {
    String sayHello();
}
JAVA

cat > "$PKG/Greeter.java" <<'JAVA'
package com.example;

public class Greeter implements Greeting {
    public String sayHello() {
        return "hello";
    }
}
JAVA

cat > "$PKG/Main.java" <<'JAVA'
package com.example;

public class Main {
    public static void main(String[] args) {
        Greeting g = new Greeter();
        System.out.println(g.sayHello());
    }
}
JAVA

# Its own file, so the type error below cannot spoil Main.java's definition,
# typeDefinition, implementation and workspace/symbol results.
cat > "$PKG/Broken.java" <<'JAVA'
package com.example;

public class Broken {
    public int broken() {
        return "not an int";
    }
}
JAVA

cat > "$FORM" <<'ELISP'
;;; java-eglot-check evidence -*- lexical-binding: t -*-

(defvar edmacs-java-check-root (getenv "JAVA_EGLOT_CHECK_PROJECT"))
(defvar edmacs-java-check-failures 0)
(defvar edmacs-java-check-total 0)

(defun edmacs-java-check--report (ok name fmt &rest args)
  (setq edmacs-java-check-total (1+ edmacs-java-check-total))
  (unless ok (setq edmacs-java-check-failures (1+ edmacs-java-check-failures)))
  (princ (format "assert: [%s] %-24s %s\n" (if ok "PASS" "FAIL") name
                 (apply #'format fmt args))))

(defun edmacs-java-check--note (fmt &rest args)
  (princ (concat "assert:        " (apply #'format fmt args) "\n")))

(defun edmacs-java-check--wait (predicate seconds)
  "Poll PREDICATE until it returns non-nil or SECONDS elapse."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (sit-for 0.05))
    (funcall predicate)))

(defun edmacs-java-check--connect (seconds)
  "Open the buffer's server the way a command loop would.
`eglot-ensure' only appends to `post-command-hook'; batch runs no command
loop, so without this every later check fails against a config that is
perfectly fine interactively."
  (run-hooks 'post-command-hook)
  (edmacs-java-check--wait (lambda () (eglot-managed-p)) seconds))

(defun edmacs-java-check--workspace-configuration (server)
  (save-window-excursion (eglot-show-workspace-configuration server))
  (let ((buffer (seq-find (lambda (b)
                            (string-match-p "workspace configuration" (buffer-name b)))
                          (buffer-list))))
    (prog1 (and buffer
                (with-current-buffer buffer
                  (replace-regexp-in-string
                   "[ \n]+" " " (buffer-substring-no-properties (point-min) (point-max)))))
      (when buffer (kill-buffer buffer)))))

(defun edmacs-java-check--request (server method params seconds)
  "Poll METHOD until it returns a non-empty answer, or nil past SECONDS."
  (edmacs-java-check--wait
   (lambda ()
     (ignore-errors
       (let ((r (eglot--request server method params)))
         (and (not (eq r :null)) (> (length r) 0) r))))
   seconds))

(defun edmacs-java-check--uri-of (location)
  "LOCATION's target file URI -- jdtls answers with a plain Location."
  (or (plist-get location :uri) (plist-get location :targetUri)))

;;; ------------------------------------------------------------- Main.java

(find-file (expand-file-name "src/main/java/com/example/Main.java" edmacs-java-check-root))

(edmacs-java-check--report (eq major-mode 'java-ts-mode) "major-mode" "%S" major-mode)
(edmacs-java-check--report (memq 'eglot-ensure java-ts-mode-hook)
                           "java-ts-mode-hook" "eglot-ensure")

;; The trap, shown rather than described: managed is nil until the deferred
;; `post-command-hook' entry gets a chance to run.
(edmacs-java-check--note "before post-command-hook: managed=%S server=%S"
                         (eglot-managed-p) (eglot-current-server))
;; jdtls is a cold JVM plus a Maven project import on every run.
(edmacs-java-check--connect 150)

(let* ((server (eglot-current-server))
       (command (and server (process-command (jsonrpc--process server)))))
  (edmacs-java-check--report (eglot-managed-p) "eglot-managed" "%S" (eglot-managed-p))
  (edmacs-java-check--report (and command (string-match-p "jdtls" (mapconcat #'identity command " ")))
                             "server-is-jdtls" "%S" command)
  (edmacs-java-check--report (not (or (bound-and-true-p lsp-mode)
                                      (and (fboundp 'lsp-workspaces) (lsp-workspaces))))
                             "no-lsp-mode" "lsp-mode=%S lsp-workspaces=%S"
                             (bound-and-true-p lsp-mode)
                             (and (fboundp 'lsp-workspaces) (lsp-workspaces)))

  (when server
    (let ((configuration (edmacs-java-check--workspace-configuration server)))
      (edmacs-java-check--report
       (and configuration
            (string-match-p "\"java\"" configuration)
            (string-match-p "\"gradle\": { \"enabled\": true }" configuration)
            (string-match-p "\"maven\": { \"enabled\": true }" configuration)
            (string-match-p "\"downloadSources\": true" configuration)
            (string-match-p "\"autobuild\": { \"enabled\": true }" configuration))
       "workspace-config" "%s" configuration))

    (goto-char (point-min))
    (search-forward "new Greeter(")
    (backward-char 3)
    (let ((locations (edmacs-java-check--request
                      server :textDocument/definition
                      (eglot--TextDocumentPositionParams) 60)))
      (edmacs-java-check--report
       (and locations (string-match-p "Greeter\\.java"
                                      (edmacs-java-check--uri-of (elt locations 0))))
       "definition" "%S" locations))

    (goto-char (point-min))
    (search-forward "new Greeter(")
    (backward-char 3)
    (let ((refs (edmacs-java-check--request
                server :textDocument/references
                (append (eglot--TextDocumentPositionParams) '(:context (:includeDeclaration t)))
                60)))
      (edmacs-java-check--report (and refs (>= (length refs) 2)) "references" "%S" refs))

    (goto-char (point-min))
    (search-forward "g.sayHello")
    (backward-char 10)
    (let ((typedef (edmacs-java-check--request
                    server :textDocument/typeDefinition
                    (eglot--TextDocumentPositionParams) 60)))
      (edmacs-java-check--report
       (and typedef (string-match-p "Greeting\\.java" (edmacs-java-check--uri-of (elt typedef 0))))
       "type-definition" "%S" typedef))

    (let ((symbols (edmacs-java-check--request server :workspace/symbol '(:query "Greeter") 60)))
      (edmacs-java-check--report
       (seq-some (lambda (s) (equal (plist-get s :name) "Greeter")) (or symbols []))
       "workspace-symbol" "%S" symbols))

    ;; Real rename, not just a capability check. "g" is local to this
    ;; buffer's main() method, so the resulting WorkspaceEdit touches only
    ;; the buffer already visited here -- `eglot--apply-workspace-edit'
    ;; treats that as "peaceful" and applies it with no y-or-n-p prompt.
    ;; A single call, not a poll-and-retry: this one *applies* the edit, and
    ;; every earlier check already proved the project is fully indexed.
    (goto-char (point-min))
    (search-forward "Greeting g")
    (backward-char 1)
    (let ((result (eglot--apply-workspace-edit
                   server
                   (eglot--request
                    server :textDocument/rename
                    (append (eglot--TextDocumentPositionParams) '(:newName "greeting")))
                   'edmacs-java-check--rename)))
      (edmacs-java-check--report
       (and (car result)
            (save-excursion (goto-char (point-min))
                            (search-forward "greeting = new Greeter" nil t))
            (save-excursion (goto-char (point-min))
                            (search-forward "greeting.sayHello" nil t)))
       "rename-applied" "%S buffer-now=%S" result
       (buffer-substring-no-properties (point-min) (point-max))))

    (let ((capabilities (eglot--capabilities server)))
      (edmacs-java-check--report (plist-get capabilities :implementationProvider)
                                 "implementation-capability" "%S"
                                 (plist-get capabilities :implementationProvider)))))

;;; --------------------------------------------------------- Greeting.java
;; A second buffer of an already-managed project, and the source of the
;; implementation request: Greeter is Greeting's one implementer.

(find-file (expand-file-name "src/main/java/com/example/Greeting.java" edmacs-java-check-root))
(edmacs-java-check--connect 60)
(let ((server (eglot-current-server)))
  (goto-char (point-min))
  (search-forward "sayHello")
  (backward-char 4)
  (let ((impls (edmacs-java-check--request
               server :textDocument/implementation
               (eglot--TextDocumentPositionParams) 60)))
    (edmacs-java-check--report
     (and impls (string-match-p "Greeter\\.java" (edmacs-java-check--uri-of (elt impls 0))))
     "implementation" "%S" impls)))

;;; ----------------------------------------------------------- Broken.java

(find-file (expand-file-name "src/main/java/com/example/Broken.java" edmacs-java-check-root))
(edmacs-java-check--connect 60)
(flymake-start t t)
(let ((diagnostic (edmacs-java-check--wait
                   (lambda ()
                     (seq-find (lambda (d) (eq (flymake-diagnostic-type d) 'eglot-error))
                               (flymake-diagnostics)))
                   60)))
  (edmacs-java-check--report diagnostic "error-diagnostic" "%S"
                             (and diagnostic (flymake-diagnostic-text diagnostic)))
  ;; Real code actions, not just a capability check: request them for the
  ;; diagnostic's own range and confirm jdtls actually proposes fixes.
  (when diagnostic
    (let ((actions (edmacs-java-check--wait
                    (lambda ()
                      (ignore-errors
                        (let ((a (eglot-code-actions (flymake-diagnostic-beg diagnostic)
                                                     (flymake-diagnostic-end diagnostic)
                                                     nil nil)))
                          (and a (> (length a) 0) a))))
                    60)))
      (edmacs-java-check--report
       (and actions (> (length actions) 0)
            (seq-every-p (lambda (a) (plist-get a :title)) actions))
       "code-action-invoked" "%S" (mapcar (lambda (a) (plist-get a :title)) actions)))))
(edmacs-java-check--report (and (bound-and-true-p flymake-mode)
                                (string-match-p "\\`E[0-9]" (edmacs-modeline-diagnostics)))
                           "modeline-from-flymake" "%S flymake=%S"
                           (substring-no-properties (edmacs-modeline-diagnostics))
                           (bound-and-true-p flymake-mode))

(princ (format "assert: java-eglot-check: %d checks, %d failed\n"
               edmacs-java-check-total edmacs-java-check-failures))
ELISP

export JAVA_EGLOT_CHECK_PROJECT="$PROJECT"
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

if ! grep -q '^assert: java-eglot-check: ' <<<"$OUT"; then
  echo "FAIL: the evidence form did not run to completion"
  exit 1
fi

if grep -q '^assert: \[FAIL\]' <<<"$OUT"; then
  echo "FAIL: one or more Java/eglot checks failed"
  exit 1
fi

echo "PASS: Java opens under eglot and jdtls"
