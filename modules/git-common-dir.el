;;; git-common-dir.el --- Shared, TRAMP-safe git-common-dir resolution -*- lexical-binding: t -*-

;;; Commentary:
;; A single `git rev-parse --git-common-dir' resolver, with
;; cached-miss memoization, factored out so it has exactly one
;; implementation instead of two independently-maintained copies.
;;
;; Both `modules/sessions.el' (tab naming: disambiguating two
;; same-basename worktrees of different repos) and
;; `modules/claude-term-registry.el' (picker/list-mode repo-name
;; labeling) need "what repository owns this worktree root", derived
;; from git-common-dir's parent directory -- and both need the same
;; TRAMP-safety and memoization this function provides. Landing it here
;; once means a future fix (a TRAMP quoting edge case, a git version
;; whose --git-common-dir output needs different handling) only has to
;; be applied in one place.
;;
;; Loaded early by init.el's `load-module', before both consumers --
;; this codebase's modules share one obarray via plain sequential
;; `load' (see init.el's `load-module'), so no `require' is needed on
;; the consumer side; `provide' below only mirrors this repo's existing
;; end-of-file convention, in case a future module wants to `require'
;; this one explicitly.

;;; Code:

(require 'subr-x)

(defvar edmacs-git-common-dir-cache (make-hash-table :test #'equal)
  "Memoized ROOT -> git-common-dir results from `edmacs-git-common-dir'.
A worktree's git-common-dir cannot change during the life of a running
Emacs, so entries are never invalidated. A miss is cached too (as the
symbol `none', since a plain nil can't be told apart from \"not yet
looked up\" in `gethash''s single optional-default arg) so a root git
can't identify doesn't get re-shelled-out-to on every subsequent
caller.")

(defun edmacs-git-common-dir-1 (root)
  "Uncached implementation of `edmacs-git-common-dir' for ROOT.
Every worktree of one repository shares this path (it is the main
checkout's `.git', per git-worktree(1)), so its parent directory names
the repository independent of any individual worktree's own directory
name.

Uses `process-file', not `call-process': ROOT may be a TRAMP remote
directory, and `call-process' is documented to run in `default-directory'
only when that is local, silently falling back to running the command
in `~' otherwise -- exactly the directory-confusion bug this function
exists to avoid, just relocated to a remote-vs-local split instead of a
stale-`default-directory' one. `process-file' dispatches through TRAMP
for a remote `default-directory' and runs locally otherwise.

For a linked worktree, git prints an already-absolute path here; for
the *main* worktree, though, it prints a path relative to the
directory git was invoked from (typically \".git\"). That expansion to
an absolute path has to happen right here, while `default-directory'
is still bound to ROOT -- a caller expanding the returned string later
against its own, unrelated `default-directory' would silently resolve
it against the wrong base and misidentify the repository.

When ROOT is remote, git itself only ever prints a bare on-host path
(git has no notion of TRAMP), so an already-\"absolute\" answer like
\"/home/user/repo/.git\" still needs ROOT's own TRAMP method/host
prefix grafted back on by hand -- `expand-file-name' leaves an
already-absolute NAME untouched and would otherwise silently drop the
remote host, resolving to a same-named but purely local path.

`process-file' can *signal* rather than merely exit non-zero -- e.g.
`file-missing' when git itself isn't found, or when ROOT no longer
exists on disk (a pruned worktree) or a TRAMP connection to it has
dropped. The `condition-case' below folds that into the same nil
result as an ordinary non-zero exit, so the caller's memoization-of-nil
in `edmacs-git-common-dir' covers this case too instead of
re-shelling-out (and re-signaling) on every subsequent call."
  (let ((default-directory root))
    (condition-case nil
        (with-temp-buffer
          (when (zerop (process-file "git" nil t nil "rev-parse" "--git-common-dir"))
            (let ((raw (string-trim (buffer-string)))
                  (remote (file-remote-p root)))
              (cond
               ((not (file-name-absolute-p raw)) (expand-file-name raw root))
               ((and remote (not (file-remote-p raw))) (concat remote raw))
               (t raw)))))
      (file-error nil))))

(defun edmacs-git-common-dir (root)
  "Return the absolute git common directory for the worktree at ROOT, or nil.
Memoized per ROOT; see `edmacs-git-common-dir-cache' and
`edmacs-git-common-dir-1' for why and how."
  (let ((cached (gethash root edmacs-git-common-dir-cache 'edmacs-git-common-dir--miss)))
    (if (not (eq cached 'edmacs-git-common-dir--miss))
        (and (not (eq cached 'none)) cached)
      (let ((result (edmacs-git-common-dir-1 root)))
        (puthash root (or result 'none) edmacs-git-common-dir-cache)
        result))))

(provide 'git-common-dir)
;;; git-common-dir.el ends here
