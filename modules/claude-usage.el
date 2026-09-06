;;; claude-usage.el --- Parse and normalize Claude CLI usage cache JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edward Paget
;; Author: Edward Paget <ed.paget@gmail.com>
;; Keywords: claude usage metrics

;; This file is part of edmacs.

;;; Commentary:

;; Pure Emacs Lisp normalization layer for parsing Claude CLI usage metrics.
;; Transforms a `cachedUsageUtilization'-shaped envelope (fetchedAtMs,
;; accountUuid, utilization) into an ordered list of meter plists
;; (:id :label :percent :severity :resets-at :model), along with helper
;; functions for formatting and staleness checks.
;;
;; Two sources feed that envelope, resolved in one place
;; (`claude-usage--apply-refresh-result') with a strict precedence:
;;
;; 1. `claude-usage--fetch' -- an async GET of the CLI's own
;;    https://api.anthropic.com/api/oauth/usage endpoint, bearer-authed
;;    from a token read out of the login keychain
;;    (`claude-usage--access-token'). This is the primary source: the CLI
;;    itself only trusts its on-disk cache for one hour.
;; 2. `claude-usage--read-cache' -- ~/.claude.json's `cachedUsageUtilization',
;;    used only when the live fetch fails (no token, network error, non-200,
;;    unparseable body).
;;
;; The keychain read and the network call both happen only inside the
;; deferred refresh path (`claude-usage--refresh', single-flight guarded,
;; triggered from a 0-second idle timer after buffer open, from `g', and
;; from a periodic idle timer) -- never from the synchronous buffer-open
;; path, so opening `*claude-usage*' never blocks on an unlock prompt or
;; the network. The access token itself never reaches a buffer, a message,
;; or an error payload.
;;
;; Two cheap surfaces compress that state for glancing at:
;; `claude-usage-mode-line-mode' (a nano-modeline footer segment, spliced
;; in by `:filter-args' advice on `nano-modeline-footer') and a sidebar
;; section contributed through `edmacs-sidebar-bottom-anchor-section-functions'
;; (owned by sidebar.el; registered here, never touching that file), which
;; also pins it to the sidebar window's bottom edge. Both read only
;; pre-rendered values off `claude-usage--state-envelope', so neither reads
;; a file, stats a directory, or starts a process.

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

;; `modules/git.el's `use-package magit :commands (...)' only activates
;; magit's autoloads file, which does not autoload `magit-section-mode',
;; `magit-insert-section', or `magit-insert-heading' (see sidebar.el's
;; identical require and its longer comment for the verification). This
;; require resolves cleanly at real init.el load time because straight
;; already puts a built package's directory (and its transitive deps:
;; compat, cond-let, llama, transient, seq) on `load-path' at
;; build/registration time. Under `-Q --batch', claude-usage-test.el fixes
;; `load-path' against the straight build tree before loading this file.
(require 'magit-section)

;; `evil' loads only in a real init.el session; declared here so the
;; byte-compiler doesn't warn about the forward references inside
;; `claude-usage-mode' and the `with-eval-after-load' block below --
;; sidebar.el omits this pair and a load-path-fixed compile of it emits
;; "the function `evil-define-key' is not known to be defined" as a
;; result; this file does not repeat that gap.
(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil-core")

;; Optional forward references, resolved only in a real init.el session:
;; sidebar.el's redraw (called through `fboundp'), and the two mode-line
;; constructors the load-time re-bake picks between.
(declare-function edmacs-sidebar--redraw "sidebar" (frame))
(declare-function edmacs-modeline-text-mode "ui" (&optional default))
(declare-function nano-modeline-text-mode "nano-modeline" (&optional default))

;; sidebar.el owns these hooks; declared here only so the byte-compiler
;; doesn't warn about `add-hook' on a free variable when this file is
;; compiled standalone. `add-hook' below creates each if sidebar.el
;; hasn't loaded yet -- the dependency stays one-way.
(defvar edmacs-sidebar-bottom-anchor-section-functions)
(defvar edmacs-sidebar-collapsed-bottom-anchor-section-functions)

(defgroup claude-usage nil
  "Claude usage metrics and cache integration."
  :group 'claude
  :prefix "claude-usage-")

;; ============================================================================
;; Faces
;; ============================================================================
;; Hardcoded Solarized Dark hex values rather than named theme faces: the
;; `semantic-face-vocabulary' task has not landed anywhere in this codebase
;; (confirmed by grep) -- once it does, these should `:inherit' its
;; severity ladder instead of naming colors directly.

(defface claude-usage-ok
  '((t :foreground "#859900"))
  "Face for a usage meter below the warning threshold."
  :group 'claude-usage)

(defface claude-usage-warn
  '((t :foreground "#b58900"))
  "Face for a usage meter at or above the warning threshold."
  :group 'claude-usage)

(defface claude-usage-crit
  '((t :foreground "#dc322f"))
  "Face for a usage meter at or above the critical threshold."
  :group 'claude-usage)

(defface claude-usage-stale
  '((t :foreground "#586e75"))
  "Face for a usage reading whose cache is stale."
  :group 'claude-usage)

(defcustom claude-usage-cache-file "~/.claude.json"
  "Path to the Claude CLI's config file containing usage cache.
Expanded via `expand-file-name`."
  :type 'string
  :group 'claude-usage)

(defcustom claude-usage-stale-threshold 3600
  "Seconds until cached usage data is considered stale.
Default: 3600 (one hour)."
  :type 'integer
  :group 'claude-usage)

(defcustom claude-usage-keychain-service "Claude Code-credentials"
  "macOS login-keychain service name holding the Claude CLI's OAuth token.

The CLI computes this name itself: it is exactly \"Claude Code-credentials\"
only when neither CLAUDE_SECURESTORAGE_CONFIG_DIR nor CLAUDE_CONFIG_DIR is
set in its environment; otherwise it appends \"-<first 8 hex chars of
sha256(config dir)>\". Set this to match if either variable is set for
your `claude' invocations -- a wrong service name is indistinguishable
from \"no token\" and silently falls back to the on-disk cache."
  :type 'string
  :group 'claude-usage)

(defcustom claude-usage-idle-refresh-delay 300
  "Seconds of Emacs idle time between automatic usage refreshes.
Default: 300 (five minutes), matching the CLI's own write throttle."
  :type 'integer
  :group 'claude-usage)

(defun claude-usage--parse-iso8601 (iso-string)
  "Parse an ISO 8601 timestamp string to Emacs time value.

ISO-STRING should be in format like \"2026-09-03T20:00:00Z\".

Returns a time value suitable for use with `float-time' and
`format-time-string', or nil if parsing fails."
  (when (string-match
         "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
         iso-string)
    (let ((year (string-to-number (match-string 1 iso-string)))
          (month (string-to-number (match-string 2 iso-string)))
          (day (string-to-number (match-string 3 iso-string)))
          (hour (string-to-number (match-string 4 iso-string)))
          (min (string-to-number (match-string 5 iso-string)))
          (sec (string-to-number (match-string 6 iso-string))))
      (condition-case nil
          (encode-time sec min hour day month year t)
        (error nil)))))

(defun claude-usage--read-cache ()
  "Read the Claude CLI's usage cache from `claude-usage-cache-file'.

Returns the parsed `cachedUsageUtilization' object as an alist,
containing fetchedAtMs, accountUuid, and utilization fields.
Returns nil if the file is missing, unreadable, or malformed.
Never signals an error."
  (let* ((cache-file (expand-file-name claude-usage-cache-file))
         (inhibit-message t))
    (if (not (file-readable-p cache-file))
        nil
      (with-temp-buffer
        (condition-case nil
            (progn
              (insert-file-contents cache-file)
              (goto-char (point-min))
              (let ((parsed (json-parse-buffer :object-type 'alist
                                               :array-type 'list
                                               :null-object nil)))
                (alist-get 'cachedUsageUtilization parsed)))
          (error nil))))))

;; ============================================================================
;; Keychain token
;; ============================================================================

(defun claude-usage--extract-access-token (raw)
  "Parse RAW, a keychain password blob, and return its access token.

RAW is expected to be the JSON credentials blob the CLI itself writes:
`{\"claudeAiOauth\": {\"accessToken\": \"...\", ...}}'. Returns the token
string, or nil for nil/empty/non-JSON/wrong-shaped input. Never signals."
  (when (and (stringp raw) (not (string-empty-p (string-trim raw))))
    (condition-case nil
        (let* ((parsed (json-parse-string raw :object-type 'alist
                                           :array-type 'list
                                           :null-object nil))
               (oauth (and (listp parsed) (alist-get 'claudeAiOauth parsed)))
               (token (and (listp oauth) (alist-get 'accessToken oauth))))
          (and (stringp token) token))
      (error nil))))

(defun claude-usage--read-keychain-secret (service)
  "Return the raw stdout of the macOS keychain lookup for SERVICE, or nil.
Runs `security find-generic-password -s SERVICE -w', discarding stderr.
Never signals -- a missing `security' binary, a missing entry, or any
other failure all resolve to nil."
  (condition-case nil
      (with-temp-buffer
        (let ((exit-code (call-process "security" nil (list t nil) nil
                                        "find-generic-password" "-s" service "-w")))
          (and (eql exit-code 0) (buffer-string))))
    (error nil)))

(defun claude-usage--access-token ()
  "Return the Claude CLI's OAuth access token from the login keychain.

Reads the `claude-usage-keychain-service' entry via `security
find-generic-password'. Returns nil on any failure and never signals.

The returned token (like the raw keychain blob it is parsed from) must
never reach a buffer, the echo area, `message', a log, or an error
payload. Call this only from inside the deferred refresh path -- never
from a display entry point -- a locked keychain can block on an
unlock/ACL prompt."
  (claude-usage--extract-access-token
   (claude-usage--read-keychain-secret claude-usage-keychain-service)))

(defun claude-usage--severity-face (severity percent)
  "Map usage severity string or percent threshold to a face name.

SEVERITY is a string: \"normal\", \"warning\", or \"critical\".
PERCENT is a numeric utilization percentage.

Returns a face name (symbol) for rendering. Falls back to thresholds:
- >= 90%: `claude-usage-crit' face
- >= 70%: `claude-usage-warn' face
- < 70%: `claude-usage-ok' face

If SEVERITY is provided and not nil, uses it directly."
  (cond
   ((string-equal severity "critical") 'claude-usage-crit)
   ((string-equal severity "warning") 'claude-usage-warn)
   ((string-equal severity "normal") 'claude-usage-ok)
   ;; Fallback to percent-based thresholds
   ((>= percent 90) 'claude-usage-crit)
   ((>= percent 70) 'claude-usage-warn)
   (t 'claude-usage-ok)))

(defun claude-usage--format-reset (resets-at-iso &optional current-time-for-test)
  "Format an ISO 8601 timestamp as local time plus relative duration.

RESETS-AT-ISO is an ISO 8601 string like \"2026-09-02T18:30:00Z\".
CURRENT-TIME-FOR-TEST is an optional time value for testing; if omitted,
uses `current-time'.

Returns a string like \"3:30 PM (in 2h 15m)\" showing local wall time
and remaining duration until reset."
  (condition-case nil
      (let* ((resets-at (claude-usage--parse-iso8601 resets-at-iso))
             (current (or current-time-for-test (current-time)))
             (secs-until (when resets-at
                           (truncate (float-time (time-subtract resets-at current))))))
        (if (or (null resets-at) (<= secs-until 0))
            (if resets-at
                (format-time-string "%-I:%M %p (now)" resets-at (getenv "TZ"))
              "")
          (let* ((hours (/ secs-until 3600))
                 (mins (/ (mod secs-until 3600) 60))
                 (time-str (format-time-string "%-I:%M %p" resets-at (getenv "TZ"))))
            (if (> hours 0)
                (format "%s (in %dh %dm)" time-str hours mins)
              (format "%s (in %dm)" time-str mins)))))
    (error "")))

(defun claude-usage--format-age (fetched-at-ms &optional current-time-for-test)
  "Format milliseconds since fetch as a human-readable age string.

FETCHED-AT-MS is a numeric timestamp in milliseconds (as from the CLI's
fetchedAtMs field).
CURRENT-TIME-FOR-TEST is an optional time value for testing; if omitted,
uses `current-time'.

Returns a string like \"2m ago\", \"1h 15m ago\", or \"3d ago\"."
  (condition-case nil
      (let* ((fetched-secs (/ fetched-at-ms 1000.0))
             (current-secs (float-time (or current-time-for-test (current-time))))
             (age-secs (truncate (- current-secs fetched-secs))))
        (cond
         ((< age-secs 60) "now")
         ((< age-secs 3600)
          (format "%dm ago" (/ age-secs 60)))
         ((< age-secs 86400)
          (let ((hours (/ age-secs 3600))
                (mins (/ (mod age-secs 3600) 60)))
            (if (> mins 0)
                (format "%dh %dm ago" hours mins)
              (format "%dh ago" hours))))
         (t
          (format "%dd ago" (/ age-secs 86400)))))
    (error "")))

(defun claude-usage--bar (percent width)
  "Generate a bar visualization using Unicode block characters.

PERCENT is a numeric percentage (0-100+).
WIDTH is the desired character width of the bar (typically 10-20).

Returns a string of block characters, with one character representing
roughly (100 / WIDTH)% of capacity."
  (let* ((filled (min width (truncate (/ (* percent width) 100))))
         (empty (- width filled)))
    (concat
     (make-string filled ?█)
     (make-string empty ?░))))

(defun claude-usage-stale-p (fetched-at-ms)
  "Check whether cached usage data is considered stale.

FETCHED-AT-MS is a numeric timestamp in milliseconds.

Returns t if the data is older than `claude-usage-stale-threshold',
nil otherwise."
  (condition-case nil
      (let* ((fetched-secs (/ fetched-at-ms 1000.0))
             (current-secs (float-time (current-time)))
             (age-secs (- current-secs fetched-secs)))
        (> age-secs claude-usage-stale-threshold))
    (error nil)))

(defun claude-usage-meters (cached-util)
  "Transform cachedUsageUtilization into meter plists.

CACHED-UTIL is the full `cachedUsageUtilization' object from
`claude-usage--read-cache', with fetchedAtMs, accountUuid, and
utilization fields.

Returns a list of plists, one per limit entry.
Each plist contains: :id KIND :label LABEL :percent PERCENT
:severity SEVERITY :resets-at ISO8601 :model MODEL

If `limits' array is present, extracts meters from it; otherwise
falls back to legacy `five_hour' and `seven_day' entries.

Maps limit `kind' to a human-readable label:
  - \"session\" -> \"Session (5h)\"
  - \"weekly_all\" -> \"Week (all)\"
  - \"weekly_scoped\" -> \"Week (<model>)\"

For limits[], reads `percent' (already 0-100) directly.
For fallback, reads `utilization' (already 0-100). Preserves
`severity' string, and extracts model name from
`scope.model.display_name' when present."
  (let* ((meters '())
         (utilization (alist-get 'utilization cached-util))
         (limits (alist-get 'limits utilization)))

    ;; Primary path: use limits array if present
    (when limits
      (dolist (limit limits)
        (let* ((kind (alist-get 'kind limit))
               (percent (alist-get 'percent limit))
               (severity (alist-get 'severity limit))
               (resets-at (alist-get 'resets_at limit))
               (model-info (alist-get 'scope limit))
               (model-name (when model-info
                             (alist-get 'display_name
                                       (alist-get 'model model-info))))
               (kind-sym (when kind (intern kind)))
               label)

          ;; Skip null percent entries
          (when percent
            ;; Derive label from kind
            (setq label
                  (cond
                   ((string-equal kind "session") "Session (5h)")
                   ((string-equal kind "weekly_all") "Week (all)")
                   ((string-equal kind "weekly_scoped")
                    (if model-name
                        (format "Week (%s)" model-name)
                      "Week (unknown)"))
                   (t kind)))

            ;; Build meter plist
            (push (list :id kind-sym
                        :label label
                        :percent percent
                        :severity severity
                        :resets-at resets-at
                        :model model-name)
                  meters)))))

    ;; Fallback path: use legacy five_hour and seven_day if limits absent
    (unless limits
      (let ((five-hour (alist-get 'five_hour utilization))
            (seven-day (alist-get 'seven_day utilization)))

        (when five-hour
          (let* ((percent (alist-get 'utilization five-hour))
                 (severity (alist-get 'severity five-hour))
                 (resets-at (alist-get 'resets_at five-hour)))
            (when percent
              (push (list :id 'session
                          :label "Session (5h)"
                          :percent percent
                          :severity severity
                          :resets-at resets-at
                          :model nil)
                    meters))))

        (when seven-day
          (let* ((percent (alist-get 'utilization seven-day))
                 (severity (alist-get 'severity seven-day))
                 (resets-at (alist-get 'resets_at seven-day)))
            (when percent
              (push (list :id 'weekly_all
                          :label "Week (all)"
                          :percent percent
                          :severity severity
                          :resets-at resets-at
                          :model nil)
                    meters))))))

    ;; Return in reverse order (since we pushed)
    (nreverse meters)))

;; ============================================================================
;; Live fetch
;; ============================================================================

(defconst claude-usage--endpoint-url "https://api.anthropic.com/api/oauth/usage"
  "The Claude CLI's own live usage-utilization endpoint.
Returns the bare utilization object -- `claude-usage--read-cache's
`cachedUsageUtilization' is that same object wrapped with fetchedAtMs
and accountUuid, so `claude-usage--fetch' synthesizes an identical
envelope around it.")

(defun claude-usage--parse-fetch-buffer (buf)
  "Parse BUF, a raw HTTP response, into a utilization alist or nil.

BUF holds a full HTTP/1.x response: status line, headers, a blank line,
then the JSON body -- exactly what `url-retrieve' hands its callback, and
also the shape of a fixture built from a captured real response. Parses
the body with `json-parse-buffer' using alist objects, list arrays, and a
nil null-object -- load-bearing: any other option set makes a hash table
and `claude-usage-meters' then signals `wrong-type-argument listp'.
Returns that parsed body only when the status line reports 200; nil on
any other status or parse failure. Never signals."
  (condition-case nil
      (with-current-buffer buf
        (goto-char (point-min))
        (when (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
          (let ((status (string-to-number (match-string 1))))
            (when (and (= status 200)
                       (re-search-forward "\r?\n\r?\n" nil t))
              (json-parse-buffer :object-type 'alist
                                  :array-type 'list
                                  :null-object nil)))))
    (error nil)))

(defun claude-usage--fetch (callback)
  "Asynchronously GET the live usage endpoint; call CALLBACK with the result.

CALLBACK is invoked with a synthesized envelope alist (fetchedAtMs,
accountUuid, utilization) on success, or nil on any failure: no token,
a network error, a non-200 status, or an unparseable body. Never blocks
the caller and never signals into it -- CALLBACK always runs, exactly
once, with nil standing in for every failure mode."
  (let ((token (claude-usage--access-token)))
    (if (null token)
        (funcall callback nil)
      (let ((url-request-method "GET")
            (url-request-extra-headers
             (list (cons "Authorization" (concat "Bearer " token)))))
        (condition-case nil
            (url-retrieve
             claude-usage--endpoint-url
             (lambda (status)
               (let ((buf (current-buffer))
                     (fetch-time-ms (* 1000.0 (float-time (current-time)))))
                 (unwind-protect
                     (let ((body (unless (plist-get status :error)
                                   (claude-usage--parse-fetch-buffer buf))))
                       (funcall callback
                                (and body
                                     (list (cons 'fetchedAtMs fetch-time-ms)
                                           (cons 'accountUuid nil)
                                           (cons 'utilization body)))))
                   (when (buffer-live-p buf) (kill-buffer buf)))))
             nil t t)
          (error (funcall callback nil)))))))

;; ============================================================================
;; Source precedence and refresh
;; ============================================================================

(defvar claude-usage--state-envelope nil
  "The most recently resolved usage envelope, or nil.
Set only by `claude-usage--apply-refresh-result'; every display surface
reads this rather than re-resolving the fetch/cache precedence itself.")

(defvar claude-usage--state-source nil
  "The source of `claude-usage--state-envelope': `live', `cache', or nil.")

(defvar claude-usage--refresh-in-flight nil
  "Non-nil while a `claude-usage--refresh' fetch is outstanding.
Guards against overlapping refreshes from the open-buffer timer, `g',
and the periodic idle timer all firing close together.")

(defvar claude-usage--idle-timer nil
  "The periodic idle timer driving automatic refreshes, or nil.
Recreated (cancel-then-recreate) by `claude-usage--setup-idle-timer' on
every load of this file, so reloading never leaves a duplicate running.")

(defun claude-usage--apply-refresh-result (fetch-envelope)
  "Resolve source precedence from FETCH-ENVELOPE and update module state.

FETCH-ENVELOPE is the result of a `claude-usage--fetch' callback: a
synthesized envelope on success, nil on any failure. A non-nil
FETCH-ENVELOPE always wins; on nil, `claude-usage--read-cache' is
consulted; when both are nil, state resolves to nil/nil, and the
existing \"No usage data\" render branch applies.

Returns (ENVELOPE . SOURCE) after setting `claude-usage--state-envelope'
and `claude-usage--state-source' to the same values, and refreshing the
two cheap surfaces (`claude-usage--update-surfaces') -- the single funnel
every state change already reaches, so neither surface needs a second
call site."
  (let* ((envelope (or fetch-envelope (claude-usage--read-cache)))
         (source (cond (fetch-envelope 'live)
                       (envelope 'cache)
                       (t nil))))
    (setq claude-usage--state-envelope envelope
          claude-usage--state-source source)
    (claude-usage--update-surfaces)
    (cons envelope source)))

(defun claude-usage--sync-render ()
  "Redraw the current buffer synchronously, touching neither keychain nor network.
Reuses already-resolved state when present; otherwise reads the on-disk
cache fresh. Called from the buffer-open path, which must never block."
  (if claude-usage--state-envelope
      (claude-usage--redraw claude-usage--state-envelope nil claude-usage--state-source)
    (let ((cached (claude-usage--read-cache)))
      (claude-usage--redraw cached nil (and cached 'cache)))))

(defun claude-usage--refresh ()
  "Refresh usage state via a single-flight guarded async fetch.
No-ops immediately if a refresh is already in flight. On completion,
resolves source precedence (`claude-usage--apply-refresh-result') and
redraws the `*claude-usage*' buffer if it is still live."
  (unless claude-usage--refresh-in-flight
    (setq claude-usage--refresh-in-flight t)
    (claude-usage--fetch
     (lambda (fetch-envelope)
       (unwind-protect
           (progn
             (claude-usage--apply-refresh-result fetch-envelope)
             (let ((buf (get-buffer "*claude-usage*")))
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (claude-usage--sync-render)))))
         (setq claude-usage--refresh-in-flight nil))))))

(defun claude-usage--setup-idle-timer ()
  "(Re)create the periodic idle-refresh timer.
Cancels any existing `claude-usage--idle-timer' first, so reloading this
file (e.g. during interactive development) never leaves a second timer
running alongside the first."
  (when (timerp claude-usage--idle-timer)
    (cancel-timer claude-usage--idle-timer))
  (setq claude-usage--idle-timer
        (run-with-idle-timer claude-usage-idle-refresh-delay t
                              #'claude-usage--refresh)))

;; ============================================================================
;; Major mode
;; ============================================================================

(defconst claude-usage--em-dash "—"
  "Placeholder for a meter field whose source value is missing.")

(defconst claude-usage--stale-marker "STALE "
  "Prefix applied to the cache-age string when the cache is stale.
Used only by the standalone `*claude-usage*' buffer's own header
(`claude-usage--redraw'); the mode-line segment and the sidebar section
heading use `claude-usage--stale-glyph' instead.")

(defconst claude-usage--stale-glyph "~"
  "Tilde prefix applied to a stale reading on the mode-line segment and
the sidebar section heading, paired with the `claude-usage-stale' face.")

(define-derived-mode claude-usage-mode magit-section-mode "Claude-Usage"
  "Major mode for the `*claude-usage*' buffer, showing Claude CLI usage meters."
  (setq-local revert-buffer-function #'claude-usage--revert)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'claude-usage-mode 'motion)))

;; `magit-section-mode's parent is `special-mode', which already binds
;; g -> `revert-buffer', q -> `quit-window', and leaves TAB unbound (see
;; straight/repos/magit/lisp/magit-section.el) -- only q needs a plain
;; override here, to `bury-buffer' instead of `quit-window'.
(define-key claude-usage-mode-map (kbd "q") #'bury-buffer)

;; A plain `define-key' on `claude-usage-mode-map' alone is invisible to
;; real key lookup in motion state: evil installs its state keymaps via
;; `emulation-mode-map-alists', consulted BEFORE the buffer's local map --
;; `evil-motion-state-map's `g' is itself a full prefix keymap (gg, gt,
;; ...), so it shadows a plain `g' binding exactly the way sidebar.el's
;; RET was shadowed by `evil-motion-state-map's RET. `q' is unbound in
;; motion state today, so a plain binding would "happen to work" -- bound
;; the same defended way anyway, per sidebar.el's own stated policy
;; against relying on that coincidence.
(with-eval-after-load 'evil
  (evil-define-key 'motion claude-usage-mode-map
    (kbd "g") #'revert-buffer
    (kbd "q") #'bury-buffer))

;; ============================================================================
;; Rendering
;; ============================================================================

(defun claude-usage--available-width ()
  "Columns available for one meter row.
The live window when there is one, else the sidebar's configured width
-- these rows are drawn once before `display-buffer' has shown the
sidebar. The last resort is deliberately WIDE, not narrow: with neither
a window nor sidebar.el loaded there is nothing to fit, and guessing
narrow would truncate a plain `claude-usage--render-to-string' caller
that has no width constraint at all."
  (let ((w (get-buffer-window (current-buffer) t)))
    (cond ((window-live-p w) (window-body-width w))
          ((bound-and-true-p edmacs-sidebar-width) edmacs-sidebar-width)
          (t 80))))

(defun claude-usage--row-layout (avail)
  "Return (LABEL-WIDTH BAR-WIDTH SHOW-RESET TRUNCATE-LABEL) for AVAIL columns.
The reset column (\"6:59 AM (in 129h 3m)\") is 20 columns on its own, so
it is the first thing dropped: a row that does not fit is truncated by
redisplay, which costs the percentage -- the one number worth reading.

TRUNCATE-LABEL is nil at the widest tier, where the label column pads but
never cuts. That is the pre-existing behaviour for a full-width render
(a label like \"Week (Claude 3.5 Opus)\" overflows its column rather than
losing its model name), and only a genuinely constrained row cuts."
  (cond ((>= avail 55) (list 20 12 t nil))
        ((>= avail 34) (list 16 8 nil t))
        ((>= avail 24) (list 12 6 nil t))
        (t (list 8 4 nil t))))

(defun claude-usage--insert-meter-row (meter now)
  "Insert one row for METER into the current buffer.

NOW is threaded through to `claude-usage--format-reset' for deterministic
rendering in tests. Never calls `claude-usage--severity-face' with a nil
percent -- that fallback path does `(>= percent 90)' and would error --
falling back to `claude-usage--em-dash' and a neutral face for the bar,
percent, and (when `:resets-at' is missing) reset columns instead.

The row is laid out against the window's real width rather than a fixed
43-column prefix: at the sidebar's default 30 columns the fixed form ran
to 53 and redisplay truncated the percentage off the right edge."
  (let* ((label (plist-get meter :label))
         (percent (plist-get meter :percent))
         (severity (plist-get meter :severity))
         (resets-at (plist-get meter :resets-at))
         (layout (claude-usage--row-layout (claude-usage--available-width)))
         (label-width (nth 0 layout))
         (bar-width (nth 1 layout))
         (show-reset (nth 2 layout))
         (truncate-label (nth 3 layout))
         (face (if percent (claude-usage--severity-face severity percent) 'default))
         (bar (if percent (claude-usage--bar percent bar-width) claude-usage--em-dash))
         (percent-str (if percent (format "%d%%" percent) claude-usage--em-dash))
         (reset-str (if resets-at
                        (claude-usage--format-reset resets-at now)
                      claude-usage--em-dash)))
    (magit-insert-section (claude-usage-meter)
      ;; Emacs `format' has no `*' width specifier (that is C printf), so the
      ;; column width is baked into the control string.
      (insert (format (format "  %%-%ds %%s %%4s%%s\n" label-width)
                       (if truncate-label
                           (truncate-string-to-width label label-width nil nil t)
                         label)
                       (propertize bar 'face face)
                       (propertize percent-str 'face face)
                       (if show-reset (concat "  " reset-str) ""))))))

(defun claude-usage--redraw (cached-util &optional now source)
  "Erase the current buffer and redraw it from CACHED-UTIL.

CACHED-UTIL is a `cachedUsageUtilization' alist as returned by
`claude-usage--read-cache' or synthesized by `claude-usage--fetch', or
nil when neither is available. NOW, if given, is threaded through to
`claude-usage--format-age' and `claude-usage--format-reset' for
deterministic rendering in tests -- `claude-usage-stale-p' has no such
parameter and always reads the real clock. SOURCE, if given, is `live'
or `cache' and is shown in the header so a fallback to a stale cache can
never be mistaken for a live reading.

Inserts a header section (title plus source and cache age, or a plain
\"No usage data\" line when CACHED-UTIL is nil) and, when CACHED-UTIL is
non-nil, a Limits section with one row per `claude-usage-meters' entry.
When the cache is stale, the age is prefixed with
`claude-usage--stale-marker' and the whole Limits section is dimmed with
the `shadow' face, so an aged number can never be mistaken for a live
one."
  (let ((inhibit-read-only t)
        (fetched-at-ms (and cached-util (alist-get 'fetchedAtMs cached-util))))
    (erase-buffer)
    (let ((stale (and fetched-at-ms (claude-usage-stale-p fetched-at-ms))))
      (magit-insert-section (claude-usage-root)
        (magit-insert-section (claude-usage-header)
          (if (null cached-util)
              (insert "No usage data\n")
            (magit-insert-heading
              (format "Claude Usage (%s%s%s)"
                      (if source (format "%s, " (symbol-name source)) "")
                      (if stale claude-usage--stale-marker "")
                      (claude-usage--format-age fetched-at-ms now)))))
        (when cached-util
          (magit-insert-section (claude-usage-limits)
            (let ((section-start (point)))
              (magit-insert-heading "Limits")
              (let ((meters (claude-usage-meters cached-util)))
                (if meters
                    (dolist (m meters)
                      (claude-usage--insert-meter-row m now))
                  (insert "  no meters\n")))
              (when stale
                (add-face-text-property section-start (point) 'shadow)))))))))

(defun claude-usage--render-to-string (cached-util &optional now source)
  "Render CACHED-UTIL as `claude-usage--redraw' would, returning a string.
NOW and SOURCE are passed through unchanged. Runs in a temp buffer, so
tests need neither a display nor the real `claude-usage-cache-file'."
  (with-temp-buffer
    (claude-usage-mode)
    (claude-usage--redraw cached-util now source)
    (buffer-string)))

;; ============================================================================
;; Buffer management and commands
;; ============================================================================

(defun claude-usage--ensure-buffer ()
  "Return the `*claude-usage*' buffer, creating and (re)populating it.

Renders synchronously from already-resolved state or the on-disk cache
only -- never the keychain or network, so this never blocks -- then
schedules a deferred refresh via a 0-second idle timer, which fires only
once the current command finishes and Emacs goes idle."
  (let ((buf (get-buffer-create "*claude-usage*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'claude-usage-mode)
        (claude-usage-mode))
      (claude-usage--sync-render))
    (run-with-idle-timer 0 nil #'claude-usage--refresh)
    buf))

(defun claude-usage--revert (&rest _ignore)
  "`revert-buffer-function' for `claude-usage-mode': trigger a refresh.
Redraws immediately from current state as a stopgap, then lets
`claude-usage--refresh's own callback redraw again once its fetch (or
fallback) resolves."
  (claude-usage--sync-render)
  (claude-usage--refresh))

;;;###autoload
(defun claude-usage ()
  "Show the `*claude-usage*' buffer, creating or reverting it first."
  (interactive)
  (pop-to-buffer (claude-usage--ensure-buffer)))

(claude-usage--setup-idle-timer)


;; ============================================================================
;; Cheap surfaces: nano-modeline segment and sidebar section
;; ============================================================================
;; Both are compressions of the `*claude-usage*' buffer that must cost
;; nothing at redisplay time, so both read pre-rendered values off
;; `claude-usage--state-envelope' and never the on-disk cache, the
;; keychain, or the network.

(defcustom claude-usage-mode-line-format 'full
  "How `claude-usage-mode-line-mode' renders its segment.
`full' shows a bar and percentage per headline meter plus the next reset
clock; `percent' shows the percentages alone."
  :type '(choice (const :tag "Bars, percentages and next reset" full)
                 (const :tag "Percentages only" percent))
  :group 'claude-usage)

(defcustom claude-usage-sidebar-section t
  "Whether `edmacs-sidebar' shows a Claude usage section."
  :type 'boolean
  :group 'claude-usage)

(defconst claude-usage--mode-line-bar-width 5
  "Bar width, in characters, of a mode-line meter.")

(defconst claude-usage--sidebar-bar-width 8
  "Bar width, in characters, of a sidebar meter row.")

(defvar claude-usage-mode-line-string nil
  "The pre-rendered, propertized mode-line segment, or nil.
Recomputed only when the usage state changes, so
`claude-usage-mode-line-segment' can hand it back untouched on every
render.")

(defvar claude-usage--surface-digest 'unset
  "What `claude-usage-mode-line-string' was last rendered from.
Compared with `equal' to suppress redundant work when a refresh returns
values that render identically.  The initial value is a sentinel rather
than nil so the first update out of a nil state still counts as a
change.")

(defun claude-usage--format-reset-clock (resets-at-iso)
  "Format RESETS-AT-ISO as a bare local wall clock like \"8:00 PM\".
Returns \"\" for nil or unparseable input.  Deliberately absolute rather
than relative: the mode-line string is cached between state changes, so
a countdown baked into it would silently drift by the elapsed time."
  (condition-case nil
      (let ((resets-at (and (stringp resets-at-iso)
                            (claude-usage--parse-iso8601 resets-at-iso))))
        (if resets-at
            (format-time-string "%-I:%M %p" resets-at (getenv "TZ"))
          ""))
    (error "")))

(defun claude-usage--nearest-future-reset (rows)
  "Return the soonest still-future `:resets-at' string among ROWS, or nil."
  (let ((now (float-time (current-time)))
        (best nil)
        (best-secs nil))
    (dolist (row rows)
      (let* ((iso (plist-get row :resets-at))
             (tm (and (stringp iso) (claude-usage--parse-iso8601 iso)))
             (secs (and tm (float-time tm))))
        (when (and secs (> secs now) (or (null best-secs) (< secs best-secs)))
          (setq best iso
                best-secs secs))))
    best))

(defun claude-usage--surface-values (&optional envelope now)
  "Return the rendered values both cheap surfaces draw from, or nil.

ENVELOPE defaults to `claude-usage--state-envelope'; the on-disk cache is
never consulted.  NOW is threaded through the formatters for
deterministic rendering in tests.

The result is a plist (:source SYM :stale BOOL :age STR :rows ROWS),
where ROWS holds one plist per `claude-usage-meters' entry:
\(:id :label :percent :percent-str :bar :reset :resets-at :face).
Returns nil when there is no envelope or it yields no meters, which both
callers treat as \"render nothing at all\"."
  (let* ((env (or envelope claude-usage--state-envelope))
         (meters (and env (claude-usage-meters env))))
    (when meters
      (let* ((fetched-at-ms (alist-get 'fetchedAtMs env))
             (stale (and fetched-at-ms (claude-usage-stale-p fetched-at-ms))))
        (list
         :source claude-usage--state-source
         :stale stale
         :age (if fetched-at-ms (claude-usage--format-age fetched-at-ms now) "")
         :rows
         (mapcar
          (lambda (meter)
            (let* ((percent (plist-get meter :percent))
                   (resets-at (plist-get meter :resets-at))
                   ;; A nil percent must never reach `claude-usage--severity-face':
                   ;; its threshold fallback does `(>= percent 90)'.
                   (face (if percent
                             (claude-usage--severity-face
                              (plist-get meter :severity) percent)
                           'default)))
              (list :id (plist-get meter :id)
                    :label (plist-get meter :label)
                    :percent percent
                    :percent-str (if percent
                                     (format "%d%%" percent)
                                   claude-usage--em-dash)
                    :bar (if percent
                             (claude-usage--bar
                              percent claude-usage--sidebar-bar-width)
                           claude-usage--em-dash)
                    :reset (if resets-at
                               (claude-usage--format-reset resets-at now)
                             claude-usage--em-dash)
                    :resets-at resets-at
                    :face face)))
          meters))))))

(defun claude-usage--escape-mode-line-percent (string)
  "Return STRING with every \"%\" doubled.
Display expands %-constructs in whatever an `:eval' element hands back,
so an unescaped \"45%\" reaches the footer as \"45\" plus whatever the
next character happens to mean to the mode line.  Escaping happens before
`propertize' so the doubled sign keeps its severity face."
  (replace-regexp-in-string "%" "%%" string t t))

(defun claude-usage--render-mode-line (&optional values)
  "Render VALUES, from `claude-usage--surface-values', as a mode-line string.
Returns \"\" when VALUES is nil or carries neither headline meter."
  (if (null values)
      ""
    (let ((rows (plist-get values :rows))
          (parts nil))
      ;; Use shared headline-rows helper to extract session and weekly_all meters
      (dolist (pair (claude-usage--headline-rows rows))
        (let* ((abbr (car pair))
               (row (cdr pair)))
          (when (plist-get row :percent)
            (let ((face (plist-get row :face))
                  (percent-str (claude-usage--escape-mode-line-percent
                                (plist-get row :percent-str))))
              (push (concat
                     abbr " "
                     (propertize
                      (if (eq claude-usage-mode-line-format 'percent)
                          percent-str
                        (concat (claude-usage--bar
                                 (plist-get row :percent)
                                 claude-usage--mode-line-bar-width)
                                percent-str))
                      'face face))
                    parts)))))
      (if (null parts)
          ""
        (let* ((clock (if (eq claude-usage-mode-line-format 'percent)
                          ""
                        (claude-usage--format-reset-clock
                         (claude-usage--nearest-future-reset rows))))
               (rendered (concat (if (plist-get values :stale)
                                     claude-usage--stale-glyph
                                   "")
                                 (mapconcat #'identity (nreverse parts) " ")
                                 (if (string-empty-p clock)
                                     ""
                                   (concat " →" clock)))))
          (when (plist-get values :stale)
            (add-face-text-property 0 (length rendered) 'claude-usage-stale nil rendered))
          rendered)))))

(defun claude-usage--recompute-surfaces (values digest &optional redraw-sidebars)
  "Rewrite the cached segment from VALUES and record DIGEST, unconditionally.
Redraws every frame's sidebar only when REDRAW-SIDEBARS is non-nil --
`edmacs-sidebar--redraw' already no-ops for a frame that never showed
one.  This is the worker `claude-usage--update-surfaces' gates and the
minor mode deliberately calls ungated."
  (setq claude-usage-mode-line-string (claude-usage--render-mode-line values)
        claude-usage--surface-digest digest)
  (force-mode-line-update t)
  (when (and redraw-sidebars (fboundp 'edmacs-sidebar--redraw))
    (dolist (frame (frame-list))
      (edmacs-sidebar--redraw frame))))

(defun claude-usage--update-surfaces ()
  "Refresh both cheap surfaces, skipping the work when nothing rendered moved.
The digest compares rendered strings, not raw state, so a live fetch that
returns unchanged percentages drives no sidebar redraw even though
`fetchedAtMs' moved.  `claude-usage-mode-line-format' is part of it:
changing that defcustom is not a state change but does invalidate the
cached string."
  (let* ((values (claude-usage--surface-values))
         (digest (list claude-usage-mode-line-format values)))
    (unless (equal digest claude-usage--surface-digest)
      (claude-usage--recompute-surfaces values digest t))))

;;;###autoload
(define-minor-mode claude-usage-mode-line-mode
  "Show the two headline Claude usage meters in the mode line.

The mode-line construct itself is never touched.  The segment is spliced
in once at module load by advice on `nano-modeline-footer', and this mode
only changes what `claude-usage-mode-line-segment' returns -- which is why
it takes effect in buffers created before it was enabled, and why
toggling it leaves `mode-line-format' byte-identical."
  :global t
  :group 'claude-usage
  (if claude-usage-mode-line-mode
      (let ((values (claude-usage--surface-values)))
        ;; Ungated on purpose: an off/on toggle is not a state change, so the
        ;; digest would match and leave the segment blank.  REDRAW-SIDEBARS is
        ;; nil for the same reason -- nothing about the usage state moved.
        (claude-usage--recompute-surfaces
         values (list claude-usage-mode-line-format values))
        (run-with-idle-timer 0 nil #'claude-usage--refresh))
    ;; Clear both together: the digest must always describe what the cached
    ;; string was rendered from.
    (setq claude-usage-mode-line-string nil
          claude-usage--surface-digest 'unset)
    (force-mode-line-update t)))

(defun claude-usage-mode-line-segment ()
  "Return the cached usage segment, or \"\" when the mode is off.
Nullary and allocation-free: this runs on every mode-line render."
  (if (and claude-usage-mode-line-mode claude-usage-mode-line-string)
      claude-usage-mode-line-string
    ""))

(defun claude-usage--nano-modeline-footer-filter-args (args)
  "Append the usage segment to `nano-modeline-footer's RIGHT element list.
ARGS is (LEFT [RIGHT [DEFAULT]]); the two-argument shape is the mainline
one for `nano-modeline-message-mode', `nano-modeline-term-mode' and
ui.el's ghostel line.  The element must be a list, not a bare symbol:
`nano-modeline--make' `apply's its car to its cdr.  RIGHT is a shared
quoted literal inside nano-modeline, so it is appended to, never
mutated, and the `member' check keeps a re-bake from doubling it."
  (let ((element '(claude-usage-mode-line-segment))
        (right (nth 1 args)))
    (list (nth 0 args)
          (if (member element right) right (append right (list element)))
          (nth 2 args))))

(defun claude-usage--install-mode-line-advice ()
  "Splice the usage segment into every nano-modeline footer.
Targets `nano-modeline-footer' because ui.el binds
`nano-modeline-position' to it; a switch to `nano-modeline-header' there
would silently drop the segment.  The default line is then re-baked
because ui.el bakes it long before init.el loads this file -- through
ui.el's own wrapper when present, since re-baking with plain
`nano-modeline-text-mode' would strip that line's filtered buffer name
and diagnostics."
  (advice-add 'nano-modeline-footer :filter-args
              #'claude-usage--nano-modeline-footer-filter-args)
  (when (and (consp (default-value 'mode-line-format))
             (eq (car (default-value 'mode-line-format)) :eval))
    (with-temp-buffer
      (cond ((fboundp 'edmacs-modeline-text-mode) (edmacs-modeline-text-mode t))
            ((fboundp 'nano-modeline-text-mode) (nano-modeline-text-mode t))))))

;; Not a bare `advice-add': advising an undefined `nano-modeline-footer'
;; succeeds and defines its function cell, making `fboundp' lie.
(with-eval-after-load 'nano-modeline
  (claude-usage--install-mode-line-advice))

(defun claude-usage--sidebar-meter-line (row)
  "Format ROW as one sidebar meter line, fitted to the sidebar's width.
Laid out against the live window rather than a fixed 43-column prefix:
at the sidebar's default 30 body columns the fixed form ran to 53 and
redisplay cut the percentage -- the one number worth reading -- off the
right edge. The reset field (\"7:00 AM (in 128h 51m)\") is 20 columns on
its own and so is dropped first; then the label column shrinks.

The bar arrives pre-rendered in ROW (`claude-usage--surface-values'
builds it at a fixed width for every surface), so a narrow sidebar
truncates it rather than re-rendering it at a smaller width."
  (let* ((layout (claude-usage--row-layout (claude-usage--available-width)))
         (label-width (nth 0 layout))
         (bar-width (nth 1 layout))
         (show-reset (nth 2 layout))
         (truncate-label (nth 3 layout))
         (label (plist-get row :label))
         (face (plist-get row :face)))
    (format (format " %%-%ds %%s %%4s%%s\n" label-width)
            (if truncate-label
                (truncate-string-to-width label label-width nil nil t)
              label)
            (propertize (truncate-string-to-width (plist-get row :bar) bar-width)
                        'face face)
            (propertize (plist-get row :percent-str) 'face face)
            (if show-reset (concat "  " (plist-get row :reset)) ""))))

(defun claude-usage--insert-sidebar-section (frame)
  "Insert the usage section into the current sidebar buffer.
FRAME is accepted (per `edmacs-sidebar-bottom-anchor-section-functions')
but unused: the section's content is the whole (frame-independent)
usage state, identical on every frame. Inserts nothing at all -- not a
heading, not a separator -- when the section is switched off or no
usage state has been resolved yet."
  (ignore frame)
  (when claude-usage-sidebar-section
    (let ((values (claude-usage--surface-values)))
      (when values
        (let ((start (point)))
          (insert "\n")
          (magit-insert-section (claude-usage-sidebar)
            (magit-insert-heading
              (format "Claude · %s%s"
                      (if (plist-get values :stale) claude-usage--stale-glyph "")
                      (plist-get values :age)))
            (dolist (row (plist-get values :rows))
              ;; Section value stays nil: `edmacs-sidebar-activate' has no
              ;; cond branch for it, so RET on a usage row signals
              ;; user-error ("Nothing to do on this row") instead of
              ;; silently doing nothing.
              (magit-insert-section (claude-usage-sidebar-meter)
                (insert (claude-usage--sidebar-meter-line row)))))
          (when (plist-get values :stale)
            (add-face-text-property start (point) 'claude-usage-stale)))))))

;; `add-hook' creates the variable when sidebar.el has not loaded yet, so
;; this file needs no `(require 'sidebar)' and the dependency stays
;; one-way: sidebar.el owns the hook and never mentions claude-usage.
;; Registered on the bottom-anchor hook, not `edmacs-sidebar-extra-section-functions':
;; sidebar.el pins whatever this hook inserts to the window's bottom edge, so
;; ordering relative to other `edmacs-sidebar-extra-section-functions'
;; registrants (e.g. sidebar-agents.el's ALL AGENTS block) no longer matters.
(add-hook 'edmacs-sidebar-bottom-anchor-section-functions
          #'claude-usage--insert-sidebar-section)

;; ============================================================================
;; Collapsed sidebar strip: usage summary
;; ============================================================================

(defun claude-usage--headline-rows (rows)
  "Return headline rows from ROWS in attention order (session, weekly_all).
Returns (ABBR . ROW) pairs where ABBR is \"S\" or \"W\"."
  (let ((headline-pairs nil))
    (dolist (spec '((session . "S") (weekly_all . "W")))
      (dolist (row rows)
        (when (eq (plist-get row :id) (car spec))
          (push (cons (cdr spec) row) headline-pairs))))
    (nreverse headline-pairs)))

(defun claude-usage--collapsed-section (frame width)
  "Insert usage data into the current buffer's collapsed sidebar strip for FRAME.
Inserts one line per headline meter (session, weekly_all), or nothing when no
usage data is available. Each line is formatted compactly as ABBR followed by
the percent value (e.g. \"S45%\"), fitted independently within WIDTH columns
using `edmacs-sidebar--fit'. Measures with `string-width' to respect
double-width nerd-font glyphs.
FRAME is currently unused (usage data is frame-independent); accepted
for registration on `edmacs-sidebar-collapsed-bottom-anchor-section-functions'
compatibility."
  (ignore frame)
  (let ((values (claude-usage--surface-values)))
    (when values
      (let ((rows (plist-get values :rows)))
        (when rows
          ;; Get headline rows from surface values using shared helper
          (let ((headline-pairs (claude-usage--headline-rows rows)))
            (dolist (pair headline-pairs)
              (let* ((abbr (car pair))
                     (row (cdr pair))
                     (percent (plist-get row :percent))
                     ;; For 100%+ usage, use a special indicator instead of truncating digits
                     (percent-str (if (>= percent 100)
                                      "●"  ;; Bullet indicates full/overflow
                                    (plist-get row :percent-str)))
                     ;; Format as "S45%" or "W60%" or "S●" (all fit in 4 columns)
                     (line (format "%s%s" abbr percent-str))
                     (face (plist-get row :face))
                     ;; Fit each line independently using edmacs-sidebar--fit
                     (fitted (edmacs-sidebar--fit line width))
                     ;; Apply face to the fitted line
                     (propertized (if face (propertize fitted 'face face) fitted)))
                (insert propertized "\n")))))))))

;; Registered on the collapsed bottom-anchor hook, not
;; `edmacs-sidebar-collapsed-section-functions': see the identical
;; ordering-no-longer-matters comment above `--insert-sidebar-section's
;; own registration.
(add-hook 'edmacs-sidebar-collapsed-bottom-anchor-section-functions
          #'claude-usage--collapsed-section)

;; Seed synchronously from the on-disk cache so the sidebar section has
;; figures the moment Emacs starts, rather than only once an async fetch
;; resolves.  `--apply-refresh-result' with nil means "no live result": it
;; falls back to the cache, marks the source `cache', and updates both
;; surfaces.  A file read only -- no keychain, no network -- so it is safe
;; on init's load path.
;; `noninteractive' guard: batch is where the ERT suites live, and they own
;; module state through their own fixtures.  Seeding from the real
;; `~/.claude.json' there would leak this machine's figures into their
;; assertions -- which is exactly what phase 1's "no reads of the real
;; ~/.claude.json" criterion forbids.
(unless noninteractive
  (claude-usage--apply-refresh-result nil))

;; Then upgrade to live.  The periodic timer above needs
;; `claude-usage-idle-refresh-delay' seconds of continuous idleness before
;; its first fire, so without this the figures stay cache-sourced until
;; `*claude-usage*' is opened.  Deferred through a 0-second idle timer
;; because the token read is a subprocess and the fetch is network, neither
;; of which may run on init's own load path.
(unless noninteractive
  (run-with-idle-timer 0 nil #'claude-usage--refresh))

(provide 'claude-usage)

;;; claude-usage.el ends here
