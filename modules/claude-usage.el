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

(defgroup claude-usage nil
  "Claude usage metrics and cache integration."
  :group 'claude
  :prefix "claude-usage-")

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
- >= 90%: `error' face
- >= 70%: `warning' face
- < 70%: `success' face

If SEVERITY is provided and not nil, uses it directly."
  (cond
   ((string-equal severity "critical") 'error)
   ((string-equal severity "warning") 'warning)
   ((string-equal severity "normal") 'success)
   ;; Fallback to percent-based thresholds
   ((>= percent 90) 'error)
   ((>= percent 70) 'warning)
   (t 'success)))

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
and `claude-usage--state-source' to the same values."
  (let* ((envelope (or fetch-envelope (claude-usage--read-cache)))
         (source (cond (fetch-envelope 'live)
                       (envelope 'cache)
                       (t nil))))
    (setq claude-usage--state-envelope envelope
          claude-usage--state-source source)
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
  "Prefix applied to the cache-age string when the cache is stale.")

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

(defun claude-usage--insert-meter-row (meter now)
  "Insert one row for METER into the current buffer.

NOW is threaded through to `claude-usage--format-reset' for deterministic
rendering in tests. Never calls `claude-usage--severity-face' with a nil
percent -- that fallback path does `(>= percent 90)' and would error --
falling back to `claude-usage--em-dash' and a neutral face for the bar,
percent, and (when `:resets-at' is missing) reset columns instead."
  (let* ((label (plist-get meter :label))
         (percent (plist-get meter :percent))
         (severity (plist-get meter :severity))
         (resets-at (plist-get meter :resets-at))
         (face (if percent (claude-usage--severity-face severity percent) 'default))
         (bar (if percent (claude-usage--bar percent 12) claude-usage--em-dash))
         (percent-str (if percent (format "%d%%" percent) claude-usage--em-dash))
         (reset-str (if resets-at
                        (claude-usage--format-reset resets-at now)
                      claude-usage--em-dash)))
    (magit-insert-section (claude-usage-meter)
      (insert (format "  %-20s %s %5s  %s\n"
                       label
                       (propertize bar 'face face)
                       (propertize percent-str 'face face)
                       reset-str)))))

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

(provide 'claude-usage)

;;; claude-usage.el ends here
