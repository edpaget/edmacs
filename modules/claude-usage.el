;;; claude-usage.el --- Parse and normalize Claude CLI usage cache JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edward Paget
;; Author: Edward Paget <ed.paget@gmail.com>
;; Keywords: claude usage metrics

;; This file is part of edmacs.

;;; Commentary:

;; Pure Emacs Lisp normalization layer for parsing the Claude CLI's cached usage
;; metrics from ~/.claude.json. Transforms the CLI's cachedUsageUtilization JSON
;; into an ordered list of meter plists (:id :label :percent :severity :resets-at :model),
;; along with helper functions for formatting and staleness checks.
;;
;; No network, subprocess, or UI concerns — this is the single testable surface
;; for usage cache normalization.

;;; Code:

(require 'json)

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
;; Major mode
;; ============================================================================

(defconst claude-usage--em-dash "—"
  "Placeholder for a meter field whose source value is missing.")

(defconst claude-usage--stale-marker "STALE "
  "Prefix applied to the cache-age string when the cache is stale.")

(define-derived-mode claude-usage-mode magit-section-mode "Claude-Usage"
  "Major mode for the `*claude-usage*' buffer, showing Claude CLI usage meters."
  (setq revert-buffer-function #'claude-usage--revert)
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

(defun claude-usage--redraw (cached-util &optional now)
  "Erase the current buffer and redraw it from CACHED-UTIL.

CACHED-UTIL is a `cachedUsageUtilization' alist as returned by
`claude-usage--read-cache', or nil when the cache is absent entirely.
NOW, if given, is threaded through to `claude-usage--format-age' and
`claude-usage--format-reset' for deterministic rendering in tests --
`claude-usage-stale-p' has no such parameter and always reads the real
clock.

Inserts a header section (title plus cache age, or a plain \"No usage
data\" line when CACHED-UTIL is nil) and, when CACHED-UTIL is non-nil, a
Limits section with one row per `claude-usage-meters' entry. When the
cache is stale, the age is prefixed with `claude-usage--stale-marker' and
the whole Limits section is dimmed with the `shadow' face, so an aged
number can never be mistaken for a live one."
  (let ((inhibit-read-only t)
        (fetched-at-ms (and cached-util (alist-get 'fetchedAtMs cached-util))))
    (erase-buffer)
    (let ((stale (and fetched-at-ms (claude-usage-stale-p fetched-at-ms))))
      (magit-insert-section (claude-usage-root)
        (magit-insert-section (claude-usage-header)
          (if (null cached-util)
              (insert "No usage data\n")
            (magit-insert-heading
              (format "Claude Usage (%s%s)"
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

(defun claude-usage--render-to-string (cached-util &optional now)
  "Render CACHED-UTIL as `claude-usage--redraw' would, returning a string.
NOW is passed through unchanged. Runs in a temp buffer, so tests need
neither a display nor the real `claude-usage-cache-file'."
  (with-temp-buffer
    (claude-usage-mode)
    (claude-usage--redraw cached-util now)
    (buffer-string)))

;; ============================================================================
;; Buffer management and commands
;; ============================================================================

(defun claude-usage--ensure-buffer ()
  "Return the `*claude-usage*' buffer, creating and (re)populating it."
  (let ((buf (get-buffer-create "*claude-usage*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'claude-usage-mode)
        (claude-usage-mode))
      (claude-usage--redraw (claude-usage--read-cache)))
    buf))

(defun claude-usage--revert (&rest _ignore)
  "`revert-buffer-function' for `claude-usage-mode': re-read and redraw."
  (claude-usage--redraw (claude-usage--read-cache)))

;;;###autoload
(defun claude-usage ()
  "Show the `*claude-usage*' buffer, creating or reverting it first."
  (interactive)
  (pop-to-buffer (claude-usage--ensure-buffer)))

(provide 'claude-usage)

;;; claude-usage.el ends here
