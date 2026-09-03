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
  "Transform parsed cachedUsageUtilization into an ordered list of meter plists.

CACHED-UTIL is the full `cachedUsageUtilization' object from
`claude-usage--read-cache', containing fetchedAtMs, accountUuid, and utilization.

Returns a list of plists, one per limit entry.
Each plist contains: :id KIND :label LABEL :percent PERCENT
:severity SEVERITY :resets-at ISO8601 :model MODEL

Meters are extracted from the `limits' array if present, or falls back
to the legacy `five_hour' and `seven_day' entries.

Maps limit `kind' to human-readable label:
  - \"session\" -> \"Session (5h)\"
  - \"weekly_all\" -> \"Week (all)\"
  - \"weekly_scoped\" -> \"Week (<model>)\"

For limits[], reads `percent' (already 0-100) directly.
For five_hour/seven_day fallback, reads `utilization' (already 0-100).
Preserves `severity' string, and extracts model name from
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

(provide 'claude-usage)

;;; claude-usage.el ends here
