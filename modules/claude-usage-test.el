;;; claude-usage-test.el --- Tests for claude-usage.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edward Paget
;; Author: Edward Paget <ed.paget@gmail.com>

;; This file is part of edmacs.

;;; Commentary:

;; ERT test suite for claude-usage.el. Run in batch from the repository root:
;;
;;   emacs -Q --batch -l ert -l modules/claude-usage.el -l modules/claude-usage-test.el -f ert-run-tests-batch-and-exit
;;
;; Tests cover: full payload with limits array, fallback to legacy keys,
;; null value handling, missing files, malformed JSON, and fixed-timestamp
;; format functions with no reliance on current-time.

;;; Code:

(require 'ert)
(require 'claude-usage)

;;; Fixtures

;; Full payload with limits array containing 3 entries: session, weekly_all, weekly_scoped
(defconst claude-usage-test--fixture-full-payload
  '((cachedUsageUtilization
     (fetchedAtMs . 1725274680000)
     (accountUuid . "test-uuid-123")
     (limits .
             (((kind . "session")
               (utilization . 0.45)
               (severity . "normal")
               (resets_at . "2026-09-03T20:00:00Z")
               (limit_dollars . 10.0)
               (used_dollars . 4.5)
               (remaining_dollars . 5.5))
              ((kind . "weekly_all")
               (utilization . 0.62)
               (severity . "warning")
               (resets_at . "2026-09-07T00:00:00Z")
               (limit_dollars . 100.0)
               (used_dollars . 62.0)
               (remaining_dollars . 38.0))
              ((kind . "weekly_scoped")
               (utilization . 0.88)
               (severity . "critical")
               (resets_at . "2026-09-07T00:00:00Z")
               (limit_dollars . 50.0)
               (used_dollars . 44.0)
               (remaining_dollars . 6.0)
               (scope
                (model
                 (id . "claude-opus-4-1")
                 (display_name . "Claude 3.5 Opus"))))))
     (extra_usage . 0.0)
     (spend . 110.5)))
  "Full payload with three limits entries.")

;; Fallback payload without limits key but with five_hour and seven_day
(defconst claude-usage-test--fixture-fallback-payload
  '((cachedUsageUtilization
     (fetchedAtMs . 1725274680000)
     (accountUuid . "test-uuid-456")
     (five_hour
      (utilization . 0.30)
      (severity . "normal")
      (resets_at . "2026-09-03T20:00:00Z")
      (limit_dollars . 10.0)
      (used_dollars . 3.0)
      (remaining_dollars . 7.0))
     (seven_day
      (utilization . 0.55)
      (severity . "warning")
      (resets_at . "2026-09-07T00:00:00Z")
      (limit_dollars . 100.0)
      (used_dollars . 55.0)
      (remaining_dollars . 45.0))
     (extra_usage . 0.0)
     (spend . 58.0)))
  "Fallback payload with five_hour and seven_day instead of limits.")

;; Payload with null values in fallback keys
(defconst claude-usage-test--fixture-null-values-fallback
  '((cachedUsageUtilization
     (fetchedAtMs . 1725274680000)
     (accountUuid . "test-uuid-789")
     (five_hour
      (utilization . 0.25)
      (severity . "normal")
      (resets_at . "2026-09-03T20:00:00Z")
      (limit_dollars . 10.0)
      (used_dollars . 2.5)
      (remaining_dollars . 7.5))
     (seven_day
      (utilization . 0.50)
      (severity . "normal")
      (resets_at . "2026-09-07T00:00:00Z")
      (limit_dollars . 100.0)
      (used_dollars . 50.0)
      (remaining_dollars . 50.0))
     (seven_day_opus . nil)
     (seven_day_sonnet
      (utilization . 0.75)
      (severity . "warning")
      (resets_at . "2026-09-07T00:00:00Z")
      (limit_dollars . 50.0)
      (used_dollars . 37.5)
      (remaining_dollars . 12.5))
     (extra_usage . 0.0)
     (spend . 90.0)))
  "Fallback payload with null values for opus but present sonnet.")

;; Payload with limits array containing null utilization entries
(defconst claude-usage-test--fixture-null-values-in-limits
  '((cachedUsageUtilization
     (fetchedAtMs . 1725274680000)
     (accountUuid . "test-uuid-999")
     (limits .
             (((kind . "session")
               (utilization . 0.35)
               (severity . "normal")
               (resets_at . "2026-09-03T20:00:00Z")
               (limit_dollars . 10.0)
               (used_dollars . 3.5)
               (remaining_dollars . 6.5))
              ((kind . "weekly_all")
               (utilization . nil)
               (severity . "normal")
               (resets_at . "2026-09-07T00:00:00Z")
               (limit_dollars . 100.0)
               (used_dollars . 0.0)
               (remaining_dollars . 100.0))
              ((kind . "weekly_scoped")
               (utilization . 0.80)
               (severity . "warning")
               (resets_at . "2026-09-07T00:00:00Z")
               (limit_dollars . 50.0)
               (used_dollars . 40.0)
               (remaining_dollars . 10.0)
               (scope
                (model
                 (id . "claude-sonnet-4")
                 (display_name . "Claude 3.5 Sonnet"))))))
     (extra_usage . 0.0)
     (spend . 43.5)))
  "Limits array with null utilization in one entry (weekly_all).")

;;; Tests

(ert-deftest claude-usage-test-meters-full-payload ()
  "Full payload with limits array should produce three meters."
  (let* ((cache (alist-get 'cachedUsageUtilization claude-usage-test--fixture-full-payload))
         (meters (claude-usage-meters cache)))
    ;; Should have 3 meters
    (should (= (length meters) 3))

    ;; First meter: session
    (let ((m1 (nth 0 meters)))
      (should (eq (plist-get m1 :id) 'session))
      (should (string-equal (plist-get m1 :label) "Session (5h)"))
      (should (= (plist-get m1 :percent) 45))
      (should (string-equal (plist-get m1 :severity) "normal")))

    ;; Second meter: weekly_all
    (let ((m2 (nth 1 meters)))
      (should (string-equal (plist-get m2 :id) "weekly_all"))
      (should (string-equal (plist-get m2 :label) "Week (all)"))
      (should (= (plist-get m2 :percent) 62))
      (should (string-equal (plist-get m2 :severity) "warning")))

    ;; Third meter: weekly_scoped
    (let ((m3 (nth 2 meters)))
      (should (string-equal (plist-get m3 :id) "weekly_scoped"))
      (should (string-match "Week (Claude 3.5 Opus)" (plist-get m3 :label)))
      (should (= (plist-get m3 :percent) 88))
      (should (string-equal (plist-get m3 :severity) "critical"))
      (should (string-equal (plist-get m3 :model) "Claude 3.5 Opus")))))

(ert-deftest claude-usage-test-meters-fallback ()
  "Payload without limits key should fall back to five_hour and seven_day."
  (let* ((cache (alist-get 'cachedUsageUtilization claude-usage-test--fixture-fallback-payload))
         (meters (claude-usage-meters cache)))
    ;; Should have 2 meters (five_hour and seven_day)
    (should (= (length meters) 2))

    ;; First meter: session (from five_hour)
    (let ((m1 (nth 0 meters)))
      (should (eq (plist-get m1 :id) 'session))
      (should (string-equal (plist-get m1 :label) "Session (5h)"))
      (should (= (plist-get m1 :percent) 30)))

    ;; Second meter: weekly_all (from seven_day)
    (let ((m2 (nth 1 meters)))
      (should (eq (plist-get m2 :id) 'weekly_all))
      (should (string-equal (plist-get m2 :label) "Week (all)"))
      (should (= (plist-get m2 :percent) 55)))))

(ert-deftest claude-usage-test-meters-null-values-fallback ()
  "Null utilization in fallback keys should be skipped."
  (let* ((cache (alist-get 'cachedUsageUtilization claude-usage-test--fixture-null-values-fallback))
         (meters (claude-usage-meters cache)))
    ;; Should have 2 meters (five_hour and seven_day, but not seven_day_opus which is nil)
    ;; This fixture has no limits array, so we fall back to the five_hour/seven_day case.
    (should (= (length meters) 2))))

(ert-deftest claude-usage-test-meters-null-values-in-limits ()
  "Null utilization within limits array should be skipped."
  (let* ((cache (alist-get 'cachedUsageUtilization claude-usage-test--fixture-null-values-in-limits))
         (meters (claude-usage-meters cache)))
    ;; Should have 2 meters (session and weekly_scoped), but not weekly_all which has nil utilization
    (should (= (length meters) 2))

    ;; First meter: session
    (let ((m1 (nth 0 meters)))
      (should (eq (plist-get m1 :id) 'session))
      (should (string-equal (plist-get m1 :label) "Session (5h)"))
      (should (= (plist-get m1 :percent) 35)))

    ;; Second meter: weekly_scoped (skipping the nil weekly_all)
    (let ((m2 (nth 1 meters)))
      (should (string-equal (plist-get m2 :id) "weekly_scoped"))
      (should (string-match "Week (Claude 3.5 Sonnet)" (plist-get m2 :label)))
      (should (= (plist-get m2 :percent) 80)))))

(ert-deftest claude-usage-test-read-cache-missing-file ()
  "Reading a non-existent cache file should return nil without signalling."
  ;; Bind to a path that definitely doesn't exist
  (let ((claude-usage-cache-file "/nonexistent/path/.claude.json"))
    (let ((result (claude-usage--read-cache)))
      ;; Result must be nil when file doesn't exist
      (should (null result)))))

(ert-deftest claude-usage-test-read-cache-malformed-json ()
  "Reading malformed JSON should return nil without signalling."
  ;; Create a temporary file with malformed JSON
  (let ((temp-file (make-temp-file "claude-usage-test" nil ".json")))
    (unwind-protect
        (progn
          ;; Write malformed JSON to the temp file
          (with-temp-buffer
            (insert "{invalid json")
            (write-region (point-min) (point-max) temp-file))
          ;; Bind claude-usage-cache-file to the temp file and call the function
          (let ((claude-usage-cache-file temp-file))
            (let ((result (claude-usage--read-cache)))
              ;; Should return nil without signalling
              (should (null result)))))
      ;; Clean up the temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest claude-usage-test-format-reset-fixed-timestamp ()
  "Format reset with fixed timestamp should produce consistent output."
  ;; Use a fixed ISO 8601 string and a fixed current-time
  ;; Current time is 3 hours and 12 minutes before reset
  (let* ((resets-at-iso "2026-09-03T20:00:00Z")
         ;; Mock current-time to be 2026-09-03 16:48:00 UTC (3h 12m before 20:00)
         ;; (seconds since epoch high-order bits low-order bits microseconds)
         (mock-current-time (encode-time 2026 9 3 16 48 0 t))
         (result (claude-usage--format-reset resets-at-iso mock-current-time)))
    ;; Should return a non-empty string
    (should (> (length result) 0))
    ;; Should contain relative time indicator
    (should (string-match "in.*h" result))))

(ert-deftest claude-usage-test-format-age-fixed-timestamp ()
  "Format age with fixed timestamp should produce consistent output."
  ;; Use a fixed fetchedAtMs timestamp and current-time for deterministic output
  ;; Fetched 2 minutes ago (120 seconds)
  (let* ((current-time-val (encode-time 2026 9 3 16 48 0 t))
         ;; 120 seconds before current time
         (fetched-at-ms (* (- (float-time current-time-val) 120) 1000))
         (result (claude-usage--format-age fetched-at-ms current-time-val)))
    ;; Should return a non-empty string
    (should (> (length result) 0))
    ;; Should contain "ago" and "2m"
    (should (string-match "2m ago" result))))

(ert-deftest claude-usage-test-severity-face-critical ()
  "Severity face should map critical to error face."
  (let ((face (claude-usage--severity-face "critical" 50)))
    (should (eq face 'error))))

(ert-deftest claude-usage-test-severity-face-warning ()
  "Severity face should map warning to warning face."
  (let ((face (claude-usage--severity-face "warning" 50)))
    (should (eq face 'warning))))

(ert-deftest claude-usage-test-severity-face-normal ()
  "Severity face should map normal to success face."
  (let ((face (claude-usage--severity-face "normal" 50)))
    (should (eq face 'success))))

(ert-deftest claude-usage-test-severity-face-fallback-high ()
  "Severity face should fallback to critical at 90%+ without severity string."
  (let ((face (claude-usage--severity-face nil 95)))
    (should (eq face 'error))))

(ert-deftest claude-usage-test-severity-face-fallback-mid ()
  "Severity face should fallback to warning at 70-89% without severity string."
  (let ((face (claude-usage--severity-face nil 75)))
    (should (eq face 'warning))))

(ert-deftest claude-usage-test-severity-face-fallback-low ()
  "Severity face should fallback to success below 70% without severity string."
  (let ((face (claude-usage--severity-face nil 50)))
    (should (eq face 'success))))

(ert-deftest claude-usage-test-bar ()
  "Bar should generate Unicode block characters."
  (let ((bar (claude-usage--bar 50 10)))
    ;; 50% of 10 width = 5 filled blocks and 5 empty blocks
    (should (and (= (length bar) 10)
                 (string-match "^█+░+$" bar)))))

(ert-deftest claude-usage-test-bar-zero ()
  "Bar at 0% should be all empty."
  (let ((bar (claude-usage--bar 0 10)))
    (should (string-equal bar "░░░░░░░░░░"))))

(ert-deftest claude-usage-test-bar-full ()
  "Bar at 100% should be all filled."
  (let ((bar (claude-usage--bar 100 10)))
    (should (string-equal bar "██████████"))))

(ert-deftest claude-usage-test-stale-p ()
  "Stale check should compare age against threshold."
  ;; With a very old timestamp, should be stale
  (let* ((very-old-ms 1000000000000)  ; Sep 2001
         (is-stale (claude-usage-stale-p very-old-ms)))
    (should is-stale)))

(ert-deftest claude-usage-test-stale-p-recent ()
  "Recent data should not be stale."
  ;; Use a timestamp from very recently (within threshold)
  (let* ((recent-ms (float-time (current-time)))
         ;; Convert to milliseconds
         (recent-ms-int (truncate (* recent-ms 1000)))
         (is-stale (claude-usage-stale-p recent-ms-int)))
    (should (not is-stale))))

(provide 'claude-usage-test)

;;; claude-usage-test.el ends here
