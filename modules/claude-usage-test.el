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
;; This is the cachedUsageUtilization object structure returned by claude-usage--read-cache
(defconst claude-usage-test--fixture-full-payload
  '((fetchedAtMs . 1725274680000)
    (accountUuid . "test-uuid-123")
    (utilization
     (limits .
             (((kind . "session")
               (percent . 45)
               (severity . "normal")
               (resets_at . "2026-09-03T20:00:00Z")
               (limit_dollars . 10.0)
               (used_dollars . 4.5)
               (remaining_dollars . 5.5))
              ((kind . "weekly_all")
               (percent . 62)
               (severity . "warning")
               (resets_at . "2026-09-07T00:00:00Z")
               (limit_dollars . 100.0)
               (used_dollars . 62.0)
               (remaining_dollars . 38.0))
              ((kind . "weekly_scoped")
               (percent . 88)
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
  "cachedUsageUtilization object with three limits entries, each using percent field.")

;; Fallback payload without limits key but with five_hour and seven_day
;; Uses utilization field for five_hour/seven_day (already 0-100)
(defconst claude-usage-test--fixture-fallback-payload
  '((fetchedAtMs . 1725274680000)
    (accountUuid . "test-uuid-456")
    (utilization
     (five_hour
      (utilization . 30)
      (severity . "normal")
      (resets_at . "2026-09-03T20:00:00Z")
      (limit_dollars . 10.0)
      (used_dollars . 3.0)
      (remaining_dollars . 7.0))
     (seven_day
      (utilization . 55)
      (severity . "warning")
      (resets_at . "2026-09-07T00:00:00Z")
      (limit_dollars . 100.0)
      (used_dollars . 55.0)
      (remaining_dollars . 45.0))
     (extra_usage . 0.0)
     (spend . 58.0)))
  "cachedUsageUtilization object with five_hour and seven_day instead of limits.")

;; Payload with limits array containing null percent entries
(defconst claude-usage-test--fixture-null-percent-in-limits
  '((fetchedAtMs . 1725274680000)
    (accountUuid . "test-uuid-999")
    (utilization
     (limits .
             (((kind . "session")
               (percent . 35)
               (severity . "normal")
               (resets_at . "2026-09-03T20:00:00Z")
               (limit_dollars . 10.0)
               (used_dollars . 3.5)
               (remaining_dollars . 6.5))
              ((kind . "weekly_all")
               (percent . nil)
               (severity . "normal")
               (resets_at . "2026-09-07T00:00:00Z")
               (limit_dollars . 100.0)
               (used_dollars . 0.0)
               (remaining_dollars . 100.0))
              ((kind . "weekly_scoped")
               (percent . 80)
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
  "cachedUsageUtilization object with limits array containing null percent entry.")

;; Fallback payload with null seven_day_opus (per-model field)
;; Tests that null per-model fields don't create meters
(defconst claude-usage-test--fixture-fallback-with-null-seven-day-opus
  '((fetchedAtMs . 1725274680000)
    (accountUuid . "test-uuid-789")
    (utilization
     (five_hour
      (utilization . 40)
      (severity . "normal")
      (resets_at . "2026-09-03T20:00:00Z")
      (limit_dollars . 10.0)
      (used_dollars . 4.0)
      (remaining_dollars . 6.0))
     (seven_day
      (utilization . 65)
      (severity . "warning")
      (resets_at . "2026-09-07T00:00:00Z")
      (limit_dollars . 100.0)
      (used_dollars . 65.0)
      (remaining_dollars . 35.0))
     (seven_day_opus . nil)
     (seven_day_sonnet . nil)
     (extra_usage . 0.0)
     (spend . 69.0)))
  "cachedUsageUtilization object with fallback keys and null per-model fields.")

;;; Tests

(ert-deftest claude-usage-test-meters-full-payload ()
  "Full payload with three limits entries produces three meters."
  (let ((meters (claude-usage-meters claude-usage-test--fixture-full-payload)))
    (should (= (length meters) 3))

    ;; First meter: session
    (let ((m1 (nth 0 meters)))
      (should (eq (plist-get m1 :id) 'session))
      (should (string-equal (plist-get m1 :label) "Session (5h)"))
      (should (= (plist-get m1 :percent) 45))
      (should (string-equal (plist-get m1 :severity) "normal")))

    ;; Second meter: weekly_all
    (let ((m2 (nth 1 meters)))
      (should (eq (plist-get m2 :id) 'weekly_all))
      (should (string-equal (plist-get m2 :label) "Week (all)"))
      (should (= (plist-get m2 :percent) 62))
      (should (string-equal (plist-get m2 :severity) "warning")))

    ;; Third meter: weekly_scoped with model
    (let ((m3 (nth 2 meters)))
      (should (eq (plist-get m3 :id) 'weekly_scoped))
      (should (string-match "Week (" (plist-get m3 :label)))
      (should (string-match "Claude 3.5 Opus" (plist-get m3 :label)))
      (should (= (plist-get m3 :percent) 88))
      (should (string-equal (plist-get m3 :severity) "critical"))
      (should (string-equal (plist-get m3 :model) "Claude 3.5 Opus")))))

(ert-deftest claude-usage-test-meters-fallback ()
  "Fallback payload without limits key produces two meters from five_hour and seven_day."
  (let ((meters (claude-usage-meters claude-usage-test--fixture-fallback-payload)))
    (should (= (length meters) 2))

    ;; First meter: session from five_hour
    (let ((m1 (nth 0 meters)))
      (should (eq (plist-get m1 :id) 'session))
      (should (string-equal (plist-get m1 :label) "Session (5h)"))
      (should (= (plist-get m1 :percent) 30)))

    ;; Second meter: weekly_all from seven_day
    (let ((m2 (nth 1 meters)))
      (should (eq (plist-get m2 :id) 'weekly_all))
      (should (string-equal (plist-get m2 :label) "Week (all)"))
      (should (= (plist-get m2 :percent) 55)))))

(ert-deftest claude-usage-test-meters-null-seven-day-opus ()
  "Fallback with null seven_day_opus (per-model) produces meters only from five_hour and seven_day."
  (let ((meters (claude-usage-meters claude-usage-test--fixture-fallback-with-null-seven-day-opus)))
    ;; Should have exactly 2 meters, not creating any for null seven_day_opus/sonnet
    (should (= (length meters) 2))

    ;; Verify we get meters from five_hour and seven_day only
    (should (eq (plist-get (nth 0 meters) :id) 'session))
    (should (eq (plist-get (nth 1 meters) :id) 'weekly_all))))

(ert-deftest claude-usage-test-meters-null-percent ()
  "Payload with null percent in limits produces only non-null entries."
  (let ((meters (claude-usage-meters claude-usage-test--fixture-null-percent-in-limits)))
    ;; Should have 2 meters (session and weekly_scoped), skipping weekly_all with nil percent
    (should (= (length meters) 2))
    
    ;; Verify it's session and weekly_scoped
    (should (eq (plist-get (nth 0 meters) :id) 'session))
    (should (eq (plist-get (nth 1 meters) :id) 'weekly_scoped))))

(ert-deftest claude-usage-test-read-cache-missing-file ()
  "Missing cache file returns nil without signaling error."
  (let ((orig-val claude-usage-cache-file))
    (unwind-protect
        (progn
          (setq claude-usage-cache-file "/nonexistent/path/to/.claude.json")
          (should (null (claude-usage--read-cache))))
      (setq claude-usage-cache-file orig-val))))

(ert-deftest claude-usage-test-read-cache-malformed-json ()
  "Malformed JSON returns nil without signaling error."
  (let* ((temp-file (make-temp-file "claude-usage-test-"))
         (orig-val claude-usage-cache-file))
    (unwind-protect
        (progn
          ;; Write malformed JSON
          (with-temp-file temp-file
            (insert "{\"cachedUsageUtilization\": {\"utilization\": [invalid json}}"))
          (setq claude-usage-cache-file temp-file)
          (should (null (claude-usage--read-cache))))
      (setq claude-usage-cache-file orig-val)
      (delete-file temp-file))))

(ert-deftest claude-usage-test-format-reset-fixed-timestamp ()
  "Format reset with fixed timestamp produces expected output format."
  ;; Use a fixed resets_at and a mock current-time
  ;; resets_at = 2026-09-03T20:00:00Z
  ;; current-time mocked to 2026-09-03T16:48:00Z (3h 12m before reset)
  (let* ((resets-at "2026-09-03T20:00:00Z")
         ;; Create a mock current-time that is 3h 12m before the reset
         (reset-time (claude-usage--parse-iso8601 resets-at))
         ;; 3h 12m = 11520 seconds
         (current-time (time-subtract reset-time (seconds-to-time 11520)))
         (output (claude-usage--format-reset resets-at current-time)))
    ;; Output should contain "in" and time components
    (should (string-match "in.*[0-9]" output))))

(ert-deftest claude-usage-test-format-age-fixed-timestamp ()
  "Format age with fixed timestamp produces expected output."
  ;; fetchedAtMs = 1725274680000 (milliseconds)
  ;; This is 2026-09-02T18:18:00 UTC
  ;; Mock current-time to be exactly 120 seconds (2 minutes) later
  (let* ((fetched-ms 1725274680000)
         (fetched-time (/ fetched-ms 1000.0))
         ;; current-time 120 seconds later
         (current-time (seconds-to-time (+ fetched-time 120)))
         (output (claude-usage--format-age fetched-ms current-time)))
    ;; Output should be "2m ago"
    (should (string-equal output "2m ago"))))

(ert-deftest claude-usage-test-severity-face-normal ()
  "Severity \"normal\" maps to success face."
  (should (eq (claude-usage--severity-face "normal" 50) 'success)))

(ert-deftest claude-usage-test-severity-face-warning ()
  "Severity \"warning\" maps to warning face."
  (should (eq (claude-usage--severity-face "warning" 75) 'warning)))

(ert-deftest claude-usage-test-severity-face-critical ()
  "Severity \"critical\" maps to error face."
  (should (eq (claude-usage--severity-face "critical" 95) 'error)))

(ert-deftest claude-usage-test-severity-face-percent-fallback-warning ()
  "Fallback to percent: 75% maps to warning face."
  (should (eq (claude-usage--severity-face nil 75) 'warning)))

(ert-deftest claude-usage-test-severity-face-percent-fallback-critical ()
  "Fallback to percent: 95% maps to error face."
  (should (eq (claude-usage--severity-face nil 95) 'error)))

(ert-deftest claude-usage-test-bar-full ()
  "Bar with 100% percent generates all full blocks."
  (let ((bar (claude-usage--bar 100 10)))
    (should (string-equal bar "██████████"))))

(ert-deftest claude-usage-test-bar-half ()
  "Bar with 50% percent generates half-filled bar."
  (let ((bar (claude-usage--bar 50 10)))
    (should (string-equal bar "█████░░░░░"))))

(ert-deftest claude-usage-test-bar-empty ()
  "Bar with 0% percent generates all empty blocks."
  (let ((bar (claude-usage--bar 0 10)))
    (should (string-equal bar "░░░░░░░░░░"))))

(ert-deftest claude-usage-test-stale-p-fresh ()
  "Fresh data (0 seconds old) is not stale."
  (let ((now-ms (* (float-time (current-time)) 1000)))
    (should (null (claude-usage-stale-p now-ms)))))

(ert-deftest claude-usage-test-stale-p-old ()
  "Very old data is stale."
  (let ((old-ms (- (* (float-time (current-time)) 1000) 
                   ;; 2 hours in milliseconds
                   (* 2 3600 1000))))
    (should (claude-usage-stale-p old-ms))))

(provide 'claude-usage-test)

;;; claude-usage-test.el ends here
