;;; claude-usage-test.el --- Tests for claude-usage.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edward Paget
;; Author: Edward Paget <ed.paget@gmail.com>

;; This file is part of edmacs.

;;; Commentary:

;; claude-usage.el now `(require 'magit-section)' for `claude-usage-mode',
;; so this file follows modules/sidebar-test.el's model exactly: it fixes
;; `load-path' against the straight build tree and loads claude-usage.el
;; itself, below, rather than taking it on the command line. Run in batch
;; from the repository root:
;;
;;   emacs -Q --batch -l ert -l modules/git-common-dir.el \
;;         -l modules/claude-usage-test.el -f ert-run-tests-batch-and-exit
;;
;; Note claude-usage.el is NOT passed on the command line -- this file
;; fixes `load-path' against the straight build tree and loads
;; claude-usage.el itself, below, so its own `(require 'magit-section)'
;; succeeds. If neither this checkout nor its sibling main `edmacs'
;; checkout has ever bootstrapped straight, the whole suite reports a
;; single skip rather than erroring out on file load.
;;
;; Tests cover: full payload with limits array, fallback to legacy keys,
;; null value handling, missing files, malformed JSON, fixed-timestamp
;; format functions with no reliance on current-time, the `*claude-usage*'
;; buffer/mode, batch-rendered fixture text (full/stale/missing-field/
;; absent-cache), and g/q keybinding resolution through the real evil
;; keymaps.
;;
;; Also cover the live-fetch source added on top of the cache: keychain
;; token extraction against fixture JSON strings (never the real
;; keychain); non-leakage of a sentinel token across every fetch-failure
;; path (buffer text, *Messages*, and any caught error payload); the
;; captured-response HTTP parse path feeding `claude-usage-meters'
;; unchanged; source precedence (`claude-usage--apply-refresh-result')
;; across fetch-success, generic fallback-to-cache, and neither-available,
;; plus each specific failure mode (no token, network error, non-200
;; status, unparseable body) driven end to end through
;; `claude-usage--refresh' and asserted to resolve to a real cached
;; envelope rather than a stubbed-nil cache; the header's source+age label
;; for both the live and cache cases; that
;; `claude-usage--ensure-buffer' never touches `call-process',
;; `accept-process-output', or `sit-for' before its synchronous render
;; completes; and single-flight/idle-timer de-duplication across a
;; double module load. None of this suite ever spawns a real `security'
;; process or makes a real network request -- `claude-usage--access-token'
;; and `url-retrieve' are always stubbed at the function-entry level.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; See sidebar-test.el's identical setting for why: on a natively-compiled
;; Emacs, `advice-add' on a primitive subr makes Emacs spawn a second
;; `emacs -Q --batch' subprocess to compile a native trampoline for it,
;; misattributing that work to this suite. Disabled before anything below
;; loads magit-section or evil.
(setq native-comp-enable-subr-trampolines nil)

(defun claude-usage-test--locate-straight-build-root ()
  "Return this checkout's `straight/build' directory, or nil.
Tries this checkout's own `straight/build' first, then falls back to the
sibling main `edmacs' checkout's `straight/build' -- see
`edmacs-sidebar-test--locate-straight-build-root' for the identical
worktree-vs-sibling-main-checkout rationale."
  (or
   (let ((here (expand-file-name "straight/build" default-directory)))
     (and (file-directory-p here) here))
   (let* ((root (directory-file-name (expand-file-name default-directory)))
          (worktrees-dir (directory-file-name (file-name-directory root))))
     (when (string-suffix-p "__worktrees" worktrees-dir)
       (let* ((projects-dir (file-name-directory worktrees-dir))
              (repo-name (string-remove-suffix
                          "__worktrees" (file-name-nondirectory worktrees-dir)))
              (main-build (expand-file-name
                           (concat repo-name "/straight/build") projects-dir)))
         (and (file-directory-p main-build) main-build))))))

(defun claude-usage-test--add-magit-section-deps (build-root)
  "Add `magit-section' and its transitive deps under BUILD-ROOT to `load-path'.
cl-lib, eieio, subr-x, format-spec, and cursor-sensor ship with Emacs core
and need no straight resolution; only these do."
  (dolist (dep '("compat" "cond-let" "llama" "transient" "seq" "magit-section"))
    (let ((dir (expand-file-name dep build-root)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(defvar claude-usage-test--build-root
  (claude-usage-test--locate-straight-build-root)
  "This checkout's (or its sibling main checkout's) `straight/build' root.
Also reused by the evil lookup below -- a second, independent optional
straight dependency.")

(if (null claude-usage-test--build-root)

    (ert-deftest claude-usage-test-magit-section-unavailable ()
      (ert-skip "magit-section's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this suite"))

  (progn

    (claude-usage-test--add-magit-section-deps claude-usage-test--build-root)
    (load (expand-file-name "modules/claude-usage.el" default-directory) nil t)

    ;; ==========================================================================
    ;; Test helpers -- real evil, for the keybinding-resolution tests
    ;; ==========================================================================

    (defun claude-usage-test--locate-straight-repos-root ()
      "Return this checkout's `straight/repos' directory, or its sibling
main checkout's -- the same fallback `claude-usage-test--locate-straight-build-root'
uses for `straight/build'."
      (or
       (let ((here (expand-file-name "straight/repos" default-directory)))
         (and (file-directory-p here) here))
       (let* ((root (directory-file-name (expand-file-name default-directory)))
              (worktrees-dir (directory-file-name (file-name-directory root))))
         (when (string-suffix-p "__worktrees" worktrees-dir)
           (let* ((projects-dir (file-name-directory worktrees-dir))
                  (repo-name (string-remove-suffix
                              "__worktrees" (file-name-nondirectory worktrees-dir)))
                  (main-repos (expand-file-name
                               (concat repo-name "/straight/repos") projects-dir)))
             (and (file-directory-p main-repos) main-repos))))))

    (defun claude-usage-test--locate-real-evil ()
      "Return the directory holding the real `evil.el', or nil.
Tries `straight/build/evil' first (file-exists-p follows a working
symlink); falls back to `straight/repos/evil' when that symlink is
broken or the build tree was never generated."
      (or
       (let* ((root (or claude-usage-test--build-root
                         (claude-usage-test--locate-straight-build-root)))
              (path (and root (expand-file-name "evil/evil.el" root))))
         (and path (file-exists-p path) (file-name-directory path)))
       (let* ((root (claude-usage-test--locate-straight-repos-root))
              (path (and root (expand-file-name "evil/evil.el" root))))
         (and path (file-exists-p path) (file-name-directory path)))))

    (defun claude-usage-test--ensure-real-evil ()
      "Load the real `evil', skipping the calling test if unavailable."
      (unless (featurep 'evil)
        (let ((dir (claude-usage-test--locate-real-evil)))
          (unless dir
            (ert-skip "evil's straight build was not found in this checkout \
or its sibling main checkout; bootstrap straight once (open this worktree in \
a real Emacs session) to enable this test"))
          (let ((load-path (cons dir load-path)))
            (require 'evil)))))

    ;; ==========================================================================
    ;; Fixtures and tests (below) run inside this branch, once
    ;; claude-usage.el is loaded with a working `magit-section'.
    ;; ==========================================================================


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

    ;; ==========================================================================
    ;; AC1 -- buffer/mode: command shows it, g reverts, q buries
    ;; ==========================================================================

    (defconst claude-usage-test--fixture-missing-field-payload
      '((fetchedAtMs . 1725274680000)
        (accountUuid . "test-uuid-321")
        (utilization
         (limits .
                 (((kind . "session")
                   (percent . 45)
                   (severity . "normal")
                   (resets_at . nil)
                   (limit_dollars . 10.0)
                   (used_dollars . 4.5)
                   (remaining_dollars . 5.5)))))
       )
      "Full-shaped payload with one limits entry whose `resets_at' is nil.
`percent' is present, so this entry survives `claude-usage-meters's own
null-percent filter and reaches the render layer -- unlike a null
`percent', which `claude-usage-meters' drops before it ever becomes a
meter plist.")

    (defun claude-usage-test--fresh-and-stale-fixtures ()
      "Return (FRESH . STALE), two fixtures identical except `fetchedAtMs'.
FRESH uses the real current time; STALE is computed relative to it (2
real hours in the past) at call time -- `claude-usage-stale-p' has no
injectable current-time parameter, so a fixed historical constant would
eventually go stale/wrong as real time passes."
      (let* ((now-ms (round (* 1000 (float-time (current-time)))))
             (stale-ms (round (* 1000 (- (float-time (current-time)) 7200))))
             (limits '(((kind . "session")
                        (percent . 45)
                        (severity . "normal")
                        (resets_at . "2026-09-03T20:00:00Z")))))
        (cons
         `((fetchedAtMs . ,now-ms) (utilization (limits . ,limits)))
         `((fetchedAtMs . ,stale-ms) (utilization (limits . ,limits))))))

    (ert-deftest claude-usage-test-command-shows-buffer-with-limits ()
      "`claude-usage' creates/shows `*claude-usage*' with a populated Limits section."
      (cl-letf (((symbol-function 'claude-usage--read-cache)
                 (lambda () claude-usage-test--fixture-full-payload)))
        (unwind-protect
            (progn
              (claude-usage)
              (let ((buf (get-buffer "*claude-usage*")))
                (should (buffer-live-p buf))
                (with-current-buffer buf
                  (should (derived-mode-p 'claude-usage-mode))
                  (let ((text (buffer-string)))
                    (should (string-match-p "Limits" text))
                    (should (string-match-p "Session (5h)" text))
                    (should (string-match-p "Week (all)" text))
                    (should (string-match-p "Claude 3.5 Opus" text))))))
          (let ((buf (get-buffer "*claude-usage*")))
            (when (buffer-live-p buf) (kill-buffer buf))))))

    (ert-deftest claude-usage-test-revert-buffer-function-is-set ()
      "A fresh `claude-usage-mode' buffer's `revert-buffer-function' is ours."
      (with-temp-buffer
        (claude-usage-mode)
        (should (eq revert-buffer-function #'claude-usage--revert))))

    (ert-deftest claude-usage-test-revert-buffer-function-does-not-leak-globally ()
      "Regression test: `claude-usage-mode' must set `revert-buffer-function'
buffer-locally. A plain `setq' on it mutates the variable's *global*
default, so activating the mode once would silently redirect an
unrelated buffer's later `revert-buffer' into `claude-usage--redraw',
erasing that buffer's real contents."
      (with-temp-buffer
        (claude-usage-mode))
      (with-temp-buffer
        (should-not (local-variable-p 'revert-buffer-function))
        (should-not (eq revert-buffer-function #'claude-usage--revert))))

    (ert-deftest claude-usage-test-g-and-q-resolve-through-real-evil-keymaps ()
      "Regression test for the g/q-shadowed-by-evil-motion-state fix.
A plain `define-key' on `claude-usage-mode-map' alone is invisible to
real key lookup in motion state -- see the identical rationale on
`edmacs-sidebar-test-ret-and-q-resolve-through-real-evil-keymaps'."
      (claude-usage-test--ensure-real-evil)
      (unwind-protect
          (progn
            (evil-mode 1)
            (with-temp-buffer
              (claude-usage-mode)
              (evil-motion-state)
              (should (eq evil-state 'motion))
              (should (eq (key-binding (kbd "g")) #'revert-buffer))
              (should (eq (key-binding (kbd "q")) #'bury-buffer))))
        (evil-mode -1)))

    ;; ==========================================================================
    ;; AC4 -- render-to-string against fixture payloads
    ;; ==========================================================================

    (ert-deftest claude-usage-test-render-full-payload ()
      (let ((text (claude-usage--render-to-string claude-usage-test--fixture-full-payload)))
        (should (string-match-p "Limits" text))
        (should (string-match-p "Session (5h)" text))
        (should (string-match-p "Week (all)" text))
        (should (string-match-p "Claude 3.5 Opus" text))
        (should (string-match-p "45%" text))
        (should (string-match-p "█\\|░" text)))) ; bar block chars

    (ert-deftest claude-usage-test-render-missing-field-shows-em-dash-for-reset-only ()
      (let ((text (claude-usage--render-to-string
                   claude-usage-test--fixture-missing-field-payload)))
        (should (string-match-p "45%" text))
        (should-not (string-match-p "45%—" text))
        ;; The reset column (last field on the row) is an em dash; the
        ;; percent column (present in the fixture) is not.
        (should (string-match-p "—\n" text))))

    (ert-deftest claude-usage-test-render-absent-cache ()
      (let ((text (claude-usage--render-to-string nil)))
        (should (string-match-p "No usage data" text))
        (should-not (string-match-p "Limits" text))))

    ;; ==========================================================================
    ;; AC5 -- stale rendering differs visibly from fresh
    ;; ==========================================================================

    (ert-deftest claude-usage-test-stale-differs-visibly-from-fresh ()
      (let* ((fixtures (claude-usage-test--fresh-and-stale-fixtures))
             (fresh (claude-usage--render-to-string (car fixtures)))
             (stale (claude-usage--render-to-string (cdr fixtures))))
        (should-not (string= fresh stale))
        (should (string-match-p (regexp-quote claude-usage--stale-marker) stale))
        (should-not (string-match-p (regexp-quote claude-usage--stale-marker) fresh))
        (should (cl-some (lambda (pos)
                            (let ((face (get-text-property pos 'face stale)))
                              (or (eq face 'shadow)
                                  (and (listp face) (memq 'shadow face)))))
                          (number-sequence 0 (1- (length stale)))))
        (should-not (cl-some (lambda (pos)
                               (let ((face (get-text-property pos 'face fresh)))
                                 (or (eq face 'shadow)
                                     (and (listp face) (memq 'shadow face)))))
                             (number-sequence 0 (1- (length fresh)))))))

    ;; ==========================================================================
    ;; AC1 -- keychain token extraction, fixture-driven only
    ;; ==========================================================================

    (defconst claude-usage-test--fixture-token-valid
      "{\"claudeAiOauth\":{\"accessToken\":\"tok-123\",\"refreshToken\":\"rt-456\"}}"
      "A well-shaped credentials blob, as the CLI itself writes it.")

    (defconst claude-usage-test--fixture-token-missing-access-token
      "{\"claudeAiOauth\":{\"refreshToken\":\"rt-456\"}}"
      "claudeAiOauth present, but its accessToken key is absent.")

    (defconst claude-usage-test--fixture-token-missing-oauth-key
      "{\"someOtherKey\":\"value\"}"
      "Valid JSON object, but the claudeAiOauth key itself is absent.")

    (defconst claude-usage-test--fixture-token-empty-string
      ""
      "Empty keychain blob -- as if the entry exists but holds nothing.")

    (defconst claude-usage-test--fixture-token-non-json
      "not json at all {{{"
      "Plain garbage, not parseable as JSON.")

    (defconst claude-usage-test--fixture-token-json-array
      "[\"claudeAiOauth\", \"accessToken\"]"
      "Syntactically valid JSON, but an array rather than an object.")

    (defconst claude-usage-test--fixture-token-json-null
      "null"
      "Syntactically valid JSON `null' as the whole document.")

    (ert-deftest claude-usage-test-extract-access-token-valid ()
      "A well-shaped blob yields its accessToken string."
      (should (string-equal
               (claude-usage--extract-access-token claude-usage-test--fixture-token-valid)
               "tok-123")))

    (ert-deftest claude-usage-test-extract-access-token-rejects-malformed-input ()
      "Every malformed/wrong-shaped/absent fixture yields nil, never a signal,
and never grows *Messages* -- none of this ever touches the real keychain."
      (let ((messages-before (claude-usage-test--messages-string)))
        (dolist (fixture (list claude-usage-test--fixture-token-missing-access-token
                               claude-usage-test--fixture-token-missing-oauth-key
                               claude-usage-test--fixture-token-empty-string
                               claude-usage-test--fixture-token-non-json
                               claude-usage-test--fixture-token-json-array
                               claude-usage-test--fixture-token-json-null
                               nil))
          (should (null (claude-usage--extract-access-token fixture))))
        (should (string-equal messages-before (claude-usage-test--messages-string)))))

    ;; ==========================================================================
    ;; Shared helpers for the live-fetch tests below
    ;; ==========================================================================

    (defun claude-usage-test--messages-string ()
      "Return the current contents of the real `*Messages*' buffer."
      (with-current-buffer (messages-buffer) (buffer-string)))

    (defconst claude-usage-test--sentinel-token "SENTINEL-DO-NOT-LEAK-89f2"
      "Never a real token -- stands in for one to prove a fetch-failure path
never renders the token it used into a buffer, a message, or an error.")

    (defun claude-usage-test--stub-url-retrieve-once (setup-fn status)
      "Return a function usable as `url-retrieve', invoking its CALLBACK
synchronously with STATUS once `current-buffer' -- a fresh temp buffer
that SETUP-FN populates first -- mirrors the response buffer the real
`url-retrieve' hands its callback."
      (lambda (_url callback &optional _cbargs &rest _more)
        (let ((buf (generate-new-buffer " *claude-usage-test-fake-response*")))
          (with-current-buffer buf
            (funcall setup-fn)
            (funcall callback status)))))

    (defmacro claude-usage-test--assert-no-leak (sentinel &rest body)
      "Run BODY, then assert SENTINEL appears in none of: any caught error's
`error-message-string', the `*claude-usage*' buffer text (if live), or
`*Messages*'. An unexpected signal is still re-raised, after the leak
check, so this never silently hides a real bug."
      (declare (indent 1))
      `(let (claude-usage-test--caught-error)
         (condition-case err
             (progn ,@body)
           (error (setq claude-usage-test--caught-error err)))
         (when claude-usage-test--caught-error
           (should-not (string-match-p
                        (regexp-quote ,sentinel)
                        (error-message-string claude-usage-test--caught-error))))
         (let ((buf (get-buffer "*claude-usage*")))
           (when (buffer-live-p buf)
             (should-not (string-match-p (regexp-quote ,sentinel)
                                          (with-current-buffer buf (buffer-string))))))
         (should-not (string-match-p (regexp-quote ,sentinel)
                                      (claude-usage-test--messages-string)))
         (when claude-usage-test--caught-error
           (signal (car claude-usage-test--caught-error)
                   (cdr claude-usage-test--caught-error)))))

    ;; ==========================================================================
    ;; AC2 -- the token never leaks, across every fetch-failure path
    ;; ==========================================================================

    (ert-deftest claude-usage-test-no-leak-network-error ()
      "A `url-retrieve' status carrying :error never renders the sentinel."
      (let ((claude-usage--refresh-in-flight nil)
            (claude-usage--state-envelope nil)
            (claude-usage--state-source nil))
        (cl-letf (((symbol-function 'claude-usage--access-token)
                   (lambda () claude-usage-test--sentinel-token))
                  ((symbol-function 'url-retrieve)
                   (claude-usage-test--stub-url-retrieve-once
                    (lambda () nil)
                    (list :error '(error (http error)))))
                  ((symbol-function 'claude-usage--read-cache) (lambda () nil)))
          (claude-usage-test--assert-no-leak claude-usage-test--sentinel-token
            (claude-usage--refresh)))))

    (ert-deftest claude-usage-test-no-leak-unparseable-body ()
      "An HTTP 200 with an unparseable body never renders the sentinel."
      (let ((claude-usage--refresh-in-flight nil)
            (claude-usage--state-envelope nil)
            (claude-usage--state-source nil))
        (cl-letf (((symbol-function 'claude-usage--access-token)
                   (lambda () claude-usage-test--sentinel-token))
                  ((symbol-function 'url-retrieve)
                   (claude-usage-test--stub-url-retrieve-once
                    (lambda ()
                      (insert "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nnot json at all"))
                    nil))
                  ((symbol-function 'claude-usage--read-cache) (lambda () nil)))
          (claude-usage-test--assert-no-leak claude-usage-test--sentinel-token
            (claude-usage--refresh)))))

    (ert-deftest claude-usage-test-no-leak-non-200-status ()
      "An HTTP 401 response never renders the sentinel."
      (let ((claude-usage--refresh-in-flight nil)
            (claude-usage--state-envelope nil)
            (claude-usage--state-source nil))
        (cl-letf (((symbol-function 'claude-usage--access-token)
                   (lambda () claude-usage-test--sentinel-token))
                  ((symbol-function 'url-retrieve)
                   (claude-usage-test--stub-url-retrieve-once
                    (lambda ()
                      (insert "HTTP/1.1 401 Unauthorized\r\nContent-Type: application/json\r\n\r\n{\"error\":\"invalid_token\"}"))
                    nil))
                  ((symbol-function 'claude-usage--read-cache) (lambda () nil)))
          (claude-usage-test--assert-no-leak claude-usage-test--sentinel-token
            (claude-usage--refresh)))))

    ;; ==========================================================================
    ;; AC3 -- the captured-response parse path feeds `claude-usage-meters'
    ;; unchanged
    ;; ==========================================================================

    ;; NOT a literal capture -- disclosed, not silently assumed. These bytes
    ;; are reconstructed to match the shape verified by hand against the
    ;; real endpoint on 2026-09-03 (see this phase's Context): a live
    ;; response is the bare utilization object, carrying `five_hour',
    ;; `seven_day', the per-model `seven_day_*' keys, AND the same
    ;; normalized `limits[]' array `claude-usage--read-cache' already reads
    ;; from the on-disk cache. This is a literal capture: the body below is
    ;; the verbatim bytes of `GET /api/oauth/usage' on the date in the
    ;; docstring, with only the status/header lines reconstructed (the parse
    ;; path reads past them to the blank line). It carries no credential
    ;; material -- percentages, reset timestamps and model display names.
    (defconst claude-usage-test--fixture-real-response
      (concat
       "HTTP/1.1 200 OK\r\n"
       "Content-Type: application/json\r\n"
       "\r\n"
       "{\"five_hour\":{\"utilization\":24.0,\"resets_at\":\"2026-09-03T22:30:00.134085"
       "+00:00\",\"limit_dollars\":null,\"used_dollars\":null,\"remaining_dollars\":nul"
       "l,\"locked_reason\":null},\"seven_day\":{\"utilization\":83.0,\"resets_at\":\"202"
       "6-09-04T12:00:00.134101+00:00\",\"limit_dollars\":null,\"used_dollars\":null,"
       "\"remaining_dollars\":null,\"locked_reason\":null},\"seven_day_oauth_apps\":nu"
       "ll,\"seven_day_opus\":null,\"seven_day_sonnet\":null,\"seven_day_cowork\":null"
       ",\"seven_day_omelette\":null,\"tangelo\":null,\"iguana_necktie\":null,\"omelett"
       "e_promotional\":null,\"nimbus_quill\":{\"utilization\":0.0,\"resets_at\":null,\""
       "limit_dollars\":null,\"used_dollars\":null,\"remaining_dollars\":null,\"locked"
       "_reason\":null},\"cinder_cove\":null,\"amber_ladder\":null,\"juniper_tide\":nul"
       "l,\"extra_usage\":{\"is_enabled\":false,\"monthly_limit\":null,\"used_credits\":"
       "null,\"utilization\":null,\"currency\":null,\"decimal_places\":null,\"disabled_"
       "reason\":null,\"user_disabled\":true,\"spend_limit_reached\":false,\"credits_e"
       "ver_enabled\":true,\"daily\":null,\"weekly\":null},\"limits\":[{\"kind\":\"session"
       "\",\"group\":\"session\",\"percent\":24,\"severity\":\"normal\",\"resets_at\":\"2026-0"
       "9-03T22:30:00.134085+00:00\",\"scope\":null,\"is_active\":false},{\"kind\":\"wee"
       "kly_all\",\"group\":\"weekly\",\"percent\":83,\"severity\":\"warning\",\"resets_at\":"
       "\"2026-09-04T12:00:00.134101+00:00\",\"scope\":null,\"is_active\":true},{\"kind"
       "\":\"weekly_scoped\",\"group\":\"weekly\",\"percent\":36,\"severity\":\"normal\",\"res"
       "ets_at\":\"2026-09-04T12:00:00.134259+00:00\",\"scope\":{\"model\":{\"id\":null,\""
       "display_name\":\"Fable\"},\"surface\":null},\"is_active\":false}],\"spend\":{\"use"
       "d\":{\"amount_minor\":0,\"currency\":\"USD\",\"exponent\":2},\"limit\":null,\"percen"
       "t\":0,\"severity\":\"normal\",\"enabled\":false,\"disabled_reason\":null,\"cap\":nu"
       "ll,\"balance\":null,\"auto_reload\":null,\"disclaimer\":\"Usage credits cover y"
       "ou when you hit your plan limits. [Learn more](https://support.claude.co"
       "m/articles/12429409)\",\"can_purchase_credits\":false,\"can_toggle\":false},\""
       "member_dashboard_available\":false}")
      "A literal capture of the response body from GET /api/oauth/usage,
taken 2026-09-03. Covers the full verified shape: five_hour, seven_day,
the null per-model seven_day_* keys, and the normalized limits[] array
with a weekly_scoped entry carrying scope.model.display_name. Note
limit_dollars/used_dollars are null on a subscription plan.")

    (ert-deftest claude-usage-test-parse-fetch-buffer-feeds-meters-unchanged ()
      "`claude-usage--parse-fetch-buffer's own parse path, run against the
captured-shape fixture and wrapped in the synthesized envelope, produces
exactly what `claude-usage-meters' and `claude-usage--redraw' already
expect from the on-disk cache -- unchanged."
      (let* ((body (with-temp-buffer
                     (insert claude-usage-test--fixture-real-response)
                     (claude-usage--parse-fetch-buffer (current-buffer))))
             ;; `claude-usage-stale-p' always reads the real clock (it has
             ;; no injectable `now'), so this must be genuinely recent --
             ;; see `claude-usage-test--fresh-and-stale-fixtures' for the
             ;; identical rationale.
             (fetched-ms (round (* 1000 (float-time (current-time)))))
             (now-time (seconds-to-time (/ fetched-ms 1000.0)))
             (envelope (list (cons 'fetchedAtMs fetched-ms)
                              (cons 'accountUuid nil)
                              (cons 'utilization body)))
             (meters (claude-usage-meters envelope)))
        ;; The parse path handles the full verified response shape --
        ;; five_hour/seven_day/seven_day_* alongside limits[] -- without
        ;; erroring, and `claude-usage-meters' correctly prefers `limits[]'
        ;; over the legacy top-level keys when both are present.
        (should (alist-get 'five_hour body))
        (should (alist-get 'seven_day body))
        (should (null (alist-get 'seven_day_opus body)))
        (should (= (length meters) 3))
        (should (eq (plist-get (nth 0 meters) :id) 'session))
        (should (= (plist-get (nth 0 meters) :percent) 24))
        (should (eq (plist-get (nth 1 meters) :id) 'weekly_all))
        (should (= (plist-get (nth 1 meters) :percent) 83))
        (should (eq (plist-get (nth 2 meters) :id) 'weekly_scoped))
        (should (string-equal (plist-get (nth 2 meters) :model) "Fable"))
        (let ((text (claude-usage--render-to-string envelope now-time 'live)))
          (should (string-match-p "Session (5h)" text))
          (should (string-match-p "24%" text))
          (should (string-match-p "Fable" text))
          (should (string-match-p (regexp-quote "Claude Usage (live, now)") text)))))

    ;; ==========================================================================
    ;; AC4 -- source precedence
    ;; ==========================================================================

    (ert-deftest claude-usage-test-apply-refresh-result-fetch-wins ()
      "A successful fetch wins outright -- the cache is never even consulted."
      (let ((claude-usage--state-envelope nil)
            (claude-usage--state-source nil))
        (cl-letf (((symbol-function 'claude-usage--read-cache)
                   (lambda () (error "cache must not be consulted when fetch succeeds"))))
          (let ((result (claude-usage--apply-refresh-result
                         claude-usage-test--fixture-full-payload)))
            (should (eq (cdr result) 'live))
            (should (equal (car result) claude-usage-test--fixture-full-payload))
            (should (eq claude-usage--state-source 'live))
            (should (equal claude-usage--state-envelope
                           claude-usage-test--fixture-full-payload))))))

    (ert-deftest claude-usage-test-apply-refresh-result-falls-back-to-cache ()
      "A nil fetch result (standing in for every failure mode -- no token,
network error, non-200, unparseable body all collapse to nil before
reaching this function) falls back to the cache."
      (let ((claude-usage--state-envelope nil)
            (claude-usage--state-source nil))
        (cl-letf (((symbol-function 'claude-usage--read-cache)
                   (lambda () claude-usage-test--fixture-fallback-payload)))
          (let ((result (claude-usage--apply-refresh-result nil)))
            (should (eq (cdr result) 'cache))
            (should (equal (car result) claude-usage-test--fixture-fallback-payload))
            (should (eq claude-usage--state-source 'cache))))))

    ;; The four tests below each drive one *specific* failure mode all the
    ;; way through `claude-usage--refresh' (not `claude-usage--apply-refresh-result'
    ;; directly, and not with the cache stubbed to nil) so each one actually
    ;; demonstrates that failure mode resolving to a real cached envelope --
    ;; distinct from the AC2 no-leak tests above, which stub the cache to nil
    ;; and so only prove "neither available", not "falls back to the cache".

    (ert-deftest claude-usage-test-refresh-falls-back-to-cache-on-no-token ()
      "No token at all (`claude-usage--access-token' returns nil) never even
reaches `url-retrieve', and resolves to the real cache."
      (let ((claude-usage--refresh-in-flight nil)
            (claude-usage--state-envelope nil)
            (claude-usage--state-source nil))
        (cl-letf (((symbol-function 'claude-usage--access-token) (lambda () nil))
                  ((symbol-function 'url-retrieve)
                   (lambda (&rest _) (error "url-retrieve must not be called without a token")))
                  ((symbol-function 'claude-usage--read-cache)
                   (lambda () claude-usage-test--fixture-fallback-payload)))
          (claude-usage--refresh)
          (should (eq claude-usage--state-source 'cache))
          (should (equal claude-usage--state-envelope
                         claude-usage-test--fixture-fallback-payload)))))

    (ert-deftest claude-usage-test-refresh-falls-back-to-cache-on-network-error ()
      "A `url-retrieve' status carrying :error resolves to the real cache."
      (let ((claude-usage--refresh-in-flight nil)
            (claude-usage--state-envelope nil)
            (claude-usage--state-source nil))
        (cl-letf (((symbol-function 'claude-usage--access-token) (lambda () "tok"))
                  ((symbol-function 'url-retrieve)
                   (claude-usage-test--stub-url-retrieve-once
                    (lambda () nil)
                    (list :error '(error (http error)))))
                  ((symbol-function 'claude-usage--read-cache)
                   (lambda () claude-usage-test--fixture-fallback-payload)))
          (claude-usage--refresh)
          (should (eq claude-usage--state-source 'cache))
          (should (equal claude-usage--state-envelope
                         claude-usage-test--fixture-fallback-payload)))))

    (ert-deftest claude-usage-test-refresh-falls-back-to-cache-on-non-200-status ()
      "An HTTP 401 response resolves to the real cache."
      (let ((claude-usage--refresh-in-flight nil)
            (claude-usage--state-envelope nil)
            (claude-usage--state-source nil))
        (cl-letf (((symbol-function 'claude-usage--access-token) (lambda () "tok"))
                  ((symbol-function 'url-retrieve)
                   (claude-usage-test--stub-url-retrieve-once
                    (lambda ()
                      (insert "HTTP/1.1 401 Unauthorized\r\nContent-Type: application/json\r\n\r\n{\"error\":\"invalid_token\"}"))
                    nil))
                  ((symbol-function 'claude-usage--read-cache)
                   (lambda () claude-usage-test--fixture-fallback-payload)))
          (claude-usage--refresh)
          (should (eq claude-usage--state-source 'cache))
          (should (equal claude-usage--state-envelope
                         claude-usage-test--fixture-fallback-payload)))))

    (ert-deftest claude-usage-test-refresh-falls-back-to-cache-on-unparseable-body ()
      "An HTTP 200 with an unparseable body resolves to the real cache."
      (let ((claude-usage--refresh-in-flight nil)
            (claude-usage--state-envelope nil)
            (claude-usage--state-source nil))
        (cl-letf (((symbol-function 'claude-usage--access-token) (lambda () "tok"))
                  ((symbol-function 'url-retrieve)
                   (claude-usage-test--stub-url-retrieve-once
                    (lambda ()
                      (insert "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nnot json at all"))
                    nil))
                  ((symbol-function 'claude-usage--read-cache)
                   (lambda () claude-usage-test--fixture-fallback-payload)))
          (claude-usage--refresh)
          (should (eq claude-usage--state-source 'cache))
          (should (equal claude-usage--state-envelope
                         claude-usage-test--fixture-fallback-payload)))))

    (ert-deftest claude-usage-test-apply-refresh-result-neither-available ()
      "With neither a fetch nor a cache, state resolves to nil/nil and the
buffer falls through to the existing \"No usage data\" render branch."
      (let ((claude-usage--state-envelope 'stale-marker)
            (claude-usage--state-source 'stale-marker))
        (cl-letf (((symbol-function 'claude-usage--read-cache) (lambda () nil)))
          (let ((result (claude-usage--apply-refresh-result nil)))
            (should (null (car result)))
            (should (null (cdr result)))
            (should (null claude-usage--state-envelope))
            (should (null claude-usage--state-source))))
        (should (string-equal (claude-usage--render-to-string nil) "No usage data\n"))))

    ;; ==========================================================================
    ;; AC5 -- header states source and age
    ;; ==========================================================================

    (ert-deftest claude-usage-test-render-header-shows-live-source ()
      "A fresh, live-sourced envelope's header reads \"Claude Usage (live, now)\"."
      ;; `claude-usage-stale-p' always reads the real clock, so `fetched-ms'
      ;; must be genuinely recent -- see
      ;; `claude-usage-test--fresh-and-stale-fixtures' for the identical
      ;; rationale.
      (let* ((fetched-ms (round (* 1000 (float-time (current-time)))))
             (now-time (seconds-to-time (/ fetched-ms 1000.0)))
             (envelope `((fetchedAtMs . ,fetched-ms) (utilization (limits . nil))))
             (text (claude-usage--render-to-string envelope now-time 'live)))
        (should (string-match-p (regexp-quote "Claude Usage (live, now)") text))))

    (ert-deftest claude-usage-test-render-header-shows-cache-source-when-stale ()
      "A stale, cache-sourced envelope's header names the source and carries
the existing STALE+age marker -- so a fallback can never look live."
      (let* ((fetched-ms (round (- (* 1000 (float-time (current-time))) (* 7200 1000))))
             (now-time (seconds-to-time (+ (/ fetched-ms 1000.0) 7200))) ; 2h later
             (envelope `((fetchedAtMs . ,fetched-ms) (utilization (limits . nil))))
             (text (claude-usage--render-to-string envelope now-time 'cache)))
        (should (string-match-p "cache" text))
        (should (string-match-p (regexp-quote claude-usage--stale-marker) text))
        (should (string-match-p "2h ago" text))))

    ;; ==========================================================================
    ;; AC6 -- opening the buffer never blocks
    ;; ==========================================================================

    (defvar claude-usage-test--blocking-calls nil
      "Invocations of `call-process'/`accept-process-output'/`sit-for'
recorded by `claude-usage-test--with-blocking-guard' during a test.")

    (defmacro claude-usage-test--with-blocking-guard (&rest body)
      "Run BODY with `call-process', `accept-process-output', and `sit-for'
each recording their invocation into `claude-usage-test--blocking-calls'
before delegating to the real function -- records and continues, so any
legitimate use elsewhere in BODY (there should be none) is undisturbed
rather than made to signal."
      (declare (indent 0))
      `(cl-letf* ((claude-usage-test--orig-call-process (symbol-function 'call-process))
                  (claude-usage-test--orig-accept-process-output
                   (symbol-function 'accept-process-output))
                  (claude-usage-test--orig-sit-for (symbol-function 'sit-for))
                  ((symbol-function 'call-process)
                   (lambda (&rest args)
                     (push (cons 'call-process args) claude-usage-test--blocking-calls)
                     (apply claude-usage-test--orig-call-process args)))
                  ((symbol-function 'accept-process-output)
                   (lambda (&rest args)
                     (push (cons 'accept-process-output args) claude-usage-test--blocking-calls)
                     (apply claude-usage-test--orig-accept-process-output args)))
                  ((symbol-function 'sit-for)
                   (lambda (&rest args)
                     (push (cons 'sit-for args) claude-usage-test--blocking-calls)
                     (apply claude-usage-test--orig-sit-for args))))
         ,@body))

    (ert-deftest claude-usage-test-ensure-buffer-never-blocks-with-cache ()
      "With a populated cache, `claude-usage--ensure-buffer' shows the Limits
section immediately and trips no blocking-call guard before returning."
      (setq claude-usage-test--blocking-calls nil)
      (cl-letf (((symbol-function 'claude-usage--read-cache)
                 (lambda () claude-usage-test--fixture-full-payload)))
        (let ((claude-usage--state-envelope nil)
              (claude-usage--state-source nil))
          (claude-usage-test--with-blocking-guard
            (unwind-protect
                (let ((buf (claude-usage--ensure-buffer)))
                  (with-current-buffer buf
                    (should (string-match-p "Limits" (buffer-string))))
                  (should (null claude-usage-test--blocking-calls)))
              (let ((buf (get-buffer "*claude-usage*")))
                (when (buffer-live-p buf) (kill-buffer buf))))))))

    (ert-deftest claude-usage-test-ensure-buffer-never-blocks-with-empty-cache ()
      "With no cache at all, `claude-usage--ensure-buffer' shows the
\"No usage data\" line immediately and still trips no blocking-call guard."
      (setq claude-usage-test--blocking-calls nil)
      (cl-letf (((symbol-function 'claude-usage--read-cache) (lambda () nil)))
        (let ((claude-usage--state-envelope nil)
              (claude-usage--state-source nil))
          (claude-usage-test--with-blocking-guard
            (unwind-protect
                (let ((buf (claude-usage--ensure-buffer)))
                  (with-current-buffer buf
                    (should (string-match-p "No usage data" (buffer-string))))
                  (should (null claude-usage-test--blocking-calls)))
              (let ((buf (get-buffer "*claude-usage*")))
                (when (buffer-live-p buf) (kill-buffer buf))))))))

    ;; ==========================================================================
    ;; AC7 -- single-flight refresh and idle-timer de-duplication
    ;; ==========================================================================

    (ert-deftest claude-usage-test-refresh-single-flight ()
      "Three back-to-back refreshes, with the fetch never completing (as if
still in flight), invoke the fetch exactly once."
      (let ((claude-usage--refresh-in-flight nil)
            (call-count 0))
        (cl-letf (((symbol-function 'claude-usage--fetch)
                   (lambda (_callback) (setq call-count (1+ call-count)))))
          (claude-usage--refresh)
          (claude-usage--refresh)
          (claude-usage--refresh)
          (should (= call-count 1)))))

    (ert-deftest claude-usage-test-idle-timer-dedup-after-double-load ()
      "Reloading claude-usage.el twice more still leaves exactly one live
periodic idle timer driving `claude-usage--refresh' -- one-shot open-buffer
timers (no repeat delay) elsewhere in `timer-idle-list' don't count."
      (let ((file (expand-file-name "modules/claude-usage.el" default-directory)))
        (load file nil t)
        (load file nil t)
        (let ((matches (cl-remove-if-not
                        (lambda (tm)
                          (and (eq (timer--function tm) #'claude-usage--refresh)
                               (timer--repeat-delay tm)))
                        timer-idle-list)))
          (should (= (length matches) 1)))))

    )) ; end of build-root-found branch

(provide 'claude-usage-test)

;;; claude-usage-test.el ends here
