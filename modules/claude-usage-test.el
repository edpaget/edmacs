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

    )) ; end of build-root-found branch

(provide 'claude-usage-test)

;;; claude-usage-test.el ends here
