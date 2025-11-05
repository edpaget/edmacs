;;; claude-code-approval-test.el --- Tests for approval system -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for claude-code-approval.el
;; Note: This tests only the public API. Internal policy checking is tested
;; through integration with claude-code-process tests.

;;; Code:

(require 'buttercup)
(require 'test-helper)

(describe "claude-code-approval"

  (describe "Module loading"
    (it "defines all expected functions"
      (expect 'claude-code-approval-start-server :to-have-function)
      (expect 'claude-code-approval-stop-server :to-have-function)
      (expect 'claude-code-approval-clear-session-rules :to-have-function)
      (expect 'claude-code-approval-show-session-rules :to-have-function)))

  (describe "Approval modes"
    (it "defines all approval modes as valid symbols"
      (expect (member claude-code-approval-mode '(interactive auto-approve deny-all hybrid)) :to-be-truthy)))

  (describe "Session rules"
    (before-each
      (clrhash claude-code-approval-session-rules))

    (it "clears session rules"
      (puthash "test-key" 'allow claude-code-approval-session-rules)
      (expect (hash-table-empty-p claude-code-approval-session-rules) :to-be nil)
      (claude-code-approval-clear-session-rules)
      (expect (hash-table-empty-p claude-code-approval-session-rules) :to-be t)))

  (describe "Server lifecycle"
    (after-each
      (claude-code-test-teardown))

    (it "creates socket server"
      (let* ((project-root "/tmp/test-approval/")
             (socket-path (claude-code-approval-start-server project-root)))
        (expect socket-path :to-be-truthy)
        (expect (file-exists-p socket-path) :to-be t)
        ;; Cleanup
        (claude-code-approval-stop-server project-root)))

    (it "stores server info"
      (let* ((project-root "/tmp/test-approval/")
             (socket-path (claude-code-approval-start-server project-root))
             (server-info (gethash project-root claude-code-approval-servers)))
        (expect server-info :to-be-truthy)
        (expect (plist-get server-info :socket-path) :to-equal socket-path)
        (expect (plist-get server-info :server) :to-be-truthy)
        ;; Cleanup
        (claude-code-approval-stop-server project-root)))

    (it "cleans up socket on stop"
      (let* ((project-root "/tmp/test-approval/")
             (socket-path (claude-code-approval-start-server project-root)))
        (expect (file-exists-p socket-path) :to-be t)
        (claude-code-approval-stop-server project-root)
        (expect (file-exists-p socket-path) :to-be nil)
        (expect (gethash project-root claude-code-approval-servers) :to-be nil)))

    (it "handles multiple servers for different projects"
      (let* ((project1 "/tmp/test1/")
             (project2 "/tmp/test2/")
             (socket1 (claude-code-approval-start-server project1))
             (socket2 (claude-code-approval-start-server project2)))
        (expect socket1 :not :to-equal socket2)
        (expect (file-exists-p socket1) :to-be t)
        (expect (file-exists-p socket2) :to-be t)
        ;; Cleanup
        (claude-code-approval-stop-server project1)
        (claude-code-approval-stop-server project2))))

  ;; Note: Policy and hook response formatting are tested through integration
  ;; with claude-code-process tests, which initialize the approval server
  ;; and test the complete flow.

  (describe "Evil-mode integration"
    (it "sets initial state to motion when evil-mode is available"
      (with-temp-buffer
        ;; Mock evil-mode functions
        (let ((evil-state-called nil)
              (evil-state-arg nil))
          ;; Define mock function
          (cl-letf (((symbol-function 'evil-set-initial-state)
                     (lambda (mode state)
                       (setq evil-state-called t
                             evil-state-arg (list mode state)))))
            ;; Activate the mode
            (claude-code-approval-mode)
            ;; Check that evil integration was called with correct arguments
            (expect evil-state-called :to-be t)
            (expect (car evil-state-arg) :to-equal 'claude-code-approval-mode)
            (expect (cadr evil-state-arg) :to-equal 'motion)))))

    (it "defines keybindings that work in the approval buffer"
      (with-temp-buffer
        (claude-code-approval-mode)
        ;; Check that keybindings are properly defined
        (expect (lookup-key (current-local-map) (kbd "a"))
                :to-equal 'claude-code-approval--action-allow)
        (expect (lookup-key (current-local-map) (kbd "d"))
                :to-equal 'claude-code-approval--action-deny)
        (expect (lookup-key (current-local-map) (kbd "A"))
                :to-equal 'claude-code-approval--action-allow-always)
        (expect (lookup-key (current-local-map) (kbd "D"))
                :to-equal 'claude-code-approval--action-deny-always)
        (expect (lookup-key (current-local-map) (kbd "q"))
                :to-equal 'claude-code-approval--action-deny)
        (expect (lookup-key (current-local-map) (kbd "RET"))
                :to-equal 'claude-code-approval--action-allow))))
  )

(provide 'claude-code-approval-test)
;;; claude-code-approval-test.el ends here
