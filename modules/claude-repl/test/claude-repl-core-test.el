;;; claude-repl-core-test.el --- Tests for core commands -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for claude-repl-core.el

;;; Code:

(require 'buttercup)
(require 'test-helper)

(describe "claude-repl-core"

  (describe "Module loading"
    (it "defines all expected commands"
      (expect 'claude-repl-ask :to-have-function)
      (expect 'claude-repl-open-buffer :to-have-function)
      (expect 'claude-repl-clear-buffer :to-have-function)
      (expect 'claude-repl-show-processes :to-have-function))

    (it "provides backward compatibility alias"
      (expect 'claude-repl-quick-ask :to-have-function)))

  (describe "claude-repl-ask"

    (after-each
      (claude-repl-test-teardown))

    (it "creates a process and buffer"
      (claude-repl-test-spy-on-make-process)

      ;; Mock projectile-project-root
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (claude-repl-ask "Test prompt")

      (expect (claude-repl-process-get "/tmp/test/") :to-be-truthy)
      (expect (get-buffer "*claude-repl: test*") :to-be-truthy))

    (it "starts interaction in buffer"
      (claude-repl-test-spy-on-make-process)
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (claude-repl-ask "My question")

      (let ((buffer (get-buffer "*claude-repl: test*")))
        (expect buffer :to-match-buffer "My question")))

    (it "sets up response callback"
      (claude-repl-test-spy-on-make-process)
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (claude-repl-ask "Test")

      (let ((proc-obj (claude-repl-process-get "/tmp/test/")))
        (expect (claude-repl-process-response-callbacks proc-obj)
                :to-be-truthy)))

    (it "sets up error callback"
      (claude-repl-test-spy-on-make-process)
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (claude-repl-ask "Test")

      (let ((proc-obj (claude-repl-process-get "/tmp/test/")))
        (expect (claude-repl-process-error-callbacks proc-obj)
                :to-be-truthy))))

  (describe "claude-repl-open-buffer"

    (after-each
      (claude-repl-test-teardown))

    (it "opens buffer for current project"
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")
      (spy-on 'pop-to-buffer)

      (claude-repl-open-buffer)

      (expect 'pop-to-buffer :to-have-been-called)
      (let ((args (spy-calls-args-for 'pop-to-buffer 0)))
        (expect (buffer-name (car args)) :to-equal "*claude-repl: test*"))))

  (describe "claude-repl-clear-buffer"

    (after-each
      (claude-repl-test-teardown))

    (it "clears existing buffer"
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (let ((buffer (claude-repl-buffer-get-or-create "/tmp/test/")))
        (claude-repl-buffer-start-interaction buffer "Test")

        (claude-repl-clear-buffer)

        (with-current-buffer buffer
          ;; Buffer should have input prompt after clearing
          (expect (string-match-p "> " (buffer-string)) :to-be-truthy))))

    (it "does nothing if buffer doesn't exist"
      (spy-on 'projectile-project-root :and-return-value "/tmp/nonexistent/")

      ;; Should not error
      (expect (claude-repl-clear-buffer) :not :to-throw)))

  (describe "claude-repl-show-processes"

    (after-each
      (claude-repl-test-teardown))

    (it "creates processes buffer"
      (claude-repl-show-processes)

      (expect (get-buffer "*claude-repl-processes*") :to-be-truthy))

    (it "lists active processes"
      (claude-repl-test-spy-on-make-process)

      (claude-repl-process-start "/tmp/test1/")
      (claude-repl-process-start "/tmp/test2/")

      (claude-repl-show-processes)

      (let ((buffer (get-buffer "*claude-repl-processes*")))
        (expect buffer :to-match-buffer "/tmp/test1/")
        (expect buffer :to-match-buffer "/tmp/test2/")))

    (it "shows status information"
      (claude-repl-test-spy-on-make-process)

      (claude-repl-process-start "/tmp/test/")

      (claude-repl-show-processes)

      (let ((buffer (get-buffer "*claude-repl-processes*")))
        (expect buffer :to-match-buffer "Status:")
        (expect buffer :to-match-buffer "Alive:")
        (expect buffer :to-match-buffer "Model:")))

    (it "handles no processes gracefully"
      (claude-repl-show-processes)

      (let ((buffer (get-buffer "*claude-repl-processes*")))
        (expect buffer :to-match-buffer "No active processes")))))

(provide 'claude-repl-core-test)
;;; claude-repl-core-test.el ends here
