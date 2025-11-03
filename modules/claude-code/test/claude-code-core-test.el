;;; claude-code-core-test.el --- Tests for core commands -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for claude-code-core.el

;;; Code:

(require 'buttercup)
(require 'test-helper)

(describe "claude-code-core"

  (describe "Module loading"
    (it "defines all expected commands"
      (expect 'claude-code-ask :to-have-function)
      (expect 'claude-code-open-buffer :to-have-function)
      (expect 'claude-code-clear-buffer :to-have-function)
      (expect 'claude-code-show-processes :to-have-function))

    (it "provides backward compatibility alias"
      (expect 'claude-code-quick-ask :to-have-function)
      (expect (symbol-function 'claude-code-quick-ask)
              :to-be (symbol-function 'claude-code-ask))))

  (describe "claude-code-ask"

    (after-each
      (claude-code-test-teardown))

    (it "creates a process and buffer"
      (claude-code-test-spy-on-make-process)

      ;; Mock projectile-project-root
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (claude-code-ask "Test prompt")

      (expect (claude-code-process-get "/tmp/test/") :to-be-truthy)
      (expect (get-buffer "*claude-code: test*") :to-be-truthy))

    (it "starts interaction in buffer"
      (claude-code-test-spy-on-make-process)
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (claude-code-ask "My question")

      (let ((buffer (get-buffer "*claude-code: test*")))
        (expect buffer :to-match-buffer "My question")))

    (it "sets up response callback"
      (claude-code-test-spy-on-make-process)
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (claude-code-ask "Test")

      (let ((proc-obj (claude-code-process-get "/tmp/test/")))
        (expect (claude-code-process-response-callbacks proc-obj)
                :to-be-truthy)))

    (it "sets up error callback"
      (claude-code-test-spy-on-make-process)
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (claude-code-ask "Test")

      (let ((proc-obj (claude-code-process-get "/tmp/test/")))
        (expect (claude-code-process-error-callbacks proc-obj)
                :to-be-truthy))))

  (describe "claude-code-open-buffer"

    (after-each
      (claude-code-test-teardown))

    (it "opens buffer for current project"
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")
      (spy-on 'pop-to-buffer)

      (claude-code-open-buffer)

      (expect 'pop-to-buffer :to-have-been-called)
      (let ((args (spy-calls-args-for 'pop-to-buffer 0)))
        (expect (buffer-name (car args)) :to-equal "*claude-code: test*"))))

  (describe "claude-code-clear-buffer"

    (after-each
      (claude-code-test-teardown))

    (it "clears existing buffer"
      (spy-on 'projectile-project-root :and-return-value "/tmp/test/")

      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test")

        (claude-code-clear-buffer)

        (with-current-buffer buffer
          (expect (buffer-string) :to-equal ""))))

    (it "does nothing if buffer doesn't exist"
      (spy-on 'projectile-project-root :and-return-value "/tmp/nonexistent/")

      ;; Should not error
      (expect (claude-code-clear-buffer) :not :to-throw)))

  (describe "claude-code-show-processes"

    (after-each
      (claude-code-test-teardown))

    (it "creates processes buffer"
      (claude-code-show-processes)

      (expect (get-buffer "*claude-code-processes*") :to-be-truthy))

    (it "lists active processes"
      (claude-code-test-spy-on-make-process)

      (claude-code-process-start "/tmp/test1/")
      (claude-code-process-start "/tmp/test2/")

      (claude-code-show-processes)

      (let ((buffer (get-buffer "*claude-code-processes*")))
        (expect buffer :to-match-buffer "/tmp/test1/")
        (expect buffer :to-match-buffer "/tmp/test2/")))

    (it "shows status information"
      (claude-code-test-spy-on-make-process)

      (claude-code-process-start "/tmp/test/")

      (claude-code-show-processes)

      (let ((buffer (get-buffer "*claude-code-processes*")))
        (expect buffer :to-match-buffer "Status:")
        (expect buffer :to-match-buffer "Alive:")
        (expect buffer :to-match-buffer "Model:")))

    (it "handles no processes gracefully"
      (claude-code-show-processes)

      (let ((buffer (get-buffer "*claude-code-processes*")))
        (expect buffer :to-match-buffer "No active processes")))))

(provide 'claude-code-core-test)
;;; claude-code-core-test.el ends here
