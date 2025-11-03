;;; claude-code-process-test.el --- Tests for process management -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for claude-code-process.el

;;; Code:

(require 'buttercup)
(require 'test-helper)

(describe "claude-code-process"

  (describe "Module loading"
    (it "defines all expected functions"
      (expect 'claude-code-process-start :to-have-function)
      (expect 'claude-code-process-get :to-have-function)
      (expect 'claude-code-process-get-or-create :to-have-function)
      (expect 'claude-code-process-send-prompt :to-have-function)
      (expect 'claude-code-process-kill :to-have-function)
      (expect 'claude-code-process-kill-all :to-have-function)
      (expect 'claude-code-process-add-response-callback :to-have-function)
      (expect 'claude-code-process-add-error-callback :to-have-function)
      (expect 'claude-code-process-alive-p :to-have-function)))

  (describe "Process creation"

    (after-each
      (claude-code-test-teardown))

    (it "creates a process struct with correct fields"
      (claude-code-test-spy-on-make-process)

      (let ((proc-obj (claude-code-process-start "/tmp/test-project/")))
        (expect (claude-code-process-p proc-obj) :to-be t)
        (expect (claude-code-process-project-root proc-obj) :to-equal "/tmp/test-project/")
        (expect (claude-code-process-status proc-obj) :to-equal 'running)
        (expect (claude-code-process-response-callbacks proc-obj) :to-equal nil)
        (expect (claude-code-process-error-callbacks proc-obj) :to-equal nil)))

    (it "stores process in hash table by project root"
      (claude-code-test-spy-on-make-process)

      (let* ((project-root "/tmp/test-project/")
             (proc-obj (claude-code-process-start project-root)))
        (expect (claude-code-process-get project-root) :to-be proc-obj)))

    (it "creates unique buffers for different projects"
      (claude-code-test-spy-on-make-process)

      (let ((proc1 (claude-code-process-start "/tmp/project1/"))
            (proc2 (claude-code-process-start "/tmp/project2/")))
        (expect (claude-code-process-buffer proc1)
                :not :to-be (claude-code-process-buffer proc2))))

    (it "passes correct arguments to make-process"
      (spy-on 'make-process :and-return-value (claude-code-test-create-mock-process))

      (claude-code-process-start "/tmp/test/" nil "opus")

      (expect 'make-process :to-have-been-called)
      (let ((args (spy-calls-args-for 'make-process 0)))
        (expect (plist-get args :command) :to-equal
                '("claude" "--print" "--verbose" "--output-format" "stream-json" "--model" "opus")))))

  (describe "Process retrieval"

    (after-each
      (claude-code-test-teardown))

    (it "returns nil for non-existent process"
      (expect (claude-code-process-get "/tmp/nonexistent/") :to-be nil))

    (it "retrieves existing process"
      (claude-code-test-spy-on-make-process)

      (let* ((project-root "/tmp/test/")
             (proc-obj (claude-code-process-start project-root)))
        (expect (claude-code-process-get project-root) :to-be proc-obj)))

    (it "get-or-create returns existing process"
      (claude-code-test-spy-on-make-process)

      (let* ((project-root "/tmp/test/")
             (proc1 (claude-code-process-start project-root))
             (proc2 (claude-code-process-get-or-create project-root)))
        (expect proc1 :to-be proc2)))

    (it "get-or-create creates new process if none exists"
      (claude-code-test-spy-on-make-process)

      (let* ((project-root "/tmp/test/")
             (proc-obj (claude-code-process-get-or-create project-root)))
        (expect (claude-code-process-p proc-obj) :to-be t))))

  (describe "JSON parsing"

    (it "parses valid JSON events"
      (let ((json-str "{\"type\":\"assistant\",\"message\":{\"content\":[]}}"))
        (expect (claude-code-process--parse-json-event json-str) :to-be-truthy)))

    (it "handles malformed JSON gracefully"
      (let ((json-str "{invalid json"))
        (expect (claude-code-process--parse-json-event json-str) :to-be nil)))

    (it "extracts event type correctly"
      (let* ((json-str "{\"type\":\"assistant\",\"data\":\"value\"}")
             (event (claude-code-process--parse-json-event json-str)))
        (expect (alist-get 'type event) :to-equal "assistant")))

    (it "preserves nested structures"
      (let* ((json-str "{\"type\":\"test\",\"nested\":{\"key\":\"value\"}}")
             (event (claude-code-process--parse-json-event json-str))
             (nested (alist-get 'nested event)))
        (expect (alist-get 'key nested) :to-equal "value"))))

  (describe "JSON line handling"

    (after-each
      (claude-code-test-teardown))

    (it "processes complete JSON lines"
      (claude-code-test-spy-on-make-process)

      (let* ((proc-obj (claude-code-process-start "/tmp/test/"))
             (callback-called nil)
             (received-event nil))

        (claude-code-process-add-response-callback
         proc-obj
         (lambda (event)
           (setq callback-called t)
           (setq received-event event)))

        (claude-code-process--handle-json-line
         proc-obj
         "{\"type\":\"test\",\"data\":\"value\"}")

        (expect callback-called :to-be t)
        (expect (alist-get 'type received-event) :to-equal "test")
        (expect (alist-get 'data received-event) :to-equal "value")))

    (it "stores last response for debugging"
      (claude-code-test-spy-on-make-process)

      (let ((proc-obj (claude-code-process-start "/tmp/test/")))
        (claude-code-process--handle-json-line
         proc-obj
         "{\"type\":\"test\"}")

        (expect (alist-get 'type (claude-code-process-last-response proc-obj))
                :to-equal "test"))))

  (describe "Callback system"

    (after-each
      (claude-code-test-teardown))

    (it "registers response callbacks"
      (claude-code-test-spy-on-make-process)

      (let ((proc-obj (claude-code-process-start "/tmp/test/"))
            (callback (lambda (_) nil)))
        (claude-code-process-add-response-callback proc-obj callback)
        (expect (claude-code-process-response-callbacks proc-obj)
                :to-contain callback)))

    (it "registers error callbacks"
      (claude-code-test-spy-on-make-process)

      (let ((proc-obj (claude-code-process-start "/tmp/test/"))
            (callback (lambda (_) nil)))
        (claude-code-process-add-error-callback proc-obj callback)
        (expect (claude-code-process-error-callbacks proc-obj)
                :to-contain callback)))

    (it "calls multiple response callbacks"
      (claude-code-test-spy-on-make-process)

      (let* ((proc-obj (claude-code-process-start "/tmp/test/"))
             (callback1-called nil)
             (callback2-called nil))

        (claude-code-process-add-response-callback
         proc-obj (lambda (_) (setq callback1-called t)))
        (claude-code-process-add-response-callback
         proc-obj (lambda (_) (setq callback2-called t)))

        (claude-code-process--handle-json-line
         proc-obj "{\"type\":\"test\"}")

        (expect callback1-called :to-be t)
        (expect callback2-called :to-be t)))

    (it "handles callback errors gracefully"
      (claude-code-test-spy-on-make-process)

      (let* ((proc-obj (claude-code-process-start "/tmp/test/"))
             (good-callback-called nil))

        (claude-code-process-add-response-callback
         proc-obj (lambda (_) (error "Test error")))
        (claude-code-process-add-response-callback
         proc-obj (lambda (_) (setq good-callback-called t)))

        (claude-code-process--handle-json-line
         proc-obj "{\"type\":\"test\"}")

        ;; Second callback should still run despite first one erroring
        (expect good-callback-called :to-be t))))

  (describe "Process lifecycle"

    (after-each
      (claude-code-test-teardown))

    (it "kills process and cleans up resources"
      (claude-code-test-spy-on-make-process)

      (let* ((project-root "/tmp/test/")
             (proc-obj (claude-code-process-start project-root))
             (buffer (claude-code-process-buffer proc-obj)))

        (claude-code-process-kill proc-obj)

        (expect (claude-code-process-get project-root) :to-be nil)
        (expect (buffer-live-p buffer) :to-be nil)))

    (it "kills all processes"
      (claude-code-test-spy-on-make-process)

      (let ((proc1 (claude-code-process-start "/tmp/test1/"))
            (proc2 (claude-code-process-start "/tmp/test2/")))

        (claude-code-process-kill-all)

        (expect (claude-code-process-get "/tmp/test1/") :to-be nil)
        (expect (claude-code-process-get "/tmp/test2/") :to-be nil)))

    (it "checks if process is alive"
      (claude-code-test-spy-on-make-process)

      (let ((proc-obj (claude-code-process-start "/tmp/test/")))
        ;; Mock process should be alive
        (expect (claude-code-process-alive-p proc-obj) :to-be t)

        ;; After killing, should be dead
        (kill-process (claude-code-process-process proc-obj))
        (sleep-for 0.1) ; Give process time to die
        (expect (claude-code-process-alive-p proc-obj) :to-be nil))))

  (describe "Process status"

    (after-each
      (claude-code-test-teardown))

    (it "initializes with running status"
      (claude-code-test-spy-on-make-process)

      (let ((proc-obj (claude-code-process-start "/tmp/test/")))
        (expect (claude-code-process-status proc-obj) :to-equal 'running)))

    (it "updates status on process completion"
      (claude-code-test-spy-on-make-process)

      (let* ((proc-obj (claude-code-process-start "/tmp/test/"))
             (process (claude-code-process-process proc-obj)))

        ;; Simulate process finishing
        (process-put process 'project-root (claude-code-process-project-root proc-obj))
        (claude-code-process--sentinel process "finished\n")

        (expect (claude-code-process-status proc-obj) :to-equal 'stopped)))

    (it "updates status on process error"
      (claude-code-test-spy-on-make-process)

      (let* ((proc-obj (claude-code-process-start "/tmp/test/"))
             (process (claude-code-process-process proc-obj)))

        ;; Simulate process error
        (process-put process 'project-root (claude-code-process-project-root proc-obj))
        (claude-code-process--sentinel process "exited abnormally\n")

        (expect (claude-code-process-status proc-obj) :to-equal 'error))))

  (describe "Helper functions"

    (it "lists all processes"
      (claude-code-test-spy-on-make-process)

      (let ((proc1 (claude-code-process-start "/tmp/test1/"))
            (proc2 (claude-code-process-start "/tmp/test2/")))

        (let ((all-procs (claude-code-process-list-all)))
          (expect (length all-procs) :to-equal 2)
          (expect all-procs :to-contain proc1)
          (expect all-procs :to-contain proc2))))

    (it "returns empty list when no processes"
      (expect (claude-code-process-list-all) :to-equal nil))))

(provide 'claude-code-process-test)
;;; claude-code-process-test.el ends here
