;;; claude-repl-process-test.el --- Tests for process management -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for claude-repl-process.el

;;; Code:

(require 'buttercup)
(require 'test-helper)

(describe "claude-repl-process"

  (describe "Module loading"
    (it "defines all expected functions"
      (expect 'claude-repl-process-start :to-have-function)
      (expect 'claude-repl-process-get :to-have-function)
      (expect 'claude-repl-process-get-or-create :to-have-function)
      (expect 'claude-repl-process-send-prompt :to-have-function)
      (expect 'claude-repl-process-kill :to-have-function)
      (expect 'claude-repl-process-kill-all :to-have-function)
      (expect 'claude-repl-process-add-response-callback :to-have-function)
      (expect 'claude-repl-process-add-error-callback :to-have-function)
      (expect 'claude-repl-process-alive-p :to-have-function)))

  (describe "Process creation"

    (after-each
      (claude-repl-test-teardown))

    (it "creates a process struct with correct fields"
      (claude-repl-test-spy-on-make-process)

      (let ((proc-obj (claude-repl-process-start "/tmp/test-project/")))
        (expect (claude-repl-process-p proc-obj) :to-be t)
        (expect (claude-repl-process-project-root proc-obj) :to-equal "/tmp/test-project/")
        (expect (claude-repl-process-status proc-obj) :to-equal 'running)
        (expect (claude-repl-process-response-callbacks proc-obj) :to-equal nil)
        (expect (claude-repl-process-error-callbacks proc-obj) :to-equal nil)))

    (it "stores process in hash table by project root"
      (claude-repl-test-spy-on-make-process)

      (let* ((project-root "/tmp/test-project/")
             (proc-obj (claude-repl-process-start project-root)))
        (expect (claude-repl-process-get project-root) :to-be proc-obj)))

    (it "creates unique buffers for different projects"
      (claude-repl-test-spy-on-make-process)

      (let ((proc1 (claude-repl-process-start "/tmp/project1/"))
            (proc2 (claude-repl-process-start "/tmp/project2/")))
        (expect (claude-repl-process-buffer proc1)
                :not :to-be (claude-repl-process-buffer proc2))))

    (it "passes correct arguments to make-process"
      (spy-on 'make-process :and-return-value (claude-repl-test-create-mock-process))
      (spy-on 'claude-repl-approval-start-server :and-return-value "/tmp/test-socket.sock")

      (claude-repl-process-start "/tmp/test/" nil "opus")

      (expect 'make-process :to-have-been-called)
      (let* ((args (spy-calls-args-for 'make-process 0))
             (command (plist-get args :command)))
        ;; Check that all expected arguments are present (order matters for some)
        (expect (nth 0 command) :to-equal "claude")
        (expect (nth 1 command) :to-equal "--print")
        (expect (nth 2 command) :to-equal "--verbose")
        (expect (nth 3 command) :to-equal "--output-format")
        (expect (nth 4 command) :to-equal "stream-json")
        (expect (nth 5 command) :to-equal "--model")
        (expect (nth 6 command) :to-equal "opus")
        (expect (nth 7 command) :to-equal "--settings")
        ;; The 8th argument is the temp settings file path, just check it exists
        (expect (nth 8 command) :to-match "claude-settings.*\\.json"))))

  (describe "Process retrieval"

    (after-each
      (claude-repl-test-teardown))

    (it "returns nil for non-existent process"
      (expect (claude-repl-process-get "/tmp/nonexistent/") :to-be nil))

    (it "retrieves existing process"
      (claude-repl-test-spy-on-make-process)

      (let* ((project-root "/tmp/test/")
             (proc-obj (claude-repl-process-start project-root)))
        (expect (claude-repl-process-get project-root) :to-be proc-obj)))

    (it "get-or-create returns existing process"
      (claude-repl-test-spy-on-make-process)

      (let* ((project-root "/tmp/test/")
             (proc1 (claude-repl-process-start project-root))
             (proc2 (claude-repl-process-get-or-create project-root)))
        (expect proc1 :to-be proc2)))

    (it "get-or-create creates new process if none exists"
      (claude-repl-test-spy-on-make-process)

      (let* ((project-root "/tmp/test/")
             (proc-obj (claude-repl-process-get-or-create project-root)))
        (expect (claude-repl-process-p proc-obj) :to-be t))))

  (describe "JSON parsing"

    (it "parses valid JSON events"
      (let ((json-str "{\"type\":\"assistant\",\"message\":{\"content\":[]}}"))
        (expect (claude-repl-process--parse-json-event json-str) :to-be-truthy)))

    (it "handles malformed JSON gracefully"
      (let ((json-str "{invalid json"))
        (expect (claude-repl-process--parse-json-event json-str) :to-be nil)))

    (it "extracts event type correctly"
      (let* ((json-str "{\"type\":\"assistant\",\"data\":\"value\"}")
             (event (claude-repl-process--parse-json-event json-str)))
        (expect (alist-get 'type event) :to-equal "assistant")))

    (it "preserves nested structures"
      (let* ((json-str "{\"type\":\"test\",\"nested\":{\"key\":\"value\"}}")
             (event (claude-repl-process--parse-json-event json-str))
             (nested (alist-get 'nested event)))
        (expect (alist-get 'key nested) :to-equal "value"))))

  (describe "JSON line handling"

    (after-each
      (claude-repl-test-teardown))

    (it "processes complete JSON lines"
      (claude-repl-test-spy-on-make-process)

      (let* ((proc-obj (claude-repl-process-start "/tmp/test/"))
             (callback-called nil)
             (received-event nil))

        (claude-repl-process-add-response-callback
         proc-obj
         (lambda (event)
           (setq callback-called t)
           (setq received-event event)))

        (claude-repl-process--handle-json-line
         proc-obj
         "{\"type\":\"test\",\"data\":\"value\"}")

        (expect callback-called :to-be t)
        (expect (alist-get 'type received-event) :to-equal "test")
        (expect (alist-get 'data received-event) :to-equal "value")))

    (it "stores last response for debugging"
      (claude-repl-test-spy-on-make-process)

      (let ((proc-obj (claude-repl-process-start "/tmp/test/")))
        (claude-repl-process--handle-json-line
         proc-obj
         "{\"type\":\"test\"}")

        (expect (alist-get 'type (claude-repl-process-last-response proc-obj))
                :to-equal "test"))))

  (describe "Callback system"

    (after-each
      (claude-repl-test-teardown))

    (it "registers response callbacks"
      (claude-repl-test-spy-on-make-process)

      (let ((proc-obj (claude-repl-process-start "/tmp/test/"))
            (callback (lambda (_) nil)))
        (claude-repl-process-add-response-callback proc-obj callback)
        (expect (claude-repl-process-response-callbacks proc-obj)
                :to-contain callback)))

    (it "registers error callbacks"
      (claude-repl-test-spy-on-make-process)

      (let ((proc-obj (claude-repl-process-start "/tmp/test/"))
            (callback (lambda (_) nil)))
        (claude-repl-process-add-error-callback proc-obj callback)
        (expect (claude-repl-process-error-callbacks proc-obj)
                :to-contain callback)))

    (it "calls multiple response callbacks"
      (claude-repl-test-spy-on-make-process)

      (let* ((proc-obj (claude-repl-process-start "/tmp/test/"))
             (callback1-called nil)
             (callback2-called nil))

        (claude-repl-process-add-response-callback
         proc-obj (lambda (_) (setq callback1-called t)))
        (claude-repl-process-add-response-callback
         proc-obj (lambda (_) (setq callback2-called t)))

        (claude-repl-process--handle-json-line
         proc-obj "{\"type\":\"test\"}")

        (expect callback1-called :to-be t)
        (expect callback2-called :to-be t)))

    (it "handles callback errors gracefully"
      (claude-repl-test-spy-on-make-process)

      (let* ((proc-obj (claude-repl-process-start "/tmp/test/"))
             (good-callback-called nil))

        (claude-repl-process-add-response-callback
         proc-obj (lambda (_) (error "Test error")))
        (claude-repl-process-add-response-callback
         proc-obj (lambda (_) (setq good-callback-called t)))

        (claude-repl-process--handle-json-line
         proc-obj "{\"type\":\"test\"}")

        ;; Second callback should still run despite first one erroring
        (expect good-callback-called :to-be t))))

  (describe "Process lifecycle"

    (after-each
      (claude-repl-test-teardown))

    (it "kills process and cleans up resources"
      (claude-repl-test-spy-on-make-process)

      (let* ((project-root "/tmp/test/")
             (proc-obj (claude-repl-process-start project-root))
             (buffer (claude-repl-process-buffer proc-obj)))

        (claude-repl-process-kill proc-obj)

        (expect (claude-repl-process-get project-root) :to-be nil)
        (expect (buffer-live-p buffer) :to-be nil)))

    (it "kills all processes"
      (claude-repl-test-spy-on-make-process)

      (let ((proc1 (claude-repl-process-start "/tmp/test1/"))
            (proc2 (claude-repl-process-start "/tmp/test2/")))

        (claude-repl-process-kill-all)

        (expect (claude-repl-process-get "/tmp/test1/") :to-be nil)
        (expect (claude-repl-process-get "/tmp/test2/") :to-be nil)))

    (it "checks if process is alive"
      (claude-repl-test-spy-on-make-process)

      (let ((proc-obj (claude-repl-process-start "/tmp/test/")))
        ;; Mock process should be alive
        (expect (claude-repl-process-alive-p proc-obj) :to-be-truthy)

        ;; After killing, should be dead
        (kill-process (claude-repl-process-process proc-obj))
        (sleep-for 0.1) ; Give process time to die
        (expect (claude-repl-process-alive-p proc-obj) :to-be nil))))

  (describe "Process status"

    (after-each
      (claude-repl-test-teardown))

    (it "initializes with running status"
      (claude-repl-test-spy-on-make-process)

      (let ((proc-obj (claude-repl-process-start "/tmp/test/")))
        (expect (claude-repl-process-status proc-obj) :to-equal 'running)))

    (it "updates status on process completion"
      (claude-repl-test-spy-on-make-process)

      (let* ((proc-obj (claude-repl-process-start "/tmp/test/"))
             (process (claude-repl-process-process proc-obj)))

        ;; Simulate process finishing
        (process-put process 'project-root (claude-repl-process-project-root proc-obj))
        (claude-repl-process--sentinel process "finished\n")

        (expect (claude-repl-process-status proc-obj) :to-equal 'stopped)))

    (it "updates status on process error"
      (claude-repl-test-spy-on-make-process)

      (let* ((proc-obj (claude-repl-process-start "/tmp/test/"))
             (process (claude-repl-process-process proc-obj)))

        ;; Simulate process error
        (process-put process 'project-root (claude-repl-process-project-root proc-obj))
        (claude-repl-process--sentinel process "exited abnormally\n")

        (expect (claude-repl-process-status proc-obj) :to-equal 'error))))

  (describe "Helper functions"

    (after-each
      (claude-repl-test-teardown))

    (it "lists all processes"
      (claude-repl-test-spy-on-make-process)

      (let ((proc1 (claude-repl-process-start "/tmp/test1/"))
            (proc2 (claude-repl-process-start "/tmp/test2/")))

        (let ((all-procs (claude-repl-process-list-all)))
          (expect (length all-procs) :to-equal 2)
          (expect all-procs :to-contain proc1)
          (expect all-procs :to-contain proc2))))

    (it "returns empty list when no processes"
      (claude-repl-test-teardown)  ; Ensure clean state
      (expect (claude-repl-process-list-all) :to-equal nil))))

(provide 'claude-repl-process-test)
;;; claude-repl-process-test.el ends here
