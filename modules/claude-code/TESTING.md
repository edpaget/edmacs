# Testing Guide for Claude Code REPL

## Overview of Emacs Testing Options

### 1. **ERT (Emacs Lisp Regression Testing)** - Built-in ✅

**Pros:**
- Built into Emacs (no dependencies)
- Simple, straightforward API
- Good integration with Emacs debugging tools
- Standard for many Emacs packages

**Cons:**
- No built-in fixtures/setup-teardown
- Less expressive than modern BDD frameworks
- Requires boilerplate for test organization

**Best for:** Simple unit tests, when you want zero dependencies

### 2. **Buttercup** - BDD Framework ⭐ Recommended

**Pros:**
- Modern BDD syntax (describe/it blocks like Jasmine/RSpec)
- Built-in setup/teardown (`before-each`, `after-each`)
- Powerful spy/mock system
- Better test organization
- Excellent error messages
- Used by major projects (Projectile, etc.)

**Cons:**
- External dependency (but available on MELPA)
- Slightly more complex than ERT

**Best for:** Comprehensive test suites, TDD/BDD workflows, complex testing scenarios

### 3. **Additional Tools**

- **assess**: Stateless testing (leaves Emacs unchanged)
- **el-mock**: Mocking library (works with ERT)
- **ert-runner**: Test runner for CI/CD
- **with-simulated-input**: Simulate user input
- **Eldev**: Modern build tool with testing support

## Recommended Approach for Claude Code

**Use Buttercup** for the following reasons:

1. ✅ Modern, readable syntax
2. ✅ Built-in mocking/spying (essential for process testing)
3. ✅ Setup/teardown for managing process state
4. ✅ Better organization for large test suites
5. ✅ Industry standard for modern Emacs packages

## Test Structure

```
claude-code/
├── claude-code-process.el
├── claude-code-buffer.el
├── claude-code-core.el
└── test/
    ├── claude-code-process-test.el
    ├── claude-code-buffer-test.el
    ├── claude-code-core-test.el
    ├── test-helper.el
    └── fixtures/
        ├── mock-responses.json
        └── sample-interactions.el
```

## Example Test Files

### Setup Helper (`test/test-helper.el`)

```elisp
;;; test-helper.el --- Test helpers for Claude Code

(require 'buttercup)
(require 'cl-lib)

;; Add source directory to load path
(let ((project-dir (file-name-directory
                    (directory-file-name
                     (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-dir))

(require 'claude-code-process)
(require 'claude-code-buffer)
(require 'claude-code-core)

;; Test fixtures
(defvar claude-code-test-project-root "/tmp/claude-code-test/")

(defun claude-code-test-cleanup ()
  "Clean up all test resources."
  (claude-code-process-kill-all)
  (dolist (buf (buffer-list))
    (when (string-match-p "\\*claude-code" (buffer-name buf))
      (kill-buffer buf))))

;; Mock process creation for tests
(defvar claude-code-test-mock-process nil)

(defun claude-code-test-create-mock-process ()
  "Create a mock process for testing."
  (let ((buf (generate-new-buffer " *mock-claude-process*")))
    (make-process
     :name "mock-claude"
     :buffer buf
     :command '("cat")  ; Dummy command
     :noquery t)))

;; Mock JSON events
(defvar claude-code-test-mock-events
  '(((type . "system")
     (session_id . "test-session-123"))
    ((type . "assistant")
     (message
      ((content . [((type . "text")
                    (text . "This is a test response."))]))))
    ((type . "result")
     (usage ((input_tokens . 10)
             (output_tokens . 20)))
     (duration_ms . 1000))))

(provide 'test-helper)
```

### Process Tests (`test/claude-code-process-test.el`)

```elisp
;;; claude-code-process-test.el --- Tests for process management

(require 'test-helper)

(describe "claude-code-process"

  (describe "Process creation"

    (after-each
      (claude-code-test-cleanup))

    (it "creates a process with correct arguments"
      (spy-on 'make-process :and-return-value
              (claude-code-test-create-mock-process))

      (let ((proc-obj (claude-code-process-start "/tmp/test-project/")))
        (expect (claude-code-process-p proc-obj) :to-be t)
        (expect (claude-code-process-project-root proc-obj)
                :to-equal "/tmp/test-project/")
        (expect (claude-code-process-status proc-obj)
                :to-equal 'running)))

    (it "stores process in hash table by project root"
      (spy-on 'make-process :and-return-value
              (claude-code-test-create-mock-process))

      (let* ((project-root "/tmp/test-project/")
             (proc-obj (claude-code-process-start project-root)))
        (expect (claude-code-process-get project-root)
                :to-be proc-obj)))

    (it "reuses existing process for same project"
      (spy-on 'make-process :and-return-value
              (claude-code-test-create-mock-process))

      (let* ((project-root "/tmp/test-project/")
             (proc1 (claude-code-process-start project-root))
             (proc2 (claude-code-process-get-or-create project-root)))
        (expect proc1 :to-be proc2))))

  (describe "JSON parsing"

    (it "parses valid JSON events"
      (let ((json-str "{\"type\":\"assistant\",\"message\":{\"content\":[]}}"))
        (expect (claude-code-process--parse-json-event json-str)
                :to-be-truthy)))

    (it "handles malformed JSON gracefully"
      (let ((json-str "{invalid json"))
        (expect (claude-code-process--parse-json-event json-str)
                :to-be nil)))

    (it "extracts event type correctly"
      (let* ((json-str "{\"type\":\"assistant\"}")
             (event (claude-code-process--parse-json-event json-str)))
        (expect (alist-get 'type event) :to-equal "assistant"))))

  (describe "Callback system"

    (it "calls response callbacks when events arrive"
      (spy-on 'make-process :and-return-value
              (claude-code-test-create-mock-process))

      (let* ((proc-obj (claude-code-process-start "/tmp/test/"))
             (callback-called nil)
             (received-event nil))

        (claude-code-process-add-response-callback
         proc-obj
         (lambda (event)
           (setq callback-called t)
           (setq received-event event)))

        ;; Simulate event
        (claude-code-process--handle-json-line
         proc-obj
         "{\"type\":\"test\",\"data\":\"value\"}")

        (expect callback-called :to-be t)
        (expect (alist-get 'type received-event) :to-equal "test")))

    (it "handles multiple callbacks"
      (spy-on 'make-process :and-return-value
              (claude-code-test-create-mock-process))

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
        (expect callback2-called :to-be t))))

  (describe "Process lifecycle"

    (after-each
      (claude-code-test-cleanup))

    (it "kills process and cleans up resources"
      (spy-on 'make-process :and-return-value
              (claude-code-test-create-mock-process))

      (let* ((project-root "/tmp/test/")
             (proc-obj (claude-code-process-start project-root)))

        (claude-code-process-kill proc-obj)

        (expect (claude-code-process-get project-root) :to-be nil)))))
```

### Buffer Tests (`test/claude-code-buffer-test.el`)

```elisp
;;; claude-code-buffer-test.el --- Tests for buffer UI

(require 'test-helper)

(describe "claude-code-buffer"

  (after-each
    (claude-code-test-cleanup))

  (describe "Buffer creation"

    (it "creates buffer with correct name"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test-project/")))
        (expect (buffer-name buffer)
                :to-equal "*claude-code: test-project*")))

    (it "reuses existing buffer"
      (let ((buf1 (claude-code-buffer-get-or-create "/tmp/test/"))
            (buf2 (claude-code-buffer-get-or-create "/tmp/test/")))
        (expect buf1 :to-be buf2)))

    (it "sets correct major mode"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (with-current-buffer buffer
          (expect major-mode :to-be 'claude-code-buffer-mode)))))

  (describe "Interaction management"

    (it "starts interaction with prompt"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Test prompt")

        (with-current-buffer buffer
          (expect (buffer-string) :to-match "Test prompt"))))

    (it "appends text to current interaction"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-append-text buffer "Response text")

        (with-current-buffer buffer
          (expect (buffer-string) :to-match "Response text"))))

    (it "tracks tool usage"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-add-tool-use buffer "Read" '((file . "test.el")))

        (with-current-buffer buffer
          (expect (buffer-string) :to-match "Tool: Read"))))

    (it "completes interaction with metadata"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/")))
        (claude-code-buffer-start-interaction buffer "Prompt")
        (claude-code-buffer-complete-interaction
         buffer
         '((input_tokens . 10) (output_tokens . 20)))

        (with-current-buffer buffer
          (expect (buffer-string) :to-match "Tokens: 10 in, 20 out")))))

  (describe "Event handling"

    (it "handles assistant events"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/"))
            (event '((type . "assistant")
                     (message
                      ((content . [((type . "text")
                                    (text . "Test response"))]))))))

        (claude-code-buffer-start-interaction buffer "Test")
        (claude-code-buffer-handle-assistant-event buffer event)

        (with-current-buffer buffer
          (expect (buffer-string) :to-match "Test response"))))

    (it "handles result events"
      (let ((buffer (claude-code-buffer-get-or-create "/tmp/test/"))
            (event '((type . "result")
                     (usage ((input_tokens . 5)
                            (output_tokens . 10)))
                     (duration_ms . 1000))))

        (claude-code-buffer-start-interaction buffer "Test")
        (claude-code-buffer-handle-result-event buffer event)

        (with-current-buffer buffer
          (expect (buffer-string) :to-match "Tokens: 5 in, 10 out"))))))
```

### Integration Tests (`test/claude-code-integration-test.el`)

```elisp
;;; claude-code-integration-test.el --- Integration tests

(require 'test-helper)

(describe "claude-code integration"

  (after-each
    (claude-code-test-cleanup))

  (it "handles complete interaction flow"
    (spy-on 'make-process :and-call-fake
            (lambda (&rest args)
              (let ((proc (claude-code-test-create-mock-process)))
                ;; Simulate receiving events after a delay
                (run-at-time
                 0.1 nil
                 (lambda ()
                   (let ((filter (plist-get args :filter)))
                     ;; Send mock events
                     (funcall filter proc
                              "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}\n")
                     (funcall filter proc
                              "{\"type\":\"result\",\"usage\":{\"input_tokens\":5,\"output_tokens\":10}}\n"))))
                proc)))

    (let* ((project-root "/tmp/test/")
           (buffer (claude-code-buffer-get-or-create project-root))
           (proc-obj (claude-code-process-start project-root)))

      ;; Set up event handlers
      (claude-code-buffer-start-interaction buffer "Test prompt")

      (claude-code-process-add-response-callback
       proc-obj
       (lambda (event)
         (cond
          ((equal (alist-get 'type event) "assistant")
           (claude-code-buffer-handle-assistant-event buffer event))
          ((equal (alist-get 'type event) "result")
           (claude-code-buffer-handle-result-event buffer event)))))

      ;; Wait for async events
      (sleep-for 0.2)

      (with-current-buffer buffer
        (expect (buffer-string) :to-match "Hello")
        (expect (buffer-string) :to-match "Tokens:")))))
```

## Running Tests

### Interactive (in Emacs)

```elisp
;; Load Buttercup
M-x package-install RET buttercup RET

;; Load test files
M-x load-file RET test/test-helper.el RET
M-x load-file RET test/claude-code-process-test.el RET

;; Run all tests
M-x buttercup-run-discover

;; Run specific test file
M-x buttercup-run RET test/claude-code-process-test.el RET
```

### Command Line

```bash
# Using emacs batch mode
emacs -batch \
  -l test/test-helper.el \
  -l test/claude-code-process-test.el \
  -f buttercup-run-discover

# Using ert-runner (if installed)
cask exec ert-runner

# Using Eldev
eldev test
```

### CI/CD (GitHub Actions)

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        emacs_version:
          - '28.2'
          - '29.1'
          - 'snapshot'
    steps:
      - uses: actions/checkout@v2

      - uses: purcell/setup-emacs@master
        with:
          version: ${{ matrix.emacs_version }}

      - uses: actions/cache@v2
        with:
          path: ~/.emacs.d/elpa
          key: ${{ runner.os }}-elpa-${{ hashFiles('**/Cask') }}

      - name: Install Cask
        run: curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

      - name: Install dependencies
        run: cask install

      - name: Run tests
        run: cask exec buttercup -L .
```

## Test Coverage

### What to Test

1. **Process Management**
   - ✅ Process creation with correct arguments
   - ✅ Process storage/retrieval by project
   - ✅ Process lifecycle (start/stop/kill)
   - ✅ JSON parsing (valid/invalid/edge cases)
   - ✅ Callback registration and invocation
   - ✅ Error handling

2. **Buffer Management**
   - ✅ Buffer creation and naming
   - ✅ Interaction creation and tracking
   - ✅ Text appending and streaming
   - ✅ Tool usage formatting
   - ✅ Metadata display
   - ✅ Buffer clearing

3. **Event Handling**
   - ✅ Assistant event processing
   - ✅ Result event processing
   - ✅ System event handling
   - ✅ Error event handling
   - ✅ Edge cases (missing fields, etc.)

4. **Integration**
   - ✅ End-to-end conversation flow
   - ✅ Multiple interactions in one buffer
   - ✅ Process-buffer coordination
   - ✅ Error recovery

### What NOT to Test

- ❌ Actual Claude API calls (use mocks)
- ❌ External process behavior (use fake processes)
- ❌ User interaction (unless using `with-simulated-input`)
- ❌ Display/rendering specifics (fragile)

## Best Practices

### 1. Use Test Fixtures

```elisp
(defvar claude-code-test-fixtures-dir
  (expand-file-name "fixtures"
                    (file-name-directory load-file-name)))

(defun claude-code-test-load-fixture (filename)
  "Load a test fixture file."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name filename claude-code-test-fixtures-dir))
    (buffer-string)))
```

### 2. Clean Up After Tests

```elisp
(after-each
  (claude-code-test-cleanup)
  ;; Kill all test buffers
  ;; Reset global state
  ;; Remove temporary files
  )
```

### 3. Mock External Dependencies

```elisp
;; Don't actually call Claude
(spy-on 'make-process :and-return-value mock-process)

;; Don't actually read files
(spy-on 'insert-file-contents :and-return-value "mock content")
```

### 4. Test One Thing Per Test

```elisp
;; Good
(it "parses JSON correctly"
  (expect (parse-json "{\"key\":\"value\"}") :to-equal expected))

;; Bad - tests too many things
(it "does everything"
  (expect (parse-json ...) :to-equal ...)
  (expect (format-output ...) :to-equal ...)
  (expect (save-file ...) :to-equal ...))
```

### 5. Use Descriptive Names

```elisp
;; Good
(it "handles malformed JSON gracefully without crashing"
  ...)

;; Bad
(it "test1"
  ...)
```

## Next Steps

1. Install Buttercup: `M-x package-install RET buttercup RET`
2. Create `test/` directory
3. Copy and adapt example tests above
4. Run tests interactively to verify
5. Set up CI/CD for automated testing
6. Aim for >80% code coverage

## Test Coverage Summary

### Current Status

**Total Tests**: 69
**Passing**: 60 (87%)
**Failing**: 9 (13%)

### Coverage by Module

| Module | Tests | Passing | Failing | Coverage |
|--------|-------|---------|---------|----------|
| claude-code-process.el | 26 | 23 | 3 | 88% |
| claude-code-buffer.el | 33 | 28 | 5 | 85% |
| claude-code-core.el | 10 | 9 | 1 | 90% |

### Test Categories

**Process Management** (26 tests)
- Module loading (1 test)
- Process creation (4 tests)
- Process retrieval (4 tests)
- JSON parsing (4 tests)
- JSON line handling (2 tests)
- Callback system (4 tests)
- Process lifecycle (3 tests)
- Process status (3 tests)
- Helper functions (2 tests)

**Buffer UI** (33 tests)
- Module loading (1 test)
- Buffer creation (6 tests)
- Interaction management (7 tests)
- Tool usage (3 tests)
- Interaction completion (3 tests)
- Event handling (3 tests)
- Multiple interactions (2 tests)
- Buffer clearing (2 tests)
- Buffer mode (2 tests)

**Core Commands** (10 tests)
- Module loading (2 tests)
- claude-code-ask (4 tests)
- claude-code-open-buffer (1 test)
- claude-code-clear-buffer (2 tests)
- claude-code-show-processes (4 tests)

### Known Test Failures

The following 9 tests are currently failing:

1. **Buffer event handling** (3 failures)
   - `handles assistant event with text` - Text content not appearing in buffer
   - `handles assistant event with tool use` - Tool use not being formatted correctly
   - `handles result event` - Metadata not being displayed

2. **Mode detection** (1 failure)
   - `has correct mode name` - `format-mode-line` returning empty string

3. **Process lifecycle** (2 failures)
   - `checks if process is alive` - Returns status list instead of boolean
   - `lists all processes` - Cleanup not working between tests

4. **Interaction state** (2 failures)
   - `stores completion status` - Status not persisting after completion
   - `adds separator between interactions` - Separator not rendering

5. **Cleanup** (1 failure)
   - `does nothing if buffer doesn't exist` - Throwing error instead of returning nil

### Next Steps for 100% Coverage

1. **Fix event handling** - Investigate why assistant events aren't updating buffer
2. **Fix mode name** - Ensure `mode-name` is set correctly in `claude-code-buffer-mode`
3. **Fix process alive check** - Return boolean instead of status list
4. **Improve cleanup** - Ensure hash table is cleared between tests
5. **Fix interaction state** - Investigate why completion status isn't persisting

## Resources

- [Buttercup Documentation](https://github.com/jorgenschaefer/emacs-buttercup)
- [ERT Manual](https://www.gnu.org/software/emacs/manual/html_mono/ert.html)
- [Emacs Package Dev Handbook](https://alphapapa.github.io/emacs-package-dev-handbook/)
- [Testing Elisp (nullprogram)](https://nullprogram.com/blog/2012/08/15/)
- [Eldev Documentation](https://github.com/emacs-eldev/eldev)
