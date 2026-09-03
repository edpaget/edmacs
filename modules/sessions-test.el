;;; sessions-test.el --- Tests for sessions.el -*- lexical-binding: t -*-

;;; Commentary:
;; Covers the daemon lifecycle commands `SPC q' dispatches to.  They exist
;; because homebrew.mxcl.emacs-plus@31.plist sets `KeepAlive'
;; unconditionally: launchd relaunches the daemon on ANY exit, so the three
;; intents behind `SPC q q'/`q r'/`q Q' cannot share one command, and each
;; one's daemon-vs-not branch is exactly the kind of dispatch that regresses
;; silently.  Every test stubs the process-ending call rather than making it
;; -- nothing here exits Emacs, kills a frame, or shells out to `brew'.
;;
;; Run with:
;;   emacs -Q --batch -l ert -l modules/sessions-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; sessions.el is NOT passed on the command line -- this file stubs the
;; cross-module functions it calls at load time and loads it itself, below,
;; the same treatment sidebar-test.el gives sidebar.el.  Those functions
;; (`edmacs-evil-config-add-c-x-chord' from evil-config.el,
;; `general-define-key' from the general package) would otherwise abort the
;; load with a void-function error, since neither module is reachable under
;; `-Q'.  `use-package bufferlo' fails non-fatally on its own and prints one
;; benign "Cannot load bufferlo" notice.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; `cl-letf' on a C primitive (`yes-or-no-p', `call-process',
;; `file-executable-p' below) makes Emacs build a native trampoline so
;; already-native callers see the redefinition, and that compile fails under
;; `-Q' when `user-emacs-directory''s eln-cache is not writable -- the whole
;; suite then errors on a missing .eln rather than on anything it tests.
;; Nothing here needs a trampoline: sessions.el is loaded from source, so the
;; caller under test is interpreted and reads the stub straight out of the
;; symbol's function cell.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

;; Stubs for the cross-module calls sessions.el makes at load time; see the
;; Commentary above. Defined before the load, never after.
(unless (fboundp 'edmacs-evil-config-add-c-x-chord)
  (defun edmacs-evil-config-add-c-x-chord (_key _command) nil))
(unless (fboundp 'general-define-key)
  (defun general-define-key (&rest _) nil))

(load (expand-file-name "modules/sessions.el" default-directory) nil t)

;; ============================================================================
;; edmacs-quit -- close the frame under a daemon, kill the terminal otherwise
;; ============================================================================

(ert-deftest edmacs-sessions-test-quit-closes-the-frame-under-a-daemon ()
  "`SPC q q' must not end the process under launchd: an exit is relaunched,
so quitting means closing the frame and leaving the session up."
  (let (closed killed)
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'edmacs-ns-close-frame)
               (lambda (&rest _) (setq closed t)))
              ((symbol-function 'save-buffers-kill-terminal)
               (lambda (&rest _) (setq killed t))))
      (edmacs-quit)
      (should closed)
      (should-not killed))))

(ert-deftest edmacs-sessions-test-quit-kills-terminal-outside-a-daemon ()
  "Without a daemon there is no relaunch to work around, so the stock
behavior is the correct one."
  (let (closed killed)
    (cl-letf (((symbol-function 'daemonp) (lambda () nil))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'edmacs-ns-close-frame)
               (lambda (&rest _) (setq closed t)))
              ((symbol-function 'save-buffers-kill-terminal)
               (lambda (&rest _) (setq killed t))))
      (edmacs-quit)
      (should killed)
      (should-not closed))))

(ert-deftest edmacs-sessions-test-quit-kills-terminal-on-a-tty-daemon-frame ()
  "A non-graphical daemon frame has no Dock tile to keep owned, so the
hide-the-last-frame dance does not apply to it."
  (let (closed killed)
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
              ((symbol-function 'edmacs-ns-close-frame)
               (lambda (&rest _) (setq closed t)))
              ((symbol-function 'save-buffers-kill-terminal)
               (lambda (&rest _) (setq killed t))))
      (edmacs-quit)
      (should killed)
      (should-not closed))))

;; ============================================================================
;; edmacs-restart-daemon -- exiting IS the restart under KeepAlive
;; ============================================================================

(ert-deftest edmacs-sessions-test-restart-daemon-saves-then-exits ()
  "Under a daemon the restart is `kill-emacs' plus launchd, and buffers are
offered for saving BEFORE the exit -- `desktop-save-mode' writes the layout
from `kill-emacs-hook' on the way out, but modified buffers are not its job."
  (let (saved exited restarted)
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'save-some-buffers)
               (lambda (&rest _) (setq saved t)))
              ((symbol-function 'kill-emacs)
               (lambda (&rest _) (should saved) (setq exited t)))
              ((symbol-function 'restart-emacs)
               (lambda (&rest _) (setq restarted t))))
      (edmacs-restart-daemon)
      (should saved)
      (should exited)
      (should-not restarted))))

(ert-deftest edmacs-sessions-test-restart-daemon-passes-prefix-to-save-some-buffers ()
  "A prefix arg reaches `save-some-buffers' as its ARG, so `C-u SPC q r'
saves every modified buffer without prompting."
  (let (save-arg)
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'save-some-buffers)
               (lambda (&optional arg &rest _) (setq save-arg arg)))
              ((symbol-function 'kill-emacs) (lambda (&rest _) nil)))
      (edmacs-restart-daemon t)
      (should (eq save-arg t)))))

(ert-deftest edmacs-sessions-test-restart-daemon-defers-to-restart-emacs-otherwise ()
  "Outside a daemon nothing relaunches the process, so exiting would be a
quit, not a restart -- `restart-emacs' spawns the replacement itself."
  (let (exited restarted)
    (cl-letf (((symbol-function 'daemonp) (lambda () nil))
              ((symbol-function 'kill-emacs)
               (lambda (&rest _) (setq exited t)))
              ((symbol-function 'restart-emacs)
               (lambda (&rest _) (setq restarted t))))
      (edmacs-restart-daemon)
      (should restarted)
      (should-not exited))))

;; ============================================================================
;; edmacs-stop-daemon -- the only one that really quits
;; ============================================================================

(ert-deftest edmacs-sessions-test-stop-daemon-runs-brew-services-stop-detached ()
  "DESTINATION 0 is load-bearing: `brew' has to outlive the Emacs it is
killing, which a process Emacs tracks would not, so assert the argument
rather than only the command line."
  (let (call)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/opt/homebrew/bin/brew"))
              ((symbol-function 'file-executable-p) (lambda (&rest _) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
              ((symbol-function 'call-process)
               (lambda (&rest args) (setq call args) 0)))
      (let ((edmacs-sessions-launchd-service "emacs-plus@31"))
        (edmacs-stop-daemon))
      (should call)
      (should (equal (nth 0 call) "/opt/homebrew/bin/brew"))
      (should (equal (nth 2 call) 0))
      (should (equal (nthcdr 4 call) '("services" "stop" "emacs-plus@31"))))))

(ert-deftest edmacs-sessions-test-stop-daemon-honors-the-service-variable ()
  (let (call)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/opt/homebrew/bin/brew"))
              ((symbol-function 'file-executable-p) (lambda (&rest _) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
              ((symbol-function 'call-process)
               (lambda (&rest args) (setq call args) 0)))
      (let ((edmacs-sessions-launchd-service "emacs-plus@42"))
        (edmacs-stop-daemon))
      (should (equal (nthcdr 4 call) '("services" "stop" "emacs-plus@42"))))))

(ert-deftest edmacs-sessions-test-stop-daemon-declining-runs-nothing ()
  "Answering no leaves the service alone AND leaves buffers unsaved -- the
confirmation gates the whole command, not just the `brew' call."
  (let (call saved)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/opt/homebrew/bin/brew"))
              ((symbol-function 'file-executable-p) (lambda (&rest _) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'save-some-buffers) (lambda (&rest _) (setq saved t)))
              ((symbol-function 'call-process)
               (lambda (&rest args) (setq call args) 0)))
      (edmacs-stop-daemon)
      (should-not call)
      (should-not saved))))

(ert-deftest edmacs-sessions-test-stop-daemon-errors-without-brew ()
  "Fails loudly rather than silently doing nothing when `brew' is absent --
the user asked for the daemon to stay down and has to know it will not."
  (let (call)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'file-executable-p) (lambda (&rest _) nil))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'call-process)
               (lambda (&rest args) (setq call args) 0)))
      (should-error (edmacs-stop-daemon) :type 'user-error)
      (should-not call))))

;;; sessions-test.el ends here
