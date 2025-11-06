;;; Directory Local Variables for claude-repl
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  ;; Use spaces for indentation, never tabs
  (indent-tabs-mode . nil)
  ;; Standard Emacs Lisp indentation (not Common Lisp)
  (eval . (progn
            ;; Buttercup BDD-style test macros
            (put 'describe 'lisp-indent-function 1)
            (put 'context 'lisp-indent-function 1)
            (put 'it 'lisp-indent-function 1)
            (put 'before-each 'lisp-indent-function 0)
            (put 'after-each 'lisp-indent-function 0)
            (put 'before-all 'lisp-indent-function 0)
            (put 'after-all 'lisp-indent-function 0)
            ;; Spy-related functions
            (put 'spy-on 'lisp-indent-function 1)))))
