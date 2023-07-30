;; [[file:../synthmacs.org::*Common Lisp][Common Lisp:1]]
(defvar inferior-lisp-program "sbcl")
(use-package sly
  :general
  (synthmacs/local-leader-keys
    :keymaps 'lisp-mode-map
    "'" 'sly
    "m" 'macrostep-expand
    
    "c" '(:ignore t :wk "compile")
    "cc" 'sly-compile-file
    "cC" 'sly-compile-and-load-file
    "cf" 'sly-compile-defun
    "cl" 'sly-load-file
    "cn" 'sly-remove-notes
    "cr" 'sly-compile-region
    
    "e" '(:ignore t :wk "evaluate")
    "eb" '(sly-eval-buffer :wk "Evaluate buffer")
    "ee" '(sly-eval-last-expression :wk "Evaluate last")
    "eE" '(sly-eval-print-last-expression :wk "Evaluate/print last")
    "ef" '(sly-eval-defun :wk "Evaluate defun")
    "eF" '(sly-undefine-function :wk "Undefine function")
    "er" '(sly-eval-region :wk "Evaluate region")
    
    "g" '(:ignore t :wk "goto")
    "gb" '(sly-pop-find-definition-stack :wk "Go back")
    "gd" '(sly-edit-definition :wk "Go to")
    "gD" '(sly-edit-definition-other-window :wk "Go to (other window)")
    "gn" '(sly-next-note :wk "Next note")
    "gN" '(sly-previous-note :wk "Previous note")
    "gs" '(sly-stickers-next-sticker :wk "Next sticker")
    "gS" '(sly-stickers-prev-sticker :wk "Previous sticker")
    
    "h" '(:ignore :wk "help")
    "h<" '(sly-who-calls :wk "Who calls")
    "h>" '(sly-calls-who :wk "Calls who")
    "h~" '(hyperspec-lookup-format :wk "Lookup format directive")
    "h#" '(hyperspec-lookup-reader-macro :wk "Lookup reader macro")
    "ha" '(sly-apropos :wk "Apropos")
    "hb" '(sly-who-binds :wk "Who binds")
    "hd" '(sly-disassemble-symbol :wk "Disassemble symbol")
    "hh" '(sly-describe-symbol :wk "Describe symbol")
    "hH" '(sly-hyperspec-lookup :wk "HyperSpec lookup")
    "hm" '(sly-who-macroexpands :wk "Who macro-expands")
    "hp" '(sly-apropos-package :wk "Apropos package")
    "hr" '(sly-who-references :wk "Who references")
    "hs" '(sly-who-specializes :wk "Who specializes")
    "hS" '(sly-who-sets :wk "Who sets")
    
    "r" '(:ignore :wk "repl")
    "rc" '(sly-mrepl-clear-repl :wk "Clear REPL")
    "rl" '(+lisp/load-project-systems :wk "Load Project")
    "rq" '(sly-quit-lisp :wk "Quit connection")
    "rr" '(sly-restart-inferior-lisp :wk "Restart connection")
    "rR" '(+lisp/reload-project :wk "Reload Project")
    "rs" '(sly-mrepl-sync :wk "Sync REPL")
    
    "s" '(:ignore :wk "stickers")
    "sb" '(sly-stickers-toggle-break-on-stickers :wk "Toggle breaking stickers")
    "sc" '(sly-stickers-clear-defun-stickers :wk "Clear defun stickers")
    "sC" '(sly-stickers-clear-buffer-stickers :wk "Clear buffer stickers")
    "sf" '(sly-stickers-fetch :wk "Fetch stickers")
    "sr" '(sly-stickers-replay :wk "Replay stickers")
    "ss" '(sly-stickers-dwim :wk "Add/remove sticker")
    
    "t" '(:ignore t :wk "test")
    "ts" '(+lisp/test-system :wk "Test System")
    
    "T" '(:ignore t :wk "trace")
    "Tt" '(sly-toggle-trace-fdefinition :wk "Toggle")
    "TT" '(sly-toggle-fancy-trace :wk "Toggle (fancy)")
    "Tu" '(sly-untrace-all :wk "Untrace all")
    )
  :hook (lisp-mode-local-vars . sly-editing-mode)
  :config
  (setq sly-complete-symbol-function 'sly-flex-completions)
  )

(use-package sly-macrostep)

(use-package sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))
;; Common Lisp:1 ends here

;; [[file:../synthmacs.org::*synthmacs-lang-clisp][synthmacs-lang-clisp:1]]
(provide 'synthmacs-lang-clisp)
;;; synthmacs-lang-clisp.el ends here
;; synthmacs-lang-clisp:1 ends here
