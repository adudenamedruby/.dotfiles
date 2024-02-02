;; [[file:../synthmacs.org::*synthmacs-programming-languages][synthmacs-programming-languages:1]]
;;; synthmacs-programming-languages.el --- Synthmacs Programming Languages

;;; Commentary:

;; Set up programming languages & language tools

;;; Code:
;; synthmacs-programming-languages:1 ends here

;; [[file:../synthmacs.org::*C & C++][C & C++:1]]
(use-package ccls
  :straight t
  :defer t
  :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp)
  :commands lsp)
;; C & C++:1 ends here

;; [[file:../synthmacs.org::*CSS][CSS:1]]
(use-package css-mode
  :mode "\\.css\\'"
  :hook
  (css-mode . siren-css-mode-setup)
  :custom
  (css-indent-offset 2)
  )
;; CSS:1 ends here

;; [[file:../synthmacs.org::*Git-Modes][Git-Modes:1]]
(use-package git-modes
  :defer t)
;; Git-Modes:1 ends here

;; [[file:../synthmacs.org::*HTML][HTML:1]]
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))
;; HTML:1 ends here

;; [[file:../synthmacs.org::*JSON][JSON:1]]
(use-package json-mode
  :general
  (synthmacs/local-leader-keys
    :keymaps 'json-mode-map
    "p" 'json-pretty-print)
  )
;; JSON:1 ends here

;; [[file:../synthmacs.org::*clojure-mode][clojure-mode:1]]
(use-package clojure-mode
  :mode "\\.clj$"
  :init
  (setq clojure-align-forms-automatically t))
;; clojure-mode:1 ends here

;; [[file:../synthmacs.org::*clojure-lsp][clojure-lsp:1]]
(use-package clojure-mode
  :hook
  ((clojure-mode clojurescript-mode)
   . (lambda ()
       (setq-local lsp-enable-indentation nil ; cider indentation
                   lsp-enable-completion-at-point nil ; cider completion
                   )
       (lsp-deferred)))
  )
;; clojure-lsp:1 ends here

;; [[file:../synthmacs.org::*Cider][Cider:1]]
(use-package cider
  :hook ((cider-repl-mode . evil-normalize-keymaps)
         (cider-mode . (lambda ()
                         (setq-local evil-lookup-func #'cider-doc)))
         (cider-mode . eldoc-mode))
  :general
  (synthmacs/local-leader-keys
    :keymaps 'clojure-mode-map
    "c" '(cider-connect-clj :wk "connect")
    "C" '(cider-connect-cljs :wk "connect (cljs)")
    "j" '(cider-jack-in :wk "jack in")
    "J" '(cider-jack-in-cljs :wk "jack in (cljs)")
    "d" 'cider-debug-defun-at-point 
    
    "e" '(:ignore t :wk "evaluate")
    "eb" 'cider-eval-buffer
    "el" 'cider-eval-last-sexp
    "eL" 'cider-pprint-eval-last-sexp-to-comment
    "ed" '(cider-eval-defun-at-point :wk "defun")
    "eD" 'cider-pprint-eval-defun-to-comment
    
    "h" 'cider-clojuredocs-web 
    "D" 'cider-doc
    "q" '(cider-quit :wk "quit")
    )
  
  (synthmacs/local-leader-keys
    :keymaps 'clojure-mode-map
    :states 'visual
    "e" 'cider-eval-region)
  
  :init
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-sync-request-timeout nil)
  (setq cider-repl-display-help-banner nil)
  )
;; Cider:1 ends here

;; [[file:../synthmacs.org::*ob-clojure][ob-clojure:1]]
(use-package org
  :config
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider))
;; ob-clojure:1 ends here

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

;; [[file:../synthmacs.org::*Racket & Scheme][Racket & Scheme:1]]
(use-package geiser
  :straight t
  :config
  (setq geiser-active-implementations '(chez)))

(use-package geiser-chez
  :straight t
  :init
  (setq geiser-chez-binary "scheme"))

(use-package racket-mode
  :straight t
  :mode ("\\.rkt\\'" "\\.scrbl\\'")
  :hook ((racket-mode . racket-xp-mode)))
;; Racket & Scheme:1 ends here

;; [[file:../synthmacs.org::*Emacs-lisp][Emacs-lisp:1]]
(use-package emacs
  :straight (:type built-in)
  :general
  (general-nmap
    :keymaps 'emacs-lisp-mode-map
    :states 'normal
    "gr" nil) ;; interferes with eval-operator
  )
;; Emacs-lisp:1 ends here

;; [[file:../synthmacs.org::*aggressive-indent][aggressive-indent:1]]
(use-package aggressive-indent
    :hook ((clojure-mode . aggressive-indent-mode)
           (lisp-mode . aggressive-indent-mode)
           (emacs-lisp-mode . aggressive-indent-mode)))
;; aggressive-indent:1 ends here

;; [[file:../synthmacs.org::*evil-lisp-state][evil-lisp-state:1]]
(use-package evil-lisp-state
  :after evil
  :demand
  :init
  (setq evil-lisp-state-enter-lisp-state-on-command nil)
  (setq evil-lisp-state-global t)
  ;; (setq evil-lisp-state-major-modes '(org-mode emacs-lisp-mode clojure-mode clojurescript-mode lisp-interaction-mode))
  :config
  (evil-lisp-state-leader "SPC l")
  )
;; evil-lisp-state:1 ends here

;; [[file:../synthmacs.org::*Lispy][Lispy:1]]
(use-package lispy
  :straight t
  :hook ((emacs-lisp-mode . lispy-mode)
         (lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package lispyville
  :straight t
  :hook (lispy-mode . lispyville-mode))
;; Lispy:1 ends here

;; [[file:../synthmacs.org::*LUA][LUA:1]]
(use-package lua-mode)
;; LUA:1 ends here

;; [[file:../synthmacs.org::*Markdown][Markdown:1]]
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))
;; Markdown:1 ends here

;; [[file:../synthmacs.org::*Rust][Rust:1]]
(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :straight t
  :defer t)
;; Rust:1 ends here

;; [[file:../synthmacs.org::*Swift][Swift:1]]
(use-package lsp-sourcekit
  :after swift-mode
  :config
  (setq lsp-sourcekit-executable (cl-find-if #'executable-find
                                             (list lsp-sourcekit-executable ; 'sourcekit-lsp' by default
                                                   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"
                                                   "sourcekit"
                                                   "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit-lsp"
                                                   "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit"))))

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

(use-package flycheck-swift
  :after swift-mode
  :config
  (flycheck-swift-setup)
  ;; (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS15.2.sdk")
  ;; (setq flycheck-swift-target "arm64-apple-ios10") 
  )
;; Swift:1 ends here

;; [[file:../synthmacs.org::*YAML][YAML:1]]
(use-package yaml-mode
  :mode ((rx ".yml" eos) . yaml-mode))
;; YAML:1 ends here

;; [[file:../synthmacs.org::*synthmacs-programming-languages][synthmacs-programming-languages:1]]
(provide 'synthmacs-programming-languages)
;;; synthmacs-programming-languages.el ends here
;; synthmacs-programming-languages:1 ends here
