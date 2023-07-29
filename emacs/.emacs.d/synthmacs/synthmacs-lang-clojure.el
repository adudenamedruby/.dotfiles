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
    "d d" 'cider-debug-defun-at-point 
    "e b" 'cider-eval-buffer
    "e l" 'cider-eval-last-sexp
    "e L" 'cider-pprint-eval-last-sexp-to-comment
    "e d" '(cider-eval-defun-at-point :wk "defun")
    "e D" 'cider-pprint-eval-defun-to-comment
    "h" 'cider-clojuredocs-web 
    "K" 'cider-doc
    "q" '(cider-quit :qk "quit")
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

;; [[file:../synthmacs.org::*synthmacs-lang-clojure][synthmacs-lang-clojure:1]]
(provide 'synthmacs-lang-clojure)
;;; synthmacs-lang-clojure.el ends here
;; synthmacs-lang-clojure:1 ends here
