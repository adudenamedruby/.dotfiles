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

;; [[file:../synthmacs.org::*synthmacs-lang-elisp][synthmacs-lang-elisp:1]]
(provide 'synthmacs-lang-elisp)
;;; synthmacs-lang-elisp.el ends here
;; synthmacs-lang-elisp:1 ends here
