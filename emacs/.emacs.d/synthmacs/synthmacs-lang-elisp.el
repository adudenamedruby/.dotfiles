(use-package emacs
  :straight (:type built-in)
  :general
  (general-nmap
    :keymaps 'emacs-lisp-mode-map
    :states 'normal
   "gr" nil) ;; interferes with eval-operator
  )

(provide 'synthmacs-lang-clojure)
;;; synthmacs-lang-clojure.el ends here
