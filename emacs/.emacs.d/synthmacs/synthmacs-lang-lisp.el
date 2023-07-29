(use-package aggressive-indent
    :hook ((clojure-mode . aggressive-indent-mode)
           (lisp-mode . aggressive-indent-mode)
           (emacs-lisp-mode . aggressive-indent-mode)))

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

(provide 'synthmacs-lang-lisp)
;;; synthmacs-lang-lisp.el ends here
