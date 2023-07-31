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

;; [[file:../synthmacs.org::*synthmacs-lang-lisp][synthmacs-lang-lisp:1]]
(provide 'synthmacs-lang-lisp)
;;; synthmacs-lang-lisp.el ends here
;; synthmacs-lang-lisp:1 ends here
