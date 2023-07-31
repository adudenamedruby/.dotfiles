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

;; [[file:../synthmacs.org::*synthmacs-lang-racket][synthmacs-lang-racket:1]]
(provide 'synthmacs-lang-racket)
;;; synthmacs-lang-racket.el ends here
;; synthmacs-lang-racket:1 ends here
