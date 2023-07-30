;; [[file:../synthmacs.org::*CSS][CSS:1]]
(use-package css-mode
  :mode "\\.css\\'"
  :hook
  (css-mode . siren-css-mode-setup)
  :custom
  (css-indent-offset 2)
  )
;; CSS:1 ends here

;; [[file:../synthmacs.org::*synthmacs-lang-css][synthmacs-lang-css:1]]
(provide 'synthmacs-lang-css)
;;; synthmacs-lang-css.el ends here
;; synthmacs-lang-css:1 ends here
