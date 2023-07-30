;; [[file:../synthmacs.org::*HTML][HTML:1]]
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))
;; HTML:1 ends here

;; [[file:../synthmacs.org::*synthmacs-lang-html][synthmacs-lang-html:1]]
(provide 'synthmacs-lang-html)
;;; synthmacs-lang-html.el ends here
;; synthmacs-lang-html:1 ends here
