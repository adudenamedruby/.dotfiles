(use-package yasnippet)

;; (yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(use-package yasnippet-snippets)
(use-package common-lisp-snippets)

(provide 'synthmacs-snippets)
