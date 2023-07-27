;; [[file:../synthmacs.org::*WS-Butler][WS-Butler:1]]
(use-package ws-butler
  :init
  ;; (ws-butler-global-mode 1)
  (add-hook 'prog-mode-hook #'ws-butler-mode))
;; WS-Butler:1 ends here

;; [[file:../synthmacs.org::*synthmacs-tools][synthmacs-tools:1]]
(provide 'synthmacs-tools)
;;; synthmacs-tools.el ends here
;; synthmacs-tools:1 ends here
