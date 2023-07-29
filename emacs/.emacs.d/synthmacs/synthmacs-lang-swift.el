;; [[file:../synthmacs.org::*Swift][Swift:1]]
(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))
;; Swift:1 ends here

;; [[file:../synthmacs.org::*synthmacs-lang-swift][synthmacs-lang-swift:1]]
(provide 'synthmacs-lang-swift)
;;; synthmacs-lang-swift.el ends here
;; synthmacs-lang-swift:1 ends here
