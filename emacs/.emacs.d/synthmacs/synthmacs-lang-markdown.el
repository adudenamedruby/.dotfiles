;; [[file:../synthmacs.org::*Markdown][Markdown:1]]
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))
;; Markdown:1 ends here

;; [[file:../synthmacs.org::*synthmacs-lang-markdown][synthmacs-lang-markdown:1]]
(provide 'synthmacs-lang-markdown)
;;; synthmacs-lang-markdown.el ends here
;; synthmacs-lang-markdown:1 ends here
