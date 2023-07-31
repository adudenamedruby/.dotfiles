;; [[file:../synthmacs.org::*C & C++][C & C++:1]]
(use-package ccls
  :straight t
  :defer t
  :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp)
  :commands lsp)
;; C & C++:1 ends here

;; [[file:../synthmacs.org::*synthmacs-lang-cpp][synthmacs-lang-cpp:1]]
(provide 'synthmacs-lang-cpp)
;;; synthmacs-lang-cpp.el ends here
;; synthmacs-lang-cpp:1 ends here
