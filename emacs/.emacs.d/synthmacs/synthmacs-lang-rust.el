;; [[file:../synthmacs.org::*Rust][Rust:1]]
(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :straight t
  :defer t)
;; Rust:1 ends here

;; [[file:../synthmacs.org::*synthmacs-lang-rust][synthmacs-lang-rust:1]]
(provide 'synthmacs-lang-rust)
;;; synthmacs-lang-rust.el ends here
;; synthmacs-lang-rust:1 ends here
