(setq nerd-icons-scale-factor 1.2)

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "FiraCode Nerd Font"))

(use-package all-the-icons
  :if (display-graphic-p))

(provide 'synthmacs-icons)
