;; https://github.com/abo-abo/avy
(use-package avy
  :general
  (synthmacs/leader-keys
    "jj" '(avy-goto-char-timer :wk "avy-goto-char-timer")
    "jc" '(avy-goto-char :wk "avy-goto-char")
    "jl" '(avy-goto-line :wk "avy-goto-line")
    ))

(provide 'synthmacs-avy)
