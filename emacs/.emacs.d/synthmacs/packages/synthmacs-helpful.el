;; https://github.com/Wilfred/helpful
(use-package helpful
  :general
  (synthmacs/leader-keys
    "h." '(helpful-at-point :wk "helpful-at-point")
    "hdc" '(helpful-command :wk "describe-command")
    "hdf" '(helpful-callable :wk "describe-function")
    "hdk" '(helpful-key :wk "describe-key")
    "hdv" '(helpful-variable :wk "describe-variable")))

(provide 'synthmacs-helpful)
