;;; package --- Summary

;;; Commentary

;;; Code

(use-package 'dirvish
  :init
  (dirvish-override-dired-mode))

(synthmacs/leader-keys
  "fd" '(dirvish :wk "directory"))

(provide 'synthmacs-dirvish)

;;; synthmacs-dirvish.el ends here
