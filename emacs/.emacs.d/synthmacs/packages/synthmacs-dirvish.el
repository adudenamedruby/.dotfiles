;;; package --- Summary

;;; Commentary

;;; Code

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq delete-by-moving-to-trash t))

(synthmacs/leader-keys
  "fm" '(dirvish :wk "file manager"))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(provide 'synthmacs-dirvish)

;;; synthmacs-dirvish.el ends here
