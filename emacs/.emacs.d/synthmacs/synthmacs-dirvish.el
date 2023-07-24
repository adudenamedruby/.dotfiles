;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq delete-by-moving-to-trash t
	dirvish-attributes
	'(all-the-icons file-size collapse subtree-state vc-state)
	dirvish-side-width 45
	dirvish-side-follow-mode t
	))

(synthmacs/leader-keys
  "fm" '(dirvish :wk "file manager")
  "ft" '(dirvish-side :wk "file tree"))

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
