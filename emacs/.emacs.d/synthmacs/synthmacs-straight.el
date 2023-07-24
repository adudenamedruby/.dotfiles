;; [[file:../synthmacs.org::*bootstrap straight and straight-use-package][bootstrap straight and straight-use-package:1]]
(setq straight-repository-branch "develop")
(setq straight-use-package-by-default t)
;; (setq straight-recipes-gnu-elpa-use-mirror t)
;; (setq straight-check-for-modifications '(check-on-save find-when-checking))
;;(setq straight-check-for-modifications nil)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
;; bootstrap straight and straight-use-package:1 ends here

;; [[file:../synthmacs.org::*synthmacs-straight][synthmacs-straight:1]]
(provide 'synthmacs-straight)
;;; synthmacs-straight.el ends here
;; synthmacs-straight:1 ends here
