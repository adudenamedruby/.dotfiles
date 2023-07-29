;; [[file:../synthmacs.org::*Initial setup][Initial setup:1]]
(setq straight-repository-branch "develop")
(setq straight-use-package-by-default t)
;; (setq straight-recipes-gnu-elpa-use-mirror t)
;; (setq straight-check-for-modifications '(check-on-save find-when-checking))
;;(setq straight-check-for-modifications nil)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
;; Initial setup:1 ends here

;; [[file:../synthmacs.org::*Debug][Debug:1]]
(setq debug-on-error t)
;; Debug:1 ends here

;; [[file:../synthmacs.org::*Fetching ~straight.el~][Fetching ~straight.el~:1]]
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
;; Fetching ~straight.el~:1 ends here

;; [[file:../synthmacs.org::*Package cleanup][Package cleanup:1]]
(require 'straight-x)
;; Package cleanup:1 ends here

;; [[file:../synthmacs.org::*Install ~use-package~][Install ~use-package~:1]]
;; Install use-package
(straight-use-package 'use-package)
;; Install ~use-package~:1 ends here

;; [[file:../synthmacs.org::*synthmacs-package-management][synthmacs-package-management:1]]
(provide 'synthmacs-package-management)
;;; synthmacs-package-management.el ends here
;; synthmacs-package-management:1 ends here
