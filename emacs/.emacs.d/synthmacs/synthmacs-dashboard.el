;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :init
  (setq dashboard-startup-banner "~/.dotfiles/emacs/.emacs.d/synthmacs/assets/logo.txt")
  (setq dashboard-banner-logo-title "adudenamedruby's Emacs")
  (setq dashboard-center-content t)
  ;; (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-items '((recents  . 5)
			  (projects . 5)))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-backend 'projectile))

(provide 'synthmacs-dashboard)
