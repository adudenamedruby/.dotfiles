;; https://github.com/bbatsov/projectile
(use-package projectile
  :diminish
  :general
  (synthmacs/leader-keys
    "pf" '(projectile-find-file :wk "projectile-find-file")
    "pk" '(projectile-kill-buffers :wk "projectile-kill-buffers")
    "pp" '(projectile-switch-project :wk "projectile-switch-project")
    "pr" '(projectile-recentf :wk "projectile-recentf")
    "pm" '(projectile-command-map :wk "projectile menu"))
  :custom ((projectile-completion-system 'auto))
  :init
  (projectile-mode +1)
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Developer")
    (setq projectile-project-search-path '("~/Developer")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package consult-projectile)

(provide 'synthmacs-projectile)
