;; [[file:../synthmacs.org::*Helpful][Helpful:1]]
(use-package helpful
  :general
  (synthmacs/leader-keys
    "h." '(helpful-at-point :wk "helpful-at-point")
    "hc" '(helpful-command :wk "describe-command")
    "hf" '(helpful-callable :wk "describe-function")
    "hk" '(helpful-key :wk "describe-key")
    "hv" '(helpful-variable :wk "describe-variable")))
;; Helpful:1 ends here

;; [[file:../synthmacs.org::*Rg][Rg:1]]
(use-package rg
  :ensure-system-package rg)
;; Rg:1 ends here

;; [[file:../synthmacs.org::*Projectile][Projectile:1]]
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
;; Projectile:1 ends here

;; [[file:../synthmacs.org::*Git/VCS Integration][Git/VCS Integration:1]]
(use-package magit
  ;; :custom
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  :general
  (synthmacs/leader-keys
    "gs" '(magit-status :wk "magit-status")))

;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;; (use-package forge)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(synthmacs/leader-keys
  "gb" '(magit-blame-addition :wk "magit-blame")
  "gc" '(magit-clone :wk "magit-clone")
  "gd" '(magit-dispatch :wk "magit-dispatch")
  "gf" '(magit-file-dispatch :wk "magit-file-dispatch")
  "gh" '(magit-info :wk "magit-help")
  "gi" '(magit-init :wk "magit-init"))
;; Git/VCS Integration:1 ends here

;; [[file:../synthmacs.org::*Dashboard][Dashboard:1]]
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
;; Dashboard:1 ends here

;; [[file:../synthmacs.org::*WS-Butler][WS-Butler:1]]
(use-package ws-butler
  :init
  ;; (ws-butler-global-mode 1)
  (add-hook 'prog-mode-hook #'ws-butler-mode))
;; WS-Butler:1 ends here

;; [[file:../synthmacs.org::*synthmacs-tools][synthmacs-tools:1]]
(provide 'synthmacs-tools)
;;; synthmacs-tools.el ends here
;; synthmacs-tools:1 ends here
