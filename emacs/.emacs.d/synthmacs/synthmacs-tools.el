(use-package helpful
  :general
  (synthmacs/leader-keys
    "h." '(helpful-at-point :wk "helpful-at-point")
    "hc" '(helpful-command :wk "describe-command")
    "hf" '(helpful-callable :wk "describe-function")
    "hk" '(helpful-key :wk "describe-key")
    "hv" '(helpful-variable :wk "describe-variable")))

(use-package rg
  :ensure-system-package rg)

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

(use-package ws-butler
  :init
  ;; (ws-butler-global-mode 1)
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(provide 'synthmacs-tools)
;;; synthmacs-tools.el ends here
