(use-package magit
  ;; :custom
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  :general
  (synthmacs/leader-keys
    "gb" 'magit-blame-addition
    "gc" 'magit-clone
    "gd" 'magit-dispatch
    "gf" 'magit-file-dispatch
    "gh" 'magit-info
    "gi" 'magit-init
    "gl" 'magit-log
    "gs" 'magit-status)
  (general-nmap
    :keymaps '(magit-status-mode-map
	       magit-stash-mode-map
               magit-revision-mode-map
               magit-process-mode-map
               magit-diff-mode-map)
    "TAB" #'magit-section-toggle
    "<escape>" #'transient-quit-one)
  :init
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq git-commit-fill-column 72)
  :config
  (evil-define-key* '(normal visual) magit-mode-map
    "zz" #'evil-scroll-line-to-center)
  )

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

(use-package helpful
  :general
  (synthmacs/leader-keys
    "h." '(helpful-at-point :wk "helpful-at-point")
    "hc" '(helpful-command :wk "describe-command")
    "hf" '(helpful-callable :wk "describe-function")
    "hk" '(helpful-key :wk "describe-key")
    "hv" '(helpful-variable :wk "describe-variable")))

(use-package projectile
  :demand
  :diminish
  :general
  (synthmacs/leader-keys
    "pf" '(projectile-find-file :wk "projectile-find-file")
    "pk" '(projectile-kill-buffers :wk "projectile-kill-buffers")
    "pp" '(projectile-switch-project :wk "projectile-switch-project")
    "pr" '(projectile-recentf :wk "projectile-recentf")
    "pm" '(projectile-command-map :wk "projectile menu"))
  :init
  (setq projectile-completion-system 'auto)
  (setq projectile-project-root-files '(".envrc" ".projectile" "project.clj" "deps.edn"))
  (setq projectile-switch-project-action 'projectile-commander)
  ;; Do not include straight repos (emacs packages) to project list
  (setq projectile-ignored-project-function
        (lambda (project-root)
          (string-prefix-p (expand-file-name "straight/" user-emacs-directory) project-root)))
  :config
  (projectile-mode +1)
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Developer")
    (setq projectile-project-search-path '("~/Developer")))
  (setq projectile-switch-project-action #'projectile-dired)
  ;; projectile commander methods
  (setq projectile-commander-methods nil)
  (def-projectile-commander-method ?? "Commander help buffer."
				   (ignore-errors (kill-buffer projectile-commander-help-buffer))
				   (with-current-buffer (get-buffer-create projectile-commander-help-buffer)
				     (insert "Projectile Commander Methods:\n\n")
				     (dolist (met projectile-commander-methods)
				       (insert (format "%c:\t%s\n" (car met) (cadr met))))
				     (goto-char (point-min))
				     (help-mode)
				     (display-buffer (current-buffer) t))
				   (projectile-commander))
  (def-projectile-commander-method ?t
				   "Open a *shell* buffer for the project."
				   (projectile-run-vterm))
  (def-projectile-commander-method ?\C-? ;; backspace
				   "Go back to project selection."
				   (projectile-switch-project))
  (def-projectile-commander-method ?d
				   "Open project root in dired."
				   (projectile-dired))
  (def-projectile-commander-method ?f
				   "Find file in project."
				   (projectile-find-file))
  (def-projectile-commander-method ?s
				   "Ripgrep in project."
				   (consult-ripgrep))
  (def-projectile-commander-method ?g
				   "Git status in project."
				   (projectile-vc))
  )

(use-package consult-projectile)

(use-package rg
  :ensure-system-package rg)

(use-package undo-fu
  :demand
  :general
  (:states 'normal
           "u" 'undo-fu-only-undo
           "s-z" 'undo-fu-only-undo
           "\C-r" 'undo-fu-only-redo))

(use-package ws-butler
  :init
  ;; (ws-butler-global-mode 1)
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(provide 'synthmacs-tools)
;;; synthmacs-tools.el ends here
