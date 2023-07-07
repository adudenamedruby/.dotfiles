;; The main file that handles the startup.

;; Everything else is esentially inside their respective folders
;; - core        Files essential to running Emacs, excluding packages
;;
;; - packages    Files outlining what packages to load. Each pagckage
;;               should be in its own file, with minor exceptions where
;;               packages are interrelated deeply enough that they can
;;               be considered one package. Settings and keybindings
;;               related to a package can also be found in these files.
;;
;; - modules     These are sets of things
;;
;; - assets      Miscellaneous files.

(message "SynthMacs is powering up, please be patient...")

(defun synthmacs-recursive-add-to-load-path (dir)
  "Add DIR and all its sub-directories to `load-path'."
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir))
    (let ((name (expand-file-name f dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (synthmacs-recursive-add-to-load-path name)))))

;; Setup paths for files, recursively
(defvar synthmacs-dir (expand-file-name (file-name-directory load-file-name)))

(synthmacs-recursive-add-to-load-path synthmacs-dir)

(require 'synthmacs-general-settings)
(require 'synthmacs-variables)
(require 'synthmacs-functions)
(require 'synthmacs-straight)
(require 'synthmacs-general)
(require 'synthmacs-evil)
(require 'synthmacs-keybindings)
(require 'synthmacs-winum)
(require 'synthmacs-vertico-stack)
(require 'synthmacs-icons)
(require 'synthmacs-modeline)
(require 'synthmacs-themes)
(require 'synthmacs-hydra)
(require 'synthmacs-which-key)



;; iEdit - https://github.com/victorhge/iedit
;; avy -  https://github.com/abo-abo/av







;; Custom functions




;; LISP rainbow delimiters!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))



;; Helpful
(use-package helpful)

(synthmacs/leader-keys
  "h." '(helpful-at-point :wk "helpful-at-point")
  "hb" '(describe-bindings :wk "describe-bindings")
  "hc" '(helpful-command :wk "describe-command")
  "hf" '(helpful-callable :wk "describe-function")
  "hk" '(helpful-key :wk "describe-key")
  "hp" '(describe-package :wk "describe-package")
  "hv" '(helpful-variable :wk "describe-variable"))



;; ripgrep
(use-package rg
  :ensure-system-package rg)


;; Projectile
(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'auto))
  :init
  (projectile-mode +1)
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Developer")
    (setq projectile-project-search-path '("~/Developer")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package consult-projectile)

(synthmacs/leader-keys
  "pf" '(projectile-find-file :wk "projectile-find-file")
  "pk" '(projectile-kill-buffers :wk "projectile-kill-buffers")
  "pp" '(projectile-switch-project :wk "projectile-switch-project")
  "pr" '(projectile-recentf :wk "projectile-recentf")
  "pm" '(projectile-command-map :wk "projectile menu"))






;; magit
(use-package magit)
  ;; :custom
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(synthmacs/leader-keys
  "gs" '(magit-status :wk "magit-status"))

;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;; (use-package forge)

;; (setq gc-cons-threshold 16777216
;;       gc-cons-percentage 0.1)))


;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-backend 'projectile))

(setq dashboard-startup-banner "~/.dotfiles/emacs/.emacs.d/synthmacs/assets/logo.txt")
(setq dashboard-banner-logo-title "adudenamedruby's Emacs")
(setq dashboard-center-content t)
;; (setq dashboard-icon-type 'all-the-icons)
;; (setq dashboard-set-heading-icons t)
;; (setq dashboard-set-file-icons t)
(setq dashboard-set-init-info t)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)))





;; https://github.com/licht1stein/obsidian.el
;; (use-package obsidian
;;   :ensure t
;;   :demand t
;;   :config
;;   (obsidian-specify-path "~/MY_OBSIDIAN_FOLDER")
;;   (global-obsidian-mode t)
;;   :custom
;;   ;; This directory will be used for `obsidian-capture' if set.
;;   (obsidian-inbox-directory "Inbox")
;;   :bind (:map obsidian-mode-map
;;   ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
;;   ("C-c C-o" . obsidian-follow-link-at-point)
;;   ;; Jump to backlinks
;;   ("C-c C-b" . obsidian-backlink-jump)
;;   ;; If you prefer you can use `obsidian-insert-link'
;;   ("C-c C-l" . obsidian-insert-wikilink)))
;; Ideas for obsidian: https://github.com/0atman/noboilerplate/blob/main/scripts/29-obsidian.md

(provide 'synthmacs-init)
