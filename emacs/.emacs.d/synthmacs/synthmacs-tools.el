;; [[file:../synthmacs.org::*synthmacs-tools][synthmacs-tools:1]]
;;; synthmacs-tools.el --- Synthmacs Useful Tools

;;; Commentary:

;; Set up a variety of useful tools for Synthmacs

;;; Code:
;; synthmacs-tools:1 ends here

;; [[file:../synthmacs.org::*Magit][Magit:1]]
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
  (synthmacs/leader-keys
    :keymaps 'magit-mode-map
    "z" 'magit-stash)
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
  ;; :config
  ;; (evil-define-key* '(normal visual) magit-mode-map
  ;;   "zz" #'evil-scroll-line-to-center)
  )
;; Magit:1 ends here

;; [[file:../synthmacs.org::*magit-delta][magit-delta:1]]
(use-package magit-delta
  :after magit
  :commands magit-delta-mode
  :hook (magit-mode . magit-delta-mode)
  )
;; magit-delta:1 ends here

;; [[file:../synthmacs.org::*git-gutter][git-gutter:1]]
(use-package git-gutter
  :init
  (global-git-gutter-mode +1)
  :config
  (setq git-gutter:update-interval 0.02))
;; git-gutter:1 ends here

;; [[file:../synthmacs.org::*git-gutter-fringe][git-gutter-fringe:1]]
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
;; git-gutter-fringe:1 ends here

;; [[file:../synthmacs.org::*Treemacs][Treemacs:1]]
(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :hook (treemacs-mode . treemacs-project-follow-mode)
  :bind (("M-J" . treemacs-find-file))
  :general
  (synthmacs/leader-keys
    "ft" '(treemacs-find-file :wk "treemacs-find-file")
    "tn" '(treemacs :wk "treemacs-toggle")
)
  :custom-face
  (treemacs-directory-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-directory-collapsed-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-ignored-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-unmodified-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-untracked-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-added-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-renamed-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-modified-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-tags-face ((t (:family "SF Pro Display" :height 0.9))))
  :config
  (setq treemacs-follow-after-init t)
  (setq treemacs-collapse-dirs 1)
  (setq treemacs-directory-name-transformer #'identity)
  (setq treemacs-file-name-transformer #'identity)
  (setq treemacs-show-cursor nil)
  (setq treemacs-display-current-project-exclusively t)
  (setq treemacs-filewatch-mode t)
  (setq treemacs-follow-mode nil)
  (setq treemacs-hide-dot-git-directory t)
  (setq treemacs-git-integration t)
  (setq treemacs-space-between-root-nodes t)
  (setq treemacs-hide-gitignored-files-mode t)
  (setq treemacs-git-mode 'extended)
  (setq treemacs-indentation 1)
  (setq treemacs-is-never-other-window t)
  (setq treemacs-silent-refresh	t)
  (setq treemacs-sorting 'treemacs--sort-alphabetic-case-insensitive-asc)
  (setq treemacs-width 30))

(defun treemacs-mode-handler()
  (set (make-local-variable 'face-remapping-alist)
       '((default :background "#1c1c24"))))

(add-hook 'treemacs-mode-hook 'treemacs-mode-handler)

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))
;; Treemacs:1 ends here

;; [[file:../synthmacs.org::*Spell-checking][Spell-checking:1]]
(use-package flyspell
  :straight nil
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . turn-on-flyspell))
  :config
  (flyspell-mode +1))
;; Spell-checking:1 ends here

;; [[file:../synthmacs.org::*Helpful][Helpful:1]]
(use-package helpful
  :after evil
  :general
  (synthmacs/leader-keys
    "h." '(helpful-at-point :wk "helpful-at-point")
    "hc" '(helpful-command :wk "describe-command")
    "hf" '(helpful-callable :wk "describe-function")
    "hk" '(helpful-key :wk "describe-key")
    "hv" '(helpful-variable :wk "describe-variable"))
  :init
  (setq evil-lookup-func #'helpfus-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  )
;; Helpful:1 ends here

;; [[file:../synthmacs.org::*Projectile][Projectile:1]]
(use-package projectile
  :demand
  :diminish
  :general
  (synthmacs/leader-keys
    "pf" 'projectile-find-file
    "pk" 'projectile-kill-buffers
    "pp" 'projectile-switch-project
    "pd" 'projectile-dired
    "pr" 'projectile-recentf
    "pm" '(projectile-command-map :wk "projectile menu"))
  :init
  (setq projectile-completion-system 'auto)
  (setq projectile-project-root-files '(".envrc" ".projectile" "project.clj" "deps.edn"))
  (setq projectile-switch-project-action 'projectile-commander)
  (setq projectile-sort-order 'recently-active)
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

(use-package consult-projectile
  :after consult projectile
  :demand t
  :general
  (synthmacs/leader-keys
    "pP" 'consult-projectile)
  )
;; Projectile:1 ends here

;; [[file:../synthmacs.org::*Rg][Rg:1]]
(use-package rg
  :ensure-system-package rg)
;; Rg:1 ends here

;; [[file:../synthmacs.org::*undo-fu][undo-fu:1]]
(use-package undo-fu
  :demand
  :general
  (:states 'normal
           "u" 'undo-fu-only-undo
           "s-z" 'undo-fu-only-undo
           "\C-r" 'undo-fu-only-redo))
;; undo-fu:1 ends here

;; [[file:../synthmacs.org::*WS-Butler][WS-Butler:1]]
(use-package ws-butler
  :init
  ;; (ws-butler-global-mode 1)
  (add-hook 'prog-mode-hook #'ws-butler-mode))
;; WS-Butler:1 ends here

;; [[file:../synthmacs.org::*Olivetti Mode][Olivetti Mode:1]]
(use-package olivetti
  :general
  (synthmacs/leader-keys
    "to" 'olivetti-mode)
  :init
  (setq olivetti-body-width 100)
  (setq olivetti-recall-visual-line-mode-entry-state t))
;; Olivetti Mode:1 ends here

;; [[file:../synthmacs.org::*vterm][vterm:1]]
(use-package vterm
  :general
  (general-imap
    :keymaps 'vterm-mode-map
    "M-l" 'vterm-send-right
    "M-h" 'vterm-send-left)
  :config
  (setq vterm-shell (executable-find "zsh")
        vterm-max-scrollback 10000))
;; vterm:1 ends here

;; [[file:../synthmacs.org::*vterm-toggle][vterm-toggle:1]]
(use-package vterm-toggle
  :general
  (synthmacs/leader-keys
    "'" 'vterm-toggle)
  )
;; vterm-toggle:1 ends here

;; [[file:../synthmacs.org::*restart-emacs][restart-emacs:1]]
(defun synthmacs/reload-init ()
  "Reload init.el."
  (interactive)
  (message "Reloading init.el...")
  (load user-init-file nil 'nomessage)
  (message "Reloading init.el... done."))

(use-package restart-emacs
  :commands restart-emacs
  :general
  (synthmacs/leader-keys
    "qR" 'restart-emacs
    "qn" 'restart-emacs-start-new-emacs
    "qr" 'synthmacs/reload-init)
  )
;; restart-emacs:1 ends here

;; [[file:../synthmacs.org::*Prescient][Prescient:1]]
(use-package prescient
  :defer 1
  :config
  (prescient-persist-mode 1))

(use-package corfu-prescient)

(use-package vertico-prescient)
;; Prescient:1 ends here

;; [[file:../synthmacs.org::*Transient][Transient:1]]
(use-package transient
  :general
  (synthmacs/leader-keys
    "h h" 'synthmacs/help-transient)
  :config
  (transient-define-prefix synthmacs/help-transient ()
    ["Help Commands"
     ["Mode & Bindings"
      ("m" "Mode" describe-mode)
      ("b" "Major Bindings" which-key-show-full-major-mode)
      ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
      ("d" "Descbinds" describe-bindings)
      ]
     ["Describe"
      ("c" "Command" helpful-command)
      ("f" "Function" helpful-callable)
      ("v" "Variable" helpful-variable)
      ("k" "Key" helpful-key)
      ]
     ["Info on"
      ("C-c" "Emacs Command" Info-goto-emacs-command-node)
      ("C-f" "Function" info-lookup-symbol) 
      ("C-v" "Variable" info-lookup-symbol)
      ("C-k" "Emacs Key" Info-goto-emacs-key-command-node)
      ]
     ["Goto Source"
      ("L" "Library" find-library)
      ("F" "Function" find-function)
      ("V" "Variable" find-variable)
      ("K" "Key" find-function-on-key)
      ]
     ]
    [
     ["Internals"
      ("e" "Echo Messages" view-echo-area-messages)
      ("l" "Lossage" view-lossage)
      ]
     ["Describe"
      ("s" "Symbol" helpful-symbol)
      ("." "At Point" helpful-at-point)
      ;; ("C-f" "Face" counsel-describe-face)
      ("w" "Where Is" where-is)
      ("=" "Position" what-cursor-position)
      ]
     ["Info Manuals"
      ("C-i" "Info" info)
      ("C-4" "Other Window" info-other-window)
      ("C-e" "Emacs" info-emacs-manual)
      ;; ("C-l" "Elisp" info-elisp-manual)
      ]
     ["Exit"
      ("q" "Quit" transient-quit-one)
      ("<escape>" "Quit" transient-quit-one)
      ]
     ;; ["External"
     ;;  ("W" "Dictionary" lookup-word-at-point)
     ;;  ("D" "Dash" dash-at-point)
     ;;  ]
     ]
    )
  )
;; Transient:1 ends here

;; [[file:../synthmacs.org::*Request][Request:1]]
(use-package request
  :defer t)
;; Request:1 ends here

;; [[file:../synthmacs.org::*Spinner][Spinner:1]]
(use-package spinner
  :defer t)
;; Spinner:1 ends here

;; [[file:../synthmacs.org::*synthmacs-tools][synthmacs-tools:1]]
(provide 'synthmacs-tools)
;;; synthmacs-tools.el ends here
;; synthmacs-tools:1 ends here
