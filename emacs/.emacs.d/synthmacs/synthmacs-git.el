;; https://magit.vc/
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

(provide 'synthmacs-git)
