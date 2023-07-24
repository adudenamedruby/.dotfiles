(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 45
	doom-modeline-project-detection 'auto
	doom-modeline-icon t
	doom-modeline-major-mode-icon t
	doom-modeline-major-mode-color-icon t
	doom-modeline-buffer-state-icon t
	doom-modeline-buffer-modification-icon t
	doom-modeline-time-icon nil
	doom-modeline-buffer-encoding t
	doom-modeline-vcs-max-length 15
	doom-modeline-lsp t
	doom-modeline-modal-icon t))

(provide 'synthmacs-modeline)
