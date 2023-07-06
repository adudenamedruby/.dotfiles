(require 'straight)
(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer synthmacs/leader-keys
    :states '(normal insert visual emacs)

    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC")) ;; access leader in insert mode

(provide 'synthmacs-general)
