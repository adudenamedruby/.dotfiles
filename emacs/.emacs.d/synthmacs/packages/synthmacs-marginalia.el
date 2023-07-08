(use-package marginalia
  :init
  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(general-define-key
 ;; NOTE: keymaps specified with :keymaps must be quoted
 :keymaps 'minibuffer-local-map
 "M-A" 'marginalia-cycle)

(provide 'synthmacs-marginalia)
