;; (evil-select-search-module 'evil-search-module 'evil-search)
(setq evil-search-module 'evil-search)

;; Don't use the clipboard for base yank/delete ops
(setq x-select-enable-clipboard nil)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-select-search-module 'evil-search-module 'evil-search)
  )

(use-package evil-collection
 :after evil
 :config
 (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; (setq evil-ex-search-persistent-highlight t)
;; (setq evil-search-module 'evil-anzu)

;; Anzu mode
(use-package anzu
  :init (global-anzu-mode +1))

(use-package evil-anzu
  :after 'evil)

;; LION - https://github.com/edkolev/evil-lion
;;(use-package evil-lion
;;  :config
;;  (evil-lion-mode))

;; Evil-Vimish-Fold - https://github.com/alexmurray/evil-vimish-fold
;;(use-package evil-vimish-fold
;;  :after vimish-fold
;;  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

(provide 'synthmacs-evil)
