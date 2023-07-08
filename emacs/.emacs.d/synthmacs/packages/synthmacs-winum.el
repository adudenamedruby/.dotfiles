(use-package winum
  :general
  (synthmacs/leader-keys
    "1" '(winum-select-window-1 :wk "winum-select-window-1")
    "2" '(winum-select-window-2 :wk "winum-select-window-2")
    "3" '(winum-select-window-3 :wk "winum-select-window-3")
    "4" '(winum-select-window-4 :wk "winum-select-window-4")
    "5" '(winum-select-window-5 :wk "winum-select-window-5")
    "6" '(winum-select-window-6 :wk "winum-select-window-6")
    "7" '(winum-select-window-7 :wk "winum-select-window-7")
    "8" '(winum-select-window-8 :wk "winum-select-window-8")
    "9" '(winum-select-window-9 :wk "winum-select-window-9"))
  :init
  (setq winum-auto-setup-mode-line nil
	winum-ignored-buffers '(" *which-key*"))
  (winum-mode))

(provide 'synthmacs-winum)
