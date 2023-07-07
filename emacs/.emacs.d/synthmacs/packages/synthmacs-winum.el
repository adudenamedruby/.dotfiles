(use-package winum
  :init
  (setq winum-auto-assign-0-to-minibuffer nil
	winum-auto-setup-mode-line nil
	winum-ignored-buffers '(" *which-key*"))
  (winum-mode))
  ;; From spacemacs, group which-key descriptions
  ;; :after which-key
  ;; ;;   (push '(("\\(.*\\)0" . "winum-select-window-0-or-10") .
  ;; ;; 	    ("\\10" . "select window 0 or 10"))
  ;; ;; 	  which-key-replacement-alist)
  ;; ;; (push '(("\\(.*\\)1" . "winum-select-window-1") .
  ;; ;; 	  ("\\11..9" . "select window 1..9"))
  ;; ;; 	which-key-replacement-alist)
  ;; (dolist (item '(;; rename winum-select-window-1 entry to 1..9
  ;; 		  (("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "select window 1..9"))
  ;; 		  ;; hide winum-select-window-[2-9] entries
  ;; 		  ((nil . "winum-select-window-[2-9]") . t)))
  ;;   (cl-pushnew item which-key-replacement-alist :test #'equal)))
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

(provide 'synthmacs-winum)
