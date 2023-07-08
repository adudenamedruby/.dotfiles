;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; M-x bindingns
(synthmacs/leader-keys
  "SPC" '(:ignore t :wk "M-x")
  "SPC" '(execute-extended-command :wk "M-x")
  "TAB" '(synthmacs/alternate-buffer :wk "last buffer"))
  ;; "'" '(execute-extended-command :wk "open shell")

;; Application bindings
(synthmacs/leader-keys
  "a" '(:ignore t :wk "applications")
  "af" '(find-file :wk "Find file"))

;; Buffer bindings
(synthmacs/leader-keys
  "b" '(:ignore t :wk "buffer")
  "bb" '(switch-to-buffer :wk "Switch buffer")
  "bd" '(kill-this-buffer :wk "Kill this buffer")
  "bm" '((lambda ()
	   (interactive)
	   (switch-to-buffer " *Message-Log*"))
	 :wk "Messages buffer")
  "bn" '(next-buffer :wk "Next buffer")
  "bp" '(previous-buffer :wk "Previous buffer")
  "br" '(revert-buffer :wk "Reload buffer")
  "bs" '(scratch-buffer :wk "Scratch buffer")
  "bu" '(synthmacs/reopen-killed-buffer :wk "Reopen last killed buffer"))

;; Compilation bindingns
(synthmacs/leader-keys
  "c" '(:ignore t :wk "compile")
  "cf" '(find-file :wk "Find file"))

;; Error bindings
(synthmacs/leader-keys
  "e" '(:ignore t :wk "errors")
  "ef" '(find-file :wk "Find file"))

;; Files bindings
(synthmacs/leader-keys
  "f" '(:ignore t :wk "files")
  "f." '(find-file-at-point :wk "find-file-at-point")
  "ff" '(find-file :wk "find-file")
  "fl" '(find-file-literally :wk "find-file")
  "fs" '(save-buffer :wk "save file"))

;; Files (Emacs) bindings
(synthmacs/leader-keys
  "fe" '(:ignore t :wk "Emacs Files")
  "fed" '((lambda ()
	    (interactive)
	    (find-file "~/.emacs.d/synthmacs/synthmacs-init.el"))
	  :wk "synthmacs-init.el")
  "fei" '(
	  (lambda ()
	    (interactive)
	    (find-file "~/.emacs.d/init.el"))
	  :wk "init.el")
  "fee" '(
	  (lambda ()
	    (interactive)
	    (find-file "~/.emacs.d/early-init.el"))
	  :wk "early-init.el")
  )


;; Git bindings
(synthmacs/leader-keys
  "g" '(:ignore t :wk "git"))

;; Help bindings
(synthmacs/leader-keys
  "h" '(:ignore t :wk "help")
  "hp" '(describe-package :wk "describe-package"))

(synthmacs/leader-keys
  "hdm" '(:ignore t :wk "describe modes")
  "hdmM" '(describe-mode :wk "describe-mode (Major)")
  "hdmm" '(describe-minor-mode :wk "describe-minor-mode"))

;; Jump bindings
(synthmacs/leader-keys
  "j" '(:ignore t :wk "jump/join/split")
  "jf" '(find-file :wk "Find file"))

;; Major mode bindings
(synthmacs/leader-keys
  "m" '(:ignore t :wk "major mode")
  "mf" '(find-file :wk "Find file"))

;; Org bindings
(synthmacs/leader-keys
  "o" '(:ignore t :wk "org")
  "of" '(find-file :wk "Find file"))

;; User Bindings
(synthmacs/leader-keys
  "u" '(:ignore t :wk "user bindings")
  "uf" '(find-file :wk "Find file"))

;; Project bindings
(synthmacs/leader-keys
  "p" '(:ignore t :wk "projects"))

;; Quit bindings
(synthmacs/leader-keys
  "q" '(:ignore t :wk "quit")
  "qq" '(synthmacs/prompt-kill-emacs :wk "prompt-kill-emacs")
  "qs" '(save-buffers-kill-emacs :wk "save-buffers-kill-emacs")
  "qQ" '(kill-emacs :wk "kill-emacs")
  "qR" '(restart-emacs :wk "restart-emacs"))

;; Search bindings
(synthmacs/leader-keys
  "s" '(:ignore t :wk "search"))

;; Toggles bindings
(synthmacs/leader-keys
  "t" '(:ignore t :wk "toggles"))

;; Windows bindings
(synthmacs/leader-keys
  "w" '(:ignore t :wk "window")

  "wd" '(evil-window-delete :wk "delete-window")
  "wD" '(delete-other-windows :wk "delete-other-windows")

  "ws" '(evil-window-split :wk "horizontal split window")
  "wv" '(evil-window-vsplit :wk "vertical split window")

  "wm" '(maximize-window :wk "maximize-window")

  ;; Window motions
  "wh" '(evil-window-left :wk "window left")
  "wj" '(evil-window-down :wk "window down")
  "wk" '(evil-window-up :wk "window up")
  "wl" '(evil-window-right :wk "window right")
  "wn" '(evil-window-next :wk "go to next window")
  "wp" '(evil-window-prev :wk "go to previous window")

  ;; Move Windows
  "wH" '(synthmacs/buf-move-left :wk "window move left")
  "wJ" '(synthmacs/buf-move-down :wk "window move down")
  "wK" '(synthmacs/buf-move-up :wk "window move up")
  "wL" '(synthmacs/buf-move-right :wk "window move right"))

(provide 'synthmacs-general-keybindings)
