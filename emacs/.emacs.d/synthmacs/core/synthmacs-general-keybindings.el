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
  "bh" '(dashboard-refresh-buffer :wk "Home buffer")
  "bm" '((lambda ()
	   (interactive)
	   (switch-to-buffer " *Message-Log*"))
	 :wk "Messages buffer")
  "bn" '(next-buffer :wk "Next buffer")
  "bp" '(previous-buffer :wk "Previous buffer")
  "br" '(revert-buffer :wk "Reload buffer")
  "bs" '(scratch-buffer :wk "Scratch buffer")
  "bu" '(synthmacs/reopen-killed-buffer :wk "Reopen last killed buffer"))

;; Compilation bindings
(synthmacs/leader-keys
  "c" '(:ignore t :wk "compile")
  "cf" '(find-file :wk "Find file"))

;; Compilation bindings
(synthmacs/leader-keys
  "d" '(:ignore t :wk "debug")
  "df" '(find-file :wk "Find file"))

;; Error bindings
(synthmacs/leader-keys
  "e" '(:ignore t :wk "errors")
  "ef" '(find-file :wk "Find file"))

;; Files bindings
(synthmacs/leader-keys
  "f" '(:ignore t :wk "files")
  "f." '(find-file-at-point :wk "find-file-at-point")
  "ff" '(find-file :wk "find-file")
  "fl" '(find-file-literally :wk "find-file-literally")
  "fr" '(consult-recent-file :wk "recent file")
  "fR" '(synthmacs/rename-current-buffer-file :wk "Rename file")
  "fs" '(save-buffer :wk "save file"))

(synthmacs/leader-keys
  "fy" '(:ignore t :wk "yank")
  "fyb" '(synthmacs/copy-buffer-name :wk "buffer name")
  "fyd" '(synthmacs/copy-directory-path :wk "directory path")
  "fyf" '(synthmacs/copy-file-path :wk "file path")
  "fyl" '(synthmacs/copy-file-path-with-line :wk "file path with line number")
  "fyn" '(synthmacs/copy-file-name :wk "file name")
  "fyN" '(synthmacs/copy-file-name-base :wk "file name without extension"))

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

;; Frame bindings
(synthmacs/leader-keys
  "F" '(:ignore t :wk "Frames")
  "Fd" '(delete-frame :wk "delete-frame")
  "FD" '(delete-other-frames :wk "delete-other-frames")
  "Fn" '(make-frame :wk "make-frame"))

;; Git bindings
(synthmacs/leader-keys
  "g" '(:ignore t :wk "git"))

;; Help bindings
(synthmacs/leader-keys
  "h" '(:ignore t :wk "help")
  "hp" '(describe-package :wk "describe-package")
  "hM" '(describe-mode :wk "describe-mode (Major)")
  "hm" '(describe-minor-mode :wk "describe-minor-mode")

  "hE" '(:ignore t :wk "Emacs")
  "hEf" '(view-emacs-FAQ :wk "view-emacs-faq")
  "hEm" '(info-emacs-manual :wk "info-emacs-manual")
  "hEn" '(view-emacs-news :wk "view-emacs-news")
  "hEp" '(view-emacs-problems :wk "view-emacs-problems")
  "hEt" '(view-emacs-todo :wk "view-emacs-todo"))

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

;; Register bindings
(synthmacs/leader-keys
  "r" '(:ignore t :wk "registers")
  "re" '(evil-show-registers :wk "evil-show-registers")
  "rk" '(consult-yank-from-kill-ring :wk "consult-yank-from-kill-ring"))

;; Search bindings
(synthmacs/leader-keys
  "s" '(:ignore t :wk "search")
  "sc" '(evil-ex-nohighlight :wk "clear-search-highlights"))

;; Toggles bindings
(synthmacs/leader-keys
  "t" '(:ignore t :wk "toggles")
  "ta" '(global-corfu-mode :wk "auto-completion")
  "tc" '(global-display-fill-column-indicator-mode :wk "fill-column")
  "tp" '(smartparens-global-mode :wk "smartparens-global-mode")
  "tt" '(toggle-truncate-lines :wk "truncate-lines")
  "tv" '(visual-line-mode :wk "visual-line-mode")
  "tw" '(global-whitespace-mode :wk "global-whitespace-mode")
  "tz" '(zone :wk "zone"))

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
