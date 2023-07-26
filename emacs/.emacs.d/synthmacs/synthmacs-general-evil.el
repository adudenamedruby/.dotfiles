;; [[file:../synthmacs.org::*General][General:1]]
(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer synthmacs/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (general-create-definer synthmacs/local-leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix ","
    :global-prefix "SPC m")
  )
;; General:1 ends here

;; [[file:../synthmacs.org::*General keybindings][General keybindings:1]]
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; General keybindings:1 ends here

;; [[file:../synthmacs.org::*Application bindings][Application bindings:1]]
;; Application bindings
(synthmacs/leader-keys
  "a" '(:ignore t :wk "applications")
  "af" '(find-file :wk "Find file"))
;; Application bindings:1 ends here

;; [[file:../synthmacs.org::*Buffer bindings][Buffer bindings:1]]
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
;; Buffer bindings:1 ends here

;; [[file:../synthmacs.org::*Compilation bindings][Compilation bindings:1]]
(synthmacs/leader-keys
  "c" '(:ignore t :wk "compile")
  "cf" '(find-file :wk "Find file"))
;; Compilation bindings:1 ends here

;; [[file:../synthmacs.org::*Debug bindings][Debug bindings:1]]
(synthmacs/leader-keys
  "d" '(:ignore t :wk "debug")
  "df" '(find-file :wk "Find file"))
;; Debug bindings:1 ends here

;; [[file:../synthmacs.org::*Error bindings][Error bindings:1]]
(synthmacs/leader-keys
  "e" '(:ignore t :wk "errors")
  "ef" '(find-file :wk "Find file"))
;; Error bindings:1 ends here

;; [[file:../synthmacs.org::*Files bindings][Files bindings:1]]
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

(synthmacs/leader-keys
  "fe" '(:ignore t :wk "Emacs Files")
  "fed" '((lambda ()
	    (interactive)
	    (find-file "~/.emacs.d/synthmacs.org"))
	  :wk "synthmacs.org"))
;; Files bindings:1 ends here

;; [[file:../synthmacs.org::*Frame bindings][Frame bindings:1]]
(synthmacs/leader-keys
  "F" '(:ignore t :wk "Frames")
  "Fd" '(delete-frame :wk "delete-frame")
  "FD" '(delete-other-frames :wk "delete-other-frames")
  "Fn" '(make-frame :wk "make-frame"))
;; Frame bindings:1 ends here

;; [[file:../synthmacs.org::*Git/VCS bindings][Git/VCS bindings:1]]
(synthmacs/leader-keys
  "g" '(:ignore t :wk "git"))
;; Git/VCS bindings:1 ends here

;; [[file:../synthmacs.org::*Help bindings][Help bindings:1]]
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
;; Help bindings:1 ends here

;; [[file:../synthmacs.org::*Jump bindings][Jump bindings:1]]
(synthmacs/leader-keys
  "j" '(:ignore t :wk "jump/join/split"))
;; Jump bindings:1 ends here

;; [[file:../synthmacs.org::*M-x bindings][M-x bindings:1]]
(synthmacs/leader-keys
  "SPC" '(:ignore t :wk "M-x")
  "SPC" '(execute-extended-command :wk "M-x")
  "TAB" '(synthmacs/alternate-buffer :wk "last buffer"))
  ;; "'" '(execute-extended-command :wk "open shell")
;; M-x bindings:1 ends here

;; [[file:../synthmacs.org::*Major-mode bindings][Major-mode bindings:1]]
(synthmacs/leader-keys
  "m" '(:ignore t :wk "major mode"))
;; Major-mode bindings:1 ends here

;; [[file:../synthmacs.org::*Org bindings][Org bindings:1]]
(synthmacs/leader-keys
  "o" '(:ignore t :wk "org"))
;; Org bindings:1 ends here

;; [[file:../synthmacs.org::*Project bindings][Project bindings:1]]
(synthmacs/leader-keys
  "p" '(:ignore t :wk "projects"))
;; Project bindings:1 ends here

;; [[file:../synthmacs.org::*Quit bindings][Quit bindings:1]]
(synthmacs/leader-keys
  "q" '(:ignore t :wk "quit")
  "qq" '(synthmacs/prompt-kill-emacs :wk "prompt-kill-emacs")
  "qs" '(save-buffers-kill-emacs :wk "save-buffers-kill-emacs")
  "qQ" '(kill-emacs :wk "kill-emacs")
  "qR" '(restart-emacs :wk "restart-emacs"))
;; Quit bindings:1 ends here

;; [[file:../synthmacs.org::*Registers bindings][Registers bindings:1]]
(synthmacs/leader-keys
  "r" '(:ignore t :wk "registers")
  "re" '(evil-show-registers :wk "evil-show-registers")
  "rk" '(consult-yank-from-kill-ring :wk "consult-yank-from-kill-ring"))
;; Registers bindings:1 ends here

;; [[file:../synthmacs.org::*Search bindings][Search bindings:1]]
(synthmacs/leader-keys
  "s" '(:ignore t :wk "search")
  "sc" '(evil-ex-nohighlight :wk "clear-search-highlights"))
;; Search bindings:1 ends here

;; [[file:../synthmacs.org::*Toggles bindings][Toggles bindings:1]]
(synthmacs/leader-keys
  "t" '(:ignore t :wk "toggles")
  "tc" '(global-display-fill-column-indicator-mode :wk "fill-column")
  "tp" '(smartparens-global-mode :wk "smartparens-global-mode")
  "tl" '(toggle-truncate-lines :wk "truncate-lines")
  "tv" '(visual-line-mode :wk "visual-line-mode")
  "tw" '(global-whitespace-mode :wk "global-whitespace-mode")
  "tz" '(zone :wk "zone"))
;; Toggles bindings:1 ends here

;; [[file:../synthmacs.org::*User bindings][User bindings:1]]
(synthmacs/leader-keys
  "u" '(:ignore t :wk "user bindings"))
;; User bindings:1 ends here

;; [[file:../synthmacs.org::*Window bindings][Window bindings:1]]
(synthmacs/leader-keys
  "w" '(:ignore t :wk "window")

  "wd" '(evil-window-delete :wk "delete-window")

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
  "wL" '(synthmacs/buf-move-right :wk "window move right")

  "w{" '(shrink-window :wk "shrink-window")
  "w[" '(synthmacs/buf-move-down :wk "shrink-window-horizontally")
  "w}" '(synthmacs/buf-move-up :wk "expand-window")
  "w]" '(synthmacs/buf-move-right :wk "expand-window-horizontally"))
;; Window bindings:1 ends here

;; [[file:../synthmacs.org::*synthmacs-general-evil][synthmacs-general-evil:1]]
(provide 'synthmacs-general-evil)
;;; synthmacs-general-evil.el ends here
;; synthmacs-general-evil:1 ends here
