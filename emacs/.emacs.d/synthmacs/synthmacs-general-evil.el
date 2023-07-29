;; [[file:../synthmacs.org::*General][General:1]]
(use-package general
  :demand t
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
  (general-nmap
    :states 'nermal
    "gD" '(xref-find-references :wk "xref-references"))
  )
;; General:1 ends here

;; [[file:../synthmacs.org::*Application bindings][Application bindings:1]]
(synthmacs/leader-keys
  "a" '(:ignore t :wk "applications"))
;; Application bindings:1 ends here

;; [[file:../synthmacs.org::*Buffer bindings][Buffer bindings:1]]
(synthmacs/leader-keys
  "b" '(:ignore t :wk "buffer")
  "bd" 'kill-current-buffer
  "bh" 'dashboard-refresh-buffer
  "bm" '((lambda ()
	   (interactive)
	   (switch-to-buffer " *Message-Log*"))
	 :wk "Messages buffer")
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "br" 'revert-buffer
  "bs" 'scratch-buffer
  "bu" '(synthmacs/reopen-killed-buffer :wk "Reopen last killed buffer")
  )
;; Buffer bindings:1 ends here

;; [[file:../synthmacs.org::*Compilation bindings][Compilation bindings:1]]
(synthmacs/leader-keys
  "c" '(:ignore t :wk "code/compile")
  )
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
  "fR" '(synthmacs/rename-current-buffer-file :wk "Rename file")
  "fs" '(save-buffer :wk "save file"))

(synthmacs/leader-keys
  "fy" '(:ignore t :wk "yank")
  "fyb" '(synthmacs//copy-buffer-name :wk "buffer name")
  "fyd" '(synthmacs//copy-directory-path :wk "directory path")
  "fyf" '(synthmacs//copy-file-path :wk "file path")
  "fyl" '(synthmacs//copy-file-path-with-line :wk "file path with line number")
  "fyn" '(synthmacs//copy-file-name :wk "file name")
  "fyN" '(synthmacs//copy-file-name-base :wk "file name without extension"))

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
  "hp" 'describe-package
  "hM" '(describe-mode :wk "describe-mode (Major)")
  "hm" 'describe-minor-mode
  "he" 'view-echo-area-messages
  "hF" 'describe-face
  "hl" 'view-lossage
  "hL" 'find-library
  "hK" 'describe-keymap

  "hE" '(:ignore t :wk "Emacs")
  "hEf" 'view-emacs-FAQ
  "hEm" 'info-emacs-manual
  "hEn" 'view-emacs-news
  "hEp" 'view-emacs-problems
  "hEt" 'view-emacs-todo
  )
;; Help bindings:1 ends here

;; [[file:../synthmacs.org::*Jump bindings][Jump bindings:1]]
(synthmacs/leader-keys
  "j" '(:ignore t :wk "jump/join/split"))
;; Jump bindings:1 ends here

;; [[file:../synthmacs.org::*M-x bindings][M-x bindings:1]]
(synthmacs/leader-keys
  "SPC" '(:ignore t :wk "M-x")
  "SPC" '(execute-extended-command :wk "M-x")
  "TAB" '(synthmacs/alternate-buffer :wk "last buffer")
  "<escape>" 'keyboard-escape-quit
  )
;; "'" '(execute-extended-command :wk "open shell")
;; M-x bindings:1 ends here

;; [[file:../synthmacs.org::*Major-mode bindings][Major-mode bindings:1]]
(synthmacs/leader-keys
  "m" '(:ignore t :wk "major mode"))
;; Major-mode bindings:1 ends here

;; [[file:../synthmacs.org::*Org bindings][Org bindings:1]]
(synthmacs/leader-keys
  "o" '(:ignore t :wk "org")
  )
;; Org bindings:1 ends here

;; [[file:../synthmacs.org::*Project bindings][Project bindings:1]]
(synthmacs/leader-keys
  "p" '(:ignore t :wk "project"))
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
  "tp" 'smartparens-global-mode
  "tl" '(toggle-truncate-lines :wk "truncate-lines")
  "tv" 'visual-line-mode
  "tw" 'global-whitespace-mode
  "tz" 'zone
  )
;; Toggles bindings:1 ends here

;; [[file:../synthmacs.org::*User bindings][User bindings:1]]
(synthmacs/leader-keys
  "u" '(:ignore t :wk "user bindings"))
;; User bindings:1 ends here

;; [[file:../synthmacs.org::*Window bindings][Window bindings:1]]
(synthmacs/leader-keys
  "w" '(:ignore t :wk "window")

  "wo" '(synthmacs/window-enlargen :wk "enlargen")
  "wm" 'maximize-window

  "w{" 'shrink-window
  "w[" 'shrink-window-horizontally
  "w}" 'expand-window
  "w]" 'expand-window-horizontally
  )
;; Window bindings:1 ends here

;; [[file:../synthmacs.org::*Hydra][Hydra:1]]
(use-package hydra
  :general
  (synthmacs/leader-keys
    "tf" '(hydra/text-scale/body :wk "font size")))

(defhydra hydra/text-scale (:timeout 7)
  "
^Zoom Menu
^^^^^^^^----------------------
_+_: text-scale-increase
_-_: text-scale-decrease
_q_: quit
"
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil :exit t))


;; Buffer menu
;; (defhydra hydra/buffer-menu (:color pink
;;                              :hint nil)
;;   "
;; ^Mark^             ^Unmark^           ^Actions^          ^Search
;; ^^^^^^^^-----------------------------------------------------------------
;; _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
;; _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
;; _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
;; _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
;; _~_: modified
;; "
;;   ("m" Buffer-menu-mark)
;;   ("u" Buffer-menu-unmark)
;;   ("U" Buffer-menu-backup-unmark)
;;   ("d" Buffer-menu-delete)
;;   ("D" Buffer-menu-delete-backwards)
;;   ("s" Buffer-menu-save)
;;   ("~" Buffer-menu-not-modified)
;;   ("x" Buffer-menu-execute)
;;   ("b" Buffer-menu-bury)
;;   ("g" revert-buffer)
;;   ("T" Buffer-menu-toggle-files-only)
;;   ("O" Buffer-menu-multi-occur :color blue)
;;   ("I" Buffer-menu-isearch-buffers :color blue)
;;   ("R" Buffer-menu-isearch-buffers-regexp :color blue)
;;   ("c" nil "cancel")
;;   ("v" Buffer-menu-select "select" :color blue)
;;   ("o" Buffer-menu-other-window "other-window" :color blue)
;;   ("q" quit-window "quit" :color blue))

;; (synthmacs/leader-keys
;;   "bl" '(buffer-menu :wk "buffer list"))

;; ;; More keymaps
;; (general-define-key
;;  :keymaps 'Buffer-menu-mode-map
;;  "C-?" 'hydra/buffer-menu/body)
;; Hydra:1 ends here

;; [[file:../synthmacs.org::*evil][evil:1]]
(use-package evil
  :demand
  :general
  (synthmacs/leader-keys
    "wd" '(evil-window-delete :wk "delete-window")
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit
    "wh" 'evil-window-left 
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wn" 'evil-window-next 
    "wp" 'evil-window-prev)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-persistent-highlight t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map "`" 'evil-goto-mark-line)
  (define-key evil-normal-state-map "'" 'evil-goto-mark)
  (define-key evil-visual-state-map "'" 'evil-goto-mark)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )
;; evil:1 ends here

;; [[file:../synthmacs.org::*evil-anzu][evil-anzu:1]]
(use-package evil-anzu
  :init (global-anzu-mode t)
  :config
  (setq anzu-search-threshold 1000
	anzu-cons-mode-line-p nil))
;; evil-anzu:1 ends here

;; [[file:../synthmacs.org::*evil-args][evil-args:1]]
(use-package evil-args
  :demand
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))
;; evil-args:1 ends here

;; [[file:../synthmacs.org::*evil-cleverparens][evil-cleverparens:1]]
(use-package evil-cleverparens
  :init
  (setq evil-cleverparens-use-regular-insert t)
  :config
  ;; `evil-cp-change` should move to point
  (evil-set-command-properties 'evil-cp-change :move-point t))
;; evil-cleverparens:1 ends here

;; [[file:../synthmacs.org::*evil-collection][evil-collection:1]]
(use-package evil-collection
  :after evil
  :demand
  :init
  (setq evil-collection-magit-use-z-for-folds nil)
  :config
  (evil-collection-init))
;; evil-collection:1 ends here

;; [[file:../synthmacs.org::*evil-commentary][evil-commentary:1]]
(use-package evil-commentary
  :demand
  :config
  (evil-commentary-mode))
;; evil-commentary:1 ends here

;; [[file:../synthmacs.org::*evil-exchange][evil-exchange:1]]
(use-package evil-exchange
  :config
  (setq evil-exchange-key (kbd "gx"))
  (setq evil-exchange-cancel-key (kbd "gX"))
  (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
  (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
  (define-key evil-normal-state-map evil-exchange-cancel-key
              'evil-exchange-cancel)
  (define-key evil-visual-state-map evil-exchange-cancel-key
              'evil-exchange-cancel))
;; evil-exchange:1 ends here

;; [[file:../synthmacs.org::*evil-goggles][evil-goggles:1]]
(use-package evil-goggles
  :after evil
  :demand
  :init
  ;; disable pulses as it is more distracting than useful and
  ;; less readable.
  (setq evil-goggles-pulse nil
	evil-goggles-async-duration 0.1
	evil-goggles-blocking-duration 0.05)
  :config
  (push '(evil-operator-eval
	  :face evil-goggles-yank-face
	  :switch evil-goggles-enable-yank
	  :advice evil-goggles--generic-async-advice)
	evil-goggles--commands)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))
;; evil-goggles:1 ends here

;; [[file:../synthmacs.org::*evil-iedit-state][evil-iedit-state:1]]
(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil)
  :general
  (synthmacs/leader-keys
    "se" '(evil-iedit-state/iedit-mode :wk "iedit-mode")
    "sq" '(evil-iedit-state/quit-iedit-mode :wk "quit-iedit-mode"))
  )
;; evil-iedit-state:1 ends here

;; [[file:../synthmacs.org::*evil-lion][evil-lion:1]]
(use-package evil-lion
  :init
  (evil-define-key '(normal visual) 'global
    "gl" #'evil-lion-left
    "gL" #'evil-lion-right)
  :config
  (evil-lion-mode))
;; evil-lion:1 ends here

;; [[file:../synthmacs.org::*evil-lisp-state][evil-lisp-state:1]]
;; https://github.com/syl20bnr/evil-lisp-state
;; (use-package evil-lisp-state
;; :defer t
;; :init
;; (add-hook 'prog-mode-hook 'synthmacs/load-evil-lisp-state)
;; (setq evil-lisp-state-global t))

;; (synthmacs/leader-keys
;;   "l" '(evil-lisp-state-map :wk "lisp-state"))
;; (spacemacs/declare-prefix
;;   "l" "lisp"
;;   "ld" "delete"
;;   "lD" "delete-backward"
;;   "l`" "hybrid")))

;; (defun synthmacs/load-evil-lisp-state ()
;;     "Loads evil-lisp-state lazily"
;;   (remove-hook 'prog-mode-hook #'synthmacs/load-evil-lisp-state))
;; evil-lisp-state:1 ends here

;; [[file:../synthmacs.org::*evil-matchit][evil-matchit:1]]
(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))
;; evil-matchit:1 ends here

;; [[file:../synthmacs.org::*evil-numbers][evil-numbers:1]]
(use-package evil-numbers
  :defer t
  :general
  (synthmacs/leader-keys
    "n" '(synthmacs/hydra/numbers-state/body :wk "numbers")))

(defhydra synthmacs/hydra/numbers-state (:timeout 7)
  "
^Numbers Menu
^^^^^^^^----------------------
_+_: increment-at-point
_-_: decrement-at-point
_q_: quit
"
  ("+" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt)
  ("q" nil :exit t))
;; evil-numbers:1 ends here

;; [[file:../synthmacs.org::*evil-surround][evil-surround:1]]
(use-package evil-surround
  :init
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
  :config
  (global-evil-surround-mode 1))
;; evil-surround:1 ends here

;; [[file:../synthmacs.org::*evil-textobj-line][evil-textobj-line:1]]
(use-package evil-textobj-line)
;; evil-textobj-line:1 ends here

;; [[file:../synthmacs.org::*evil-visual-mark-mode][evil-visual-mark-mode:1]]
(use-package evil-visual-mark-mode
  :defer t
  :general
  (synthmacs/leader-keys
    "t`" '(evil-visual-mark-mode :wk "visual-mark-mode")))
;; evil-visual-mark-mode:1 ends here

;; [[file:../synthmacs.org::*evil-visualstar][evil-visualstar:1]]
(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (define-key evil-visual-state-map (kbd "*") 'evil-visualstar/begin-search-forward)
  (define-key evil-visual-state-map (kbd "#") 'evil-visualstar/begin-search-backward))
;; evil-visualstar:1 ends here

;; [[file:../synthmacs.org::*evil-vimish-fold][evil-vimish-fold:1]]
;; Evil-Vimish-Fold - https://github.com/alexmurray/evil-vimish-fold
;;(use-package evil-vimish-fold
;;  :after vimish-fold
;;  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))
;; evil-vimish-fold:1 ends here

;; [[file:../synthmacs.org::*synthmacs-general-evil][synthmacs-general-evil:1]]
(provide 'synthmacs-general-evil)
;;; synthmacs-general-evil.el ends here
;; synthmacs-general-evil:1 ends here
