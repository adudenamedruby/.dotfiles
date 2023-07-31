;; [[file:../synthmacs.org::*nerd-icons][nerd-icons:1]]
(use-package nerd-icons
  :init
  (setq nerd-icons-scale-factor 1.2)
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "FiraCode Nerd Font"))
;; nerd-icons:1 ends here

;; [[file:../synthmacs.org::*all-the-icons][all-the-icons:1]]
(use-package all-the-icons
  :if (display-graphic-p)
  :demand
  )
;; all-the-icons:1 ends here

;; [[file:../synthmacs.org::*all-the-icons-completion][all-the-icons-completion:1]]
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))
;; all-the-icons-completion:1 ends here

;; [[file:../synthmacs.org::*A variety of themes][A variety of themes:1]]
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package afternoon-theme)
(use-package alect-themes)
(use-package ample-theme)
(use-package ample-zen-theme)
(use-package badwolf-theme)
(use-package catppuccin-theme)
(use-package clues-theme)
(use-package color-theme-sanityinc-solarized)
(use-package color-theme-sanityinc-tomorrow)
(use-package cyberpunk-theme)
(use-package darktooth-theme)
(use-package flatland-theme)
(use-package gruvbox-theme)
(use-package jazz-theme)
(use-package kaolin-themes)
(use-package material-theme)
(use-package modus-themes)
(use-package monokai-theme)
(use-package seti-theme)
(use-package soothe-theme)
(use-package subatomic-theme)
(use-package sublime-themes)
;; A variety of themes:1 ends here

;; [[file:../synthmacs.org::*Theme functions][Theme functions:1]]
(defvar synthmacs--fallback-theme 'kaolin-bubblegum
  "Fallback theme if user theme cannot be applied.")

(defvar synthmacs--cur-theme nil
  "Internal variable storing currently loaded theme.")

(defvar synthmacs--user-themes '(kaolin-bubblegum
				 doom-challenger-deep
				 cyberpunk
				 jazz
				 afternoon
				 ample-zen
				 doom-1337
				 catppuccin
				 manoj-dark
				 doom-snazzy
				 kaolin-dark
				 doom-gruvbox
				 doom-old-hope
				 kaolin-aurora
				 doom-acario-dark
				 gruvbox-dark-hard
				 modus-vivendi
				 alect-black
				 modus-operandi
				 gruvbox-light-hard))

(defun synthmacs/load-theme (&optional theme)
  "Apply user theme."
  (if theme
      (progn
	(load-theme theme t)
	(setq-default synthmacs--cur-theme theme))
    (progn
      (load-theme synthmacs--fallback-theme t)
      (setq-default spacemacs--cur-theme synthmacs--fallback-theme))))

(defun synthmacs/load-random-theme ()
  (interactive)
  (let* ((size (length synthmacs--user-themes))
         (index (random size))
         (randomTheme (nth index synthmacs--user-themes)))
    (synthmacs/load-theme randomTheme)))

(defun synthmacs/cycle-synthmacs-theme (&optional backward)
  "Cycle through themes defined in `synthmacs-themes'.
When BACKWARD is non-nil, or with universal-argument, cycle backwards."
  (interactive "P")
  (let* (
	 ;; (theme-names (mapcar 'synthmacs--user-themes)
         (themes (if backward
		     (reverse synthmacs--user-themes)
		   synthmacs--user-themes))
         (next-theme
	  (car (or (cdr (memq synthmacs--cur-theme themes))
		   ;; if current theme isn't in cycleable themes, start
		   ;; over
		   themes))))
    (when synthmacs--cur-theme
      (disable-theme synthmacs--cur-theme))
    (let ((progress-reporter
           (make-progress-reporter
            (format "Loading theme %s..." next-theme))))
      (synthmacs/load-theme next-theme)
      (progress-reporter-done progress-reporter))))

(defun synthmacs/cycle-synthmacs-theme-backward ()
  "Cycle through themes defined in `dotsynthmacs-themes' backward."
  (interactive)
  (synthmacs/cycle-synthmacs-theme t))

(synthmacs/leader-keys
  "tt" '(:ignore t :wk "themes")
  "ttn" '(synthmacs/hydra-theme-cycle :wk "cycle-themes")
  "ttN" '(synthmacs/hydra-theme-cycle-backward :wk "cycle-themes-backwards")
  "ttr" '(synthmacs/hydra-theme-random :wk "random-theme"))

(defun synthmacs/hydra-theme-cycle ()
  (interactive)
  (synthmacs/cycle-synthmacs-theme)
  (synthmacs/hydra/cycle-themes/body))

(defun synthmacs/hydra-theme-cycle-backward ()
  (interactive)
  (synthmacs/cycle-synthmacs-theme t)
  (synthmacs/hydra/cycle-themes/body))

(defun synthmacs/hydra-theme-random ()
  (interactive)
  (synthmacs/load-random-theme)
  (synthmacs/hydra/cycle-themes/body))

(defhydra synthmacs/hydra/cycle-themes (:timeout 20)
  "
^Themes Menu
^^^^^^^^------------------------
[_n_]     cycle-theme
[_p_/_N_]   cycle-theme-backward
[_r_]     random-theme
[_q_] quit
"
  ("n" synthmacs/cycle-synthmacs-theme)
  ("p" synthmacs/cycle-synthmacs-theme-backward)
  ("N" synthmacs/cycle-synthmacs-theme-backward)
  ("r" synthmacs/load-random-theme)
  ("q" nil :exit t))

(synthmacs/load-random-theme)
;; Theme functions:1 ends here

;; [[file:../synthmacs.org::*solaire-mode][solaire-mode:1]]
(use-package solaire-mode
  :defer 1
  :hook
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  (change-major-mode . turn-on-solaire-mode)
  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  (after-revert . turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (ediff-prepare-buffer . solaire-mode)
  :custom
  (solaire-mode-auto-swap-bg t)
  :config
  (solaire-global-mode +1))
;; solaire-mode:1 ends here

;; [[file:../synthmacs.org::*info-colors][info-colors:1]]
(use-package info-colors
  :defer 1
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))
;; info-colors:1 ends here

;; [[file:../synthmacs.org::*Winum][Winum:1]]
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
;; Winum:1 ends here

;; [[file:../synthmacs.org::*ace-window][ace-window:1]]
(use-package ace-window
  :general
  (synthmacs/leader-keys
    "wD" '(ace-delete-window :wk "ace-delete-window")
    "wS" '(ace-swap-window :wk "ace-swap-window")
    ))
;; ace-window:1 ends here

;; [[file:../synthmacs.org::*Avy][Avy:1]]
(use-package avy
  :general
  (synthmacs/leader-keys
    "jj" '(avy-goto-char-timer :wk "avy-goto-char-timer")
    "jc" '(avy-goto-char :wk "avy-goto-char")
    "jl" '(avy-goto-line :wk "avy-goto-line")
    ))
;; Avy:1 ends here

;; [[file:../synthmacs.org::*minions][minions:1]]
(use-package minions
  :hook (doom-modeline-mode . minions-mode))
;; minions:1 ends here

;; [[file:../synthmacs.org::*doom-modeline][doom-modeline:1]]
(use-package doom-modeline
  :demand
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 45)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-vcs-max-length 15)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-modal-icon t)
  )
;; doom-modeline:1 ends here

;; [[file:../synthmacs.org::*Dashboard][Dashboard:1]]
(use-package dashboard
  :demand
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/.dotfiles/emacs/.emacs.d/synthmacs/assets/logo.txt")
  (setq dashboard-banner-logo-title "adudenamedruby's Emacs")
  ;; (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-items '((recents  . 5)
			  (projects . 5)))
  (setq dashboard-set-navigator t)
  :config
  (dashboard-setup-startup-hook)
  )
;; Dashboard:1 ends here

;; [[file:../synthmacs.org::*Ligatures][Ligatures:1]]
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :defer 1
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures
   'prog-mode
   '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
     "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
     "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
     "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
     "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
     "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
     "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
     "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
     ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
     "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
     "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
     "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
     "&="))
  (global-ligature-mode t))
;; Ligatures:1 ends here

;; [[file:../synthmacs.org::*Colours in compilation buffer][Colours in compilation buffer:1]]
(defun synthmacs/colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'synthmacs/colorize-compilation-buffer)
;; Colours in compilation buffer:1 ends here

;; [[file:../synthmacs.org::*which-key][which-key:1]]
(setq which-key-idle-delay 0.4)
(use-package which-key
  :demand
  :general
  (synthmacs/leader-keys
    "?" 'which-key-show-top-level)
  :init
  (setq which-key-side-window-location 'bottom)
  ;; (setq which-key-sort-order #'which-key-key-order-alpha)
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (setq which-key-sort-uppercase-first nil)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-display-columns nil)
  (setq which-key-min-display-lines 6)
  (setq which-key-side-window-slot -10)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-max-description-length 25)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-separator " → ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1)
  ;; (which-key-setup-minibuffer)
  ;; Rename the entry for M-1 in the SPC h k Top-level bindings,
  ;; and for 1 in the SPC- Spacemacs root, to 1..9
  (push '(("\\(.*\\)1" . "winum-select-window-1") .
	  ("\\11..9" . "select window 1..9"))
	which-key-replacement-alist)

  ;; Hide the entries for M-[2-9] in the SPC h k Top-level bindings,
  ;; and for [2-9] in the SPC- Spacemacs root
  (push '((nil . "winum-select-window-[2-9]") . t)
	which-key-replacement-alist))
;; which-key:1 ends here

;; [[file:../synthmacs.org::*rainbow-delimiters][rainbow-delimiters:1]]
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; rainbow-delimiters:1 ends here

;; [[file:../synthmacs.org::*Popup management][Popup management:1]]
(use-package emacs
  :init
  (setq display-buffer-alist
        `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.33)
           (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))
  )
;; Popup management:1 ends here

;; [[file:../synthmacs.org::*centered-cursor-mode][centered-cursor-mode:1]]
(use-package centered-cursor-mode
  :general
  (synthmacs/leader-keys
    "t=" '((lambda () (interactive) (centered-cursor-mode 'toggle)) :wk "center cursor")
    )
  )
;; centered-cursor-mode:1 ends here

;; [[file:../synthmacs.org::*highlight-indent-guides][highlight-indent-guides:1]]
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  ;; (setq highlight-indent-guides-method 'column)
  ;; (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?|)
  (setq highlight-indent-guides-responsive 'top)
  ;; (setq highlight-indent-guides-responsive 'stack)
  ;; (setq highlight-indent-guides-auto-enabled nil)
  ;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  )
;; highlight-indent-guides:1 ends here

;; [[file:../synthmacs.org::*Line numbers][Line numbers:1]]
(use-package emacs
  :init
  ;; set type of line numbering (global variable)
  (setq display-line-numbers-type 'relative)
  ;; activate line numbering in all buffers/modes
  (global-display-line-numbers-mode 1)

  (dolist (mode '(org-mode-hook
		  term-mode-hook
		  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  ;; Activate line numbering in programming modes
  ;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  )
;; Line numbers:1 ends here

;; [[file:../synthmacs.org::*Fill column & modeline column info][Fill column & modeline column info:1]]
(use-package emacs
  ;; :hook (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column 85)
  (global-display-fill-column-indicator-mode)

  ;; Columns number in the modeline
  (setq column-number-mode t)
  )
;; Fill column & modeline column info:1 ends here

;; [[file:../synthmacs.org::*synthmacs-ui][synthmacs-ui:1]]
(provide 'synthmacs-ui)
;;; synthmacs-ui.el ends here
;; synthmacs-ui:1 ends here
