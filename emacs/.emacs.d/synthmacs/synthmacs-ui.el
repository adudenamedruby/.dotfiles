;; [[file:../synthmacs.org::*Themes][Themes:1]]
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

(use-package solaire-mode
  :init
  (solaire-global-mode +1))


;;; Functions:
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
;; Themes:1 ends here

;; [[file:../synthmacs.org::*synthmacs-ui][synthmacs-ui:1]]
(provide 'synthmacs-ui)
;;; synthmacs-ui.el ends here
;; synthmacs-ui:1 ends here
