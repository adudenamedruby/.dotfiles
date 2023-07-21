;;;  package --- Summary

;;; Commentary:

;;; Code:
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

(use-package solaire-mode
  :init
  (solaire-global-mode +1))


;;; Functions:
(defvar synthmacs--fallback-theme 'doom-challenger-deep
  "Fallback theme if user theme cannot be applied.")

(defvar spacemacs--cur-theme nil
  "Internal variable storing currently loaded theme.")

(defvar synthmacs--user-themes '(doom-challenger-deep wombat))

(defun synthmacs/load-theme (theme)
  "Apply user theme."
    (condition-case err
        (progn
	  (load-theme theme t))
          (setq-default spacemacs--cur-theme theme))
      ('error
       (message "error: %s" err)
       (load-theme synthmacs--fallback-theme t)))

(defun synthmacs/cycle-synthmacs-theme (&optional backward)
  "Cycle through themes defined in `synthmacs-themes'.
When BACKWARD is non-nil, or with universal-argument, cycle backwards."
  (interactive "P")
  (let* ((theme-names 'synthmacs--user-themes)
         (themes (if backward
		     (reverse theme-names)
		   theme-names))
         (next-theme (car (or (cdr (memq synthmacs--cur-theme themes))
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

(provide 'synthmacs-themes)

;;; synthmacs-themes.el ends here
