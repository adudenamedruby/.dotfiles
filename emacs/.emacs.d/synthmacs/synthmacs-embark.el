(use-package embark
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; (setq embark-prompter 'embark-completing-read-prompter)
  ;; (setq embark-indicators '(embark-minimal-indicator))
  ;; Sets the buffer at the bottom. But then it must be scrolled with C-M-v or C-M-S-v
  ;; (setq embark-verbose-indicator-display-action '(display-buffer-at-bottom))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

;; Definind keys here, because there's some weird overlaps with an evil mode
;; key "C-." that I never use.
(general-define-key
 :states 'normal
 "C-." nil)
(general-define-key 
  "C-." 'embark-act)
;; (general-define-key
;;   :keymaps 'embark-general-map
;;   "G" . synthmacs/embark-google-search)
(synthmacs/leader-keys
  ;; help
  "hb" '(embark-bindings :wk "embark-bindings"))

(defun synthmacs/embark-google-search (term)
  "An action to search for anything at point in Google."
    (interactive "sSearch Term: ")
    (browse-url
     (format "http://google.com/search?q=%s" term)))

;; --------------- Use Embark in WhichKey -------------
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

;; (setq embark-indicators
;;   '(embark-which-key-indicator
;;     embark-highlight-indicator
;;     embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

;; (advice-add #'embark-completing-read-prompter
;;             :around #'embark-hide-which-key-indicator)

;; -------- Package actions using Straight -------------
(defvar-keymap embark-straight-map
  :parent embark-general-map
  "u" #'straight-visit-package-website
  "r" #'straight-get-recipe
  "i" #'straight-use-package
  "c" #'straight-check-package
  "F" #'straight-pull-package
  "f" #'straight-fetch-package
  "p" #'straight-push-package
  "n" #'straight-normalize-package
  "m" #'straight-merge-package)

(add-to-list 'embark-keymap-alist '(straight . embark-straight-map))

(add-to-list 'marginalia-prompt-categories '("recipe\\|package" . straight))
(use-package embark-consult
   ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep)

(provide 'synthmacs-embark)
