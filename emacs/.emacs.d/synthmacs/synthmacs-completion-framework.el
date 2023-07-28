(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                ;; vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :demand
  :general
  ;; (:keymaps 'minibuffer-local-map
  (:keymaps 'vertico-map
	    "C-~" #'synthmacs/minibuffer-backwards-kill
	    "C-h" #'vertico-directory-delete-word
	    )
  
  ;; :hook
  ;; ((minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved for `vertico-repeat'
  ;;  (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
  ;;  ) 
  :init
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  :config
  (vertico-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  )

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Focus the help window when bringing it up, so that I can quit it easily
  (setq help-window-select t)
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :general
  (:keymaps 'minibuffer-local-map
	    "M-A" 'marginalia-cycle)
  :init
  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)
  (with-eval-after-load 'projectile
    (add-to-list 'marginalia-command-categories '(projectile-find-file . file)))
  )

(use-package embark
  :after vertico
  :general
  (general-define-key
   :states 'normal
   "C-." nil)
  (general-nmap "C-." 'embark-act)
  (vertico-map "C-." #'embark-act)
  (:keymaps 'embark-general-map
	    "G" #'synthmacs/embark-google-search)
  (synthmacs/leader-keys
    "hb" '(embark-bindings :wk "embark-bindings"))

  :init
  (defun synthmacs/embark-google-search (term)
    "An action to search for anything at point in Google."
    (interactive "sSearch Term: ")
    (browse-url
     (format "http://google.com/search?q=%s" term)))


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
;; (defvar-keymap embark-straight-map
;;   :parent embark-general-map
;;   "u" #'straight-visit-package-website
;;   "r" #'straight-get-recipe
;;   "i" #'straight-use-package
;;   "c" #'straight-check-package
;;   "F" #'straight-pull-package
;;   "f" #'straight-fetch-package
;;   "p" #'straight-push-package
;;   "n" #'straight-normalize-package
;;   "m" #'straight-merge-package)

;; (add-to-list 'embark-keymap-alist '(straight . embark-straight-map))

(add-to-list 'marginalia-prompt-categories '("recipe\\|package" . straight))

(use-package embark-consult
  :after (embark consult)
    ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  ;; :general
  ;; (grep-mode-may "W" 'wgrep-change-to-wgrep-mode)
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  )

(provide 'synthmacs-completion-framework)
;;; synthmacs-completion-framework.el ends here
