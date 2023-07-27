;; [[file:../synthmacs.org::*Vertico][Vertico:1]]
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  :bind (:map minibuffer-local-map
	      ("C-h" . synthmacs/minibuffer-backwards-kill)))

;; -------------------- SaveHist -------------------
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; -------------------- Useful Emacs Configs for Vectico -------------------
;; A few more useful configurations...
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
;; Vertico:1 ends here

;; [[file:../synthmacs.org::*Orderless][Orderless:1]]
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;; Orderless:1 ends here

;; [[file:../synthmacs.org::*Consult][Consult:1]]
(use-package consult
  :general
  (synthmacs/leader-keys
    "/" '(synthmacs/consult-ripgrep :wk "search project")

    ;; files
    "fr" '(consult-recent-file :wk "recent files")

    ;; search
    "sp" '(synthmacs/consult-ripgrep :wk "search project (consult-ripgrep)")
    "ss" '(consult-line :wk "swoop")

    ;; toggles
    "ttt" '(consult-theme :wk "consult-themes"))


  ;; Replace bindings. Lazily loaded due by `use-package'.
  ;; :bind (;; C-c bindings in `mode-specific-map'
         ;; ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ;; ("C-c i" . consult-info)
         ;; ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ;; ("M-s d" . consult-find)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         ;; :map isearch-mode-map
         ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         ;; :map minibuffer-local-map
         ;; ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ;; ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
	 ;; )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  (setq consult-ripgrep-args "rg --null --line-buffered --no-ignore --hidden --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )
;; Consult:1 ends here

;; [[file:../synthmacs.org::*Marginalia][Marginalia:1]]
(use-package marginalia
  :init
  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(general-define-key
 ;; NOTE: keymaps specified with :keymaps must be quoted
 :keymaps 'minibuffer-local-map
 "M-A" 'marginalia-cycle)
;; Marginalia:1 ends here

;; [[file:../synthmacs.org::*Embark][Embark:1]]
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
   ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep)
;; Embark:1 ends here

;; [[file:../synthmacs.org::*synthmacs-completion-framework][synthmacs-completion-framework:1]]
(provide 'synthmacs-completion-framework)
;;; synthmacs-completion-framework.el ends here
;; synthmacs-completion-framework:1 ends here
