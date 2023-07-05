(setq inhibit-startup-message t)

;; Always edit symlinked files under VC
(setq vc-follow-symlinks t)

;; UI Stuff
(scroll-bar-mode -1)    ; Disable
(tool-bar-mode -1)      ; Disable the toolbar    
(tooltip-mode -1)       ; Disable tooltips       
(set-fringe-mode 10)    ; Give some breathing room    
(menu-bar-mode -1)      ; Disable the menubar

;; Line Numbering
;; set type of line numbering (global variable)
(setq display-line-numbers-type 'relative) 

;; activate line numbering in all buffers/modes
(global-display-line-numbers-mode 1) 

(dolist (mode '(;;org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Columns!
(column-number-mode)

;; Electric indent mode messes up with a bunch of languages indenting.
;; So disable it.
(setq electric-indent-inhibit t)


;; Activate line numbering in programming modes
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-visual-line-mode t)

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 140)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Install straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default so you no
;; longer have to do :straight t to tell use-pacage to use straight
(use-package straight
  :custom
  (straight-use-package-by-default t))

(setq use-package-always-ensure t)

(setq evil-ex-search-persistent-highlight t)
;; (setq evil-search-module 'evil-anzu)

(setq evil-want-C-u-scroll t)
(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))

(use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))

(use-package evil-surround
  
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  
  :config
  (evil-commentary-mode))


;; LION - https://github.com/edkolev/evil-lion
;;(use-package evil-lion
;;  
;;  :config
;;  (evil-lion-mode))

;; Evil-Vimish-Fold - https://github.com/alexmurray/evil-vimish-fold
;;(use-package evil-vimish-fold
;;  :after vimish-fold
;;  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

;; Keybindings
;; General
(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer rubymacs/leader-keys
    :states '(normal insert visual emacs)

    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (rubymacs/leader-keys
    "SPC" '(:ignore t :wk "M-x")
    "SPC" '(execute-extended-command :wk "M-x")
    "TAB" '(rubymacs/alternate-buffer :wk "last buffer")
    "'" '(execute-extended-command :wk "open shell")
    "*" '(execute-extended-command :wk "search proj w/ input")
    "/" '(execute-extended-command :wk "search project"))

  (rubymacs/leader-keys
    "a" '(:ignore t :wk "applications")
    "af" '(find-file :wk "Find file"))

  ;; WhichKey buffers
  (rubymacs/leader-keys
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
    "bu" '(rubymacs/reopen-killed-buffer :wk "Reopen last killed buffer"))

  (rubymacs/leader-keys
    "c" '(:ignore t :wk "compile")
    "cf" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "e" '(:ignore t :wk "errors")
    "ef" '(find-file :wk "Find file"))

  ;; WhichKey files
  (rubymacs/leader-keys
    "f" '(:ignore t :wk "files")
    "ff" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "fe" '(:ignore t :wk "Emacs Files")
    "fed" '((lambda ()
	      (interactive)
	      (find-file "~/.emacs.d/rubymacs/rubymacs-init.el"))
	    :wk "init.el")
    "fei" '(
	    (lambda ()
	      (interactive)
	      (find-file "~/.emacs.d/init.el"))
	    :wk "early-init.el")
    )


  (rubymacs/leader-keys
    "g" '(:ignore t :wk "git")
    "gs" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "h" '(:ignore t :wk "help")
    "h." '(helpful-at-point :wk "helpful-at-point")
    "hb" '(describe-bindings :wk "describe-bindings")
    "hc" '(helpful-command :wk "describe-command")
    "hf" '(helpful-callable :wk "describe-function")
    "hk" '(helpful-key :wk "describe-key")
    "hp" '(describe-package :wk "describe-package")
    "hv" '(helpful-variable :wk "describe-variable"))

  (rubymacs/leader-keys
    "hm" '(:ignore t :wk "describe modes")
    "hmM" '(describe-mode :wk "describe-mode")
    "hmm" '(describe-mode :wk "describe-minor-mode"))

  (rubymacs/leader-keys
    "j" '(:ignore t :wk "jump/join/split")
    "jf" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "m" '(:ignore t :wk "major mode")
    "mf" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "o" '(:ignore t :wk "org")
    "of" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "u" '(:ignore t :wk "user bindings")
    "uf" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "p" '(:ignore t :wk "projects")
    "pf" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "q" '(:ignore t :wk "quit")
    "qf" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "s" '(:ignore t :wk "search")
    "ss" '(consult-line :wk "swoop"))

  (rubymacs/leader-keys
    "t" '(:ignore t :wk "toggles")
    "tf" '(find-file :wk "Find file"))

  (rubymacs/leader-keys
    "w" '(:ignore t :wk "window")
    ;; Window splits
    "wd" '(evil-window-delete :wk "Delete window")
    "wn" '(evil-window-new :wk "New window")
    "ws" '(evil-window-split :wk "Horizontal split window")
    "wv" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "wh" '(evil-window-left :wk "Window left")
    "wj" '(evil-window-down :wk "Window down")
    "wk" '(evil-window-up :wk "Window up")
    "wl" '(evil-window-right :wk "Window right")
    "wn" '(evil-window-next :wk "Go to next window")
    "wp" '(evil-window-prev :wk "Go to previous window")
    ;; Move Windows
    "wH" '(buf-move-left :wk "Buffer move left")
    "wJ" '(buf-move-down :wk "Buffer move down")
    "wK" '(buf-move-up :wk "Buffer move up")
    "wL" '(buf-move-right :wk "Buffer move right"))

  (rubymacs/leader-keys
    "z" '(:ignore t :wk "zoom")
    "zf" '(text-scale-adjust :wk "font size"))
  )

(setq 	which-key-idle-delay 0.4)
(use-package which-key
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	;; which-key-sort-order #'which-key-prefix-then-key-order
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-separator " → "
	which-key-prefix-prefix "+"))

;; Anzu mode
(use-package anzu
  
  :init (global-anzu-mode +1))

(use-package evil-anzu
  
  :after 'evil)

;; Modeline

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 40
	doom-modeline-project-detection 'auto
	doom-modeline-icon t
	doom-modeline-major-mode-icon t
	doom-modeline-major-mode-color-icon t
	doom-modeline-buffer-state-icon t
	doom-modeline-buffer-modification-icon t
	doom-modeline-time-icon nil
	doom-modeline-buffer-encoding t
	doom-modeline-vcs-max-length 15
	doom-modeline-lsp t
	doom-modeline-modal-icon t))

(setq nerd-icons-scale-factor 1.3)
(use-package nerd-icons
  
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "FiraCode Nerd Font"))

(use-package all-the-icons
  
  :if (display-graphic-p))

;; Themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-challenger-deep t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Enable vertico
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
  (setq vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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

;; Optionally use the `orderless' completion style.
(use-package orderless
  
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
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
	 )

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
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )







;; (use-package embark
;;   
;;
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;
;;   :init
;;
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;
;;   ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
;;   ;; strategy, if you want to see the documentation from multiple providers.
;;   (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
;;   ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
;;
;;   :config
;;
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))
;;
;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;    ; only need to install it, embark loads it after consult if found
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))
;;
;; iEdit - https://github.com/victorhge/iedit
;; avy -  https://github.com/abo-abo/av


;; Custom functions

(defun rubymacs/alternate-buffer (&optional window)
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (if (bound-and-true-p nil)
	  (let ((buffer-list (persp-buffer-list))
		(my-buffer (window-buffer window)))
	    (seq-find (lambda (it)
			(and (not (eq (car it) my-buffer))
			     (member (car it) buffer-list)))
		      (window-prev-buffers)
		      (list nil nil nil)))
	(or (cl-find (window-buffer window) (window-prev-buffers)
		     :key #'car :test-not #'eq)
	    (list (other-buffer) nil nil)))
    (if (not buf)
	(message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))




;; Moving windows around
(require 'windmove)

;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
;;  "Switches between the current buffer, and the buffer above the
;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-down ()
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-left ()
"Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-right ()
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


;; reopening the last killed buffer
(defvar rubymacs--killed-buffer-list nil
  "List of recently killed buffers.")

(defun rubymacs/add-buffer-to-killed-list ()
  "If buffer is associated with a file name, add that file
to the `killed-buffer-list` when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name rubymacs--killed-buffer-list)))

(add-hook 'kill-buffer-hook #'rubymacs/add-buffer-to-killed-list)

(defun rubymacs/reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when rubymacs--killed-buffer-list
    (find-file (pop rubymacs--killed-buffer-list))))


;; LISP rainbow delimiters!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful)

;; (setq gc-cons-threshold 16777216
;;       gc-cons-percentage 0.1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("570263442ce6735821600ec74a9b032bc5512ed4539faf61168f2fdf747e0668" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

