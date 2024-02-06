;; [[file:../synthmacs.org::*synthmacs-programming][synthmacs-programming:1]]
;;; synthmacs-programming.el --- Synthmacs Programming Environment

;;; Commentary:

;; Set up conditions for programming modes in Synthmacs

;;; Code:
;; synthmacs-programming:1 ends here

;; [[file:../synthmacs.org::*lsp-mode][lsp-mode:1]]
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((prog-mode . lsp-deferred)
   (lsp-mode . (lambda () (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))
   (lsp-mode . lsp-enable-which-key-integration))
  ;; :general
  ;; (synthmacs/local-leader-keys
  ;;   :states 'normal
  ;;   :keymaps 'lsp-mode-map
  ;;   "i" '(:ignore t :which-key "import")
  ;;   "io" '(lsp-organize-imports :wk "optimize")
  ;;   "l" '(:keymap lsp-command-map :wk "lsp")
  ;;   "a" '(lsp-execute-code-action :wk "code action")  
  ;;   "r" '(lsp-rename :wk "rename"))
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; (setq lsp-restart 'ignore)
  ;;   (setq lsp-eldoc-enable-hover nil)
  ;;   (setq lsp-enable-file-watchers nil)
  ;;   (setq lsp-signature-auto-activate nil)
  ;;   (setq lsp-modeline-diagnostics-enable nil)
  ;;   (setq lsp-keep-workspace-alive nil)
  ;;   (setq lsp-auto-execute-action nil)
  ;;   (setq lsp-before-save-edits nil)
  ;;   (setq lsp-headerline-breadcrumb-enable nil)
  ;;   (setq lsp-diagnostics-provider :none)
  )
;; lsp-mode:1 ends here

;; [[file:../synthmacs.org::*lsp-ui][lsp-ui:1]]
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :general
  (synthmacs/local-leader-keys
    "h" 'lsp-ui-doc-show
    "H" 'lsp-ui-doc-hide)
  (lsp-ui-peek-mode-map
   :states 'normal
   "C-n" 'lsp-ui-peek--select-next
   "C-p" 'lsp-ui-peek--select-prev)
  (outline-mode-map
   :states 'normal
   "C-j" 'nil
   "C-k" 'nil)
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-peek-fontify 'always)
  :custom
  (lsp-ui-doc-position 'bottom)
  )
;; lsp-ui:1 ends here

;; [[file:../synthmacs.org::*lsp-ui][lsp-ui:2]]
(use-package lsp-treemacs
  :after lsp)
;; lsp-ui:2 ends here

;; [[file:../synthmacs.org::*Flycheck][Flycheck:1]]
(use-package flycheck
  :defer 1
  :init
  (global-flycheck-mode t))

;; (use-package flycheck
;;  :hook (prog-mode . flycheck-mode)
;;  :diminish t
;;  :bind
;;   ("C-c e n" . flycheck-next-error)
;;   ("C-c e p" . flycheck-previous-error)
;;   :custom
;;   (setq flycheck-checker-error-threshold 15)
;;   ;; (setq flymake-show-diagnostics-at-end-of-line 'short)
;;   )

;; (use-package flycheck-inline
;;   :hook (flycheck-mode . flycheck-inline-mode))

(use-package flycheck-posframe
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p))
;; Flycheck:1 ends here

;; [[file:../synthmacs.org::*iMenu][iMenu:1]]
(use-package imenu-list)

(synthmacs/leader-keys
  "ti" '(imenu-list-smart-toggle :wx "imenu-list-smart-toggle"))
;; iMenu:1 ends here

;; [[file:../synthmacs.org::*yasnippet][yasnippet:1]]
(use-package yasnippet
  ;; :general
  ;; (yas-minor-mode-map
  ;; :states 'insert
  ;; "TAB" 'nil
  ;; "C-TAB" 'yas-expand)
  :hook
  ((prog-mode org-mode dap-ui-repl-mode vterm-mode) . yas-minor-mode)
  :init
  (defun synthmacs/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  :config
  (yas-reload-all)
  (add-hook 'post-command-hook #'synthmacs/yas-try-expanding-auto-snippets)
  )

(use-package yasnippet-snippets)
;; yasnippet:1 ends here

;; [[file:../synthmacs.org::*commond-lisp-snippets][commond-lisp-snippets:1]]
(use-package common-lisp-snippets)
;; commond-lisp-snippets:1 ends here

;; [[file:../synthmacs.org::*Apheleia][Apheleia:1]]
(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(swift-mode . swift-format))
  (add-to-list 'apheleia-formatters '(swift-format "swift-format" (buffer-file-name)))
  (apheleia-global-mode +1))
;; Apheleia:1 ends here

;; [[file:../synthmacs.org::*expand-region][expand-region:1]]
(use-package expand-region
  :bind ("C-=" . er/expand-region))
;; expand-region:1 ends here

;; [[file:../synthmacs.org::*Treesitter languages][Treesitter languages:1]]
;; (use-package treesit-auto
  ;; :demand t)

;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (c "https://github.com/tree-sitter/tree-sitter-c")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake")
;;         (clojure "https://github.com/sogaiu/tree-sitter-clojure")
;;         (commonlisp "https://github.com/theHamsta/tree-sitter-commonlisp")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (lua "https://github.com/Azganoth/tree-sitter-lua")
;;         (make "https://github.com/alemuller/tree-sitter-make")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (objc "https://github.com/jiyee/tree-sitter-objc")
;;         ;; (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")
;;         (racket "https://github.com/6cdh/tree-sitter-racket")
;;         (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
;;         (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;         (scheme "https://github.com/6cdh/tree-sitter-scheme")
;;         (sqlite "https://github.com/dhcmrlchtdj/tree-sitter-sqlite")
;;         (sql "https://github.com/m-novikov/tree-sitter-sql")
;;         (swift "https://gitlab.com/woolsweater/tree-sitter-swifter")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

      ;; (defun synthmacs/treesit-install-all-languages ()
      ;;     "Install all languages specified by `treesit-language-source-alist'."
      ;;     (interactive)
      ;;     (let ((languages (mapcar 'car treesit-language-source-alist)))
      ;;       (dolist (lang languages)
      ;; 	      (treesit-install-language-grammar lang)
      ;; 	      (message "`%s' parser was installed." lang)
      ;; 	      (sit-for 0.75))))

      ;; (add-hook 'c-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'c t)
      ;; 		    (c-ts-mode)
      ;; 		  (c-mode))))

      ;; (add-hook 'common-lisp-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'commonlisp t)
      ;; 		    (common-lisp-ts-mode)
      ;; 		  (common-lisp-mode))))

      ;; (add-hook 'clojure-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'clojure t)
      ;; 		    (clojure-ts-mode)
      ;; 		  (clojure-mode))))

      ;; (add-hook 'css-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'css t)
      ;; 		    (css-ts-mode)
      ;; 		  (css-mode))))

      ;; (add-hook 'html-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'html t)
      ;; 		    (html-ts-mode)
      ;; 		  (html-mode))))

      ;; (add-hook 'haskell-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'haskell t)
      ;; 		    (haskell-ts-mode)
      ;; 		  (haskell-mode))))

      ;; (add-hook 'javascript-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'javascript t)
      ;; 		    (js-ts-mode)
      ;; 		  (javascript-mode))))

      ;; (add-hook 'js-json-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'json t)
      ;; 		    (json-ts-mode)
      ;; 		  (js-json-mode))))

      ;; (add-hook 'python-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'python t)
      ;; 		    (python-ts-mode)
      ;; 		  (python-mode))))

      ;; (add-hook 'sh-mode-hook
      ;; 	  (lambda () (if (treesit-ready-p 'bash t)
      ;; 		    (bash-ts-mode)
      ;; 		  (sh-mode))))
;; Treesitter languages:1 ends here

;; [[file:../synthmacs.org::*Treesitter config][Treesitter config:1]]
(use-package tree-sitter
  :hook
  (
   (sh-mode . tree-sitter-mode)
   (c-mode . tree-sitter-mode)
   (clojure-mode . tree-sitter-mode)
   (common-lisp-mode . tree-sitter-mode)
   (css-mode . tree-sitter-mode)
   (elixir-mode . tree-sitter-mode)
   ;; elisp-mode
   (html-mode . tree-sitter-mode)
   ;; haskell-mode
   (js-mode . tree-sitter-mode)
   (json-mode . tree-sitter-mode)
   (lua-mode . tree-sitter-mode)
   ;; make-mode
   (markdown-mode . tree-sitter-mode)
   (objc-mode . tree-sitter-mode)
   (python-mode . tree-sitter-mode)
   (racket-mode . tree-sitter-mode)
   (ruby-mode . tree-sitter-mode)
   (rust-mode . tree-sitter-mode)
   (scheme-mode . tree-sitter-mode)
   ;; sqlite-mode
   (sql-mode . tree-sitter-mode)
   (swift-mode . tree-sitter-mode)
   ;; toml-mode
   (yaml-mode . tree-sitter-mode))

  :config
  (setq tsc-dyn-get-from nil)
  (setq tree-sitter-hl-use-font-lock-keywords t
        tree-sitter-hl-enable-query-region-extension nil)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)
;; Treesitter config:1 ends here

;; [[file:../synthmacs.org::*synthmacs-programming][synthmacs-programming:1]]
(provide 'synthmacs-programming)
;;; synthmacs-programming.el ends here
;; synthmacs-programming:1 ends here
