;; [[file:../synthmacs.org::*LSP][LSP:1]]
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; LSP:1 ends here

;; [[file:../synthmacs.org::*Corfu][Corfu:1]]
(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
		   :includes (corfu-popupinfo))
  :hook (corfu-mode . corfu-popupinfo-mode)
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-auto-delay 0.0)
  ;; (corfu-auto-prefix 0)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  (corfu-popupinfo-delay 1)
  (corfu-popupinfo-max-height 15)
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :general
  (synthmacs/leader-keys
  "ta" '(global-corfu-mode :wk "auto-completion"))
  :init
  (global-corfu-mode)
  :config
  (keymap-set corfu-map "C-d" 'corfu-popupinfo-scroll-down)
  (keymap-set corfu-map "C-u" 'corfu-popupinfo-scroll-up)
  (keymap-set corfu-map "C-i" 'corfu-popupinfo-toggle))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;; Corfu:1 ends here

;; [[file:../synthmacs.org::*Flycheck][Flycheck:1]]
(use-package flycheck
  :init (global-flycheck-mode))
;; Flycheck:1 ends here

;; [[file:../synthmacs.org::*iMenu][iMenu:1]]
(use-package imenu-list)

(synthmacs/leader-keys
  "ti" '(imenu-list-smart-toggle :wx "imenu-list-smart-toggle"))
;; iMenu:1 ends here

;; [[file:../synthmacs.org::*Snippets][Snippets:1]]
(use-package yasnippet)

;; (yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(use-package yasnippet-snippets)
(use-package common-lisp-snippets)
;; Snippets:1 ends here

;; [[file:../synthmacs.org::*Treesitter][Treesitter:1]]
(require 'treesit)

(use-package tree-sitter-langs)

;; Load the language bundle
(require 'tree-sitter-langs)

;; (setq treesit-language-source-alist
;;    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;      (c "https://github.com/tree-sitter/tree-sitter-c")
;;      (cmake "https://github.com/uyha/tree-sitter-cmake")
;;      (clojure "https://github.com/sogaiu/tree-sitter-clojure")
;;      (commonlisp "https://github.com/theHamsta/tree-sitter-commonlisp")
;;      (css "https://github.com/tree-sitter/tree-sitter-css")
;;      (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
;;      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;      (go "https://github.com/tree-sitter/tree-sitter-go")
;;      (html "https://github.com/tree-sitter/tree-sitter-html")
;;      (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
;;      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;      (json "https://github.com/tree-sitter/tree-sitter-json")
;;      (lua "https://github.com/Azganoth/tree-sitter-lua")
;;      (make "https://github.com/alemuller/tree-sitter-make")
;;      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;      (objc "https://github.com/jiyee/tree-sitter-objc")
;;      ;; (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
;;      (python "https://github.com/tree-sitter/tree-sitter-python")
;;      (racket "https://github.com/6cdh/tree-sitter-racket")
;;      (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
;;      (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;      (scheme "https://github.com/6cdh/tree-sitter-scheme")
;;      (sqlite "https://github.com/dhcmrlchtdj/tree-sitter-sqlite")
;;      (sql "https://github.com/m-novikov/tree-sitter-sql")
;;      ;; (swift "https://gitlab.com/woolsweater/tree-sitter-swifter")
;;      (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")
;;      (zig "https://github.com/maxxnino/tree-sitter-zig")))

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
;; Treesitter:1 ends here

;; [[file:../synthmacs.org::*synthmacs-programming][synthmacs-programming:1]]
(provide 'synthmacs-programming)
;;; synthmacs-programming.el ends here
;; synthmacs-programming:1 ends here
