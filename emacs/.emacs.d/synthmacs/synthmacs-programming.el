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

(use-package flycheck
  :init (global-flycheck-mode))

(use-package imenu-list)

(synthmacs/leader-keys
  "ti" '(imenu-list-smart-toggle :wx "imenu-list-smart-toggle"))

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

(use-package common-lisp-snippets)

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

(use-package vterm
  :general
  (general-imap
    :keymaps 'vterm-mode-map
    "M-l" 'vterm-send-right
    "M-h" 'vterm-send-left)
  :config
  (setq vterm-shell (executable-find "zsh")
        vterm-max-scrollback 10000))

(use-package vterm-toggle
  :general
  (synthmacs/leader-keys
    "'" 'vterm-toggle)
  )

(provide 'synthmacs-programming)
;;; synthmacs-programming.el ends here
