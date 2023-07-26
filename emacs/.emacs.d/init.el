;; [[file:synthmacs.org::*init.el: startup optimization][init.el: startup optimization:1]]
;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-
;; NOTE: init.el is now generated from synthmacs.org.  Please edit that file instead

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine)
(unless (daemonp)
  (defvar doom--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun doom-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist'
    ;; since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'doom--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist doom--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h)
  (add-hook 'after-init-hook '(lambda ()
				;; restore after startup
				(setq gc-cons-threshold 16777216
				      gc-cons-percentage 0.1)))
  )

;; Ensure Synthmacs is running out of this file's directory
(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))
;; init.el: startup optimization:1 ends here

;; [[file:synthmacs.org::*init.el: load modules][init.el: load modules:1]]
(message "SynthMacs is powering up, please be patient...")

;; (add-to-list 'load-path "~/.emacs.d/synthmacs/")
(add-to-list 'load-path (expand-file-name "synthmacs" user-emacs-directory))

(let ((file-name-handler-alist nil)
      (gc-cons-threshold 100000000))
  (require 'synthmacs-general-settings)
  (require 'synthmacs-variables)
  (require 'synthmacs-functions)

  (require 'synthmacs-core)
  (require 'synthmacs-package-management)
  (require 'synthmacs-general-evil)
  ;;(require 'synthmacs-org)
  ;;(require 'synthmacs-ui)
  ;;(require 'synthmacs-completion-framework)
  ;;(require 'synthmacs-tools)
  ;;(require 'synthmacs-programming)
  ;;(require 'synthmacs-lang-clojure)
  ;;(require 'synthmacs-lang-haskell)
  ;;(require 'synthmacs-lang-html)
  ;;(require 'synthmacs-lang-lisp)
  ;;(require 'synthmacs-lang-markdown)
  ;;(require 'synthmacs-lang-rust)
  ;;(require 'synthmacs-lang-swift)
  ;;(require 'synthmacs-lang-yaml)

  ;; Load packages
  ;;(require 'synthmacs-general)
  (require 'synthmacs-hydra)
  (require 'synthmacs-evil)
  ;;(require 'synthmacs-general-keybindings)
  (require 'synthmacs-themes)
  (require 'synthmacs-windows)
  (require 'synthmacs-vertico)
  (require 'synthmacs-orderless)
  (require 'synthmacs-consult)
  (require 'synthmacs-marginalia)
  (require 'synthmacs-embark)
  ;; (require 'synthmacs-dirvish)
  (require 'synthmacs-icons)
  (require 'synthmacs-modeline)
  (require 'synthmacs-which-key)
  (require 'synthmacs-rainbow)
  (require 'synthmacs-helpful)
  (require 'synthmacs-rg)
  (require 'synthmacs-projectile)
  (require 'synthmacs-git)
  (require 'synthmacs-dashboard)
  (require 'synthmacs-org)
  (require 'synthmacs-avy)
  ;; (require 'synthmacs-treesitter)
  (require 'synthmacs-lsp)
  (require 'synthmacs-corfu)
  (require 'synthmacs-flycheck)
  (require 'synthmacs-imenu)
  (require 'synthmacs-snippets)

  ;; Load modules
  ;; (require 'synthmacs-swift)
  ;; (require 'synthmacs-lisp)

  )

    ;;; init.el ends here
;; init.el: load modules:1 ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
	   (lambda nil
	     (progn
	       (synthmacs/org-add-ids-to-headlines-in-file)
	       (synthmacs/tangle-config)))
	   nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
