;; The main file that handles the startup.

;; Everything else is esentially inside their respective folders
;; - core        Files essential to running Emacs, excluding packages
;;
;; - packages    Files outlining what packages to load. Each pagckage
;;               should be in its own file, with minor exceptions where
;;               packages are interrelated deeply enough that they can
;;               be considered one package. Settings and keybindings
;;               related to a package can also be found in these files.
;;
;; - modules     These are sets of things
;;
;; - assets      Miscellaneous files.

(message "SynthMacs is powering up, please be patient...")

(defun synthmacs-recursive-add-to-load-path (dir)
  "Add DIR and all its sub-directories to `load-path'."
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir))
    (let ((name (expand-file-name f dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (synthmacs-recursive-add-to-load-path name)))))

;; Setup paths for files, recursively
(defvar synthmacs-dir (expand-file-name (file-name-directory load-file-name)))

(synthmacs-recursive-add-to-load-path synthmacs-dir)

;; Load core configs
(require 'synthmacs-general-settings)
(require 'synthmacs-variables)
(require 'synthmacs-functions)
(require 'synthmacs-straight)

;; Load packages
(require 'synthmacs-general)
(require 'synthmacs-hydra)
(require 'synthmacs-evil)
(require 'synthmacs-general-keybindings)
(require 'synthmacs-winum)
(require 'synthmacs-vertico)
(require 'synthmacs-orderless)
(require 'synthmacs-consult)
(require 'synthmacs-marginalia)
(require 'synthmacs-embark)
(require 'synthmacs-dirvish)
(require 'synthmacs-icons)
(require 'synthmacs-modeline)
(require 'synthmacs-themes)
(require 'synthmacs-which-key)
(require 'synthmacs-rainbow)
(require 'synthmacs-helpful)
(require 'synthmacs-rg)
(require 'synthmacs-projectile)
(require 'synthmacs-git)
(require 'synthmacs-dashboard)
(require 'synthmacs-org)
(require 'synthmacs-avy)
(require 'synthmacs-treesitter)
(require 'synthmacs-lsp)
(require 'synthmacs-corfu)
(require 'synthmacs-flycheck)

;; Load modules
;; (require 'synthmacs-swift)


;; (setq gc-cons-threshold 16777216
;;       gc-cons-percentage 0.1)))

(provide 'synthmacs-init)

;;; synthmacs-init.el ends here
