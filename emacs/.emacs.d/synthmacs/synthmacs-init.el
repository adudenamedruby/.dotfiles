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

(require 'synthmacs-core)
(require 'synthmacs-packages)
(require 'synthmacs-modules)

;; iEdit - https://github.com/victorhge/iedit


;; (setq gc-cons-threshold 16777216
;;       gc-cons-percentage 0.1)))

(provide 'synthmacs-init)
