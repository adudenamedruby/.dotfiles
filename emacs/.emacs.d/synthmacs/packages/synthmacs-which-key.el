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
	which-key-prefix-prefix "+")
  ;; Rename the entry for M-1 in the SPC h k Top-level bindings,
  ;; and for 1 in the SPC- Spacemacs root, to 1..9
  (push '(("\\(.*\\)1" . "winum-select-window-1") .
	  ("\\11..9" . "select window 1..9"))
	which-key-replacement-alist)

  ;; Hide the entries for M-[2-9] in the SPC h k Top-level bindings,
  ;; and for [2-9] in the SPC- Spacemacs root
  (push '((nil . "winum-select-window-[2-9]") . t)
	which-key-replacement-alist))
  ;; (dolist (item '(;; rename winum-select-window-1 entry to 1..9
  ;; 		  (("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "select window 1..9"))
  ;; 		  ;; hide winum-select-window-[2-9] entries
  ;; 		  ((nil . "winum-select-window-[2-9]") . t)))
  ;;   (push item which-key-replacement-alist)))

(provide 'synthmacs-which-key)
