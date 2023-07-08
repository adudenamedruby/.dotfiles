(use-package hydra
  :general
  (synthmacs/leader-keys
    "tf" '(hydra/text-scale/body :wk "font size")))

(defhydra hydra/text-scale (:timeout 7)
	  "
^Zoom Menu
^^^^^^^^----------------------
_+_: text-scale-increase
_-_: text-scale-decrease
_q_: quit
"
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil :exit t))


;; Buffer menu
;; (defhydra hydra/buffer-menu (:color pink
;;                              :hint nil)
;;   "
;; ^Mark^             ^Unmark^           ^Actions^          ^Search
;; ^^^^^^^^-----------------------------------------------------------------
;; _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
;; _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
;; _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
;; _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
;; _~_: modified
;; "
;;   ("m" Buffer-menu-mark)
;;   ("u" Buffer-menu-unmark)
;;   ("U" Buffer-menu-backup-unmark)
;;   ("d" Buffer-menu-delete)
;;   ("D" Buffer-menu-delete-backwards)
;;   ("s" Buffer-menu-save)
;;   ("~" Buffer-menu-not-modified)
;;   ("x" Buffer-menu-execute)
;;   ("b" Buffer-menu-bury)
;;   ("g" revert-buffer)
;;   ("T" Buffer-menu-toggle-files-only)
;;   ("O" Buffer-menu-multi-occur :color blue)
;;   ("I" Buffer-menu-isearch-buffers :color blue)
;;   ("R" Buffer-menu-isearch-buffers-regexp :color blue)
;;   ("c" nil "cancel")
;;   ("v" Buffer-menu-select "select" :color blue)
;;   ("o" Buffer-menu-other-window "other-window" :color blue)
;;   ("q" quit-window "quit" :color blue))

;; (synthmacs/leader-keys
;;   "bl" '(buffer-menu :wk "buffer list"))

;; ;; More keymaps
;; (general-define-key
;;  :keymaps 'Buffer-menu-mode-map
;;  "C-?" 'hydra/buffer-menu/body)


(provide 'synthmacs-hydra)
