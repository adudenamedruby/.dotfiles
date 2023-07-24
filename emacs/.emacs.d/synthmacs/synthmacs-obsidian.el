;; https://github.com/licht1stein/obsidian.el
;; (use-package obsidian
;;   :demand t
;;   :config
;;   (obsidian-specify-path "~/MY_OBSIDIAN_FOLDER")
;;   (global-obsidian-mode t)
;;   :custom
;;   ;; This directory will be used for `obsidian-capture' if set.
;;   ;; (obsidian-inbox-directory "Inbox")
;;   :bind (:map obsidian-mode-map
;;   ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
;;   ("C-c C-o" . obsidian-follow-link-at-point)
;;   ;; Jump to backlinks
;;   ("C-c C-b" . obsidian-backlink-jump)
;;   ;; If you prefer you can use `obsidian-insert-link'
;;   ("C-c C-l" . obsidian-insert-wikilink)))

;; Ideas for obsidian: https://github.com/0atman/noboilerplate/blob/main/scripts/29-obsidian.md

(provide 'synthmacs-obisdian)
