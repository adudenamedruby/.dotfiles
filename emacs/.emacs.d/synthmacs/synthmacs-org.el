;; [[file:../synthmacs.org::*org mode][org mode:1]]
(use-package org
  ;; :straight org-plus-contrib
  ;; :straight (:type built-in)
  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . visual-line-mode))
  :general
  ;; (lc/leader-keys
  ;;   "f t" '(org-babel-tangle :wk "tangle")
  ;;   "o C" '(org-capture :wk "capture")
  ;;   "o l" '(org-todo-list :wk "todo list")

  ;;   "o c" '((lambda () (interactive)
  ;;             (persp-switch "main")
  ;;             (find-file (concat user-emacs-directory "readme.org")))
  ;;           :wk "open config")
  ;;   )
  (synthmacs/leader-keys
  ;;   :keymaps 'org-mode-map
  ;;   "a" '(org-archive-subtree :wk "archive subtree")
  ;;   "E" '(org-export-dispatch :wk "export")
    "ui" '(org-insert-structure-template :wk "insert src")
  ;;   "l" '(:ignore true :wk "link")
    "ul" '(org-insert-link :wk "insert link")
  ;;   "l s" '(org-store-link :wk "store link")
  ;;   "L" '((lambda () (interactive) (org-latex-preview)) :wk "latex preview")
  ;;   ;; "L" '((lambda () (interactive) (org--latex-preview-region (point-min) (point-max))) :wk "latex")
  ;;   "r" '(org-refile :wk "refile")
  ;;   "n" '(org-toggle-narrow-to-subtree :wk "narrow subtree")
  ;;   "p" '(org-priority :wk "priority")
  ;;   "q" '(org-set-tags-command :wk "tag")
  ;;   "s" '(org-sort :wk "sort")
  ;;   "t" '(:ignore true :wk "todo")
  ;;   "t t" '(org-todo :wk "heading todo")
  ;;   "t s" '(org-schedule :wk "schedule")
  ;;   "t d" '(org-deadline :wk "deadline")
  ;;   "x" '(org-toggle-checkbox :wk "toggle checkbox")
    )
  ;; (org-mode-map
  ;;  :states 'insert
  ;;  "TAB" 'lc/org-indent-or-complete
  ;;  "S-TAB" nil)
  ;; (org-mode-map
  ;;  :states 'normal
  ;;  "z i" '(org-toggle-inline-images :wk "inline images"))
  :init
  ;; general settings
  (when (file-directory-p "~/Developer/ExoCortex/org")
    (setq org-directory "~/Developer/ExoCortex/org"
          +org-export-directory "~/Developer/ExoCortex/org/export"
          org-default-notes-file "~/Developer/ExoCortex/org/notes.org"
          org-id-locations-file "~/Developer/ExoCortex/org/.orgids"
          ))	
  ;; (setq org-export-in-background t)
  (setq org-src-preserve-indentation t) ;; do not put two spaces on the left
  (setq org-startup-indented t)
   ;;(setq org-startup-with-inline-images t)
  (setq org-hide-emphasis-markers nil)
  (setq org-catch-invisible-edits 'smart)
  (setq org-image-actual-width nil)
  (setq org-indent-indentation-per-level 1)
  (setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "*")))
  ;; disable modules for faster startup
  ;; (setq org-modules
  ;;       '(ol-docview
  ;;         org-habit))
  ;; (setq org-todo-keywords
  ;;       '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "HOLD(h)" "DONE(d)")))
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                         ("#+END_SRC" . "«")
                                         ("#+begin_src" . "»")
                                         ("#+end_src" . "«")
                                         ("lambda"  . "λ")
                                         ("->" . "→")
                                         ("->>" . "↠")))
  ;; (setq prettify-symbols-unprettify-at-point 'right-edge)
;;   (defun lc/org-indent-or-complete ()
;;     "Complete 
;; if point is at end of a word, otherwise indent line."
;;     (interactive)
;;     (if (looking-at "\\>")
;;         (dabbrev-expand nil)
;;       (org-cycle)
;;       ))
  ;; (setq warning-
  ;; 	suppress-types (append warning-suppress-types '((org-element-cache))))
  :config
  ;; ;; (efs/org-font-setup)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  ;; ;; fontification
  ;; (add-to-list 'org-src-lang-modes '("jupyter-python" . python))
  ;; (add-to-list 'org-src-lang-modes '("jupyter-R" . R))
  ;; ;; latex
  ;; ;; (setq org-latex-compiler "xelatex")
  ;; ;; see https://www.reddit.com/r/emacs/comments/l45528/questions_about_mving_from_standard_latex_to_org/gkp4f96/?utm_source=reddit&utm_medium=web2x&context=3
  ;; ;; (setq org-latex-pdf-process '("TEXINPUTS=:$HOME/git/AltaCV//: tectonic %f"))
  ;; (setq org-latex-pdf-process '("tectonic %f"))
  ;; (setq org-export-backends '(html))
  ;; ;; (add-to-list 'org-export-backends 'beamer)
  ;; (plist-put org-format-latex-options :scale 1.2)
  )
;; org mode:1 ends here

;; [[file:../synthmacs.org::*Enabling the Table of Contents][Enabling the Table of Contents:1]]
(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))
;; Enabling the Table of Contents:1 ends here

;; [[file:../synthmacs.org::*Using org-id in links][Using org-id in links:1]]
(use-package org
  :init
  (defun synthmacs/org-custom-id-get (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

  (defun synthmacs/org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current file.
   Only do so for those which do not already have one. Only adds ids
   if the `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
        (org-map-entries (lambda () (synthmacs/org-custom-id-get (point) 'create))))))
  :config
  (require 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  )
;; Using org-id in links:1 ends here

;; [[file:../synthmacs.org::*Tangling this file][Tangling this file:1]]
(use-package org
  :config
  (require 's)
  (defun synthmacs/async-process (command &optional name filter)
    "Start an async process by running the COMMAND string with bash. Return the
process object for it.

NAME is name for the process. Default is \"async-process\".

FILTER is function that runs after the process is finished, its args should be
\"(process output)\". Default is just messages the output."
    (make-process
     :command `("bash" "-c" ,command)
     :name (if name name
	     "async-process")
     :filter (if filter filter
	       (lambda (process output) (message (s-trim output))))))


  (defun synthmacs/tangle-config ()
    "Export code blocks from the literate config file
asynchronously."
    (interactive)
    (let ((command (if (file-directory-p "/opt/homebrew/opt/emacs-plus@29/Emacs.app")
		       "/opt/homebrew/opt/emacs-plus@29/Emacs.app/Contents/MacOS/Emacs %s --batch --eval '(org-babel-tangle nil \"%s\")'"
		     )))
      ;; prevent emacs from killing until tangle-process finished
      ;; (add-to-list 'kill-emacs-query-functions
      ;;              (lambda ()
      ;;                (or (not (process-live-p (get-process "tangle-process")))
      ;;                    (y-or-n-p "\"fk/tangle-config\" is running; kill it? "))))
      ;; tangle config asynchronously
      (synthmacs/async-process
       (format command
	       (expand-file-name "synthmacs.org" user-emacs-directory)
	       (expand-file-name "init.el" user-emacs-directory))
       "tangle-process")
      )

    )
  )
;; Tangling this file:1 ends here

;; [[file:../synthmacs.org::*synthmacs-org][synthmacs-org:1]]
(provide 'synthmacs-org)
;;; synthmacs-org.el ends here
;; synthmacs-org:1 ends here
