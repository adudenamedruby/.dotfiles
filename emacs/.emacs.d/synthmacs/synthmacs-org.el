;; [[file:../synthmacs.org::*org mode][org mode:1]]
(use-package org
  ;; :straight org-plus-contrib
  ;; :straight (:type built-in)
  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . visual-line-mode))
  :general
  (synthmacs/leader-keys
    "oc" 'org-capture
    ;;"ol" '(org-todo-list :wk "todo list")

    "ft" 'org-babel-tangle
    )

  (synthmacs/local-leader-keys
    :keymaps 'org-mode-map
    "a" '(org-archive-subtree :wk "archive subtree")
    "E" '(org-export-dispatch :wk "org-export")
    "i" 'org-indent-region
    "s" '(org-insert-structure-template :wk "insert src")
    "S" 'org-sort
    "x" 'org-toggle-checkbox

    "t" '(:ignore true :wk "todo")
    "tt" 'org-todo
    "ts" 'org-schedule
    "td" 'org-deadline

    "l" '(:ignore true :wk "link")
    "li" 'org-insert-link
    "ls" 'org-store-link

    ;;   "L" '((lambda () (interactive) (org-latex-preview)) :wk "latex preview")
    ;;   ;; "L" '((lambda () (interactive) (org--latex-preview-region (point-min) (point-max))) :wk "latex")
    ;;   "r" '(org-refile :wk "refile")
    ;;   "n" '(org-toggle-narrow-to-subtree :wk "narrow subtree")
    ;;   "p" '(org-priority :wk "priority")
    ;;   "q" '(org-set-tags-command :wk "tag")
    )

  (org-mode-map
   :states 'insert
   "TAB" 'synthmacs/org-indent-or-complete
   "S-TAB" nil)

  (org-mode-map
   :states 'normal
   "z i" '(org-toggle-inline-images :wk "inline images"))

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
  (setq org-hide-emphasis-markers t)
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
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  :config
  ;; ;; (efs/org-font-setup)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  ;; (setq org-latex-pdf-process '("tectonic %f"))
  ;; (setq org-export-backends '(html))
  ;; ;; (add-to-list 'org-export-backends 'beamer)
  ;; (plist-put org-format-latex-options :scale 1.2)
  )
;; org mode:1 ends here

;; [[file:../synthmacs.org::*Enabling the Table of Contents][Enabling the Table of Contents:1]]
(use-package toc-org
  :commands toc-org-enable
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))
;; Enabling the Table of Contents:1 ends here

;; [[file:../synthmacs.org::*org reverse datetree][org reverse datetree:1]]
(use-package org-reverse-datetree
  :after org
  :demand)
;; org reverse datetree:1 ends here

;; [[file:../synthmacs.org::*org-superstar][org-superstar:1]]
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-headline-bullets-list '("✖" "✚" "◉" "○" "▶")
        ;; org-superstar-special-todo-items t
        org-ellipsis " ↴ ")
  )
;; org-superstar:1 ends here

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

;; [[file:../synthmacs.org::*org-babel][org-babel:1]]
(use-package org
  :general
  (synthmacs/local-leader-keys
    :keymaps 'org-mode-map
    "e" '(org-edit-special :wk "edit")
    "-" '(org-babel-demarcate-block :wk "split block")
    "z" '(org-babel-hide-result-toggle :wk "fold result"))

  (synthmacs/local-leader-keys
    :keymaps 'org-src-mode-map
    "'" '(org-edit-src-exit :wk "exit")) ;;FIXME

  :init
  (setq org-confirm-babel-evaluate nil)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (ledger . t)
     (shell . t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  )
;; org-babel:1 ends here

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

;; [[file:../synthmacs.org::*evil-org-mode][evil-org-mode:1]]
(use-package evil-org-mode
  :straight (evil-org-mode
	     :type git
	     :host github
	     :repo "hlissner/evil-org-mode")
  :hook ((org-mode . evil-org-mode)
         (org-mode . (lambda () 
                       (require 'evil-org)
                       (evil-normalize-keymaps)
                       (evil-org-set-key-theme '(textobjects))
                       (require 'evil-org-agenda)
                       (evil-org-agenda-set-keys))))
  :bind
  ([remap evil-org-org-insert-heading-respect-content-below] . +org/insert-item-below) ;; "<C-return>" 
  ([remap evil-org-org-insert-todo-heading-respect-content-below] . +org/insert-item-above) ;; "<C-S-return>" 

  :general
  (general-nmap
    :keymaps 'org-mode-map
    :states 'normal
    "RET"   #'org-open-at-point
    ;; "RET"   #'+org/dwim-at-point
    )

  :init
  (defun +org--insert-item (direction)
    (let ((context (org-element-lineage
                    (org-element-context)
                    '(table table-row headline inlinetask item plain-list)
                    t)))
      (pcase (org-element-type context)
        ;; Add a new list item (carrying over checkboxes if necessary)
        ((or `item `plain-list)
         ;; Position determines where org-insert-todo-heading and org-insert-item
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (org-end-of-item)
           (backward-char))
         (org-insert-item (org-element-property :checkbox context))
         ;; Handle edge case where current item is empty and bottom of list is
         ;; flush against a new heading.
         (when (and (eq direction 'below)
                    (eq (org-element-property :contents-begin context)
                        (org-element-property :contents-end context)))
           (org-end-of-item)
           (org-end-of-line)))

        ;; Add a new table row
        ((or `table `table-row)
         (pcase direction
           ('below (save-excursion (org-table-insert-row t))
                   (org-table-next-row))
           ('above (save-excursion (org-shiftmetadown))
                   (+org/table-previous-row))))

        ;; Otherwise, add a new heading, carrying over any todo state, if
        ;; necessary.
        (_
         (let ((level (or (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
           (pcase direction
             (`below
              (let (org-insert-heading-respect-content)
                (goto-char (line-end-position))
                (org-end-of-subtree)
                (insert "\n" (make-string level ?*) " ")))
             (`above
              (org-back-to-heading)
              (insert (make-string level ?*) " ")
              (save-excursion (insert "\n"))))
           (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                       (todo-type    (org-element-property :todo-type context)))
             (org-todo
              (cond ((eq todo-type 'done)
                     ;; Doesn't make sense to create more "DONE" headings
                     (car (+org-get-todo-keywords-for todo-keyword)))
                    (todo-keyword)
                    ('todo)))))))

      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
        (evil-insert 1))))

  (defun +org/insert-item-below (count)
    "Inserts a new heading, table cell or item below the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'below)))

  (defun +org/insert-item-above (count)
    "Inserts a new heading, table cell or item above the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'above)))
  )
;; evil-org-mode:1 ends here

;; [[file:../synthmacs.org::*org-appear][org-appear:1]]
(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autoemphasis  t)
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  )
;; org-appear:1 ends here

;; [[file:../synthmacs.org::*synthmacs-org][synthmacs-org:1]]
(provide 'synthmacs-org)
;;; synthmacs-org.el ends here
;; synthmacs-org:1 ends here
