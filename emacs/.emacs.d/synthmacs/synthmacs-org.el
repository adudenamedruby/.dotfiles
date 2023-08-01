;; [[file:../synthmacs.org::*synthmacs-org][synthmacs-org:1]]
;;; synthmacs-org.el --- Synthmacs Org

;;; Commentary:

;; Set up Org mode for Synthmacs

;;; Code:
;; synthmacs-org:1 ends here

;; [[file:../synthmacs.org::*org mode][org mode:1]]
(defmacro synthmacs/org-emphasize (fname char)
  "Make function for setting the emphasis in org mode"
  `(defun ,fname () (interactive)
          (org-emphasize ,char)))

(use-package org
  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . visual-line-mode))
  :general
  ;; These keybindings need some more work, but they're mostly there!
  (synthmacs/leader-keys
    "o[" 'org-agenda-file-to-front
    "o]" 'org-remove-file
    "oa" 'org-agenda
    "oc" 'org-capture
    
    ;; More cycling options (timestamps, headlines, items, properties)
    "oL" 'org-shiftright
    "oH" 'org-shiftleft
    "oJ" 'org-shiftdown
    "oK" 'org-shiftup

    "oI" 'org-indent-region
    "op" 'org-priority
    "oS" '(org-insert-structure-template :wk "insert src")
    "oX" 'org-toggle-checkbox
    "o*" 'org-ctrl-c-star
    "o-" 'org-ctrl-c-minus
    "o#" 'org-update-statistics-cookies
    "o RET"   'org-ctrl-c-ret
    "o M-RET" 'org-meta-return
    "oA" 'org-attach

    "ob" '(:ignore t :wk "babel")
    "oba"     'org-babel-sha1-hash
    "obp"     'org-babel-previous-src-block
    "obn"     'org-babel-next-src-block
    "obe"     'org-babel-execute-maybe
    "obo"     'org-babel-open-src-block-result
    "obv"     'org-babel-expand-src-block
    "obu"     'org-babel-goto-src-block-head
    "obg"     'org-babel-goto-named-src-block
    "obr"     'org-babel-goto-named-result
    "obb"     'org-babel-execute-buffer
    "obs"     'org-babel-execute-subtree
    "obd"     'org-babel-demarcate-block
    "obt"     'org-babel-tangle
    "obf"     'org-babel-tangle-file
    "obc"     'org-babel-check-src-block
    "obj"     'org-babel-insert-header-arg
    "obl"     'org-babel-load-in-session
    "obi"     'org-babel-lob-ingest
    "obI"     'org-babel-view-src-block-info
    "obz"     'org-babel-switch-to-session
    "obZ"     'org-babel-switch-to-session-with-code
    "obx"     'org-babel-do-key-sequence-in-edit-buffer

    "ol" '(:ignore true :wk "link")
    "oli" 'org-insert-link
    "ols" 'org-store-link

    "oC" '(:ignore t :wk "Clock")
    "oCc" 'org-clock-cancel
    "oCd" 'org-clock-display
    "oCe" 'org-evaluate-time-range
    "oCg" 'org-clock-goto
    "oCi" 'org-clock-in
    "oCI" 'org-clock-in-last
    "oCo" 'org-clock-out
    "oCR" 'org-clock-report
    "oCr" 'org-resolve-clocks

    "od" '(:ignore t :wk "dates")
    "odd" 'org-deadline
    "ods" 'org-schedule
    "odt" 'org-time-stamp
    "odT" 'org-time-stamp-inactive
    
    "oe" '(:ignore t :wk "export")
    "oee" 'org-export-dispatch
    
    "of" '(:ignore t :wk "feed")
    "ofi" 'org-feed-goto-inbox
    "ofu" 'org-feed-update-all

    "oT" '(:ignore t :wk "Toggles")
    "oTc" 'org-toggle-checkbox
    "oTe" 'org-toggle-pretty-entities
    "oTi" 'org-toggle-inline-images
    "oTn" 'org-num-mode
    "oTl" 'org-toggle-link-display
    "oTt" 'org-show-todo-tree
    "oTT" 'org-todo
    "oTV" 'space-doc-mode
    "oTx" 'org-latex-preview

    "os" '(:ignore t :wk "trees/subtrees")
    "osa" 'org-toggle-archive-tag
    "osA" 'org-archive-subtree-default
    "osb" 'org-tree-to-indirect-buffer
    "osd" 'org-cut-subtree
    "osy" 'org-copy-subtree
    "osp" 'org-paste-subtree
    "osh" 'org-promote-subtree
    "osj" 'org-move-subtree-down
    "osk" 'org-move-subtree-up
    "osl" 'org-demote-subtree
    "osn" 'org-narrow-to-subtree
    "osw" 'widen
    "osr" 'org-refile
    "oss" 'org-sparse-tree
    "osS" 'org-sort

    "ot" '(:ignore t :wk "Tables")
    "ota" 'org-table-align
    "otb" 'org-table-blank-field
    "otc" 'org-table-convert
    "ote" 'org-table-eval-formula
    "otE" 'org-table-export
    "otf" 'org-table-field-info
    "oth" 'org-table-previous-field
    "otH" 'org-table-move-column-left
    
    "otd" '(:ignore t :wk "delete")
    "otdc" 'org-table-delete-column
    "otdr" 'org-table-kill-row
    
    "oti" '(:ignore t :wk "insert")
    "otic" 'org-table-insert-column
    "otih" 'org-table-insert-hline
    "otiH" 'org-table-hline-and-move
    "otir" 'org-table-insert-row
    
    "otI" 'org-table-import
    "otj" 'org-table-next-row
    "otJ" 'org-table-move-row-down
    "otK" 'org-table-move-row-up
    "otl" 'org-table-next-field
    "otL" 'org-table-move-column-right
    "otn" 'org-table-create
    "otN" 'org-table-create-with-table.el
    "otr" 'org-table-recalculate
    "otR" 'org-table-recalculate-buffer-tables
    "ots" 'org-table-sort-lines
    
    "ott" '(:ignore t :wk "toggles")
    "ottf" 'org-table-toggle-formula-debugger
    "otto" 'org-table-toggle-coordinate-overlays
    
    "otw" 'org-table-wrap-region

    "oi" '(:ignore t :wk "insert")
    "oib" 'org-insert-structure-template
    "oid" 'org-insert-drawer
    "oie" 'org-set-effort
    "oif" 'org-footnote-new
    "oih" 'org-insert-heading
    "oiH" 'org-insert-heading-after-current
    "oii" 'org-insert-item
    "oiK" 'spacemacs/insert-keybinding-org
    "oil" 'org-insert-link
    "oin" 'org-add-note
    "oip" 'org-set-property
    "ois" 'org-insert-subheading
    "oit" 'org-set-tags-command
    
    "ox" '(:ignore t :wk "text")
    "oxb" (synthmacs/org-emphasize synthmacs/org-bold ?*)
    "oxc" (synthmacs/org-emphasize synthmacs/org-code ?~)
    "oxi" (synthmacs/org-emphasize synthmacs/org-italic ?/)
    "oxo" 'org-open-at-point
    "oxr" (synthmacs/org-emphasize synthmacs/org-clear ? )
    "oxs" (synthmacs/org-emphasize synthmacs/org-strike-through ?+)
    "oxu" (synthmacs/org-emphasize synthmacs/org-underline ?_)
    "oxv" (synthmacs/org-emphasize synthmacs/org-verbatim ?=)
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
  (setq org-startup-with-inline-images t)
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
  (setq org-capture-templates
        (quote (
                ("b" "Books to read"
                 entry
                 (file+headline "~/code/git/ExoCortex/org/media.org" "Books")
                 "** %^{Title} %?\n")

                ("f" "Fix code"
                 entry
                 (file+headline "~/code/git/ExoCortex/org/tasks.org" "Fixes")
                 "* FIXME %^{Description}\n@%a\n%?")

                ("n" "Quick note"
                 entry
                 (file+headline "~/code/git/ExoCortex/org/notes.org" "Notes")
                 "** %^{Description}\nAdded: %t\n%?")

                ("o" "One on one"
                 entry
                 (file+headline "~/code/git/ExoCortex/org/mozilla.org" "1:1")
                 "** %t\n*** %?\n")

                ("r" "Reminder"
                 entry
                 (file+headline "~/code/git/ExoCortex/org/tasks.org" "Reminders")
                 "* REMINDER %^{Description}\n%?")

                ("t" "Task" entry
                 (file+function "~/code/git/ExoCortex/org/tasks.org" org-reverse-datetree-goto-date-in-file)
                 "* TODO %^{Description}\n%?")

                ("w" "Watch - Movie/Show/Documentary"
                 entry
                 (file+headline "~/code/git/ExoCortex/org/media.org" "Watch")
                 "** [[%^{Link}][%^{Title}]]\n%?"))))

  ;; Increase the size of various headings
;; (set-face-attribute 'org-document-title nil :font "Iosevka" :weight 'bold :height 1.3)
;; (dolist (face '((org-level-1 . 1.2)
;;                 (org-level-2 . 1.1)
;;                 (org-level-3 . 1.05)
;;                 (org-level-4 . 1.0)
;;                 (org-level-5 . 1.1)
;;                 (org-level-6 . 1.1)
;;                 (org-level-7 . 1.1)
;;                 (org-level-8 . 1.1)))
;;   (set-face-attribute (car face) nil :font "Iosevka" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available
;; (require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
;; (set-face-attribute 'org-column nil :background nil)
;; (set-face-attribute 'org-column-title nil :background nil)

;; TODO: Others to consider
;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
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
       "tangle-process"))))
;; Tangling this file:1 ends here

;; [[file:../synthmacs.org::*org-tree-slide][org-tree-slide:1]]
(use-package org-tree-slide
  :after org
  :hook ((org-tree-slide-play . (lambda () (+remap-faces-at-start-present)))
         (org-tree-slide-stop . (lambda () (+remap-faces-at-stop-present))))
  :general
  (synthmacs/leader-keys
    "tP" '(org-tree-slide-mode :wk "present-slides"))
  (general-nmap
    :keymaps '(org-tree-slide-mode-map org-mode-map)
    "C-j" 'org-tree-slide-move-next-tree
    "C-k" 'org-tree-slide-move-previous-tree)
  :init
  (setq org-tree-slide-activate-message "Presentation mode ON")
  (setq org-tree-slide-deactivate-message "Presentation mode OFF")
  (setq org-tree-slide-indicator nil)
  (setq org-tree-slide-breadcrumbs "    >    ")
  (setq org-tree-slide-heading-emphasis t)
  (setq org-tree-slide-slide-in-waiting 0.025)
  (setq org-tree-slide-content-margin-top 4)
  (defun +remap-faces-at-start-present ()
    (setq-local face-remapping-alist '((default (:height 1.50) variable-pitch)
                                       (fixed-pitch (:height 1.2) fixed-pitch)
                                       ;; (org-verbatim (:height 1.2) org-verbatim)
                                       ;; (org-block (:height 1.2) org-block)
                                       ))
    ;; (setq-local olivetti-body-width 95)
    (olivetti-mode 1)
    (display-fill-column-indicator-mode 0)
    (hide-mode-line-mode 1)
    (diff-hl-mode 0)
    (centaur-tabs-mode 0))
  (defun +remap-faces-at-stop-present ()
    (setq-local face-remapping-alist '((default variable-pitch default)))
    ;; (setq-local olivetti-body-width 120)
    (olivetti-mode 0)
    (display-fill-column-indicator-mode 1)
    (hide-mode-line-mode 0)
    (doom-modeline-mode 1)
    (diff-hl-mode 1)
    (centaur-tabs-mode 1))
  (setq org-tree-slide-breadcrumbs nil)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init t)
  (setq org-tree-slide-modeline-display nil)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-skip-comments t)
  (setq org-tree-slide-fold-subtrees-skipped t)
  (setq org-tree-slide-skip-outline-level 8) ;; or 0?
  (setq org-tree-slide-never-touch-face t)
  ;; :config
  ;; (org-tree-slide-presentation-profile)
  ;; :custom-face
  ;; (org-tree-slide-heading-level-1 ((t (:height 1.8 :weight bold))))
  ;; (org-tree-slide-heading-level-2 ((t (:height 1.5 :weight bold))))
  ;; (org-tree-slide-heading-level-3 ((t (:height 1.5 :weight bold))))
  ;; (org-tree-slide-heading-level-4 ((t (:height 1.5 :weight bold))))
  )
;; org-tree-slide:1 ends here

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

;; [[file:../synthmacs.org::*Enhanced HTML exports][Enhanced HTML exports:1]]
(use-package htmlize)
;; Enhanced HTML exports:1 ends here

;; [[file:../synthmacs.org::*org-roam][org-roam:1]]
(use-package org-roam
  :after org
  :init
  (setq org-enable-roam-support t)
  (setq org-enable-roam-ui t)
  (setq org-roam-directory (expand-file-name "~/Developer/ExoCortex/myWiki/zettlekasten"))
  (setq org-roam-db-location (expand-file-name "~/Developer/ExoCortex/myWiki/db/org-roam.db"))
  (setq org-roam-v2-ack t)
  (setq org-roam-capture-templates
        '(("n" "Note" plain
           ;; %? is the cursor, and the rest is what the file will be preloaded with
           ;; This can also be: (file "~/location/to/org/file")
           "\n\n* ${title}\n** Summary\n%?\n\n** More details\n\n* References\n\n* LINKS\n"
           ;; filename AND what's added to the top of the file
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+author: adudenamedruby\n#+date: %t")
           :unnarrowed t)

          ("g" "Glossary term" plain
           "\n\n* ${title}\n** Definition\n%?\n\n* References\n\n* LINKS\n- [[id:C1F1861B-20E0-4E15-9C0A-C93CE1652CC9][Glossary]]\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+author: adudenamedruby\n#+date: %t")
           :unnarrowed t)

          ("q" "Quote" plain
           "\n\n#+BEGIN_QUOTE\n%?#+END_QUOTE\n\n* References\n\n* LINKS\n-[[id:ADC4CC70-7EE8-4B34-A852-7A4F9DF8AFBF][Quotes]]\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+author: adudenamedruby\n#+date: %t")
           :unnarrowed t)
          ))
  :general
  (synthmacs/leader-keys
    "or" '(:ignore t :wk "org-roam")
    "orc" 'org-roam-capture
    "orf" 'org-roam-node-find
    "org" 'org-roam-graph
    "orr" 'org-roam-ref-add
    "ori" 'org-roam-node-insert
    "orb" 'org-roam-buffer-toggle
    "ora" 'org-roam-alias-add
    
    "ord" '(:ignore t :wk "org-roam-dailies")
    "ordy" 'org-roam-dailies-goto-yesterday
    "ordt" 'org-roam-dailies-goto-today
    "ordT" 'org-roam-dailies-goto-tomorrow
    "ordd" 'org-roam-dailies-goto-date
    )
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  (add-to-list 'display-buffer-alist
	       '(("*org-roam*"
		  (display-buffer-in-direction)
		  (direction . right)
		  (window-width . 0.33)
		  (window-height . fit-window-to-buffer))))

  )
;; org-roam:1 ends here

;; [[file:../synthmacs.org::*Obsidian][Obsidian:1]]
(use-package obsidian
  :ensure t
  :demand t
  :general
  (synthmacs/local-leader-keys
    :keymap 'obsidian-mode-map
    "o" 'obsidian-follow-link-at-point
    ;; Jump to backlinks
    "b" 'obsidian-backlink-jump
    ;; If you prefer you can use `obsidian-insert-link'
    "i" 'obsidian-insert-wikilink
    )
  :config
  (obsidian-specify-path "~/MY_OBSIDIAN_FOLDER")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Inbox")
  )
;; Obsidian:1 ends here

;; [[file:../synthmacs.org::*synthmacs-org][synthmacs-org:1]]
(provide 'synthmacs-org)
;;; synthmacs-org.el ends here
;; synthmacs-org:1 ends here
