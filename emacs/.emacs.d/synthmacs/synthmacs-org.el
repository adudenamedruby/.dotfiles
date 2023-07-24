;; [[file:../synthmacs.org::*async tangle][async tangle:1]]
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
    (let ((command (if (file-directory-p "/Applications/Emacs.app")
		       "/Applications/Emacs.app/Contents/MacOS/Emacs %s --batch --eval '(org-babel-tangle nil \"%s\")'"
		     ;; on iPad
		     "emacs %s --batch --eval '(org-babel-tangle nil \"%s\")'"
		     ;; "emacs %s --batch --eval '(org-babel-tangle nil \"%s\")'  2>&1 | grep -v '^Loading.*\.\.\.$' | grep -v '^Using ' | grep -v '^dump '| grep -v '^Finding '"
		     )))
      ;; prevent emacs from killing until tangle-process finished
      ;; (add-to-list 'kill-emacs-query-functions
      ;;              (lambda ()
      ;;                (or (not (process-live-p (get-process "tangle-process")))
      ;;                    (y-or-n-p "\"fk/tangle-config\" is running; kill it? "))))
      ;; tangle config asynchronously
      (synthmacs/async-process
       (format command
	       (expand-file-name "readme.org" user-emacs-directory)
	       (expand-file-name "init.el" user-emacs-directory))
       "tangle-process")
      )

    )
  )
;; async tangle:1 ends here

;; [[file:../synthmacs.org::*use org-id in links][use org-id in links:1]]
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
  
  (defun synchmacs/org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" 10000 t)
        (org-map-entries (lambda () (synthmacs/org-custom-id-get (point) 'create))))))
  :config
  (require 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  )
;; use org-id in links:1 ends here

;; [[file:../synthmacs.org::*synthmacs-org][synthmacs-org:1]]
(provide 'synthmacs-org)
;;; synthmacs-org.el ends here
;; synthmacs-org:1 ends here
