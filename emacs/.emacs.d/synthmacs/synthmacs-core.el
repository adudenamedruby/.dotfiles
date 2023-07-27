;; [[file:../synthmacs.org::*Emacs Setup][Emacs Setup:1]]
(use-package emacs
  :init
  (setq inhibit-startup-message t
	inhibit-startup-screen t
        initial-scratch-message nil
        sentence-end-double-space nil
        ring-bell-function 'ignore
        frame-resize-pixelwise t)

  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  ;; write over selected text on input... like all modern editors do
  ;;(delete-selection-mode t)
  )
;; Emacs Setup:1 ends here

;; [[file:../synthmacs.org::*User setup][User setup:1]]
(use-package emacs
  :init
  (setq user-full-name "roux g. buciu"
        user-mail-address "roux@fringe.foundation"))
;; User setup:1 ends here

;; [[file:../synthmacs.org::*"Yes or no" prompts]["Yes or no" prompts:1]]
(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))
;; "Yes or no" prompts:1 ends here

;; [[file:../synthmacs.org::*UTF-8 file encoding][UTF-8 file encoding:1]]
(use-package emacs
  :init
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))
;; UTF-8 file encoding:1 ends here

;; [[file:../synthmacs.org::*Recent files][Recent files:1]]
(use-package emacs
  :init
  (recentf-mode t)
  (setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                          ,(expand-file-name "eln-cache/" user-emacs-directory)
                          ,(expand-file-name "etc/" user-emacs-directory)
                          ,(expand-file-name "var/" user-emacs-directory)))
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 10)
  )
;; Recent files:1 ends here

;; [[file:../synthmacs.org::*ESC key!][ESC key!:1]]
(use-package emacs
  :init
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))
;; ESC key!:1 ends here

;; [[file:../synthmacs.org::*Custom file][Custom file:1]]
(use-package emacs
  :init
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings
  )
;; Custom file:1 ends here

;; [[file:../synthmacs.org::*Autosaves][Autosaves:1]]
(use-package emacs
  :init
  (setq make-backup-files nil
        auto-save-default t
        create-lockfiles nil))
;; Autosaves:1 ends here

;; [[file:../synthmacs.org::*Symlinks][Symlinks:1]]
(use-package emacs
  :init
  ;; follow symlinks 
  (setq vc-follow-symlinks t))
;; Symlinks:1 ends here

;; [[file:../synthmacs.org::*Window chrome][Window chrome:1]]
(use-package emacs
  :init
  (when (window-system)
    (tool-bar-mode -1)
    (tooltip-mode -1)
    (toggle-scroll-bar -1)
    (set-fringe-mode 10)
    ;; (menu-bar-mode -1)
    )
  )
;; Window chrome:1 ends here

;; [[file:../synthmacs.org::*Scrolling behaviours][Scrolling behaviours:1]]
(use-package emacs
  :init
  ;; Set scroll margin, but emulate vim scroll behaviour
  (setq scroll-conservatively 101
	scroll-margin 5
	scroll-preserve-screen-position 't)

  ;; Enables having the line the cursor is on be highlighted
  (global-hl-line-mode 1)

  ;; enable winner mode globally for undo/redo window layout changes
  (winner-mode t)

  (show-paren-mode t)
  )
;; Scrolling behaviours:1 ends here

;; [[file:../synthmacs.org::*Line numbers][Line numbers:1]]
(use-package emacs
  :init
  ;; ------------------ Line Numbering ---------------------
  ;; set type of line numbering (global variable)
  (setq display-line-numbers-type 'relative)
  ;; activate line numbering in all buffers/modes
  (global-display-line-numbers-mode 1)

  (dolist (mode '(org-mode-hook
		  term-mode-hook
		  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  ;; Activate line numbering in programming modes
  ;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  )
;; Line numbers:1 ends here

;; [[file:../synthmacs.org::*Fill column & modeline column info][Fill column & modeline column info:1]]
(use-package emacs
  :init
  (setq-default fill-column 85)
  (global-display-fill-column-indicator-mode)

  ;; Columns number in the modeline
  (setq column-number-mode t)
  )
;; Fill column & modeline column info:1 ends here

;; [[file:../synthmacs.org::*Other][Other:1]]
(use-package emacs
  :init
  ;; use common convention for indentation by default
  ;;(setq-default indent-tabs-mode t)
  ;;(setq-default tab-width 2)

  ;; Enable indentation+completion using the TAB key.
  ;; Completion is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  (global-prettify-symbols-mode 1)

  ;; ------------------ Indent Behaviours ---------------------
  ;; Electric indent mode messes up with a bunch of languages indenting.
  ;; So disable it.
  (setq electric-indent-inhibit t)

  (global-visual-line-mode t)

  )
;; Other:1 ends here

;; [[file:../synthmacs.org::*Custom variables][Custom variables:1]]
;; reopening the last killed buffer
(use-package emacs
  :init
  (defcustom synthmacs/default-font-family "FiraCode Nerd Font" 
    "Default font family"
    :type 'string
    :group 'synthmacs)

  (defcustom synthmacs/variable-pitch-font-family "Sans Serif"
    "Variable pitch font family"
    :type 'string
    :group 'synthmacs)
  
  (defcustom synthmacs--killed-buffer-list nil
    "List of recently killed buffers.")
  )
;; Custom variables:1 ends here

;; [[file:../synthmacs.org::*Fonts][Fonts:1]]
(use-package emacs
  :init
  ;; Main typeface
  (set-face-attribute 'default nil :font synthmacs/default-font-family :height 140)
  ;; Set the fixed pitch face (monospace)
  (set-face-attribute 'fixed-pitch nil :font synthmacs/default-font-family)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font synthmacs/variable-pitch-font-family)
  )
;; Fonts:1 ends here

;; [[file:../synthmacs.org::*Buffers][Buffers:1]]
(defun synthmacs/add-buffer-to-killed-list ()
  "Add killed buffer to list for undo functionality.
If buffer is associated with a file name, add that file
to the `killed-buffer-list` when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name synthmacs--killed-buffer-list)))

(add-hook 'kill-buffer-hook #'synthmacs/add-buffer-to-killed-list)

(defun synthmacs/alternate-buffer (&optional window)
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (if (bound-and-true-p nil)
	  (let ((buffer-list (persp-buffer-list))
		(my-buffer (window-buffer window)))
	    (seq-find (lambda (it)
			(and (not (eq (car it) my-buffer))
			     (member (car it) buffer-list)))
		      (window-prev-buffers)
		      (list nil nil nil)))
	(or (cl-find (window-buffer window) (window-prev-buffers)
		     :key #'car :test-not #'eq)
	    (list (other-buffer) nil nil)))
    (if (not buf)
	(message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))

(defun synthmacs/reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when synthmacs--killed-buffer-list
    (find-file (pop synthmacs--killed-buffer-list))))

;; Moving windows around
(require 'windmove)

;;;###autoload
(defun synthmacs/buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
  ;;  "Switches between the current buffer, and the buffer above the
  ;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun synthmacs/buf-move-down ()
  "Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun synthmacs/buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun synthmacs/buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;; (defun synthmacs/delete-current-buffer-file ()
;;   "Removes the file connected to the current buffer, and kills the buffer."
;;   (interactive)
;;   (let ((filename (buffer-file-name))
;; 	(buffer (current-buffer))
;; 	(name (buffer-name)))
;;     (if (not (and filename (file-exists-p filename)))
;; 	(ido-kill-buffer)
;;       (if (yes-or-no-p (format "Are you sure you want to delet this file: '%s'?" name))
;; 	  (progn
;; 	    (delete-file filename t)
;; 	    (kill-buffer buffer)
;; 	    (when (and (synthmacs/packaged-used-p 'projectile)
;; 		       (projectile-project-p))
;; 	      (call-interactively #'projectile-invalidate-cache))
;; 	    (message "File deleted: '%s'" filename))
;; 	(message "Cancelled file deletion")))))
;; Buffers:1 ends here

;; [[file:../synthmacs.org::*Copying file paths][Copying file paths:1]]
(defun synthmacs//directory-path ()
  "Retrieve the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory' variable
as a fallback to display the directory, useful in buffers like the ones created
by `magit' and `dired'.

Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
                                (file-name-directory file-name)
                              list-buffers-directory))
    (file-truename directory-name)))

(defun synthmacs//file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun synthmacs//file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (synthmacs//file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun synthmacs//copy-directory-path ()
  "Copy and show the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (synthmacs//directory-path))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun synthmacs//copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (synthmacs//file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun synthmacs//copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let* ((file-path (synthmacs//file-path))
            (file-name (file-name-nondirectory file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun synthmacs//copy-buffer-name ()
  "Copy and show the name of the current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "%s" (buffer-name)))

(defun synthmacs//copy-file-name-base ()
  "Copy and show the file name without its final extension of the current
buffer."
  (interactive)
  (if-let (file-name (file-name-base (synthmacs//file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun synthmacs//copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including line number."
  (interactive)
  (if-let (file-path (synthmacs//file-path-with-line))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))
;; Copying file paths:1 ends here

;; [[file:../synthmacs.org::*Renaming files & buffers][Renaming files & buffers:1]]
(defun synthmacs/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let ((file (buffer-file-name)))
    (if (and file (file-exists-p file))
	(synthmacs/rename-buffer-visiting-a-file arg)
      (synthmacs/rename-buffer-or-save-new-file))))

(defun synthmacs/rename-buffer-visiting-a-file (&optional arg)
  (let* ((old-filename (buffer-file-name))
         (old-short-name (file-name-nondirectory (buffer-file-name)))
         (old-dir (file-name-directory old-filename))
         (new-name (let ((path (read-file-name "New name: " (if arg old-dir old-filename))))
                     (if (string= (file-name-nondirectory path) "")
                         (concat path old-short-name)
                       path)))
         (new-dir (file-name-directory new-name))
         (new-short-name (file-name-nondirectory new-name))
         (file-moved-p (not (string-equal new-dir old-dir)))
         (file-renamed-p (not (string-equal new-short-name old-short-name))))
    (cond ((get-buffer new-name)
           (error "A buffer named '%s' already exists!" new-name))
          ((string-equal new-name old-filename)
           (message "Rename failed! Same new and old name" 1.5)
           (synthmacs/rename-current-buffer-file))
          (t
           (let ((old-directory (file-name-directory new-name)))
             (when (and (not (file-exists-p old-directory))
                        (yes-or-no-p
                         (format "Create directory '%s'?" old-directory)))
               (make-directory old-directory t)))
           (rename-file old-filename new-name 1)
           (rename-buffer new-name)
           (set-visited-file-name new-name)
           (set-buffer-modified-p nil)
           (when (fboundp 'recentf-add-file)
             (recentf-add-file new-name)
             (recentf-remove-if-non-kept old-filename))
           (when (and (require 'projectile nil 'noerror)
                      (projectile-project-p))
             (funcall #'projectile-invalidate-cache nil))
           (message (cond ((and file-moved-p file-renamed-p)
                           (concat "File Moved & Renamed\n"
                                   "From: " old-filename "\n"
                                   "To:   " new-name))
                          (file-moved-p
                           (concat "File Moved\n"
                                   "From: " old-filename "\n"
                                   "To:   " new-name))
                          (file-renamed-p
                           (concat "File Renamed\n"
                                   "From: " old-short-name "\n"
                                   "To:   " new-short-name))))))))


(defun synthmacs/rename-buffer-or-save-new-file ()
  (let ((old-short-name (buffer-name))
	key)
    (while (not (memq key '(?s ?r)))
      (setq key (read-key (propertize
			   (format
			    (concat "Buffer '%s' is not visiting a file: "
				    "[s]ave to file or [r]ename buffer?")
			    old-short-name)
			   'face 'minibuffer-prompt)))
      (cond ((eq key ?s)    ; save to file
	     (unless (buffer-modified-p) (set-buffer-modified-p t))
	     (save-buffer))
	    ((eq key ?r)    ; rename buffer
	     (let ((new-buffer-name (read-string ("New buffer namme: ")))
		   ;; ask to rename again, if the new buffer name exists
		   (if (yes-or-no-p
			(format
			 (concat "A buffer named '%s' already exists: "
				 "Rename again?")
			 new-buffer-name))
		       (setq new-buffer-name (read-string "New buffer name: "))
		     (keyboard-quit)))
	       (rename-buffer new-buffer-name)
	       (message (concat "Buffer Renamed\n"
				"From: " old-short-name "\n"
				"To:   " new-buffer-name ))))
	    ;; ?\a = C-g, ?\e = Esc and C-[
	    ((memq key '(?\a ?\e)) (keyboard-quit))))))
;; Renaming files & buffers:1 ends here

;; [[file:../synthmacs.org::*<C-h> in the minibuffer while completing a file name][<C-h> in the minibuffer while completing a file name:1]]
(defun synthmacs/minibuffer-backwards-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder; otherwise, delete a character backwards."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
	  (zap-up-to-char (- arg) ?/)
	(delete-minibuffer-contents))
    (delete-backwards-char arg)))
;; <C-h> in the minibuffer while completing a file name:1 ends here

;; [[file:../synthmacs.org::*Symbols][Symbols:1]]
(defun synthmacs/my-add-pretty-symbol ()
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          ;; ("->" . 8594)    ; →
          ;; ("=>" . 8658)    ; ⇒
          ;; ("map" . 8614)   ; ↦
          )))
;; Symbols:1 ends here

;; [[file:../synthmacs.org::*Quit (but save before doing so!)][Quit (but save before doing so!):1]]
(defun synthmacs/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Synthmacs"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))
;; Quit (but save before doing so!):1 ends here

;; [[file:../synthmacs.org::*Auto-pair paranthesis][Auto-pair paranthesis:1]]
(use-package emacs
  :hook
  ((org-mode . (lambda () (synthmacs/add-local-electric-pairs '(;(?= . ?=)
                                                         (?~ . ?~))))))
  :init
  (electric-pair-mode +1)
  (setq electric-pair-preserve-balance nil)

  ;; mode-specific local-electric pairs
  (defconst synthmacs/default-electric-pairs electric-pair-pairs)
  (defun synthmacs/add-local-electric-pairs (pairs)
    "Example usage: 
    (add-hook 'jupyter-org-interaction-mode '(lambda () (set-local-electric-pairs '())))
    "
    (setq-local electric-pair-pairs (append synthmacs/default-electric-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  ;; disable auto pairing for <  >
  (add-function :before-until electric-pair-inhibit-predicate
                (lambda (c) (eq c ?<   ;; >
                                )))
  )
;; Auto-pair paranthesis:1 ends here

;; [[file:../synthmacs.org::*synthmacs-core][synthmacs-core:1]]
(provide 'synthmacs-core)
;;; synthmacs-core.el ends here
;; synthmacs-core:1 ends here
