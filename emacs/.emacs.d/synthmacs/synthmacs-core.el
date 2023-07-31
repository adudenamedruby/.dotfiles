;; [[file:../synthmacs.org::*synthmacs-core][synthmacs-core:1]]
;;; synthmacs-core.el --- Synthmacs Core

;;; Commentary:

;; Set up core Synthmacs functionalities

;;; Code:
;; synthmacs-core:1 ends here

;; [[file:../synthmacs.org::*Emacs Setup][Emacs Setup:1]]
(use-package emacs
  :init
  (setq default-directory "~/")
  ;; quiet startup
  (setq inhibit-startup-message t)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)
  (setq sentence-end-double-space nil)
  (setq ring-bell-function 'ignore)
  (setq frame-resize-pixelwise t)
  ;; write over selected text on input... like all modern editors do
  (setq delete-selection-mode t)
  ;; clean up dired buffers
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)
  )
;; Emacs Setup:1 ends here

;; [[file:../synthmacs.org::*Universal argument][Universal argument:1]]
(use-package emacs
  :init
  (global-set-key (kbd "C-M-u") 'universal-argument))
;; Universal argument:1 ends here

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
  (set-file-name-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
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
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror))
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

;; [[file:../synthmacs.org::*Other][Other:1]]
(use-package emacs
  :init
  ;; use common convention for indentation by default
  (setq-default indent-tabs-mode nil)
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

  (defcustom synthmacs/variable-pitch-font-family "Iosevka"
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
  (set-face-attribute 'default nil
                      :font synthmacs/default-font-family
                      :height 140)
  ;; Set the fixed pitch face (monospace)
  (set-face-attribute 'fixed-pitch nil
                      :font synthmacs/default-font-family
                      :height 140)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font synthmacs/variable-pitch-font-family
                      :height 140)
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

;; [[file:../synthmacs.org::*Enlarge window][Enlarge window:1]]
(use-package emacs
  :init
  (defun synthmacs/window-enlargen (&optional arg)
    "Enlargen the current window to focus on this one. Does not close other
windows (unlike `doom/window-maximize-buffer'). Activate again to undo."
    (interactive "P")
    (let ((param 'doom--enlargen-last-wconf))
      (cl-destructuring-bind (window . wconf)
          (or (frame-parameter nil param)
              (cons nil nil))
        (set-frame-parameter
         nil param
         (if (and (equal window (selected-window))
                  (not arg)
                  wconf)
             (ignore
              (let ((source-window (selected-window)))
                (set-window-configuration wconf)
                (when (window-live-p source-window)
                  (select-window source-window))))
           (prog1 (cons (selected-window) (or wconf (current-window-configuration)))
             (let* ((window (selected-window))
                    (dedicated-p (window-dedicated-p window))
                    (preserved-p (window-parameter window 'window-preserved-size))
                    (ignore-window-parameters t)
                    (window-resize-pixelwise nil)
                    (frame-resize-pixelwise nil))
               (unwind-protect
                   (progn
                     (when dedicated-p
                       (set-window-dedicated-p window nil))
                     (when preserved-p
                       (set-window-parameter window 'window-preserved-size nil))
                     (maximize-window window))
                 (set-window-dedicated-p window dedicated-p)
                 (when preserved-p
                   (set-window-parameter window 'window-preserved-size preserved-p))
                 (add-hook 'doom-switch-window-hook #'doom--enlargened-forget-last-wconf-h)))))))))
  )
;; Enlarge window:1 ends here

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

;; [[file:../synthmacs.org::*Prefer ~rg~ and ~fd~ if available][Prefer ~rg~ and ~fd~ if available:1]]
(when (executable-find "rg")
  (setq grep-program "rg"))

(when (executable-find "fd")
  (setq find-program "fd"))
;; Prefer ~rg~ and ~fd~ if available:1 ends here

;; [[file:../synthmacs.org::*xref][xref:1]]
(use-package xref
  :init
  (setq xref-prompt-for-identifier nil) ;; always find references of symbol at point
  ;; configured in consult
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  ;; (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  ;; (setq xref-file-name-display 'project-relative)
  ;; (setq xref-search-program 'grep)
  )
;; xref:1 ends here

;; [[file:../synthmacs.org::*synthmacs-core][synthmacs-core:1]]
(provide 'synthmacs-core)
;;; synthmacs-core.el ends here
;; synthmacs-core:1 ends here
