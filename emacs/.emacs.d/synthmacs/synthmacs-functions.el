;;; package  --- Summary

;;; Commentary:

;;; Code:
;; -------------- Buffers --------------------
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

;; Copy file path

(defun synthmacs--directory-path ()
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

(defun synthmacs--file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun synthmacs--file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (synthmacs--file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun synthmacs/copy-directory-path ()
  "Copy and show the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (synthmacs--directory-path))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun synthmacs/copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (synthmacs--file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun synthmacs/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let* ((file-path (synthmacs--file-path))
            (file-name (file-name-nondirectory file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun synthmacs/copy-buffer-name ()
  "Copy and show the name of the current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "%s" (buffer-name)))

(defun synthmacs/copy-file-name-base ()
  "Copy and show the file name without its final extension of the current
buffer."
  (interactive)
  (if-let (file-name (file-name-base (synthmacs--file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun synthmacs/copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including line number."
  (interactive)
  (if-let (file-path (synthmacs--file-path-with-line))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

;; Rename a buffer
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

;; ----------- Minibuffer ------------------------
(defun synthmacs/minibuffer-backwards-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder; otherwise, delete a character backwards."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
	  (zap-up-to-char (- arg) ?/)
	(delete-minibuffer-contents))
    (delete-backwards-char arg)))

;; ---------------- Searching --------------
(defun synthmacs/consult-ripgrep ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --no-ignore --hidden --ignore-case --line-number"))
    (consult-ripgrep
     (if (projectile-project-p)
	 (projectile-project-root)
       ""))))

;; ---------------- Symbols --------------
(defun synthmacs/my-add-pretty-symbol ()
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          ;; ("->" . 8594)    ; →
          ;; ("=>" . 8658)    ; ⇒
          ;; ("map" . 8614)   ; ↦
          )))

;; ---------------- Quitting -------------------
(defun synthmacs/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Synthmacs"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(provide 'synthmacs-functions)
