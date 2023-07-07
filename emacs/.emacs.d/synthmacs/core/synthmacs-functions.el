;; -------------- Buffers --------------------
(defun synthmacs/add-buffer-to-killed-list ()
  "If buffer is associated with a file name, add that file
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
(defun buf-move-up ()
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
(defun buf-move-down ()
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
(defun buf-move-left ()
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
(defun buf-move-right ()
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

;; --------------------- Searching --------------
(defun synthmacs/consult-ripgrep ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --no-ignore --hidden --ignore-case --line-number"))
    (consult-ripgrep "")))

;; ---------------- Quitting -------------------
(defun synthmacs/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Synthmacs"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(provide 'synthmacs-functions)
