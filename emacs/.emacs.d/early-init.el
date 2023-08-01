;; [[file:synthmacs.org::*early-init.el][early-init.el:1]]
;;; early-init.el --- Synthmacs Early Init

;;; Commentary:

;; Set up conditions for Emacs startup.

;;; Code:
;; early-init.el:1 ends here

;; [[file:synthmacs.org::*Disable package/UI at first][Disable package/UI at first:1]]
;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(setq menu-bar-mode -1)
(setq tool-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;; Native-Comp
(setq native-comp-speed 2
      comp-speed 2)
(setq native-comp-async-report-warnings-errors nil
      comp-async-report-warnings-errors nil)
(setq native-comp-async-query-on-exit t
      comp-async-query-on-exit t)
;; Disable package/UI at first:1 ends here

;; [[file:synthmacs.org::*Reduce garbage collection][Reduce garbage collection:1]]
;; max memory available for gc on startup
(defvar synthmacs/gc-cons-threshold 16777216)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold synthmacs/gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; max memory available for gc when opening minibuffer
(defun synthmacs/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun synthmacs/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold synthmacs/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'synthmacs/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'synthmacs/restore-garbage-collection-h)
(setq garbage-collection-messages t)
;; Reduce garbage collection:1 ends here

;; [[file:synthmacs.org::*Temporarily avoid special handling of files][Temporarily avoid special handling of files:1]]
(unless (daemonp)
  (defvar doom--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun doom-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist'
    ;; since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'doom--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist doom--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h)
  )
;; Temporarily avoid special handling of files:1 ends here

;; [[file:synthmacs.org::*early-init.el][early-init.el:1]]
;;; early-init.el ends here
;; early-init.el:1 ends here
