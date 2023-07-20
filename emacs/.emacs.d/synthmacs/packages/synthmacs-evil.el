(setq evil-search-module 'evil-search)
(setq evil-ex-search-persistent-highlight t)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map "`" 'evil-goto-mark-line)
  (define-key evil-normal-state-map "'" 'evil-goto-mark)
  (define-key evil-visual-state-map "'" 'evil-goto-mark)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal))

(use-package evil-anzu
  :init (global-anzu-mode t)
  :config
  (setq anzu-search-threshold 1000
	anzu-cons-mode-line-p nil))

(use-package evil-args
  :defer t
  :init
  (progn
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

(use-package evil-cleverparens
  :defer t
  :init
  (setq evil-cleverparens-use-regular-insert t)
  :config
  ;; `evil-cp-change` should move to point
  (evil-set-command-properties 'evil-cp-change :move-point t))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-exchange
  :defer t
  :init
  (let ((evil-exchange-key (kbd "gx"))
	(evil-exchange-cancel-key (kbd "gX")))
    (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
    (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
    (define-key evil-normal-state-map evil-exchange-cancel-key
		'evil-exchange-cancel)
    (define-key evil-visual-state-map evil-exchange-cancel-key
		'evil-exchange-cancel)))

(use-package evil-goggles
  :init
  ;; disable pulses as it is more distracting than useful and
  ;; less readable.
  (setq evil-goggles-pulse nil
	evil-goggles-async-duration 0.1
	evil-goggles-blocking-duration 0.05)
  :config
  (evil-goggles-mode))

(use-package evil-lion
  :defer t
  :init
  (evil-define-key '(normal visual) 'global
    "gl" #'evil-lion-left
    "gL" #'evil-lion-right)
  :config (evil-lion-mode))

;; https://github.com/syl20bnr/evil-lisp-state
;; (use-package evil-lisp-state
;; :defer t
;; :init
;; (add-hook 'prog-mode-hook 'synthmacs/load-evil-lisp-state)
;; (setq evil-lisp-state-global t))

;; (synthmacs/leader-keys
;;   "l" '(evil-lisp-state-map :wk "lisp-state"))
;; (spacemacs/declare-prefix
;;   "l" "lisp"
;;   "ld" "delete"
;;   "lD" "delete-backward"
;;   "l`" "hybrid")))

;; (defun synthmacs/load-evil-lisp-state ()
;;     "Loads evil-lisp-state lazily"
;;   (remove-hook 'prog-mode-hook #'synthmacs/load-evil-lisp-state))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

(use-package evil-numbers
  :defer t
  :general
  (synthmacs/leader-keys
    "n" '(synthmacs/hydra/numbers-state/body :wk "numbers")))

(defhydra synthmacs/hydra/numbers-state (:timeout 7)
  "
^Numbers Menu
^^^^^^^^----------------------
_+_: increment-at-point
_-_: decrement-at-point
_q_: quit
"
  ("+" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt)
  ("q" nil :exit t))

(use-package evil-surround
  :init
  ;; `s' for surround instead of `substitute'
  ;; see motivation here:
  ;; https://github.com/syl20bnr/spacemacs/blob/develop/doc/DOCUMENTATION.org#the-vim-surround-case
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
  :config
  (global-evil-surround-mode 1))

(use-package evil-textobj-line)

(use-package evil-visual-mark-mode
  :defer t
  :general
  (synthmacs/leader-keys
    "t`" '(evil-visual-mark-mode :wk "visual-mark-mode")))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (define-key evil-visual-state-map (kbd "*") 'evil-visualstar/begin-search-forward)
  (define-key evil-visual-state-map (kbd "#") 'evil-visualstar/begin-search-backward))
;; Evil-Vimish-Fold - https://github.com/alexmurray/evil-vimish-fold
;;(use-package evil-vimish-fold
;;  :after vimish-fold
;;  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(provide 'synthmacs-evil)
