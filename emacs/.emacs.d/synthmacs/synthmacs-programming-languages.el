;; [[file:../synthmacs.org::*synthmacs-programming-languages][synthmacs-programming-languages:1]]
;;; synthmacs-programming-languages.el --- Synthmacs Programming Languages

;;; Commentary:

;; Set up programming languages & language tools

;;; Code:
;; synthmacs-programming-languages:1 ends here

;; [[file:../synthmacs.org::*CSS][CSS:1]]
(use-package css-mode
  :mode "\\.css\\'"
  :hook
  (css-mode . siren-css-mode-setup)
  :custom
  (css-indent-offset 2)
  )
;; CSS:1 ends here

;; [[file:../synthmacs.org::*Git-Modes][Git-Modes:1]]
(use-package git-modes)
;; Git-Modes:1 ends here

;; [[file:../synthmacs.org::*HTML][HTML:1]]
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))
;; HTML:1 ends here

;; [[file:../synthmacs.org::*JSON][JSON:1]]
(use-package json-mode
  :commands (json-mode)
  :general
  (synthmacs/local-leader-keys
    :keymaps 'json-mode-map
    "p" 'json-pretty-print)
  )
;; JSON:1 ends here

;; [[file:../synthmacs.org::*clojure-mode][clojure-mode:1]]
(use-package clojure-mode
  :mode "\\.clj$"
  :init
  (setq clojure-align-forms-automatically t))
;; clojure-mode:1 ends here

;; [[file:../synthmacs.org::*clojure-lsp][clojure-lsp:1]]
(use-package clojure-mode
  :hook
  ((clojure-mode clojurescript-mode)
   . (lambda ()
       (setq-local lsp-enable-indentation nil ; cider indentation
                   lsp-enable-completion-at-point nil ; cider completion
                   )
       (lsp-deferred)))
  )
;; clojure-lsp:1 ends here

;; [[file:../synthmacs.org::*Cider][Cider:1]]
(use-package cider
  :hook ((cider-repl-mode . evil-normalize-keymaps)
         (cider-mode . (lambda ()
                         (setq-local evil-lookup-func #'cider-doc)))
         (cider-mode . eldoc-mode))
  :general
  (synthmacs/local-leader-keys
    :keymaps 'clojure-mode-map
    "c" '(cider-connect-clj :wk "connect")
    "C" '(cider-connect-cljs :wk "connect (cljs)")
    "j" '(cider-jack-in :wk "jack in")
    "J" '(cider-jack-in-cljs :wk "jack in (cljs)")
    "d" 'cider-debug-defun-at-point 
    
    "e" '(:ignore t :wk "evaluate")
    "eb" 'cider-eval-buffer
    "el" 'cider-eval-last-sexp
    "eL" 'cider-pprint-eval-last-sexp-to-comment
    "ed" '(cider-eval-defun-at-point :wk "defun")
    "eD" 'cider-pprint-eval-defun-to-comment
    
    "h" 'cider-clojuredocs-web 
    "D" 'cider-doc
    "q" '(cider-quit :wk "quit")
    )
  
  (synthmacs/local-leader-keys
    :keymaps 'clojure-mode-map
    :states 'visual
    "e" 'cider-eval-region)
  
  :init
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-sync-request-timeout nil)
  (setq cider-repl-display-help-banner nil)
  )
;; Cider:1 ends here

;; [[file:../synthmacs.org::*ob-clojure][ob-clojure:1]]
(use-package org
  :config
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider))
;; ob-clojure:1 ends here

;; [[file:../synthmacs.org::*Common Lisp][Common Lisp:1]]
(defvar inferior-lisp-program "sbcl")
(use-package sly
  :general
  (synthmacs/local-leader-keys
    :keymaps 'lisp-mode-map
    "'" 'sly
    "m" 'macrostep-expand
    
    "c" '(:ignore t :wk "compile")
    "cc" 'sly-compile-file
    "cC" 'sly-compile-and-load-file
    "cf" 'sly-compile-defun
    "cl" 'sly-load-file
    "cn" 'sly-remove-notes
    "cr" 'sly-compile-region
    
    "e" '(:ignore t :wk "evaluate")
    "eb" '(sly-eval-buffer :wk "Evaluate buffer")
    "ee" '(sly-eval-last-expression :wk "Evaluate last")
    "eE" '(sly-eval-print-last-expression :wk "Evaluate/print last")
    "ef" '(sly-eval-defun :wk "Evaluate defun")
    "eF" '(sly-undefine-function :wk "Undefine function")
    "er" '(sly-eval-region :wk "Evaluate region")
    
    "g" '(:ignore t :wk "goto")
    "gb" '(sly-pop-find-definition-stack :wk "Go back")
    "gd" '(sly-edit-definition :wk "Go to")
    "gD" '(sly-edit-definition-other-window :wk "Go to (other window)")
    "gn" '(sly-next-note :wk "Next note")
    "gN" '(sly-previous-note :wk "Previous note")
    "gs" '(sly-stickers-next-sticker :wk "Next sticker")
    "gS" '(sly-stickers-prev-sticker :wk "Previous sticker")
    
    "h" '(:ignore :wk "help")
    "h<" '(sly-who-calls :wk "Who calls")
    "h>" '(sly-calls-who :wk "Calls who")
    "h~" '(hyperspec-lookup-format :wk "Lookup format directive")
    "h#" '(hyperspec-lookup-reader-macro :wk "Lookup reader macro")
    "ha" '(sly-apropos :wk "Apropos")
    "hb" '(sly-who-binds :wk "Who binds")
    "hd" '(sly-disassemble-symbol :wk "Disassemble symbol")
    "hh" '(sly-describe-symbol :wk "Describe symbol")
    "hH" '(sly-hyperspec-lookup :wk "HyperSpec lookup")
    "hm" '(sly-who-macroexpands :wk "Who macro-expands")
    "hp" '(sly-apropos-package :wk "Apropos package")
    "hr" '(sly-who-references :wk "Who references")
    "hs" '(sly-who-specializes :wk "Who specializes")
    "hS" '(sly-who-sets :wk "Who sets")
    
    "r" '(:ignore :wk "repl")
    "rc" '(sly-mrepl-clear-repl :wk "Clear REPL")
    "rl" '(+lisp/load-project-systems :wk "Load Project")
    "rq" '(sly-quit-lisp :wk "Quit connection")
    "rr" '(sly-restart-inferior-lisp :wk "Restart connection")
    "rR" '(+lisp/reload-project :wk "Reload Project")
    "rs" '(sly-mrepl-sync :wk "Sync REPL")
    
    "s" '(:ignore :wk "stickers")
    "sb" '(sly-stickers-toggle-break-on-stickers :wk "Toggle breaking stickers")
    "sc" '(sly-stickers-clear-defun-stickers :wk "Clear defun stickers")
    "sC" '(sly-stickers-clear-buffer-stickers :wk "Clear buffer stickers")
    "sf" '(sly-stickers-fetch :wk "Fetch stickers")
    "sr" '(sly-stickers-replay :wk "Replay stickers")
    "ss" '(sly-stickers-dwim :wk "Add/remove sticker")
    
    "t" '(:ignore t :wk "test")
    "ts" '(+lisp/test-system :wk "Test System")
    
    "T" '(:ignore t :wk "trace")
    "Tt" '(sly-toggle-trace-fdefinition :wk "Toggle")
    "TT" '(sly-toggle-fancy-trace :wk "Toggle (fancy)")
    "Tu" '(sly-untrace-all :wk "Untrace all")
    )
  :hook (lisp-mode-local-vars . sly-editing-mode)
  :config
  (setq sly-complete-symbol-function 'sly-flex-completions)
  )

(use-package sly-macrostep)

(use-package sly-repl-ansi-color
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))
;; Common Lisp:1 ends here

;; [[file:../synthmacs.org::*Racket & Scheme][Racket & Scheme:1]]
(use-package geiser
  :straight t
  :config
  (setq geiser-active-implementations '(chez)))

(use-package geiser-chez
  :straight t
  :init
  (setq geiser-chez-binary "scheme"))

(use-package racket-mode
  :straight t
  :mode ("\\.rkt\\'" "\\.scrbl\\'")
  :hook ((racket-mode . racket-xp-mode)))
;; Racket & Scheme:1 ends here

;; [[file:../synthmacs.org::*Emacs-lisp][Emacs-lisp:1]]
(use-package emacs
  :straight (:type built-in)
  :general
  (general-nmap
    :keymaps 'emacs-lisp-mode-map
    :states 'normal
    "gr" nil) ;; interferes with eval-operator
  )
;; Emacs-lisp:1 ends here

;; [[file:../synthmacs.org::*aggressive-indent][aggressive-indent:1]]
(use-package aggressive-indent
    :hook ((clojure-mode . aggressive-indent-mode)
           (lisp-mode . aggressive-indent-mode)
           (emacs-lisp-mode . aggressive-indent-mode)))
;; aggressive-indent:1 ends here

;; [[file:../synthmacs.org::*evil-lisp-state][evil-lisp-state:1]]
(use-package evil-lisp-state
  :after evil
  :demand
  :init
  (setq evil-lisp-state-enter-lisp-state-on-command nil)
  (setq evil-lisp-state-global t)
  ;; (setq evil-lisp-state-major-modes '(org-mode emacs-lisp-mode clojure-mode clojurescript-mode lisp-interaction-mode))
  :config
  (evil-lisp-state-leader "SPC l")
  )
;; evil-lisp-state:1 ends here

;; [[file:../synthmacs.org::*Lispy][Lispy:1]]
(use-package lispy
  :straight t
  :hook ((emacs-lisp-mode . lispy-mode)
         (lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package lispyville
  :straight t
  :hook (lispy-mode . lispyville-mode))
;; Lispy:1 ends here

;; [[file:../synthmacs.org::*LUA][LUA:1]]
(use-package lua-mode)
;; LUA:1 ends here

;; [[file:../synthmacs.org::*Markdown][Markdown:1]]
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))
;; Markdown:1 ends here

;; [[file:../synthmacs.org::*Rust][Rust:1]]
(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :straight t)
;; Rust:1 ends here

;; [[file:../synthmacs.org::*swift-mode][swift-mode:1]]
;; (use-package lsp-sourcekit
;;   :after swift-mode
;;   :config
;;   (setq lsp-sourcekit-executable (cl-find-if #'executable-find
;;                                              (list lsp-sourcekit-executable ; 'sourcekit-lsp' by default
;;                                                    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"
;;                                                    "sourcekit"
;;                                                    "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit-lsp"
;;                                                    "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit"))))

(use-package swift-mode
  :mode "\\.swift\\'"
  ;; :hook (swift-mode . (lambda () (lsp)))
  :config
  (setq swift-mode:basic-offset 4
        swift-mode:parenthesized-expression-offset 4
        swift-mode:multiline-statement-offset 0
        swift-mode:highlight-anchor t))

;; (use-package flycheck-swift
;;   :after swift-mode
;;   :config
;;   (flycheck-swift-setup)
;;   ;; (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS15.2.sdk")
;;   ;; (setq flycheck-swift-target "arm64-apple-ios10") 
;;   )
;; swift-mode:1 ends here

;; [[file:../synthmacs.org::*apple-docs-query][apple-docs-query:1]]
(require 'json)
(require 'url-http)
(require 'cl-lib)

(defgroup apple-docs-query nil
  "Apple Docs Query Group."
  :tag "apple-docs-query"
  :group 'apple-docs-query)

(defface apple-docs-title-face
  '((t (:inherit font-lock-keyword-face :bold t)))
  "Title face."
  :group 'apple-docs-query)

(defface apple-docs-description-face
  '((t (:inherit completions-annotations)))
  "Description face."
  :group 'apple-docs-query)

(defconst apple-developer-url "https://developer.apple.com"
  "Developer apple site.")

(cl-defun request-data-from-apple-docs (&key url)
  "Request data (as URL)."
  (let* ((request-curl-options (list "-H" (string-trim (url-http-user-agent-string)))))
    (request url
      :type "GET"
      :parser 'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let* ((c2-width (round (* (- (window-width) 9) 0.4)))
                (choices (mapcar (lambda (item)
                                   (let-alist item
                                     (cons
                                      (propertize
                                       (format "%s %s"
                                               (propertize
                                                (or
                                                 (assoc-default 'title item) "") 'face 'apple-docs-title-face)
                                               (truncate-string-to-width
                                                (propertize
                                                 (or
                                                  (assoc-default 'description item) "")
                                                 'face 'apple-docs-description-face) c2-width nil 32)))
                                      (assoc-default 'url item)))) (cdr (car data))))
                (selected (completing-read "Apple docs: " choices))
                (url (cdr (assoc selected choices))))
           (browse-apple-url url)))))
    nil))

(cl-defun browse-apple-url (url)
  "Browse URL."
  (if-let ((url url))
      (browse-url (concat apple-developer-url url))))

(defun apple-docs/query (query)
  "Query Hacking with swift (as QUERY)."
  (interactive "sQuery:")
  (when-let ((url (url-encode-url (format "%s/search/search_data.php?q=%s&type=Documentation" apple-developer-url query))))
    (request-data-from-apple-docs :url url)))

(defun apple-docs/query-thing-at-point ()
  "Query thing at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (apple-docs/query word)))

(use-package emacs
  :general
  (synthmacs/local-leader-keys
    :keymaps 'swift-mode-map
    "a" '(apple-docs/query :wk "query-apple")
    "A" '(apple-docs/query-thing-at-point :wk "query-apple-thing-at-point")))
;; apple-docs-query:1 ends here

;; [[file:../synthmacs.org::*mode-line-hud][mode-line-hud:1]]
(defgroup mode-line-hud nil
  "Mode-line hud."
  :tag "mode-line-hud"
  :group 'mode-line-hud)

(defvar-local mood-line-segment-hud--text "")

(defcustom show-in-echo-area t
  "Show weather temperature in fahrenheit."
  :group 'mode-line-hud
  :type '(boolean))

;;;###autoload
(defun mood-line-segment-hud ()
  "Return the number of active multiple-cursors."
  mood-line-segment-hud--text)

;;;###autoload
(cl-defun mode-line-hud:update (&key message)
  "Update modeline as (MESSAGE)."
  (run-with-timer 0.05 nil
                  (lambda ()
                    (setq mood-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

;;;###autoload
(cl-defun mode-line-hud:updateWith (&key message &key delay)
  "Update modeline as (MESSAGE DELAY)."
  (run-with-timer delay nil
                  (lambda ()
                    (setq mood-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

;;;###autoload
(cl-defun mode-line-hud:notification (&key message &key seconds)
  "Update modeline as (MESSAGE SECONDS)."
  (run-with-timer 0.025 nil
                  (lambda ()
                    (mode-line-hud:reset :message mood-line-segment-hud--text :delay seconds)
                    (setq mood-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

(cl-defun mode-line-hud:reset (&key message &key delay)
  "Reset to previous MESSAGE."
  (run-with-timer delay nil
                  (lambda ()
                    (setq mood-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))
;; mode-line-hud:1 ends here

;; [[file:../synthmacs.org::*xcodebuildserver][xcodebuildserver:1]]
(defgroup xcodebuildserver nil
  "Xcodebuildserver."
  :tag "xcodebuidserver"
  :group 'xcodebuildserver)

(cl-defun xcodebuildserver:check-configuration (&key root &key workspace &key scheme)
  "Check if there is a configuration in (as ROOT) (as WORKSPACE) (as SCHEME)."
  (when (not (xcodebuildserver:does-configuration-file-exist root))
    (let ((default-directory root)
          (command (format "xcode-build-server config %s -scheme '%s' > /dev/null 2>&1" workspace scheme)))
      (async-shell-command command))))

(defun xcodebuildserver:does-configuration-file-exist (root)
  "Check if configuration file exists in (as ROOT)."
  (file-exists-p (format "%s/%s" root "buildServer.json")))
;; xcodebuildserver:1 ends here

;; [[file:../synthmacs.org::*periphery][periphery:1]]
(defface periphery-filename-face
  '((t (:inherit link)))
  "Filename face."
  :group 'periphery)

(defface periphery-linenumber-face
  '((t (:inherit line-number)))
  "Line number face."
  :group 'periphery)

(defface periphery-warning-face
  '((t (:foreground "#f9e2af")))
  "Warning face."
  :group 'periphery)

(defface periphery-warning-face-full
  '((t (:foreground "#f9e2af" :bold t :background "#2E2A1E" :distant-foreground "#f9e2af" )))
  "Warning face."
  :group 'periphery)

(defface periphery-error-face
  '((t (:foreground "#f38ba8")))
  "Error face."
  :group 'periphery)

(defface periphery-error-face-full
  '((t (:foreground "#f38ba8" :bold t :background "#2D1E28" :distant-foreground "#f38ba8")))
  "Error face."
  :group 'periphery)

(defface periphery-identifier-face
  '((t (:inherit periphery-error-face :background "#2D1E28")))
  "Identifier face."
  :group 'periphery)

(defface periphery-message-face
  '((t (:foreground "#9399b2" :font-weight thin)))
  "Message face."
  :group 'periphery)

(defface periphery-fix-face
  '((t (:foreground "#89b4fa")))
  "FIX|FIXME face."
  :group 'periphery)

(defface periphery-fix-face-full
  '((t (:foreground "#1B2431" :bold t :background "#89b4fa" :distant-foreground "#89b4fa")))
  "FIX with background."
  :group 'periphery)

(defface periphery-note-face
  '((t (:inherit compilation-info)))
  "Info face."
  :group 'periphery)

(defface periphery-note-face-full
  '((t (:foreground "#1E2B2E" :bold t :background "#a6e3a1" :distant-foreground "#a6e3a1")))
  "Info face."
  :group 'periphery)

(defface periphery-info-face
  '((t (:inherit periphery-note-face)))
  "Note face."
  :group 'periphery)

(defface periphery-info-face-full
  '((t (:inherit periphery-note-face-full)))
  "Note face full."
  :group 'periphery)

(defface periphery-performance-face
  '((t (:foreground "#cba6f7")))
  "Performance face."
  :group 'periphery)

(defface periphery-performance-face-full
  '((t (:foreground "#2B1E2E" :bold t :background "#cba6f7" :distant-foreground "#cba6f7" )))
  "Performance face."
  :group 'periphery)

(defface periphery-hack-face-full
  '((t (:foreground "#28181C" :bold t :background "#f38ba8" :distant-foreground  "#f38ba8")))
  "Performance face."
  :group 'periphery)

(defface periphery-todo-face
  '((t (:foreground "#74c7ec")))
  "Performance face."
  :group 'periphery)

(defface periphery-todo-face-full
  '((t (:foreground "#182A32" :bold t :background "#74c7ec" :distant-foreground  "#74c7ec")))
  "Performance face."
  :group 'periphery)

(defface periphery-mark-face-full
  '((t (:foreground "#313244" :background "#9399b2" :distant-foreground "#9399b2" :weight light)))
  "Performance face."
  :group 'periphery)

(defface periphery-background-face
  '((t (:inherit default)))
  "Buffer background color."
  :group 'periphery)

(defvar periphery-mode-map nil
  "Keymap for periphery.")

(setq periphery-mode-map (make-sparse-keymap))
(define-key periphery-mode-map (kbd "RET") #'periphery--open-current-line)
(define-key periphery-mode-map (kbd "<return>") 'periphery--open-current-line)
(define-key periphery-mode-map (kbd "o") 'periphery--open-current-line)

(defconst periphery-buffer-name "*Periphery*")

(defvar periphery-mode-map nil "Keymap for periphery.")

(defvar default-length 8)

(defconst periphery-regex-parser "\\(\\/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)\\s?\\(\\w+\\):\\(.*\\)\n\\s+\\(.*\\(?:\n\\s+.*\\)*\\)"
  "Parse vimgrep like strings (compilation).")

(defconst mark-inside-parenteses "\\(\(.+?\)\\)"
  "Mark parenteses.")

(defconst mark-strings-regex "\\(\"[^\"]+\"\\)"
  "Mark all string.")

(defconst periphery-regex-mark-quotes "\\('[^']+'\\)"
  "Mark quotes in text.")

(defconst xctest-regex-parser "^\\([^:]+\\):\\([0-9]+\\):\\w?\\([^:]*\\)[^.]+\\.\\([^:|]*\\)\s?:\\(.*\\)"
  "XCTest regex.")

(defconst todos-clean-regex "\\(TODO\\|PERF\\|NOTE\\|FIXME\\|FIX\\|HACK\\|MARK\\)\s?:?\s?\\(.*\\)"
  "Parse Todos and hacks.")

(defconst periphery-parse-search "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).\\(.*\\)")

(defvar-local periphery-errorList '())

(define-derived-mode periphery-mode tabulated-list-mode "Periphery-mode"
  "Periphery mode.  A mode to show compile errors like Flycheck."
  (setq tabulated-list-format [
                               ("Type" 9 nil)
                               ("File" 28 t)
                               ("Line" 4 nil)
                               ("Message" 120 nil)
                               ]
        tabulated-list-padding 1
        tabulated-list-sort-key (cons "Line" nil))
  (use-local-map periphery-mode-map)
  (tabulated-list-init-header))

(defun periphery--go-to-first-error ()
  "Go to the first error in the periphery-errorList."
  (interactive)
  (when (and periphery-errorList (> (length periphery-errorList) 0))
    (let* ((first-error (car periphery-errorList))
           (file (nth 1 first-error))
           (line (string-to-number (nth 2 first-error)))))
    (message "%s" file)))

(defun periphery--open-current-line ()
  "Open current current line."
  (interactive)
  (open-current-line-with (tabulated-list-get-id)))

(defun periphery--listing-command (errorList)
  "Create an ERRORLIST for the current mode."
  (let ((errors '())
        (warnings '()))
    ;; Separate errors from warnings
    (dolist (entry errorList)
      (let ((severity (aref (car (cdr entry)) 0)))
        (if (string-prefix-p "error" severity)
            (setq errors (cons entry errors))
          (setq warnings (cons entry warnings)))))
    ;; Sort errors and warnings separately and combine them
    (setq errors (sort errors (lambda (a b) (string< (aref (car (cdr a)) 1) (aref (car (cdr b)) 1)))))
    (setq warnings (sort warnings (lambda (a b) (string< (aref (car (cdr a)) 1) (aref (car (cdr b)) 1)))))
    (setq errorList (append errors warnings)))

  (save-selected-window
    (let* ((buffer (get-buffer-create periphery-buffer-name))
           (window (get-buffer-window buffer)))
      (pop-to-buffer buffer nil)
      (periphery-mode)

      (unless (equal (current-buffer) buffer)
        (select-window window))

      (setq tabulated-list-entries (nreverse (-non-nil errorList)))

      (tabulated-list-print t)
      ;; (periphery--go-to-first-error tabulated-list-entries)

      (if (proper-list-p tabulated-list-entries)
          (periphery-message-with-count
           :tag ""
           :text "Errors or warnings"
           :count (format "%d" (length tabulated-list-entries))
           :attributes 'error)))))

(cl-defun periphery--mark-all-symbols (&key input &key regex &key property)
  "Highlight all quoted symbols (as INPUT REGEX PROPERTY)."
  (save-match-data
    (let* ((position 0)
           (normalizedInput (replace-regexp-in-string "’" "'"  input)))
      (while (string-match regex normalizedInput position)
        (let* ((ref (match-string 1 normalizedInput))
               (startPosition (string-match regex normalizedInput position)))
          (setq position (match-end 1))
          (add-text-properties startPosition position property normalizedInput)))
  normalizedInput)))

(defun periphery--parse-xctest-putput (line)
  "Run regex for xctest case."
  (save-match-data
    (and (string-match xctest-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (type (match-string 3 line))
                (result (match-string 4 line))
                (message (match-string 5 line))
                (fileWithLine (format "%s:%s" file linenumber)))

           (periphery--build-list
            :path fileWithLine
            :file result
            :line linenumber
            :keyword "Failed"
            :result message
            :regex mark-strings-regex)))))

(defun periphery--parse-output-line (line)
  "Run regex over curent LINE."
  (save-match-data
    (and (string-match periphery-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (column (match-string 3 line))
                (type (match-string 4 line))
                (result (match-string 5 line))
                (fileWithLine (format "%s:%s:%s" file linenumber column)))

           (periphery--build-list
            :path fileWithLine
            :file file
            :line linenumber
            :keyword type
            :result result
            :regex periphery-regex-mark-quotes)
           ))))

(defun periphery--propertize-severity (severity text)
  "Colorize TEXT using SEVERITY."
  (if-let* ((type (upcase (string-trim-left severity))))
    (propertize (format " %s " (periphery--center-text type)) 'face (periphery--full-color-from-keyword severity))))

(defun periphery--center-text (word)
  "Center (as WORD)."
  (if (<= (length word) default-length)
      (progn
        (setq padding  (/ (- default-length (string-width word)) 3))
        (setq copy (concat (make-string padding ?\s) word))
        
        (while (< (string-width copy) (- default-length 1))
          (setq copy (concat copy " ")))
        copy
        )
    word))

(cl-defun periphery--full-color-from-keyword (keyword)
  "Get color from KEYWORD."
  (let ((type (upcase (string-trim-left keyword))))
    (cond
     ((string= type "WARNING") 'periphery-warning-face-full)
     ((string= type "MATCH") 'periphery-warning-face-full)
     ((string= type "INFO") 'periphery-note-face-full)
     ((string= type "ERROR") 'periphery-error-face-full)
     ((string= type "NOTE") 'periphery-note-face-full)
     ((or (string= type "FIX") (string= type "FIXME")) 'periphery-fix-face-full)
     ((or (string= type "PERF") (string= type "PERFORMANCE")) 'periphery-performance-face-full)
     ((string= type "TODO") 'periphery-todo-face-full)
     ((string= type "HACK") 'periphery-hack-face-full)
     (t 'periphery-error-face-full))))

(cl-defun periphery--color-from-keyword (keyword)
  "Get color from KEYWORD."
  (let ((type (upcase (string-trim-left keyword))))
    (cond
     ((string= type "WARNING") 'periphery-warning-face)
     ((string= type "TODO") 'periphery-todo-face)
     ((or (string= type "ERROR") (string= type "HACK")) 'periphery-error-face)
     ((or (string= type "FIX") (string= type "FIXME")) 'periphery-fix-face)
     ((or (string= type "PERF") (string= type "PERFORMANCE")) 'periphery-performance-face)
     (t 'periphery-info-face))))

(defun periphery--is-list-empty-alt (lst)
  "Alternative method to check if the given list is empty."
  (and (listp lst) (null lst)))
(defun parse-compiler-errors-async (text callback)
  "Parse compiler error messages from LOG (as TEXT) asynchronously and call CALLBACK with the result."
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`tempList\\'")
      (let ((regex "\\(^/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)\\s+\\([^:]+\\):\\(.*\\)\\([^^|/]*\\)"))
        (while (string-match regex ,text)
          (let* ((path (match-string 1 ,text))
                 (line (match-string 2 ,text))
                 (column (match-string 3 ,text))
                 (error-type (match-string 4 ,text))
                 (msg-part1 (s-trim-left (match-string 5 ,text)))
                 (msg-part2 (s-trim-left (match-string 6 ,text)))
                 (msg (clean-up-newlines (format "%s: %s" msg-part1 msg-part2))))

            (push (periphery--build-list
                   :path (format "%s:%s:%s" path line column)
                   :file path
                   :line line
                   :keyword error-type
                   :result msg
                   :regex periphery-regex-mark-quotes)
                  tempList)
            ;; Update the text to the remaining unmatched portion
            (setq ,text (substring ,text (match-end 6))))))
      tempList)
   callback))

(defun parse-compiler-errors (text)
  "Parse compiler error messages from LOG (as TEXT)."
  (setq tempList nil)
  (let ((regex "\\(^\/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)\s+\\([^:]+\\):\\(.*\\)\\([^^|\/]*\\)"))
    (while (string-match regex text)
      (let* ((path (match-string 1 text))
             (line (match-string 2 text))
             (column (match-string 3 text))
             (error-type (match-string 4 text))
             (msg-part1 (s-trim-left (match-string 5 text)))
             (msg-part2 (s-trim-left (match-string 6 text)))
             (msg (clean-up-newlines (format "%s: %s" msg-part1 msg-part2))))

        (push (periphery--build-list
               :path (format "%s:%s:%s" path line column)
               :file path
               :line line
               :keyword error-type
               :result msg
               :regex periphery-regex-mark-quotes)
              tempList)
      ;; Update the text to the remaining unmatched portion
        (setq text (substring text (match-end 6))))))
  tempList)

(cl-defun periphery-run-parser (input)
  "Run parser (as INPUT as SUCCESSCALLBACK)."
  ;; Filter lines that don't start with "/" only if it doesn't contain "BUILD FAILED" or "BUILD INTERRUPTED"
  ;; (message input)
  (setq input (mapconcat #'identity (seq-filter (lambda (line) (string-match-p "^/" line)) (split-string input "\n")) "\n"))
  (setq periphery-errorList (delete-dups (parse-compiler-errors input)))

  (when (or (periphery--is-buffer-visible) periphery-errorList)
    (periphery--listing-command periphery-errorList)))

(defun periphery--is-buffer-visible ()
  "Check if a buffer is visible."
  (let ((buffer (get-buffer periphery-buffer-name)))
    (when buffer
      (let ((window (get-buffer-window buffer)))
        (if window
            t ; Buffer is visible
          nil))))) ; Buffer is not visible

(cl-defun periphery-run-test-parser (input succesCallback)
  (setq periphery-errorList nil)
  (dolist (line (split-string input "\n"))
    (let* ((compilation-entry (periphery--parse-output-line (string-trim-left line)))
           (test-entry (periphery--parse-xctest-putput (string-trim-left line))))
      (when compilation-entry
        (push compilation-entry periphery-errorList))
      (when test-entry
        (push test-entry periphery-errorList))))
  (if periphery-errorList
      (periphery--listing-command (delete-dups periphery-errorList))
    (funcall succesCallback)))

(defun periphery-kill-buffer ()
  "Kill the periphery buffer."
  (interactive)
  (when (get-buffer periphery-buffer-name)
    (kill-buffer periphery-buffer-name)))

(defun periphery-toggle-buffer ()
  "Toggle visibility of the Periphery buffer window."
  (interactive)
  (let ((buffer (get-buffer periphery-buffer-name)))
    (if (not buffer)
        (message "Buffer %s does not exist" periphery-buffer-name)
      (if (get-buffer-window buffer)
          (delete-window (get-buffer-window buffer))
        (display-buffer buffer)))))

;;; - Bartycrouch parsing
(defun periphery--clean-up-comments (text)
  "Cleanup comments from (as TEXT) fixmes and todos."
  (save-match-data
    (and (string-match todos-clean-regex text)
         (if-let* ((keyword (match-string 1 text))
                (comment (match-string 2 text)))
             (list keyword comment)))))

(cl-defun periphery-message-with-count (&key tag &key text &key count &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES with Count."
  (if (not (string= text ""))
      (mode-line-hud:update :message (format "%s %s '%s'" tag count (propertize text 'face attributes)))
    (mode-line-hud:update :message (format "%s %s" tag count ))))

(defun parse--search-query (text query)
  "Parse error and notes (as TEXT) and QUERY."
  (setq default-length 8)
  (setq-local case-fold-search nil) ;; Make regex case sensitive
  (save-match-data
    (and (string-match periphery-parse-search text)
         (let* ((file (match-string 1 text))
                (line (match-string 2 text))
                (column (match-string 3 text))
                (message (match-string 4 text))
                (fileWithLine (format "%s:%s:%s" file line column)))

           (if-let ((todo (periphery--clean-up-comments message)))
                 (periphery--build-todo-list
                  :path fileWithLine
                  :file file
                  :line line
                  :keyword (nth 0 todo)
                  :result (nth 1 todo)
                  :regex (format "\\(%s\\)" query))

             (periphery--build-list
              :path fileWithLine
              :file file
              :line line
              :keyword "Match"
              :result message
              :regex (format "\\(%s\\)" query)))))))


(cl-defun periphery--build-list (&key path &key file &key line &key keyword &key result &key regex)
  "Build list from (as PATH FILE LINE KEYWORD RESULT REGEX)."
  (list path (vector
              (periphery--propertize-severity keyword (string-trim-left keyword))
              (propertize (file-name-sans-extension (file-name-nondirectory file)) 'face 'periphery-filename-face)
              (propertize line 'face 'periphery-linenumber-face)
              (periphery--mark-all-symbols
               :input (periphery--mark-all-symbols
                       :input (periphery--mark-all-symbols
                               :input (propertize
                                       (string-trim-left result)
                                       'face
                                       'periphery-message-face)
                               :regex mark-strings-regex
                               :property '(face highlight))
                       :regex mark-inside-parenteses
                       :property '(face periphery-warning-face))
               :regex regex
               :property '(face periphery-identifier-face)))))

(cl-defun periphery--build-todo-list (&key path &key file &key line &key keyword &key result &key regex)
  "Build list from (as PATH FILE LINE KEYWORD RESULT REGEX)."
  (list path (vector
              (periphery--propertize-severity keyword (string-trim-left keyword))
              (propertize (file-name-sans-extension (file-name-nondirectory file)) 'face 'periphery-filename-face)
              (propertize line 'face 'periphery-linenumber-face)
              (periphery--mark-all-symbols
               :input (periphery--mark-all-symbols
                       :input (periphery--mark-all-symbols
                               :input
                                (propertize
                                 result
                                 'face
                                  (periphery--color-from-keyword keyword))
                               :regex mark-strings-regex
                               :property '(face highlight))
                       :regex mark-inside-parenteses
                       :property '(face periphery-warning-face))
               :regex regex
               :property '(face periphery-identifier-face)))))

(cl-defun periphery-parse-search-result (&key title &key text &key query)
  "Parse search result (as TITLE TEXT QUERY)."
  (setq default-length 8)
  (setq periphery-errorList '())
  (dolist (line (split-string text "\n"))
    (when-let ((entry (parse--search-query (string-trim-left line) query)))
      (push entry periphery-errorList)))

  (when periphery-errorList
    (progn
      (periphery--listing-command periphery-errorList)
      (if (proper-list-p tabulated-list-entries)
          (periphery-message-with-count
           :tag "Found"
           :text ""
           :count (format "%d" (length periphery-errorList))
           :attributes 'success))
      (switch-to-buffer-other-window periphery-buffer-name))))

(defun svg-color-from-tag (tag)
  "Get color from (as TAG)."
  (cond
   ((string-match-p "TODO" tag) 'periphery-todo-face-full)
   ((string-match-p "NOTE" tag) 'periphery-note-face-full)
   ((string-match-p "HACK" tag) 'periphery-hack-face-full)
   ((string-match-p "PERF" tag) 'periphery-performance-face-full)
   ((string-match-p "FIXME" tag) 'periphery-fix-face-full)
   ((string-match-p "FIX" tag) 'periphery-fix-face-full)
   ((string-match-p "MARK" tag) 'periphery-mark-face-full)
   (t 'periphery-hack-face-full)))
;; periphery:1 ends here

;; [[file:../synthmacs.org::*periphery-helper][periphery-helper:1]]
(defconst periphery-parse-line-regex "^\\([^:]+\\):\\([0-9]+\\)?:\\(\\([0-9]+\\)\\)?"
   "Parse linenumber and columns.")

;;;###autoload
(defun periphery-helper:project-root-dir ()
  "Get the root directory of the current project."
  (let ((project (project-current)))
    (when project
      (project-root project))))


(defun periphery-helper:filter-xcworkspace (lst)
  "Filter out '.xcworkspace' paths that are inside '.xcodeproj' (as LST)."
  (cl-remove-if (lambda (path)
                  (and (string-match-p "\.xcworkspace$" path)
                       (string-match-p "\.xcodeproj/" path)))
                lst))


(cl-defun async-start-shell-command-to-json (&key command &key callback)
  "Async shell command to JSON run async (as COMMAND CALLBACK)."
  (async-start-command-to-string
   :command command
   :callback (lambda (result)
               (let* ((json-object (json-read-from-string result)))
                 (funcall ,callback json-object)))))

(cl-defun async-start-command-to-string (&key command &key callback)
  "Async shell command to JSON run async (as COMMAND) and parse it json and call (as CALLBACK)."
  (async-start
   `(lambda ()
      (shell-command-to-string ,command))
   `(lambda (result)
      (funcall ,callback result))))

(cl-defun async-start-command (&key command &key callback)
  "Async shell command run async (as COMMAND) and call (as CALLBACK)."
  (async-start
   `(lambda ()
      (shell-command ,command))
   `(lambda (result)
      (funcall ,callback))))

(defun clean-up-newlines (text)
  "Clean up new lines (as TEXT)."
  (string-trim-left
   (replace-regexp-in-string "\n" "" text)))

(cl-defun message-with-color (&key tag &key text &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES."
  (message "%s %s" (propertize tag 'face attributes) text))

;;; Processes
(defun command-string-to-list (cmd)
  "Split the CMD unless it is a list.  This function respects quotes."
  (if (listp cmd) cmd (split-string-and-unquote cmd)))

(cl-defun run-async-command-in-buffer (&key command &key buffername)
  "Run async-command in xcodebuild buffer (as COMMAND and BUFFERNAME)."
  (inhibit-sentinel-messages #'async-shell-command command buffername))

(defun inhibit-sentinel-messages (fun &rest args)
  "Inhibit messages in all sentinels started by FUN and ARGS."
  (cl-letf* ((old-set-process-sentinel (symbol-function 'set-process-sentinel))
         ((symbol-function 'set-process-sentinel)
          (lambda (process sentinel)
        (funcall
         old-set-process-sentinel
         process
         `(lambda (&rest args)
            (cl-letf (((symbol-function 'message) #'ignore))
              (apply (quote ,sentinel) args)))))))
    (apply fun args)))

(defun periphery-helper:filter-keep-beginning-paths (text)
  "Filter lines starting with '/' from TEXT."
  (with-temp-buffer
    (insert text)
    (keep-lines "^/")
    (buffer-string)))

(defun open-current-line-with (data)
  "Open current line with DATA."
  (when data
  (save-match-data
    (and (string-match periphery-parse-line-regex data)
         (when-let ((file (match-string 1 data))
                    (linenumber (string-to-number (match-string 2 data)))
                    (column (match-string 3 data)))
         (with-current-buffer (find-file file)
             (when (> linenumber 0)
               (goto-char (point-min))
               (forward-line (1- linenumber))
               (if-let ((columnnumber (string-to-number column)))
                   (when (> columnnumber 0)
                     (forward-char (1- columnnumber)))))))))))

(cl-defun async-shell-command-to-string (&key process-name &key command &key callback)
  "Execute shell command COMMAND asynchronously in the background.
PROCESS-NAME is the name of the process."
  (let* ((output-buffer (generate-new-buffer process-name))
         (callback-fun callback))
    (set-process-sentinel
     (start-process process-name output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun do-call-process (executable infile destination display args)
  "Wrapper for `call-process'.

EXECUTABLE may be a string or a list.  The string is split by spaces,
then unquoted.
For INFILE, DESTINATION, DISPLAY, see `call-process'.
ARGS are rest arguments, appended to the argument list.
Returns the exit status."
  (let ((command-list
         (append (command-string-to-list executable) args)))
    (apply 'call-process
           (append
            (list (car command-list))
            (list infile destination display)
            (cdr command-list)))))

(defun call-process-to-json (executable &rest args)
  "Call EXECUTABLE synchronously in separate process.

The output is parsed as a JSON document.
EXECUTABLE may be a string or a list.  The string is split by spaces,
then unquoted.
ARGS are rest arguments, appended to the argument list."
  (with-temp-buffer
    (unless (zerop
             (do-call-process executable
                              nil
                              ;; Disregard stderr output, as it
                              ;; corrupts JSON.
                              (list t nil)
                              nil
                              args))
      (error "%s: %s %s %s" "Cannot invoke executable" executable (buffer-string) default-directory))
    (goto-char (point-min))
    (json-read)))
;; periphery-helper:1 ends here

;; [[file:../synthmacs.org::*periphery-swiftlint][periphery-swiftlint:1]]
(defun get-swiftlint-file-root ()
    "Get the path of the swiftlint file."
    (if-let* ((default-directory (periphery-helper:project-root-dir))
              (root (locate-dominating-file default-directory ".swiftlint.yml")))
        root)
    default-directory)

(defun periphery--swiftlint:analyze-result (result)
  "Analyze RESULT."
  (periphery-run-parser result (lambda ()
                                 (message-with-color
                                  :tag "[Success]"
                                  :text "No lint warning or errors."
                                  :attributes 'success))))

(defun periphery-run-swiftlint ()
  "Lint the whole project not just current file."
  (interactive)
  (if (executable-find "swiftlint")
      (progn
        (let ((default-directory (get-swiftlint-file-root)))
          (async-start-command-to-string
           :command "swiftlint"
           :callback '(lambda (result) (periphery--swiftlint:analyze-result result))))
        (message-with-color
         :tag "[Linting|Swiftlint]"
         :text (file-name-nondirectory (directory-file-name (file-name-directory default-directory)))
         :attributes 'success))
    (message-with-color
     :tag "[Failed]"
     :text (format "Install %s to use this command. 'swiftlint'")
     :attributes 'warning)))
;; periphery-swiftlint:1 ends here

;; [[file:../synthmacs.org::*ios-simulator][ios-simulator:1]]
(defgroup ios-simulator nil
  "IOS-SIMULATOR."
  :tag "ios-simulator"
  :group 'ios-simulator)

(defface ios-simulator-background-face
  '((t (:inherit default)))
  "Buffer background color."
  :group 'ios-simulator)

(defconst ios-simulator-buffer-name "*iOS Simulator*"
  "Name of the buffer.")

(defconst list-simulators-command
  "xcrun simctl list devices available -j"
  "List available simulators.")

(defconst get-booted-simulator-command
  "xcrun simctl list devices | grep -m 1 \"(Booted)\" | grep -E -o -i \"([0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12})\""
  "Get booted simulator id if any.")

(defvar current-language-selection "sv-SE")
(defvar current-simulator-name nil)
(defvar current-simulator-id nil)
(defvar-local current-root-folder-simulator nil)
(defvar secondary-simulator-id nil)
(defvar current-app-identifier nil)
(defvar current-app-name nil)
(defvar use-rosetta nil)

(defun ios-simulator:reset ()
  "Reset current settings."
  (setq current-simulator-name nil)
  (setq current-app-name nil)
  (setq current-app-identifier nil)
  (setq current-simulator-id nil))

(defun ios-simulator:current-sdk-version ()
  "Get the current simulator sdk-version."
  (clean-up-newlines (shell-command-to-string "xcrun --sdk iphonesimulator --show-sdk-version")))

(defun ios-simulator:sdk-path ()
  "Get the current simulator sdk-path."
  (clean-up-newlines (shell-command-to-string "xcrun --show-sdk-path --sdk iphonesimulator")))

(defun ios-simulator:current-arch ()
  "Get the current arch."
  (clean-up-newlines (shell-command-to-string "clang -print-target-triple")))

(defun ios-simulator:target ()
  "Get the current simulator sdk."
  (let* ((target-components (split-string (ios-simulator:current-arch) "-"))
         (arch (nth 0 target-components))
         (vendor (nth 1 target-components))
         (version (ios-simulator:current-sdk-version)))
    (format "%s-%s-ios%s-simulator" arch vendor version)))

(cl-defun ios-simulator:install-and-run-app (&key rootfolder &key build-folder &key simulatorId &key appIdentifier)
  "Install app in simulator with ROOTFOLDER BUILD-FOLDER SIMULATORID, APPIDENTIFIER BUFFER."

  (let* ((default-directory rootfolder)
         (simulator-id simulatorId)
         (buffer (get-buffer-create ios-simulator-buffer-name)))

    (setq applicationName (ios-simulator:app-name-from :folder build-folder))
    (setq simulatorName  (ios-simulator:simulator-name))
    (setq simulatorIdentifier simulator-id)
    (setq simulatorBuffer buffer)
    (setq current-app-identifier appIdentifier)
    (setq current-root-folder-simulator rootfolder)

    (ios-simulator:terminate-app-with
     :appIdentifier appIdentifier)

    (ios-simulator:install-app
     :simulatorID simulatorIdentifier
     :build-folder build-folder
     :appname applicationName
     :callback '(lambda ()
                  (inhibit-sentinel-messages #'async-shell-command
                                             (ios-simulator:launch-app
                                              :appIdentifier current-app-identifier
                                              :applicationName applicationName
                                              :simulatorName simulatorName
                                              :simulatorID simulatorIdentifier)
                                             simulatorBuffer)

                  (with-current-buffer simulatorBuffer
                    (setq-local mode-line-format nil)
                    (read-only-mode)
                    (setq-local visual-line-mode t)
                    (setq left-fringe-width 0)
                    (setq right-fringe-width 0)
                    (setq buffer-face-mode-face 'ios-simulator-background-face)
                    (buffer-face-mode 1))))))

(cl-defun ios-simulator:install-app (&key simulatorID &key build-folder &key appname &key callback)
  "Install and launch app (as SIMULATORID and BUILD-FOLDER and CALLBACK)."
  (let* ((folder build-folder)
         (install-path folder)
         (command (format "xcrun simctl install %s '%s%s'.app\n" simulatorID install-path appname)))
    (async-start-command
     :command command
     :callback callback)))

(cl-defun ios-simulator:app-name-from (&key folder)
  "Get compiled app name from (FOLDER)."
  (when-let (binary-name (directory-files (replace-regexp-in-string "\\\\" "" folder) nil "\\.app$"))
    (file-name-sans-extension (car binary-name))))

(defun ios-simulator:kill-buffer ()
  "Kill the ios-simulator buffer."
  (when (get-buffer ios-simulator-buffer-name)
    (kill-buffer ios-simulator-buffer-name)))

(defun ios-simulator:setup-simulator-dwim (id)
  "Setup simulator dwim (as ID)."
  (if (not (ios-simulator:is-simulator-app-running))
      (ios-simulator:start-simulator-with-id id)
    (ios-simulator:boot-simuator-with-id id)))

(defun ios-simulator:simulator-name ()
  "Fetches simulator name."
  (unless current-simulator-name
    (let ((simulator-name (ios-simulator:simulator-name-from :id current-simulator-id)))
      (if simulator-name
          (setq current-simulator-name simulator-name)
        (setq current-simulator-name "Simulator (unknown)"))))
  current-simulator-name)

(defun ios-simulator:boot-simuator-with-id (id)
  "Simulator app is running.  Boot simulator (as ID)."
  (inhibit-sentinel-messages
   #'call-process-shell-command (ios-simulator:boot-command :id id :rosetta use-rosetta)))

(cl-defun ios-simulator:boot-command (&key id &key rosetta)
  "Boot simulator with or without support for x86 (as ID and ROSETTA)."
  (if rosetta
      (format "xcrun simctl boot %s --arch=x86_64" id)
      (format "xcrun simctl boot %s " id)))

(defun ios-simulator:start-simulator-with-id (id)
  "Launch a specific simulator with (as ID)."
  (inhibit-sentinel-messages
   #'call-process-shell-command (format "open --background -a simulator --args -CurrentDeviceUDID %s" id)))

(defun ios-simulator:is-simulator-app-running ()
  "Check if simulator is running."
  (let ((output (shell-command-to-string "ps ax | grep -v grep | grep Simulator.app")))
    (not (string= "" output))))

(cl-defun ios-simulator:simulator-name-from (&key id)
  "Get simulator name (as ID)."
  (clean-up-newlines
   (shell-command-to-string (format "xcrun simctl list devices | grep %s | awk -F \"(\" '{ print $1 }'" id))))

(defun ios-simulator:available-simulators ()
  "List available simulators."
  (let* ((devices (ios-simulator:fetch-available-simulators))
         (items (seq-map
                 (lambda (device)
                   (cons (cdr (assoc 'name device))
                         (cdr (assoc 'udid device)))) devices)))
    items))

(cl-defun ios-simulator:build-language-menu (&key title)
  "Build language menu (as TITLE)."
  (defconst languageList '(
                           ("🏴󠁧󠁢󠁥󠁮󠁧󠁿 English (UK)" "en-UK")
                           ("🇸🇪 Swedish (Sweden)" "sv-SE")
                           ("🇦🇪 Arabic (United Arab Emirates)" "ar-AE")
                           ("🇸🇦 Arabic (Saudi Arabia)" "ar-EG")
                           ("🇫🇷 French (France)" "fr-FR")
                           ("🇳🇱 Dutch (Netherlands)" "nl-NL")
                           ("🇳🇴 Norwegian (Bokmål)" "nb-NO")
                           ("🇯🇵 Japanese (Japan)" "ja-JP")
                           ("🇩🇪 German (Germany)" "de-DE")
                           ("🇪🇸 Spanish (Spain)" "es-ES")
                           ("🇮🇹 Italian (Italy)" "it-IT")
                           ("🇧🇷 Portuguese (Brazil)" "pt-BR")
                           ("🇵🇱 Polish (Poland)" "pl-PL")
                           ("🇨🇳 Chinese (Simplified)" "zh-CN")
                           ("🇷🇺 Russian (Russia)" "ru-RU")
                           ("🇹🇷 Turkish (Turkey)" "tr-TR")
                           ("🇮🇳 Hindi (India)" "hi-IN")
                           ("🇰🇷 Korean (Korea)" "ko-KR")
                           ("🇹🇼 Chinese (Traditional)" "zh-TW")
                           ))
    (progn
    (let* ((choices (seq-map (lambda (item) item) languageList))
           (choice (completing-read title choices)))
      (car (cdr (assoc choice choices))))))

(cl-defun ios-simulator:build-selection-menu (&key title &key list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) item) list))
             (choice (completing-read title choices)))
        (cdr (assoc choice choices))))))

(defun ios-simulator:load-simulator-id ()
  "Get the booted simulator id or fetch a suiting one."
  (if current-simulator-id
      (ios-simulator:setup-simulator-dwim current-simulator-id)
    (progn
      (mode-line-hud:update :message "Fetching simulators")
      (let ((device-id
             (or (ios-simulator:booted-simulator)
                 (ios-simulator:build-selection-menu :title "Choose a simulator:" :list (ios-simulator:available-simulators)))))
        (progn
          (ios-simulator:setup-language)
          (ios-simulator:setup-simulator-dwim current-simulator-id)
          (setq current-simulator-id device-id)))))
  current-simulator-id)

(defun ios-simulator:booted-simulator ()
  "Get booted simulator if any."
  (let ((device-id (shell-command-to-string get-booted-simulator-command)))
    (if (not (string= "" device-id))
        (clean-up-newlines device-id)
      nil)))

(defun ios-simulator:terminate-current-app ()
  "Terminate the current app running in simulator."
  (interactive)
  (if current-app-identifier
      (ios-simulator:terminate-app-with :appIdentifier current-app-identifier)))

(defun ios-simulator:change-language ()
  "Reset current language for simulator."
  (interactive)
  (setq current-language-selection (ios-simulator:build-language-menu :title "Choose simulator language")))

(defun ios-simulator:setup-language ()
  "Setup language if it isnt set."
  (unless current-language-selection
    (setq current-language-selection (ios-simulator:build-language-menu :title "Choose simulator language"))))

(cl-defun ios-simulator:launch-app (&key appIdentifier &key applicationName &key simulatorName &key simulatorID)
  "Command to filter and log the simulator (as APPIDENTIFIER APPLICATIONNAME SIMULATORNAME SIMULATORID)."
  (ios-simulator:setup-language)
  (mode-line-hud:updateWith :message (format "Running %s on %s"
                                             (propertize applicationName 'face 'font-lock-builtin-face)
                                             (propertize simulatorName 'face 'success))
                            :delay 3.0)

  (if-let ((simulatorID simulatorID))
      (format "xcrun simctl launch --console-pty %s %s --terminate-running-process -AppleLanguages \"\(%s\)\"" simulatorID appIdentifier current-language-selection)
    (format "xcrun simctl launch --console-pty booted %s --terminate-running-process -AppleLanguages \"\(%s\)\"" appIdentifier current-language-selection)))

(cl-defun ios-simulator:launch-wait-for-debugger (&key identifier)
  "Launch the current configured simulator and wait for debugger."
  (mode-line-hud:update :message "Debuggin on Simulator")
  (setq current-app-identifier identifier)
  (setq command
        (format "xcrun simctl launch -w --terminate-running-process %s %s -AppleLanguages \"\(%s\)\""
                (ios-simulator:load-simulator-id)
                identifier
                current-language-selection))
  ;; (async-start-command :command command :callback callback))
  (inhibit-sentinel-messages #'call-process-shell-command command))

(defun ios-simulator:run-command-and-get-json (command)
  "Run a shell command and return the JSON output as a string."
  (let* ((json-output (shell-command-to-string command))
         (json-data (json-read-from-string json-output)))
    json-data))

(cl-defun ios-simulator:terminate-app-with (&key appIdentifier)
  "Terminate runnings apps (as APPIDENTIFIER)."
  (setq current-app-identifier appIdentifier)
  (ios-simulator:terminate-app :simulatorID current-simulator-id :appIdentifier appIdentifier)
  (ios-simulator:terminate-app :simulatorID secondary-simulator-id :appIdentifier appIdentifier))

(cl-defun ios-simulator:terminate-app (&key simulatorID &key appIdentifier)
  "Terminate app (as APPIDENTIFIER as SIMULATORID)."
  (inhibit-sentinel-messages #'call-process-shell-command
   (concat
    (if simulatorID
        (format "xcrun simctl terminate %s %s" simulatorID appIdentifier)
      (format "xcrun simctl terminate booted %s" appIdentifier)))))

(defun ios-simulator:appcontainer ()
  "Get the app container of the current app (as SIMULATORID, APPIDENTIFIER)."
  (interactive)
  (if-let ((identifier current-app-identifier)
           (id current-simulator-id)
           (command (shell-command-to-string (format "xcrun simctl get_app_container %s %s data" id identifier))))
      (async-shell-command (concat "open " command))))

(defun ios-simulator:fetch-available-simulators ()
  "List available simulators."
  (let* ((json (call-process-to-json list-simulators-command))
         (devices (cdr (assoc 'devices json)))
         (flattened (apply 'seq-concatenate 'list (seq-map 'cdr devices)))
         (available-devices
          (seq-filter (lambda (device) (cdr (assoc 'isAvailable device))) flattened))
         ) available-devices))
;; ios-simulator:1 ends here

;; [[file:../synthmacs.org::*ios-device][ios-device:1]]
(defconst ios-device:buffer-name "*ios-device-"
  "Name of the buffer.")

(defvar current-buffer-name nil)
(defvar current-device-id nil)
(defvar current-install-command)

(defun ios-device:format-id (id)
  "Format device id (as ID)."
  (if id
      (if (not
           (string-match-p (regexp-quote "-") id))
          (concat (substring id 0 8) "-" (substring id 6))
        id)))

(defun ios-device:id ()
  "Get the id of the connected device."
  (let ((device-id
         (clean-up-newlines
          (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
    (if (= (length device-id) 0)
        (setq current-device-id nil)
      (setq current-device-id (ios-device:format-id device-id))))
  current-device-id)

(cl-defun ios-device:install-app (&key project-root &key buildfolder &key appname)
  "Install an app on device (PROJECT-ROOT BUILDFOLDER APPNAME)."
  (ios-device:kill-buffer)
  (let* ((folder buildfolder)
         (app-name (ios-simulator:app-name-from :folder folder))
         (install-path (concat current-project-root "/" folder))
         (default-directory install-path)
         (command (format "ios-deploy -b %s.app -d" app-name))
         (buffer (get-buffer-create (concat ios-device:buffer-name app-name))))

    (message-with-color :tag "[Installing]" :text (format "%s onto physical device. Will launch app when done." app-name) :attributes 'warning)
    (setq current-buffer-name buffer)
    (inhibit-sentinel-messages #'async-shell-command
                               command
                               buffer)
    (with-current-buffer buffer
      (setq-local mode-line-format nil)
      (setq-local left-fringe-width 0)
      (setq-local right-fringe-width 0))))


(defun ios-device:kill-buffer ()
  "Kill the ios-device buffer."
  (when (and current-buffer-name (get-buffer current-buffer-name))
      (kill-buffer current-buffer-name)))
;; ios-device:1 ends here

;; [[file:../synthmacs.org::*xcode-additions][xcode-additions:1]]
(defgroup xcode-additions:xcodebuild nil
  "REPL."
  :tag "xcode-additions:xcodebuild"
  :group 'xcode-additions)

(defconst xcodeproject-extension ".*\\.xcodeproj$"
  "Xcode project extensions.")

(defconst workspace-extension ".*\\.xcworkspace$"
  "Xcode workspace extensions.")

(defun xcode-additions:filename-by-extension (extension directory)
  "Get filename based on (as EXTENSION)."
  (if-let* ((name (directory-files directory t extension)))
      (file-name-sans-extension (file-name-nondirectory (car name)))))

(defun xcode-additions:project-directory-p (directory)
  "Check if xcodeproj file exists in (DIRECTORY)."
  (consp (directory-files directory nil xcodeproject-extension)))

(defun xcode-additions:workspace-directory-p (directory)
  "Check if xcodeproj file exists in (DIRECTORY)."
  (consp (directory-files directory nil workspace-extension)))

(defun xcode-additions:find-xcode-project-directory (&optional directory)
  "Try to find xcode project in (DIRECTORY)."
  (xcode-additions:find-ancestor-or-self-directory 'xcode-additions:project-directory-p directory))

(defun xcode-additions:find-workspace-directory (&optional directory)
  "Try to find xcode workspace in (DIRECTORY)."
  (xcode-additions:find-ancestor-or-self-directory 'xcode-additions:workspace-directory-p directory))

(defun xcode-additions:find-ancestor-or-self-directory (predicate &optional directory)
  ""
  (unless directory (setq directory default-directory))
  (if (funcall predicate directory)
      directory
    (let ((parent (file-name-directory (directory-file-name directory))))
      (if (or (null parent) (string-equal parent directory))
          nil
        (xcode-additions:find-ancestor-or-self-directory predicate parent)))))

(defun xcode-additions:workspace-name ()
  "Get the workspace name."
  (if-let* ((default-directory (xcode-additions:find-workspace-directory)))
      (xcode-additions:filename-by-extension workspace-extension default-directory)))

(defun xcode-additions:project-name ()
  "Get the workspace name."
  (if-let* ((default-directory (xcode-additions:find-xcode-project-directory)))
      (xcode-additions:filename-by-extension xcodeproject-extension default-directory)))

(defun xcode-additions:list-xcscheme-files (folder)
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of FOLDER."
  (let ((xcscheme-names '()))
    (setq folder (expand-file-name folder))
    (setq xcshemes-folder (concat folder "xcshareddata/xcschemes/"))
    (when (file-directory-p xcshemes-folder)
      (dolist (item (directory-files xcshemes-folder t))
        (when (and (file-regular-p item)
                   (string-match-p ".*\\.xcscheme$" item))
          (setq xcscheme-names (cons (file-name-sans-extension (file-name-nondirectory item)) xcscheme-names)))))
    (if xcscheme-names
        (setq xcscheme-names (nreverse xcscheme-names))
      (message "No '.xcscheme' files found in %s" xcshemes-folder))
    xcscheme-names))

(defun xcode-additions:list-scheme-files ()
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of the current Xcode project or workspace directory."
  (let* ((xcshemes '())
         (project-name (concat (xcode-additions:project-name) ".xcodeproj/"))
         (project-directory (concat (xcode-additions:find-xcode-project-directory) project-name)))
    (cond
     (project-directory
      (let ((xcscheme-files (xcode-additions:list-xcscheme-files project-directory)))
        (if xcscheme-files
            xcscheme-files
          nil))))))
;; xcode-additions:1 ends here

;; [[file:../synthmacs.org::*swift-additions][swift-additions:1]]
(defgroup swift-additions:xcodebuild nil
  "REPL."
  :tag "swift-additions:xcodebuild"
  :group 'swift-additions)

(defconst xcodebuild-list-config-command "xcrun xcodebuild -list -json")

(defvar current-root nil)
(defvar current-xcode-scheme nil)
(defvar current-app-identifier nil)
(defvar current-project-root nil)
(defvar current-build-configuration nil)
(defvar current-build-folder nil)
(defvar current-environment-x86 nil)
(defvar current-simulator-id nil)
(defvar current-simulator-name nil)
(defvar current-buildconfiguration-json-data nil)
(defvar current-local-device-id nil)
(defvar current-build-command nil)
(defvar build-progress-spinner nil)
(defvar current-is-xcode-project nil)
(defvar run-on-device nil)
(defvar run-app-on-build t)
(defvar compilation-time nil)
(defvar DEBUG nil)

(defun swift-additions:fetch-or-load-xcode-scheme ()
  "Get the xcode scheme if set otherwuse prompt user."
  (unless current-xcode-scheme
    (setq current-xcode-scheme (swift-additions:build-menu :title "Choose scheme: " :list (swift-additions:get-scheme-list))))
  current-xcode-scheme)

(defun swift-additions:fetch-or-load-build-configuration ()
  "Get the build configuration or promp user."
  (setq current-build-configuration "Debug")
  ;; (unless current-build-configuration
  ;;   (setq current-build-configuration (swift-additions:build-menu :title "Choose configuration: " :list (swift-additions:get-configuration-list))))
  current-build-configuration)


(defun swift-additions:fetch-or-load-app-identifier ()
  "Get the app identifier for the current configiration."
  (unless current-app-identifier
    (setq current-app-identifier (swift-additions:get-bundle-identifier (swift-additions:fetch-or-load-build-configuration))))
  current-app-identifier)

(defun swift-additions:setup-current-project (project)
  "Check if we have a new project (as PROJECT).  If true reset settings."
  (unless current-project-root
    (setq current-project-root project))
  (when (not (string= current-project-root project))
      (progn
        (swift-additions:reset-settings)
        (setq current-project-root project))))

(defun swift-additions:xcodebuild-command ()
  "Use x86 environement."
  (if current-environment-x86
      "env /usr/bin/arch -x86_64 xcrun xcodebuild build \\"
    "xcrun xcodebuild build \\"))

(defun swift-additions:get-build-folder ()
  "Get build folder. If there are more than one let the user choose wich one to use."
  (unless current-build-folder
    (setq current-build-folder
    (if-let* ((default-directory (concat (swift-additions:get-ios-project-root) "/build/Build/Products/"))
              (choosen-folder (swift-additions:build-menu :title "Choose build folder" :list (swift-additions:parse-build-folder default-directory))))
        (shell-quote-argument (concat default-directory choosen-folder "/")))))
  current-build-folder)

(defun swift-additions:parse-build-folder (directory)
  "Parse build folders from (as DIRECTORY)."
  (let ((folders (directory-files directory nil "^[^.].*" t)))
    (mapc (lambda (folder)
            (when (file-directory-p folder)
                 (expand-file-name folder directory)))
          folders)))

(defun swift-additions:get-number-of-cores ()
  "Fetch number of available cores."
  (if-let ((cores (replace-regexp-in-string "\n$" "" (shell-command-to-string "sysctl -n hw.ncpu"))))
      cores
    2))

(defun swift-additions:get-workspace-or-project ()
  "Check if there is workspace or project."
  (let ((workspace (xcode-additions:workspace-name))
        (projectname (xcode-additions:project-name)))
    (if workspace
        (format "-workspace %s.xcworkspace" workspace)
      (format "-project %s.xcodeproj" projectname))))

(cl-defun build-app-command (&key sim-id)
  "Xcodebuild with (as SIM-ID)."
  (if current-build-command
      current-build-command
    (concat
     (swift-additions:xcodebuild-command)
     (format "%s \\" (swift-additions:get-workspace-or-project))
     (format "-scheme '%s' \\" (swift-additions:fetch-or-load-xcode-scheme))
     (format "-sdk %s \\" (swift-additions:get-current-sdk))
     ;; (format "-jobs %s" (swift-additions:get-number-of-cores))
     (when sim-id
       (format "-destination 'generic/platform=iOS Simulator,id=%s' \\" sim-id))
     (when (and current-local-device-id run-on-device)
       (format "-destination 'generic/platform=iOS' \\" ))
     "-hideShellScriptEnvironment \\"
     "-derivedDataPath build | xcode-build-server parse -avv")))
;; (format "BUILD_DIR=%s "  (swift-additions:get-build-folder))

(cl-defun swift-additions:build-device-or-simulator-menu (&key title)
  "Build device or simulator menu (as TITLE)."
  (defconst deviceList '(("Simulator" nil)
                         ("Physical device" t)))
  (progn
    (let* ((choices (seq-map (lambda (item) item) deviceList))
           (choice (completing-read title choices)))
      (car (cdr (assoc choice choices))))))

(defun swift-addition:ask-for-device-or-simulator ()
  "Show menu for runnings on simulator or device."
  (interactive)
  (when current-local-device-id
    (setq run-on-device (swift-additions:build-device-or-simulator-menu :title "Run on simulator or device?"))))

(defun swift-additions:compilation-time ()
  "Get the time of the compilation."
  (if-let ((end-time (current-time)))
      (format "%.1f" (float-time (time-subtract end-time compilation-time)))
    nil))

(defun swift-additions:run-app()
  "Either in simulator or on physical."
  (mode-line-hud:update :message (format "Built %s in %s seconds"
                                         (propertize current-xcode-scheme 'face 'font-lock-builtin-face)
                                         (propertize (swift-additions:compilation-time) 'face 'warning)))

  (ios-simulator:install-and-run-app
   :rootfolder current-project-root
   :build-folder (swift-additions:get-build-folder)
   :simulatorId (ios-simulator:load-simulator-id)
   :appIdentifier (swift-additions:fetch-or-load-app-identifier)))

(defun swift-additions:check-if-build-was-successful (input-text)
  "Check if INPUT-TEXT does not contain 'BUILD FAILED' or 'BUILD INTERRUPTED' from the end."
  (and (not (string-match-p "BUILD FAILED" input-text))
       (not (string-match-p "BUILD INTERRUPTED" input-text))))

(defun swift-additions:check-for-errors (output callback)
  "Run periphery parser on TEXT (optional as OUTPUT CALLBACK)."
  (when (swift-additions:check-if-build-was-successful output)
    (funcall callback))
  (periphery-run-parser output))

(defun swift-additions:get-project-files ()
  "Get project files."
  (let* ((root-dir (periphery-helper:project-root-dir))
         (files (directory-files-recursively root-dir "\.xcworkspace$\\|\\.xcodeproj$" t))
         (file-list (cdr-safe files))
         (filtered-list (cl-remove-if (lambda (file)
                                        (string-match-p "/\.build/" file))
                                      file-list)))
    (periphery-helper:filter-xcworkspace filtered-list)))

(defun swift-additions:get-ios-project-root ()
  "Get the ios-project root."
  (unless current-project-root
    (setq current-project-root (cdr (project-current))))
  current-project-root)

(defun swift-additions:get-current-sdk ()
  "Return the current SDK."
  (if current-local-device-id
      "iphoneos"
    "iphonesimulator"))

;;;###autoload
(defun swift-additions:reset-settings ()
  "Reset current settings.  Change current configuration."
  (interactive)
  (ios-simulator:kill-buffer)
  (periphery-kill-buffer)
  (setq current-xcode-scheme nil)
  (setq current-app-identifier nil)
  (setq current-app-name nil)
  (setq current-project-root nil)
  (setq current-build-configuration nil)
  (setq current-simulator-id nil)
  (setq current-buildconfiguration-json-data nil)
  (setq current-local-device-id nil)
  (setq current-build-command nil)
  (setq current-build-folder nil)
  (mode-line-hud:update :message "Resetting configuration"))

(defun swift-additions:successful-build ()
  "Show that the build was successful."
  (message-with-color :tag "[Build]" :text "Successful" :attributes 'success))

;;;###autoload
(defun swift-additions:run-without-compiling ()
  "Run app in simulator/device without compiling."
  (interactive)
  (periphery-kill-buffer)
  (ios-simulator:kill-buffer)
  (swift-additions:run-app))

;;;###autoload
(defun swift-additions:compile-and-run-app ()
  "Compile and run app."
  (interactive)
  (swift-additions:compile-and-run-silent t))

;;;###autoload
(defun swift-additions:compile-app ()
  "Compile app."
  (interactive)
  (swift-additions:compile-and-run-silent nil))

(defun swift-additions:compile-and-run-silent (runApp)
  "Build project using xcodebuild (as RUNAPP)."
   (let ((savings (save-some-buffers t))
         (buffer (periphery-kill-buffer)))
     (swift-additions:check-root)
     (ios-simulator:kill-buffer)
     (setq current-local-device-id (ios-device:id))
     (swift-addition:ask-for-device-or-simulator)

  (if (swift-additions:is-xcodeproject)
      (progn
        (setq run-app-on-build runApp)
        (if run-on-device
            (progn
              (setq device-or-simulator "physical device")
              (swift-additions:compile-and-run-on-device))
          ;; Simulator
          (swift-additions:compile-app-for-simulator :run runApp)))

    (if (swift-additions:is-a-swift-package-base-project)
        (swift-additions:build-swift-package)
      (message "Not xcodeproject nor swift package")))))

(cl-defun swift-additions:compile-app-for-simulator (&key run)
  "Compile app (RUN)."

  (swift-additions:setup-current-project (swift-additions:get-ios-project-root))
  (ios-simulator:load-simulator-id)
  (setq current-simulator-name (ios-simulator:simulator-name))

  (let ((default-directory current-project-root)
        (build-command (build-app-command :sim-id current-simulator-id))
        (run-app-on-build run))

    (setq current-build-command build-command)
    (setq compilation-time (current-time))

    (xcodebuildserver:check-configuration :root current-project-root
                                          :workspace (swift-additions:get-workspace-or-project)
                                          :scheme current-xcode-scheme)
    (when DEBUG
      (message build-command))

    (spinner-start 'progress-bar-filled)
    (setq build-progress-spinner spinner-current)

    (mode-line-hud:update :message (format "Compiling %s/%s"
                                           (propertize current-xcode-scheme 'face 'font-lock-builtin-face)
                                           (propertize current-simulator-name 'face 'font-lock-negation-char-face)))

    (async-start-command-to-string
     :command build-command
     :callback '(lambda (text)
                  (spinner-stop build-progress-spinner)
                  (if run-app-on-build
                      (swift-additions:check-for-errors text #'swift-additions:run-app)
                    (swift-additions:check-for-errors text #'swift-additions:successful-build))))))

(defun swift-additions:compile-and-run-on-device ()
  "Compile and run on device."
  (swift-additions:setup-current-project (swift-additions:get-ios-project-root))

  (message-with-color
   :tag "[Preparing]"
   :text "Fetching build information..."
   :attributes '(:inherit warning))

  (let ((default-directory current-project-root)
        (build-command (build-app-command :sim-id nil))
        (build-folder (swift-additions:get-build-folder)))
    (setq current-build-command build-command)
    (setq current-build-folder build-folder)

    (when DEBUG
      (message current-build-command)
      (message "Build-folder: %s" current-build-folder))

    (async-start-command-to-string
     :command build-command
     :callback '(lambda (text)
                  (if run-app-on-build
                      (ios-device:install-app
                       :project-root current-project-root
                       :buildfolder current-build-folder
                       :appname (ios-simulator:app-name-from :folder current-build-folder)))
                    (swift-additions:check-for-errors text #'swift-additions:successful-build)))))

;;;###autoload
(defun swift-additions:test-module-silent ()
  "Test module."
  (interactive)
  (save-some-buffers t)
  (periphery-kill-buffer)
  (ios-simulator:kill-buffer)
  (swift-additions:test-swift-package))

;;;###autoload
(defun swift-additions:clean-build-folder ()
  "Clean app build folder."
  (interactive)
  (swift-additions:clean-build-folder-with (periphery-helper:project-root-dir) ".build" "swift package")
  (swift-additions:clean-build-folder-with (swift-additions:get-ios-project-root) "/build" (swift-additions:fetch-or-load-xcode-scheme)))

(defun swift-additions:clean-build-folder-with (projectRoot buildFolder projectName)
  "Clean build folder with PROJECTROOT BUILDFOLDER and PROJECTNAME."

  (mode-line-hud:update
   :message (format "Cleaning build folder for %s"
                    (propertize projectName 'face 'warning)))

  (let ((default-directory (concat projectRoot buildFolder)))
    (when (file-directory-p default-directory)
      (delete-directory default-directory t nil)))

  (mode-line-hud:update
   :message (format "Cleaning done for %s"
                    (propertize projectName 'face 'warning))))

(defun swift-additions:check-root ()
  "Check root of the project.  If its different reset the settings."
  (when (not (string-equal current-root (cdr (project-current))))
    (swift-additions:reset-settings)
    (setq current-root (cdr (project-current)))))

(defun swift-additions:insert-text-and-go-to-eol (text)
  "Function that that insert (as TEXT) and go to end of line."
  (save-excursion
    (indent-for-tab-command)
    (insert text)
    (move-end-of-line nil))
  (goto-char (point-at-eol))
  (evil-insert-state t))

;;;###autoload
(defun swift-additions:functions-and-pragmas ()
  "Show swift file compressed functions and pragmas."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "\\(#pragma mark\\)\\|\\(MARK:\\)")))

;;;###autoload
(defun swift-additions:print-thing-at-point ()
  "Print thing at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (end-of-line)
    (newline-and-indent)
    (insert (format "debugPrint(\"%s: \ \\(%s\)\")" word word))))

;;;###autoload
(defun swift-additions:insert-mark ()
  "Insert a mark at line."
  (interactive)
  (swift-additions:insert-text-and-go-to-eol "// MARK: - "))

;;;###autoload
(defun swift-additions:insert-todo ()
  "Insert a Todo."
  (interactive)
  (swift-additions:insert-text-and-go-to-eol "// TODO: "))

(defun swift-additions:get-bundle-identifier (config)
  "Get bundle identifier (as CONFIG)."
  (unless current-project-root
    (setq current-project-root (swift-additions:get-ios-project-root)))
  
  (let ((default-directory current-project-root)
        (json (call-process-to-json "xcrun" "xcodebuild" "-showBuildSettings" "-configuration" config "-json")))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_BUNDLE_IDENTIFIER)))

(defun swift-additions:get-buildconfiguration-json ()
  "Return a cached version or load the build configuration."
  (unless current-buildconfiguration-json-data
    (mode-line-hud:update :message "Fetching build configuration")
    (setq current-buildconfiguration-json-data (call-process-to-json xcodebuild-list-config-command)))
  current-buildconfiguration-json-data)

(defun swift-additions:get-target-list ()
  "Get list of project targets."
  (unless current-project-root
    (mode-line-hud:update :message "Fetching app targets")
    (setq current-project-root (swift-additions:get-ios-project-root)))

  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (targets (cdr (assoc 'targets project))))
    targets))

(defun swift-additions:get-scheme-list ()
  "Get list of project schemes."
  (unless current-project-root
    (mode-line-hud:update :message "Fetching build schemes")
    (setq current-project-root (swift-additions:get-ios-project-root)))
  (xcode-additions:list-scheme-files))

(defun swift-additions:get-configuration-list ()
  "Get list of project configurations."
  (unless current-project-root
    (mode-line-hud:update :message "Fetching build configurations")
    (setq current-project-root (swift-additions:get-ios-project-root)))
  
  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (result (cdr (assoc 'configurations project))))
    result))

(cl-defun swift-additions:build-menu (&key title &key list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) (cons item item)) list))
             (choice (completing-read title choices)))
        (cdr (assoc choice choices))))))

(defun swift-additions:is-xcodeproject ()
  "Check if its an xcode-project."
  (unless current-is-xcode-project
    (if-let ((default-directory (swift-additions:get-ios-project-root)))
        (setq current-is-xcode-project
        (or
         (directory-files-recursively default-directory "\\xcworkspace$" t)
         (directory-files-recursively default-directory "\\xcodeproj$" t)))))
    current-is-xcode-project)

(defun swift-additions:is-a-swift-package-base-project ()
  "Check if project is a swift package based."
  (let ((default-directory (periphery-helper:project-root-dir)))
    (file-exists-p "Package.swift")))

(defun swift-additions:check-for-spm-build-errors (text)
  "Check for Swift package build erros in TEXT."
  (when DEBUG (message text))
  (if (or
       (string-match-p (regexp-quote "error:") text)
       (string-match-p (regexp-quote "warning:") text))
      (progn
        ;; (periphery-run-parser text)
        (when (not (string-match-p (regexp-quote "error:") text))
          (swift-additions:run-async-swift-package)))
    (swift-additions:run-async-swift-package)))

(defun swift-additions:run-async-swift-package ()
  "Run async swift package and hide the normal output."
  (inhibit-sentinel-messages #'async-shell-command
                             "swift run"
                             "*Swift Package*"))

;;;###autoload
(defun swift-additions:build-swift-package ()
  "Build swift package module."
  (interactive)
  (let ((default-directory (periphery-helper:project-root-dir)))
    (swift-additions:reset-settings)
    (async-shell-command-to-string :process-name "periphery" :command "swift build" :callback #'swift-additions:check-for-spm-build-errors)
    (message-with-color :tag "[ Package]" :text (format "%s. Please wait. Patience is a virtue!" (periphery-helper:project-root-dir)) :attributes 'warning)))

;;;###autoload
(defun swift-additions:test-swift-package ()
  "Test swift package module."
  (interactive)
  (swift-additions:test-swift-package :root (periphery-helper:project-root-dir)))

;;;###autoload
(defun swift-additions:test-swift-package-from-file ()
  "Test swift package module."
  (interactive)
  (swift-additions:test-swift-package :root (swift-additions:detect-package-root)))

(cl-defun swift-additions:test-swift-package (&key root)
  "Test package in ROOT."
  (let ((default-directory root)
        (package-name (file-name-nondirectory (directory-file-name root))))
    (spinner-start 'progress-bar-filled)
    (setq build-progress-spinner spinner-current)
    (async-start-command-to-string
     :command "swift test"
     :callback '(lambda (text)
                  (spinner-stop build-progress-spinner)
                  (let ((filtered (periphery-helper:filter-keep-beginning-paths text)))
                    (periphery-run-test-parser filtered (lambda ()
                                                          (message-with-color
                                                           :tag "[All tests passed]"
                                                           :text ""
                                                           :attributes 'success))))))
    (message-with-color
     :tag (format "[Testing '%s'-package]" package-name)
     :text "Please wait. Patience is a virtue!"
     :attributes 'warning)))

(defun swift-additions:detect-package-root ()
  "Detects the root directory of the Swift package based on the current buffer."
  (let ((buffer-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (locate-dominating-file buffer-dir "Package.swift")))

(defun swift-additions:lsp-arguments ()
  "Get the lsp arguments to support UIKit."
  (let* ((sdk (ios-simulator:sdk-path))
         (target (ios-simulator:target)))
    (list
     "--completion-max-results" "12"
     "-Xswiftc" "-sdk"
     "-Xswiftc" sdk
     "-Xswiftc" "-target"
     "-Xswiftc" target)))

(defun my-swift-mode:eglot-server-contact (_ignored)
  "Construct the list that eglot needs to start sourcekit-lsp."
  (setq arglist (swift-additions:lsp-arguments))
  (add-to-list 'arglist (clean-up-newlines (shell-command-to-string "xcrun --find sourcekit-lsp"))))

(require 'tree-sitter-hl)

(defface tree-sitter-hl-face:case-pattern
  '((t :inherit tree-sitter-hl-face:property))
  "Face for enum case names in a pattern match"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment.special
  '((t :inherit tree-sitter-hl-face:comment
       :weight semi-bold))
  "Face for comments with some markup-like meaning, like MARK"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:operator.special
  '((t :inherit font-lock-negation-char-face
       :weight semi-bold))
  "Face for operators that need to stand out, like unary negation"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:punctuation.type
  '((t :inherit tree-sitter-hl-face:type
       :weight normal))
  "Face for punctuation in type names or annotations"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:annotation
  '((t :inherit font-lock-keyword-face))
  "Face for annotations or attributes attached to declarations."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:annotation.builtin
  '((t :inherit tree-sitter-hl-face:annotation))
  "Face for declaration annotations which are built in to the language."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:annotation.type
  '((t :inherit tree-sitter-hl-face:annotation))
  "Face for annotations attached to type descriptors."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.annotation
  '((t :inherit tree-sitter-hl-face:annotation.builtin))
  "Face for subelements of annotations which are built in to the language."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.compiler
  '((t :inherit tree-sitter-hl-face:keyword
       :weight semi-bold))
  "Face for compile-time keywords"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.type
  '((t :inherit tree-sitter-hl-face:keyword))
  "Face for keywords that appear in type annotations"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable.synthesized
  '((t :inherit tree-sitter-hl-face:variable))
  "Face for compiler-synthesized identifiers"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:default
  '((t :inherit default))
  "Face to override other faces, forcing the base display
attributes."
  :group 'tree-sitter-hl-faces)
;; swift-additions:1 ends here

;; [[file:../synthmacs.org::*YAML][YAML:1]]
(use-package yaml-mode
  :mode ((rx ".yml" eos) . yaml-mode))
;; YAML:1 ends here

;; [[file:../synthmacs.org::*synthmacs-programming-languages][synthmacs-programming-languages:1]]
(provide 'synthmacs-programming-languages)
;;; synthmacs-programming-languages.el ends here
;; synthmacs-programming-languages:1 ends here
