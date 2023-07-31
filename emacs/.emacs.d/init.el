;; [[file:synthmacs.org::*Header & Lexical Binding][Header & Lexical Binding:1]]
;;; init.el --- Synthmacs Configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023 Roux G. Buciu

;; Author: roux g. buciu <roux@fringe.foundation>
;; Keywords: internal
;; URL: https://fringe.foundation

;;; Commentary:
;; A mostly minimal, reproducible Emacs configuration.  This file is
;; automatically tangled from synthmacs.org, with header/footer comments on each
;; code block that allow for de-tangling the source back to synthmacs.org when
;; working on this file directly.

;;; Code:
;; Header & Lexical Binding:1 ends here

;; [[file:synthmacs.org::*Improve I/O][Improve I/O:1]]
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Ensure Synthmacs is running out of this file's directory
(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))
;; Improve I/O:1 ends here

;; [[file:synthmacs.org::*init.el load modules][init.el load modules:1]]
(message "SynthMacs is powering up, please be patient...")

;; (add-to-list 'load-path "~/.emacs.d/synthmacs/")
(add-to-list 'load-path (expand-file-name "synthmacs" user-emacs-directory))

(let ((file-name-handler-alist nil)
      (gc-cons-threshold 100000000))

  (require 'synthmacs-core)
  (require 'synthmacs-package-management)
  (require 'synthmacs-general-evil)
  (require 'synthmacs-ui)
  (require 'synthmacs-org)
  (require 'synthmacs-completion-framework)
  (require 'synthmacs-tools)
  (require 'synthmacs-programming)

  ;; programming languages
  (require 'synthmacs-lang-cpp)
  (require 'synthmacs-lang-clojure)
  (require 'synthmacs-lang-css)
  (require 'synthmacs-lang-gitModes)
  (require 'synthmacs-lang-haskell)
  (require 'synthmacs-lang-html)
  (require 'synthmacs-lang-lisp)
  (require 'synthmacs-lang-clisp)
  (require 'synthmacs-lang-elisp)
  (require 'synthmacs-lang-lua)
  (require 'synthmacs-lang-markdown)
  (require 'synthmacs-lang-racket)
  (require 'synthmacs-lang-rust)
  (require 'synthmacs-lang-swift)
  (require 'synthmacs-lang-yaml)
  )
;; init.el load modules:1 ends here

;; [[file:synthmacs.org::*init.el][init.el:1]]
;;; init.el ends here
;; init.el:1 ends here
