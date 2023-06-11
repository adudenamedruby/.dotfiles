;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;  ____
;; / ___| _ __   __ _  ___ ___ _ __ ___   __ _  ___ ___
;; \___ \| '_ \ / _` |/ __/ _ \ '_ ` _ \ / _` |/ __/ __|
;;  ___) | |_) | (_| | (_|  __/ | | | | | (_| | (__\__ \
;; |____/| .__/ \__,_|\___\___|_| |_| |_|\__,_|\___|___/
;;       |_|
;;
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; This file are the personal preferences of adudenamedruby
;; Copying it may be weird for you.

;; If something has gone terribly wrong and a complete reset of Spacemacs
;; is needed, run the following command: rm -fr ~/.emacs.d ~/.spacemacs

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(lua
     ;; Add tool tips to show doc string of functions
     ;; Show snippets in the auto-completion popup
     ;; Show suggestions by most commonly used
     ;; To have auto-completion on as soon as you start typing
     ;; (auto-completion :variables auto-completion-idle-delay nil)
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t)

     c-c++

     ;; Nyan cat tells you where you are in your file
     ;; (colors :variables
     ;;         colors-enable-nyan-cat-progress-bar (display-graphic-p))
     colors

     clojure
     common-lisp

     ;; SPC a t l displays key and command history in a separate buffer
     command-log
     csv

     emoji
     emacs-lisp
     evil-commentary

     ;; SPC g s opens Magit git client full screen (q restores previous layout)
     ;; refine hunk 'all highlights characters changed on each line
     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk 'all
          magit-refs-show-commit-count 'all)

     (go :variables
         go-tab-width 4
         gofmt-command "goimports"
         go-backend 'lsp)

     ;; helm-follow-mode sticky - remembers use of C-c C-f
     ;; - follow mode previews when scrolling through a helm list
     ;; (setq helm-follow-mode-persistent t)
     helm

     helpful
     html

     imenu-list

     (json :variables
           json-fmt-tool 'prettier)

     (lsp :variables
          ;; Commented variables are default values in the lsp layer configuration

          ;; Formatting and indentation
          ;; lsp-enable-on-type-formatting nil
          ;; Set to nil to use CIDER features instead of LSP UI
          ;; lsp-enable-indentation nil
          ;; lsp-enable-completion-at-point nil

          ;; Buffer visual elements
          ;; shows directory path at top of buffer
          ;; lsp-headerline-breadcrumb-enable nil

          ;; symbol highlighting - `lsp-toggle-symbol-highlight` toggles highlighting
          ;; change made to doom-gruvbox-light theme for subtle highlighting
          ;; lsp-enable-symbol-highlighting t

          ;; Show lint errors in the mode-bar (tested on doom-modeline)
          ;; lsp-modeline--enable-diagnostics t

          ;; popup documentation boxes
          ;; lsp-ui-doc-enable nil          ;; disable doc popups
          ;; lsp-ui-doc-show-with-cursor nil
          ;; lsp-ui-doc-show-with-mouse t
          ;; lsp-ui-doc-delay 2

          ;; show code actions and diagnostics text as right-hand side of buffer
          ;; lsp-ui-sideline-enable nil
          ;; Is this just display or disabling the actions
          ;; lsp-modeline-code-actions-enable nil
          ;; lsp-ui-sideline-show-diagnostics nil
          ;; When non-nil, the symbol information overlay includes symbol name (redundant for c-modes).
          ;; lsp-ui-sideline-show-symbol    nil

          ;; show reference count for functions (assume their maybe other lenses in future)
          ;; lsp-lens-enable t

          ;; Symbol highlighting
          ;; lsp-enable-symbol-highlighting nil
          ;; Do not highlight current symbol, only other matches ??  Avoids visual clashing with visual select
          ;; lsp-symbol-highlighting-skip-current nil

          ;; add a delay to all lsp features, how much does this affect ?
          ;; lsp-idle-delay 2

          ;; Efficient use of space in treemacs-lsp display
          treemacs-space-between-root-nodes nil

          ;; lsp-file-watch-threshold 10000
          )

     markdown

     neotree

     ;; Spacemacs Org Mode
     (org :variables
          org-enable-epub-support t
          org-enable-github-support t
          org-directory (expand-file-name "~/CodexSeraphinianus/ExoCortex/org")
          ;; org-roam
          org-enable-roam-support t
          org-enable-roam-ui t
          org-roam-directory (expand-file-name "~/CodexSeraphinianus/ExoCortex/myWiki/zettlekasten")
          org-roam-db-location (expand-file-name "~/CodexSeraphinianus/ExoCortex/myWiki/db/org-roam.db")
          org-roam-mode-section-functions
           (list #'org-roam-backlinks-section
                 #'org-roam-reflinks-section
                 #'org-roam-unlinked-references-section)
          org-roam-capture-templates
           '(;; EXAMPLE TEMPLATE
             ;; hotkey name type
             ("n" "Note" plain
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
             )

      )

     pdf
     prolog
     python

     (rust :variables
           rust-backend 'lsp)

     ;; SPC ' runs a shell in a popup buffer
     (shell :variables
            shell-default-height 65
            shell-default-position 'top
            shell-default-shell 'vterm)

     shell-scripts
     ;; Modeline of fancyness
     (spacemacs-modeline :variables
                         doom-modeline-height 40
                         doom-modeline-buffer-file-name-style 'file-name
                         doom-modeline-lsp t
                         doom-modeline-vcs-max-length 75
                         doom-modeline-icon t
                         doom-modeline-major-mode-color-icon t
                         doom-modeline-minor-modes nil
                         doom-modeline-major-mode-color-icon t
                         doom-modeline-buffer-state-icon t
                         )

     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     sql
     syntax-checking
     swift

     themes-megapack
     theming

     ;; Support font ligatures (fancy symbols) in all modes
     ;; 'prog-mode for only programming languages
     ;; including text-mode may cause issues with org-mode and magit
     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t
                    unicode-fonts-ligature-modes '(prog-mode))

     ;; Highlight changes in buffers
     ;; SPC g . transient state for navigating changes
     (version-control :variables
                      version-control-diff-tool 'git-gutter+
                      version-control-diff-side 'left
                      version-control-global-margin t)

     (yaml :variables
           yaml-enable-lsp t)

     ;; end of configure-layers
     )


     ;; List of additional packages that will be installed without being
     ;; wrapped in a layer. If you need some configuration for these
     ;; packages, then consider creating a layer. You can also put the
     ;; configuration in `dotspacemacs/user-config'.
     ;; To use a local version of a package, use the `:location' property:
     ;; '(your-package :location "~/path/to/your-package/")
     ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(lsp-sourcekit
                                      all-the-icons
                                      olivetti
                                      org-reverse-datetree)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents-by-project . (4 . 5)))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(alect-black-alt
                         lush
                         grandshell
                         rebecca
                         kaolin-bubblegum
                         kaolin-galaxy
                         kaolin-valley-dark
                         doom-old-hope
                         gruvbox-dark-hard
                         dracula
                         doom-zenburn
                         darkmine
                         sanityinc-tomorrow-night
                         cyberpunk
                         afternoon)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.3)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   pdf-view-mode)

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "rg" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
)

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq-default

   ;; Edit the real file rather than the symlinks
   vc-follow-symlinks t
   indent-tabs-mode nil

   ;; Evil shift round indents to the nearest value of 4
   evil-shift-round t

   ;; Avy jumps should work with all frames
   avy-all-windows 'all-frames
   ))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Cursor behavirous
  ;; (global-centered-cursor-mode 1)

  ;; Activate column indicator in prog-mode and text-mode
  (setq display-fill-column-indicator-column 85)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

  ;; Visual line navigation for textual modes
  (spacemacs/toggle-visual-line-navigation-on)
  (setq truncate-lines t)
  (spacemacs/toggle-truncate-lines-on)
  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
  (add-hook 'prog-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Array magic!
  (defun arrayify (start end quote)
    "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
    (interactive "r\nMQuote: ")
    (let ((insertion
           (mapconcat
            (lambda (x) (format "%s%s%s" quote x quote))
            (split-string (buffer-substring start end)) ", ")))
      (delete-region start end)
      (insert insertion)))
  (spacemacs/set-leader-keys "oa" 'arrayify)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clojure

  ;; clojure - pretty symbols
  (setq clojure-enable-fancify-symbols t)
  ;; set pretty symbols everywhere, really
  (global-prettify-symbols-mode t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Custom Keybindings

  ;; Easily call up describe-function
  (spacemacs/set-leader-keys "hdf" 'describe-function)
  (spacemacs/set-leader-keys "aoP" 'org-present)
  (spacemacs/set-leader-keys "ot" 'open-tasks-file)
  (spacemacs/set-leader-keys "on" 'open-notes-file)
  (spacemacs/set-leader-keys "om" 'open-media-file)

  (defun open-tasks-file ()
    (interactive)(find-file "~/CodexSeraphinianus/ExoCortex/org/tasks.org"))

  (defun open-notes-file ()
    (interactive)(find-file "~/CodexSeraphinianus/ExoCortex/org/notes.org"))

  (defun open-media-file ()
    (interactive)(find-file "~/CodexSeraphinianus/ExoCortex/org/media.org"))

  ;; Evil-mode stuff
  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helm - keep history clean
  (setq history-delete-duplicates t)
  (setq extended-command-history
        (delq nil (delete-dups extended-command-history)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Magit - forge configuration
  ;;
  ;; Set the files that are searched for writing tokens
  ;; by default ~/.authinfo will be used
  ;; and write a token in unencrypted format
  ;; (setq auth-sources '("~/.authinfo"))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; LSP
  ;; Swift LSP configuration
  (eval-after-load 'lsp-mode
    (progn
      (require 'lsp-sourcekit)
      (setq lsp-sourcekit-executable
            (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp")))))

  (add-hook 'swift-mode-hook (lambda () (lsp)))

  (defun xcode-build()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))
  (defun xcode-run()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))
  (defun xcode-test()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'"))
  (defun xcode-clean()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'clean targetProject' -e 'end tell'"))
  (defun xcode-open-current-file()
    (interactive)
    (shell-command-to-string
     (concat "open -a \"/Applications/Xcode.app\" " (buffer-file-name))))
  (spacemacs/declare-prefix "c x" "xcode")
  (spacemacs/set-leader-keys "cxr" 'xcode-run)
  (spacemacs/set-leader-keys "cxb" 'xcode-build)
  (spacemacs/set-leader-keys "cxt" 'xcode-test)
  (spacemacs/set-leader-keys "cxc" 'xcode-clean)
  (spacemacs/set-leader-keys "cxf" 'xcode-open-current-file)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Olivetti mode
  ;; Text takes up xx% of the buffer
  (setq olivetti-body-width 0.75)
  ;; Starts text files (like .org .txt .md) in olivetti mode
  (add-hook 'text-mode-hook 'olivetti-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org mode setup
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (python . t)
       (haskell . t)
       (clojure . t)
       (ruby . t)
       (lisp . t))))

  (setq org-ellipsis "…")
  (setq org-pretty-entities t)
  (setq org-startup-with-inline-images t
        org-image-actual-width '(600))
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "FIXME(f)" "REMINDER(m)" "UNDERWAY(u)" "BLOCKED(b)" "REVIEW(r)" "|" "CANCELLED(c)" "DONE(d)")))
  ;; Skip finished items
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  ;; To add all org files in a repository to the agenda
  (setq org-agenda-files (directory-files-recursively "~/code/git/ExoCortex/org" "org$"))
  ;; Skip deleted files
  (setq org-agenda-skip-unavailable-files t)
  ;; Store my org files in ~/documents/org, define the location of an index file (my main todo list),
  ;; and archive finished tasks in ~/.../archive/archive-YYYY.org
  (defun org-file-path (filename)
    "Return the absolute address of an org file, given its relative name."
    (concat (file-name-as-directory org-directory) filename))

  (setq org-index-file (org-file-path "tasks.org"))
  (setq org-archive-location
        (concat
         (org-file-path (format "archive/archive-%s.org" (format-time-string "%Y")))
         "::* From %s"))

  (setq org-refile-targets `((,org-index-file :level . 1)
                             (,(org-file-path "deliveries.org") :level . 1)))
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
  (setq org-confirm-babel-evaluate nil)

  ;; Separate org blocks with nearly complete lines, not rows of ===.
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")

  ;; Hide blocks in the agenda that don’t contain any tasks.
  ;; (defun org-agenda-delete-empty-blocks ()
  ;;   "Remove empty agenda blocks.
  ;; A block is identified as empty if there are fewer than 2
  ;; non-empty lines in the block (excluding the line with
  ;; `org-agenda-block-separator' characters)."
  ;;   (when org-agenda-compact-blocks
  ;;     (user-error "Cannot delete empty compact blocks"))
  ;;   (setq buffer-read-only nil)
  ;;   (save-excursion
  ;;     (goto-char (point-min))
  ;;     (let* ((blank-line-re "^\\s-*$")
  ;;           (content-line-count (if (looking-at-p blank-line-re) 0 1))
  ;;           (start-pos (point))
  ;;           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
  ;;       (while (and (not (eobp)) (forward-line))
  ;;         (cond
  ;;         ((looking-at-p block-re)
  ;;           (when (< content-line-count 2)
  ;;             (delete-region start-pos (1+ (point-at-bol))))
  ;;           (setq start-pos (point))
  ;;           (forward-line)
  ;;           (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
  ;;         ((not (looking-at-p blank-line-re))
  ;;           (setq content-line-count (1+ content-line-count)))))
  ;;       (when (< content-line-count 2)
  ;;         (delete-region start-pos (point-max)))
  ;;       (goto-char (point-min))
  ;;       ;; The above strategy can leave a separator line at the beginning
  ;;       ;; of the buffer.
  ;;       (when (looking-at-p block-re)
  ;;         (delete-region (point) (1+ (point-at-eol))))))
  ;;   (setq buffer-read-only t))
  ;; (add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)

  ;; Org-Capture templates
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


  (defface org-checkbox-done-text
    '((t (:foreground "#71696A")))
    "Face for the text part of a checked org-mode checkbox.")

  ;; Makes some things look nicer
  (setq org-startup-indented t
        org-pretty-entities t
        ;; show actually italicized text instead of /italicized text/
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
  (with-eval-after-load 'org
    (setq org-todo-keyword-faces
          '(("TODO" . "#dc752f")
            ("FIXME" . "#e75a7c")
            ("REMINDER" . "#dc752f")
            ("UNDERWAY" . "#51bbfe")
            ("BLOCKED" . "#d7263d")
            ("REVIEW" . "#f9db6d")
            ("CANCELLED" . "#adb6c4")
            ("DONE" . "#32936f"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Theme customization
  (setq theming-modifications
        '((farmhouse-dark
           (evil-ex-lazy-highlight :background "#cc0000")
           (iedit-occurrence :background "#7510F0"))))

  ;; End of user-config
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#2f2f2e" "#ffb4ac" "#8ac6f2" "#e5c06d" "#a4b5e6" "#e5786d" "#7ec98f" "#74736f"])
 '(beacon-color "#cc6666")
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "theGhostbook")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#2f2f2e" t)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(notmuch-search-line-faces
   '(("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t)))
 '(package-selected-packages
   '(company-lua lua-mode cargo company-go flycheck-rust go-eldoc go-fill-struct go-gen-test go-guru go-impl go-rename go-tag go-mode godoctor ron-mode rust-mode toml-mode ccls company-c-headers company-rtags company-ycmd cpp-auto-include disaster flycheck-rtags flycheck-ycmd gendoxy google-c-style helm-rtags rtags ycmd request-deferred org-reverse-datetree helm-bibtex olivetti org-ref ox-pandoc citeproc bibtex-completion biblio biblio-core parsebib org-roam compat ox-epub lsp-docker ox-gfm grip-mode github-search github-clone with-editor transient magit-section git-commit gist gh marshal logito lsp-sourcekit shrink-path cfrs slack circe oauth2 websocket lsp-ui lsp-python-ms lsp-pyright lsp-origami origami lsp-latex helm-lsp flyspell-correct-helm flyspell-correct auto-dictionary unicode-fonts ucs-utils font-utils persistent-soft pcache ligature keycast csv-mode command-log-mode company-quickhelp seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rbenv rake minitest enh-ruby-mode dap-mode posframe lsp-treemacs bui treemacs pfuture counsel swiper ivy chruby bundler inf-ruby tern livid-mode skewer-mode js2-refactor multiple-cursors js2-mode js-doc helm-gtags ggtags counsel-gtags add-node-modules-path typit mmt sudoku pacmacs dash-functional 2048-game zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon swift-mode sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme restart-emacs rebecca-theme rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el password-generator paradox overseer orgit organic-green-theme org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme neotree naquadah-theme nameless mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-svn magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum live-py-mode link-hint light-soap-theme kaolin-themes jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme indent-guide importmagic impatient-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme fuzzy font-lock+ flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode fill-column-indicator farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav editorconfig dumb-jump dracula-theme dotenv-mode doom-themes doom-modeline django-theme diminish diff-hl define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme dactyl-mode cython-mode cyberpunk-theme counsel-projectile company-web company-statistics company-shell company-emoji company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode clues-theme clean-aindent-mode cherry-blossom-theme centered-cursor-mode busybee-theme bubbleberry-theme browse-at-remote birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ace-window ace-link ace-jump-helm-line ac-ispell))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-ex-lazy-highlight ((t (:background "#cc0000"))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t)
 '(iedit-occurrence ((t (:background "#7510F0")))))
)
