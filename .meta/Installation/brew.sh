#!/usr/bin/env bash

# Install Loopback first and foremost because it requires
# disabling SIP and it requires serious restarts

# Also install Xcode, and then open it.
# Also also, install 1Password

# Install homebrew if it is not installed

which -s brew
if [[ $? != 0 ]]; then
    # Install Homebrew
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# Make sure we’re using the latest Homebrew and upgrade installed formulae.
# This shouldn't be needed, but it's good practice to have
brew update
brew upgrade

# ---------------------------------------------
# The basics for machine use
# ---------------------------------------------

brew install --cask alfred

# Terminals
brew install --cask ghostty
brew install --cask wezterm
brew install starship

brew install --cask nikitabobko/tap/aerospace

# zsh related things
brew install zsh-syntax-highlighting
brew install zsh-autosuggestions
brew install zsh-you-should-use

# My preferred fonts
brew tap homebrew/cask-fonts
brew install --cask font-source-code-pro
brew install --cask font-fira-code
brew install --cask font-fira-code-nerd-font
brew install --cask font-hack
brew install --cask font-hack-nerd-font
brew install --cask font-iosevka

# Keyboard related things
brew install --cask karabiner-elements
brew install yqrashawn/goku/goku
softwareupdate --install-rosetta

# ---------------------------------------------
# Core Utilities
# ---------------------------------------------

# Here we'll install what I consider core utilities. Not apps,
# not fun stuff, but development oriented things, or QoL things
# like FZF.
brew install --cask hiddenbar

brew install eza

brew install bat

brew install stats

brew install carthage

brew install cmake

brew install coreutils

brew install ffmpegthumbnailer

brew install fzf
$(brew --prefix)/opt/fzf/install

brew install git

brew install git-delta

brew install gh

brew install gls

brew install grep

brew install imagemagick

brew install lazygit

brew install librsvg

brew install broot

brew install libvterm

brew install neovim

brew install node

brew install openssh

brew install pandoc

brew install poppler

brew install stow

brew install svn

brew install swiftlint

brew install swiftformat

brew install the_silver_searcher # AG

brew install tmux

brew install tree-sitter

brew install java
# check for symlink in install

brew install leiningen

brew install unzip

brew install tldr

# rust alternatives to utils
brew install btop # instead of top

brew install choose-rust # instead of cut/awk

brew install dust # better version of du

brew install fd # instead of find

brew install grex # regex generator of sorts

brew install lsd # instead of ls

brew install ouch # unarchiver, basically

brew install ripgrep # instead of grep

brew install sd # instead of sed

brew install tealdeer # instead of tldr

# ---------------------------------------------
# Programming Languages
# ---------------------------------------------

# Python 3
brew install python3

# virtual environments for Python
pip3 install virtualenv

# Lisp family languages
brew install clisp

brew install sbcl

brew install --cask racket

brew install clojure/tools/clojure
brew install clojure-lsp/brew/clojure-lsp-native
brew install borkdude/brew/clj-kondo

## Haskell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Rust
curl --proto '=https' --tlsv1.2 https://sh.rustup.rs -sSf | sh

# Prolog
brew install swi-prolog

# ---------------------------------------------
# Editors
# ---------------------------------------------

brew install nvim

brew install xcode-build-server
# when running a new project, you'll need to run this
# xcode-build-server config -scheme <XXX> -project *.xcodeproj
brew install xcbeautify
brew install ruby
brew install pipx
gem install xcodeproj

# Emacs
# brew tap d12frosted/emacs-plus
# brew install emacs-plus@29 --with-modern-sexy-v1-icon
# brew link emacs-plus
#ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications

# ---------------------------------------------
# Applications
# ---------------------------------------------

brew install --cask affinity-designer

brew install --cask affinity-photo

brew install --cask appcleaner

brew install --cask arc

brew install --cask calibre

# brew install --cask dash

brew install --cask daisydisk

brew install --cask devcleaner

brew install --cask devutils

brew install --cask discord

brew install --cask hiddenbar

brew install --cask keycastr

brew install --cask obs

brew install --cask onyx

brew install --cask slack

brew install --cask steam

brew install --cask todoist

brew install --cask vlc

brew install --cask zoom

# Apps outside of brew:
#
# amphetamine
# band-in-a-box
# final cut pro
# logic pro
# apple productivity (numbers, keynote, pages)
# spark
# notability

# ---------------------------------------------
# Terminal gimmicks xD
# ---------------------------------------------

# Cmatricx
brew install cmatrix
brew install forture
brew install cowsay

# Now that we're done brewing...
# Remove outdated versions from the cellar
brew cleanup
