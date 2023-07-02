#!/usr/bin/env bash

# Install Loopback first and foremost because it requires
# disabling SIP and it requires serious restarts

# Also install Xcode, and then open it.
# Also also, install 1Password


# Install homebrew if it is not installed

which -s brew
if [[ $? != 0 ]] ; then
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
brew install --cask iterm2
brew install --cask kitty
brew install --cask warp
brew install starship

brew install --cask amethyst

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

brew install carthage

brew install cmake

brew install coreutils

brew install fzf
$(brew --prefix)/opt/fzf/install

brew install git

brew install grep

brew install librsvg

brew install libvterm

brew install node

brew install openssh

brew install pandoc

brew install stow

brew install svn

brew install swiftlint

brew install the_silver_searcher # AG

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

brew install scbl

brew install --cask racket

brew install clojure/tools/clojure

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

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@29 --with-modern-sexy-v1-icon
brew link emacs-plus
#ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications

# Select which branch of Spacemacs we want - master or dev
git clone https://github.com/syl20bnr/spacemacs $HOME/.emacs.d

# ---------------------------------------------
# Applications
# ---------------------------------------------

brew install --cask affinity-designer

brew install --cask affinity-photo

brew install --cask appcleaner

brew install --cask arc

brew install --cask calibre

brew install --cask dash

brew install --cask daisydisk

brew install --cask devcleaner

brew install --cask devutils

brew install --cask fantastical

brew install --cask keycastr

brew install --cask moom

brew install --cask obs

brew install --cask onyx

brew install --cask spotify

brew install --cask steam

brew install --cask todoist

brew install --cask vlc

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
