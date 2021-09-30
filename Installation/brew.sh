#!/usr/bin/env bash

# Install command-line tools using Homebrew.

# Install homebrew if it is not installed

which -s brew
if [[ $? != 0 ]] ; then
    # Install Homebrew
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Make sure we’re using the latest Homebrew and upgrade installed formulae.
# This shouldn't be needed, but it's good practice to have
brew update
brew upgrade


# ---------------------------------------------
# Core Utilities
# ---------------------------------------------

# Here we'll install what I consider core utilities. Not apps,
# not fun stuff, but development oriented things, or QoL things
# like FZF.

brew install git
brew install fzf
/usr/local/opt/fzf/install
brew install coreutils
brew install the_silver_searcher # AG
brew install openssh

# g|re|p
brew install grep

# Cocoapods
brew install cocoapods

# Carthage
brew install carthage

# Node
brew install node

# CMAKE
brew install cmake

# libvterm
brew install libvterm

# SwiftLint
brew install swiftlint

# ---------------------------------------------
# Programming Languages
# ---------------------------------------------

# Python 3
brew install python3

# virtual environments for Python
pip3 install virtualenv

# Common Lisp
brew install scbl

# ---------------------------------------------
# Applications
# ---------------------------------------------

# Vim, followed by installing Plug plugin manager
brew install vim

# The Unarchiver
brew install --cask the-unarchiver

# VLC
brew install --cask vlc

# Onyx maintenance
brew install --cask onyx

# Keycastr
brew install --cask keycastr

# Alfred
brew install --cask alfred

# Rectangle - window management
brew install --cask rectangle

# Powerline10k
brew install romkatv/powerlevel10k/powerlevel10k

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@27 --with-modern-sexy-v1-icon
brew link emacs-plus
#ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications

# Select which branch of Spacemacs we want - master or dev
echo "By default this ulitily installs Spacemacs off the MASTER branch?"
echo "Would you like to install off DEVELOP branch instead? (y/n)"
read resp
if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
    echo "Develop branch selected"
    git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
else
    echo "Master branch selected"
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi


# ---------------------------------------------
# Other
# ---------------------------------------------

# My preferred fonts
brew tap homebrew/cask-fonts
brew install --cask font-source-code-pro
brew install --cask font-fira-code
brew install --cask font-hack
brew install --cask font-hack-nerd-font
brew install --cask font-inconsolata
brew install --cask font-inconsolata-for-powerline
brew install --cask font-inconsolata-nerd-font

# Get ZSH syntax highlighting
brew install zsh-syntax-highlighting

# Get ZSH auto-completions
brew install zsh-completions


# ---------------------------------------------
# Terminal gimmicks xD
# ---------------------------------------------

# Cmatricx
brew install cmatrix


# Now that we're done brewing...
# Remove outdated versions from the cellar
brew cleanup
