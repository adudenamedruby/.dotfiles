#!/usr/bin/env bash

# Install command-line tools using Homebrew.

# Install homebrew if it is not installed
which brew 1>&/dev/null
if [ ! "$?" -eq 0 ] ; then
    echo "Failed to find HOMEBREW. It must not be installed."
	echo "Hombrew required to bootstrap system. Attempting to install Homebrew..."
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
	if [ ! "$?" -eq 0 ] ; then
		echo "Something went wrong. How depressing. Exiting..." && exit 1
	fi
fi

# Make sure we’re using the latest Homebrew
brew update

# Upgrade any already-installed formulae
brew upgrade


# ---------------------------------------------
# Core Utilities
# ---------------------------------------------

# Get the latest git
brew install git

# Install FZF
brew install fzf
/usr/local/opt/fzf/install

# Core Utils
brew install coreutils

# Ag
brew install the_silver_searcher

# OpenSSH
brew install openssh

# g|re|p
brew install grep

# Cocoapods
brew install cocoapods


# ---------------------------------------------
# Programming Languages
# ---------------------------------------------

# Python 3
brew install python3


# ---------------------------------------------
# Applications
# ---------------------------------------------

# Vim, followed by installing Plug plugin manager
brew install vim

# The Unarchiver
brew cask install the-unarchiver

# VLC
brew cask install vlc

# Onyx maintenance
brew cast install onyx

# Keycastr
brew cask install keycastr

# Alfred
brew cask install alfred

#

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications

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
brew cask install font-source-code-pro
brew cask install font-hack
brew cask install font-hack-nerd-font
brew cask install font-hack-nerd-font-mono
brew cask install font-inconsolata
brew cask install font-inconsolata-for-powerline
brew cask install font-inconsolata-nerd-font
brew cask install font-inconsolata-nerd-font-mono


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
