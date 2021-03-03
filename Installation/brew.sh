#!/usr/bin/env bash

# Install command-line tools using Homebrew.

# Install homebrew if it is not installed
which brew 1>&/dev/null
if [ ! "$?" -eq 0 ] ; then
    	echo "Failed to find HOMEBREW. It must not be installed."
	echo "Hombrew required to bootstrap system. Attempting to install Homebrew..."
  	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  	echo 'eval $(/opt/homebrew/bin/brew shellenv)' >> /Users/rbuciu/.zprofile
    	eval $(/opt/homebrew/bin/brew shellenv)
	
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
brew install emacs-plus@27 --with-spacemacs-icon
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
