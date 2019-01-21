#!/usr/bin/env bash

# Install command-line tools using Homebrew.

# Install homebrew if it is not installed
which brew 1>&/dev/null
if [ ! "$?" -eq 0 ] ; then
    echo "Failed to find HOMEBREW. It must not be installed."
	echo "Hombrew required to bootstrap system. Attempting to install Homebrew..."
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
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
brew install grep --with-default-names

# Zsh 
brew install zsh zsh-completions
chsh -s /bin/zsh


# ---------------------------------------------
# Programming Languages
# ---------------------------------------------

# Python 3
brew install python3


# ---------------------------------------------
# Applications
# ---------------------------------------------

# Vim, followed by installing Plug plugin manager
brew install vim --with-override-system-vi

# The Unarchiver
brew cask install the-unarchiver

# Google Chrome
brew cask install google-chrome

# Keycastr
brew cask install keycastr

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications

# Spacemacs - develop branch!
git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d


# ---------------------------------------------
# Misc
# ---------------------------------------------

# My preferred fonts
brew tap caskroom/fonts
brew cask install font-source-code-pro
brew cask install font-hack
brew cask install font-hack-nerd-font
brew cask install font-hack-nerd-font-mono
brew cask install font-inconsolata
brew cask install font-inconsolata-for-powerline
brew cask install font-inconsolata-nerd-font
brew cask install font-inconsolata-nerd-font-mono


# ---------------------------------------------
# Terminal gimmicks xD
# ---------------------------------------------

# The computer fortune teller 
brew install fortune

# The famous cowsay
brew install cowsay

# Multicolored text output
brew install lolcat

# Cmatricx
brew install cmatricx

# SL
brew install sl


# Now that we're done brewing...
# Remove outdated versions from the cellar
brew cleanup
