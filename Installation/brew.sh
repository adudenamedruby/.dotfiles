#!/usr/bin/env bash

# Install command-line tools using Homebrew.

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

#Alfred
brew install --cask alfred

# ---------------------------------------------
# Terminal
# ---------------------------------------------
brew install --cask iterm2

# Powerline10k
brew install romkatv/powerlevel10k/powerlevel10k

brew install zsh-syntax-highlighting

brew install zsh-autosuggestions

brew install --cask karabiner-elements

brew install yqrashawn/goku/goku

# ---------------------------------------------
# Core Utilities
# ---------------------------------------------

# Here we'll install what I consider core utilities. Not apps,
# not fun stuff, but development oriented things, or QoL things
# like FZF.

brew install git

brew install fzf

$(brew --prefix)/opt/fzf/install

brew install coreutils

brew install openssh

brew install the_silver_searcher # AG

brew install grep

brew install pandoc

brew install carthage

brew install node

brew install cmake

brew install libvterm

brew install swiftlint

brew install svn

brew install pandoc

brew install librsvg

brew install npm

npm install -g tldr

# rust alternatives to

brew install ripgrep # instead of grep

brew install fd # instead of find 

brew install sd # instead of sed

brew install choose # instead of cut/awk

brew install lsd # instead of ls

brew install tldr

brew install grex

# ---------------------------------------------
# Programming Languages
# ---------------------------------------------

# Python 3
brew install python3

# virtual environments for Python
pip3 install virtualenv

# Common Lisp
brew install scbl

brew install --cask racket

# ---------------------------------------------
# Applications
# ---------------------------------------------

brew install nvim

brew install --cask the-unarchiver

brew install --cask vlc

brew install --cask onyx

brew install --cask keycastr

brew install --cask appcleaner

brew install --cask devutils

brew install --cask moom

brew install --cask dash

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@29 --with-modern-sexy-v1-icon
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
# Fonts
# ---------------------------------------------

# My preferred fonts
brew tap homebrew/cask-fonts
brew install --cask font-source-code-pro
brew install --cask font-fira-code
brew install --cask font-hack
brew install --cask font-hack-nerd-font

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
