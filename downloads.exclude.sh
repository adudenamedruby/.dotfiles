#!/usr/bin/env bash


# Get POWERLEVER10k
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ~/.powerlevel10k

# Install oh-my-zsh framework
cd ~
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
cd ~/.dotfiles
