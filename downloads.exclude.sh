#!/usr/bin/env bash

# Install oh-my-zsh framework
cd ~
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
cd ~/.dotfiles

# Get POWERLEVER10k
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ~/powerlevel10k

# Get ZSH syntax highlighting
brew install zsh-syntax-highlighting

# Get ZSH auto-completions
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
