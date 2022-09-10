#!/usr/bin/env bash

cd ~/code/git

#fetch xvim
git clone https://github.com/XVimProject/XVim2.git

#fetch exocortex
git clone git@github.com:electricRGB/ExoCortex.git

#fetch learn2code
git clone git@github.com:electricRGB/Learn2Code.git

#fetch zsh things
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:-${ZSH:-~/.oh-my-zsh}/custom}/plugins/zsh-completions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
