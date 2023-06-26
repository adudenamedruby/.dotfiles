#
# .zshenv - Zsh environment file, loaded always.
#

export ZDOTDIR=~/.config/zsh

# Apps
export EDITOR=nvim
export VISUAL=nvim
export PAGER=bat
if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi
