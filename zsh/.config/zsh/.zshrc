# Enable colors and change prompt:
autoload -U colors && colors
export CLICOLOR=1
export LS_COLORS="DxCxcxdxbxegedabagacad"
stty stop undef		# Disable ctrl-s to freeze terminal.

# menu completion
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zmodload zsh/complist
compinit
_comp_options+=(globdots)

HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.dotfiles/zsh/.config/zsh/.zsh_history
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt incappendhistory
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# support text objects
autoload -Uz select-bracketed select-quoted
zle -N select-quoted
zle -N select-bracketed
for km in viopp visual; do
  bindkey -M $km -- '-' vi-up-line-or-history
  for c in {a,i}${(s..)^:-\'\"\`\|,./:;=+@}; do
    bindkey -M $km $c select-quoted
  done
  for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
    bindkey -M $km $c select-bracketed
  done
done

# Change cursor shape for different vi modes.
cursor_mode() {
    # See https://ttssh2.osdn.jp/manual/4/en/usage/tips/vim.html for cursor shapes
    cursor_block='\e[2 q'
    cursor_beam='\e[6 q'

    function zle-keymap-select {
        if [[ ${KEYMAP} == vicmd ]] ||
            [[ $1 = 'block' ]]; then
            echo -ne $cursor_block
        elif [[ ${KEYMAP} == main ]] ||
            [[ ${KEYMAP} == viins ]] ||
            [[ ${KEYMAP} = '' ]] ||
            [[ $1 = 'beam' ]]; then
            echo -ne $cursor_beam
        fi
    }

    zle-line-init() {
        echo -ne $cursor_beam
    }

    zle -N zle-keymap-select
    zle -N zle-line-init
}

cursor_mode

tmux() {
    if [[ $# -eq 0 ]]; then
        # Check if there are any existing tmux sessions
        if tmux ls >/dev/null 2>&1; then
            # Attach to the last session if sessions exist
            command tmux a
        else
            # Start a new session named 'random' if no sessions exist
            command tmux new -s random
        fi
    else
        # Pass all other arguments to the real tmux command
        command tmux "$@"
    fi
}

# Types
export EDITOR=nvim
export VISUAL=nvim
export PAGER=less
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

# source $(brew --prefix)/opt/powerlevel10k/powerlevel10k.zsh-theme
source $HOME/.dotfiles/zsh/.config/zsh/plugins/vi-mode.plugin.zsh
source $HOME/.dotfiles/zsh/.config/zsh/plugins/colored-man-pages.plugin.zsh
source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $(brew --prefix)/share/zsh-you-should-use/you-should-use.plugin.zsh

# Ruby
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
export PATH="/opt/homebrew/lib/ruby/gems/3.3.0/bin:$PATH"

# custom scripts
export PATH="$HOME/bin:$PATH"

# Haskell!
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env

[ -f $HOME/.config/aliases/.aliases ] && source $HOME/.config/aliases/.aliases
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# Unbind the original Ctrl+T
bindkey -r '^T'
# Bind Ctrl+F to fzf-file-widget
bindkey '^F' fzf-file-widget

# stop asking for my my password, git!
/usr/bin/ssh-add --apple-use-keychain ~/.ssh/id_ed25519 2>/dev/null

eval "$(starship init zsh)"

source /Users/adudenamedruby/.config/broot/launcher/bash/br
