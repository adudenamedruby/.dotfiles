#!/bin/bash

tmpfile=$(mktemp /tmp/tmux_select.XXXXXX)

directories=(~/Developer ~/.dotfiles/*/.config)

selection_command="find ${directories[*]} -mindepth 1 -maxdepth 1 -type d 2>/dev/null | fzf > \"$tmpfile\""

tmux display-popup -E "zsh -c '$selection_command'"

if [[ -s "$tmpfile" ]]; then
    selected=$(cat "$tmpfile")
    rm "$tmpfile"
else
    rm "$tmpfile"
    exit 0
fi

selected_name=$(basename "$selected" | tr . _)

tmux_running=$(pgrep tmux)

if [[ -z $TMUX ]] && [[ -z $tmux_running ]]; then
    tmux new-session -s "$selected_name" -c "$selected"
    exit 0
fi

if ! tmux has-session -t="$selected_name" 2>/dev/null; then
    tmux new-session -ds "$selected_name" -c "$selected"
fi

tmux switch-client -t "$selected_name"
