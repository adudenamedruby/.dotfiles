#!/bin/bash
parent_pane_id="$1"

selected=$(cat ~/.local/myScripts/assets/.cht-languages ~/.local/myScripts/assets/.cht-command | fzf)
if [[ -z $selected ]]; then
    exit 0
fi

read -p "Enter query: " query

new_window_id=$(tmux new-window -P -F "#{window_id}" -n "cheat-sheet")
tmux send-keys -t "${new_window_id}" "zsh -c '~/.local/myScripts/cheat-display.sh \"${selected}\" \"${query}\"'" C-m
