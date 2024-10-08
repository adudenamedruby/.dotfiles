#!/bin/bash
zellij action resize increase left
zellij action resize increase right
directories=(~/Developer ~/.dotfiles/*/.config)
selected=$(find "${directories[@]}" -mindepth 1 -maxdepth 1 -type d | fzf)

if [[ -z $selected ]]; then
    exit 0
fi

selected_name=$(basename "$selected" | tr . _)

zellij action detach
zellij attach textbook

# if ! tmux has-session -t=$selected_name 2>/dev/null; then
#     tmux new-session -ds $selected_name -c $selected
# fi
#
# tmux switch-client -t $selected_name
sleep 10
