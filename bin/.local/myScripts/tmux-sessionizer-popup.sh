#!/bin/bash

# Create a temporary file to store the selected directory
tmpfile=$(mktemp /tmp/tmux_select.XXXXXX)

# Define the directories to search
directories=(~/Developer ~/.dotfiles/*/.config)

# Build the selection command
selection_command="find ${directories[*]} -mindepth 1 -maxdepth 1 -type d 2>/dev/null | fzf > \"$tmpfile\""

# Run the selection command in a tmux popup window
tmux display-popup -E "zsh -c '$selection_command'"

# Check if the temporary file has content (i.e., if a selection was made)
if [[ -s "$tmpfile" ]]; then
    selected=$(cat "$tmpfile")
    rm "$tmpfile"
else
    rm "$tmpfile"
    exit 0
fi

# Proceed with starting or switching to the tmux session
selected_name=$(basename "$selected" | tr . _)

# Check if tmux is running (should be, since we're inside tmux)
tmux_running=$(pgrep tmux)

if [[ -z $TMUX ]] && [[ -z $tmux_running ]]; then
    # Not inside tmux and tmux is not running; start a new session
    tmux new-session -s "$selected_name" -c "$selected"
    exit 0
fi

# Check if the session already exists
if ! tmux has-session -t="$selected_name" 2>/dev/null; then
    # Create a new detached session
    tmux new-session -ds "$selected_name" -c "$selected"
fi

# Switch to the selected session
tmux switch-client -t "$selected_name"
