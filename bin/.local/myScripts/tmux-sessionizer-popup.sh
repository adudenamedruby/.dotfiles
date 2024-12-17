#!/bin/bash

tmpfile=$(mktemp /tmp/tmux_select.XXXXXX)

directories=(~/Developer ~/.dotfiles/*/.config ~/Developer/exercism)

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
    # No tmux server is running; start a new session
    tmux new-session -s "$selected_name" -c "$selected"
    tmux rename-window -t "${selected_name}:1" "editor"
    if [[ -f "$selected/README.md" ]]; then
        tmux send-keys -t "${selected_name}:1" "vim README.md" C-m
    fi
    exit 0
fi

# I use window 1 as an editor, basically always. And I mostly just
# want to be in NeoVim right away, too. So this helps set that up
setup_editor_window() {
    local session_name="$1"
    local selected_dir="$2"

    # I index on 1
    local window_index=1
    local pane_index=1

    # Rename the first window to "NeoVim" if it's not already named so
    first_window_name=$(tmux list-windows -t "$session_name" -F "#{window_index}:#{window_name}" | grep "^${window_index}:" | cut -d: -f2)
    if [[ "$first_window_name" != "NeoVim" ]]; then
        tmux rename-window -t "${session_name}:${window_index}" "NeoVim"
    fi

    # Check if Neovim is running; if not, open readme.md, if it exists
    pane_command=$(tmux list-panes -t "${session_name}:${window_index}.${pane_index}" -F "#{pane_current_command}")
    if [[ "$pane_command" != "nvim" ]]; then
        if [[ -f "$selected_dir/readme.md" ]]; then
            tmux send-keys -t "${session_name}:${window_index}.${pane_index}" "vim readme.md" C-m
        fi
    fi
}

if ! tmux has-session -t="$selected_name" 2>/dev/null; then
    # Create a detached session if it doesn't exist
    tmux new-session -ds "$selected_name" -c "$selected"
fi

tmux switch-client -t "$selected_name"
setup_editor_window "$selected_name" "$selected"
