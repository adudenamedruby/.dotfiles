#!/bin/bash
selected=$(cat ~/.local/myScripts/assets/.cht-languages ~/.local/myScripts/assets/.cht-command | fzf)
if [[ -z $selected ]]; then
    exit 0
fi

read -p "Enter Query: " query

if grep -qs "$selected" ~/.local/myScripts/assets/.cht-languages; then
    query=$(echo $query | tr ' ' '+')
    echo \"curl cht.sh/$selected/$query/\" &
    curl cht.sh/$selected/$query &
    wait
else
    # tmux neww bash -c "curl -s cht.sh/$selected~$query | less"
    echo "IAM COMMAND"
fi
