#!/bin/bash

# Capture the query from the argument
selected="$1"
query="$2"

if grep -qs "$selected" ~/.local/myScripts/assets/.cht-languages; then
    query=$(echo $query | tr ' ' '+')
    curl cht.sh/$selected/$query | bat &
    wait
else
    curl -s cht.sh/$selected~$query | bat &
    wait
fi
