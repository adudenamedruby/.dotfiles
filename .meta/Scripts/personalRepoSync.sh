#!/bin/bash

DEV="$HOME/Developer"
directories=(~/.dotfiles $DEV/ExoCortex $DEV/Learn2Code $DEV/AdventOfCode $DEV/LeetCode $DEV/Wargames)

for dir in "${directories[@]}"
do
  if [ -d "$dir" ]
  then
    cd "$dir"
    git status
  else
    echo "Skipping: $dir - directory not found on this machine"
  fi
done
