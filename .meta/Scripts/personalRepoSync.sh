#!/bin/bash

DEV="$HOME/Developer"
directories=($DEV/ExoCortex $DEV/Learn2Code $DEV/AdventOfCode $DEV/LeetCode $DEV/Wargames)
GIT_SYNC="$HOME/.dotfiles/.meta/Scripts/gitSync.sh"

for dir in "${directories[@]}"
do
  if [ -d "$dir" ]
  then
    cd "$dir"
    echo "Now updating: $dir"
    $GIT_SYNC
  else
    echo "Skipping: $dir - directory not found on this machine"
  fi
done
