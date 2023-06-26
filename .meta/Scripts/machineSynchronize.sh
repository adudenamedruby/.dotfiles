#!/bin/bash

SCRIPTS="$HOME/.dotfiles/.meta/Scripts"

brewOperation() {
    echo "+++ Updating Homebrew +++"
    brew update
    brew upgrade
    brew cleanup
}

dotfilesOperation() {
    echo "+++ Syncing & stowing .dotfiles repo +++"
    $SCRIPTS/stow.sh
}

reposOperation() {
    echo "+++ Syncing personal repos +++"
}

machineSynchronize() {
  # Set default values for the flags
  dotfiles_operation=false
  repos_operation=false
  brew_operation=false

  # Parse the command line arguments
  while getopts "drba" opt; do
    case $opt in
      d)
        dotfiles_operation=true
        ;;
      r)
        repos_operation=true
        ;;
      b)
        brew_operation=true
        ;;
      a)
        dotfiles_operation=true
        repos_operation=true
        brew_operation=true
        ;;
      \?)
        echo "Invalid option: -$OPTARG" >&2
        exit 1
        ;;
    esac
  done

  # Do something with the flags
  if $brew_operation; then
    brewOperation
  fi

  if $dotfiles_operation; then
    dotfilesOperation
  fi

  if $repos_operation; then
    reposOperation
  fi
}

machineSynchronize "$@"
