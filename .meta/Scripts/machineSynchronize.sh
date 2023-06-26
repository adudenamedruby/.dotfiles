#!/bin/bash

SCRIPTS="$HOME/.dotfiles/.meta/Scripts"
GIT_SYNC="$HOME/.dotfiles/.meta/Scripts/gitSync.sh"

brewOperation() {
    echo "+++ Updating Homebrew +++"
    brew update
    brew upgrade
    brew cleanup
    echo "+++ Finished homebrew operations +++"
}

dotfilesOperation() {
    echo "+++ Syncing & stowing .dotfiles repo +++"
    cd ~/.dotfiles
    $GIT_SYNC
    $SCRIPTS/stow.sh
    echo "+++ Finished dotfiles operations +++"
}

reposOperation() {
    echo "+++ Syncing personal repos +++"
    $SCRIPTS/personalRepoSync.sh
    echo "+++ Finished syncing personal repo operations +++"
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

  cd

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

  cd
}

machineSynchronize "$@"
