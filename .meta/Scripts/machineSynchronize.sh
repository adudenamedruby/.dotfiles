#!/bin/bash

SCRIPTS="$HOME/.dotfiles/.meta/Scripts"
GIT_SYNC="$HOME/.dotfiles/.meta/Scripts/gitSync.sh"

brewOperation() {
    echo "+++ Updating Homebrew +++"
    brew update
    brew upgrade
    brew cleanup
    echo "\n+++ Finished homebrew operations +++\n"
}

dotfilesOperation() {
    echo "+++ Syncing & stowing .dotfiles repo +++"
    cd ~/.dotfiles
    $GIT_SYNC
    $SCRIPTS/stow.sh
    echo "\n+++ Finished dotfiles operations +++\n"
}

reposOperation() {
    echo "+++ Syncing personal repos +++"
    $SCRIPTS/personalRepoSync.sh
    echo "\n+++ Finished syncing personal repo operations +++\n"
}

spacemacsOperation() {
    echo "+++ Updating Spacemacs +++"
    cd ~/.emacs.d
    git pull
    cd
    echo "\n+++ Finished updating Spacemacs +++\n"
}

machineSynchronize() {
  # Set default values for the flags
  dotfiles_operation=false
  repos_operation=false
  brew_operation=false
  spacemacs_operation=false

  # Parse the command line arguments
  while getopts "drbsa" opt; do
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
      s)
        spacemacs_operation=true
        ;;
      a)
        dotfiles_operation=true
        repos_operation=true
        brew_operation=true
        spacemacs_operation=true
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

  if $spacemacs_operation; then
    #spacemacsOperation
  fi

  if $repos_operation; then
    reposOperation
  fi

  if $dotfiles_operation; then
    dotfilesOperation
  fi

  cd
}

machineSynchronize "$@"
