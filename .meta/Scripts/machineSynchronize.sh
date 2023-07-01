#!/bin/bash

SCRIPTS="$HOME/.dotfiles/.meta/Scripts"
GIT_SYNC="$HOME/.dotfiles/.meta/Scripts/gitSync.sh"

brewOperation() {
    printf "\n+++ Updating Homebrew +++\n"
    brew update
    brew upgrade
    brew cleanup
    printf "+++ Finished homebrew operations +++\n\n"
}

dotfilesOperation() {
    printf "+++ Syncing & stowing .dotfiles repo +++"
    cd ~/.dotfiles
    $GIT_SYNC
    $SCRIPTS/stow.sh
    printf "+++ Finished dotfiles operations +++\n\n"
}

reposOperation() {
    printf "+++ Syncing personal repos +++"
    $SCRIPTS/personalRepoSync.sh
    printf "+++ Finished syncing personal repo operations +++\n\n"
}

spacemacsOperation() {
    printf "+++ Updating Spacemacs +++"
    cd ~/.emacs.d
    git pull
    cd
    printf "+++ Finished updating Spacemacs +++\n\n"
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
    echo "Skipping Spacemacs for now"
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
