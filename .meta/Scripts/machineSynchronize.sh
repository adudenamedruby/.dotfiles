#!/bin/bash

# Prompt helper: show `jj st` (if available) and ask to continue
confirm_continue() {
  # If jj is installed, show status
  if command -v jj >/dev/null 2>&1; then
    jj st
  fi

  while true; do
    read -r -p "Does everything look ok to continue (y/n): " ans
    case "$ans" in
    y | Y | yes | YES) return 0 ;;
    n | N | no | NO)
      printf "Abandoning operation.\n"
      return 1
      ;;
    *)
      printf "Please answer 'y' or 'n'.\n"
      ;;
    esac
  done
}

brewOperation() {
  printf "\n+++ Updating Homebrew +++\n"
  brew update
  brew upgrade
  brew cleanup
  printf "+++ Finished homebrew operations +++\n\n"
}

dotfilesOperation() {
  printf "+++ Syncing & stowing .dotfiles repo +++\n"
  # Use pushd/popd so we always return to previous dir
  if pushd ~/.dotfiles >/dev/null 2>&1; then
    # Show status + prompt; abort this operation if user says no
    if ! confirm_continue; then
      popd >/dev/null 2>&1
      printf "+++ Dotfiles operation abandoned +++\n\n"
      return 0
    fi

    jj describe -m "update dotfiles"
    jj tug
    jj git push

    popd >/dev/null 2>&1
    printf "+++ Finished dotfiles operations +++\n\n"
  else
    printf "Could not enter ~/.dotfiles\n"
  fi
}

troveOperation() {
  printf "\n+++ Syncing & stowing ruby-trove +++\n"
  if pushd ~/Documents/ruby-trove >/dev/null 2>&1; then
    if ! confirm_continue; then
      popd >/dev/null 2>&1
      printf "+++ Trove operation abandoned +++\n\n"
      return 0
    fi

    jj describe -m "organize trove"
    jj tug
    jj git push

    popd >/dev/null 2>&1
    printf "+++ Finished trove operations +++\n\n"
  else
    printf "Could not enter ~/Documents/ruby-trove\n"
  fi
}

machineSynchronize() {
  # Set default values for the flags
  dotfiles_operation=false
  brew_operation=false
  trove_operation=false

  # Parse the command line arguments
  while getopts "bdt" opt; do
    case $opt in
    d)
      dotfiles_operation=true
      ;;
    b)
      brew_operation=true
      ;;
    t)
      trove_operation=true
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
    esac
  done

  cd
  printf "\n+++ Beginning Sync Operations +++\n\n"

  # Do something with the flags
  if $brew_operation; then
    brewOperation
  fi

  if $trove_operation; then
    troveOperation
  fi

  if $dotfiles_operation; then
    dotfilesOperation
  fi

  printf "\n+++ Finished Sync Operations +++\n\n"
  cd
}

machineSynchronize "$@"
