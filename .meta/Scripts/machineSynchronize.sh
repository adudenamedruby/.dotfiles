#!/bin/bash

MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
BOLD_GREEN='\033[1;32m'
BOLD_RED='\033[1;31m'
NC='\033[0m'

confirm_continue() {
  # If jj is installed, show status
  if command -v jj >/dev/null 2>&1; then
    jj st
  else
    echo -e "${MAGENTA}SYNC: ${BOLD_RED}Error: jj is not installed. Please install using 'brew install jj' and try again${NC}"
    exit 1
  fi

  while true; do
    echo -e "${MAGENTA}SYNC: ${GREEN}Verifying change...${NC}"
    read -r -p "Continue? (y/n): " ans
    case "$ans" in
    y | Y | yes | YES) return 0 ;;
    n | N | no | NO)
      echo -e "${MAGENTA}SYNC: ${GREEN}cancelling operation ${NC}"
      exit 1
      ;;
    *)
      echo -e "${MAGENTA}SYNC: ${BOLD_RED}Input error\n${GREEN}Please answer 'y' or 'n'.${NC}"
      ;;
    esac
  done
}

brewOperation() {
  echo -e "${MAGENTA}SYNC: ${GREEN}Initializing ${CYAN}brew ${GREEN}update operation...${NC}"
  brew update
  brew upgrade
  brew cleanup
  echo -e "${MAGENTA}SYNC: ${CYAN}brew ${GREEN}maintenance process completed ${BOLD_GREEN}SUCCESSFULLY${NC}"
}

dotfilesOperation() {
  echo -e "${MAGENTA}SYNC: ${GREEN}Initializing ${CYAN}dotfiles ${GREEN}sync...${NC}"
  if pushd ~/.dotfiles >/dev/null 2>&1; then
    if ! confirm_continue; then
      popd >/dev/null 2>&1
      echo -e "${MAGENTA}SYNC: ${CYAN}dotfiles ${GREEN}operation ${BOLD_RED}CANCELLED${NC}"
      return 0
    fi

    jj git fetch
    jj rebase -s main -d @git(remote=origin,branch=main) >/dev/null 2>&1 || true
    if [[ -n "$(jj diff -s)" ]]; then
      jj describe -m "update dotfiles"
      jj rebase -d main
      jj tug
      jj git push
    fi

    popd >/dev/null 2>&1
    echo -e "${MAGENTA}SYNC: ${CYAN}dotfiles ${GREEN}operation completed ${BOLD_GREEN}SUCCESSFULLY${NC}"
  else
    echo -e "${MAGENTA}SYNC: ${GREEN}Could not pushd ~/.dotfiles"
    echo -e "${MAGENTA}SYNC: ${CYAN}dotfiles ${GREEN}sync operation ${BOLD_RED}FAILED${NC}"
  fi
}

troveOperation() {
  echo -e "${MAGENTA}SYNC: ${GREEN}Initializing ${CYAN}ruby-trove ${GREEN}sync...${NC}"
  if pushd ~/Documents/ruby-trove >/dev/null 2>&1; then
    "$HOME/.dotfiles/bin/.local/myScripts/trove-org.sh"
    echo -e "${MAGENTA}SYNC: ${CYAN}ruby-trove ${GREEN}organization completed ${BOLD_GREEN}SUCCESSFULLY${NC}"
    if ! confirm_continue; then
      popd >/dev/null 2>&1
      echo -e "${MAGENTA}SYNC: ${CYAN}ruby-trove ${GREEN}operation ${BOLD_RED}CANCELLED${NC}"
      return 0
    fi

    jj git fetch
    jj rebase -s main -d @git(remote=origin,branch=main) >/dev/null 2>&1 || true
    if [[ -n "$(jj diff -s)" ]]; then
      jj describe -m "update trove"
      jj rebase -d main
      jj tug
      jj git push
    fi

    popd >/dev/null 2>&1
    echo -e "${MAGENTA}SYNC: ${CYAN}ruby-trove ${GREEN}operation completed ${BOLD_GREEN}SUCCESSFULLY${NC}"
  else
    echo -e "${MAGENTA}SYNC: ${GREEN}Could not pushd ~/Documents/ruby-trove${NC}"
    echo -e "${MAGENTA}SYNC: ${CYAN}ruby-trove ${GREEN}sync operation ${BOLD_RED}FAILED${NC}"
  fi
}

machineSynchronize() {
  # Set default values for the flags
  dotfiles_operation=false
  brew_operation=false
  trove_operation=false

  # Parse the command line arguments
  while getopts "bdtah" opt; do
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
    a)
      brew_operation=true
      dotfiles_operation=true
      trove_operation=true
      ;;
    h)
      echo -e "${MAGENTA}SYNC: ${GREEN}Help menu"
      echo "SYNC requires at least one argument to run."
      echo "Please provide one of the following arguments:"
      echo "   -b   runs brew update, upgrade, and clean"
      echo "   -d   syncs & stows dotfiles"
      echo "   -t   syncs trove"
      echo "   -a   perform all operations"
      echo "   -h   this menu"
      exit 1
      ;;
    \?)
      echo -e "${MAGENTA}SYNC: ${BOLD_RED}Error: invalid option -$OPTARG" >&2
      exit 1
      ;;
    esac
  done

  echo -e "\n${MAGENTA}SYNC: ${GREEN}Initializisg ${CYAN}SYNC ${GREEN}Operations..."

  if $brew_operation; then
    brewOperation
  fi

  if $trove_operation; then
    troveOperation
  fi

  if $dotfiles_operation; then
    dotfilesOperation
  fi

  echo -e "${MAGENTA}SYNC: ${GREEN}all operations completed${NC}"
}

machineSynchronize "$@"
