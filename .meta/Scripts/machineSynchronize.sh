#!/bin/bash

# -------- Colors (TTY-aware) --------
if [[ -t 1 ]]; then
  MAGENTA='\033[0;35m'
  CYAN='\033[0;36m'
  GREEN='\033[0;32m'
  BOLD_GREEN='\033[1;32m'
  BOLD_RED='\033[1;31m'
  NC='\033[0m'
else
  MAGENTA='' CYAN='' GREEN='' BOLD_GREEN='' BOLD_RED='' NC=''
fi

# -------- Logging helpers --------
log() { printf "%bSYNC:%b %b\n" "$MAGENTA" "$NC" "$*"; }
ok() { printf "%bSYNC:%b %b%b%b\n" "$MAGENTA" "$NC" "$BOLD_GREEN" "$*" "$NC"; }
warn() { printf "%bSYNC:%b %b%b%b\n" "$MAGENTA" "$NC" "$GREEN" "$*" "$NC"; }
err() { printf "%bSYNC:%b %bError:%b %b\n" "$MAGENTA" "$NC" "$BOLD_RED" "$NC" "$*"; }

# -------- Globals toggled by flags --------
AUTO_YES=false # -y non-interactive
DRY_RUN=false  # -n dry-run

# -------- Utilities --------
ensure_cmd() {
  # ensure_cmd <cmd> <human-name>
  if ! command -v "$1" >/dev/null 2>&1; then
    err "$2 not found (missing '$1')"
    return 2
  fi
}

with_dir() {
  # with_dir <dir> -- <command...>
  local dir=$1
  shift
  [[ $1 == -- ]] && shift
  if [[ ! -d "$dir" ]]; then
    err "Directory not found: $dir"
    return 2
  fi
  pushd "$dir" >/dev/null 2>&1 || {
    err "Could not enter $dir"
    return 2
  }
  "$@"
  local rc=$?
  popd >/dev/null 2>&1
  return "$rc"
}

run() {
  # Honors DRY_RUN
  if $DRY_RUN; then
    printf "[dry-run] %q " "$@"
    printf "\n"
    return 0
  else
    "$@"
  fi
}

confirm_continue() {
  # confirm_continue <dir> [label]
  # 0=Yes, 1=No/Cancel, 2=Missing prereq
  local dir=$1
  local label=${2:-operation}

  if ! ensure_cmd jj "Jujutsu"; then
    warn "Skipping: Jujutsu (jj) not installed. Install with 'brew install jj'."
    return 2
  fi

  if [[ ! -d "$dir" ]]; then
    err "[$label] Directory not found: $dir"
    return 2
  fi

  # Show status *inside* the repo we're about to touch
  (
    cd "$dir" || exit 2
    run jj st
  ) || return 2

  if $AUTO_YES; then
    warn "[$label] Auto-continue enabled (-y)."
    return 0
  fi

  if [[ ! -t 0 ]]; then
    warn "[$label] No TTY available; cancelling this operation. Use -y to auto-continue."
    return 1
  fi

  while true; do
    warn "[$label] Verifying change..."
    read -r -p "[$label] Continue? (y/n): " ans </dev/tty
    case "$ans" in
    y | Y | yes | YES) return 0 ;;
    n | N | no | NO)
      warn "[$label] Cancelling this operation."
      return 1
      ;;
    *)
      err "[$label] Input error"
      warn "Please answer 'y' or 'n'."
      ;;
    esac
  done
}

confirm_continue_old() {
  # confirm_continue [label]
  # 0=Yes, 1=No/Cancel, 2=Missing prereq
  local label=${1:-operation}

  if ! ensure_cmd jj "Jujutsu"; then
    warn "Skipping: Jujutsu (jj) not installed. Install with 'brew install jj'."
    return 2
  fi

  run jj st

  if $AUTO_YES; then
    warn "[$label] Auto-continue enabled (-y)."
    return 0
  fi

  # If there is no TTY and not AUTO_YES, cancel gracefully
  if [[ ! -t 0 ]]; then
    warn "[$label] No TTY available; cancelling this operation. Use -y to auto-continue."
    return 1
  fi

  while true; do
    warn "[$label] Verifying change..."
    read -r -p "[$label] Continue? (y/n): " ans </dev/tty
    case "$ans" in
    y | Y | yes | YES) return 0 ;;
    n | N | no | NO)
      warn "[$label] Cancelling this operation."
      return 1
      ;;
    *)
      err "[$label] Input error"
      warn "Please answer 'y' or 'n'."
      ;;
    esac
  done
}

jj_sync_repo() {
  # jj_sync_repo <dir> <message> <label>
  local dir=$1 msg=$2 label=$3

  if [[ ! -d "$dir" ]]; then
    err "Directory not found: $dir"
    return 2
  fi

  (
    cd "$dir" || exit 2
    run jj git fetch || exit $?
    # Rebase onto origin/main; ignore no-op failures
    run jj rebase -s main -d '@git(remote=origin,branch=main)' >/dev/null 2>&1 || true

    # Only proceed if there are changes
    if [[ -n "$(jj diff -s)" ]]; then
      run jj describe -m "$msg" || exit $?
      run jj rebase -d main || exit $?
      run jj tug || exit $?
      run jj git push || exit $?
    fi
  ) || {
    err "$label sync encountered an error."
    return 3
  }

  ok "$label operation completed SUCCESSFULLY"
  return 0
}

brewOperation() {
  log "Initializing ${CYAN}brew${NC} update operation..."
  if ! ensure_cmd brew "Homebrew"; then
    warn "Skipping brew (not installed)."
    return 2
  fi
  run brew update || return $?
  run brew upgrade || true
  run brew cleanup || true
  ok "${CYAN}brew${NC} maintenance process completed SUCCESSFULLY"
}

dotfilesOperation() {
  log "Initializing ${CYAN}dotfiles${NC} sync..."
  confirm_continue "$HOME/.dotfiles" "dotfiles"
  local rc=$?
  if ((rc != 0)); then
    ((rc == 2)) && warn "dotfiles skipped (prereq missing)." || warn "dotfiles operation CANCELLED."
    return "$rc"
  fi
  jj_sync_repo "$HOME/.dotfiles" "update dotfiles" "${CYAN}dotfiles${NC}"
}

troveOperation() {
  log "Initializing ${CYAN}ruby-trove${NC} sync..."
  local repo="$HOME/Documents/ruby-trove"
  local org="$HOME/.dotfiles/bin/.local/myScripts/trove-org.sh"

  if [[ -x "$org" ]]; then
    with_dir "$repo" -- run "$org" || return 3
    ok "${CYAN}ruby-trove${NC} organization completed SUCCESSFULLY"
  else
    warn "Skipping org step: $org not found or not executable."
  fi

  confirm_continue "$repo" "ruby-trove"
  local rc=$?
  if ((rc != 0)); then
    ((rc == 2)) && warn "ruby-trove skipped (prereq missing)." || warn "ruby-trove operation CANCELLED."
    return "$rc"
  fi
  jj_sync_repo "$repo" "update trove" "${CYAN}ruby-trove${NC}"
}

usage() {
  cat <<EOF
Usage: $(basename "$0") [-b] [-d] [-t] [-a] [-y] [-n] [-h]

  -b   run brew update, upgrade, cleanup
  -d   sync dotfiles
  -t   sync ruby-trove
  -a   perform all operations (-b -d -t)
  -y   non-interactive (auto-yes to prompts)
  -n   dry-run (print commands, do not execute)
  -h   show this help
EOF
}

machineSynchronize() {
  local dotfiles_operation=false
  local brew_operation=false
  local trove_operation=false

  while getopts ":bdtaynh" opt; do
    case $opt in
    d) dotfiles_operation=true ;;
    b) brew_operation=true ;;
    t) trove_operation=true ;;
    a)
      brew_operation=true
      dotfiles_operation=true
      trove_operation=true
      ;;
    y) AUTO_YES=true ;;
    n) DRY_RUN=true ;;
    h)
      usage
      return 0
      ;;
    \?)
      err "invalid option -$OPTARG"
      usage
      return 1
      ;;
    esac
  done

  if ! $brew_operation && ! $dotfiles_operation && ! $trove_operation; then
    err "No operations selected."
    usage
    return 1
  fi

  printf "\n"
  log "Initializing ${CYAN}SYNC${NC} operations..."
  log "Selected: brew=$brew_operation dotfiles=$dotfiles_operation trove=$trove_operation (auto-yes=$AUTO_YES dry-run=$DRY_RUN)"

  # Optional pre-flight (non-fatal)
  $brew_operation && ensure_cmd brew "Homebrew" >/dev/null 2>&1 || true
  # jj is checked on demand in confirm_continue

  local overall_rc=0 rc=0

  if $brew_operation; then
    brewOperation
    rc=$?
    ((rc != 0)) && overall_rc=1
  fi
  if $trove_operation; then
    troveOperation
    rc=$?
    ((rc != 0)) && overall_rc=1
  fi
  if $dotfiles_operation; then
    dotfilesOperation
    rc=$?
    ((rc != 0)) && overall_rc=1
  fi

  if ((overall_rc == 0)); then
    ok "all selected operations completed"
  else
    warn "completed with some issues"
  fi
  return "$overall_rc"
}

# -------- Entry --------
machineSynchronize "$@"
