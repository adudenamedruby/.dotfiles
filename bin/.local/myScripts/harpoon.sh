#!/usr/bin/env bash

set -euo pipefail

# Global harpoon storage
HARPOON_FILE="${HOME}/.dotfiles/zed/.config/zed/harpoon.txt"

mkdir -p "$(dirname "$HARPOON_FILE")"
touch "$HARPOON_FILE"

# Given an absolute file path, figure out its repo root:
# - If it's inside a git repo: use `git rev-parse --show-toplevel`
# - Otherwise: use the file's directory as its "repo root"
repo_from_file() {
  local file="$1"

  if [[ -z "$file" ]]; then
    echo "repo_from_file: no file path provided" >&2
    exit 1
  fi

  # Normalize to absolute, just in case
  if [[ "$file" != /* ]]; then
    file="$(pwd)/$file"
  fi

  local dir
  dir=$(dirname "$file")

  if git_root=$(git -C "$dir" rev-parse --show-toplevel 2>/dev/null); then
    printf '%s\n' "$git_root"
  else
    printf '%s\n' "$dir"
  fi
}

filter_out_repo_slot() {
  local repo_root="$1"
  local slot="$2"

  awk -F '\t' -v repo="$repo_root" -v slot="$slot" '
    !($1 == repo && $2 == slot) { print }
  ' "$HARPOON_FILE"
}

filter_out_repo() {
  local repo_root="$1"

  awk -F '\t' -v repo="$repo_root" '
    $1 != repo { print }
  ' "$HARPOON_FILE"
}

find_line_for_repo_slot() {
  local repo_root="$1"
  local slot="$2"

  awk -F '\t' -v repo="$repo_root" -v slot="$slot" '
    $1 == repo && $2 == slot { print; exit }
  ' "$HARPOON_FILE"
}

# add <slot> <absolute-path>
cmd_add() {
  local slot="${1:-}"
  local file_abs="${2:-}"

  if [[ -z "$slot" || -z "$file_abs" ]]; then
    echo "Usage: harpoon.sh add <slot> <absolute-path>" >&2
    exit 1
  fi

  if [[ ! -f "$file_abs" ]]; then
    echo "File not found: $file_abs" >&2
    exit 1
  fi

  local repo_root
  repo_root=$(repo_from_file "$file_abs")

  local tmp
  tmp=$(mktemp)

  # Remove existing mapping for this repo+slot, then append the new one
  filter_out_repo_slot "$repo_root" "$slot" >"$tmp"
  printf '%s\t%s\t%s\n' "$repo_root" "$slot" "$file_abs" >>"$tmp"

  mv "$tmp" "$HARPOON_FILE"
}

# open <slot> <context-file>
# context-file is just used to know which repo we're in (e.g. ZED_FILE)
cmd_open() {
  local slot="${1:-}"
  local ctx_file="${2:-}"

  if [[ -z "$slot" || -z "$ctx_file" ]]; then
    echo "Usage: harpoon.sh open <slot> <context-file>" >&2
    exit 1
  fi

  local repo_root
  repo_root=$(repo_from_file "$ctx_file")

  local line
  line=$(find_line_for_repo_slot "$repo_root" "$slot")

  if [[ -z "$line" ]]; then
    echo "No file saved for slot $slot in repo $repo_root" >&2
    exit 1
  fi

  local path_abs
  path_abs=$(printf '%s\n' "$line" | cut -f3-)

  zed "$path_abs"
}

# delete <slot> <context-file>
cmd_delete() {
  local slot="${1:-}"
  local ctx_file="${2:-}"

  if [[ -z "$slot" || -z "$ctx_file" ]]; then
    echo "Usage: harpoon.sh delete <slot> <context-file>" >&2
    exit 1
  fi

  local repo_root
  repo_root=$(repo_from_file "$ctx_file")

  local tmp
  tmp=$(mktemp)

  filter_out_repo_slot "$repo_root" "$slot" >"$tmp"
  mv "$tmp" "$HARPOON_FILE"
}

# clear <context-file>
cmd_clear() {
  local ctx_file="${1:-}"

  if [[ -z "$ctx_file" ]]; then
    echo "Usage: harpoon.sh clear <context-file>" >&2
    exit 1
  fi

  local repo_root
  repo_root=$(repo_from_file "$ctx_file")

  local tmp
  tmp=$(mktemp)

  filter_out_repo "$repo_root" >"$tmp"
  mv "$tmp" "$HARPOON_FILE"
}

subcmd="${1:-}"
shift || true

case "$subcmd" in
add) cmd_add "$@" ;;
open) cmd_open "$@" ;;
delete) cmd_delete "$@" ;;
clear) cmd_clear "$@" ;;
*)
  cat >&2 <<EOF
Usage: harpoon.sh <command> [args]

Commands:
  add <slot> <absolute-path>
      Save file into a harpoon slot for its repo.

  open <slot> <context-file>
      Open saved file for <slot> in the repo of <context-file>.

  delete <slot> <context-file>
      Remove saved file in <slot> in the repo of <context-file>.

  clear <context-file>
      Clear all slots for the repo of <context-file>.
EOF
  exit 1
  ;;
esac
