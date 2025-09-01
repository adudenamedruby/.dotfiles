#!/usr/bin/env sh
set -eu
# Make word-splitting safer
IFS='
	'

ROOT_DIR="${ROOT_DIR:-$HOME/Documents/ruby-trove}"
INBOX_DIR="$ROOT_DIR/Inbox"
HUBS_DIR="$ROOT_DIR/Hubs"
ATOMS_DIR="$ROOT_DIR/Atoms"
SOURCES_DIR="$ROOT_DIR/Sources"

for cmd in fd rg awk; do
    command -v "$cmd" >/dev/null 2>&1 || {
        echo "Error: '$cmd' is required but not found in PATH." >&2
        exit 1
    }
done

# --- helpers ---------------------------------------------------------------

relpath() {
    p=$1
    case "$p" in
    "$ROOT_DIR") printf '.\n' ;;
    "$ROOT_DIR"/*) printf '%s\n' "${p#$ROOT_DIR/}" ;;
    *) printf '%s\n' "$p" ;; # outside root → leave unchanged
    esac
}

# true if path is under $ROOT_DIR
in_root() {
    p=$1
    case "$p" in "$ROOT_DIR" | "$ROOT_DIR"/*) return 0 ;; *) return 1 ;; esac
}

# extract_frontmatter FILE -> print YAML block (no --- lines)
extract_frontmatter() {
    awk '
    f==0 && $0=="---" {f=1; next}
    f==1 && $0=="---" {exit}
    f==1 {print}
  ' "$1"
}

# get_filing_status_from_fm FM_TEXT -> prints the value or empty
get_filing_status_from_fm() {
    printf '%s\n' "$1" |
        rg -o -m1 '^filing_status:\s*"?([^"\n]+)"?' -r '$1' -N || true
}

# get_tags_from_fm FM_TEXT -> one tag per line
get_tags_from_fm() {
    # Handles:
    #   tags:
    #     - hub
    #     - book
    # and inline:
    #   tags: [hub, book]
    awk '
    { sub(/\r$/, ""); }  # strip Windows CR if present
    # Inline list form
    /^tags:[[:space:]]*\[/ {
      line=$0
      sub(/^tags:[[:space:]]*\[/, "", line)
      sub(/\][[:space:]]*$/, "", line)
      n=split(line, a, /[[:space:]]*,[[:space:]]*/)
      for (i=1;i<=n;i++) {
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", a[i])
        if (a[i] != "") print a[i]
      }
      next
    }
    # Block list form
    /^tags:[[:space:]]*$/ { intags=1; next }
    intags {
      # Stop when we hit the next top-level key like "hubs:" or "created:"
      if ($0 ~ /^[^[:space:]].*:[[:space:]]*($|[^[])/) { exit }
      if ($0 ~ /^[[:space:]]*-[[:space:]]+/) {
        sub(/^[[:space:]]*-[[:space:]]+/, "", $0)
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", $0)
        if ($0 != "") print
      }
    }
  ' <<EOF
$1
EOF
}

# resolve_target_dir TAG -> echoes target dir or empty
resolve_target_dir() {
    t="$1"
    case "$t" in
    hub) printf '%s\n' "$HUBS_DIR" ;;
    atom) printf '%s\n' "$ATOMS_DIR" ;;
    book) printf '%s\n' "$SOURCES_DIR/Books" ;;
    video) printf '%s\n' "$SOURCES_DIR/Videos" ;;
    article) printf '%s\n' "$SOURCES_DIR/Articles" ;;
    podcast) printf '%s\n' "$SOURCES_DIR/Podcasts" ;;
    movie) printf '%s\n' "$SOURCES_DIR/Movies" ;;
    other) printf '%s\n' "$SOURCES_DIR/Others" ;;
    *) printf '%s\n' "" ;;
    esac
}

# move_safely SRC DESTDIR (creates DESTDIR if missing, avoids clobber)
move_safely() {
    src="$1"
    destdir="$2"
    mkdir -p "$destdir"
    base="$(basename "$src")"
    dest="$destdir/$base"

    if [ -e "$dest" ]; then
        ext=""
        name="$base"
        case "$base" in
        *.*)
            ext=".$(printf '%s' "$base" | awk -F. '{print $NF}')"
            name="${base%.*}"
            ;;
        esac
        n=1
        while [ -e "$destdir/${name}-${n}${ext}" ]; do
            n=$((n + 1))
        done
        dest="$destdir/${name}-${n}${ext}"
    fi

    mv "$src" "$dest"

    if in_root "$src" || in_root "$dest"; then
        printf 'Moved: %s -> %s\n' "$(relpath "$src")" "$(relpath "$dest")"
    fi
}

lower() {
    # strip CR, trim leading/trailing space, then lowercase
    printf '%s' "$1" | tr -d '\r' | awk '{$1=$1}1' | tr '[:upper:]' '[:lower:]'
}

# --- Step 1: Move "ready" items out of Inbox --------------------------------
fd -t f -e md . "$INBOX_DIR" | while IFS= read -r file; do
    [ -n "$file" ] || continue
    fm="$(extract_frontmatter "$file")" || fm=""
    status="$(lower "$(get_filing_status_from_fm "$fm" || true)")"
    [ "$status" = "ready" ] || continue

    # Gather tags as lowercase, one per line
    tags="$(get_tags_from_fm "$fm" | tr '[:upper:]' '[:lower:]')"

    target_dir=""
    for t in $tags; do
        # Clean the tag: strip CR and surrounding whitespace
        t="$(printf '%s' "$t" | tr -d '\r' | awk '{$1=$1}1')"
        [ -n "$t" ] || continue

        td="$(resolve_target_dir "$t")"
        if [ -n "$td" ]; then
            target_dir="$td"
            break
        fi
    done

    # If no mapped tag, skip
    if [ -z "$target_dir" ]; then
        printf 'Skip (no mapped tag): %s\n' "$file"
        continue
    fi

    move_safely "$file" "$target_dir"
done

# --- Step 2: Move "editing" items from Sources/* and Atoms back to Inbox -----
fd -t f -e md . "$ATOMS_DIR" "$SOURCES_DIR" | while IFS= read -r file; do
    [ -n "$file" ] || continue
    fm="$(extract_frontmatter "$file")" || fm=""
    status="$(lower "$(get_filing_status_from_fm "$fm" || true)")"
    [ "$status" = "editing" ] || continue
    move_safely "$file" "$INBOX_DIR"
done
