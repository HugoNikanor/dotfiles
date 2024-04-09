#!/bin/sh

# Installs contents of this directory into user directory.
# Items in config/ are symlinked as items in ~/.config/
# other stuff is symlinked into ~/, but with a dot added beforehand

DATA_HOME=${XDG_DATA_HOME:-~/.local/share}
CACHE_HOME=${XDG_CACHE_HOME:-~/.cache}

verbose () {
	printf "%s " "$@"
	echo
	[ "${DRY_RUN:-no}" = "no" ] && "$@"
}

link_contents () {
	confdir=$1
	destdir=$2

	for file in "$confdir"/*; do
		[ -h "$HOME/$destdir/$(basename "$file")" ] || verbose ln -s "$file" "$HOME/$destdir/"
	done

}

cd "$(dirname "$(realpath "$0")")" || {
	echo CD to dotfiles directory failed
	echo Giving Up
	exit 1
}

for file in *; do
	if printf "%s\n" "$file" | grep -Eqv "($(tr '\n' '|' < exclude)^\$)"; then
		[ -h "$HOME/.$file" ] || verbose ln -s "$(realpath "$file")" "$HOME/.$file"
	fi
done

link_contents config .config
link_contents mutt .mutt
link_contents bin .local/bin

for f in scripts/*; do
	[ -d "$f" ] && continue
	[ -x "$f" ] && verbose "$f"
done


verbose mkdir -p "$DATA_HOME/xmonad" "$CACHE_HOME/xmonad"
