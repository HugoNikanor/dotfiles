# Installs contents of this directory into user directory.
# Items in config/ are symlinked as items in ~/.config/
# other stuff is symlinked into ~/, but with a dot added beforehand

DATA_HOME=${XDG_DATA_HOME:-~/.local/share}
CACHE_HOME=${XDG_CACHE_HOME:-~/.cache}

verbose () {
	# set -x
	echo "$@"
	"$@"
	# set +x
}

link_contents () {
	confdir="$1"
	dir=$(realpath "$confdir")
	pushd "$HOME/.$confdir/" > /dev/null || exit 1
	for file in "$dir"/*; do
		[ -h "$file" ] || verbose ln -s "$dir/$file"
	done
	popd > /dev/null || return

}

cd "$(dirname "$(realpath "$0")")" || {
	echo CD to dotfiles directory failed
	echo Giving Up
	exit 1
}

for file in *; do
	case "$file" in
		LICENSE) continue ;;
		README.md) continue ;;
		bin) continue ;;
		color) continue ;;
		config)  continue ;;
		dosbox*) continue ;;
		elinks) continue ;;
		installer.sh) continue ;;
		mutt) continue ;;
		scripts) continue ;;
	esac
	[ -h "$HOME/.$file" ] || verbose ln -s "$(realpath "$file")" "$HOME/.$file"
done

for file in bin/*; do
	verbose ln -s "$(realpath "bin/$file")" "$HOME/.local/bin/"
done

link_contents config
link_contents mutt

for f in scripts/*; do
	[ -d "$f" ] && continue
	[ -x "$f" ] && verbose "$f"
done


mkdir -p $DATA_HOME/xmonad $CACHE_HOME/xmonad
