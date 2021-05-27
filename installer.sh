# Installs contents of this directory into user directory.
# Items in config/ are symlinked as items in ~/.config/
# other stuff is symlinked into ~/, but with a dot added beforehand

verbose () {
	# set -x
	echo $@
	"$@"
	# set +x
}

link_contents () {
	confdir="$1"
	dir=$(realpath $confdir)
	pushd "$HOME/.$confdir/" > /dev/null
	for file in $(ls $dir); do
		[ -h $file ] || verbose ln -s $dir/$file
	done
	popd > /dev/null

}

for file in $(ls \
	-I config \
	-I dosbox* \
	-I installer.sh \
	-I README.md \
	-I mutt \
	-I elinks \
	-I bin \
	-I scripts)
do
	[ -h $HOME/.$file ] || verbose ln -s $(realpath $file) $HOME/.$file
done

for file in $(ls bin); do
	verbose ln -s $(realpath bin/$file) $HOME/.local/bin/
done

link_contents config
link_contents mutt

for file in $(ls -I bin scripts); do
	f=scripts/$file
	[ -x $f ] && verbose $f
done
