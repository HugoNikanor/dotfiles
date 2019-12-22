# Installs contents of this directory into user directory.
# Items in config/ are symlinked as items in ~/.config/
# other stuff is symlinked into ~/, but with a dot added beforehand

verbose () {
	set -x
	"$@"
	set +x
}

for file in $(ls \
	-I config \
	-I dosbox* \
	-I installer.sh \
	-I README.md \
	-I mutt \
	-I elinks
	-I scripts)
do
	[ -h $HOME/.$file ] || verbose ln -s $(realpath $file) $HOME/.$file
done

dir=$(realpath config)
pushd $HOME/.config/
for file in $(ls $dir); do
	[ -h $file ] || verbose ln -s $dir/$file
done
popd

pushd scripts
for file in $(ls); do
	[ -x $file ] && verbose ./$file
done
popd

