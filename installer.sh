# Installs contents of this directory into user directory.
# Items in config/ are symlinked as items in ~/.config/
# other stuff is symlinked into ~/, but with a dot added beforehand

function contains() {
	list=$1
	item=$2

	echo $list | grep $item
}

set -x

ignore_list="config,README.md,installer.sh"

for file in $(ls); do
	contains $ignore_list $file && continue

	ln -s $(realpath $file) $HOME/.$file
done

pushd

dir=$(realpath config)
for file in $(ls config/); do
	cd $HOME/.config/ && ln -s $dir/$file
done

popd
