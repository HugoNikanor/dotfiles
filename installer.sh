# Installs contents of this directory into user directory.
# Items in config/ are symlinked as items in ~/.config/
# Items in bash/ do their own thing
# other stuff is symlinked into ~/, but with a dot added beforehand

function contains() {
	list=$1
	item=$2

	echo $list | grep $item
}

set -x xtrace

ignore_list="config,bash,README.md,installer.sh"

for file in $(ls); do
	contains $ignore_list $file && continue

	ln -s $(realpath $file) $HOME/.$file
done

CPWD=$PWD

dir=$(realpath config)
for file in $(ls config/); do
	cd $HOME/.config/ && ln -s $dir/$file
done

cd $CPWD

for b in bash{rc,_completion}; do
	ln -s $(realpath bash/$b) $HOME/.$b
done
