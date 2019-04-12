[ -z $HAVE_SET_PATH ] || return

for path in \
	"$HOME/.local/bin" \
	"$HOME/bin" \
	"$HOME/.gem/ruby/2.3.0/bin" \
	"$HOME/.cabal/bin"
do
	if [ -d $path ]; then
		PATH="$path:$PATH"
	fi
done

export HAVE_SET_PATH=1
