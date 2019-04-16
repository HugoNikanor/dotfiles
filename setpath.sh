#
# Helper script for setting environment variables, which should be set
# globaly for my user.
# Loaded by both .xsessionrc, as well as .bashrc.
# Note that it must be sh compatible, to work with xsessionrc.
#

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

export PATH
export HAVE_SET_PATH=${PP:- 1}
