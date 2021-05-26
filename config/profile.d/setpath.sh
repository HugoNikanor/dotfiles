add () {
	test -d "$1" && PATH="$1:$PATH"
}

add ~/.local/bin
add ~/bin
add ~/.local/$(uname -s)/bin
add ~/.gem/ruby/*/bin
add ~/.cabal/bin
add ~/go/bin

unset add
