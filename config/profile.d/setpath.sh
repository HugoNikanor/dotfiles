add () {
	test -d "$1" && PATH="$1:$PATH"
}

add ~/.local/bin
add ~/bin
add ~/.gem/ruby/*/bin
add ~/.cabal/bin

unset add
