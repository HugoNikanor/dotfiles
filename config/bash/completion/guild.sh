
_guild() {
	local cur prev
	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}


	case "$prev" in
		guild|help)
			COMPREPLY=( $(guile -c '((@ (scripts list) list-scripts))' | grep "^${cur}") )
			;;
		*)
			;;
	esac
}

complete -F _guild -f guild

