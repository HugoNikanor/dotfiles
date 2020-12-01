test -r /etc/profile.d/bash_completion.sh && . /etc/profile.d/bash_completion.sh

# Asks vim which tags it can find
_vim_search() {
	ex -N -u NONE -i NONE -c 'let &tags="'$2'"' -c 'echo "\\n"' -c 'for tag in taglist("^".escape("'$1'","."))|echo tag["name"]|endfor' -cq \
		| tr -s '\r' '\n' \
		| sed -n '/^[a-zA-Z_]/p'
}

# Overrides the usual compleations for vim with ctag completions
# if the -t flag is given.
# If no -t flag is given then fall through to the default completionse
_vim() {
	local cur prev

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}

	case "${prev}" in
		-t)
			local tagsdir=$PWD
			while [[ "$tagsdir" && ! -f "$tagsdir/tags" ]]; do
				tagsdir=${tagsdir%/*}
			done
			[[ -f "$tagsdir/tags" ]] || return

			COMPREPLY=( $(_vim_search "$cur" "$tagsdir/tags" ) )
			return
			;;
		*)
			# Perform usual completion mode
			;;
	esac
}

# Files matching this pattern are excluded
excludelist='*.@(o|O|so|SO|so.!(conf)|SO.!(CONF)|a|A|rpm|RPM|deb|DEB|gif|GIF|jp?(e)g|JP?(E)G|mp3|MP3|mp?(e)g|MP?(E)G|avi|AVI|asf|ASF|ogg|OGG|class|CLASS)'

complete -F _vim -f -X "${excludelist}" vi vim gvim rvim view rview rgvim rgview gview

# source /usr/share/bash-completion/completions/pass

if [ -d "$HOME/.config/bash/completion/" ]; then
	for file in $HOME/.config/bash/completion/*; do
		test -r "$file" && . "$file"
	done
	unset file
fi
