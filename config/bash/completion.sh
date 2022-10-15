test -r /etc/profile.d/bash_completion.sh && . /etc/profile.d/bash_completion.sh

# source /usr/share/bash-completion/completions/pass

if [ -d "$HOME/.config/bash/completion/" ]; then
	for file in $HOME/.config/bash/completion/*; do
		test -r "$file" && . "$file"
	done
	unset file
fi
