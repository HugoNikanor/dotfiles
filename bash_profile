if [ -d "$HOME/.config/profile.d/" ]; then
	for file in $HOME/.config/profile.d/*.sh; do
		test -r "$file" && . "$file"
	done
	for file in $HOME/.config/profile.d/*.sh.$(hostname --domain); do
		test -r "$file" && . "$file"
	done
	for file in $HOME/.config/profile.d/*.sh.$(hostname); do
		test -r "$file" && . "$file"
	done
	unset file
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc
