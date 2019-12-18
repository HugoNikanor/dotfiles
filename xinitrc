#!/bin/bash

resources=$HOME/.Xresources
test -f $resources && xrdb -merge $resources

compton &
termite &
feh --bg-fill https://wrya.net/services/wallpaper/4chan.php &

BROWSER=xdg-open
export BROWSER

# We have this last to allow overrides to the above specified
# defaults.
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

# Final fallback for WM, and start it.
export WM=${WM:-xmonad}
exec $WM
