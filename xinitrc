#!/bin/bash

resources=$HOME/.Xresources
test -f $resources && xrdb -merge $resources

case $(hostname) in
	STATENSlaptop)
		xbindkeys
		;;
	saruman)
		unclutter &
		dex /usr/share/applications/polkit-gnome-authentication-agent-1.desktop &
		;;
	gandalf)
		xcape -e "Shift_L=parenleft;Shift_R=parenright"
		;;
esac

compton &
termite &
feh --bg-fill https://wrya.net/services/wallpaper/4chan.php

export WM=${WM:-xmonad}
exec $WM
