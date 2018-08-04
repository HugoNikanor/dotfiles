#!/bin/bash

resources=$HOME/.Xresources
test -f $resources && xrdb -merge $resources

case $(hostname) in
	STATENS_laptop)
		export BROWSER=$(which qutebrowser)
		xbindkeys
		;;
	saruman)
		unclutter &
		dex /usr/share/applications/polkit-gnome-authentication-agent-1.desktop &
		;;
esac

exec ${WM:-i3}
