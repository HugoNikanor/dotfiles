#!/bin/bash

resources=$HOME/.Xresources
test -f $resources && xrdb -merge $resources

case "$WM" in
	# Having picom started prevents Plasma from starting its compositor
	startplasma-x11) ;;
	*)
		picom &
		;;
esac

# termite &
# feh --bg-fill https://wrya.net/services/wallpaper/4chan.php &
# xrandr --listmonitors | awk -F' ' '/^Monitors:/ { print  }' 

bgset=$HOME/.local/bin/setbackground
[ -x $bgset ] && $bgset &

BROWSER=xdg-open
export BROWSER

export XCURSOR_THEME=breeze_cursor
export XCURSOR_SIZE=48

# We have this last to allow overrides to the above specified
# defaults.

[[ -f ~/.profile ]] && . ~/.profile

# Final fallback for WM, and start it.
export WM=${WM:-xmonad}
exec $WM

