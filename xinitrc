#!/bin/bash

resources=$HOME/.Xresources
test -f $resources && xrdb -merge $resources

picom &
termite &
# feh --bg-fill https://wrya.net/services/wallpaper/4chan.php &
# xrandr --listmonitors | awk -F' ' '/^Monitors:/ { print  }' 

bgset=$HOME/.local/bin/setbackground
[ -x $bgset ] && $bgset &

BROWSER=xdg-open
export BROWSER

# We have this last to allow overrides to the above specified
# defaults.

[[ -f ~/.profile ]] && . ~/.profile

# Final fallback for WM, and start it.
export WM=${WM:-xmonad}
exec $WM

