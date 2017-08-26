#!/bin/sh

# System config mostly for when starting x without a display manager.

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# This is probably not the best way to do it
if [ "$(hostname)" == "arch2012" ] ; then 
	xset -dpms
	setterm -blank 0 -powerdown 0
	xset s off
	xrdb -merge ~/.Xresources
fi

#setxkbmap se planck compose:caps

# Uncomment this line to disable key repeats
#xset r off

# This forces the mouse to hide itself after not having been moved for a while.
unclutter   &
# transparent windows
compton     &

~/.fehbg &

export WM=xmonad

case "$WM" in
	i3)
		xrandr --noprimary &
		;;
	xmonad)
		#thunderbird &
		;;
esac

exec $WM
