resources=$HOME/.Xresources
test -f $resources && xrdb -merge $resources

unclutter &
dex /usr/share/applications/polkit-gnome-authentication-agent-1.desktop &

# export BROWSER=$(which qutebrowser)
# xbindkeys
exec ${WM:-i3}
