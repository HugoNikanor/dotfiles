[ -z "$DISPLAY" ] && return

systemctl import-environment --user DISPLAY
