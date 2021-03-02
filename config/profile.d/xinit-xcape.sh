[ -z "$DISPLAY" ] && return
[ -n "$SSH_CLIENT" ] && return

command -v xcape > /dev/null || return

xcape -e "Shift_L=parenleft;Shift_R=parenright"
