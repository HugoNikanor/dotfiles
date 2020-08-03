[ -z "$DISPLAY" ] && return

command -v xcape > /dev/null || return

xcape -e "Shift_L=parenleft;Shift_R=parenright"
