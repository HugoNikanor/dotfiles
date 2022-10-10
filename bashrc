# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='\h [\w] \$ '

if [ -d "$HOME/.config/bash/" ]; then
	for file in $HOME/.config/bash/*.sh; do
		test -r "$file" && . "$file"
	done
	unset file
fi

shopt -s checkwinsize
shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000

test "$BROWSER" = elinks || export BROWSER=elinks
export EDITOR="/usr/bin/vim"
export RTV_EDITOR="/usr/bin/vim"
export RTV_URLVIEWER="/usr/bin/urlview"
export INFO_PRINT_COMMAND=">/tmp/info"
export XDG_DOCUMENTS_DIR="$HOME/ldoc"
export XDG_DOWNLOAD_DIR="$HOME/down/other"
export XDG_PICTURES_DIR="$HOME/pic"

command -v lesspipe >/dev/null && eval "$(lesspipe)"
command -v direnv   >/dev/null && eval "$(direnv hook bash)"

# FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"

if [ "$TERM" == xterm-termite ]; then
	test -f /etc/profile.d/vte*.sh && . /etc/profile.d/vte*.sh
# 	export TERM=xterm
fi

if [ -n "$SSH_CLIENT" ]; then
	# https://bottosson.github.io/misc/colorpicker/#335e73
	case $(hostname) in
		gandalf)   color='335e73';;
		hornquist) color='5e1212';;
		*)         color='000000';;
	esac
	case $TERM in
		# https://man7.org/linux/man-pages/man4/console_codes.4.html
		# console_codes(4)
		linux) echo -en "\e]P0${color}"; clear ;;
		# OSC (Operating system command)
		# https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-Operating-System-Commands
		*)     echo -en "\e]11;#${color}\e\\" ;;
	esac
	unset color
fi

# PS1='$(printf "%%%$((`tput cols` - 2))s\r")'"$PS1"
