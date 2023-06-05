# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='\h [\w] \$ '

CACHE_DIR=${XDG_CACHE_HOME:-$HOME/.cache}
CONFIG_DIR=${XDG_CONFIG_HOME:-$HOME/.config}

if [ -d "${CONFIG_DIR}/bash/" ]; then
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
	case $TERM in
		linux) type=linux ;;
		*)     type=osc   ;;
	esac

	cat "${CACHE_DIR}/colors/$type/$(hostname)" 2>/dev/null
	unset type
fi

# PS1='$(printf "%%%$((`tput cols` - 2))s\r")'"$PS1"
