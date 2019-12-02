# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PS1='\h [\w] \$ '

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

true ${BROWSER:="elinks"}
export EDITOR="/usr/bin/vim"
export RTV_EDITOR="/usr/bin/vim"
export RTV_URLVIEWER="/usr/bin/urlview"
export INFO_PRINT_COMMAND=">/tmp/info"
export GUILE_LOAD_PATH="/home/hugo/lib/guile"
export XDG_DOCUMENTS_DIR="$HOME/ldoc"
export XDG_DOWNLOAD_DIR="$HOME/down/other"
export XDG_PICTURES_DIR="$HOME/pic"
export LESSOPEN="|lesspipe.sh %s"
export SSH_AUTH_SOCK=/run/user/$(id -u)/ssh-agent.socket
# info path temporarly disabled due to emacs
#export INFOPATH="$HOME/info"
#export INFOPATH="$HOME/info:/usr/local/share/info/:usr/share/info/"

# FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"

# if [ $TERM == "xterm-termite" ]; then
# 	source /etc/profile.d/vte*sh
# 	export TERM=xterm
# fi
