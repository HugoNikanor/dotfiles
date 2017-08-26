# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Normal Colors
Black='\e[0;30m'        # Black
Red='\e[0;31m'          # Red
Green='\e[0;32m'        # Green
Yellow='\e[0;33m'       # Yellow
Blue='\e[0;34m'         # Blue
Purple='\e[0;35m'       # Purple
Cyan='\e[0;36m'         # Cyan
White='\e[0;37m'        # White

# Bold
BBlack='\e[1;30m'       # Black
BRed='\e[1;31m'         # Red
BGreen='\e[1;32m'       # Green
BYellow='\e[1;33m'      # Yellow
BBlue='\e[1;34m'        # Blue
BPurple='\e[1;35m'      # Purple
BCyan='\e[1;36m'        # Cyan
BWhite='\e[1;37m'       # White

# Background
OnBlack='\e[40m'       # Black
OnRed='\e[41m'         # Red
OnGreen='\e[42m'       # Green
OnYellow='\e[43m'      # Yellow
OnBlue='\e[44m'        # Blue
OnPurple='\e[45m'      # Purple
OnCyan='\e[46m'        # Cyan
OnWhite='\e[47m'       # White

Normal="\e[m"               # Color Reset



# nice bash history settings
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


# Sets vim to some sort of default editor
export VISUAL=/usr/bin/vim
export EDITOR="/usr/bin/vim"

function __prompt_command() {
	local EXIT="$?"
	# I'm only interested in the hostname if I'm not at home
	if [ $(hostname) != "arch2012" ]; then
		PS1="\u@\h "
	else
		PS1="\u "
	fi

	# Make the brackets '[]' around the path green if the 
	#last command was a success, otherwise, make them red
	if [ $EXIT != 0 ]; then
		PS1+="${Red}[${Normal}"
	else
		PS1+="${Green}[${Normal}"
	fi

	#PS1+="\[${BWhite}\]\w\[${BWhite}\]"
	PS1+="\e[1;39m\w"

	if [ $EXIT != 0 ]; then
		PS1+="${Red}]${Normal}"
	else
		PS1+="${Green}]${Normal}"
	fi

	PS1+="\n\$ "
}

#PS1="\u@\h [\[${bold}\]\w\[${normal}\]]\n\$ "
export PROMPT_COMMAND=__prompt_command

if [ -d "$HOME/.gem/ruby/2.3.0/bin" ]; then
	PATH="$HOME/.gem/ruby/2.3.0/bin:$PATH"
fi

if [ -d "$HOME/.cabal/bin" ]; then
	PATH="$PATH:$HOME/.cabal/bin"
fi

if [ -f ~/.ghiToken ]; then
	source ~/.ghiToken
fi

case $TERM in
	xterm-256color)
		alias ls=tyls
		alias cat=tycat
		;;
	xterm-termite)
		TERM=xterm
		;;
esac

case `hostname` in
	arch2012)
		if [ -d "$HOME/bin" ]; then
			PATH="$HOME/bin:$PATH"
		fi
		;;
	*)
		export MPD_HOST=jukebox.lysator.liu.se
esac
