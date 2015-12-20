# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

declare arr names=(
	"Boole"
	"Hurd"
	"McCarthy"
	"Pascal"
	"Turing"
	"Knuth"
	"Stallman"
	"Gates"
)

# TODO fix this so that the same exact string is never used twice
# Preferably through some cool contraption, but a simple system for
# appending numbers to the names if need be would also work

#number=$(shuf -i 1-${#names[@]} -n 1)
#name="${names[number]}"

[[ $- != *i* ]] && return
#[[ -z "$TMUX" ]] && exec tmux new -s "$name"
[[ -z "$TMUX" ]] && exec tmux



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

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
# Relace this with a scrint:
# if it's a text file then:
#	less filename
# if't it's a binary then:
# 	lesspipe.sh filename | less

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi


# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi


if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


# Sets vim to some sort of default editor
export VISUAL=/usr/bin/vim
export EDITOR="vi -e"
export BROWSER="tmux new-window -d /usr/bin/elinks"

function __prompt_command() {
	local EXIT="$?"
	PS1="\u@\h "


	# Make the brackets '[]' around the path green if the 
	#last command was a success, otherwise, make them red
	if [ $EXIT != 0 ]; then
		PS1+="${Red}[${Normal}"
	else
		PS1+="${Green}[${Normal}"
	fi

	PS1+="\[${BWhite}\]\w\[${BWhite}\]"

	if [ $EXIT != 0 ]; then
		PS1+="${Red}]${Normal}"
	else
		PS1+="${Green}]${Normal}"
	fi


	PS1+="\n\$ "
}

### Sets the prompt string
if [ $(hostname) == "HPlinux" ]; then
	if [ "$color_prompt" = yes ]; then
		PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
	else
		PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '

	fi
	unset color_prompt force_color_prompt

	# If this is an xterm set the title to user@host:dir
	case "$TERM" in
	xterm*|rxvt*)
		PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
		;;
	*)
		;;
	esac
elif [ $(hostname) == "arch2012" ]; then
	#PS1="\u@\h [\[${bold}\]\w\[${normal}\]]\n\$ "
	export PROMPT_COMMAND=__prompt_command
fi


if [ -d "$HOME/bin" ]; then
	PATH="$HOME/bin:$PATH"
fi




#if [ $(hostname) == "HPlinux" ]; then
#	if [[ ! $TERM =~ screen ]]; then
#		exec tmux
#	fi
#fi
#number=$(shuf -i 1-${#names[@]} -n 1)
#name="${names[number]}"
#tmux new -s "$name"
#exec tmux

#if [ $(hostname) == "arch2012" ]; then
#fi

eval $(thefuck --alias)


#find ~/tmp/* -type d -ctime +5 -exec /bin/rm -rf {} \;
#find ~/Trash/* -type d -ctime +5 -exec /bin/rm -rf {} \;

