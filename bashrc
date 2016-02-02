# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return


# start tmux if tmux isn't runnig
[[ $- != *i* ]] && return
#if [ -z "$TMUX" ] && [ "$(hostname)" != "HPlinux" ]; then
if [ -z "$TMUX" ]; then
	source .start_tmux
fi

desktopEnv=$(wmctrl -m | tr '\n' ' ' | sed 's/^Name:\s\([^ ]*\).*/\1/g' &> /dev/null)

# this should possibly have some sort of check if the system is running
# by itself or if it's controlled via ssh or the like
if [ -n "$TMUX" ] && [ "$desktopEnv" == "xmonad" ]; then
	sname=$(tmux display-message -p '#S')
	tmux set-option set-titles-string "$sname"
fi




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
if [ -n $TMUX ]; then
	export BROWSER="tmux new-window -n elinks(auto) $(which elinks)"
else
	export BROWSER="$(which elinks)"
fi

function __prompt_command() {
	local EXIT="$?"
	if [ $(hostname) != "arch2012" ]; then
		PS1="\u "
	else
		PS1="\u@\h "
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

if [ -d "$HOME/bin" ]; then
	PATH="$HOME/bin:$PATH"
fi
if [ -d "$HOME/.gem/ruby/2.3.0/bin" ]; then
	PATH="$HOME/.gem/ruby/2.3.0/bin:$PATH"
fi

# eat the error message if 'food' doesn't exist.
# is it even possible to write something other than hacks in bash?
fooddata="$(food 2> /dev/null)"
if [ $? == 0 ]; then
	echo "$fooddata"
fi
