# propper aliases

alias u="cd .."
alias uu="cd ../.."
alias uuu="cd ../../.."
alias uuuu="cd ../../../.."

alias l="ls"
alias info='info --vi-keys'
alias ll="cd -"

alias todo=todo.sh

# aliases of questionable quality 
[ -x /usr/bin/perl-rename ] && alias rename="perl-rename"
alias eclim="eclim -command"
alias arnoldc="java -jar ~/Downloads/otherPackages/arnoldC/ArnoldC.jar -declaim"

# joke aliases
alias kitty="cat"
alias nano="echo \"Seriously? Why don't you just use Notepade.exe? Or MS Paint?\""

# enable colors if available
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
fi

sl() {
	$(which sl) $*
	ls $*
}
alias tmp="vim +'setlocal buftype=nofile' -n"
# Sets the TERM to something more widely recongnised, when needed
ssh() {
	[ $TERM == "xterm-termite" ] && export TERM=xterm
	$(which ssh) $*
}
# irc host is supposed to be set in ~/.ssh/config
alias irc="ssh irc -t screen -x"

# addes lesspipe to the less command
# allows for a wider range of formats to be
# opened with less
#if [ $(hostname) == "arch2012" ]; then
#	LESSOPEN="|lesspipe.sh %s"; export LESSOPEN
#else
#	export LESSOPEN="| /usr/bin/lesspipe %s";
#	export LESSCLOSE="/usr/bin/lesspipe %s %s";
#fi
LESSOPEN="|lesspipe.sh %s"; export LESSOPEN
