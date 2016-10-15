# propper aliases
alias u="cd .."
alias uu="cd ../.."
alias uuu="cd ../../.."
alias uuuu="cd ../../../.."
alias l="ls"
alias info='info --vi-keys'
alias ll="cd -"
alias x="chmod +x"

# aliases of questionable quality 
alias rename="perl-rename"
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
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

sl() {
	$(which sl) $*
	ls $*
}
tmp() {
	vim /tmp/tempFile_`date +%Y-%m-%dT%H:%M:%S`
}

# addes lesspipe to the less command
# allows for a wider range of formats to be
# opened with less
if [ $(hostname) == "arch2012" ]; then
	LESSOPEN="|lesspipe.sh %s"; export LESSOPEN
else
	export LESSOPEN="| /usr/bin/lesspipe %s";
	export LESSCLOSE="/usr/bin/lesspipe %s %s";
fi

extract () {
	if [ -f $1 ] ; then
		case $1 in
			*.tar.bz2)   tar xvjf $1    ;;
			*.tar.gz)    tar xvzf $1    ;;
			*.bz2)       bunzip2 $1     ;;
			*.rar)       unrar x $1     ;;
			*.gz)        gunzip $1      ;;
			*.tar)       tar xvf $1     ;;
			*.tbz2)      tar xvjf $1    ;;
			*.tgz)       tar xvzf $1    ;;
			*.zip)       unzip $1       ;;
			*.Z)         uncompress $1  ;;
			*.7z)        7z x $1        ;;
			*)           echo "don't know how to extract '$1'..." ;;
		esac
	else
		echo "'$1' is not a valid file!"
	fi
}

