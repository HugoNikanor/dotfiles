
alias arnoldc="java -jar ~/Downloads/otherPackages/arnoldC/ArnoldC.jar -declaim"
#lias asdf="setxkbmap se custom compose:ralt"
alias u="cd .."
alias uu="cd ../.."
alias uuu="cd ../../.."
alias ll="cd $OLDPWD";
alias l="ls"
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=always'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias kitty="cat"
alias nano="echo \"NO!\""
alias time="echo \"you want 'date'\"; time"


# addes lesspipe to the less command
# allows for a wider range of formats to be
# opened with less
if [ $(hostname) == "arch2012" ]; then
	LESSOPEN="|lesspipe.sh %s"; export LESSOPEN
else
	export LESSOPEN="| /usr/bin/lesspipe %s";
	export LESSCLOSE="/usr/bin/lesspipe %s %s";
fi

trash() {
	if [ $# -eq 1 ]; then
		mv $1 ~/Trash/
	else
		rm $*
	fi
}

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

tmp() {
	vim ~/tmp/tempFile_`date +%Y-%m-%d_%H:%M:%S`
}

sl() {
	$(which sl) $*
	ls $*
}

# close current terminal window if it's an tmux session
#texit() {
#	~/.kill_window
#}

# Note that this only uploads without an account
# TODO check how to upload to an account
imgur() {
    for i in "$@"; do
        curl -# -F "image"=@"$i" -F "key"="4907fcd89e761c6b07eeb8292d5a9b2a" imgur.com/api/upload.xml|\
        grep -Eo '<[a-z_]+>http[^<]+'|sed 's/^<.\|_./\U&/g;s/_/ /;s/<\(.*\)>/\x1B[0;34m\1:\x1B[0m /'
    done
}
