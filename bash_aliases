
# serius aliases
alias arnoldc="java -jar ~/Downloads/otherPackages/arnoldC/ArnoldC.jar -declaim"
alias pm-suspend="sudo pm-suspend"
alias pm-hibernate="sudo pm-hibernate"
alias asdf="setxkbmap se custom compose:ralt"
alias u="cd .."
alias uu="cd ../.."
alias uuu="cd ../../.."

alias l="ls"
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias ed="ex"
alias time="echo \"you want 'date'\"; time"


# addes lesspipe to the less command
# allows for a wider range of formats to be
# opened with less
LESSOPEN="|lesspipe.sh %s"; export LESSOPEN


# Mount my windows drive with the correct settings, using the same command 
# that I use for mounting ext4 partitions. This should be updated to a general
# NTFS mount command
sudo() {
	if [[ $@ == "mount /dev/sdb2 /mnt/winStorage" ]]; then
		command sudo mount -t ntfs -o umask=000 /dev/sdb2 /mnt/winStorage
	else
		command sudo "$@"
	fi
}

# 'rm' is now soft remove that moves removed files to trash directory
# 'rm' with multiple arguments is regular rm
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
	vim ~/tmp/tempFile_`date +%Y-%m%d_%H:%M:%S`
}

texit() {
	cur=$(tmux display-message -p '#S')
	tmux kill-session -t "$cur"
}

# Note that this only uploads without an account
# TODO check how to upload to an account
imgur() {
    for i in "$@"; do
        curl -# -F "image"=@"$i" -F "key"="4907fcd89e761c6b07eeb8292d5a9b2a" imgur.com/api/upload.xml|\
        grep -Eo '<[a-z_]+>http[^<]+'|sed 's/^<.\|_./\U&/g;s/_/ /;s/<\(.*\)>/\x1B[0;34m\1:\x1B[0m /'
    done
}
