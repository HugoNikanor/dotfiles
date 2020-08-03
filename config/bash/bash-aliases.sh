# propper aliases

alias u="cd .."
alias uu="cd ../.."
alias uuu="cd ../../.."
alias uuuu="cd ../../../.."

alias l="ls"
alias ll="cd -"

alias tmp="vim +'setlocal buftype=nofile' -n"
alias clip="xclip -selection clipboard"
alias wi="wiki"
alias wip="wi -w public"
alias wig="wi -w private"
alias net="watch -n1 -c nmcli -c yes"
alias nmail="watch -n1 -c newmail"

alias weeslack="weechat -d ~/.local/share/weechat/slack"


vibin() { $EDITOR $(which $1); }

# Creates paste. Uploads file if argument is given, STDIN otherwise.
# pb() { curl -F c="@${1:--}" https://ptpb.pw?u=1; }
pb() { curl -F 'f:1=<-' ix.io; }

# aliases of questionable quality 

[ -x /usr/bin/perl-rename ] && alias rename="perl-rename"
alias eclim="eclim -command"
alias arnoldc="java -jar ~/Downloads/otherPackages/arnoldC/ArnoldC.jar -declaim"

# joke aliases
alias kitty="cat"
alias nano="echo \"Seriously? Why don't you just use Notepade.exe? Or MS Paint?\""

if [ $(uname -s) == "Linux" ]; then 
	# enable colors if available
	if [ -x /usr/bin/dircolors ]; then
		test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
		alias ls='ls -N --color=auto'
		alias grep='grep --color=auto'
	fi
else
	alias ls='ls -G'
fi


sl() {
	$(which sl) $*
	ls $*
}
