# propper aliases

alias u="cd .."
alias uu="cd ../.."
alias uuu="cd ../../.."
alias uuuu="cd ../../../.."

alias l="ls"
alias ll="cd -"

alias tree='tree --gitignore'

# Mostly for Fedora
if command -v vimx >/dev/null; then
	alias vim=vimx
fi

alias tmp="vim +'setlocal buftype=nofile' -n"
alias clip="xclip -selection clipboard"
alias wi="wiki"
alias wip="wi -w public"
alias wig="wi -w private"
alias net="watch -n1 -c nmcli -c yes"
alias nmail="newmail --watch"
alias ip="ip -c"

alias weeslack="weechat -d ~/.local/share/weechat/slack"

vibin() { "$EDITOR" "$(which "$1")"; }

# Creates paste. Uploads file if argument is given, STDIN otherwise.
# pb() { curl -F c="@${1:--}" https://ptpb.pw?u=1; }
pb() { curl -F 'f:1=<-' ix.io; }

# aliases of questionable quality

[ -x /usr/bin/perl-rename ] && alias rename="perl-rename"
alias eclim="eclim -command"
alias arnoldc="java -jar ~/Downloads/otherPackages/arnoldC/ArnoldC.jar -declaim"
alias ecat='elinks -dump'

# joke aliases
alias nano="echo \"Seriously? Why don't you just use Notepade.exe? Or MS Paint?\""

if [ "$(uname -s)" == "Linux" ]; then
	# enable colors if available
	if command -v dircolors >/dev/null; then
		if [ -r ~/.dircolors ]; then
			eval "$(dircolors -b ~/.dircolors)"
		else
			eval "$(dircolors -b)"
		fi
		alias ls='ls -N --color=auto'
		alias grep='grep --color=auto'
	fi
else
	alias ls='ls -G'
fi


sl() {
	"$(which sl)" "$@"
	ls "$@"
}

__ntpq_helper() {
	case $1 in
		-p|--peers) echo peers ;;
		*) ;;
	esac
}

ntpq() {
	if [ -t 1 ]; then
		# output is a tty
		case $(__ntpq_helper $(getopt --quiet --longoptions peers --options p -- "$@")) in
			peers) $(which ntpq) "$@" | sed \
				-e "s/^*/$BLUE*/" \
				-e "s/^+/$GREEN+/" \
				-e "s/^-/$RED-/" \
				-e "s/$/$RESET/"
				;;
			*) $(which ntpq) "$@" ;;
		esac
	else
		$(which ntpq) "$@"
	fi
}
