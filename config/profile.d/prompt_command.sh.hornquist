function __prompt_command() {
 	local EXIT="$?"
# 	read PS1 <<- EOF
# 	$([ $(hostname) != "arch2012" ] && echo "\u@")\h\
# 	$([ $EXIT == 0 ] && echo "${Green}" || echo "${Red}
# 	EOF
# 
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
# export PROMPT_COMMAND=__prompt_command
