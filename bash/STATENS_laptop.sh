case `tty` in
	/dev/tty1)
		tmux ls || exec tmux && exec tmux attach
		#[ -z $TMUX ] && exec tmux attach #-t tty1
		;;
	/dev/tty2)
		exec ssh -t lys "tmux a -t lys || tmux" ;;
	/dev/tty3)
		exec ssh -t hornquist "tmux a || tmux" ;;
	/dev/tty6)
		startx ;;
esac

function __prompt_command() {
	PS1="`bat -s` [\w] \$ "
}

PS1='[\w] \$ '
