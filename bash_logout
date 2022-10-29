# kill color on logout
echo goodbye
case $TERM in
	linux) echo -en '\e]P0000000'; clear ;;
	*)     echo -en '\e]11;#000000\a' ;;
esac
