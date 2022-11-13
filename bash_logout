# kill color on logout
case $TERM in
	linux) type=linux ;;
	*)     type=osc   ;;
esac

cat ${XDG_CACHE_HOME:-~/.cache/}"/colors/$type/regular" 2>/dev/null

unset type
