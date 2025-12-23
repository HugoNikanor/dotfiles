# Used as a source point for most other stuff which wants my
# environment.

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
XDG_STATE_HOME=${XDG_STATE_HOME:-$HOME/.local/state}
XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-/run/user/$(id -u)}

export XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME XDG_STATE_HOME XDG_RUNTIME_DIR

if [ -d "${XDG_CONFIG_HOME}/profile.d/" ]; then
	for file in "${XDG_CONFIG_HOME}/profile.d/"*.sh; do
		test -r "$file" && . "$file"
	done
	for file in "${XDG_CONFIG_HOME}/profile.d/"*.sh."$(uname -s)"; do
		test -r "$file" && . "$file"
	done
	for file in "${XDG_CONFIG_HOME}/profile.d/"*.sh."$(hostname -d)"; do
		test -r "$file" && . "$file"
	done
	for file in "${XDG_CONFIG_HOME}/profile.d/"*.sh."$(uname -n)"; do
		test -r "$file" && . "$file"
	done
	unset file
fi

# vim:ft=sh
