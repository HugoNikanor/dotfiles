test -r /etc/profile.d/bash_completion.sh && . /etc/profile.d/bash_completion.sh

# source /usr/share/bash-completion/completions/pass

COMPLETION_CACHE="${CACHE_DIR}/bash-completion"
mkdir -p "$COMPLETION_CACHE"

# Takes a command line, which should produce completion rules for
# bash. Tries to source it from the cache, or if that fails, runs the
# command line and caches the result.
#
# The main speedup is that we don't have to load binaries from disk on
# each bash startup (which can be quite slow for large binaries like
# pandoc)
#
# The first token in the command line is used as the cache key.
#
# The command line will be re-tokenized, so avoid any fancy stuff.
cache_completion() {
	cmd="$1"
	cmdline="$*"
	. "${COMPLETION_CACHE}/${cmd}.sh" 2>/dev/null \
		|| {
		if command -v "$cmd" >/dev/null; then
			$cmdline > "$COMPLETION_CACHE/${cmd}.sh"
		fi
		. "${COMPLETION_CACHE}/${cmd}.sh"
	}
}

if [ -d "${CONFIG_DIR}/bash/completion/" ]; then
	for file in "${CONFIG_DIR}/bash/completion/"*; do
		test -r "$file" && . "$file"
	done
	unset file
fi
