__last_now=$(date +%s)

__exec_time() {
	__last=$?
	__now=$(date +%s)
	if [ $(( __now - __last_now )) -gt 2 ]; then
		echo "Finished at $(date +%T) ($(( __now - __last_now ))s), exit code ${__last}"
	fi
	__last_now=$__now
}

PROMPT_COMMAND+=(__exec_time)
