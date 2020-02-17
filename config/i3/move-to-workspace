#!/bin/bash

# moves the i3-wm focus to the workspace which have a name starting with the 
# provided number. Note that only 0-9 is supported

hasNumber="$(sed -n 's/^\([1-9]\|10\)$/\1/p' <<< $1)"

if [ -z "$hasNumber" ]; then
	echo "Please give a number between 1 and 10"
	exit 1
fi

currentOut="$(i3-msg -t get_workspaces | jq -r 'map(select(.focused))[0].output')"

names="$(
	i3-msg -t get_workspaces |\
	jq -r '.[].name'
)"

wanted="$(grep "^$1 $currentOut.*" <<< "$names")"

# create workspace if it doesn't exist 
if [ -z "$wanted" ]; then
	wanted="$1 $currentOut"
fi
#fullwanted="$(i3-msg -t get_workspaces | jq -r 'map(select(.focused))[0].output'):$wanted"
#echo "$fullwanted"

i3-msg "workspace $wanted"

