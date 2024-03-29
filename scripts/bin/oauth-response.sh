#!/bin/bash

# IMAP an access tokens
# https://docs.microsoft.com/en-us/exchange/client-developer/legacy-protocols/how-to-authenticate-an-imap-pop-smtp-application-by-using-oauth#get-an-access-token

# OAuth2 authentication flow
# https://docs.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow#request-an-authorization-code

# Sasl xoauth2 plugin
# https://github.com/moriyoshi/cyrus-sasl-xoauth2

# Can also be set to 'common'
TENANT='913f18ec-7f26-4c5f-a816-784fe9a58edd'

AUTHURL="https://login.microsoftonline.com/$TENANT/oauth2/v2.0/authorize"
CLIENT_ID='6731de76-14a6-49ae-97bc-6eba6914391e'

TOKENURL="https://login.microsoftonline.com/$TENANT/oauth2/v2.0/token"
SECRET='JqQX2PNo9bpM0uEihUPzyrh'

# https://docs.microsoft.com/en-gb/exchange/client-developer/legacy-protocols/how-to-authenticate-an-imap-pop-smtp-application-by-using-oauth
offline_access='offline_access'
imap='https://outlook.office.com/IMAP.AccessAsUser.All'
smtp='https://outlook.office.com/SMTP.Send'
SCOPE="$offline_access%20$imap%20$smtp"
#REDIR_URI='http%3A//localhost/myapp/'

export PASSWORD_STORE=${PASSWORD_STORE:-$HOME/.password-store}
PASSBASE=automation/

get_free_port() {
	for p in {8080..9000}; do
		# A direct if on the echo doen't work correctly, leading to us
		# allocating an already used port
		echo > /dev/tcp/localhost/$p
		if [ $? -ne 0 ]; then
			echo $p
			return
		fi
	done 2>/dev/null
}

PORT=$(get_free_port)

REDIR_URI="http%3A//localhost:$PORT/myapp/"

get_authentication_code() {
	url="$AUTHURL?client_id=$CLIENT_ID&response_type=code&redirect_uri=$REDIR_URI&response_mode=query&scope=$SCOPE"
	# echo $url
	xdg-open $url
}

get_token() {
	code=$1
	curl -X POST "$TOKENURL" \
		--silent \
		--data "client_id=$CLIENT_ID" \
		--data "code=$code" \
		--data "scope=$SCOPE" \
		--data "redirect_uri=$REDIR_URI" \
		--data 'grant_type=authorization_code' \
		--data "client_secret=$SECRET"
}

refresh_refresh_token() {

	# refresh token could be gotten here since we know token name
	refresh_token=$1
	token_name=$2

	curl -X POST "$TOKENURL" \
		--silent \
		--data "client_id=$CLIENT_ID" \
		--data "scope=$SCOPE" \
		--data "refresh_token=$refresh_token" \
		--data 'grant_type=refresh_token' \
		--data "client_secret=$SECRET" \
		| pass insert --multiline --force $PASSBASE/$token_name
}

# openssl s_client -connect outlook.office365.com:993 -crlf
# 1 AUTHENTICATE XOAUTH2 ^^

new_token() {

	token_name=$1

	echo "Getting brand new token for '$token_name'" 1>&2

	tmpfile=$(mktemp)

	nc -l $PORT | head -n1 | grep -o 'code=[^&]*' > $tmpfile &
	pid=$!

	get_authentication_code
	wait $pid
	. $tmpfile
	get_token $code | pass insert --multiline --force $PASSBASE/$token_name

	rm $tmpfile
}

main () {
	token_name=$1

	if pass $PASSBASE/$token_name > /dev/null 2>&1; then
		refresh_token=$(pass $PASSBASE/$token_name | jq -r '.refresh_token')

		if [ "$refresh_token" = 'null' ] || [ "$refresh_token" = '' ]; then
			new_token $token_name
		else
			elapsed=$(( $(date +%s) - $(stat "$PASSWORD_STORE/$PASSBASE/$token_name.gpg" -c '%Y') ))
			# 3599 should be checked from the token file...
			if [ $elapsed -gt 3599 ]; then
				echo "Refreshing token '$token_name'" 1>&2
				refresh_refresh_token $refresh_token $token_name
			else
				echo "token '$token_name' still good" 1>&2
			fi
		fi
	else
		new_token $token_name
	fi
}

# generate xoauth2 token (will be done by sasl)
write_xoauth2_token() {
	user=$1 # probably your email
	token=$2
	# this way of getting the token doesn't really work
	echo -ne "user=$userauth=Bearer $token" | base64 -w0
	echo
}


token_name=$1
shift
if [ "$token_name" = "" ]; then
	echo "Missing token name!" 1>&2
	exit 1
fi
main $token_name |& sed "s/^/$(basename $0): /" 1>&2
token=$(pass $PASSBASE/$token_name | jq -r '.access_token')

case $1 in
	--token)
		shift
		user=$1
		write_xoauth2_token "$user" "$token"
		;;
	*)
		echo $token
		;;
esac


exit 0
