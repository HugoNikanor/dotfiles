#!/usr/bin/env python3

"""
Get XOAUTH2 Token.

Procedure for going through Microsofts authentication dance to allow
IMAP access to email.
"""

#from http.server import HTTPServer, BaseHTTPRequestHandler
import base64
import http.client
import json
import os
import queue
import subprocess
import sys
import threading
import time
import urllib
import urllib.parse
from urllib.parse import urlparse
from selenium import webdriver
from selenium.webdriver.firefox.options import Options as FirefoxOptions

# Om mbsync sen matar ur sig
# > IMAP command 'NAMESPACE' returned an error: BAD User is authenticated but not connected.
# betyder det antagligen att authentiserats med fel ckonto. (Konto A
# förväntades, men du har blivit inloggad som Konto B)


# Can also be set to 'common'
TENANT = '913f18ec-7f26-4c5f-a816-784fe9a58edd'

AUTHURL = f"https://login.microsoftonline.com/{TENANT}/oauth2/v2.0/authorize"
# AUTHURL = f"http://localhost:8090"
CLIENT_ID = '6731de76-14a6-49ae-97bc-6eba6914391e'

TOKENURL = f"https://login.microsoftonline.com/{TENANT}/oauth2/v2.0/token"
# TOKENURL = f"http://localhost:8090"
SECRET = 'JqQX2PNo9bpM0uEihUPzyrh'

# https://docs.microsoft.com/en-gb/exchange/client-developer/legacy-protocols/how-to-authenticate-an-imap-pop-smtp-application-by-using-oauth
offline_access = 'offline_access'
imap = 'https://outlook.office.com/IMAP.AccessAsUser.All'
smtp = 'https://outlook.office.com/SMTP.Send'
SCOPE = ' '.join([offline_access, imap, smtp])

HOME = os.getenv('HOME') or '/tmp'

# export PASSWORD_STORE=${PASSWORD_STORE:-$HOME/.password-store}
# PASSBASE=automation/


class PasswordStore:
    """
    Wrapper around pass(1).

    [Parameters]:
        passbase -- Prefix which should be added to all keys.
        password_store -- Alternative storage location of the store.
                          Defaults to PASSWORD_STORE.
    """

    def __init__(self, passbase='automation/', password_store=None):
        self.passbase = passbase

        if password_store:
            self.password_store = password_store
        else:
            self.password_store = os.getenv('PASSWORD_STORE') \
                or os.path.join(HOME, '.password-store')

    def get(self, key):
        """
        Get value from password store.

        TODO what happens if key isn't present?
        """
        cmd = subprocess.run(['pass', os.path.join(self.passbase, key)],
                             capture_output=True)
        return cmd.stdout

    def put(self, key, value):
        """Store value in password store."""
        if isinstance(value, dict):
            value = json.dumps(value)
        if isinstance(value, str):
            value = value.encode('UTF-8')
        subprocess.run(['pass', 'insert', '--multiline', '--force',
                        os.path.join(self.passbase, key)],
                       input=value)

    def mtime(self, key):
        """
        Return mtime of item in password store.

        TODO what happens if key isn't present?
        """
        path = os.path.join(self.password_store, self.passbase, key + '.gpg')
        st = os.stat(path)
        return st.st_mtime


def write_xoauth2_token(user, token):
    """
    Output XOAUTH2 token.

    This is generally done by sasl.
    """
    msg = b''.join([b'user=', user.encode('UTF-8'), b'',
                    b'auth=Bearer ', token.encode('UTF-8'), b''])
    base64.encode(msg)
    print(msg)


def new_token(token_name):
    """Retrieve a new token."""
    print(f"Getting brand new token for '{token_name}'", file=sys.stderr)

    redir_uri = f"http%3A//localhost/myapp/"

    options = FirefoxOptions()
    options.add_argument('--safe-mode')
    browser = webdriver.Firefox(options=options)

    # Asks the user to log in in i browser window
    prompt_user_login(browser, redir_uri)
    # authentication_code = q.get(block=True)
    url = urlparse(browser.current_url)
    while url.netloc != 'localhost':
        time.sleep(0.1)
        url = urlparse(browser.current_url)
    authentication_code = urllib.parse.parse_qs(url.query)['code'][0]

    token = get_token(authentication_code, redir_uri)

    browser.quit()

    return token


def prompt_user_login(browser, redir_uri):
    """Open uri in a graphical browser."""
    params_dict = {
        'client_id': CLIENT_ID,
        'response_type': 'code',
        'redirect_uri': redir_uri,
        'response_mode': 'query',
        'scope': SCOPE,
    }
    params = '&'.join(f'{key}={value}' for key, value in params_dict.items())
    url = AUTHURL + '?' + params
    browser.get(url)

    # TODO
    # Auto fill username (and optionally password).
    # Setting the value directly doesn't seem to work, probably due to
    # Microsofts interesting ideas about how to build pages.
    # Simulating button presses is probably better.
    # browser.execute_script('document.querySelector(\'input[type="email"]\').value = "name@example.com"')


def get_token(code, redir_uri):
    """Last step of authentication."""
    parts = urllib.parse.urlparse(TOKENURL)
    conn = http.client.HTTPSConnection(parts.netloc)
    params_dict = {
        'client_id': CLIENT_ID,
        'code': code,
        'scope': SCOPE,
        'redirect_uri': redir_uri,
        'grant_type': 'authorization_code',
        'client_secret': SECRET,
    }
    params = '&'.join(f'{key}={value}' for key, value in params_dict.items())
    conn.request('POST', parts.path, params, {})
    response = conn.getresponse()
    data = response.read()
    conn.close()
    return json.loads(data)


def refresh_refresh_token(refresh_token, token_name):
    """Refresh our a refresh token."""
    parts = urllib.parse.urlparse(TOKENURL)
    conn = http.client.HTTPSConnection(parts.netloc)
    params = urllib.parse.urlencode({
        'client_id': CLIENT_ID,
        'scope': SCOPE,
        'refresh_token': refresh_token,
        'grant_type': 'refresh_token',
        'client_secret': SECRET,
    })
    conn.request('POST', parts.path, params, {})
    response = conn.getresponse()
    data = response.read()
    conn.close()
    return json.loads(data)


def __main(token_name):

    pw_store = PasswordStore()

    try:
        p = pw_store.get(token_name)
        data = json.loads(p)
        refresh_token = data['refresh_token']
        if not refresh_token:
            token = new_token(token_name)
            if token.get('error'):
                print(token['error'])
                print(token['error_description'])
            else:
                pw_store.put(token_name, token)
        else:
            elapsed = time.time() - pw_store.mtime(token_name)
            if elapsed > 3599:
                print(f"Refreshing token '{token_name}'", file=sys.stderr)
                token = refresh_refresh_token(refresh_token, token_name)
                if token.get('error'):
                    print(token['error'])
                    print(token['error_description'])
                else:
                    pw_store.put(token_name, token)
            else:
                print(f"token '{token_name}' still good", file=sys.stderr)
    except Exception:
        token = new_token(token_name)
        if token.get('error'):
            print(token['error'])
            print(token['error_description'])
        else:
            pw_store.put(token_name, token)

    print(json.loads(pw_store.get(token_name))['access_token'])


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Missing token name!', file=sys.stderr)
        sys.exit(1)
    token_name = sys.argv[1]
    if not token_name:
        print('Missing token name!', file=sys.stderr)
        sys.exit(1)
    __main(token_name)
