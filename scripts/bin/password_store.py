import os
import os.path
import subprocess
import json


class PasswordStore:
    """
    Wrapper around pass(1).

    [Parameters]:
        passbase -- Prefix which should be added to all keys.
        password_store -- Alternative storage location of the store.
                          Defaults to PASSWORD_STORE.
    """

    def __init__(self, password_store, passbase='automation/'):
        self.passbase = passbase

        self.password_store = password_store

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
