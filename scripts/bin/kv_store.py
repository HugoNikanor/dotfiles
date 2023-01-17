import os
import os.path
import json

class KVStore:

    def __init__(self, store):
        self.store = store
        try:
            os.mkdir(self.store, mode=0o700)
        except FileExistsError:
            pass

    def get(self, key):
        with open(os.path.join(self.store, key)) as f:
            return json.load(f)

    def put(self, key, value):
        with open(os.path.join(self.store, key), 'w') as f:
            json.dump(value, f)

    def mtime(self, key):
        return os.stat(os.path.join(self.store, key)).st_mtime
