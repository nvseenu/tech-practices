import json
import os


class FileDataSource:
    """
    It creates and maintains collection objects which can load and store given object into / from file system
    """

    def __init__(self, data_dir):
        self._data_dir = data_dir
        self._collections = {}
        print('data_dir = ', data_dir)
        if not os.path.exists(self._data_dir):
            os.makedirs(self._data_dir)

    def collection(self, name):
        if name not in self._collections:
            self._collections[name] = FileDataCollection(self._data_dir, name)

        return self._collections[name]

    def close(self):
        pass


class FileDataCollection:

    def __init__(self, parent_path, name):
        self._name = name
        self._path = os.path.join(parent_path, name)
        if not os.path.exists(self._path):
            os.makedirs(self._path)

    def store(self, fields):
        if not fields['id']:
            raise ValueError('id field of given object is empty')

        with open(os.path.join(self._path, self._file_name(fields['id'])), 'w') as f:
            f.write(json.dumps(fields))

    def load(self, id):
        if not id:
            raise ValueError('Given id field is empty')

        with open(os.path.join(self._path, self._file_name(id)), 'r') as f:
            return json.loads(f.read())

    def load_all(self):
        return [self.load(id) for id in self.ids()]

    def ids(self):
        files = os.listdir(self._path)
        return [f.replace('.json', '') for f in files]

    def _file_name(self, id):
        return str(id) + '.json'
