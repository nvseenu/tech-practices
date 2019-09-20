import json


class Employee:

    def __init__(self, collection, fields):
        if 'id' not in fields:
            raise ValueError('id field is not found')

        self._collection = collection
        self._fields = fields

    def id(self):
        return self._fields['id']

    def save(self):
        self._collection.store(self._fields)

    def fields(self):
        return self._fields
