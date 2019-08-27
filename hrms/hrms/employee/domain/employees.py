from .employee import Employee
import json


class Employees:

    def __init__(self, collection):
        self._collection = collection

    def all(self):
        emps = self._collection.load_all()
        return json.dumps(emps)

    def create(self, fields):
        e = Employee(self._collection, fields)
        e.save()
        return e

    def find_by_id(self, id):
        return Employee(self._collection, self._collection.load(id))
