from flask import request

class EmployeeRoutes:

    def __init__(self, employees):
        self._employees = employees

    def all_employees(self):
        emps = self._employees.all()
        return emps

    def create_employee(self):
        e = self._employees.create(request.form)
        return e.fields()

    def get_employee(self, employee_id):
        emp = self._employees.find_by_id(employee_id)
        return emp.fields()
