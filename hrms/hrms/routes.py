from flask import request
from flask import jsonify

class EmployeeRoutes:

	def __init__(self, employees):
		self._employees = employees

	def all_employees(self):
		emps = self._employees.all()
		return jsonify(emps)

	def create_employee(self):		
		e = self._employees.create(request.form)
		return e.to_json()

	def get_employee(self, employee_id):
		return "get_employee: {}".format(employee_id)











