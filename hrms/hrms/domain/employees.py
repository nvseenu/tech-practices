from .employee import Employee

class Employees:

	def __init__(self):
		pass


	def all(self):
		return []	
	
	def create(self, employee_fields):
		print('employee_fields ::::', employee_fields)
		e = Employee(employee_fields)
		e.save()
		return e


