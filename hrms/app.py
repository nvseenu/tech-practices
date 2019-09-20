from hrms.datasource import FileDataSource
from hrms.employee.domain import Employees
from hrms.employee.routes import EmployeeRoutes
from flask import Flask
import sys
import os

parent_dir = os.path.dirname(__file__)
sys.path.append(os.path.join(parent_dir, 'hrms'))

file_data_source = FileDataSource(data_dir=os.path.join(parent_dir, 'data'))
employees = Employees(file_data_source.collection('employees'))
employee_routes = EmployeeRoutes(employees)

# Register all urls
app = Flask(__name__)
app.add_url_rule('/api/employees', view_func=employee_routes.all_employees)
app.add_url_rule('/api/employees', view_func=employee_routes.create_employee, methods=['POST'])
app.add_url_rule('/api/employees/<employee_id>', view_func=employee_routes.get_employee)
