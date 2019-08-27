import sys
import os

parent_dir = os.path.dirname(__file__)
sys.path.append(os.path.join(parent_dir, 'hrms'))
print(sys.path)

from flask import Flask
from hrms.routes import EmployeeRoutes
from hrms.domain import Employees

app = Flask(__name__)

employees = Employees()
employee_routes = EmployeeRoutes(employees)

# Register all urls
app.add_url_rule('/employees', view_func=employee_routes.all_employees)
app.add_url_rule('/employees', view_func=employee_routes.create_employee, methods=['POST'])
app.add_url_rule('/employees/<employee_id>', view_func=employee_routes.get_employee)




