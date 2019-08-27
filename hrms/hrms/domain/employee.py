import json

class Employee:

	def __init__(self, fields):
		if 'id' not in fields:
			raise ValueError('id field is not found')

		self._fields = fields	

	
	def save(self):		
		print('dumps ===> ', self.to_json())

	
	def to_json(self):
		return json.dumps(self._fields)
