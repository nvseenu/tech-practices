class Query:

	def __init__(self, sql):
		self._sql = sql

	def execute(self):
		print(f'executing query: {self._sql}')


class Transaction:

	def __init__(self):
		self._counter = 0

	def __enter__(self):
		print('__enter__')
		return self

	def __exit__(self, exc_type, exc_value, tb):
		print('__exit__')


if __name__ == '__main__':
	with Transaction() as t:
		q  = Query('select * from user')   	
		q.execute()
	print("Done!!!!")	

