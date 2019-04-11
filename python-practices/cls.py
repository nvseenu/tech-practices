class A:

	def __init__(self):
		self._pri = 1
		self.__pri = 2

	def __str__(self):
		return f"{self._pri} -- {self.__pri}"


if __name__ == '__main__':
	a = A()
	print(a)
	print(a.__dict__)			