class lazyproperty:

	def __init__(self, func):
		self.func = func
		print('__init__ ', func)

	
	def __get__(self, instance, cls):
		print('instance: ', instance)
		if instance is None:
			return self
		else:
			value = self.func(instance)
			print('___setattr__')
			setattr(instance, self.func.__name__, value)
			return value



class Square:

	def __init__(self, side):
		self.side = side

	@lazyproperty
	def area(self):
		print("computing area for ", self.side)
		return self.side * self.side



if __name__ == '__main__':
	s = Square(10)
	print('first time: ', s.area)
	print('second time: ', s.area)	
	print(vars(s))	


