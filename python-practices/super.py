class A:

	def spam(self):
		print('A.spam')
		super().spam()



class B:
	def spam(self):
		print('B.spam')
		super().spam()


class C(B,A):
	pass



class User:
	
	def __init__(self, fname, lname):
		self.fname = fname
		self.lname = lname

	@property
	def fname(self):
		print('__getter__')
		return self._fname		

	@fname.setter
	def fname(self, fname):
		print('__setter__')
		self._fname = fname

if __name__=='__main__':

	u = User("srini", "vasan")
	print(u.fname)
	c = C()
	c.spam()	
