import time
from functools import wraps

def timethis(func):

	@wraps(func)
	def wrapper(*args, **kwargs):
		start = time.time()
		res = func(*args, **kwargs)
		end = time.time()
		print(func.__name__, end - start)
		return res
	return wrapper	



@timethis
def fact(n):
	if n == 1:
		return 1

	res = 1	
	for i in range(n, 0, -1):
		res = res * i
	return res


if __name__ == '__main__':
	res = fact(1000)
	print(res)
	






