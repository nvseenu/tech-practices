from functools  import wraps
import logging


def logged(level, name=None, message=None):

	def decorate(func):
		lname = func.__module__ if name is None else name
		log = logging.getLogger(lname)
		logmsg = message if message else func.__name__

		@wraps(func)
		def wrapper(*args, **kwargs):
			log.log(level, logmsg)
			return func(*args, **kwargs)
		return wrapper	

	return decorate	


@logged(logging.DEBUG)
def add(x,y):
	return x+y

@logged(logging.CRITICAL, 'example')
def spam():
	print('spam,,,')


if __name__ == '__main__':
	logging.basicConfig(level=logging.DEBUG)
	add(10, 14)
	spam()



