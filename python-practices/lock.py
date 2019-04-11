import threading
from threading import Thread, RLock
import time

class A:

	def __init__(self):
		self._lock = RLock()
		self._local = threading.local()
		self._local1 = threading.local()
		print('_lock :', self._local)
		print('_lock1: ', self._local1)

	def execute(self):

		with self._lock:
			print('Acquired a lock for execute, ')
			self.execute1()
			print('Done')

		
	def execute1(self):
		with self._lock:
			print('Acquired lock for execute1')
			time.sleep(1)
			print('Done...')
	

if __name__ == '__main__':
	a = A()
	a.execute()
	print('main done')
