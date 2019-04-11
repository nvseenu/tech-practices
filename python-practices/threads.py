from threading import Thread
import time

def do_task(id):
	print('thread ', id ,' starts')
	sum = 0
	for i in range(100000):
		sum+=i
	print('thread ',id,' sum:', sum)	

ts = [Thread(target=do_task, args=(i,))  for i in range(2000)]
for t in ts:
	t.start()

for t in ts:
	t.join()

print('main is done!!!')


