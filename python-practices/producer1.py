from queue import  Queue
from threading import Thread

_stop_val = object()

def produce(n, que):
	for i in range(n):
		que.put(i)
		print("Produced val:", i)

	que.put(_stop_val)
	print("Producer is done")	



def consume(que):
	while True:
		val = que.get()	
		if val == _stop_val:
			que.put(val)
			break

		print('consumed val:', val)
	print('Consumer is done!!!')	


que = Queue()
pt = Thread(target=produce, args=(10, que))
pt.start()

ts = [Thread(target=consume, args=(que,)) for i in range(4)]
	 
for t in ts:
	t.start()


pt.join()
for t in ts:
	t.join()	

print("Main is done")

