from threading import Thread, Condition

def produce(n, que, cv, stop_val):
	i = 0
	while True:
		print('Producer is acquiring a lock')
		with cv:	
			if len(que) == 0 and i == n:
				que.append(stop_val)
				break
			elif len(que) > 0:
				print("Wait for consumer to consume")
				cv.wait()
			else:
				que.append(i)
				print('Produced value: ', i, ' , queue len: ', len(que))
				i += 1
				cv.notify_all()


	print("Producer is done")			


def consume(que, cv, stop_val):
	while True:
		print('Consumer is acquiring a lock')
		with cv:
			if len(que) > 0 and que[0] == stop_val:
				break
			elif len(que) == 0:
				print("Wait for producer to produce")
				cv.wait()
			else:
				val = que.pop()
				print("consumed value: ", val, ', que: ', len(que))	
				cv.notify_all()
	print('Consumer is done')

que = []
cv = Condition()
_stop_val = object()
pt = Thread(target=produce, args=(10, que, cv, _stop_val))
ct = Thread(target=consume, args=(que, cv, _stop_val))
pt.start()
ct.start()
pt.join()
ct.join()
print('DONE')


