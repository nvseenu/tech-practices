from threading import Thread, Event
import time

def do_something(start_event, end_event):
	print("start exeution ...")
	start_event.set()
	time.sleep(3)

	print("end exeution ...")	
	end_event.set()



start_event = Event()
end_event = Event()
t = Thread(target=do_something, args=(start_event,end_event))
t.start()

start_event.wait()
print('i know thread has started working')
end_event.wait()
print('i know thread has stoppedworking')


		

