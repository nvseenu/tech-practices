
import time

def sum(n):
	sum = 0
	time.sleep(5)
	for i in range(1,n):		
		sum += i
	time.sleep(5)	
	print(sum)	


from threading import Thread

t = Thread(target=sum, args=(100,))
t.start()
t.join()
print("Done!!!!")


