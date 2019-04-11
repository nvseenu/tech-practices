import heapq

class PriorityQueue:
	
	def __init__(self):
		self._queue = []
		self._index = 0
		
	

	def push(self, item, priority):
		heapq.heappush(self._queue, (-priority, self._index, item))
		self._index += 1
		
	
	def pop(self):
		return heapq.heappop(self._queue)[-1]



class Item:		
	
	def __init__(self, name):
		self.name = name
	

	def __repr__(self):
		return f"Item({self.name})"
		
		


pq = PriorityQueue()		
pq.push(Item("Rice"), 30)
pq.push(Item("Noodles"), 20)
pq.push(Item("Grill"), 180)

print(pq.pop())
		
		
	
	

		