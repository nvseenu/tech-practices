from heap import MaxHeap

class PriorityQueue:

    def __init__(self):
        self._heap = MaxHeap()

    def enqueue(self, item, priority):
        self._heap.insert(item, priority)       

    
    def dequeue(self):
        return self._heap.extract_max()       


    def increase_priority(self, item, priority):
        self._heap.increase_key(item, priority)    

    
    def peek(self):
        return self._heap.maximum()