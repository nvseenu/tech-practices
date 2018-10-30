from heap import MinHeap

class PriorityQueue:

    def __init__(self, capacity):
        self._capacity = capacity
        self._heap = MinHeap()

    def enqueue(self, item, priority):
        if self._capacity == len(self):
            raise ValueError("Queue is full")

        self._heap.insert(item, priority)       

    
    def dequeue(self):
        if len(self) == 0:
            raise ValueError("Queue is empty")
        return self._heap.extract_min()       


    def set_priority(self, item, priority):        
        self._heap.increase_key(item, priority)    

    
    def get(self, item):
        e = self.get_entry(item)        
        if not e:
            return None
        else:
            return e._key 

    
    def get_entry(self, item):
        return self._heap.get_entry(item)      

    
    def peek(self):
        return self._heap.minimum()

    def __len__(self):
        return len(self._heap)    