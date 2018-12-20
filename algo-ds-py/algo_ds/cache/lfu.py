from algo_ds import PriorityQueue
from algo_ds.heap import Entry

class LFU:
    """
        Least Frequently Used cache. It keeps elements that are frequently used,
        and evicts less frequently used ones when the cache is full.
    """

    def __init__(self, capacity):
        self._capacity = capacity
        self._priority_queue = PriorityQueue(capacity)


    def get(self, key):
        entry = self._priority_queue.get_entry(key)
        if not entry:
            return None

        self._priority_queue.set_priority(key, entry._priority + 1)
        return entry._key
    

    def put(self, key):
        self.evict_if_needed()
        self._priority_queue.enqueue(key, 0)
        


    def evict_if_needed(self):
        if len(self._priority_queue) == self._capacity:  
            e = self._priority_queue.dequeue()

    
    def __len__(self):
        return len(self._priority_queue)        


