def left_index(i):
    return 2*i +  1


def right_index(i):
    return 2*i + 2

def parent(i):
    return (i + 1) // 2 


def swap(heaps, i, j):
    heaps[i], heaps[j] = heaps[j], heaps[i]

def max_heapify(heaps, i):
    """
      Compare item at given index against its left and right childs. 
      if one of the child has value greater than given one, then the value will be swapped.
      This process will continue from the next largest child onwards until leaf node
      is reached.
    """
    li = left_index(i)
    ri = right_index(i)
    largest = i

    if li < len(heaps) and heaps[li] > heaps[i]:
        largest = li

    if ri < len(heaps) and heaps[ri] > heaps[largest]:
        largest = ri

    if largest != i:
        swap(heaps, largest, i)
        max_heapify(heaps, largest)    


def min_heapify(heaps, i):
    """
      Compare item at given index against its left and right childs. 
      if one of the child has value smaller than given one, then the value will be swapped.
      This process will continue from the next smallest child onwards until leaf node
      is reached.
    """
    li = left_index(i)
    ri = right_index(i)
    largest = i

    if li < len(heaps) and heaps[li] < heaps[i]:
        largest = li

    if ri < len(heaps) and heaps[ri] < heaps[largest]:
        largest = ri

    if largest != i:
        swap(heaps, largest, i)
        min_heapify(heaps, largest)              

def build_max_heap(heaps):
    n = (len(heaps) // 2 ) 
    for i in range(n, -1, -1):
        max_heapify(heaps, i)


def heap_sort(arr):
    build_max_heap(arr)
    for i in range(len(arr)-1, 0, -1):
        t = arr.pop(0)
        arr.insert(len(arr)-1, t)
        max_heapify()

class Entry:
    def __init__(self, priority, key):
        self._priority = priority
        self._key = key

    def __lt__(self, other):
        return self._priority < other._priority

    
    def __gt__(self, other):
        return self._priority > other._priority


    def __eq__(self, other):
        return self._priority == other._priority    

    def __str__(self):
        return f"Entry[priority: {self._priority}, key: {self._key}]"        



class MaxHeap:

    def __init__(self):        
        self._entries = []        

    def insert(self, key, priority):        
        e = Entry(priority, key)
        self._entries.append(e)
        n = (len(self._entries) // 2 ) 
        for i in range(n, -1, -1):
            max_heapify(self._entries, i)

    def increase_key(self, key, priority=None):        
        idx = self.find_index(key)
        e = Entry(priority, key)
        self._entries[idx] = e       
        
        for i in range(idx, -1, -1):
            max_heapify(self._entries, i)
        
        
    def maximum(self):
        return self._entries[0]._key

    def extract_max(self):
        m = self.maximum()
        self._entries.pop(0)
        n = (len(self._entries) // 2 ) 
        for i in range(n, -1, -1):
            max_heapify(self._entries, i)
        return m 

    def find_index(self, key):
        for i,e in enumerate(self._entries):
            if e._key == key:
                return i

        return -1        
        

class MinHeap:

    def __init__(self):        
        self._entries = []        

    def insert(self, key, priority):        
        e = Entry(priority, key)
        self._entries.append(e)
        n = (len(self._entries) // 2 ) 
        for i in range(n, -1, -1):
            min_heapify(self._entries, i)

    def increase_key(self, key, priority=None):        
        idx = self.find_index(key)
        e = Entry(priority, key)
        self._entries[idx] = e       
        
        for i in range(idx, -1, -1):
            min_heapify(self._entries, i)
        
        
    def minimum(self):
        return self._entries[0]._key

    def extract_min(self):
        m = self.minimum()
        self._entries.pop(0)
        n = (len(self._entries) // 2 ) 
        for i in range(n, -1, -1):
            min_heapify(self._entries, i)
        return m 

    def get_entry(self, key):        
        idx = self.find_index(key)
        if idx == -1:
            return None

        return self._entries[idx]   

    def get(self, key):
        entry =  self.get_entry(key)
        if not entry:
            raise ValueError(f"There is no key:{key} found")
        else:
            return entry._key

    def find_index(self, key):
        for i,e in enumerate(self._entries):
            if e._key == key:
                return i

        return -1        

    def __len__(self):
        return len(self._entries)


    