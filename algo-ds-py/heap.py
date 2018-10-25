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
    #print(f"li: {li}, ri: {ri}") 
    largest = i

    if li < len(heaps) and heaps[li] > heaps[i]:
        largest = li

    if ri < len(heaps) and heaps[ri] > heaps[largest]:
        largest = ri

    if largest != i:
        #print(f"swap largest: {largest}, i: {i}")
        swap(heaps, largest, i)
        max_heapify(heaps, largest)         

def build_max_heap(heaps):
    n = (len(heaps) // 2 ) 
    for i in range(n, -1, -1):
        #print(f">>>>>>>>>>>>>>>>> max heapify at i:{i}")
        max_heapify(heaps, i)
        #print(f"After i={i}, heap={heaps}")


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



class MaxHeap:

    def __init__(self, priority_callback):        
        self._entries = []
        self._priority_callback = priority_callback

    def insert(self, key):        
        e = Entry(self._priority_callback(key), key)
        self._entries.append(e)
        n = (len(self._entries) // 2 ) 
        for i in range(n, -1, -1):
            max_heapify(self._entries, i)

    def increase_key(self, idx,  key):        
        e = Entry(self._priority_callback(key), key)
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
        e = Entry(self._priority_callback(key), key)
        return self._entries.index(e)
               


    


    