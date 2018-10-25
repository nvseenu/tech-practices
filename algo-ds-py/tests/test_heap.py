import unittest
import heap
from heap import MaxHeap

class TestHeap(unittest.TestCase):

    def setUp(self):
        pass
    

    def tearDown(self):
        pass

    def test_max_heapify(self):
        heaps = [16, 4, 10, 14, 7, 9, 3, 2, 8, 1]
        heap.max_heapify(heaps, 1)
        heaps1 = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        self.assertEqual(heaps1, heaps)

    def test_build_max_heap(self):
        expected_heaps = [16,14,10,8,7,9,3,2,4,1]
        heaps = [4,1,3,2,16,9,10,14,8,7]
        heap.build_max_heap(heaps)
        self.assertEqual(expected_heaps, heaps)
   
    def test_max_heap(self):
        arr = [1,2,3,4,5,6,7,8,9,10]
        def priority(x):            
            return x
        
        h = MaxHeap(priority)        
        for i in arr:
            h.insert(i)

        expected_heaps = [10,9,8,7,6,5,4,3,2,1]
        for i in expected_heaps:  
            self.assertEqual(i, h.extract_max(), f"For i:{i}, Got unexpected maximum value")      

    def test_increase_key(self):
        arr = [1,2,3,4,5,6,7,8,9,10]

        def priority(x):            
            return x
        
        h = MaxHeap(priority)        
        for i in arr:
            h.insert(i)

        # Increase key 1 to 20         
        h.increase_key(h.find_index(1), 20)

        expected_heaps = [20,10, 9,8,7,6,5,4,3,2]
        for i in expected_heaps:
            self.assertEqual(i, h.extract_max(), f"Got unexpected maximum value")      


    def test_max_heap_for_custom_object(self):

        arr = [Job(i, "Job"+str(i)) for i in range(1,11)]

        def priority(job):            
            return job.priority()
        
        h = MaxHeap(priority)        
        for i in arr:
            h.insert(i)

        expected_heaps = [arr[i] for i in range(8, -1, -1)]
        for i in expected_heaps:  
            self.assertEqual(i, h.extract_max(), f"For i:{i}, Got unexpected maximum value")              

       

class Job:
    def __init__(self, id, name):
        self._id = id
        self._name = name

    
    def priority(self):
        return self._id    

    def __str__(self):
        return f"Job[id={self._id}, name={self._name}"    

    def __repr__(self):
        return str(self)



        
