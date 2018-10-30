import unittest

from priority_queue import PriorityQueue

class TestPriorityQueue(unittest.TestCase):

    def setUp(self):
        pass
    

    def tearDown(self):
        pass

   
    def test_priority_queue(self):
        arr = [10,9,8,7,6,5,4,3,2,1]
               
        pq = PriorityQueue(10)        
        for i in arr:
            pq.enqueue(i,i)

        expected_items = [1,2,3,4,5,6,7,8,9,10]

        for i in expected_items:  
            self.assertEqual(i, pq.dequeue(), f"For i:{i}, Dequeued unexpected item")      

   
    def test_priority_queue_for_custom_object(self):

        jobs = [
            Job(5, "Job5"),
            Job(4, "Job4"),
            Job(3, "Job3"),
            Job(2, "Job2"),
            Job(1, "Job1")            
        ]        

        pq = PriorityQueue(5)   
        for job in jobs:
            pq.enqueue(job, job._id)

        expected_jobs = [
            Job(1, "Job1"),
            Job(2, "Job2"),
            Job(3, "Job3"),
            Job(4, "Job4"),
            Job(5, "Job5")
        ]

        for job in expected_jobs:  
            self.assertEqual(job, pq.dequeue(), f"Got unexpected job")              

    
    def test_set_priority(self):

        jobs = [
            Job(30, "Job3"),
            Job(20, "Job2"),
            Job(10, "Job1")            
        ]

        pq = PriorityQueue(3)   
        for job in jobs:
            pq.enqueue(job, job._id)

        # set priority for job3 to 20, now it should be the first item to evict 
        pq.set_priority(Job(30, "Job3"), 3)   

        # As we increased priority of job1 to 20, first item evicted from the queue should 
        # be job1
        expected_jobs = [
            Job(30, "Job3"),   
            Job(10, "Job1"),           
            Job(20, "Job2")                        
        ]

        for job in expected_jobs:  
            self.assertEqual(job, pq.dequeue(), f"Got unexpected job")         
    

    def test_get(self):
        jobs = [
            Job(30, "Job3"),
            Job(20, "Job2"),
            Job(10, "Job1")            
        ]

        pq = PriorityQueue(3)   
        for job in jobs:
            pq.enqueue(job, job._id)

        self.assertEqual(Job(30, "Job3"), pq.get(Job(30, "Job3")), f"Got unexpected job")
        self.assertIsNone(pq.get(Job(4, "Job4")))  
        

class Job:
    def __init__(self, id, name):
        self._id = id
        self._name = name
    
    def priority(self):
        return self._id    

    def __eq__(self, other):
        return (self._id, self._name) ==  (other._id, other._name)    

    def __str__(self):
        return f"Job[id={self._id}, name={self._name}]"    

    def __repr__(self):
        return str(self)



        
