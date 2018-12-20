from algo_ds.cache.lfu import LFU
import unittest


class TestLFU(unittest.TestCase):


    def setUp(self):
        pass

    
    def tearDown(self):
        pass


    def test_contains(self):
        """It tests if a cache keeps all elements stored into. Assume total elements are less than
        total capacity. hence there is no eviction.
        """

        lfu = LFU(4)  
        for i in range(1,5):
            lfu.put(i)

        for i in range(1,5):
            self.assertEqual(i, lfu.get(i), "Cache does not contain a element that was stored before")    

    def test_eviction(self):
        """It tests if right element has been evicted out from the cache especially
        when the cache is full and we try to store a new element.
        """
        lfu = LFU(4)        

        lfu.put(1)
        lfu.put(2)
        lfu.put(3)
        lfu.put(4)

        lfu.get(1)
        lfu.get(3)

        # Inserting 5 needs a room but lfu is full. 
        # since 2 is less frequently used item , it should be evicted
        lfu.put(5)
        self.assertEqual(4, len(lfu), "LFU length is not correct")
        self.assertIsNone(lfu.get(2), "Eviction is not happening properly")

        lfu.get(5)

        # Inserting 6 needs a room but lfu is full. 
        # since 4 is less frequently used item , it should be evicted
        lfu.put(6)
        self.assertEqual(4, len(lfu), "LFU length is not correct")
        self.assertIsNone(lfu.get(4), "Eviction is not happening properly")



        