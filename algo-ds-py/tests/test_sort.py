import unittest
import sort

class TestSort(unittest.TestCase):

    def setUp(self):
        pass

    
    def tearDown(self):
        pass

    
    def test_insertion_sort(self):
        input = [5,4,3,2,1,6,7,9,8,10]
        expected_output = [1,2,3,4,5,6,7,8,9,10]
        sort.insertion_sort(input)
        self.assertEqual(expected_output, input)
        
