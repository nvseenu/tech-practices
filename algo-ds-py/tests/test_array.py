import unittest
from algo_ds import array

class TestArray(unittest.TestCase):

    def setUp(self):
        pass

    
    def tearDown(self):
        pass

    
    def test_maximum_subarray(self):
        arr = [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7]

        funcs = [array.maximum_subarray_brute_force, 
                    array.maximum_subarray_divide_and_conquer]
        
        for func in funcs:
            start_index, end_index, max = func(arr)        
            self.assertEqual(43, max, f"Wrong maximum sum from func: {func.__name__}")
            self.assertEqual(7, start_index, f"Wrong start_index from func: {func.__name__}")
            self.assertEqual(10, end_index, f"Wrong end_index from func: {func.__name__}")

    
    