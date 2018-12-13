import unittest
import geek_for_geek.array as array

class TestArray(unittest.TestCase):

    def setUp(self):
        pass
        

    def tearDown(self):
        pass


    def test_find_largest_sum_in_contiguous_subarray(self):
        arr = [4, -3, -1, 7, 1, -3]
        sum = array.find_largest_sum_in_contiguous_subarray(arr)
        self.assertEqual(8, sum)

        arr = [4, -3, -1, -7, -1, -3]
        sum = array.find_largest_sum_in_contiguous_subarray(arr)
        self.assertEqual(4, sum)

        # Implementation looks for largest positive sum. When there is no such one
        # in given array, it returns 0.
        arr = [-4, -3, -1, -7, -1, -3]
        sum = array.find_largest_sum_in_contiguous_subarray(arr)
        self.assertEqual(0, sum)


    def test_left_rotate(self):
        input = [1,2,3,4,5]        
        expected = [4,5,1,2,3]

        # Since the left rotation is a mutable operation,
        # We need to take a copy before sending it.
        arr = input[:]
        array.left_rotate(arr, 3)
        self.assertEqual(expected, arr)


    def test_left_rotate1(self):
        input = [1,2,3,4,5]        
        expected = [4,5,1,2,3]

        # Since the left rotation is a mutable operation,
        # We need to take a copy before sending it.
        arr = input[:]
        array.left_rotate1(arr, 3)
        self.assertEqual(expected, arr)    






        



