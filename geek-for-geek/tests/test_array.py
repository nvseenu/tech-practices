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

        



