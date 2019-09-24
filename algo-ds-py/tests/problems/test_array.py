import unittest
from  problems import array

class TestArray(unittest.TestCase):

    def _test_data_set(self):
        return [
            [[-2, -5, 6, -2, -3, 1, 5, -6], 7,  [6,-2, -3, 1, 5]],
            [[100, -5, -6, -2, -3, 1, 5, -6], 100,  [100]],
            [[-1, -1, -1, -1, -1, 1, 5, -1], 6,  [1,5]],
        ]

    def test_maximum_subarray_by_divide_and_conquer(self):
        for td in self._test_data_set():
            with self.subTest(td = td):
                arr, exp_sum, expected_output = td
                sum, start, end = array.maximum_subarray_by_divide_and_conquer(arr)
                self.assertEqual(sum, exp_sum, 'Sum is not matching')
                self.assertEqual(arr[start: end+1] , expected_output, 'Subarray is not matching')

    def test_maximum_subarray_by_kadane_algorithm(self):
        for td in self._test_data_set():
            with self.subTest(td = td):
                arr, exp_sum, expected_output = td
                sum, start, end = array.maximum_subarray_by_kadane_algorithm(arr)
                self.assertEqual(sum, exp_sum, 'Sum is not matching')
                self.assertEqual(arr[start: end+1] , expected_output, 'Subarray is not matching')            