import unittest
from problems import array


class TestArray(unittest.TestCase):

    def _test_data_set(self):
        return [
            [[-2, -5, 6, -2, -3, 1, 5, -6], 7, [6, -2, -3, 1, 5]],
            [[100, -5, -6, -2, -3, 1, 5, -6], 100, [100]],
            [[-1, -1, -1, -1, -1, 1, 5, -1], 6, [1, 5]]
        ]

    def test_maximum_subarray_by_divide_and_conquer(self):
        for td in self._test_data_set():
            with self.subTest(td=td):
                arr, exp_sum, expected_output = td
                sum, start, end = array.maximum_subarray_by_divide_and_conquer(arr)
                self.assertEqual(sum, exp_sum, 'Sum is not matching')
                self.assertEqual(arr[start: end + 1], expected_output, 'Subarray is not matching')

    def test_maximum_subarray_by_kadane_algorithm(self):
        for td in self._test_data_set():
            with self.subTest(td=td):
                arr, exp_sum, expected_output = td
                sum, start, end = array.maximum_subarray_by_kadane_algorithm(arr)
                self.assertEqual(sum, exp_sum, 'Sum is not matching')
                self.assertEqual(arr[start: end + 1], expected_output, 'Subarray is not matching')

    def test_counting_valleys(self):
        test_data_set = [
            ['DDUUUUDD', 1],
            ['DDUUDDUU', 2],
            ['UUDDUUDD', 0],
            ['DDDUDUDUUU', 1],

        ]
        for td in test_data_set:
            with self.subTest(td=td):
                self.assertEqual(array.counting_valleys(td[0]), td[1])

    def test_count_matching_pair_of_socks(self):
        test_data_set = [
            [[1, 1, 2, 2, 3, 3], 3],
            [[1, 2, 1, 2, 1, 3, 2], 2],
            [[1, 2, 3, 4, 5, 6], 0]
        ]
        for td in test_data_set:
            with self.subTest(td=td):
                self.assertEqual(array.count_matching_pair_of_socks(td[0]), td[1])

    def test_minimum_jumps_among_clouds(self):
        test_data_set = [
            [[0, 0, 1, 0, 0, 1, 0], 4],
            [[0, 0, 0, 0, 1, 0], 3],
            [[0, 1, 0, 1, 0, 1, 0], 3],

        ]
        for td in test_data_set:
            with self.subTest(td=td):
                self.assertEqual(array.minimum_jumps_among_clouds(td[0]), td[1])
