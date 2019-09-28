import unittest
from algo_ds import sorting


class TestSort(unittest.TestCase):

    def setUp(self):
        self.sortings = [sorting.InsertionSort(), sorting.MergeSort()]
        self.test_data = [
            ([10, 9, 8, 7, 6, 5, 4, 3, 2, 1], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
            ([5, 4, 3, 2, 1, 6, 7, 9, 8, 10], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
            ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
        ]

    def tearDown(self):
        pass

    def test_sort(self):
        for sort in self.sortings:
            for tinput, expected in self.test_data:
                # Since all sorts are done in place replacement, we need
                # take a copy of the input
                input = tinput[:]
                sort.sort(input)
                self.assertEqual(expected, input, f"{sort.__class__.__name__} fails!!!")
