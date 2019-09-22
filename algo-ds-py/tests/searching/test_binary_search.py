import unittest
from algo_ds.searching import binary_search

class TestBinarySearch(unittest.TestCase):

    def test_binary_search(self):

        test_data_set = [
            [[1,2,3,4,5], 1 , 0],
            [[1,2,3,4,5], 7 , -1],
            [[1,2,3,4,5], 4 , 3],
            [[1,2,3,4,5], 5 , 4],
            [[1,2,3,4,5], 0 , -1]
        ]

        for td in test_data_set:
            with self.subTest(td = td):
                arr, key, exp_result = td
                self.assertEqual(binary_search.search(arr, key), exp_result)
    

    def test_binary_search_for_big_data_set(self):

        arr = [i for i in range(100)]
        for key in arr:
            self.assertEqual(binary_search.search(arr, key), key)

        self.assertEqual(binary_search.search(arr, -1), -1)    
        self.assertEqual(binary_search.search(arr, 100), -1)    