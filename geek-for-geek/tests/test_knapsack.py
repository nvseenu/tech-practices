import unittest
import geek_for_geek.knapsack as knapsack

class TestKnapsack(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    
    def test_find_maximum_value(self):
        arr = [
            (60,10),
            (100, 20),
            (120, 30)
        ]

        expected_output = [
            (60, 10),
            (100, 20),
            (80, 20)
        ]

        result = knapsack.find_maximum_value(arr, capacity=50)        
        self.assertEqual(expected_output, result)
