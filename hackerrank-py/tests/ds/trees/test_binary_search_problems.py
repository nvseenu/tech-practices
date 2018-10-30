import ds.trees.binary_search_tree as bst

import unittest

class TestBinarySearchProblems(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass


    def test_height(self):
        tree = bst.BinarySearchTree()
        elms = [3,2,5,1,4,6,7]
        for e in elms:
            tree.create(e)
        
        self.assertEqual(3, bst.height(tree.root))

     
    def test_top_view(self):        
        test_data = [
                ([], []),
                ([1,2,3,4,5], [1,2,3,4,5]),  # Right linear subtree
                ([5,4,3,2,1], [1,2,3,4,5]),  # Left linear subtree

                ([1,2,5,3,6,4], [1,2,5,6]),
                ([10,2,13,1,4,5,6,7,8], [1,2,10,13]),
                ([10,5,4,3,12,13,14], [3,4,5,10,12,13,14])

                ]

        for i in range(len(test_data)):
            arr = test_data[i][0]
            expected_top=test_data[i][1]
            with self.subTest(arr=arr, expected_top=expected_top):
                tree = bst.BinarySearchTree()
                for a in arr:
                    tree.create(a)
                topview = bst._topview(tree.root)                
                self.assertEqual(expected_top, topview, "TopView is not generated properly")

    
    def test_top_view_str(self):        
        arr = [1,2,5,3,6,4]
        expected_top= "1 2 5 6"
        tree = bst.BinarySearchTree()
        for a in arr:
            tree.create(a)
        
        topview = bst.topView(tree.root)                
        self.assertEqual(expected_top, topview, "TopView is not generated properly") 

    @unittest.skip("")
    def test_top_view_str1(self):        
        input = "37 23 108 59 86 64 94 14 105 17 111 65 55 31 79 97 78 25 50 22 66 46 104 98 81 90 68 40 103 77 74 18 69 82 41 4 48 83 67 6 2 95 54 100 99 84 34 88 27 72 32 62 9 56 109 115 33 15 91 29 85 114 112 20 26 30 93 96 87 42 38 60 7 73 35 12 10 57 80 13 52 44 16 70 8 39 107 106 63 24 92 45 75 116 5 61 49 101 71 11 53 43 102 110 1 58 36 28 76 47 113 21 89 51 19 3"
        arr = list(map(int, input.split()))
        expected_top= "1 2 4 14 23 37 108 111 115 116 83 84 85"
        tree = bst.BinarySearchTree()
        for a in arr:
            tree.create(a)
        
        topview = bst.topView(tree.root)                
        self.assertEqual(expected_top, topview, "TopView is not generated properly")                

                