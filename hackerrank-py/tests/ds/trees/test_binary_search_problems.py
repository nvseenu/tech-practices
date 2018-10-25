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
        tree = bst.BinarySearchTree()
        arr = [1,2,5,3,6,4]
        for a in arr:
            tree.create(a)

        top = bst.topView(tree.root)
        expected_top = [1,2,5,6]
        self.assertEqual(expected_top, top, "TopView is not generated properly")

                