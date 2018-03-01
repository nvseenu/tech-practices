import unittest
from btree_node import BTreeNode

class TestBTreeNode(unittest.TestCase):    

    def test_add_key(self):
        """
            Test if keys are added in ascending order
        """
        b = BTreeNode(4)
        b.add_key(10)
        b.add_key(8)
        b.add_key(6)
        b.add_key(4)
        
        rs = [b.get_key(i) for i in range(4)]
        exp = [4, 6, 8, 10]
        self.assertEqual(exp, rs)      


    def test_add_key_when_btreenode_is_full(self):
        b = BTreeNode(4)
        
        with self.assertRaises(ValueError):
            for i in range(5):
                b.add_key(i*2)        


    def test_get_key_when_index_exceeds(self):
        b = BTreeNode(4)
        inp = [8, 2, 4, 10]
        for i in inp:
            b.add_key(i)

        with self.assertRaises(ValueError):
            for i in range(5):
                b.get_key(i)


    def test_find_key_index(self):
        b = BTreeNode(4)
        inp = [8, 2, 4, 10]
        for i in inp:
            b.add_key(i)

        self.assertEqual(2, b.find_key_index(8))    
        self.assertEqual(1, b.find_key_index(4))

    def test_find_key_not_exist(self):
        b = BTreeNode(4)
        inp = [8, 2, 4, 10]
        for i in inp:
            b.add_key(i)

        self.assertEqual(-1, b.find_key_index(12))            


    def test_is_full(self):
        b = BTreeNode(4)
        self.assertFalse(b.is_full())
        inp = [8, 2, 4, 10]
        for i in inp:
            b.add_key(i)

        self.assertTrue(b.is_full())    
            

    def test_is_leaf(self):
        b = BTreeNode(4)
        b.add_key(2)
        self.assertTrue(b.is_leaf())

        b1 = BTreeNode(4)
        b1.add_key(1)
        b.add_child(b1)
        self.assertFalse(b.is_leaf())


    def test_add_child(self):
        b = BTreeNode(4)
        b.add_key(2)        

        c1 = BTreeNode(4)
        c1.add_key(1)
        b.add_child(c1)

        c2 = BTreeNode(4)
        c2.add_key(3)
        b.add_child(c2)

        self.assertEqual(c1, b._children[0])
        self.assertEqual(c2, b._children[1])


    def test_find_child(self):
        """
            Tests if node can find a child which given key belongs to
        """
        b = BTreeNode(4)
        b.add_key(2)        

        c1 = BTreeNode(4)
        c1.add_key(1)
        b.add_child(c1)

        c2 = BTreeNode(4)
        c2.add_key(3)
        b.add_child(c2)

        self.assertEqual(c1,  b.find_child(1))
        self.assertEqual(c2,  b.find_child(3))
        self.assertEqual(c2,  b.find_child(5))   


    def test_move_keys_and_children(self):
        a = BTreeNode(4)
        a.add_key(20)        
        a.add_key(30)        
        a.add_key(40)        
        a.add_key(50)        

        c1 = BTreeNode(4)
        c1.add_key(15)
        a.add_child(c1)

        c2 = BTreeNode(4)
        c2.add_key(25)
        a.add_child(c2)

        c3 = BTreeNode(4)
        c3.add_key(35)
        a.add_child(c3)

        c4 = BTreeNode(4)
        c4.add_key(45)
        a.add_child(c4)

        c5 = BTreeNode(4)
        c5.add_key(55)
        a.add_child(c5)

        b = BTreeNode(4)
        a.move_keys_and_children(2, b)
        
        self.assertEqual(2, len(a._keys), "Key size of node 'a' after move is not matching")
        self.assertEqual([20,30], a._keys)
        self.assertEqual(3, len(a._children), "children size of node 'a' after move is not matching")
        self.assertEqual(c1,  a._children[0])
        self.assertEqual(c2,  a._children[1])
        self.assertEqual(c3,  a._children[2])

        self.assertEqual(1, len(b._keys), "Key size of node 'b' after move is not matching")
        self.assertEqual([50], b._keys)
        self.assertEqual(2, len(b._children), "children size of node 'b' after move is not matching")
        self.assertEqual(c4,  b._children[0])
        self.assertEqual(c5,  b._children[1])