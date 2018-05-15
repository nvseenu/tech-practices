import unittest
import logging
from btree_node import BTreeNode
from btree_file import BTreeFile
import json

class TestBTreeFileTest(unittest.TestCase):

    def test_root_block_id(self):        
        btf = BTreeFile('test-btree-file-root.bin', 4*1024, int)
        n = BTreeNode(4)
        n.block_id = 10
        n.add_key(10)
        n.add_key(8)
        n.add_key(6)
        n.add_key(4)
        n.set_as_root(True)
        block_id = btf.write_node(n)
        btf.close()

        btf = BTreeFile('test-btree-file-root.bin', 4*1024, int)
        self.assertEqual(n.block_id, btf.root_block_id, "Root block id is wrong")
            
    unittest.skip
    def test_read_and_write(self):
        btf = BTreeFile('test-btree-file.bin', 4*1024, int)
        n = BTreeNode(4)
        n.add_key(10)
        n.add_key(8)
        n.add_key(6)
        n.add_key(4)

        block_id = btf.write_node(n)

        n1 =  btf.read_node(block_id)
        logging.debug("Actual node: %s", n1)

        self.assertEqual(len(n), len(n1), 'Node length is not matching')
        for i in range(len(n)):
            self.assertEqual(n.keys[i] , n1.keys[i], "Key at index :{} is not matching after reading".format(i))
        
        btf.close()        

    
    def test_read_and_write_for_scalar(self):
        btf = BTreeFile('test-btree-file.bin', 4*1024, int)
        n = BTreeNode(4)
        n.add_key(User(10, "10fname", "10lname", None))
        n.add_key(User(8, "8fname", "8lname", None))
        n.add_key(User(6, "6fname", "6lname", None))
        n.add_key(User(4, "4fname", "4lname", None))

        block_id = btf.write_node(n)

        n1 =  btf.read_node(block_id)
        logging.debug("Actual node: %s", n1)

        self.assertEqual(len(n), len(n1), 'Node length is not matching')
        for i in range(len(n)):
            self.assertEqual(n.keys[i] , n1.keys[i], "Key at index :{} is not matching after reading".format(i))

        btf.close()     


class User:
    def __init__(self, id, fname, lname, dob):
        self.id = id
        self.fname = fname
        self.lname = lname
        self.dob = dob

    #def __eq__(self, u):
    #    return (self.id,self.fname) == (u.id, u.fname)    

    def __le__(self, u):
        return (self.id,self.fname) <= (u.id, u.fname)

    def __eq__(self, u ):
        logging.debug('eq => %s', u)
        return (self.id,self.fname) == (u.id, u.fname)    
            

