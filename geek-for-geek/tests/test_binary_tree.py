import unittest
import geek_for_geek.tree.binary_tree as bt


class TestBinaryTree(unittest.TestCase):

	def setUp(self):
		pass	

	def tearDown(self):
		pass


	def test_preorder(self):	
		t = self.binary_tree()		
		self.assertEqual([1,2,4,5,3], t.preorder())

	
	def test_inorder(self):	
		t = self.binary_tree()
		self.assertEqual([4,2, 5, 1, 3], t.inorder())

	
	def test_postorder(self):	
		t = self.binary_tree()
		self.assertEqual([4,5,2,3,1], t.postorder())		


	def binary_tree(self):
		"""
				1
			  /	 \
			2	  3
		   / \
		  4  5

		"""
		n3 = bt.Node(3, None, None)
		n4 = bt.Node(4, None, None)
		n5 = bt.Node(5, None, None)
		n2 = bt.Node(2, n4, n5)
		n1 = bt.Node(1, n2, n3)
		t = bt.BinaryTree(n1)
		return t
