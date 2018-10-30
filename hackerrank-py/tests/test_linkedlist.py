import unittest
import linkedlist 

class TestLinkedList(unittest.TestCase):

	def test_compare_lists(self):

		ll = linkedlist.SinglyLinkedList()
		for i in range(10):
			ll.insert_node(i)

		ll1 = linkedlist.SinglyLinkedList()
		for i in range(10):
			ll1.insert_node(i)	

		self.assertEqual(1, linkedlist.compare_lists(ll.head, ll1.head))


	def test_merge_lists(self):
		ll = linkedlist.SinglyLinkedList()
		for i in range(10):
			ll.insert_node(i)


		ll1 = linkedlist.SinglyLinkedList()
		for i in range(10):
			ll1.insert_node(i)

		h = linkedlist.merge_lists(ll.head, ll1.head)	
					
	def test_has_cycle(self):
		ll = linkedlist.SinglyLinkedList()
		ll.insert_node(1)
		self.assertFalse(linkedlist.has_cycle(ll.head))			

	
	def test_get_node_from_tail(self):
		ll = linkedlist.SinglyLinkedList()
		ll.insert_node(3)
		ll.insert_node(2)
		ll.insert_node(1)

		self.assertEqual(3, linkedlist.getNode(ll.head, 2))

		ll = linkedlist.SinglyLinkedList()
		ll.insert_node(1)
		self.assertEqual(1, linkedlist.getNode(ll.head, 0))

		ll = linkedlist.SinglyLinkedList()
		ll.insert_node(4)
		ll.insert_node(3)
		ll.insert_node(2)
		ll.insert_node(1)
		self.assertEqual(3, linkedlist.getNode(ll.head, 2))



