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

		print("ll => " , linkedlist.items(ll.head))	

		ll1 = linkedlist.SinglyLinkedList()
		for i in range(10):
			ll1.insert_node(i)

		print("ll1 => " , linkedlist.items(ll1.head))		

		h = linkedlist.merge_lists(ll.head, ll1.head)	
		print("header => " , linkedlist.items(h))
					
	def test_has_cycle(self):
		ll = linkedlist.SinglyLinkedList()
		for i in range(10):
			ll.insert_node(i)

		print("ll => " , linkedlist.items(ll.head))	
		h = linkedlist.has_cycle(ll.head)	
		print("cycle => " , h)

if __name__ == '__main__':
    unittest.main()
