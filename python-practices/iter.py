class Node:

	def __init__(self, value):
		self._value =  value
		self._children = []


	def add_child(self, node):
		self._children.append(node)

	
	def __repr__(self):
		return f"Node({self._value})"	

	
	def __iter__(self):		
		return iter(self._children)	


if __name__ == '__main__':
	root = Node(1)
	c1 = Node(2)
	c2 = Node(3)
	root.add_child(2)
	root.add_child(3)

	for n in root:
		print(f"Node: {n}")			