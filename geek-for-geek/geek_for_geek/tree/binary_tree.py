class Node:
	def __init__(self, key, left, right):
		self._key = key
		self._left = left
		self._right = right

	@property
	def key(self):
		return self._key		

	@property
	def left(self):
		return self._left

	@property
	def right(self):
		return self._right
	
	def __repr__(self):
		return f"Node({self._key}, {self._left}, {self._right})"	


class BinaryTree:

	def __init__(self, root):
		self._root = root

	def preorder(self):
		path = []
		self._preorder(self._root, path)
		return path
	
	def _preorder(self, node, path):
		if not node:
			return 

		path.append(node.key)
		self._preorder(node.left, path)
		self._preorder(node.right, path)

	
	def inorder(self):
		path = []
		self._inorder(self._root, path)
		return path
	
	def _inorder(self, node, path):
		if not node:
			return 
		
		self._inorder(node.left, path)
		path.append(node.key)
		self._inorder(node.right, path)	
		


	def postorder(self):
		path = []
		self._postorder(self._root, path)
		return path
	
	def _postorder(self, node, path):
		if not node:
			return 
		
		self._postorder(node.left, path)
		self._postorder(node.right, path)	
		path.append(node.key)

