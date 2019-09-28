import time


class Node:

	def __init__(self, id=-1, keys=[], root=False, child_node_ids=[]):
		self._id = id
		self._keys = keys
		self._child_node_ids = child_node_ids
		self._root = root


    def add_key(self, key):
    	i = self._find_free_index(key)
    	self._keys.insert(i, key)


	def _find_free_index(self, key):
		for i, k in enumerate(self._keys):
			if k >= key:
				return i
		return len(self._keys)		


	def is_root(self):
		return self.root	


	def __str__(self):
		return "id: {} , keys: {}".format(self._id, self._keys)	
