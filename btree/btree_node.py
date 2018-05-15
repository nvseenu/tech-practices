import logging
import json

class BTreeNode:
    """
       This class represents a node in BTree  data structure. 
       The node keeps its keys and childrens in ascending order.

       The node can have 'n' keys and 'n+1' children.
    """
    
    def __init__(self, size):
        self._keys = []
        self._children = []
        self._size = size
        # It denotes which block the current node belongs to
        self._block_id=None
        self._loaded = False
        self._root = False        
        
    @property    
    def block_id(self):
        return self._block_id

    @block_id.setter
    def block_id(self, block_id):
        self._block_id = block_id

    @property    
    def keys(self):
        return self._keys   

    
    def is_loaded(self):
        """
        Tells if the node is loaded completely with keys and children  
        """
        return self._loaded

    def set_loaded(self, loaded):
        self._loaded = loaded    

    @property
    def children(self):
        return self._children    

   
    def load(self, keys, children):
        self._keys = keys
        self._children = children
        self._loaded = True        
   
    def add_key(self, k):
        """
        Added a given key into the node
        """
        if self.is_full():
            raise ValueError("Node is full")

        idx = self.find_free_index(k)
        self._keys.insert(idx, k)    
        

    def remove_key(self, index):
        """
        Removes the key located at the given index 
        """
        # print("remove key at ", index)
        del self._keys[index]


    def add_child(self, node):
        """
        Adds a given child at its appropriate index 
        """
        i = self.find_child_index(node.keys[0])
        self._children.insert(i, node)


    def remove_child(self, index):
        """
        Removes the child located at given index    
        """
        del self._children[index]


    def add_empty_child(self, n):
        self._children.append(n)    

    def find_child(self, key):
        return self.find_child_index(key) 

    def find_child_index(self, key):
        """
        Finds a child node where the given key can be stored 
        """
        return self.find_free_index(key)
        

    def move_keys_and_children(self, index, target):
        """
        Moves keys and their respective children from given index to target node.     
        """
        i = index + 1
        while i < len(self):
            #print("copying key {} to right node".format(self.get_key(i)))
            target.add_key(self.keys[i])            
            i += 1

        # move children associated with the keys moved
        i = index + 1
        while i < len(self._children):
            #print("copying child {} to right node".format(self._children[i]._keys))
            target.add_child(self._children[i])            
            i += 1    

        # Remove the moved keys  from left
        i = len(self) - 1
        while i >= index:
            #print("removing key {} from left node".format(self.get_key(i)))
            self.remove_key(i)
            i -= 1

        # Remove the moved children  from left
        i = len(self._children) - 1
        while i > index:
            #print("removing child {} from left node".format(self._children[i]._keys))
            self.remove_child(i)
            i -= 1    

    def __len__(self):
        return len(self._keys)

    def keys_len(self):
        return len(self._keys)        

    def children_len(self):
        return len(self._children)    

    def is_full(self):        
        return len(self._keys) == self._size

    def is_leaf(self):
        return len(self._children) == 0

    def set_as_root(self, val):
        self._root = val    

    def is_root(self):
        return self._root   

   
    def find_free_index(self, key):
        """
        Finds a free index at which new key can be inserted    
        """        
        for i, k  in enumerate(self._keys):
            if k >= key:
                return i

        return len(self.keys)


    def key_at(self, key):
        """
        Finds an index at whuch given key is found
        """
        for i, k in enumerate(self._keys):
            if k == key:
                return i
        return -1
    

    def __str__(self):        
        keys = [str(k) for i, k in enumerate(self._keys)]
        s = []
        s.append("block_id: {}, keys: {}".format(self.block_id, keys))
        s.append("\n")           
        for i,c in enumerate(self.children):            
            s.append("  child{}: {}".format(i,c))

        return "".join(s)    
        #return json.dumps(self, default= lambda o : o.__dict__)
        #return "{{block_id: {}, keys: {}, children:{}}}".format(self.block_id,keys, children)