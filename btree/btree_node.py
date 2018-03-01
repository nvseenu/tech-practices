
class BTreeNode:
    
    def __init__(self, size):
        self._keys = []
        self._children = []
        self._size = size
        self._block_id=None
        self._loaded = False        
        
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
        return self._loaded

    @property
    def children(self):
        return self._children    


    def load(self, keys, children):
        self._keys = keys
        self._children = children
        self._loaded = True        

    def add_key(self, k):
        if self.is_full():
            raise ValueError("Node is full")

        pos = self.find_key_index(k)
        if pos == -1:
            self._keys.insert(len(self._keys), k)    
        else:
            self._keys.insert(pos, k)

    def get_key(self, index):
        if index >= len(self._keys):
            raise ValueError("Index is out of range")

        return self._keys[index]

    def remove_key(self, index):
        # print("remove key at ", index)
        del self._keys[index]

    def add_child(self, node):
        i = self._find_free_child_index(node)
        self._children.insert(i, node)

    def remove_child(self, index):
        del self._children[index]


    def add_empty_child(self, n):
        self._children.append(n)    

    def find_child(self, key):
        i = self._find_child_index(key)
        return self._children[i] if i < len(self._children) else None

    def move_keys_and_children(self, index, target):
        """
           Moves keys and their respective children from given index to target node.     
        """
        i = index + 1
        while i < len(self):
            #print("copying key {} to right node".format(self.get_key(i)))
            target.add_key(self.get_key(i))            
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

    def find_key_index(self, k):
        for i in range(len(self._keys)):
            if k <= self._keys[i]:
                return i
        return -1

    def _find_free_child_index(self, node):
        #print("find_free_child_index: node: ", node)
        k = node._keys[0]
        for i, ch in enumerate(self._children):
            if k <= ch._keys[0]:
                return i

        return len(self._children)

    def _find_child_index(self, key):
        for i in range(len(self._keys)):
            if self._keys[i] >= key:
                return i
        return len(self._keys)
    

    def __str__(self):
        s = []
        ks = [str(i) + ":" + str(v) for i, v in enumerate(self._keys)]
        s.append("keys => " + ", ".join(ks))

        cs = []
        cs.append("Children => ")
        cs = [
            str(i) + ":" + str(v._keys) for i, v in enumerate(self._children)
        ]
        s.append("children => " + ", ".join(cs))
        return "\n".join(s)