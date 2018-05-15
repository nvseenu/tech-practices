import json
import struct
import logging
from collections import namedtuple
from btree_node import BTreeNode
from btree_file import BTreeFile

class BTree:
    def __init__(self, keys_size, key_class, file_path='btree.bin'):
        self._keys_size = keys_size
        self._root = None
        self._key_class = key_class
        self._bfile = BTreeFile(file_path, 4*1024, key_class)  
        self._elms_size = 0           

    def insert(self, key):
        """
        Inserts a given key into BTree. 

        First it tries find a target node which can accomodate the given key.
        In order to find such a node, It starts from root and visits subsequent levels based
        on key comparison.
        Along the visit, if it finds any node that cross the path to suitable node is full,
        It executes a split operation.
        """
        if not self._root:
            try:
                #Assume block 0 is root node
                logging.debug('Loading root from block id : %d', self._bfile.root_block_id)                
                self._root = self._bfile.read_node(self._bfile.root_block_id)                
                self._root._loaded = True
                logging.info("Root node : %s", self._root)
            except ValueError:                
                self._root = BTreeNode(self._keys_size)
                logging.info("new root node has created in memory as file has no root node data")
                    

        node = self._find_node_to_insert_and_split_if_needed(None, self._root, key)
        node.add_key(key)
        self._elms_size += 1

        # node has added a new key. so we need to write it into disk
        logging.debug("Writing node: %s", node)
        bid = self._bfile.write_node(node)
        logging.info("Node: %s has been written to disk at block_id: %d", node.keys, bid)
        node.block_id = bid

    def __len__(self):
        return self._elms_size

    
    # It finds a node to insert a key.
    # While finding a suitable node,  if it encounters a full node, it split that right away
    def _find_node_to_insert_and_split_if_needed(self, parent, node, key):
        logging.debug("find_node_to_insert_and_split_if_needed with node: %s , parent: %s", node.keys, parent.keys if parent else [])
        if node.is_full():
            node, left, right = self._split_node(parent, node) 
            
            bid = self._bfile.write_node(left)
            left.block_id = bid
            logging.info("Left node: %s has been written to disk at  block_id: %d", left.keys, bid)
        
            bid = self._bfile.write_node(right)
            logging.info("Right node: %s has been written to disk at block_id: %d", right.keys, bid)
            right.block_id = bid

            # Write parent node as last so that it can stores its child block ids.
            bid = self._bfile.write_node(node)
            node.block_id = bid
            logging.info("Parent node: %s has been written to disk at block_id: %d", node.keys, bid)

         
        if node.is_leaf():
            return node
        else:
            n = node.find_child(key)
            return self._find_node_to_insert_and_split_if_needed(node, node.children[n], key)

    def _split_node(self, parent, node):
        logging.debug("Split the node : %s", node.keys)
        left = node

        # create a new node
        right = BTreeNode(self._keys_size)
        # find median node
        m = self._keys_size // 2
        median = left.keys[m]
        logging.debug("median index: %d and median key : %s", m, median)

        if left is self._root:
            logging.debug("Splitting root node...")
            parent = BTreeNode(self._keys_size)
            parent.add_key(median)
            self._root = parent

            left.move_keys_and_children(m, right)

            # new root should point to left and right nodes
            parent.add_child(left)
            parent.add_child(right)
            logging.info("Root node has been split")

        else:
            # split is in non root node
            left.move_keys_and_children(m, right)
            # move the median key to parent
            parent.add_key(median)
            parent.add_child(right)

        return (parent,left,right)


    def _find_node(self, node, key):
        logging.debug("Finding key: %s in node: %s", key, node.keys if node else [])
        
        if not node.is_loaded():
            logging.debug("Node is not yet loaded. Hence Loading it from block_id: %d", node.block_id)
            node = self._bfile.read_node(node.block_id)            
            node.set_loaded(True)
            logging.info("Loaded the node from the disk: %s", node) 
                
        idx = node.key_at(key)
        if idx != -1:
            logging.debug("Key: %s found at index: %d in node: %s", key, idx, node.keys)
            return node.keys[idx]    

        
        idx = node.find_child_index(key)
        logging.debug("Looking key: %s at child node: %d", key, idx)        
        return self._find_node(node.children[idx], key)
        
            

    def find(self, key):
        logging.debug("Root node : %s", self._root.keys if self._root else [])
        if not self._root:
            logging.debug("Loading root node from block id: %s", self._bfile.root_block_id)
            self._root = self._bfile.read_node(self._bfile.root_block_id)
            self._root.set_loaded(True)
            logging.info("Loaded root node from the disk: %s", self._root)

        return self._find_node(self._root, key)


    def get_nodes_by_level(self):
        levels = []
        stack = []
        root = None
        if not self._root.is_loaded():            
            # TOFIX Hard coded root node
            self._root = self._bfile.read_node(0)
            logging.debug("block id = %d", self._root.block_id)
            logging.debug("root => %d", self._root)

        stack.insert(0, [[root]])        
        while stack:                       
            level = stack.pop()   
            elm = []
            for nodes in level:     
                for n in nodes:                
                    elm.append(n.keys)                    

            levels.append(elm)    

            elm = []  
            for nodes in level:
                for n in nodes:               
                    if n._children:
                        elm.append(n.children)             

            if elm:
                stack.insert(0, elm)
        return levels        

    def __str__(self):
        return str(self.get_nodes_by_level())

    
    def close(self):
        self._bfile.close()    

