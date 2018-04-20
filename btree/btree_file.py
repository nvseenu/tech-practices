from block_file import BlockFile
from btree_node import BTreeNode
import struct
import logging
import pickle
from collections import namedtuple

class BTreeFile:
    """
    This class stores and retrives given BTreeNode into a disk.   
    """
    LENGTH_HEADER_SIZE = struct.calcsize('L')

    def __init__(self, filepath, block_size, key_class):
        self._block_size = block_size
        self._blockfile = BlockFile(filepath, block_size)
        self._block_id_count = 0
        self._key_class = key_class
    

    def write_node(self, node):
        # Get a block id from instance variable when argument node has no block id defined
        block_id = node.block_id
        if not block_id:
            block_id = self._block_id_count
            self._block_id_count += 1

        block_bytes = self._get_block_bytes(node)
        self._blockfile.write(block_id, block_bytes)
        return block_id 


    def read_node(self, block_id):    
        block_bytes = self._blockfile.read(block_id)         
        return self._get_node(block_bytes)

    def close(self):
        self._blockfile.close()   

    
    def _get_block_bytes(self, node):
        # Collect all keys from the node
        keys = node.keys                 

        # Collect block ids of children of given node    
        child_block_ids = []
        for c in node.children:
            child_block_ids.append(c.block_id)
        
        payload = [keys, child_block_ids]
        payload_bytes = pickle.dumps(payload)
        length = len(payload_bytes)
       
        barr = bytearray(BTreeFile.LENGTH_HEADER_SIZE + length)    
        struct.pack_into('L', barr, 0, length)
        # Start fill the bytearray from 4th byte as length header takes first 4 bytes
        struct.pack_into(str(length)+'s', barr, BTreeFile.LENGTH_HEADER_SIZE, payload_bytes)
        return barr

    def _get_node(self, block_bytes):
        # Read length header first and unpack_from method returns length as tuple
        length_tup = struct.unpack_from('L', block_bytes, 0)            
        if length_tup is None:
            raise ValueError('Unable to read length header')

        length = length_tup[0]

        # Read payload from 4th byte as the length header takes first 4 bytes 
        payload_tup = struct.unpack_from(str(length)+'s', block_bytes, BTreeFile.LENGTH_HEADER_SIZE)
        if payload_tup is None:
            raise ValueError('Unable to read payload body')

        payload_bytes = payload_tup[0]
    
        keys, child_block_ids = pickle.loads(payload_bytes)
        return self._create_node(keys, child_block_ids)


    def _create_node(self, keys, child_block_ids):
        node = BTreeNode(4)
        for k in keys:            
            node.add_key(k)
     
        for block_id in child_block_ids:
            c = BTreeNode(4)
            c.block_id = block_id
            node.add_empty_child(c) 

        node.set_loaded(True)
        return node


