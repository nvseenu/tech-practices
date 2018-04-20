import struct
import os

class BlockFile:
    """
       This class splits a file into blocks. Each block is associated with an id starting from 0.
       This class can store and retrive given data bytes into/from given block id.
       It does not care about what bytes it is asked to store / retrieve as long as they fit with given 
       block size.
    """
    META_SIZE = 32

    def __init__(self,filepath, block_size):        
        self.bfile = None
        self.block_size = block_size        
        filemode = 'rb+' if os.path.exists(filepath) else 'wb+'        
        self.bfile = open(filepath, filemode)     

    
    def offset(self, block_id):
        return block_id * self.block_size       

    def read(self, block_id):
        self.bfile.seek(self.offset(block_id))
        bdata = self.bfile.read(self.block_size)        
        if len(bdata) == 0:
            raise ValueError("Unable to unpack the data at block id : {} since it is empty".format(block_id))

        data = self.unpack_block(bdata)
        return data

    def write(self, block_id, data):
        if len(data) > self.block_size:
            raise ValueError("Given data size: {} is greater than configured block size: {}".format(len(data), self.block_size))
        self.bfile.seek(self.offset(block_id))
        b = self.pack_block(block_id, data)        
        self.bfile.write(b)

    def close(self):
        if self.bfile:
            self.bfile.flush()
            self.bfile.close()  


    def pack_block(self, block_id, data):
        barr = bytearray(self.block_size)
        mb = self._write_meta(block_id, len(data))
        if len(mb) > BlockFile.META_SIZE:
            raise ValueError("Block meta data size {} exceeds defined META_SIZE: {}".format(len(mb), Block.META_SIZE))
        barr[0:BlockFile.META_SIZE] = mb
        barr[BlockFile.META_SIZE:] = data
        return barr

    def unpack_block(self, bdata):
        meta = self._read_meta(bdata)
        bsize = meta[1]
        data = bdata[BlockFile.META_SIZE:BlockFile.META_SIZE+bsize]
        return data

    def _read_meta(self, bdata):
        bmeta = bdata[:BlockFile.META_SIZE]        
        msize = struct.calcsize('LL')         
        if len(bmeta) < msize:
            raise ValueError("Unable to read meta data as block that has been read has {} bytes".format(len(bmeta)))         
        meta = struct.unpack("LL", bmeta[:msize])
        return meta

    def _write_meta(self, block_id, data_size):
        return struct.pack('LL', block_id, data_size)   

