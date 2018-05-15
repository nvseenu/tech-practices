# tech-practices

This repo is a placeholder to practice concepts, designs and technologies i read.


class Meta:
    def __init__(self, size, block_id, data_size):
        self.block_id = block_id
        self.data_size = data_size
        self.size = size

    def to_bytes(self):
        return struct.pack('LL', self.block_id, self.data_size) 

    def from_bytes(bytes):
        bmeta = bytes[:self.size]        
        msize = struct.calcsize('LL')         
        if len(bmeta) < msize:
            raise ValueError("Unable to read meta data as block that has been read has {} bytes".format(len(bmeta)))         
        meta = struct.unpack("LL", bmeta[:msize])
        return Meta(meta[0], meta[1])         
            


class Block:
    def __init__(self, size, meta, data):        
        self.size = size
        self.meta = meta
        self.data = data        
        
    
    def to_bytes(self):
        barr = bytearray(self.size)
        barr[:self.meta.size] = self.meta.to_bytes()
        barr[self.meta.size:] = data
        return barr

    def from_bytes(bytes, meta_size, block_size):
        metabytes = bytes[0:meta_size]
        meta = Meta.from_bytes(metabytes)
        data = bytes[meta_size:meta_size+meta.data_size]
        return Block(meta, )
