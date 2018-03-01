import unittest
from block_file import BlockFile


class BlockFileTest(unittest.TestCase):
    SIZE = 10*1024

    def test_init(self):
        b = BlockFile('btree.bin', BlockFileTest.SIZE)
        self.assertIsNotNone(b)       
        data_seed =  100
        total_writes = 1000            
        tdata = self.generate_data(data_seed)            

        for i in range(total_writes):            
            bs = bytes(tdata, 'utf-8')
            b.write(i, bs)
        b.close()   

        b = BlockFile('btree.bin', BlockFileTest.SIZE)
        for i in range(total_writes):
            d = b.read(i)           
            self.assertEqual(tdata, d.decode('utf-8'))
        b.close()

    def generate_data(self, n):
        ds = [str(i) for i in range(n)]            
        return "".join(ds)      

