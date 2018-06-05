package btree

import (
	"bytes"
	"fmt"
	"testing"
)

//Tests if Read and Write methods of BlockFile interface are working properly
func TestBlockFile(t *testing.T) {
	//Bydefault, current execution directory will be in ds-go/src/btree
	//Since our output dir is in ds-go directory, we have to refer it like below.
	bfile, err := NewBlockFile("../../output/btree-data/btree.bin", 1024)
	if err != nil {
		t.Errorf("Got an error openining a file: %v", err)
	}

	for i := 0; i < 10; i++ {
		s := fmt.Sprintf("This is test content:%d", i)
		if err := bfile.Write(i, []byte(s)); err != nil {
			t.Errorf("Got an error while writing blockId: %d and error: %v", i, err)
		}
	}

	if err = bfile.Close(); err != nil {
		t.Errorf("Got an error while closing a file: %v", err)
	}

	//Open block file again to check the data written above can be read properly

	bfile, err = NewBlockFile("../../output/btree-data/btree.bin", 1024)
	if err != nil {
		t.Errorf("Got an error openining a file: %v", err)
	}

	for i := 9; i >= 0; i-- {
		s := fmt.Sprintf("This is test content:%d", i)
		if data, err := bfile.Read(i); err != nil {
			t.Errorf("Got an error while reading blockId: %d and error: %v", i, err)
		} else {
			if !bytes.Equal([]byte(s), data) {
				t.Errorf("Data Mismatch: Expected data: >>>%x<<<, but got >>>%x<<<", s, data)
			}
		}
	}

	if err = bfile.Close(); err != nil {
		t.Errorf("Got an error while closing a file: %v", err)
	}
}

//Tests if block struct can be constructed from bytes and vice versa.
func TestBlockBytes(t *testing.T) {
	data := []byte("srinivasan")
	block := newBlock(data)
	bb, _ := bytesFromBlock(block)

	if block1, err := newBlockFromBytes(bb); err != nil {
		t.Errorf("Got an error while getting a block from given byes. error = %v", err)
	} else {

		if block.Len != block1.Len {
			t.Errorf("Expected block data length: %d , but got %d", block.Len, block1.Len)
		}

		if !bytes.Equal(block.Data, block1.Data) {
			t.Errorf("Expected block: <%x> , but got <%x>", block.Data, block1.Data)
		}
	}
}
