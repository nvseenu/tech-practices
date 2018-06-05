package btree

import (
	"bytes"
	"encoding/binary"
	"log"
	"os"
)

type BlockFile interface {
	Read(blockId int) ([]byte, error)
	Write(blockId int, data []byte) error
	Close() error
}

type BlockFileRecord struct {
	filePath  string
	blockSize int
	file      *os.File
}

type block struct {
	Len  int
	Data []byte
}

func NewBlockFile(filePath string, blockSize int) (BlockFile, error) {

	f, err := os.OpenFile(filePath, os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		return nil, err
	}
	return &BlockFileRecord{filePath, blockSize, f}, nil
}

func (b *BlockFileRecord) Read(blockId int) ([]byte, error) {
	off, err := b.file.Seek(offset(blockId, b.blockSize), 0)
	if err != nil {
		return nil, err
	}

	bytesToRead := int64(b.blockSize)
	var finfo os.FileInfo
	if finfo, err = b.file.Stat(); err != nil {
		return nil, err
	}

	if off+int64(b.blockSize) > finfo.Size() {
		bytesToRead = finfo.Size() - off
	}

	bs := make([]byte, bytesToRead)
	if _, err = b.file.ReadAt(bs, off); err != nil {
		return nil, err
	}

	if block, err := newBlockFromBytes(bs); err != nil {
		return nil, err
	} else {
		return block.Data, nil
	}
}

func (b *BlockFileRecord) Write(blockId int, data []byte) error {
	off, err := b.file.Seek(offset(blockId, b.blockSize), 0)
	if err != nil {
		return err
	}
	var bs []byte
	if bs, err = bytesFromBlock(newBlock(data)); err != nil {
		return err
	}
	if _, err = b.file.WriteAt(bs, off); err != nil {
		return err
	}
	return nil
}

func (b *BlockFileRecord) Close() error {
	if b.file != nil {
		if err := b.file.Close(); err != nil {
			return err
		}
	}
	return nil
}

func offset(blockId, blockSize int) int64 {
	return int64(blockId * blockSize)
}

func bytesFromBlock(b *block) ([]byte, error) {
	var err error
	var buf bytes.Buffer
	if err = binary.Write(&buf, binary.BigEndian, uint32(b.Len)); err != nil {
		return nil, err
	}
	log.Printf("Len %d=>%x, total bytes: %d", b.Len, buf.Bytes(), len(buf.Bytes()))

	if _, err = buf.Write(b.Data); err != nil {
		return nil, err
	}
	log.Printf("Data %s=>%x", b.Data, buf.Bytes())
	log.Printf("Total bytes: %d", len(buf.Bytes()))

	return buf.Bytes(), nil
}

func newBlock(data []byte) *block {
	return &block{
		len(data),
		data,
	}
}

func newBlockFromBytes(bs []byte) (*block, error) {
	log.Printf("block bytes=>%x", bs)
	lenbs := bs[:4]
	log.Printf("len bytes=>%x", lenbs)
	buf := bytes.NewBuffer(lenbs)
	var len uint32
	var err error
	if err = binary.Read(buf, binary.BigEndian, &len); err != nil {
		return nil, err
	}
	log.Printf("length of data: %d", len)

	data := bs[4 : 4+int(len)]
	log.Printf("data bytes=>%x -- %s", data, data)

	return newBlock(data), nil
}
