require "minitest/autorun"

require_relative "../../../lib/btree"
require_relative "../../../lib/btree/block_file"
require_relative "../../../lib/btree/block_error"

class BlockFileTest < MiniTest::Test
  FILE_PATH = File.expand_path("../../../temp/block-file-data.bin", __dir__)

  # Deletes a data file and instantiates BlockFile class
  # before every test begins
  def setup
    File.delete(FILE_PATH) if File.exist? FILE_PATH
    block_size = 1024
    @block_file = BTree::BlockFile.new(block_size, FILE_PATH)
  end

  # Tests if block can be created into a file
  # and read back in any order from the file again.
  def test_create_and_read_blocks
    text = "Hi how are you?"
    10.times.each do |x|
      s = "#{text} : #{x}"
      @block_file.create(s.bytes)
    end
    @block_file.close

    @block_file = BTree::BlockFile.new(1024, FILE_PATH)

    10.times.reverse_each do |x|
      exp = "#{text} : #{x}".bytes
      data = @block_file.read(x)
      assert_equal(exp, data, "Block mismatch error")
    end
  end

  # Tests if read() throws an error when tries to read a block from an empty file.
  def test_read_when_file_is_empty
    assert_raises BTree::BlockIOError do
      @block_file.read(0)
    end
  end

  # Tests if read() throws BlockIOError when it is passed with non exisitng block id.
  def test_read_when_block_is_not_found
    # Insert 10 blocks
    10.times.each do |x|
      s = x.to_s
      @block_file.create(s.bytes)
    end

    # Look for non existing block id: 25
    assert_raises BTree::BlockIOError do
      @block_file.read(25)
    end
  end

  # Tests whether read() returns the same empty block stored earlier
  def test_read_when_block_is_empty
    block_id = @block_file.create("".bytes)
    # Look for non existing block id
    bytes = @block_file.read(block_id)
    assert_equal("", bytes.pack("c*"), "Empty block is not returned")
  end

  # Closes BlockFile instance after every test has run.
  def teardown
    @block_file.close
  end
end
