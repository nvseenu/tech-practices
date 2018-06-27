require "minitest"

require_relative "../../../lib/btree/metadata_file"
require_relative "helper"

class MetadataFileTest < MiniTest::Test
  def setup; end

  # Tests if write() writes given metadata into block file properly or not.
  # Also test if read() can read the metadata written earlier.
  def test_read_and_write
    mdata = BTree::Metadata.new(10, 25)

    block_file = BlockFileStub.new
    mfile = BTree::MetadataFile.new(block_file)
    mfile.write(mdata)
    mfile.close

    mfile = BTree::MetadataFile.new(block_file)
    actual_mdata = mfile.read
    assert_equal(mdata, actual_mdata, "Metadata got mismatch")
    mfile.close
  end

  def teardown; end

  def create_metadata_file; end
end
