require "minitest/autorun"

require_relative "../../lib/btree"
require_relative "../../lib/btree/btree_impl"
require_relative "../../lib/btree/block_file"
require_relative "../../lib/btree/node_file"

class BTreeTest < MiniTest::Test
  def setup
    metadata_file_stub = MetadataFileStub.new
    node_file_stub = NodeFileStub.new
    @btree = BTree::BTreeImpl.new(16, metadata_file_stub, node_file_stub)
  end

  # Tests if insert() can insert given key at appropriate node
  # by following BTree rules. 
  # We use find_value() to ensure the keys we inserted are found.
  def test_insert
   
    total_keys = 100000
    (1..total_keys).reverse_each do |k|      
      @btree.insert(k)      
    end

    assert_equal(total_keys, @btree.length, "Length is not matching")

    (1..total_keys).reverse_each do |k|     
     assert_equal(k, @btree.find_value(k), "Expected key is not found")
    end
  end

  def teardown
    @btree.close
  end
end

