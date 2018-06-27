require "minitest/autorun"
require "benchmark"
require_relative "../../../lib/btree"
require_relative "../../../lib/btree/btree_impl"
require_relative "../../../lib/btree/block_file"
require_relative "../../../lib/btree/node_file"
require_relative "helper"

class BTreeImplTest < MiniTest::Test
  DATA_FILE_PATH = File.expand_path("../../../temp/btree-data.bin", __dir__)
  META_FILE_PATH = File.expand_path("../../../temp/btree-meta-data.bin", __dir__)

  def setup
    # Delete the data files. so that tests can run with new data files.
    [DATA_FILE_PATH, META_FILE_PATH].each {|f| File.delete(f) if File.exist? f }
  end

  # Tests if insert() can store given set of User objects. It also
  # tests if find_value() can find all inserted records.
  def test_insert_and_find
    total_keys = 1000
    users = (1..total_keys).map {|id| User.new(id, "name:#{id}", "#{id}@email.com") }

    # Insert given user records
    btree = create_btree
    users.reverse_each {|u| btree.insert(u) }
    btree.close

    # Check all inserted records are found
    btree = create_btree
    users.each do |u|
      actual_user = btree.find_value(u)
      assert_equal(u, actual_user, "Key not found")
    end
    btree.close
  end

  def create_btree
    block_file = BTree::BlockFile.new(4086, DATA_FILE_PATH)
    node_file = BTree::NodeFile.new(block_file)
    metadata_file = BTree::MetadataFile.new(BTree::BlockFile.new(1024, META_FILE_PATH))
    BTree::BTreeImpl.new(8, metadata_file, node_file)
  end

  def teardown; end
end
