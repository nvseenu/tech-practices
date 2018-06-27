require "minitest/autorun"

require_relative "../../../lib/btree"
require_relative "../../../lib/btree/btree_impl"
require_relative "../../../lib/btree/block_file"
require_relative "../../../lib/btree/node_file"
require_relative "helper"

class BTreeTest < MiniTest::Test
  # Tests if insert() can insert given key at appropriate node
  # by following BTree rules.
  # We use find_value() to ensure the keys we inserted are found.
  def test_insert
    total_keys = 10
    users = (1..total_keys).map {|id| User.new(id, "name:#{id}", "#{id}@email.com") }

    # Insert given user records
    metadata_file_stub = MetadataFileStub.new
    node_file_stub = NodeFileStub.new
    btree = BTree::BTreeImpl.new(4, metadata_file_stub, node_file_stub)
    users.reverse_each {|u| btree.insert(u) }
    btree.close

    assert_equal(total_keys, btree.length, "Wrong length counted")

    btree = BTree::BTreeImpl.new(4, metadata_file_stub, node_file_stub)
    users.each do |u|
      assert_equal(u, btree.find_value(u), "Expected key is not found")
    end
    btree.close
  end
end
