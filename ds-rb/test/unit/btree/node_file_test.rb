require "minitest/autorun"

require_relative "../../../lib/btree"
require_relative "../../../lib/btree/block_file"
require_relative "../../../lib/btree/node_file"
require_relative "helper"

# Tests if nodes can be stored and reetrieved successfully.
# Since the NodeFile class internally uses BlockFile, we created a stub for it
# and plug it into NodeFile object.
class NodeFileTest < MiniTest::Test
  # Tets if Node objects can be stored / retrieved successfully
  def test_create_and_read_node
    users = (1..10).map do |i|
      User.new(i, "name#{i}", "e#{i}@gmail.com")
    end

    nodes = users.map do |u|
      n = BTree::Node.new(4)
      n.add_key(u)
      n
    end

    block_file_stub = BlockFileStub.new
    node_file = BTree::NodeFile.new(block_file_stub)
    nodes.each do |n|
      id = node_file.write(n)
      n.id = id
    end
    node_file.close

    node_file = BTree::NodeFile.new(block_file_stub)
    nodes.each do |n|
      node1 = node_file.read(n.id)
      assert_equal(n, node1, "Unable to get back expected node")
    end
    node_file.close
  end
end
