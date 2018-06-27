require "minitest/autorun"
require_relative "../../../lib/btree"
require_relative "../../../lib/btree/block_file"

# Tests Block instance works as expected
class TestBlock < MiniTest::Test
  def test_to_and_from_bytes
    data = "Hi how are you?"
    b = BTree::Block.new(data.bytes)
    b1 = BTree::Block.from_bytes(b.to_bytes)
    assert(b == b1, "Expected block: #{b}, but got #{b1}")
  end
end
