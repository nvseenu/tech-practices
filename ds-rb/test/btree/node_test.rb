require "minitest/autorun"
require_relative "../../lib/btree"
require_relative "helper"

# Tests if Node class works as expected
class TestNode < MiniTest::Test
  # Tests if given key can be added into a node ascendign order
  def test_add_key
    n = BTree::Node.new

    users = (1..10).map do |i|
      User.new(i, "name#{i}", "g#{i}@gmail.com")
    end

    # I insert them in reverse orde. so that i can test if they are in
    # ascending order after they have been inserted
    users.reverse_each do |u|
      n.add_key(u)
    end
    assert_equal(users, n.keys, "Keys are not matching")
  end
end

