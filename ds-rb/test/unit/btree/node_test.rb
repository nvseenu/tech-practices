require "minitest/autorun"
require_relative "../../../lib/btree"
require_relative "helper"

# Tests if Node class works as expected
class TestNode < MiniTest::Test
  # Tests if given key can be added into a node ascendign order
  def test_add_key
    n = BTree::Node.new(4)

    users = (1..4).map do |i|
      User.new(i, "name#{i}", "g#{i}@gmail.com")
    end

    # I insert them in reverse orde. so that i can test if they are in
    # ascending order after they have been inserted
    users.reverse_each do |u|
      n.add_key(u)
    end
    assert_equal(users, n.keys, "Keys are not matching")
  end

  # Tests if full? returns true when the node is full otherwise false
  def test_full
    n = BTree::Node.new(4)
    (1..3).each do |i|
      n.add_key(i)
      assert(n.full? == false, "Node is full")
    end

    # Add a last key that makes node full.
    n.add_key(4)
    assert(n.full? == true, "Node is not full")
  end

  # Tests if leaf? returns true when there are no children, otherwise false
  def test_leaf
    n = BTree::Node.new(4)
    assert(n.leaf? == true,
           "Node is leaf as it has #{n.child_node_ids.length} children")
    n.child_node_ids << 1
    assert(n.leaf? == false,
           "Node is not a leaf a it has #{n.child_node_ids.length} children")
  end

  # Tests if find_child_id() finds an appropriate child node id for
  # given key
  def test_find_child_node_id
    n = BTree::Node.new(4)
    # Add keys
    [10, 25, 40, 50].each {|k| n.add_key(k) }
    # Add child node ids
    (1..5).each {|i| n.child_node_ids << i }

    # Array of key with its expected child node id.
    test_data = [[5, 1],
                 [15, 2],
                 [30, 3],
                 [45, 4],
                 [55, 5]]

    test_data.each do |t|
      key = t[0]
      expected_child_id = t[1]
      assert_equal(expected_child_id,
                   n.find_child_node_id(key),
                   "Expected child id is not found")
    end
  end

  # Test if find_key_index() finds given key and returns its index
  def test_find_key_index
    n = BTree::Node.new(4)
    # Add keys
    [10, 25, 40, 50].each {|k| n.add_key(k) }

    # Array of key with its expected child node id.
    test_data = [[10, 0],
                 [25, 1],
                 [40, 2],
                 [50, 3],
                 [30, -1],
                 [55, -1]]

    test_data.each do |t|
      key = t[0]
      expected_index = t[1]
      assert_equal(expected_index,
                   n.find_key_index(key),
                   "Expected key index is not found")
    end
  end

  def test_move_keys_and_child_node_ids
    n = BTree::Node.new(4)
    # Add keys and children
    [10, 25, 40, 50].each {|k| n.add_key(k) }
    [1, 2, 3, 4, 5].each {|i| n.child_node_ids << i }

    t = BTree::Node.new(4)
    n.move_keys_and_child_node_ids(t, 3)
    assert_equal([10, 25, 40], n.keys, "Keys are mismatching")
    assert_equal([1, 2, 3], n.child_node_ids, "Child node ids are mismatching")
    assert_equal([50], t.keys, "Keys are mismatching in target node")
    assert_equal([4, 5], t.child_node_ids, "Child node ids are mismatching in target node")
  end

  def move_keys_and_child_node_ids(node, right, median)
    right_keys = node.keys.slice(median + 1, node.keys.length - median)
    right_keys.each do |k|
      right.add_key(k)
    end

    # Move the respective children to right node
    children = node.child_node_ids.slice(median + 1, node.child_node_ids.length - median)
    unless children.nil?
      children.each_with_index do |idx, i|
        right.child_node_ids.insert(i, idx)
        node.child_node_ids.delete(idx)
      end
    end
  end
end
