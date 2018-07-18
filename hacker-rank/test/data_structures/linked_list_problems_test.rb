require 'minitest/autorun'
require_relative '../../lib/data_structures/linked_list_problems'

class LinkedListProblemsTest < MiniTest::Test
  def test_delete_node
    ls = create_linked_list([1, 2, 3, 4, 5])
    head = DataStructures.deleteNode(ls.head, 2)
    assert_equal([1, 2, 4, 5],  DataStructures.to_a(head), 'Wrong node got deleted')
  end

  def test_delete_root_node
    ls = create_linked_list([1, 2, 3, 4, 5])
    head = DataStructures.deleteNode(ls.head, 0)
    assert_equal([2, 3, 4, 5],  DataStructures.to_a(head), 'Wrong node got deleted')
  end

  def test_delete_node_with_invalid_position
    ls = create_linked_list([1, 2, 3, 4, 5])
    head = DataStructures.deleteNode(ls.head, 20)
    assert_equal([1, 2, 3, 4, 5], DataStructures.to_a(head), 'Wrong node got deleted')

    ls = create_linked_list([1, 2, 3, 4, 5])
    head = DataStructures.deleteNode(ls.head, -20)
    assert_equal([1, 2, 3, 4, 5], DataStructures.to_a(head), 'Wrong node got deleted')
  end

  def test_reverse
    ls = create_linked_list([1, 2, 3, 4, 5])
    DataStructures.reverse(ls.head)
    assert_equal([5, 4, 3, 2, 1], DataStructures.to_a(ls.head), 'List is not reversed')
  end

  def test_reverse_for_even_size
    ls = create_linked_list([1, 2, 3, 4])
    DataStructures.reverse(ls.head)
    assert_equal([4, 3, 2, 1], DataStructures.to_a(ls.head), 'List is not reversed')
  end

  def create_linked_list(items)
    ls = DataStructures::SinglyLinkedList.new
    items.each { |item| ls.insert_node item }
    ls
  end
end
