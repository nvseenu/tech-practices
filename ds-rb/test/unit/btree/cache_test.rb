require "minitest"
require_relative "../../../lib/btree/cache"

class LFUCacheTest < Minitest::Test
  def test_add_and_get
    c = BTree::LFUCache.new(4)
    entries = (1..4).to_a
    entries.each {|e| c.put(e, e) }
    assert_equal(entries.length, c.length, "Length is not counted properly")

    entries.each do |e|
      e1 = c.get(e)
      assert_equal(e, e1, "Key mismatch")
    end
  end

  def test_get_when_key_not_found
    c = BTree::LFUCache.new(4)
    assert_nil(c.get("1"))
  end

  def test_lfu
    c = BTree::LFUCache.new(4)
    entries = (1..4).to_a
    entries.each {|e| c.put(e, e) }

    # Below keys are accessed recently from the cache
    # it means 1 and 4 are accessed more frequently than 2 and 3.
    c.get(1)
    c.get(4)
    c.get(1)
    c.get(4)

    # When we insert key 5, there is no free space in the cache
    # hence 2 should be evicted since it is least frequently used.
    c.put(5, 5)
    assert_equal(4, c.length, "Length is not counted properly")
    assert_nil(c.get(2), "Key 2 is not evicted.")

    c.put(6, 6)
    assert_equal(4, c.length, "Length is not counted properly")
    assert_nil(c.get(3), "Key 3 is not evicted.")

    # Key 4 and 5 have been accessed 5 times. now they are the most frequently
    # used one comapared to others.
    4.times {  c.get(4); c.get(5) }

    c.put(7, 7)
    assert_equal(4, c.length, "Length is not counted properly")
    assert_nil(c.get(6), "Key 6 is not evicted.")
  end

  def test_update
    c = BTree::LFUCache.new(4)
    c.put(1, :one)
    assert_equal(:one, c.get(1), "Key mismatch")
    c.put(1, :new_one)
    assert_equal(:new_one, c.get(1), "Key mismatch")
  end
end
