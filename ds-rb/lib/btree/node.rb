module BTree
  # Represents a node in btree data structure
  class Node
    include Comparable
    attr_reader :id, :keys, :child_node_ids
    attr_writer :id, :loaded, :root

    def initialize
      @id = 0
      @keys = []
      @child_node_ids = []
      @loaded = false
      @root = false
    end

    # Adds given key in ascending order
    def add_key(key)
      i = find_free_index(key)
      @keys.insert(i, key)
    end

    def loaded?
      @loaded
    end

    def root?
      @root
    end

    private

    # Find a free index to insert a given key.
    # Keys that comes beore the index should be smaller than given key.
    def find_free_index(key)
      @keys.each_with_index do |k, i|
        return i if k >= key
      end
      @keys.length
    end

    # Compares its attributes against given one.
    # Returns -1 if it is greater than given one.
    # Returns 1 if it is smaller
    # Returns 0 if both are equal
    def <=>(other)
      res = @id <=> id
      if res.zero?
        res
      else
        @keys <=> other.keys
      end
    end
  end
end
