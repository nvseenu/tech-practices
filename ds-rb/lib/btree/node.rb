module BTree
  # Represents a node in btree data structure
  class Node
    include Comparable
    attr_reader :child_node_ids, :loaded
    attr_accessor :id, :keys, :root
    alias loaded? loaded
    alias root? root

    # Initializes the instance with below args:
    # total_keys - total keys can be stored per node
    # kargs - keyword arguements for other optional attributes
    #         such as id, keys, child_node_ids, loaded, root
    def initialize(total_keys,
                   id: -1,
                   keys: [],
                   child_node_ids: [],
                   loaded: false,
                   root: false)

      @total_keys = total_keys
      @id = id
      @keys = keys
      @child_node_ids = child_node_ids
      @loaded = loaded
      @root = root
    end

    # Adds given key in ascending order
    def add_key(key)
      raise "Node is full" if full?

      i = find_free_index(key)
      @keys.insert(i, key)
    end

    def find_key_index(key)
      @keys.each_with_index do |k, i|
        return i if k == key
      end
      -1
    end

    def full?
      @keys.length == @total_keys
    end

    def leaf?
      @child_node_ids.length.zero?
    end

    def find_child_node_id(key)
      @child_node_ids[find_child_node_id_index(key)]
    end

    def find_child_node_id_index(key)
      @keys.each_with_index do |k, i|
        return i if k >= key
      end
      @keys.length
    end

    # Compares its attributes against given one.
    # Returns an integer value
    #    -1 if it is greater than given one.
    #     1 if it is smaller
    #     0 if both are equal
    def <=>(other)
      res = @id <=> id
      if res.zero?
        res
      else
        @keys <=> other.keys
      end
    end

    def to_s
      "Node{id:#{id}, ks:#{keys}, cs:#{child_node_ids}, root:#{@root}, leaf:#{leaf?}}"
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
  end
end
