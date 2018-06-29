require_relative "node"
require_relative "metadata_file"
require_relative "node_error"

module BTree
  class BTreeImpl
    attr_reader :length

    def initialize(keys_per_node, metadata_file, node_file)
      @keys_per_node = keys_per_node
      @metadata_file = metadata_file
      @node_file = node_file
      @root = nil
      @length = 0
      @metadata = nil
    end

    def insert(key)
      # if the root node is not loaded, load it first
      load_root_if_required
      node = find_free_node(nil, @root, key)
      node.add_key(key)
      @node_file.write(node)
      # puts "[WRITE] node: #{node.id}"
      @length += 1
    end

    def find_value(key)
      load_root_if_required
      # puts "Root node: #{@root}"
      node = find_node(@root, key)
      ki = node.find_key_index(key)
      if ki == -1
        nil
      else
        node.keys[ki]
      end
    end

    def find_node(node, key)
      return node if node.leaf?
      return node if node.find_key_index(key) != -1

      cid = node.find_child_node_id(key)
      child = @node_file.read(cid)
      # puts "[READ] Child: #{child.id}"
      find_node(child, key)
    end

    def close
      [@node_file, @metadata_file].each {|f| f.close unless f.nil? }
    end

    private

    def find_free_node(parent, node, key)
      # puts "find_free_node => parent: #{parent},\n node: #{node},\n key: #{key}"
      if node.full?
        # puts "Split the node as it is full"
        node = split(parent, node)
        if node.root?
          @root = node if node.root?
          @metadata = Metadata.new(@root.id, @length)
          @metadata_file.write(@metadata)
        end
      end

      unless node.leaf?
        cid = node.find_child_node_id(key)
        child = @node_file.read(cid)
        # puts "[READ] Child: #{child.id}"
        return find_free_node(node, child, key)
      end

      node
    end

    def split(parent, node)
      median = node.keys.length / 2
      median_key = node.keys[median]
      parent = BTree::Node.new(@keys_per_node, root: true) if parent.nil?
      parent.add_key(median_key)

      # Create right node and move the keys that come after median index
      right = BTree::Node.new(@keys_per_node)
      node.move_keys_and_child_node_ids(right, median + 1)
      node.keys.delete_at(median) # Delete the median as it is moved to parent

      right.id = @node_file.write(right)
      # puts "[WRITE] Right node: #{right.id}"

      # Delete the keys that are moved into the right node, from left node
      # node.keys = node.keys.slice(0, median)
      node.root = false # This is not a root any more
      @node_file.write(node)
      # puts "[WRITE] Left node: #{node.id}"

      # Add left and right nodes as children to the parent
      idx = parent.find_child_node_id_index(node.keys[0])
      parent.child_node_ids[idx] = node.id

      idx = parent.find_child_node_id_index(right.keys[0])
      parent.child_node_ids.insert(idx, right.id)
      # puts "child index to insert right node: #{idx} , id: #{right.id}"
      parent.id = @node_file.write(parent)
      # puts "[WRITE] parent node: #{parent.id}"
      parent
    end

    def load_root_if_required
      return @root unless @root.nil?

      begin
        # puts "Reading  metadata: #{@metadata}"
        @metadata = @metadata_file.read
        # puts "[READ] metadata: #{@metadata}"
        @root = @node_file.read(@metadata.root_node_id)
        # puts "[READ] #{@root}"
      rescue NodeError, BlockIOError
        # puts "Error occured: #{e}"
        @root = Node.new(@keys_per_node, root: true)
        @root.id = @node_file.write(@root)
        @metadata = BTree::Metadata.new(@root.id, 0)
        @metadata_file.write(@metadata)
        # puts "Created new root: #{@root}"
      end
    end
  end
end
