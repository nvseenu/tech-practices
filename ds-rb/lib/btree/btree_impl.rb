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
    end

    def insert(key)
      # if the root node is not loaded, load it first
      load_root_if_required
      node = find_free_node(nil, @root, key)
      node.add_key(key)
      @node_file.write(node)
      #puts "[WRITE] node: #{node.id}"
      @length += 1
    end

    def find_value(key)
      load_root_if_required
      #puts "Root node: #{@root}"
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
      #puts "[READ] Child: #{child.id}"
      find_node(child, key)
    end

    def close
      @node_file.close unless @node_file.nil?
    end

    private

    def find_free_node(parent, node, key)
       #puts "find_free_node => parent: #{parent},\n node: #{node},\n key: #{key}"
      if node.full?
        #puts "Split the node as it is full"
        node = split(parent, node)
        @root = node if node.root?
      end

      unless node.leaf?
        cid = node.find_child_node_id(key)
        child = @node_file.read(cid)        
        #puts "[READ] Child: #{child.id}"
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
      right_keys = node.keys.slice(median + 1, node.keys.length - median)
      right_keys.each do |k|
        right.add_key(k)
      end
      children = node.child_node_ids.slice(median+1, node.child_node_ids.length - median)
      unless children == nil 
        children.each_with_index do |idx, i|
           right.child_node_ids.insert(i, idx) 
           node.child_node_ids.delete(idx)
        end
      end   

  
      right.id = @node_file.write(right)
      #puts "[WRITE] Right node: #{right.id}"

      # Delete the keys that are moved into the right node, from left node
      node.keys = node.keys.slice(0, median)
      node.root = false # This is not a root any more
      @node_file.write(node)
      #puts "[WRITE] Left node: #{node.id}"

     
      # Add left and right nodes as children to the parent
      idx = parent.find_child_node_id_index(node.keys[0])
      parent.child_node_ids[idx] = node.id                 


      idx = parent.find_child_node_id_index(right.keys[0])
      parent.child_node_ids.insert(idx, right.id)                 
      #puts "child index to insert right node: #{idx} , id: #{right.id}"
      parent.id = @node_file.write(parent)
      #puts "[WRITE] parent node: #{parent.id}"
      parent
    end

    def load_root_if_required
      return @root unless @root.nil?

      begin
        metadata = @metadata_file.read
        #puts "[READ] metadata: #{metadata}"
        @root = @node_file.read(metadata.root_node_id)
        #puts "[READ] #{@root}"
      rescue NodeError => e
        puts e.to_s
        @root = Node.new(@keys_per_node, root: true)
        @root.id = @node_file.write(@root)
        puts "Created new root: #{@root}"
      end
    end
  end
end
