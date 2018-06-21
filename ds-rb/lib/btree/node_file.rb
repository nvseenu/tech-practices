require_relative "block_error"
require_relative "node_error"

module BTree
  # This class is used to store / retrieve node objects
  # It internally uses BlockFile object to store it.
  class NodeFile
    # Root node's id will be stored in a block 0. Hence it is reserved for root.
    ROOT_BLOCK_ID = 0

    # This class internally uses BlockFile object
    # to store / retrieve node objects
    def initialize(block_file)
      @block_file = block_file
    end

    # Reads a node associated with a given node id (int)
    # returns a node object
    def read(node_id)
      data = @block_file.read(node_id)
      return nil if data.nil?

      Marshal.load(data.pack("c*"))
    rescue BlockError => e
      raise NodeError, "Unable to read a node associated with given id: #{node_id} due to an error: #{e.message}"
    end

    # Writes a given node and returns an id if the node is new.
    def write(node)
       data = Marshal.dump(node)
       if node.id == -1
          return @block_file.create(data.bytes)  
       else 
          @block_file.update(node.id , data.bytes)     
          return node.id
       end    
    rescue BlockError => e
      raise NodeError, "Unable to read a node associated with given id: #{node_id} due to an error: #{e.message}"
    end

    # Deletes a Node object associated with given node id (int)
    def delete(node_id); end

    # Closes the block file
    def close
      @block_file.close unless @block_file.nil?
    end

    # Read a root node it stored at ROOT_BLOCK_ID,
    # and then reads a node associated with the node id.
    def read_root
      bytes = read(ROOT_BLOCK_ID)
      raise NodeError, "Unable to read root node" if bytes.nil?

      ids = bytes.pack("c*").unpack("N*")
      raise NodeError, "Unable to read root node as we dont kow its block id" if ids.empty?

      root_id = ids[0]
      read(root_id)
    end

    # Writes given node, and store its node_id  at the root block id
    # for later reference. 
    def write_root(node)
      @block_file.update(node)
      rb = [node.id].pack("N*").bytes
      @block_file.update(ROOT_BLOCK_ID, rb)
    end
  end
end
