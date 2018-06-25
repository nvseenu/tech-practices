require_relative "block_error"
require_relative "node_error"

module BTree
  # This class is used to store / retrieve node objects
  # It internally uses BlockFile object to store it.
  class NodeFile
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
    rescue BlockIOError => e
      raise NodeError, "Unable to read a node associated with given id: #{node_id} due to an error: #{e.message}"
    end

    # Writes a given node and returns an id if the node is new.
    def write(node)
      data = Marshal.dump(node)
      if node.id == -1
        return @block_file.create(data.bytes)
      else
        @block_file.update(node.id, data.bytes)
        return node.id
      end
    rescue BlockIOError => e
      raise NodeError, "Unable to read a node associated with given id: #{node_id} due to an error: #{e.message}"
    end

    # Deletes a Node object associated with given node id (int)
    def delete(node_id); end

    # Closes the block file
    def close
      @block_file.close unless @block_file.nil?
    end
  end
end
