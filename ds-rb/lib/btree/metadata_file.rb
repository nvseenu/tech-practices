module BTree
  class Metadata
    attr_reader :root_node_id, :length
    def initialize(root_node_id, length)
      @root_node_id = root_node_id
      @length = length
    end

    def to_s
      "Metadata[#{@root_node_id}, #{@length}]"
    end
  end

  class MetadataFile
    BLOCK_ID = 0

    def initialize(block_file)
      @block_file = block_file
    end

    def read
      bytes = @block_file.read(BLOCK_ID)
      Marshal.load(bytes)
    end

    def write(metadata)
      data = Marshal.dump(metadata)
      @block_file.write(BLOCK_ID, data.bytes)
    end
  end
end
