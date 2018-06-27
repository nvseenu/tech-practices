module BTree
  class Metadata
    include Comparable
    attr_reader :root_node_id, :length
    def initialize(root_node_id, length)
      @root_node_id = root_node_id
      @length = length
    end

    def <=>(other)
      res = @root_node_id <=> other.root_node_id
      if res.zero?
        @length <=> other.length
      else
        res
      end
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
      Marshal.load(bytes.pack("c*"))
    end

    def write(metadata)
      data = Marshal.dump(metadata)
      @block_file.update(BLOCK_ID, data.bytes)
    end

    def close
      @block_file.close unless @block_file.nil?
    end
  end
end
