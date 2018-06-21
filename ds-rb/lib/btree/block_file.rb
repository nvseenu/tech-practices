require_relative "block_error"

module BTree
  # This class provides an abstraction that reading and writing blocks
  # to the disk.
  # Users of this class do not need to worry about how their blocks
  # are stored internally.
  class BlockFile
    # Inits with below params
    #   block_size -  size of each block stored and retrieved
    #   file_path - The file at which blocks are stored.

    def initialize(block_size, file_path)
      @block_size = block_size
      @file_path = file_path

      unless File.exist?(file_path)
        File.open(file_path, "wb") do |f|
        end
      end

      @file = File.open(file_path, "rb+")
      @total_blocks = 0
    end

    #  Inserts given bytes of data and returns a block id
    def create(data)
      update(@total_blocks, data)
      @total_blocks += 1
    end

    # Returns a bytes of data associtated with given  block id
    # Returns nil if given block id is not found
    def read(block_id)
      off = offset(block_id)
      @file.seek(off)
      data = @file.readpartial(@block_size)
      bytes = data.unpack("c*")
      b = Block.from_bytes(bytes)
      b.data
    rescue EOFError => e
      raise BlockError, "Unable to read a block associated with id: #{block_id} due to error: #{e.message}"
    end

    # Finds a bytes of data associated with given block id, and replaces it with given
    # bytes of data
    def update(block_id, data)
      off = offset(block_id)
      @file.seek(off)
      @file.write(Block.new(data).to_bytes.pack("c*"))
    rescue IOError => e
      raise BlockError, "Unable to update the block associated with id: #{block_id} due to error: #{e.message}"
    end

    # Deletes the block associtated with given block id
    def delete(block_id); end

    # Closes the file handle if it is not nil
    def close
      @file.close unless @file.nil?
    end

    private

    # Calculates offset from given block id
    def offset(block_id)
      block_id * @block_size
    end
  end

  class Block
    include Comparable

    attr_reader :length, :data
    def initialize(data=[])
      @length = data.length
      @data = data
    end

    def <=>(other)
      res = @length <=> other.length

      if res.zero?
        @data <=> other.data
      else
        res
      end
    end

    def self.from_bytes(bytes)
      lb = bytes[0, 4]
      lens = lb.pack("c*").unpack("N*")
      raise "unable to find block length" if lens.length <= 0

      data = bytes[4, lens[0]]
      Block.new(data)
    end

    def to_bytes
      bytes = [@data.length].pack("N*").bytes
      bytes.append(*@data)
      bytes
    end

    def to_s
      "Block{#{@length}, #{@data}}"
    end
  end
end
