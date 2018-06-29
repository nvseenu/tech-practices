require_relative "cache"

module BTree

  #
  # This class caches the block data. Whenever block data is required, first it
  # will check if it contains it. if so, it will return it, instead of
  # hitting BlockFile object. This saves the time spent on reading
  # the cached block.
  # 
  # It tracks the cache stats such as cache hit and cache miss.
  # We can use them to know whether given capacity provides 
  # any performance gain or not.
  #
  class BlockFileWithCache

    attr_reader :cache_hit, :cache_miss
    def initialize(block_file, capacity=4)
      @cache = BTree::LFUCache.new(capacity)
      @block_file = block_file
      @cache_hit = 0
      @cache_miss = 0
    end

    def create(block)
      @block_file.create(block)
    end

    def read(block_id)
      if !@cache.contains(block_id)
        data = @block_file.read(block_id)
        @cache.put(block_id, data)
        @cache_miss+=1
      else
        @cache_hit+=1
      end

      @cache.get(block_id)
    end

    def update(block_id, data)
      @block_file.update(block_id, data)
      @cache.remove(block_id)
    end

    def delete(node_id); end

    def close
      @block_file.close
    end

    def to_s
      "BlockFileWithCache {cache_hit: #{@cache_hit}, cache_miss: #{@cache_miss}}"
    end

  end
end
