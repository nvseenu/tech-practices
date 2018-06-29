module BTree

  # 
  # It keeps given keys based on cache hit count. 
  # Less frequently used items are evicted when cache does not have a space.
  #
  class LFUCache
    def initialize(capacity=4)
      @capacity = capacity
      @entries = []
    end

    def put(key, value)
      new_entry = CacheEntry.new(key, value)
      idx = @entries.find_index(new_entry)
      if idx.nil?
        evict_entries_if_required
        @entries << new_entry
      else
        @entries[idx] = new_entry
      end
    end

    def get(key)
      idx = @entries.find_index(CacheEntry.new(key, ""))
      if idx.nil?
        nil
      else
        entry = @entries[idx]
        entry.hit
        entry.value
      end
    end

    def contains(key)
      idx = @entries.find_index(CacheEntry.new(key, ""))
      !idx.nil?
    end

    def remove(key)
      idx = @entries.find_index(CacheEntry.new(key, ""))
      @entries.delete_at(idx) unless idx.nil?
    end

    def remove_first
      @entries.shift
    end

    def length
      @entries.length
    end

    private

    def evict_entries_if_required
      return unless full?
      es = find_entries_to_evict
      es.take(@entries.length - @capacity + 1).each do |e|        
        @entries.delete(e)        
      end      
    end

    def find_entries_to_evict
      @entries.sort_by(&:hit_count)
    end

    def full?
      @entries.length == @capacity
    end
  end

  class CacheEntry
    include Comparable
    attr_reader :key, :value, :hit_count

    def initialize(key, value)
      @key = key
      @value = value
      @hit_count = 0
    end

    def hit
      @hit_count += 1
    end

    def <=>(other)
      @key <=> other.key
    end

    def to_s
      "CacheEntry{hit_count: #{@hit_count}, key: #{@key}"
    end

  end
end
