require_relative "../../lib/btree/node_error"
require_relative "../../lib/btree/metadata_file"

# Instances of below classeses are used to insert into Node instance
class User
  include Comparable
  attr_reader :id, :name, :email
  def initialize(id, name, email)
    @id = id
    @name = name
    @email = email
  end

  def <=>(other)
    @id <=> other.id
  end

  def to_s
    "User{ #{id}, #{name}, #{email} }"
  end
end

# A Stub to BlockFile class.
# It can be plugged into NodeFile to test NodeFile functionality alone.
class BlockFileStub
  def initialize
    @data_map = {}
  end

  def create(data)
    len = @data_map.length
    @data_map[len] = data
    len
  end

  def update(id, data)
    @data_map[id] = data
  end

  def read(id)
    @data_map[id]
  end

  def close; end
end

class NodeFileStub
  def initialize
    @node_map = {}
  end

  def write(node)
    id = if node.id == -1
           @node_map.length
         else
           node.id
         end

    @node_map[id] = node
    id
  end

  def read(id)
    raise BTree::NodeError, "Unable to find node for id: #{id}" unless @node_map.key? id
    @node_map[id]
  end

  def close; end
end

class MetadataFileStub
  def initialize
    @metadata = BTree::Metadata.new(0, 0)
  end

  def write(metadata)
    @metadata = metadata
  end

  def read
    @metadata
  end

  def close; end
end
