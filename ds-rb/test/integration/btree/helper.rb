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
    "User{ #{@id}, #{@name}, #{@email} }"
  end
end
