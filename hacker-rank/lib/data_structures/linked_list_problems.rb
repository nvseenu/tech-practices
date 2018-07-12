module DataStructures
  class SinglyLinkedListNode
    attr_accessor :data, :next

    def initialize(node_data)
      @data = node_data
      @next = nil
    end
  end

  class SinglyLinkedList
    attr_accessor :head, :tail

    def initialize
      @head = nil
      @tail = nil
    end

    def insert_node(node_data)
      node = SinglyLinkedListNode.new node_data

      if !head
        self.head = node
      else
        tail.next = node
      end

      self.tail = node
    end
  end

  def self.to_a(head)
    n = head
    a = []
    until n.nil?
      a << n.data
      n = n.next
     end
    a
  end

  def self.deleteNode(head, position)
    return head if position < 0
    if position.zero?
      return head.next
    else
      n = find_node(head, position - 1)
      return head if n.nil?
      x = n.next
      n.next = x.next
    end
    head
  end

  def self.find_node(head, position)
    return nil if head.nil?

    n = head
    count = 0
    while !n.nil? && count < position
      n = n.next
      count += 1
    end
    n
  end
end
