require_relative "./commit"

module GitCommitTree
  module Core
    #
    # This class represents commits of a version control system.
    #
    class Tree
      def initialize
        @branch_heads = {}
      end

      #
      #  Adds given commit right next to head of the given branch.
      #
      #  If we pass parent_branch_name, it finds out a head of parent branch and
      #  adds the commit right next to it.
      #
      #  If we pass parent_branch_name and commit_id, it finds the specific commit id
      #  and adds given commit rigt next to it.
      #

      def add_commit(commit, parent_branch_name=nil, parent_commit_id=nil)
        raise "Arg commit is nil" if commit.nil?
        raise "id field is nil in commit arguement" if commit.id.nil?
        branch_name = commit.branch.to_sym
        branch_head = @branch_heads[branch_name]
        # If there is no branches in the tree,
        # insert it by adding given branch.
        if @branch_heads.length.zero?
          @branch_heads[branch_name] = Node.new(branch_head, commit)
          return
        end

        # If branch for the given commit is already found,
        # then update its head
        if @branch_heads.key? branch_name
          @branch_heads[branch_name] = Node.new(branch_head, commit)
          return
        elsif parent_branch_name.nil? 
          @branch_heads[branch_name] = Node.new(nil, commit)
          return
        end

        # If parent_branch_name is passed, ensure if it exists in the tree
        raise "Given parent branch:#{parent_branch_name} is not found" unless @branch_heads.key? parent_branch_name

        parent_branch_head = @branch_heads[parent_branch_name]

        # If parent commit id is nill, just insert
        # given commit next to head of the parent branch
        if parent_commit_id.nil?
          @branch_heads[branch_name] = Node.new(parent_branch_head, commit)
          return
        end

        parent_branch_head = find_commmit_node(parent_branch_head, parent_commit_id)
        raise "Given commit id: #{parent_commit_id} is invalid" if parent_branch_head.nil?
        @branch_heads[branch_name] = Node.new(parent_branch_head, commit)
      end

      #
      #  Returns list of branches being tracked
      #
      def branches
        ks = @branch_heads.keys.find_all { |k| k != :master }
        ks.prepend(:master)
      end

      #
      # Returns an iterator to head of given branch.
      #
      def commits(branch)
        raise "Given branch: #{branch} is not found in this repository" unless @branch_heads.key? branch.to_sym
        head = @branch_heads[branch.to_sym]
        CommitIterator.new(head)
      end

      private

      # Finds node contains given commit id and returns it
      def find_commmit_node(branch_head, commit_id)
        found = false
        n = branch_head
        while !found && !n.nil?
          found = true if n.commit.id == commit_id
          n = n.parent
        end
        n
      end
    end

    class Node
      attr_reader :parent, :commit

      def initialize(parent, commit)
        @parent = parent
        @commit = commit
      end

      def to_s
        "Node{commit_id: #{commit.id}}"
      end
    end

    class CommitIterator
      include Enumerable

      def initialize(head)
        @head = head
        @current = @head
      end

      def each
        until @current.nil?
          yield @current.commit
          @current = @current.parent
        end
      end
    end
  end
end
