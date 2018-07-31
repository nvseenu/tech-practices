require_relative "./commit"

module GitCommitTree
  module Core
    #
    # This class represents commits of a version control system.
    #
    class Tree
      def initialize
        @branch_heads = {}
        @commits = {}
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

      def add_commit(commit)
        raise "Arg commit is nil" if commit.nil?
        raise "id field is nil in commit arguement" if commit.id.nil?
        branch_name = commit.branch.to_sym        

        # If there is no branches in the tree,
        # insert it by adding given branch.        
        if @branch_heads.length.zero?  
          node = Node.new(commit)        
          @branch_heads[branch_name] = node
          @commits[commit.id] = node
          return
        end

        node = nil  
        if @commits.key? commit.id 
          node = @commits[commit.id]  
        else
          branch_head = @branch_heads[branch_name]
          node = Node.new(commit, [branch_head])          
          @commits[commit.id] = node
        end

        @branch_heads[branch_name] = node
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
      attr_reader :commit, :parents

      def initialize(commit, parents=[])        
        @commit = commit
        @parents = parents
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
          @current = @current.parents[0]
        end
      end
    end
  end
end
