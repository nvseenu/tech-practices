require_relative "./commit"

module GitCommitTree
  module Core
    #
    # This class represents entire commits of a git repository along with various
    # branches.
    #
    class Tree
      def initialize
        @branch_heads = {}

        #
        # It keeps track of all commits by its id (sha).        
        # It is helpful for the use cases such as creatign new branch from another one,
        # and merging branches.        
        #
        @commits = {}
      end

      #
      #  Adds given commit right next to head of the given branch.
      #  if given commit is already found, it will not create a new link. 
      #  Instead, simply updates a branch head with the link
      #  
      #  If the commit is not found, new link between previous commit and
      #  current one will be created, and the link will be updated in 
      #  respective branch head
      #

      def add_commit(commit)
        raise "Arg commit is nil" if commit.nil?
        raise "id field is nil in commit arguement" if commit.id.nil?
        branch_name = commit.branch.to_sym        

        # Initially , the tree will be empty. 
        # So we can insert given commit directly
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
        @branch_heads.keys
      end

      #
      # Returns an iterator to head of given branch.
      #
      def commits(branch)
        raise "Given branch: #{branch} is not found in this repository" unless @branch_heads.key? branch.to_sym
        head = @branch_heads[branch.to_sym]
        CommitIterator.new(head)
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
        cs = []
        current = @current
        while !current.nil?
          cs << current.commit
          current = current.parents[0]
        end
        cs.reverse.each do |c|
          yield c
        end  
      end
    end
  end
end
