require_relative "./tree.rb"
module GitCommitTree
  module Core
    class TreeBuilder
      #
      #  Builds a tree by reading logs from given CommitReader instance
      #  and returns a complete tree
      #
      def build(commit_reader)
        tree = Tree.new
        commit_reader.read do |c|
          tree.add_commit(c)
        end
        tree
      end
    end
  end
end
